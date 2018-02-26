

-- | <fullname>AWS IoT</fullname> <p>AWS IoT provides secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each thing (Thing Registry), configure logging, and create and manage policies and credentials to authenticate things.</p> <p>For more information about how AWS IoT works, see the <a href="http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html">Developer Guide</a>.</p>
module AWS.Iot where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Iot" :: String


-- | <p>Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p>
acceptCertificateTransfer :: forall eff. AcceptCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
acceptCertificateTransfer = AWS.request serviceName "AcceptCertificateTransfer" 


-- | <p>Adds a thing to a thing group.</p>
addThingToThingGroup :: forall eff. AddThingToThingGroupRequest -> Aff (err :: AWS.RequestError | eff) AddThingToThingGroupResponse
addThingToThingGroup = AWS.request serviceName "AddThingToThingGroup" 


-- | <p>Associates a group with a continuous job. The following criteria must be met: </p> <ul> <li> <p>The job must have been created with the <code>targetSelection</code> field set to "CONTINUOUS".</p> </li> <li> <p>The job status must currently be "IN_PROGRESS".</p> </li> <li> <p>The total number of targets associated with a job must not exceed 100.</p> </li> </ul>
associateTargetsWithJob :: forall eff. AssociateTargetsWithJobRequest -> Aff (err :: AWS.RequestError | eff) AssociateTargetsWithJobResponse
associateTargetsWithJob = AWS.request serviceName "AssociateTargetsWithJob" 


-- | <p>Attaches a policy to the specified target.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachPolicy = AWS.request serviceName "AttachPolicy" 


-- | <p>Attaches the specified policy to the specified principal (certificate or other credential).</p> <p> <b>Note:</b> This API is deprecated. Please use <a>AttachPolicy</a> instead.</p>
attachPrincipalPolicy :: forall eff. AttachPrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachPrincipalPolicy = AWS.request serviceName "AttachPrincipalPolicy" 


-- | <p>Attaches the specified principal to the specified thing.</p>
attachThingPrincipal :: forall eff. AttachThingPrincipalRequest -> Aff (err :: AWS.RequestError | eff) AttachThingPrincipalResponse
attachThingPrincipal = AWS.request serviceName "AttachThingPrincipal" 


-- | <p>Cancels a pending transfer for the specified certificate.</p> <p> <b>Note</b> Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use <a>RejectCertificateTransfer</a> instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.</p> <p>After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.</p>
cancelCertificateTransfer :: forall eff. CancelCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
cancelCertificateTransfer = AWS.request serviceName "CancelCertificateTransfer" 


-- | <p>Cancels a job.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResponse
cancelJob = AWS.request serviceName "CancelJob" 


-- | <p>Clears the default authorizer.</p>
clearDefaultAuthorizer :: forall eff. ClearDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) ClearDefaultAuthorizerResponse
clearDefaultAuthorizer = AWS.request serviceName "ClearDefaultAuthorizer" 


-- | <p>Creates an authorizer.</p>
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) CreateAuthorizerResponse
createAuthorizer = AWS.request serviceName "CreateAuthorizer" 


-- | <p>Creates an X.509 certificate using the specified certificate signing request.</p> <p> <b>Note:</b> The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves. </p> <p> <b>Note:</b> Reusing the same certificate signing request (CSR) results in a distinct certificate.</p> <p>You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.</p> <p>Assuming a set of CSRs are located inside of the directory my-csr-directory:</p> <p>On Linux and OS X, the command is:</p> <p>$ ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.</p> <p>The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:</p> <p>$ ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}</p> <p>On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"</p>
createCertificateFromCsr :: forall eff. CreateCertificateFromCsrRequest -> Aff (err :: AWS.RequestError | eff) CreateCertificateFromCsrResponse
createCertificateFromCsr = AWS.request serviceName "CreateCertificateFromCsr" 


-- | <p>Creates a job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "CreateJob" 


-- | <p>Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key.</p> <p> <b>Note</b> This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.</p>
createKeysAndCertificate :: forall eff. CreateKeysAndCertificateRequest -> Aff (err :: AWS.RequestError | eff) CreateKeysAndCertificateResponse
createKeysAndCertificate = AWS.request serviceName "CreateKeysAndCertificate" 


-- | <p>Creates an AWS IoT OTAUpdate on a target group of things or groups.</p>
createOTAUpdate :: forall eff. CreateOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) CreateOTAUpdateResponse
createOTAUpdate = AWS.request serviceName "CreateOTAUpdate" 


-- | <p>Creates an AWS IoT policy.</p> <p>The created policy is the default version for the policy. This operation creates a policy version with a version identifier of <b>1</b> and sets <b>1</b> as the policy's default version.</p>
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyResponse
createPolicy = AWS.request serviceName "CreatePolicy" 


-- | <p>Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use <a>DeletePolicyVersion</a> to delete an existing version before you create a new one.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).</p>
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyVersionResponse
createPolicyVersion = AWS.request serviceName "CreatePolicyVersion" 


-- | <p>Creates a role alias.</p>
createRoleAlias :: forall eff. CreateRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) CreateRoleAliasResponse
createRoleAlias = AWS.request serviceName "CreateRoleAlias" 


-- | <p>Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream. The total size of a file associated with the stream cannot exceed more than 2 MB. The stream will be created with version 0. If a stream is created with the same streamID as a stream that existed and was deleted within last 90 days, we will resurrect that old stream by incrementing the version by 1.</p>
createStream :: forall eff. CreateStreamRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamResponse
createStream = AWS.request serviceName "CreateStream" 


-- | <p>Creates a thing record in the thing registry.</p>
createThing :: forall eff. CreateThingRequest -> Aff (err :: AWS.RequestError | eff) CreateThingResponse
createThing = AWS.request serviceName "CreateThing" 


-- | <p>Create a thing group.</p>
createThingGroup :: forall eff. CreateThingGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateThingGroupResponse
createThingGroup = AWS.request serviceName "CreateThingGroup" 


-- | <p>Creates a new thing type.</p>
createThingType :: forall eff. CreateThingTypeRequest -> Aff (err :: AWS.RequestError | eff) CreateThingTypeResponse
createThingType = AWS.request serviceName "CreateThingType" 


-- | <p>Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
createTopicRule :: forall eff. CreateTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
createTopicRule = AWS.request serviceName "CreateTopicRule" 


-- | <p>Deletes an authorizer.</p>
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DeleteAuthorizerResponse
deleteAuthorizer = AWS.request serviceName "DeleteAuthorizer" 


-- | <p>Deletes a registered CA certificate.</p>
deleteCACertificate :: forall eff. DeleteCACertificateRequest -> Aff (err :: AWS.RequestError | eff) DeleteCACertificateResponse
deleteCACertificate = AWS.request serviceName "DeleteCACertificate" 


-- | <p>Deletes the specified certificate.</p> <p>A certificate cannot be deleted if it has a policy attached to it or if its status is set to ACTIVE. To delete a certificate, first use the <a>DetachPrincipalPolicy</a> API to detach all policies. Next, use the <a>UpdateCertificate</a> API to set the certificate to the INACTIVE status.</p>
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteCertificate = AWS.request serviceName "DeleteCertificate" 


-- | <p>Delete an OTA update.</p>
deleteOTAUpdate :: forall eff. DeleteOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) DeleteOTAUpdateResponse
deleteOTAUpdate = AWS.request serviceName "DeleteOTAUpdate" 


-- | <p>Deletes the specified policy.</p> <p>A policy cannot be deleted if it has non-default versions or it is attached to any certificate.</p> <p>To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.</p> <p>When a policy is deleted using DeletePolicy, its default version is deleted with it.</p>
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicy = AWS.request serviceName "DeletePolicy" 


-- | <p>Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use ListPolicyVersions.</p>
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicyVersion = AWS.request serviceName "DeletePolicyVersion" 


-- | <p>Deletes a CA certificate registration code.</p>
deleteRegistrationCode :: forall eff. DeleteRegistrationCodeRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegistrationCodeResponse
deleteRegistrationCode = AWS.request serviceName "DeleteRegistrationCode" 


-- | <p>Deletes a role alias</p>
deleteRoleAlias :: forall eff. DeleteRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoleAliasResponse
deleteRoleAlias = AWS.request serviceName "DeleteRoleAlias" 


-- | <p>Deletes a stream.</p>
deleteStream :: forall eff. DeleteStreamRequest -> Aff (err :: AWS.RequestError | eff) DeleteStreamResponse
deleteStream = AWS.request serviceName "DeleteStream" 


-- | <p>Deletes the specified thing.</p>
deleteThing :: forall eff. DeleteThingRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingResponse
deleteThing = AWS.request serviceName "DeleteThing" 


-- | <p>Deletes a thing group.</p>
deleteThingGroup :: forall eff. DeleteThingGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingGroupResponse
deleteThingGroup = AWS.request serviceName "DeleteThingGroup" 


-- | <p>Deletes the specified thing type . You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling <a>DeprecateThingType</a>, then remove any associated things by calling <a>UpdateThing</a> to change the thing type on any associated thing, and finally use <a>DeleteThingType</a> to delete the thing type.</p>
deleteThingType :: forall eff. DeleteThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingTypeResponse
deleteThingType = AWS.request serviceName "DeleteThingType" 


-- | <p>Deletes the rule.</p>
deleteTopicRule :: forall eff. DeleteTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteTopicRule = AWS.request serviceName "DeleteTopicRule" 


-- | <p>Deletes a logging level.</p>
deleteV2LoggingLevel :: forall eff. DeleteV2LoggingLevelRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteV2LoggingLevel = AWS.request serviceName "DeleteV2LoggingLevel" 


-- | <p>Deprecates a thing type. You can not associate new things with deprecated thing type.</p>
deprecateThingType :: forall eff. DeprecateThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DeprecateThingTypeResponse
deprecateThingType = AWS.request serviceName "DeprecateThingType" 


-- | <p>Describes an authorizer.</p>
describeAuthorizer :: forall eff. DescribeAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DescribeAuthorizerResponse
describeAuthorizer = AWS.request serviceName "DescribeAuthorizer" 


-- | <p>Describes a registered CA certificate.</p>
describeCACertificate :: forall eff. DescribeCACertificateRequest -> Aff (err :: AWS.RequestError | eff) DescribeCACertificateResponse
describeCACertificate = AWS.request serviceName "DescribeCACertificate" 


-- | <p>Gets information about the specified certificate.</p>
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (err :: AWS.RequestError | eff) DescribeCertificateResponse
describeCertificate = AWS.request serviceName "DescribeCertificate" 


-- | <p>Describes the default authorizer.</p>
describeDefaultAuthorizer :: forall eff. DescribeDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) DescribeDefaultAuthorizerResponse
describeDefaultAuthorizer = AWS.request serviceName "DescribeDefaultAuthorizer" 


-- | <p>Returns a unique endpoint specific to the AWS account making the call.</p>
describeEndpoint :: forall eff. DescribeEndpointRequest -> Aff (err :: AWS.RequestError | eff) DescribeEndpointResponse
describeEndpoint = AWS.request serviceName "DescribeEndpoint" 


-- | <p>Describes event configurations.</p>
describeEventConfigurations :: forall eff. DescribeEventConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventConfigurationsResponse
describeEventConfigurations = AWS.request serviceName "DescribeEventConfigurations" 


-- | <p>Describes a search index.</p>
describeIndex :: forall eff. DescribeIndexRequest -> Aff (err :: AWS.RequestError | eff) DescribeIndexResponse
describeIndex = AWS.request serviceName "DescribeIndex" 


-- | <p>Describes a job.</p>
describeJob :: forall eff. DescribeJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobResponse
describeJob = AWS.request serviceName "DescribeJob" 


-- | <p>Describes a job execution.</p>
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobExecutionResponse
describeJobExecution = AWS.request serviceName "DescribeJobExecution" 


-- | <p>Describes a role alias.</p>
describeRoleAlias :: forall eff. DescribeRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) DescribeRoleAliasResponse
describeRoleAlias = AWS.request serviceName "DescribeRoleAlias" 


-- | <p>Gets information about a stream.</p>
describeStream :: forall eff. DescribeStreamRequest -> Aff (err :: AWS.RequestError | eff) DescribeStreamResponse
describeStream = AWS.request serviceName "DescribeStream" 


-- | <p>Gets information about the specified thing.</p>
describeThing :: forall eff. DescribeThingRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingResponse
describeThing = AWS.request serviceName "DescribeThing" 


-- | <p>Describe a thing group.</p>
describeThingGroup :: forall eff. DescribeThingGroupRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingGroupResponse
describeThingGroup = AWS.request serviceName "DescribeThingGroup" 


-- | <p>Describes a bulk thing provisioning task.</p>
describeThingRegistrationTask :: forall eff. DescribeThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingRegistrationTaskResponse
describeThingRegistrationTask = AWS.request serviceName "DescribeThingRegistrationTask" 


-- | <p>Gets information about the specified thing type.</p>
describeThingType :: forall eff. DescribeThingTypeRequest -> Aff (err :: AWS.RequestError | eff) DescribeThingTypeResponse
describeThingType = AWS.request serviceName "DescribeThingType" 


-- | <p>Detaches a policy from the specified target.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachPolicy = AWS.request serviceName "DetachPolicy" 


-- | <p>Removes the specified policy from the specified certificate.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>DetachPolicy</a> instead.</p>
detachPrincipalPolicy :: forall eff. DetachPrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachPrincipalPolicy = AWS.request serviceName "DetachPrincipalPolicy" 


-- | <p>Detaches the specified principal from the specified thing.</p>
detachThingPrincipal :: forall eff. DetachThingPrincipalRequest -> Aff (err :: AWS.RequestError | eff) DetachThingPrincipalResponse
detachThingPrincipal = AWS.request serviceName "DetachThingPrincipal" 


-- | <p>Disables the rule.</p>
disableTopicRule :: forall eff. DisableTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
disableTopicRule = AWS.request serviceName "DisableTopicRule" 


-- | <p>Enables the rule.</p>
enableTopicRule :: forall eff. EnableTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
enableTopicRule = AWS.request serviceName "EnableTopicRule" 


-- | <p>Gets effective policies.</p>
getEffectivePolicies :: forall eff. GetEffectivePoliciesRequest -> Aff (err :: AWS.RequestError | eff) GetEffectivePoliciesResponse
getEffectivePolicies = AWS.request serviceName "GetEffectivePolicies" 


-- | <p>Gets the search configuration.</p>
getIndexingConfiguration :: forall eff. GetIndexingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) GetIndexingConfigurationResponse
getIndexingConfiguration = AWS.request serviceName "GetIndexingConfiguration" 


-- | <p>Gets a job document.</p>
getJobDocument :: forall eff. GetJobDocumentRequest -> Aff (err :: AWS.RequestError | eff) GetJobDocumentResponse
getJobDocument = AWS.request serviceName "GetJobDocument" 


-- | <p>Gets the logging options.</p>
getLoggingOptions :: forall eff. GetLoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) GetLoggingOptionsResponse
getLoggingOptions = AWS.request serviceName "GetLoggingOptions" 


-- | <p>Gets an OTA update.</p>
getOTAUpdate :: forall eff. GetOTAUpdateRequest -> Aff (err :: AWS.RequestError | eff) GetOTAUpdateResponse
getOTAUpdate = AWS.request serviceName "GetOTAUpdate" 


-- | <p>Gets information about the specified policy with the policy document of the default version.</p>
getPolicy :: forall eff. GetPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyResponse
getPolicy = AWS.request serviceName "GetPolicy" 


-- | <p>Gets information about the specified policy version.</p>
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyVersionResponse
getPolicyVersion = AWS.request serviceName "GetPolicyVersion" 


-- | <p>Gets a registration code used to register a CA certificate with AWS IoT.</p>
getRegistrationCode :: forall eff. GetRegistrationCodeRequest -> Aff (err :: AWS.RequestError | eff) GetRegistrationCodeResponse
getRegistrationCode = AWS.request serviceName "GetRegistrationCode" 


-- | <p>Gets information about the rule.</p>
getTopicRule :: forall eff. GetTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) GetTopicRuleResponse
getTopicRule = AWS.request serviceName "GetTopicRule" 


-- | <p>Gets the fine grained logging options.</p>
getV2LoggingOptions :: forall eff. GetV2LoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) GetV2LoggingOptionsResponse
getV2LoggingOptions = AWS.request serviceName "GetV2LoggingOptions" 


-- | <p>Lists the policies attached to the specified thing group.</p>
listAttachedPolicies :: forall eff. ListAttachedPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedPoliciesResponse
listAttachedPolicies = AWS.request serviceName "ListAttachedPolicies" 


-- | <p>Lists the authorizers registered in your account.</p>
listAuthorizers :: forall eff. ListAuthorizersRequest -> Aff (err :: AWS.RequestError | eff) ListAuthorizersResponse
listAuthorizers = AWS.request serviceName "ListAuthorizers" 


-- | <p>Lists the CA certificates registered for your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCACertificates :: forall eff. ListCACertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListCACertificatesResponse
listCACertificates = AWS.request serviceName "ListCACertificates" 


-- | <p>Lists the certificates registered in your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>
listCertificates :: forall eff. ListCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListCertificatesResponse
listCertificates = AWS.request serviceName "ListCertificates" 


-- | <p>List the device certificates signed by the specified CA certificate.</p>
listCertificatesByCA :: forall eff. ListCertificatesByCARequest -> Aff (err :: AWS.RequestError | eff) ListCertificatesByCAResponse
listCertificatesByCA = AWS.request serviceName "ListCertificatesByCA" 


-- | <p>Lists the search indices.</p>
listIndices :: forall eff. ListIndicesRequest -> Aff (err :: AWS.RequestError | eff) ListIndicesResponse
listIndices = AWS.request serviceName "ListIndices" 


-- | <p>Lists the job executions for a job.</p>
listJobExecutionsForJob :: forall eff. ListJobExecutionsForJobRequest -> Aff (err :: AWS.RequestError | eff) ListJobExecutionsForJobResponse
listJobExecutionsForJob = AWS.request serviceName "ListJobExecutionsForJob" 


-- | <p>Lists the job executions for the specified thing.</p>
listJobExecutionsForThing :: forall eff. ListJobExecutionsForThingRequest -> Aff (err :: AWS.RequestError | eff) ListJobExecutionsForThingResponse
listJobExecutionsForThing = AWS.request serviceName "ListJobExecutionsForThing" 


-- | <p>Lists jobs.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResponse
listJobs = AWS.request serviceName "ListJobs" 


-- | <p>Lists OTA updates.</p>
listOTAUpdates :: forall eff. ListOTAUpdatesRequest -> Aff (err :: AWS.RequestError | eff) ListOTAUpdatesResponse
listOTAUpdates = AWS.request serviceName "ListOTAUpdates" 


-- | <p>Lists certificates that are being transferred but not yet accepted.</p>
listOutgoingCertificates :: forall eff. ListOutgoingCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListOutgoingCertificatesResponse
listOutgoingCertificates = AWS.request serviceName "ListOutgoingCertificates" 


-- | <p>Lists your policies.</p>
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPoliciesResponse
listPolicies = AWS.request serviceName "ListPolicies" 


-- | <p>Lists the principals associated with the specified policy.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListTargetsForPolicy</a> instead.</p>
listPolicyPrincipals :: forall eff. ListPolicyPrincipalsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyPrincipalsResponse
listPolicyPrincipals = AWS.request serviceName "ListPolicyPrincipals" 


-- | <p>Lists the versions of the specified policy and identifies the default version.</p>
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyVersionsResponse
listPolicyVersions = AWS.request serviceName "ListPolicyVersions" 


-- | <p>Lists the policies attached to the specified principal. If you use an Cognito identity, the ID must be in <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html#API_GetCredentialsForIdentity_RequestSyntax">AmazonCognito Identity format</a>.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListAttachedPolicies</a> instead.</p>
listPrincipalPolicies :: forall eff. ListPrincipalPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPrincipalPoliciesResponse
listPrincipalPolicies = AWS.request serviceName "ListPrincipalPolicies" 


-- | <p>Lists the things associated with the specified principal.</p>
listPrincipalThings :: forall eff. ListPrincipalThingsRequest -> Aff (err :: AWS.RequestError | eff) ListPrincipalThingsResponse
listPrincipalThings = AWS.request serviceName "ListPrincipalThings" 


-- | <p>Lists the role aliases registered in your account.</p>
listRoleAliases :: forall eff. ListRoleAliasesRequest -> Aff (err :: AWS.RequestError | eff) ListRoleAliasesResponse
listRoleAliases = AWS.request serviceName "ListRoleAliases" 


-- | <p>Lists all of the streams in your AWS account.</p>
listStreams :: forall eff. ListStreamsRequest -> Aff (err :: AWS.RequestError | eff) ListStreamsResponse
listStreams = AWS.request serviceName "ListStreams" 


-- | <p>List targets for the specified policy.</p>
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (err :: AWS.RequestError | eff) ListTargetsForPolicyResponse
listTargetsForPolicy = AWS.request serviceName "ListTargetsForPolicy" 


-- | <p>List the thing groups in your account.</p>
listThingGroups :: forall eff. ListThingGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListThingGroupsResponse
listThingGroups = AWS.request serviceName "ListThingGroups" 


-- | <p>List the thing groups to which the specified thing belongs.</p>
listThingGroupsForThing :: forall eff. ListThingGroupsForThingRequest -> Aff (err :: AWS.RequestError | eff) ListThingGroupsForThingResponse
listThingGroupsForThing = AWS.request serviceName "ListThingGroupsForThing" 


-- | <p>Lists the principals associated with the specified thing.</p>
listThingPrincipals :: forall eff. ListThingPrincipalsRequest -> Aff (err :: AWS.RequestError | eff) ListThingPrincipalsResponse
listThingPrincipals = AWS.request serviceName "ListThingPrincipals" 


-- | <p>Information about the thing registration tasks.</p>
listThingRegistrationTaskReports :: forall eff. ListThingRegistrationTaskReportsRequest -> Aff (err :: AWS.RequestError | eff) ListThingRegistrationTaskReportsResponse
listThingRegistrationTaskReports = AWS.request serviceName "ListThingRegistrationTaskReports" 


-- | <p>List bulk thing provisioning tasks.</p>
listThingRegistrationTasks :: forall eff. ListThingRegistrationTasksRequest -> Aff (err :: AWS.RequestError | eff) ListThingRegistrationTasksResponse
listThingRegistrationTasks = AWS.request serviceName "ListThingRegistrationTasks" 


-- | <p>Lists the existing thing types.</p>
listThingTypes :: forall eff. ListThingTypesRequest -> Aff (err :: AWS.RequestError | eff) ListThingTypesResponse
listThingTypes = AWS.request serviceName "ListThingTypes" 


-- | <p>Lists your things. Use the <b>attributeName</b> and <b>attributeValue</b> parameters to filter your things. For example, calling <code>ListThings</code> with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute <b>Color</b> with the value <b>Red</b>. </p>
listThings :: forall eff. ListThingsRequest -> Aff (err :: AWS.RequestError | eff) ListThingsResponse
listThings = AWS.request serviceName "ListThings" 


-- | <p>Lists the things in the specified group.</p>
listThingsInThingGroup :: forall eff. ListThingsInThingGroupRequest -> Aff (err :: AWS.RequestError | eff) ListThingsInThingGroupResponse
listThingsInThingGroup = AWS.request serviceName "ListThingsInThingGroup" 


-- | <p>Lists the rules for the specific topic.</p>
listTopicRules :: forall eff. ListTopicRulesRequest -> Aff (err :: AWS.RequestError | eff) ListTopicRulesResponse
listTopicRules = AWS.request serviceName "ListTopicRules" 


-- | <p>Lists logging levels.</p>
listV2LoggingLevels :: forall eff. ListV2LoggingLevelsRequest -> Aff (err :: AWS.RequestError | eff) ListV2LoggingLevelsResponse
listV2LoggingLevels = AWS.request serviceName "ListV2LoggingLevels" 


-- | <p>Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.</p>
registerCACertificate :: forall eff. RegisterCACertificateRequest -> Aff (err :: AWS.RequestError | eff) RegisterCACertificateResponse
registerCACertificate = AWS.request serviceName "RegisterCACertificate" 


-- | <p>Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.</p>
registerCertificate :: forall eff. RegisterCertificateRequest -> Aff (err :: AWS.RequestError | eff) RegisterCertificateResponse
registerCertificate = AWS.request serviceName "RegisterCertificate" 


-- | <p>Provisions a thing.</p>
registerThing :: forall eff. RegisterThingRequest -> Aff (err :: AWS.RequestError | eff) RegisterThingResponse
registerThing = AWS.request serviceName "RegisterThing" 


-- | <p>Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from <b>PENDING_TRANSFER</b> to <b>INACTIVE</b>.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p> <p>This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.</p>
rejectCertificateTransfer :: forall eff. RejectCertificateTransferRequest -> Aff (err :: AWS.RequestError | eff) Unit
rejectCertificateTransfer = AWS.request serviceName "RejectCertificateTransfer" 


-- | <p>Remove the specified thing from the specified group.</p>
removeThingFromThingGroup :: forall eff. RemoveThingFromThingGroupRequest -> Aff (err :: AWS.RequestError | eff) RemoveThingFromThingGroupResponse
removeThingFromThingGroup = AWS.request serviceName "RemoveThingFromThingGroup" 


-- | <p>Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>
replaceTopicRule :: forall eff. ReplaceTopicRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
replaceTopicRule = AWS.request serviceName "ReplaceTopicRule" 


-- | <p>The query search index.</p>
searchIndex :: forall eff. SearchIndexRequest -> Aff (err :: AWS.RequestError | eff) SearchIndexResponse
searchIndex = AWS.request serviceName "SearchIndex" 


-- | <p>Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.</p>
setDefaultAuthorizer :: forall eff. SetDefaultAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) SetDefaultAuthorizerResponse
setDefaultAuthorizer = AWS.request serviceName "SetDefaultAuthorizer" 


-- | <p>Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.</p>
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
setDefaultPolicyVersion = AWS.request serviceName "SetDefaultPolicyVersion" 


-- | <p>Sets the logging options.</p>
setLoggingOptions :: forall eff. SetLoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
setLoggingOptions = AWS.request serviceName "SetLoggingOptions" 


-- | <p>Sets the logging level.</p>
setV2LoggingLevel :: forall eff. SetV2LoggingLevelRequest -> Aff (err :: AWS.RequestError | eff) Unit
setV2LoggingLevel = AWS.request serviceName "SetV2LoggingLevel" 


-- | <p>Sets the logging options for the V2 logging service.</p>
setV2LoggingOptions :: forall eff. SetV2LoggingOptionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
setV2LoggingOptions = AWS.request serviceName "SetV2LoggingOptions" 


-- | <p>Creates a bulk thing provisioning task.</p>
startThingRegistrationTask :: forall eff. StartThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) StartThingRegistrationTaskResponse
startThingRegistrationTask = AWS.request serviceName "StartThingRegistrationTask" 


-- | <p>Cancels a bulk thing provisioning task.</p>
stopThingRegistrationTask :: forall eff. StopThingRegistrationTaskRequest -> Aff (err :: AWS.RequestError | eff) StopThingRegistrationTaskResponse
stopThingRegistrationTask = AWS.request serviceName "StopThingRegistrationTask" 


-- | <p>Test custom authorization.</p>
testAuthorization :: forall eff. TestAuthorizationRequest -> Aff (err :: AWS.RequestError | eff) TestAuthorizationResponse
testAuthorization = AWS.request serviceName "TestAuthorization" 


-- | <p>Invoke the specified custom authorizer for testing purposes.</p>
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) TestInvokeAuthorizerResponse
testInvokeAuthorizer = AWS.request serviceName "TestInvokeAuthorizer" 


-- | <p>Transfers the specified certificate to the specified AWS account.</p> <p>You can cancel the transfer until it is acknowledged by the recipient.</p> <p>No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.</p> <p>The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.</p> <p>The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.</p>
transferCertificate :: forall eff. TransferCertificateRequest -> Aff (err :: AWS.RequestError | eff) TransferCertificateResponse
transferCertificate = AWS.request serviceName "TransferCertificate" 


-- | <p>Updates an authorizer.</p>
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (err :: AWS.RequestError | eff) UpdateAuthorizerResponse
updateAuthorizer = AWS.request serviceName "UpdateAuthorizer" 


-- | <p>Updates a registered CA certificate.</p>
updateCACertificate :: forall eff. UpdateCACertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateCACertificate = AWS.request serviceName "UpdateCACertificate" 


-- | <p>Updates the status of the specified certificate. This operation is idempotent.</p> <p>Moving a certificate from the ACTIVE state (including REVOKED) will not disconnect currently connected devices, but these devices will be unable to reconnect.</p> <p>The ACTIVE state is required to authenticate devices connecting to AWS IoT using a certificate.</p>
updateCertificate :: forall eff. UpdateCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateCertificate = AWS.request serviceName "UpdateCertificate" 


-- | <p>Updates the event configurations.</p>
updateEventConfigurations :: forall eff. UpdateEventConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) UpdateEventConfigurationsResponse
updateEventConfigurations = AWS.request serviceName "UpdateEventConfigurations" 


-- | <p>Updates the search configuration.</p>
updateIndexingConfiguration :: forall eff. UpdateIndexingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) UpdateIndexingConfigurationResponse
updateIndexingConfiguration = AWS.request serviceName "UpdateIndexingConfiguration" 


-- | <p>Updates a role alias.</p>
updateRoleAlias :: forall eff. UpdateRoleAliasRequest -> Aff (err :: AWS.RequestError | eff) UpdateRoleAliasResponse
updateRoleAlias = AWS.request serviceName "UpdateRoleAlias" 


-- | <p>Updates an existing stream. The stream version will be incremented by one.</p>
updateStream :: forall eff. UpdateStreamRequest -> Aff (err :: AWS.RequestError | eff) UpdateStreamResponse
updateStream = AWS.request serviceName "UpdateStream" 


-- | <p>Updates the data for a thing.</p>
updateThing :: forall eff. UpdateThingRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingResponse
updateThing = AWS.request serviceName "UpdateThing" 


-- | <p>Update a thing group.</p>
updateThingGroup :: forall eff. UpdateThingGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingGroupResponse
updateThingGroup = AWS.request serviceName "UpdateThingGroup" 


-- | <p>Updates the groups to which the thing belongs.</p>
updateThingGroupsForThing :: forall eff. UpdateThingGroupsForThingRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingGroupsForThingResponse
updateThingGroupsForThing = AWS.request serviceName "UpdateThingGroupsForThing" 


-- | <p>The input for the AcceptCertificateTransfer operation.</p>
newtype AcceptCertificateTransferRequest = AcceptCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }


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


newtype ActionList = ActionList (Array Action)


newtype ActionType = ActionType String


newtype AddThingToThingGroupRequest = AddThingToThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  }


newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse 
  { 
  }


newtype AdditionalParameterMap = AdditionalParameterMap (Map Key Value)


newtype AlarmName = AlarmName String


newtype AllowAutoRegistration = AllowAutoRegistration Boolean


-- | <p>Contains information that allowed the authorization.</p>
newtype Allowed = Allowed 
  { "Policies'" :: NullOrUndefined (Policies)
  }


newtype AscendingOrder = AscendingOrder Boolean


newtype AssociateTargetsWithJobRequest = AssociateTargetsWithJobRequest 
  { "Targets'" :: (JobTargets)
  , "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined (Comment)
  }


newtype AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }


-- | <p>The input for the AttachPrincipalPolicy operation.</p>
newtype AttachPrincipalPolicyRequest = AttachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }


-- | <p>The input for the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalRequest = AttachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }


-- | <p>The output from the AttachThingPrincipal operation.</p>
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse 
  { 
  }


newtype AttributeName = AttributeName String


-- | <p>The attribute payload.</p>
newtype AttributePayload = AttributePayload 
  { "Attributes'" :: NullOrUndefined (Attributes)
  , "Merge'" :: NullOrUndefined (Flag)
  }


newtype AttributeValue = AttributeValue String


newtype Attributes = Attributes (Map AttributeName AttributeValue)


newtype AttributesMap = AttributesMap (Map Key Value)


newtype AuthDecision = AuthDecision String


-- | <p>A collection of authorization information.</p>
newtype AuthInfo = AuthInfo 
  { "ActionType'" :: NullOrUndefined (ActionType)
  , "Resources'" :: NullOrUndefined (Resources)
  }


newtype AuthInfos = AuthInfos (Array AuthInfo)


-- | <p>The authorizer result.</p>
newtype AuthResult = AuthResult 
  { "AuthInfo'" :: NullOrUndefined (AuthInfo)
  , "Allowed'" :: NullOrUndefined (Allowed)
  , "Denied'" :: NullOrUndefined (Denied)
  , "AuthDecision'" :: NullOrUndefined (AuthDecision)
  , "MissingContextValues'" :: NullOrUndefined (MissingContextValues)
  }


newtype AuthResults = AuthResults (Array AuthResult)


newtype AuthorizerArn = AuthorizerArn String


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


newtype AuthorizerFunctionArn = AuthorizerFunctionArn String


newtype AuthorizerName = AuthorizerName String


newtype AuthorizerStatus = AuthorizerStatus String


-- | <p>The authorizer summary.</p>
newtype AuthorizerSummary = AuthorizerSummary 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }


newtype Authorizers = Authorizers (Array AuthorizerSummary)


newtype AutoRegistrationStatus = AutoRegistrationStatus String


newtype AwsAccountId = AwsAccountId String


newtype AwsArn = AwsArn String


newtype AwsIotJobArn = AwsIotJobArn String


newtype AwsIotJobId = AwsIotJobId String


newtype AwsIotSqlVersion = AwsIotSqlVersion String


newtype BucketName = BucketName String


-- | <p>A CA certificate.</p>
newtype CACertificate = CACertificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CACertificateStatus)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }


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


newtype CACertificateStatus = CACertificateStatus String


newtype CACertificates = CACertificates (Array CACertificate)


-- | <p>The input for the CancelCertificateTransfer operation.</p>
newtype CancelCertificateTransferRequest = CancelCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  }


newtype CancelJobRequest = CancelJobRequest 
  { "JobId'" :: (JobId)
  , "Comment'" :: NullOrUndefined (Comment)
  }


newtype CancelJobResponse = CancelJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }


newtype CanceledThings = CanceledThings Int


newtype CannedAccessControlList = CannedAccessControlList String


-- | <p>Information about a certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "Status'" :: NullOrUndefined (CertificateStatus)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }


newtype CertificateArn = CertificateArn String


-- | <p>Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.</p>
newtype CertificateConflictException = CertificateConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


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


newtype CertificateId = CertificateId String


newtype CertificateName = CertificateName String


-- | <p>The PEM of a certificate.</p>
newtype CertificatePem = CertificatePem String


newtype CertificateSigningRequest = CertificateSigningRequest String


-- | <p>The certificate operation is not allowed.</p>
newtype CertificateStateException = CertificateStateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype CertificateStatus = CertificateStatus String


-- | <p>The certificate is invalid.</p>
newtype CertificateValidationException = CertificateValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Certificates = Certificates (Array Certificate)


newtype ClearDefaultAuthorizerRequest = ClearDefaultAuthorizerRequest 
  { 
  }


newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse 
  { 
  }


newtype ClientId = ClientId String


-- | <p>Describes an action that updates a CloudWatch alarm.</p>
newtype CloudwatchAlarmAction = CloudwatchAlarmAction 
  { "RoleArn'" :: (AwsArn)
  , "AlarmName'" :: (AlarmName)
  , "StateReason'" :: (StateReason)
  , "StateValue'" :: (StateValue)
  }


-- | <p>Describes an action that captures a CloudWatch metric.</p>
newtype CloudwatchMetricAction = CloudwatchMetricAction 
  { "RoleArn'" :: (AwsArn)
  , "MetricNamespace'" :: (MetricNamespace)
  , "MetricName'" :: (MetricName)
  , "MetricValue'" :: (MetricValue)
  , "MetricUnit'" :: (MetricUnit)
  , "MetricTimestamp'" :: NullOrUndefined (MetricTimestamp)
  }


newtype Code = Code String


-- | <p>Describes the method to use when code signing a file.</p>
newtype CodeSigning = CodeSigning 
  { "AwsSignerJobId'" :: NullOrUndefined (SigningJobId)
  , "CustomCodeSigning'" :: NullOrUndefined (CustomCodeSigning)
  }


-- | <p>Describes the certificate chain being used when code signing a file.</p>
newtype CodeSigningCertificateChain = CodeSigningCertificateChain 
  { "Stream'" :: NullOrUndefined (Stream)
  , "CertificateName'" :: NullOrUndefined (CertificateName)
  , "InlineDocument'" :: NullOrUndefined (InlineDocument)
  }


-- | <p>Describes the signature for a file.</p>
newtype CodeSigningSignature = CodeSigningSignature 
  { "Stream'" :: NullOrUndefined (Stream)
  , "InlineDocument'" :: NullOrUndefined (Signature)
  }


newtype CognitoIdentityPoolId = CognitoIdentityPoolId String


newtype Comment = Comment String


-- | <p>Configuration.</p>
newtype Configuration = Configuration 
  { "Enabled" :: NullOrUndefined (Enabled)
  }


-- | <p>A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.</p>
newtype ConflictingResourceUpdateException = ConflictingResourceUpdateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Count = Count Int


newtype CreateAuthorizerRequest = CreateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: (AuthorizerFunctionArn)
  , "TokenKeyName'" :: (TokenKeyName)
  , "TokenSigningPublicKeys'" :: (PublicKeyMap)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }


newtype CreateAuthorizerResponse = CreateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }


-- | <p>The input for the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrRequest = CreateCertificateFromCsrRequest 
  { "CertificateSigningRequest'" :: (CertificateSigningRequest)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }


-- | <p>The output from the CreateCertificateFromCsr operation.</p>
newtype CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  }


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


newtype CreateJobResponse = CreateJobResponse 
  { "JobArn'" :: NullOrUndefined (JobArn)
  , "JobId'" :: NullOrUndefined (JobId)
  , "Description'" :: NullOrUndefined (JobDescription)
  }


-- | <p>The input for the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateRequest = CreateKeysAndCertificateRequest 
  { "SetAsActive'" :: NullOrUndefined (SetAsActive)
  }


-- | <p>The output of the CreateKeysAndCertificate operation.</p>
newtype CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "KeyPair'" :: NullOrUndefined (KeyPair)
  }


newtype CreateOTAUpdateRequest = CreateOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  , "Description'" :: NullOrUndefined (OTAUpdateDescription)
  , "Targets'" :: (Targets)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "Files'" :: (OTAUpdateFiles)
  , "RoleArn'" :: (RoleArn)
  , "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap)
  }


newtype CreateOTAUpdateResponse = CreateOTAUpdateResponse 
  { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId)
  , "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId)
  , "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn)
  , "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn)
  , "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus)
  }


-- | <p>The input for the CreatePolicy operation.</p>
newtype CreatePolicyRequest = CreatePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  }


-- | <p>The output from the CreatePolicy operation.</p>
newtype CreatePolicyResponse = CreatePolicyResponse 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  }


-- | <p>The input for the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionRequest = CreatePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyDocument'" :: (PolicyDocument)
  , "SetAsDefault'" :: NullOrUndefined (SetAsDefault)
  }


-- | <p>The output of the CreatePolicyVersion operation.</p>
newtype CreatePolicyVersionResponse = CreatePolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  }


newtype CreateRoleAliasRequest = CreateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  }


newtype CreateRoleAliasResponse = CreateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn)
  }


newtype CreateStreamRequest = CreateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "Files'" :: (StreamFiles)
  , "RoleArn'" :: (RoleArn)
  }


newtype CreateStreamResponse = CreateStreamResponse 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  }


newtype CreateThingGroupRequest = CreateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ParentGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties)
  }


newtype CreateThingGroupResponse = CreateThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  }


-- | <p>The input for the CreateThing operation.</p>
newtype CreateThingRequest = CreateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  }


-- | <p>The output of the CreateThing operation.</p>
newtype CreateThingResponse = CreateThingResponse 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "ThingId'" :: NullOrUndefined (ThingId)
  }


-- | <p>The input for the CreateThingType operation.</p>
newtype CreateThingTypeRequest = CreateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  }


-- | <p>The output of the CreateThingType operation.</p>
newtype CreateThingTypeResponse = CreateThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeId'" :: NullOrUndefined (ThingTypeId)
  }


-- | <p>The input for the CreateTopicRule operation.</p>
newtype CreateTopicRuleRequest = CreateTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }


newtype CreatedAtDate = CreatedAtDate Number


newtype CreationDate = CreationDate Number


newtype CredentialDurationSeconds = CredentialDurationSeconds Int


-- | <p>Describes a custom method used to code sign a file.</p>
newtype CustomCodeSigning = CustomCodeSigning 
  { "Signature'" :: NullOrUndefined (CodeSigningSignature)
  , "CertificateChain'" :: NullOrUndefined (CodeSigningCertificateChain)
  , "HashAlgorithm'" :: NullOrUndefined (HashAlgorithm)
  , "SignatureAlgorithm'" :: NullOrUndefined (SignatureAlgorithm)
  }


newtype DateType = DateType Number


newtype DeleteAuthorizerRequest = DeleteAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }


newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse 
  { 
  }


-- | <p>Input for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateRequest = DeleteCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }


-- | <p>The output for the DeleteCACertificate operation.</p>
newtype DeleteCACertificateResponse = DeleteCACertificateResponse 
  { 
  }


-- | <p>The input for the DeleteCertificate operation.</p>
newtype DeleteCertificateRequest = DeleteCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "ForceDelete'" :: NullOrUndefined (ForceDelete)
  }


-- | <p>You can't delete the resource because it is attached to one or more resources.</p>
newtype DeleteConflictException = DeleteConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype DeleteOTAUpdateRequest = DeleteOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }


newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse 
  { 
  }


-- | <p>The input for the DeletePolicy operation.</p>
newtype DeletePolicyRequest = DeletePolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }


-- | <p>The input for the DeletePolicyVersion operation.</p>
newtype DeletePolicyVersionRequest = DeletePolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }


-- | <p>The input for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeRequest = DeleteRegistrationCodeRequest 
  { 
  }


-- | <p>The output for the DeleteRegistrationCode operation.</p>
newtype DeleteRegistrationCodeResponse = DeleteRegistrationCodeResponse 
  { 
  }


newtype DeleteRoleAliasRequest = DeleteRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }


newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse 
  { 
  }


newtype DeleteStreamRequest = DeleteStreamRequest 
  { "StreamId'" :: (StreamId)
  }


newtype DeleteStreamResponse = DeleteStreamResponse 
  { 
  }


newtype DeleteThingGroupRequest = DeleteThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }


newtype DeleteThingGroupResponse = DeleteThingGroupResponse 
  { 
  }


-- | <p>The input for the DeleteThing operation.</p>
newtype DeleteThingRequest = DeleteThingRequest 
  { "ThingName'" :: (ThingName)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }


-- | <p>The output of the DeleteThing operation.</p>
newtype DeleteThingResponse = DeleteThingResponse 
  { 
  }


-- | <p>The input for the DeleteThingType operation.</p>
newtype DeleteThingTypeRequest = DeleteThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }


-- | <p>The output for the DeleteThingType operation.</p>
newtype DeleteThingTypeResponse = DeleteThingTypeResponse 
  { 
  }


-- | <p>The input for the DeleteTopicRule operation.</p>
newtype DeleteTopicRuleRequest = DeleteTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }


newtype DeleteV2LoggingLevelRequest = DeleteV2LoggingLevelRequest 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: (LogTargetName)
  }


newtype DeliveryStreamName = DeliveryStreamName String


-- | <p>Contains information that denied the authorization.</p>
newtype Denied = Denied 
  { "ImplicitDeny'" :: NullOrUndefined (ImplicitDeny)
  , "ExplicitDeny'" :: NullOrUndefined (ExplicitDeny)
  }


-- | <p>The input for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeRequest = DeprecateThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  , "UndoDeprecate'" :: NullOrUndefined (UndoDeprecate)
  }


-- | <p>The output for the DeprecateThingType operation.</p>
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse 
  { 
  }


newtype DeprecationDate = DeprecationDate Number


newtype DescribeAuthorizerRequest = DescribeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }


newtype DescribeAuthorizerResponse = DescribeAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription)
  }


-- | <p>The input for the DescribeCACertificate operation.</p>
newtype DescribeCACertificateRequest = DescribeCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }


-- | <p>The output from the DescribeCACertificate operation.</p>
newtype DescribeCACertificateResponse = DescribeCACertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined (CACertificateDescription)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  }


-- | <p>The input for the DescribeCertificate operation.</p>
newtype DescribeCertificateRequest = DescribeCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  }


-- | <p>The output of the DescribeCertificate operation.</p>
newtype DescribeCertificateResponse = DescribeCertificateResponse 
  { "CertificateDescription'" :: NullOrUndefined (CertificateDescription)
  }


newtype DescribeDefaultAuthorizerRequest = DescribeDefaultAuthorizerRequest 
  { 
  }


newtype DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse 
  { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription)
  }


-- | <p>The input for the DescribeEndpoint operation.</p>
newtype DescribeEndpointRequest = DescribeEndpointRequest 
  { "EndpointType'" :: NullOrUndefined (EndpointType)
  }


-- | <p>The output from the DescribeEndpoint operation.</p>
newtype DescribeEndpointResponse = DescribeEndpointResponse 
  { "EndpointAddress'" :: NullOrUndefined (EndpointAddress)
  }


newtype DescribeEventConfigurationsRequest = DescribeEventConfigurationsRequest 
  { 
  }


newtype DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse 
  { "EventConfigurations'" :: NullOrUndefined (EventConfigurations)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  , "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate)
  }


newtype DescribeIndexRequest = DescribeIndexRequest 
  { "IndexName'" :: (IndexName)
  }


newtype DescribeIndexResponse = DescribeIndexResponse 
  { "IndexName'" :: NullOrUndefined (IndexName)
  , "IndexStatus'" :: NullOrUndefined (IndexStatus)
  , "Schema'" :: NullOrUndefined (IndexSchema)
  }


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (JobId)
  , "ThingName'" :: (ThingName)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }


newtype DescribeJobRequest = DescribeJobRequest 
  { "JobId'" :: (JobId)
  }


newtype DescribeJobResponse = DescribeJobResponse 
  { "DocumentSource'" :: NullOrUndefined (JobDocumentSource)
  , "Job'" :: NullOrUndefined (Job)
  }


newtype DescribeRoleAliasRequest = DescribeRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  }


newtype DescribeRoleAliasResponse = DescribeRoleAliasResponse 
  { "RoleAliasDescription'" :: NullOrUndefined (RoleAliasDescription)
  }


newtype DescribeStreamRequest = DescribeStreamRequest 
  { "StreamId'" :: (StreamId)
  }


newtype DescribeStreamResponse = DescribeStreamResponse 
  { "StreamInfo'" :: NullOrUndefined (StreamInfo)
  }


newtype DescribeThingGroupRequest = DescribeThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  }


newtype DescribeThingGroupResponse = DescribeThingGroupResponse 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "Version'" :: NullOrUndefined (Version)
  , "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties)
  , "ThingGroupMetadata'" :: NullOrUndefined (ThingGroupMetadata)
  }


newtype DescribeThingRegistrationTaskRequest = DescribeThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }


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


-- | <p>The input for the DescribeThing operation.</p>
newtype DescribeThingRequest = DescribeThingRequest 
  { "ThingName'" :: (ThingName)
  }


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


-- | <p>The input for the DescribeThingType operation.</p>
newtype DescribeThingTypeRequest = DescribeThingTypeRequest 
  { "ThingTypeName'" :: (ThingTypeName)
  }


-- | <p>The output for the DescribeThingType operation.</p>
newtype DescribeThingTypeResponse = DescribeThingTypeResponse 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeId'" :: NullOrUndefined (ThingTypeId)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata)
  }


newtype Description = Description String


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Target'" :: (PolicyTarget)
  }


-- | <p>The input for the DetachPrincipalPolicy operation.</p>
newtype DetachPrincipalPolicyRequest = DetachPrincipalPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Principal'" :: (Principal)
  }


-- | <p>The input for the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalRequest = DetachThingPrincipalRequest 
  { "ThingName'" :: (ThingName)
  , "Principal'" :: (Principal)
  }


-- | <p>The output from the DetachThingPrincipal operation.</p>
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse 
  { 
  }


newtype DetailsKey = DetailsKey String


newtype DetailsMap = DetailsMap (Map DetailsKey DetailsValue)


newtype DetailsValue = DetailsValue String


newtype DisableAllLogs = DisableAllLogs Boolean


-- | <p>The input for the DisableTopicRuleRequest operation.</p>
newtype DisableTopicRuleRequest = DisableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }


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


-- | <p>Describes an action to write to a DynamoDB table.</p> <p>This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.</p>
newtype DynamoDBv2Action = DynamoDBv2Action 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "PutItem'" :: NullOrUndefined (PutItemInput)
  }


newtype DynamoKeyType = DynamoKeyType String


newtype DynamoOperation = DynamoOperation String


newtype EffectivePolicies = EffectivePolicies (Array EffectivePolicy)


-- | <p>The policy that has the effect on the authorization results.</p>
newtype EffectivePolicy = EffectivePolicy 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  }


-- | <p>Describes an action that writes data to an Amazon Elasticsearch Service domain.</p>
newtype ElasticsearchAction = ElasticsearchAction 
  { "RoleArn'" :: (AwsArn)
  , "Endpoint'" :: (ElasticsearchEndpoint)
  , "Index'" :: (ElasticsearchIndex)
  , "Type'" :: (ElasticsearchType)
  , "Id'" :: (ElasticsearchId)
  }


newtype ElasticsearchEndpoint = ElasticsearchEndpoint String


newtype ElasticsearchId = ElasticsearchId String


newtype ElasticsearchIndex = ElasticsearchIndex String


newtype ElasticsearchType = ElasticsearchType String


-- | <p>The input for the EnableTopicRuleRequest operation.</p>
newtype EnableTopicRuleRequest = EnableTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }


newtype Enabled = Enabled Boolean


newtype EndpointAddress = EndpointAddress String


newtype EndpointType = EndpointType String


-- | <p>Error information.</p>
newtype ErrorInfo = ErrorInfo 
  { "Code'" :: NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined (OTAUpdateErrorMessage)
  }


newtype ErrorMessage = ErrorMessage String


newtype EventConfigurations = EventConfigurations (Map EventType Configuration)


newtype EventType = EventType String


newtype ExecutionNumber = ExecutionNumber Number


newtype ExpiresInSec = ExpiresInSec Number


-- | <p>Information that explicitly denies authorization.</p>
newtype ExplicitDeny = ExplicitDeny 
  { "Policies'" :: NullOrUndefined (Policies)
  }


newtype FailedThings = FailedThings Int


newtype FileId = FileId Int


newtype FileName = FileName String


-- | <p>Describes an action that writes data to an Amazon Kinesis Firehose stream.</p>
newtype FirehoseAction = FirehoseAction 
  { "RoleArn'" :: (AwsArn)
  , "DeliveryStreamName'" :: (DeliveryStreamName)
  , "Separator'" :: NullOrUndefined (FirehoseSeparator)
  }


newtype FirehoseSeparator = FirehoseSeparator String


newtype Flag = Flag Boolean


newtype ForceDelete = ForceDelete Boolean


newtype FunctionArn = FunctionArn String


newtype GEMaxResults = GEMaxResults Int


newtype GetEffectivePoliciesRequest = GetEffectivePoliciesRequest 
  { "Principal'" :: NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId)
  , "ThingName'" :: NullOrUndefined (ThingName)
  }


newtype GetEffectivePoliciesResponse = GetEffectivePoliciesResponse 
  { "EffectivePolicies'" :: NullOrUndefined (EffectivePolicies)
  }


newtype GetIndexingConfigurationRequest = GetIndexingConfigurationRequest 
  { 
  }


newtype GetIndexingConfigurationResponse = GetIndexingConfigurationResponse 
  { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration)
  }


newtype GetJobDocumentRequest = GetJobDocumentRequest 
  { "JobId'" :: (JobId)
  }


newtype GetJobDocumentResponse = GetJobDocumentResponse 
  { "Document'" :: NullOrUndefined (JobDocument)
  }


-- | <p>The input for the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsRequest = GetLoggingOptionsRequest 
  { 
  }


-- | <p>The output from the GetLoggingOptions operation.</p>
newtype GetLoggingOptionsResponse = GetLoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }


newtype GetOTAUpdateRequest = GetOTAUpdateRequest 
  { "OtaUpdateId'" :: (OTAUpdateId)
  }


newtype GetOTAUpdateResponse = GetOTAUpdateResponse 
  { "OtaUpdateInfo'" :: NullOrUndefined (OTAUpdateInfo)
  }


-- | <p>The input for the GetPolicy operation.</p>
newtype GetPolicyRequest = GetPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  }


-- | <p>The output from the GetPolicy operation.</p>
newtype GetPolicyResponse = GetPolicyResponse 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "DefaultVersionId'" :: NullOrUndefined (PolicyVersionId)
  }


-- | <p>The input for the GetPolicyVersion operation.</p>
newtype GetPolicyVersionRequest = GetPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }


-- | <p>The output from the GetPolicyVersion operation.</p>
newtype GetPolicyVersionResponse = GetPolicyVersionResponse 
  { "PolicyArn'" :: NullOrUndefined (PolicyArn)
  , "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  }


-- | <p>The input to the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeRequest = GetRegistrationCodeRequest 
  { 
  }


-- | <p>The output from the GetRegistrationCode operation.</p>
newtype GetRegistrationCodeResponse = GetRegistrationCodeResponse 
  { "RegistrationCode'" :: NullOrUndefined (RegistrationCode)
  }


-- | <p>The input for the GetTopicRule operation.</p>
newtype GetTopicRuleRequest = GetTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  }


-- | <p>The output from the GetTopicRule operation.</p>
newtype GetTopicRuleResponse = GetTopicRuleResponse 
  { "RuleArn'" :: NullOrUndefined (RuleArn)
  , "Rule'" :: NullOrUndefined (TopicRule)
  }


newtype GetV2LoggingOptionsRequest = GetV2LoggingOptionsRequest 
  { 
  }


newtype GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs)
  }


-- | <p>The name and ARN of a group.</p>
newtype GroupNameAndArn = GroupNameAndArn 
  { "GroupName'" :: NullOrUndefined (ThingGroupName)
  , "GroupArn'" :: NullOrUndefined (ThingGroupArn)
  }


newtype HashAlgorithm = HashAlgorithm String


newtype HashKeyField = HashKeyField String


newtype HashKeyValue = HashKeyValue String


-- | <p>Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.</p>
newtype ImplicitDeny = ImplicitDeny 
  { "Policies'" :: NullOrUndefined (Policies)
  }


newtype InProgressThings = InProgressThings Int


newtype IndexName = IndexName String


newtype IndexNamesList = IndexNamesList (Array IndexName)


-- | <p>The index is not ready.</p>
newtype IndexNotReadyException = IndexNotReadyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype IndexSchema = IndexSchema String


newtype IndexStatus = IndexStatus String


newtype InlineDocument = InlineDocument String


-- | <p>An unexpected error has occurred.</p>
newtype InternalException = InternalException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>An unexpected error has occurred.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The query is invalid.</p>
newtype InvalidQueryException = InvalidQueryException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request is not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The response is invalid.</p>
newtype InvalidResponseException = InvalidResponseException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype IsAuthenticated = IsAuthenticated Boolean


newtype IsDefaultVersion = IsDefaultVersion Boolean


newtype IsDisabled = IsDisabled Boolean


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


newtype JobArn = JobArn String


newtype JobDescription = JobDescription String


newtype JobDocument = JobDocument String


newtype JobDocumentParameters = JobDocumentParameters (Map ParameterKey ParameterValue)


newtype JobDocumentSource = JobDocumentSource String


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


newtype JobExecutionStatus = JobExecutionStatus String


-- | <p>Details of the job execution status.</p>
newtype JobExecutionStatusDetails = JobExecutionStatusDetails 
  { "DetailsMap'" :: NullOrUndefined (DetailsMap)
  }


-- | <p>The job execution summary.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "QueuedAt'" :: NullOrUndefined (DateType)
  , "StartedAt'" :: NullOrUndefined (DateType)
  , "LastUpdatedAt'" :: NullOrUndefined (DateType)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }


-- | <p>Contains a summary of information about job executions for a specific job.</p>
newtype JobExecutionSummaryForJob = JobExecutionSummaryForJob 
  { "ThingArn'" :: NullOrUndefined (ThingArn)
  , "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary)
  }


newtype JobExecutionSummaryForJobList = JobExecutionSummaryForJobList (Array JobExecutionSummaryForJob)


-- | <p>The job execution summary for a thing.</p>
newtype JobExecutionSummaryForThing = JobExecutionSummaryForThing 
  { "JobId'" :: NullOrUndefined (JobId)
  , "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary)
  }


newtype JobExecutionSummaryForThingList = JobExecutionSummaryForThingList (Array JobExecutionSummaryForThing)


-- | <p>Allows you to create a staged rollout of a job.</p>
newtype JobExecutionsRolloutConfig = JobExecutionsRolloutConfig 
  { "MaximumPerMinute'" :: NullOrUndefined (MaxJobExecutionsPerMin)
  }


newtype JobId = JobId String


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


newtype JobStatus = JobStatus String


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


newtype JobSummaryList = JobSummaryList (Array JobSummary)


newtype JobTargets = JobTargets (Array TargetArn)


newtype JsonDocument = JsonDocument String


newtype Key = Key String


newtype KeyName = KeyName String


-- | <p>Describes a key pair.</p>
newtype KeyPair = KeyPair 
  { "PublicKey" :: NullOrUndefined (PublicKey)
  , "PrivateKey" :: NullOrUndefined (PrivateKey)
  }


newtype KeyValue = KeyValue String


-- | <p>Describes an action to write data to an Amazon Kinesis stream.</p>
newtype KinesisAction = KinesisAction 
  { "RoleArn'" :: (AwsArn)
  , "StreamName'" :: (StreamName)
  , "PartitionKey'" :: NullOrUndefined (PartitionKey)
  }


-- | <p>Describes an action to invoke a Lambda function.</p>
newtype LambdaAction = LambdaAction 
  { "FunctionArn'" :: (FunctionArn)
  }


newtype LaserMaxResults = LaserMaxResults Int


newtype LastModifiedDate = LastModifiedDate Number


-- | <p>The number of attached entities exceeds the limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype ListAttachedPoliciesRequest = ListAttachedPoliciesRequest 
  { "Target'" :: (PolicyTarget)
  , "Recursive'" :: NullOrUndefined (Recursive)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  }


newtype ListAttachedPoliciesResponse = ListAttachedPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


newtype ListAuthorizersRequest = ListAuthorizersRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }


newtype ListAuthorizersResponse = ListAuthorizersResponse 
  { "Authorizers'" :: NullOrUndefined (Authorizers)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>Input for the ListCACertificates operation.</p>
newtype ListCACertificatesRequest = ListCACertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output from the ListCACertificates operation.</p>
newtype ListCACertificatesResponse = ListCACertificatesResponse 
  { "Certificates'" :: NullOrUndefined (CACertificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input to the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCARequest = ListCertificatesByCARequest 
  { "CaCertificateId'" :: (CertificateId)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output of the ListCertificatesByCA operation.</p>
newtype ListCertificatesByCAResponse = ListCertificatesByCAResponse 
  { "Certificates'" :: NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input for the ListCertificates operation.</p>
newtype ListCertificatesRequest = ListCertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output of the ListCertificates operation.</p>
newtype ListCertificatesResponse = ListCertificatesResponse 
  { "Certificates'" :: NullOrUndefined (Certificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


newtype ListIndicesRequest = ListIndicesRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (QueryMaxResults)
  }


newtype ListIndicesResponse = ListIndicesResponse 
  { "IndexNames'" :: NullOrUndefined (IndexNamesList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListJobExecutionsForJobRequest = ListJobExecutionsForJobRequest 
  { "JobId'" :: (JobId)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse 
  { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForJobList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListJobExecutionsForThingRequest = ListJobExecutionsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse 
  { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForThingList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListJobsRequest = ListJobsRequest 
  { "Status'" :: NullOrUndefined (JobStatus)
  , "TargetSelection'" :: NullOrUndefined (TargetSelection)
  , "MaxResults'" :: NullOrUndefined (LaserMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupId'" :: NullOrUndefined (ThingGroupId)
  }


newtype ListJobsResponse = ListJobsResponse 
  { "Jobs'" :: NullOrUndefined (JobSummaryList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListOTAUpdatesRequest = ListOTAUpdatesRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus)
  }


newtype ListOTAUpdatesResponse = ListOTAUpdatesResponse 
  { "OtaUpdates'" :: NullOrUndefined (OTAUpdatesSummary)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>The input to the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesRequest = ListOutgoingCertificatesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output from the ListOutgoingCertificates operation.</p>
newtype ListOutgoingCertificatesResponse = ListOutgoingCertificatesResponse 
  { "OutgoingCertificates'" :: NullOrUndefined (OutgoingCertificates)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input for the ListPolicies operation.</p>
newtype ListPoliciesRequest = ListPoliciesRequest 
  { "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output from the ListPolicies operation.</p>
newtype ListPoliciesResponse = ListPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input for the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsRequest = ListPolicyPrincipalsRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output from the ListPolicyPrincipals operation.</p>
newtype ListPolicyPrincipalsResponse = ListPolicyPrincipalsResponse 
  { "Principals'" :: NullOrUndefined (Principals)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input for the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsRequest = ListPolicyVersionsRequest 
  { "PolicyName'" :: (PolicyName)
  }


-- | <p>The output from the ListPolicyVersions operation.</p>
newtype ListPolicyVersionsResponse = ListPolicyVersionsResponse 
  { "PolicyVersions'" :: NullOrUndefined (PolicyVersions)
  }


-- | <p>The input for the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesRequest = ListPrincipalPoliciesRequest 
  { "Principal'" :: (Principal)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


-- | <p>The output from the ListPrincipalPolicies operation.</p>
newtype ListPrincipalPoliciesResponse = ListPrincipalPoliciesResponse 
  { "Policies'" :: NullOrUndefined (Policies)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


-- | <p>The input for the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsRequest = ListPrincipalThingsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "Principal'" :: (Principal)
  }


-- | <p>The output from the ListPrincipalThings operation.</p>
newtype ListPrincipalThingsResponse = ListPrincipalThingsResponse 
  { "Things'" :: NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListRoleAliasesRequest = ListRoleAliasesRequest 
  { "PageSize'" :: NullOrUndefined (PageSize)
  , "Marker'" :: NullOrUndefined (Marker)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


newtype ListRoleAliasesResponse = ListRoleAliasesResponse 
  { "RoleAliases'" :: NullOrUndefined (RoleAliases)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


newtype ListStreamsRequest = ListStreamsRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "AscendingOrder'" :: NullOrUndefined (AscendingOrder)
  }


newtype ListStreamsResponse = ListStreamsResponse 
  { "Streams'" :: NullOrUndefined (StreamsSummary)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListTargetsForPolicyRequest = ListTargetsForPolicyRequest 
  { "PolicyName'" :: (PolicyName)
  , "Marker'" :: NullOrUndefined (Marker)
  , "PageSize'" :: NullOrUndefined (PageSize)
  }


newtype ListTargetsForPolicyResponse = ListTargetsForPolicyResponse 
  { "Targets'" :: NullOrUndefined (PolicyTargets)
  , "NextMarker'" :: NullOrUndefined (Marker)
  }


newtype ListThingGroupsForThingRequest = ListThingGroupsForThingRequest 
  { "ThingName'" :: (ThingName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }


newtype ListThingGroupsForThingResponse = ListThingGroupsForThingResponse 
  { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListThingGroupsRequest = ListThingGroupsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "ParentGroup'" :: NullOrUndefined (ThingGroupName)
  , "NamePrefixFilter'" :: NullOrUndefined (ThingGroupName)
  , "Recursive'" :: NullOrUndefined (RecursiveWithoutDefault)
  }


newtype ListThingGroupsResponse = ListThingGroupsResponse 
  { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>The input for the ListThingPrincipal operation.</p>
newtype ListThingPrincipalsRequest = ListThingPrincipalsRequest 
  { "ThingName'" :: (ThingName)
  }


-- | <p>The output from the ListThingPrincipals operation.</p>
newtype ListThingPrincipalsResponse = ListThingPrincipalsResponse 
  { "Principals'" :: NullOrUndefined (Principals)
  }


newtype ListThingRegistrationTaskReportsRequest = ListThingRegistrationTaskReportsRequest 
  { "TaskId'" :: (TaskId)
  , "ReportType'" :: (ReportType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }


newtype ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse 
  { "ResourceLinks'" :: NullOrUndefined (S3FileUrlList)
  , "ReportType'" :: NullOrUndefined (ReportType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListThingRegistrationTasksRequest = ListThingRegistrationTasksRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "Status'" :: NullOrUndefined (Status)
  }


newtype ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse 
  { "TaskIds'" :: NullOrUndefined (TaskIdList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>The input for the ListThingTypes operation.</p>
newtype ListThingTypesRequest = ListThingTypesRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  }


-- | <p>The output for the ListThingTypes operation.</p>
newtype ListThingTypesResponse = ListThingTypesResponse 
  { "ThingTypes'" :: NullOrUndefined (ThingTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListThingsInThingGroupRequest = ListThingsInThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "Recursive'" :: NullOrUndefined (Recursive)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  }


newtype ListThingsInThingGroupResponse = ListThingsInThingGroupResponse 
  { "Things'" :: NullOrUndefined (ThingNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>The input for the ListThings operation.</p>
newtype ListThingsRequest = ListThingsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (RegistryMaxResults)
  , "AttributeName'" :: NullOrUndefined (AttributeName)
  , "AttributeValue'" :: NullOrUndefined (AttributeValue)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  }


-- | <p>The output from the ListThings operation.</p>
newtype ListThingsResponse = ListThingsResponse 
  { "Things'" :: NullOrUndefined (ThingAttributeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>The input for the ListTopicRules operation.</p>
newtype ListTopicRulesRequest = ListTopicRulesRequest 
  { "Topic'" :: NullOrUndefined (Topic)
  , "MaxResults'" :: NullOrUndefined (GEMaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  }


-- | <p>The output from the ListTopicRules operation.</p>
newtype ListTopicRulesResponse = ListTopicRulesResponse 
  { "Rules'" :: NullOrUndefined (TopicRuleList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListV2LoggingLevelsRequest = ListV2LoggingLevelsRequest 
  { "TargetType'" :: NullOrUndefined (LogTargetType)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (SkyfallMaxResults)
  }


newtype ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse 
  { "LogTargetConfigurations'" :: NullOrUndefined (LogTargetConfigurations)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype LogLevel = LogLevel String


-- | <p>A log target.</p>
newtype LogTarget = LogTarget 
  { "TargetType'" :: (LogTargetType)
  , "TargetName'" :: NullOrUndefined (LogTargetName)
  }


-- | <p>The target configuration.</p>
newtype LogTargetConfiguration = LogTargetConfiguration 
  { "LogTarget'" :: NullOrUndefined (LogTarget)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }


newtype LogTargetConfigurations = LogTargetConfigurations (Array LogTargetConfiguration)


newtype LogTargetName = LogTargetName String


newtype LogTargetType = LogTargetType String


-- | <p>Describes the logging options payload.</p>
newtype LoggingOptionsPayload = LoggingOptionsPayload 
  { "RoleArn'" :: (AwsArn)
  , "LogLevel'" :: NullOrUndefined (LogLevel)
  }


-- | <p>The policy documentation is not valid.</p>
newtype MalformedPolicyException = MalformedPolicyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Marker = Marker String


newtype MaxJobExecutionsPerMin = MaxJobExecutionsPerMin Int


newtype MaxResults = MaxResults Int


newtype Message = Message String


newtype MessageFormat = MessageFormat String


newtype MetricName = MetricName String


newtype MetricNamespace = MetricNamespace String


newtype MetricTimestamp = MetricTimestamp String


newtype MetricUnit = MetricUnit String


newtype MetricValue = MetricValue String


newtype MissingContextValue = MissingContextValue String


newtype MissingContextValues = MissingContextValues (Array MissingContextValue)


newtype NextToken = NextToken String


-- | <p>The resource is not configured.</p>
newtype NotConfiguredException = NotConfiguredException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype OTAUpdateArn = OTAUpdateArn String


newtype OTAUpdateDescription = OTAUpdateDescription String


newtype OTAUpdateErrorMessage = OTAUpdateErrorMessage String


-- | <p>Describes a file to be associated with an OTA update.</p>
newtype OTAUpdateFile = OTAUpdateFile 
  { "FileName'" :: NullOrUndefined (FileName)
  , "FileVersion'" :: NullOrUndefined (OTAUpdateFileVersion)
  , "FileSource'" :: NullOrUndefined (Stream)
  , "CodeSigning'" :: NullOrUndefined (CodeSigning)
  , "Attributes'" :: NullOrUndefined (AttributesMap)
  }


newtype OTAUpdateFileVersion = OTAUpdateFileVersion String


newtype OTAUpdateFiles = OTAUpdateFiles (Array OTAUpdateFile)


newtype OTAUpdateId = OTAUpdateId String


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


newtype OTAUpdateStatus = OTAUpdateStatus String


-- | <p>An OTA update summary.</p>
newtype OTAUpdateSummary = OTAUpdateSummary 
  { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId)
  , "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }


newtype OTAUpdatesSummary = OTAUpdatesSummary (Array OTAUpdateSummary)


newtype OptionalVersion = OptionalVersion Number


-- | <p>A certificate that has been transferred but not yet accepted.</p>
newtype OutgoingCertificate = OutgoingCertificate 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  , "TransferredTo'" :: NullOrUndefined (AwsAccountId)
  , "TransferDate'" :: NullOrUndefined (DateType)
  , "TransferMessage'" :: NullOrUndefined (Message)
  , "CreationDate'" :: NullOrUndefined (DateType)
  }


newtype OutgoingCertificates = OutgoingCertificates (Array OutgoingCertificate)


newtype PageSize = PageSize Int


newtype Parameter = Parameter String


newtype ParameterKey = ParameterKey String


newtype ParameterValue = ParameterValue String


newtype Parameters = Parameters (Map Parameter Value)


newtype PartitionKey = PartitionKey String


newtype PayloadField = PayloadField String


newtype Percentage = Percentage Int


newtype Policies = Policies (Array Policy)


-- | <p>Describes an AWS IoT policy.</p>
newtype Policy = Policy 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyArn'" :: NullOrUndefined (PolicyArn)
  }


newtype PolicyArn = PolicyArn String


newtype PolicyDocument = PolicyDocument String


newtype PolicyDocuments = PolicyDocuments (Array PolicyDocument)


newtype PolicyName = PolicyName String


newtype PolicyNames = PolicyNames (Array PolicyName)


newtype PolicyTarget = PolicyTarget String


newtype PolicyTargets = PolicyTargets (Array PolicyTarget)


-- | <p>Describes a policy version.</p>
newtype PolicyVersion = PolicyVersion 
  { "VersionId'" :: NullOrUndefined (PolicyVersionId)
  , "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion)
  , "CreateDate'" :: NullOrUndefined (DateType)
  }


newtype PolicyVersionId = PolicyVersionId String


newtype PolicyVersions = PolicyVersions (Array PolicyVersion)


-- | <p>Configuration for pre-signed S3 URLs.</p>
newtype PresignedUrlConfig = PresignedUrlConfig 
  { "RoleArn'" :: NullOrUndefined (RoleArn)
  , "ExpiresInSec'" :: NullOrUndefined (ExpiresInSec)
  }


newtype Principal = Principal String


newtype PrincipalArn = PrincipalArn String


newtype PrincipalId = PrincipalId String


newtype Principals = Principals (Array PrincipalArn)


newtype PrivateKey = PrivateKey String


newtype ProcessingTargetName = ProcessingTargetName String


newtype ProcessingTargetNameList = ProcessingTargetNameList (Array ProcessingTargetName)


newtype PublicKey = PublicKey String


newtype PublicKeyMap = PublicKeyMap (Map KeyName KeyValue)


-- | <p>The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.</p>
newtype PutItemInput = PutItemInput 
  { "TableName'" :: (TableName)
  }


newtype QueryMaxResults = QueryMaxResults Int


newtype QueryString = QueryString String


newtype QueryVersion = QueryVersion String


newtype QueueUrl = QueueUrl String


newtype QueuedThings = QueuedThings Int


newtype RangeKeyField = RangeKeyField String


newtype RangeKeyValue = RangeKeyValue String


newtype Recursive = Recursive Boolean


newtype RecursiveWithoutDefault = RecursiveWithoutDefault Boolean


-- | <p>The input to the RegisterCACertificate operation.</p>
newtype RegisterCACertificateRequest = RegisterCACertificateRequest 
  { "CaCertificate'" :: (CertificatePem)
  , "VerificationCertificate'" :: (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined (SetAsActive)
  , "AllowAutoRegistration'" :: NullOrUndefined (AllowAutoRegistration)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  }


-- | <p>The output from the RegisterCACertificateResponse operation.</p>
newtype RegisterCACertificateResponse = RegisterCACertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  }


-- | <p>The input to the RegisterCertificate operation.</p>
newtype RegisterCertificateRequest = RegisterCertificateRequest 
  { "CertificatePem'" :: (CertificatePem)
  , "CaCertificatePem'" :: NullOrUndefined (CertificatePem)
  , "SetAsActive'" :: NullOrUndefined (SetAsActiveFlag)
  , "Status'" :: NullOrUndefined (CertificateStatus)
  }


-- | <p>The output from the RegisterCertificate operation.</p>
newtype RegisterCertificateResponse = RegisterCertificateResponse 
  { "CertificateArn'" :: NullOrUndefined (CertificateArn)
  , "CertificateId'" :: NullOrUndefined (CertificateId)
  }


newtype RegisterThingRequest = RegisterThingRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "Parameters'" :: NullOrUndefined (Parameters)
  }


newtype RegisterThingResponse = RegisterThingResponse 
  { "CertificatePem'" :: NullOrUndefined (CertificatePem)
  , "ResourceArns'" :: NullOrUndefined (ResourceArns)
  }


newtype RegistrationCode = RegistrationCode String


-- | <p>The registration code is invalid.</p>
newtype RegistrationCodeValidationException = RegistrationCodeValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The registration configuration.</p>
newtype RegistrationConfig = RegistrationConfig 
  { "TemplateBody'" :: NullOrUndefined (TemplateBody)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }


newtype RegistryMaxResults = RegistryMaxResults Int


newtype RegistryS3BucketName = RegistryS3BucketName String


newtype RegistryS3KeyName = RegistryS3KeyName String


-- | <p>The input for the RejectCertificateTransfer operation.</p>
newtype RejectCertificateTransferRequest = RejectCertificateTransferRequest 
  { "CertificateId'" :: (CertificateId)
  , "RejectReason'" :: NullOrUndefined (Message)
  }


newtype RejectedThings = RejectedThings Int


newtype RemoveAutoRegistration = RemoveAutoRegistration Boolean


newtype RemoveThingFromThingGroupRequest = RemoveThingFromThingGroupRequest 
  { "ThingGroupName'" :: NullOrUndefined (ThingGroupName)
  , "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  }


newtype RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse 
  { 
  }


newtype RemoveThingType = RemoveThingType Boolean


newtype RemovedThings = RemovedThings Int


-- | <p>The input for the ReplaceTopicRule operation.</p>
newtype ReplaceTopicRuleRequest = ReplaceTopicRuleRequest 
  { "RuleName'" :: (RuleName)
  , "TopicRulePayload'" :: (TopicRulePayload)
  }


newtype ReportType = ReportType String


-- | <p>Describes an action to republish to another topic.</p>
newtype RepublishAction = RepublishAction 
  { "RoleArn'" :: (AwsArn)
  , "Topic'" :: (TopicPattern)
  }


newtype Resource = Resource String


-- | <p>The resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  , "ResourceId'" :: NullOrUndefined (ResourceId')
  , "ResourceArn'" :: NullOrUndefined (ResourceArn')
  }


newtype ResourceArn = ResourceArn String


newtype ResourceArns = ResourceArns (Map ResourceLogicalId ResourceArn)


newtype ResourceLogicalId = ResourceLogicalId String


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The resource registration failed.</p>
newtype ResourceRegistrationFailureException = ResourceRegistrationFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Resources = Resources (Array Resource)


newtype RoleAlias = RoleAlias String


newtype RoleAliasArn = RoleAliasArn String


-- | <p>Role alias description.</p>
newtype RoleAliasDescription = RoleAliasDescription 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "Owner'" :: NullOrUndefined (AwsAccountId)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  , "CreationDate'" :: NullOrUndefined (DateType)
  , "LastModifiedDate'" :: NullOrUndefined (DateType)
  }


newtype RoleAliases = RoleAliases (Array RoleAlias)


newtype RoleArn = RoleArn String


newtype RuleArn = RuleArn String


newtype RuleName = RuleName String


-- | <p>Describes an action to write data to an Amazon S3 bucket.</p>
newtype S3Action = S3Action 
  { "RoleArn'" :: (AwsArn)
  , "BucketName'" :: (BucketName)
  , "Key'" :: (Key)
  , "CannedAcl'" :: NullOrUndefined (CannedAccessControlList)
  }


newtype S3Bucket = S3Bucket String


newtype S3FileUrl = S3FileUrl String


newtype S3FileUrlList = S3FileUrlList (Array S3FileUrl)


newtype S3Key = S3Key String


-- | <p>The location in S3 the contains the files to stream.</p>
newtype S3Location = S3Location 
  { "Bucket'" :: (S3Bucket)
  , "Key'" :: (S3Key)
  , "Version'" :: NullOrUndefined (S3Version)
  }


newtype S3Version = S3Version String


newtype SQL = SQL String


-- | <p>Describes an action to write a message to a Salesforce IoT Cloud Input Stream.</p>
newtype SalesforceAction = SalesforceAction 
  { "Token'" :: (SalesforceToken)
  , "Url'" :: (SalesforceEndpoint)
  }


newtype SalesforceEndpoint = SalesforceEndpoint String


newtype SalesforceToken = SalesforceToken String


newtype SearchIndexRequest = SearchIndexRequest 
  { "IndexName'" :: NullOrUndefined (IndexName)
  , "QueryString'" :: (QueryString)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (QueryMaxResults)
  , "QueryVersion'" :: NullOrUndefined (QueryVersion)
  }


newtype SearchIndexResponse = SearchIndexResponse 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "Things'" :: NullOrUndefined (ThingDocumentList)
  }


newtype SearchableAttributes = SearchableAttributes (Array AttributeName)


newtype Seconds = Seconds Int


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype SetAsActive = SetAsActive Boolean


newtype SetAsActiveFlag = SetAsActiveFlag Boolean


newtype SetAsDefault = SetAsDefault Boolean


newtype SetDefaultAuthorizerRequest = SetDefaultAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  }


newtype SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }


-- | <p>The input for the SetDefaultPolicyVersion operation.</p>
newtype SetDefaultPolicyVersionRequest = SetDefaultPolicyVersionRequest 
  { "PolicyName'" :: (PolicyName)
  , "PolicyVersionId'" :: (PolicyVersionId)
  }


-- | <p>The input for the SetLoggingOptions operation.</p>
newtype SetLoggingOptionsRequest = SetLoggingOptionsRequest 
  { "LoggingOptionsPayload'" :: (LoggingOptionsPayload)
  }


newtype SetV2LoggingLevelRequest = SetV2LoggingLevelRequest 
  { "LogTarget'" :: (LogTarget)
  , "LogLevel'" :: (LogLevel)
  }


newtype SetV2LoggingOptionsRequest = SetV2LoggingOptionsRequest 
  { "RoleArn'" :: NullOrUndefined (AwsArn)
  , "DefaultLogLevel'" :: NullOrUndefined (LogLevel)
  , "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs)
  }


newtype Signature = Signature String


newtype SignatureAlgorithm = SignatureAlgorithm String


newtype SigningJobId = SigningJobId String


newtype SkyfallMaxResults = SkyfallMaxResults Int


-- | <p>Describes an action to publish to an Amazon SNS topic.</p>
newtype SnsAction = SnsAction 
  { "TargetArn'" :: (AwsArn)
  , "RoleArn'" :: (AwsArn)
  , "MessageFormat'" :: NullOrUndefined (MessageFormat)
  }


-- | <p>The Rule-SQL expression can't be parsed correctly.</p>
newtype SqlParseException = SqlParseException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Describes an action to publish data to an Amazon SQS queue.</p>
newtype SqsAction = SqsAction 
  { "RoleArn'" :: (AwsArn)
  , "QueueUrl'" :: (QueueUrl)
  , "UseBase64'" :: NullOrUndefined (UseBase64)
  }


newtype StartThingRegistrationTaskRequest = StartThingRegistrationTaskRequest 
  { "TemplateBody'" :: (TemplateBody)
  , "InputFileBucket'" :: (RegistryS3BucketName)
  , "InputFileKey'" :: (RegistryS3KeyName)
  , "RoleArn'" :: (RoleArn)
  }


newtype StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse 
  { "TaskId'" :: NullOrUndefined (TaskId)
  }


newtype StateReason = StateReason String


newtype StateValue = StateValue String


newtype Status = Status String


newtype StopThingRegistrationTaskRequest = StopThingRegistrationTaskRequest 
  { "TaskId'" :: (TaskId)
  }


newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse 
  { 
  }


-- | <p>Describes a group of files that can be streamed.</p>
newtype Stream = Stream 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "FileId'" :: NullOrUndefined (FileId)
  }


newtype StreamArn = StreamArn String


newtype StreamDescription = StreamDescription String


-- | <p>Represents a file to stream.</p>
newtype StreamFile = StreamFile 
  { "FileId'" :: NullOrUndefined (FileId)
  , "S3Location'" :: NullOrUndefined (S3Location)
  }


newtype StreamFiles = StreamFiles (Array StreamFile)


newtype StreamId = StreamId String


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


newtype StreamName = StreamName String


-- | <p>A summary of a stream.</p>
newtype StreamSummary = StreamSummary 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  , "Description'" :: NullOrUndefined (StreamDescription)
  }


newtype StreamVersion = StreamVersion Int


newtype StreamsSummary = StreamsSummary (Array StreamSummary)


newtype SucceededThings = SucceededThings Int


newtype TableName = TableName String


newtype Target = Target String


newtype TargetArn = TargetArn String


newtype TargetSelection = TargetSelection String


newtype Targets = Targets (Array Target)


newtype TaskId = TaskId String


newtype TaskIdList = TaskIdList (Array TaskId)


newtype TemplateBody = TemplateBody String


newtype TestAuthorizationRequest = TestAuthorizationRequest 
  { "Principal'" :: NullOrUndefined (Principal)
  , "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId)
  , "AuthInfos'" :: (AuthInfos)
  , "ClientId'" :: NullOrUndefined (ClientId)
  , "PolicyNamesToAdd'" :: NullOrUndefined (PolicyNames)
  , "PolicyNamesToSkip'" :: NullOrUndefined (PolicyNames)
  }


newtype TestAuthorizationResponse = TestAuthorizationResponse 
  { "AuthResults'" :: NullOrUndefined (AuthResults)
  }


newtype TestInvokeAuthorizerRequest = TestInvokeAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "Token'" :: (Token)
  , "TokenSignature'" :: (TokenSignature)
  }


newtype TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse 
  { "IsAuthenticated'" :: NullOrUndefined (IsAuthenticated)
  , "PrincipalId'" :: NullOrUndefined (PrincipalId)
  , "PolicyDocuments'" :: NullOrUndefined (PolicyDocuments)
  , "RefreshAfterInSeconds'" :: NullOrUndefined (Seconds)
  , "DisconnectAfterInSeconds'" :: NullOrUndefined (Seconds)
  }


newtype ThingArn = ThingArn String


-- | <p>The properties of the thing, including thing name, thing type name, and a list of thing attributes.</p>
newtype ThingAttribute = ThingAttribute 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingArn'" :: NullOrUndefined (ThingArn)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "Version'" :: NullOrUndefined (Version)
  }


newtype ThingAttributeList = ThingAttributeList (Array ThingAttribute)


-- | <p>The thing search index document.</p>
newtype ThingDocument = ThingDocument 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingId'" :: NullOrUndefined (ThingId)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingGroupNames'" :: NullOrUndefined (ThingGroupNameList)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "Shadow'" :: NullOrUndefined (JsonDocument)
  }


newtype ThingDocumentList = ThingDocumentList (Array ThingDocument)


newtype ThingGroupArn = ThingGroupArn String


newtype ThingGroupDescription = ThingGroupDescription String


newtype ThingGroupId = ThingGroupId String


newtype ThingGroupList = ThingGroupList (Array ThingGroupName)


-- | <p>Thing group metadata.</p>
newtype ThingGroupMetadata = ThingGroupMetadata 
  { "ParentGroupName'" :: NullOrUndefined (ThingGroupName)
  , "RootToParentThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  }


newtype ThingGroupName = ThingGroupName String


newtype ThingGroupNameAndArnList = ThingGroupNameAndArnList (Array GroupNameAndArn)


newtype ThingGroupNameList = ThingGroupNameList (Array ThingGroupName)


-- | <p>Thing group properties.</p>
newtype ThingGroupProperties = ThingGroupProperties 
  { "ThingGroupDescription'" :: NullOrUndefined (ThingGroupDescription)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  }


newtype ThingId = ThingId String


-- | <p>Thing indexing configuration.</p>
newtype ThingIndexingConfiguration = ThingIndexingConfiguration 
  { "ThingIndexingMode'" :: NullOrUndefined (ThingIndexingMode)
  }


newtype ThingIndexingMode = ThingIndexingMode String


newtype ThingName = ThingName String


newtype ThingNameList = ThingNameList (Array ThingName)


newtype ThingTypeArn = ThingTypeArn String


-- | <p>The definition of the thing type, including thing type name and description.</p>
newtype ThingTypeDefinition = ThingTypeDefinition 
  { "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn)
  , "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties)
  , "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata)
  }


newtype ThingTypeDescription = ThingTypeDescription String


newtype ThingTypeId = ThingTypeId String


newtype ThingTypeList = ThingTypeList (Array ThingTypeDefinition)


-- | <p>The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.</p>
newtype ThingTypeMetadata = ThingTypeMetadata 
  { "Deprecated'" :: NullOrUndefined (Boolean)
  , "DeprecationDate'" :: NullOrUndefined (DeprecationDate)
  , "CreationDate'" :: NullOrUndefined (CreationDate)
  }


newtype ThingTypeName = ThingTypeName String


-- | <p>The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.</p>
newtype ThingTypeProperties = ThingTypeProperties 
  { "ThingTypeDescription'" :: NullOrUndefined (ThingTypeDescription)
  , "SearchableAttributes'" :: NullOrUndefined (SearchableAttributes)
  }


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Token = Token String


newtype TokenKeyName = TokenKeyName String


newtype TokenSignature = TokenSignature String


newtype Topic = Topic String


newtype TopicPattern = TopicPattern String


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


newtype TopicRuleList = TopicRuleList (Array TopicRuleListItem)


-- | <p>Describes a rule.</p>
newtype TopicRuleListItem = TopicRuleListItem 
  { "RuleArn'" :: NullOrUndefined (RuleArn)
  , "RuleName'" :: NullOrUndefined (RuleName)
  , "TopicPattern'" :: NullOrUndefined (TopicPattern)
  , "CreatedAt'" :: NullOrUndefined (CreatedAtDate)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  }


-- | <p>Describes a rule.</p>
newtype TopicRulePayload = TopicRulePayload 
  { "Sql'" :: (SQL)
  , "Description'" :: NullOrUndefined (Description)
  , "Actions'" :: (ActionList)
  , "RuleDisabled'" :: NullOrUndefined (IsDisabled)
  , "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion)
  , "ErrorAction'" :: NullOrUndefined (Action)
  }


-- | <p>You can't revert the certificate transfer because the transfer is already complete.</p>
newtype TransferAlreadyCompletedException = TransferAlreadyCompletedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The input for the TransferCertificate operation.</p>
newtype TransferCertificateRequest = TransferCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "TargetAwsAccount'" :: (AwsAccountId)
  , "TransferMessage'" :: NullOrUndefined (Message)
  }


-- | <p>The output from the TransferCertificate operation.</p>
newtype TransferCertificateResponse = TransferCertificateResponse 
  { "TransferredCertificateArn'" :: NullOrUndefined (CertificateArn)
  }


-- | <p>You can't transfer the certificate because authorization policies are still attached.</p>
newtype TransferConflictException = TransferConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Data used to transfer a certificate to an AWS account.</p>
newtype TransferData = TransferData 
  { "TransferMessage'" :: NullOrUndefined (Message)
  , "RejectReason'" :: NullOrUndefined (Message)
  , "TransferDate'" :: NullOrUndefined (DateType)
  , "AcceptDate'" :: NullOrUndefined (DateType)
  , "RejectDate'" :: NullOrUndefined (DateType)
  }


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype UndoDeprecate = UndoDeprecate Boolean


newtype UpdateAuthorizerRequest = UpdateAuthorizerRequest 
  { "AuthorizerName'" :: (AuthorizerName)
  , "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn)
  , "TokenKeyName'" :: NullOrUndefined (TokenKeyName)
  , "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap)
  , "Status'" :: NullOrUndefined (AuthorizerStatus)
  }


newtype UpdateAuthorizerResponse = UpdateAuthorizerResponse 
  { "AuthorizerName'" :: NullOrUndefined (AuthorizerName)
  , "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn)
  }


-- | <p>The input to the UpdateCACertificate operation.</p>
newtype UpdateCACertificateRequest = UpdateCACertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: NullOrUndefined (CACertificateStatus)
  , "NewAutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus)
  , "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig)
  , "RemoveAutoRegistration'" :: NullOrUndefined (RemoveAutoRegistration)
  }


-- | <p>The input for the UpdateCertificate operation.</p>
newtype UpdateCertificateRequest = UpdateCertificateRequest 
  { "CertificateId'" :: (CertificateId)
  , "NewStatus'" :: (CertificateStatus)
  }


newtype UpdateEventConfigurationsRequest = UpdateEventConfigurationsRequest 
  { "EventConfigurations'" :: NullOrUndefined (EventConfigurations)
  }


newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse 
  { 
  }


newtype UpdateIndexingConfigurationRequest = UpdateIndexingConfigurationRequest 
  { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration)
  }


newtype UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse 
  { 
  }


newtype UpdateRoleAliasRequest = UpdateRoleAliasRequest 
  { "RoleAlias'" :: (RoleAlias)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds)
  }


newtype UpdateRoleAliasResponse = UpdateRoleAliasResponse 
  { "RoleAlias'" :: NullOrUndefined (RoleAlias)
  , "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn)
  }


newtype UpdateStreamRequest = UpdateStreamRequest 
  { "StreamId'" :: (StreamId)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "Files'" :: NullOrUndefined (StreamFiles)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }


newtype UpdateStreamResponse = UpdateStreamResponse 
  { "StreamId'" :: NullOrUndefined (StreamId)
  , "StreamArn'" :: NullOrUndefined (StreamArn)
  , "Description'" :: NullOrUndefined (StreamDescription)
  , "StreamVersion'" :: NullOrUndefined (StreamVersion)
  }


newtype UpdateThingGroupRequest = UpdateThingGroupRequest 
  { "ThingGroupName'" :: (ThingGroupName)
  , "ThingGroupProperties'" :: (ThingGroupProperties)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  }


newtype UpdateThingGroupResponse = UpdateThingGroupResponse 
  { "Version'" :: NullOrUndefined (Version)
  }


newtype UpdateThingGroupsForThingRequest = UpdateThingGroupsForThingRequest 
  { "ThingName'" :: NullOrUndefined (ThingName)
  , "ThingGroupsToAdd'" :: NullOrUndefined (ThingGroupList)
  , "ThingGroupsToRemove'" :: NullOrUndefined (ThingGroupList)
  }


newtype UpdateThingGroupsForThingResponse = UpdateThingGroupsForThingResponse 
  { 
  }


-- | <p>The input for the UpdateThing operation.</p>
newtype UpdateThingRequest = UpdateThingRequest 
  { "ThingName'" :: (ThingName)
  , "ThingTypeName'" :: NullOrUndefined (ThingTypeName)
  , "AttributePayload'" :: NullOrUndefined (AttributePayload)
  , "ExpectedVersion'" :: NullOrUndefined (OptionalVersion)
  , "RemoveThingType'" :: NullOrUndefined (RemoveThingType)
  }


-- | <p>The output from the UpdateThing operation.</p>
newtype UpdateThingResponse = UpdateThingResponse 
  { 
  }


newtype UseBase64 = UseBase64 Boolean


newtype Value = Value String


newtype Version = Version Number


-- | <p>An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.</p>
newtype VersionConflictException = VersionConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The number of policy versions exceeds the limit.</p>
newtype VersionsLimitExceededException = VersionsLimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype ErrorMessage' = ErrorMessage' String


newtype ResourceArn' = ResourceArn' String


newtype ResourceId' = ResourceId' String
