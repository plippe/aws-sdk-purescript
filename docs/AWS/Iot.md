## Module AWS.Iot

<fullname>AWS IoT</fullname> <p>AWS IoT provides secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each thing (Thing Registry), configure logging, and create and manage policies and credentials to authenticate things.</p> <p>For more information about how AWS IoT works, see the <a href="http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html">Developer Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptCertificateTransfer`

``` purescript
acceptCertificateTransfer :: forall eff. AcceptCertificateTransferRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p>

#### `addThingToThingGroup`

``` purescript
addThingToThingGroup :: forall eff. AddThingToThingGroupRequest -> Aff (err :: RequestError | eff) AddThingToThingGroupResponse
```

<p>Adds a thing to a thing group.</p>

#### `associateTargetsWithJob`

``` purescript
associateTargetsWithJob :: forall eff. AssociateTargetsWithJobRequest -> Aff (err :: RequestError | eff) AssociateTargetsWithJobResponse
```

<p>Associates a group with a continuous job. The following criteria must be met: </p> <ul> <li> <p>The job must have been created with the <code>targetSelection</code> field set to "CONTINUOUS".</p> </li> <li> <p>The job status must currently be "IN_PROGRESS".</p> </li> <li> <p>The total number of targets associated with a job must not exceed 100.</p> </li> </ul>

#### `attachPolicy`

``` purescript
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches a policy to the specified target.</p>

#### `attachPrincipalPolicy`

``` purescript
attachPrincipalPolicy :: forall eff. AttachPrincipalPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches the specified policy to the specified principal (certificate or other credential).</p> <p> <b>Note:</b> This API is deprecated. Please use <a>AttachPolicy</a> instead.</p>

#### `attachThingPrincipal`

``` purescript
attachThingPrincipal :: forall eff. AttachThingPrincipalRequest -> Aff (err :: RequestError | eff) AttachThingPrincipalResponse
```

<p>Attaches the specified principal to the specified thing.</p>

#### `cancelCertificateTransfer`

``` purescript
cancelCertificateTransfer :: forall eff. CancelCertificateTransferRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels a pending transfer for the specified certificate.</p> <p> <b>Note</b> Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use <a>RejectCertificateTransfer</a> instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.</p> <p>After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.</p>

#### `cancelJob`

``` purescript
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: RequestError | eff) CancelJobResponse
```

<p>Cancels a job.</p>

#### `clearDefaultAuthorizer`

``` purescript
clearDefaultAuthorizer :: forall eff. ClearDefaultAuthorizerRequest -> Aff (err :: RequestError | eff) ClearDefaultAuthorizerResponse
```

<p>Clears the default authorizer.</p>

#### `createAuthorizer`

``` purescript
createAuthorizer :: forall eff. CreateAuthorizerRequest -> Aff (err :: RequestError | eff) CreateAuthorizerResponse
```

<p>Creates an authorizer.</p>

#### `createCertificateFromCsr`

``` purescript
createCertificateFromCsr :: forall eff. CreateCertificateFromCsrRequest -> Aff (err :: RequestError | eff) CreateCertificateFromCsrResponse
```

<p>Creates an X.509 certificate using the specified certificate signing request.</p> <p> <b>Note:</b> The CSR must include a public key that is either an RSA key with a length of at least 2048 bits or an ECC key from NIST P-256 or NIST P-384 curves. </p> <p> <b>Note:</b> Reusing the same certificate signing request (CSR) results in a distinct certificate.</p> <p>You can create multiple certificates in a batch by creating a directory, copying multiple .csr files into that directory, and then specifying that directory on the command line. The following commands show how to create a batch of certificates given a batch of CSRs.</p> <p>Assuming a set of CSRs are located inside of the directory my-csr-directory:</p> <p>On Linux and OS X, the command is:</p> <p>$ ls my-csr-directory/ | xargs -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>This command lists all of the CSRs in my-csr-directory and pipes each CSR file name to the aws iot create-certificate-from-csr AWS CLI command to create a certificate for the corresponding CSR.</p> <p>The aws iot create-certificate-from-csr part of the command can also be run in parallel to speed up the certificate creation process:</p> <p>$ ls my-csr-directory/ | xargs -P 10 -I {} aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/{}</p> <p>On Windows PowerShell, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; ls -Name my-csr-directory | %{aws iot create-certificate-from-csr --certificate-signing-request file://my-csr-directory/$_}</p> <p>On a Windows command prompt, the command to create certificates for all CSRs in my-csr-directory is:</p> <p>&gt; forfiles /p my-csr-directory /c "cmd /c aws iot create-certificate-from-csr --certificate-signing-request file://@path"</p>

#### `createJob`

``` purescript
createJob :: forall eff. CreateJobRequest -> Aff (err :: RequestError | eff) CreateJobResponse
```

<p>Creates a job.</p>

#### `createKeysAndCertificate`

``` purescript
createKeysAndCertificate :: forall eff. CreateKeysAndCertificateRequest -> Aff (err :: RequestError | eff) CreateKeysAndCertificateResponse
```

<p>Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key.</p> <p> <b>Note</b> This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.</p>

#### `createOTAUpdate`

``` purescript
createOTAUpdate :: forall eff. CreateOTAUpdateRequest -> Aff (err :: RequestError | eff) CreateOTAUpdateResponse
```

<p>Creates an AWS IoT OTAUpdate on a target group of things or groups.</p>

#### `createPolicy`

``` purescript
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: RequestError | eff) CreatePolicyResponse
```

<p>Creates an AWS IoT policy.</p> <p>The created policy is the default version for the policy. This operation creates a policy version with a version identifier of <b>1</b> and sets <b>1</b> as the policy's default version.</p>

#### `createPolicyVersion`

``` purescript
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (err :: RequestError | eff) CreatePolicyVersionResponse
```

<p>Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use <a>DeletePolicyVersion</a> to delete an existing version before you create a new one.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).</p>

#### `createRoleAlias`

``` purescript
createRoleAlias :: forall eff. CreateRoleAliasRequest -> Aff (err :: RequestError | eff) CreateRoleAliasResponse
```

<p>Creates a role alias.</p>

#### `createStream`

``` purescript
createStream :: forall eff. CreateStreamRequest -> Aff (err :: RequestError | eff) CreateStreamResponse
```

<p>Creates a stream for delivering one or more large files in chunks over MQTT. A stream transports data bytes in chunks or blocks packaged as MQTT messages from a source like S3. You can have one or more files associated with a stream. The total size of a file associated with the stream cannot exceed more than 2 MB. The stream will be created with version 0. If a stream is created with the same streamID as a stream that existed and was deleted within last 90 days, we will resurrect that old stream by incrementing the version by 1.</p>

#### `createThing`

``` purescript
createThing :: forall eff. CreateThingRequest -> Aff (err :: RequestError | eff) CreateThingResponse
```

<p>Creates a thing record in the thing registry.</p>

#### `createThingGroup`

``` purescript
createThingGroup :: forall eff. CreateThingGroupRequest -> Aff (err :: RequestError | eff) CreateThingGroupResponse
```

<p>Create a thing group.</p>

#### `createThingType`

``` purescript
createThingType :: forall eff. CreateThingTypeRequest -> Aff (err :: RequestError | eff) CreateThingTypeResponse
```

<p>Creates a new thing type.</p>

#### `createTopicRule`

``` purescript
createTopicRule :: forall eff. CreateTopicRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>

#### `deleteAuthorizer`

``` purescript
deleteAuthorizer :: forall eff. DeleteAuthorizerRequest -> Aff (err :: RequestError | eff) DeleteAuthorizerResponse
```

<p>Deletes an authorizer.</p>

#### `deleteCACertificate`

``` purescript
deleteCACertificate :: forall eff. DeleteCACertificateRequest -> Aff (err :: RequestError | eff) DeleteCACertificateResponse
```

<p>Deletes a registered CA certificate.</p>

#### `deleteCertificate`

``` purescript
deleteCertificate :: forall eff. DeleteCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified certificate.</p> <p>A certificate cannot be deleted if it has a policy attached to it or if its status is set to ACTIVE. To delete a certificate, first use the <a>DetachPrincipalPolicy</a> API to detach all policies. Next, use the <a>UpdateCertificate</a> API to set the certificate to the INACTIVE status.</p>

#### `deleteOTAUpdate`

``` purescript
deleteOTAUpdate :: forall eff. DeleteOTAUpdateRequest -> Aff (err :: RequestError | eff) DeleteOTAUpdateResponse
```

<p>Delete an OTA update.</p>

#### `deletePolicy`

``` purescript
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified policy.</p> <p>A policy cannot be deleted if it has non-default versions or it is attached to any certificate.</p> <p>To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.</p> <p>When a policy is deleted using DeletePolicy, its default version is deleted with it.</p>

#### `deletePolicyVersion`

``` purescript
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use ListPolicyVersions.</p>

#### `deleteRegistrationCode`

``` purescript
deleteRegistrationCode :: forall eff. DeleteRegistrationCodeRequest -> Aff (err :: RequestError | eff) DeleteRegistrationCodeResponse
```

<p>Deletes a CA certificate registration code.</p>

#### `deleteRoleAlias`

``` purescript
deleteRoleAlias :: forall eff. DeleteRoleAliasRequest -> Aff (err :: RequestError | eff) DeleteRoleAliasResponse
```

<p>Deletes a role alias</p>

#### `deleteStream`

``` purescript
deleteStream :: forall eff. DeleteStreamRequest -> Aff (err :: RequestError | eff) DeleteStreamResponse
```

<p>Deletes a stream.</p>

#### `deleteThing`

``` purescript
deleteThing :: forall eff. DeleteThingRequest -> Aff (err :: RequestError | eff) DeleteThingResponse
```

<p>Deletes the specified thing.</p>

#### `deleteThingGroup`

``` purescript
deleteThingGroup :: forall eff. DeleteThingGroupRequest -> Aff (err :: RequestError | eff) DeleteThingGroupResponse
```

<p>Deletes a thing group.</p>

#### `deleteThingType`

``` purescript
deleteThingType :: forall eff. DeleteThingTypeRequest -> Aff (err :: RequestError | eff) DeleteThingTypeResponse
```

<p>Deletes the specified thing type . You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling <a>DeprecateThingType</a>, then remove any associated things by calling <a>UpdateThing</a> to change the thing type on any associated thing, and finally use <a>DeleteThingType</a> to delete the thing type.</p>

#### `deleteTopicRule`

``` purescript
deleteTopicRule :: forall eff. DeleteTopicRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the rule.</p>

#### `deleteV2LoggingLevel`

``` purescript
deleteV2LoggingLevel :: forall eff. DeleteV2LoggingLevelRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a logging level.</p>

#### `deprecateThingType`

``` purescript
deprecateThingType :: forall eff. DeprecateThingTypeRequest -> Aff (err :: RequestError | eff) DeprecateThingTypeResponse
```

<p>Deprecates a thing type. You can not associate new things with deprecated thing type.</p>

#### `describeAuthorizer`

``` purescript
describeAuthorizer :: forall eff. DescribeAuthorizerRequest -> Aff (err :: RequestError | eff) DescribeAuthorizerResponse
```

<p>Describes an authorizer.</p>

#### `describeCACertificate`

``` purescript
describeCACertificate :: forall eff. DescribeCACertificateRequest -> Aff (err :: RequestError | eff) DescribeCACertificateResponse
```

<p>Describes a registered CA certificate.</p>

#### `describeCertificate`

``` purescript
describeCertificate :: forall eff. DescribeCertificateRequest -> Aff (err :: RequestError | eff) DescribeCertificateResponse
```

<p>Gets information about the specified certificate.</p>

#### `describeDefaultAuthorizer`

``` purescript
describeDefaultAuthorizer :: forall eff. DescribeDefaultAuthorizerRequest -> Aff (err :: RequestError | eff) DescribeDefaultAuthorizerResponse
```

<p>Describes the default authorizer.</p>

#### `describeEndpoint`

``` purescript
describeEndpoint :: forall eff. DescribeEndpointRequest -> Aff (err :: RequestError | eff) DescribeEndpointResponse
```

<p>Returns a unique endpoint specific to the AWS account making the call.</p>

#### `describeEventConfigurations`

``` purescript
describeEventConfigurations :: forall eff. DescribeEventConfigurationsRequest -> Aff (err :: RequestError | eff) DescribeEventConfigurationsResponse
```

<p>Describes event configurations.</p>

#### `describeIndex`

``` purescript
describeIndex :: forall eff. DescribeIndexRequest -> Aff (err :: RequestError | eff) DescribeIndexResponse
```

<p>Describes a search index.</p>

#### `describeJob`

``` purescript
describeJob :: forall eff. DescribeJobRequest -> Aff (err :: RequestError | eff) DescribeJobResponse
```

<p>Describes a job.</p>

#### `describeJobExecution`

``` purescript
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (err :: RequestError | eff) DescribeJobExecutionResponse
```

<p>Describes a job execution.</p>

#### `describeRoleAlias`

``` purescript
describeRoleAlias :: forall eff. DescribeRoleAliasRequest -> Aff (err :: RequestError | eff) DescribeRoleAliasResponse
```

<p>Describes a role alias.</p>

#### `describeStream`

``` purescript
describeStream :: forall eff. DescribeStreamRequest -> Aff (err :: RequestError | eff) DescribeStreamResponse
```

<p>Gets information about a stream.</p>

#### `describeThing`

``` purescript
describeThing :: forall eff. DescribeThingRequest -> Aff (err :: RequestError | eff) DescribeThingResponse
```

<p>Gets information about the specified thing.</p>

#### `describeThingGroup`

``` purescript
describeThingGroup :: forall eff. DescribeThingGroupRequest -> Aff (err :: RequestError | eff) DescribeThingGroupResponse
```

<p>Describe a thing group.</p>

#### `describeThingRegistrationTask`

``` purescript
describeThingRegistrationTask :: forall eff. DescribeThingRegistrationTaskRequest -> Aff (err :: RequestError | eff) DescribeThingRegistrationTaskResponse
```

<p>Describes a bulk thing provisioning task.</p>

#### `describeThingType`

``` purescript
describeThingType :: forall eff. DescribeThingTypeRequest -> Aff (err :: RequestError | eff) DescribeThingTypeResponse
```

<p>Gets information about the specified thing type.</p>

#### `detachPolicy`

``` purescript
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a policy from the specified target.</p>

#### `detachPrincipalPolicy`

``` purescript
detachPrincipalPolicy :: forall eff. DetachPrincipalPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified policy from the specified certificate.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>DetachPolicy</a> instead.</p>

#### `detachThingPrincipal`

``` purescript
detachThingPrincipal :: forall eff. DetachThingPrincipalRequest -> Aff (err :: RequestError | eff) DetachThingPrincipalResponse
```

<p>Detaches the specified principal from the specified thing.</p>

#### `disableTopicRule`

``` purescript
disableTopicRule :: forall eff. DisableTopicRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disables the rule.</p>

#### `enableTopicRule`

``` purescript
enableTopicRule :: forall eff. EnableTopicRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables the rule.</p>

#### `getEffectivePolicies`

``` purescript
getEffectivePolicies :: forall eff. GetEffectivePoliciesRequest -> Aff (err :: RequestError | eff) GetEffectivePoliciesResponse
```

<p>Gets effective policies.</p>

#### `getIndexingConfiguration`

``` purescript
getIndexingConfiguration :: forall eff. GetIndexingConfigurationRequest -> Aff (err :: RequestError | eff) GetIndexingConfigurationResponse
```

<p>Gets the search configuration.</p>

#### `getJobDocument`

``` purescript
getJobDocument :: forall eff. GetJobDocumentRequest -> Aff (err :: RequestError | eff) GetJobDocumentResponse
```

<p>Gets a job document.</p>

#### `getLoggingOptions`

``` purescript
getLoggingOptions :: forall eff. GetLoggingOptionsRequest -> Aff (err :: RequestError | eff) GetLoggingOptionsResponse
```

<p>Gets the logging options.</p>

#### `getOTAUpdate`

``` purescript
getOTAUpdate :: forall eff. GetOTAUpdateRequest -> Aff (err :: RequestError | eff) GetOTAUpdateResponse
```

<p>Gets an OTA update.</p>

#### `getPolicy`

``` purescript
getPolicy :: forall eff. GetPolicyRequest -> Aff (err :: RequestError | eff) GetPolicyResponse
```

<p>Gets information about the specified policy with the policy document of the default version.</p>

#### `getPolicyVersion`

``` purescript
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (err :: RequestError | eff) GetPolicyVersionResponse
```

<p>Gets information about the specified policy version.</p>

#### `getRegistrationCode`

``` purescript
getRegistrationCode :: forall eff. GetRegistrationCodeRequest -> Aff (err :: RequestError | eff) GetRegistrationCodeResponse
```

<p>Gets a registration code used to register a CA certificate with AWS IoT.</p>

#### `getTopicRule`

``` purescript
getTopicRule :: forall eff. GetTopicRuleRequest -> Aff (err :: RequestError | eff) GetTopicRuleResponse
```

<p>Gets information about the rule.</p>

#### `getV2LoggingOptions`

``` purescript
getV2LoggingOptions :: forall eff. GetV2LoggingOptionsRequest -> Aff (err :: RequestError | eff) GetV2LoggingOptionsResponse
```

<p>Gets the fine grained logging options.</p>

#### `listAttachedPolicies`

``` purescript
listAttachedPolicies :: forall eff. ListAttachedPoliciesRequest -> Aff (err :: RequestError | eff) ListAttachedPoliciesResponse
```

<p>Lists the policies attached to the specified thing group.</p>

#### `listAuthorizers`

``` purescript
listAuthorizers :: forall eff. ListAuthorizersRequest -> Aff (err :: RequestError | eff) ListAuthorizersResponse
```

<p>Lists the authorizers registered in your account.</p>

#### `listCACertificates`

``` purescript
listCACertificates :: forall eff. ListCACertificatesRequest -> Aff (err :: RequestError | eff) ListCACertificatesResponse
```

<p>Lists the CA certificates registered for your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>

#### `listCertificates`

``` purescript
listCertificates :: forall eff. ListCertificatesRequest -> Aff (err :: RequestError | eff) ListCertificatesResponse
```

<p>Lists the certificates registered in your AWS account.</p> <p>The results are paginated with a default page size of 25. You can use the returned marker to retrieve additional results.</p>

#### `listCertificatesByCA`

``` purescript
listCertificatesByCA :: forall eff. ListCertificatesByCARequest -> Aff (err :: RequestError | eff) ListCertificatesByCAResponse
```

<p>List the device certificates signed by the specified CA certificate.</p>

#### `listIndices`

``` purescript
listIndices :: forall eff. ListIndicesRequest -> Aff (err :: RequestError | eff) ListIndicesResponse
```

<p>Lists the search indices.</p>

#### `listJobExecutionsForJob`

``` purescript
listJobExecutionsForJob :: forall eff. ListJobExecutionsForJobRequest -> Aff (err :: RequestError | eff) ListJobExecutionsForJobResponse
```

<p>Lists the job executions for a job.</p>

#### `listJobExecutionsForThing`

``` purescript
listJobExecutionsForThing :: forall eff. ListJobExecutionsForThingRequest -> Aff (err :: RequestError | eff) ListJobExecutionsForThingResponse
```

<p>Lists the job executions for the specified thing.</p>

#### `listJobs`

``` purescript
listJobs :: forall eff. ListJobsRequest -> Aff (err :: RequestError | eff) ListJobsResponse
```

<p>Lists jobs.</p>

#### `listOTAUpdates`

``` purescript
listOTAUpdates :: forall eff. ListOTAUpdatesRequest -> Aff (err :: RequestError | eff) ListOTAUpdatesResponse
```

<p>Lists OTA updates.</p>

#### `listOutgoingCertificates`

``` purescript
listOutgoingCertificates :: forall eff. ListOutgoingCertificatesRequest -> Aff (err :: RequestError | eff) ListOutgoingCertificatesResponse
```

<p>Lists certificates that are being transferred but not yet accepted.</p>

#### `listPolicies`

``` purescript
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: RequestError | eff) ListPoliciesResponse
```

<p>Lists your policies.</p>

#### `listPolicyPrincipals`

``` purescript
listPolicyPrincipals :: forall eff. ListPolicyPrincipalsRequest -> Aff (err :: RequestError | eff) ListPolicyPrincipalsResponse
```

<p>Lists the principals associated with the specified policy.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListTargetsForPolicy</a> instead.</p>

#### `listPolicyVersions`

``` purescript
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (err :: RequestError | eff) ListPolicyVersionsResponse
```

<p>Lists the versions of the specified policy and identifies the default version.</p>

#### `listPrincipalPolicies`

``` purescript
listPrincipalPolicies :: forall eff. ListPrincipalPoliciesRequest -> Aff (err :: RequestError | eff) ListPrincipalPoliciesResponse
```

<p>Lists the policies attached to the specified principal. If you use an Cognito identity, the ID must be in <a href="http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html#API_GetCredentialsForIdentity_RequestSyntax">AmazonCognito Identity format</a>.</p> <p> <b>Note:</b> This API is deprecated. Please use <a>ListAttachedPolicies</a> instead.</p>

#### `listPrincipalThings`

``` purescript
listPrincipalThings :: forall eff. ListPrincipalThingsRequest -> Aff (err :: RequestError | eff) ListPrincipalThingsResponse
```

<p>Lists the things associated with the specified principal.</p>

#### `listRoleAliases`

``` purescript
listRoleAliases :: forall eff. ListRoleAliasesRequest -> Aff (err :: RequestError | eff) ListRoleAliasesResponse
```

<p>Lists the role aliases registered in your account.</p>

#### `listStreams`

``` purescript
listStreams :: forall eff. ListStreamsRequest -> Aff (err :: RequestError | eff) ListStreamsResponse
```

<p>Lists all of the streams in your AWS account.</p>

#### `listTargetsForPolicy`

``` purescript
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (err :: RequestError | eff) ListTargetsForPolicyResponse
```

<p>List targets for the specified policy.</p>

#### `listThingGroups`

``` purescript
listThingGroups :: forall eff. ListThingGroupsRequest -> Aff (err :: RequestError | eff) ListThingGroupsResponse
```

<p>List the thing groups in your account.</p>

#### `listThingGroupsForThing`

``` purescript
listThingGroupsForThing :: forall eff. ListThingGroupsForThingRequest -> Aff (err :: RequestError | eff) ListThingGroupsForThingResponse
```

<p>List the thing groups to which the specified thing belongs.</p>

#### `listThingPrincipals`

``` purescript
listThingPrincipals :: forall eff. ListThingPrincipalsRequest -> Aff (err :: RequestError | eff) ListThingPrincipalsResponse
```

<p>Lists the principals associated with the specified thing.</p>

#### `listThingRegistrationTaskReports`

``` purescript
listThingRegistrationTaskReports :: forall eff. ListThingRegistrationTaskReportsRequest -> Aff (err :: RequestError | eff) ListThingRegistrationTaskReportsResponse
```

<p>Information about the thing registration tasks.</p>

#### `listThingRegistrationTasks`

``` purescript
listThingRegistrationTasks :: forall eff. ListThingRegistrationTasksRequest -> Aff (err :: RequestError | eff) ListThingRegistrationTasksResponse
```

<p>List bulk thing provisioning tasks.</p>

#### `listThingTypes`

``` purescript
listThingTypes :: forall eff. ListThingTypesRequest -> Aff (err :: RequestError | eff) ListThingTypesResponse
```

<p>Lists the existing thing types.</p>

#### `listThings`

``` purescript
listThings :: forall eff. ListThingsRequest -> Aff (err :: RequestError | eff) ListThingsResponse
```

<p>Lists your things. Use the <b>attributeName</b> and <b>attributeValue</b> parameters to filter your things. For example, calling <code>ListThings</code> with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute <b>Color</b> with the value <b>Red</b>. </p>

#### `listThingsInThingGroup`

``` purescript
listThingsInThingGroup :: forall eff. ListThingsInThingGroupRequest -> Aff (err :: RequestError | eff) ListThingsInThingGroupResponse
```

<p>Lists the things in the specified group.</p>

#### `listTopicRules`

``` purescript
listTopicRules :: forall eff. ListTopicRulesRequest -> Aff (err :: RequestError | eff) ListTopicRulesResponse
```

<p>Lists the rules for the specific topic.</p>

#### `listV2LoggingLevels`

``` purescript
listV2LoggingLevels :: forall eff. ListV2LoggingLevelsRequest -> Aff (err :: RequestError | eff) ListV2LoggingLevelsResponse
```

<p>Lists logging levels.</p>

#### `registerCACertificate`

``` purescript
registerCACertificate :: forall eff. RegisterCACertificateRequest -> Aff (err :: RequestError | eff) RegisterCACertificateResponse
```

<p>Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.</p>

#### `registerCertificate`

``` purescript
registerCertificate :: forall eff. RegisterCertificateRequest -> Aff (err :: RequestError | eff) RegisterCertificateResponse
```

<p>Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.</p>

#### `registerThing`

``` purescript
registerThing :: forall eff. RegisterThingRequest -> Aff (err :: RequestError | eff) RegisterThingResponse
```

<p>Provisions a thing.</p>

#### `rejectCertificateTransfer`

``` purescript
rejectCertificateTransfer :: forall eff. RejectCertificateTransferRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from <b>PENDING_TRANSFER</b> to <b>INACTIVE</b>.</p> <p>To check for pending certificate transfers, call <a>ListCertificates</a> to enumerate your certificates.</p> <p>This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.</p>

#### `removeThingFromThingGroup`

``` purescript
removeThingFromThingGroup :: forall eff. RemoveThingFromThingGroupRequest -> Aff (err :: RequestError | eff) RemoveThingFromThingGroupResponse
```

<p>Remove the specified thing from the specified group.</p>

#### `replaceTopicRule`

``` purescript
replaceTopicRule :: forall eff. ReplaceTopicRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Replaces the rule. You must specify all parameters for the new rule. Creating rules is an administrator-level action. Any user who has permission to create rules will be able to access data processed by the rule.</p>

#### `searchIndex`

``` purescript
searchIndex :: forall eff. SearchIndexRequest -> Aff (err :: RequestError | eff) SearchIndexResponse
```

<p>The query search index.</p>

#### `setDefaultAuthorizer`

``` purescript
setDefaultAuthorizer :: forall eff. SetDefaultAuthorizerRequest -> Aff (err :: RequestError | eff) SetDefaultAuthorizerResponse
```

<p>Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.</p>

#### `setDefaultPolicyVersion`

``` purescript
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.</p>

#### `setLoggingOptions`

``` purescript
setLoggingOptions :: forall eff. SetLoggingOptionsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the logging options.</p>

#### `setV2LoggingLevel`

``` purescript
setV2LoggingLevel :: forall eff. SetV2LoggingLevelRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the logging level.</p>

#### `setV2LoggingOptions`

``` purescript
setV2LoggingOptions :: forall eff. SetV2LoggingOptionsRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the logging options for the V2 logging service.</p>

#### `startThingRegistrationTask`

``` purescript
startThingRegistrationTask :: forall eff. StartThingRegistrationTaskRequest -> Aff (err :: RequestError | eff) StartThingRegistrationTaskResponse
```

<p>Creates a bulk thing provisioning task.</p>

#### `stopThingRegistrationTask`

``` purescript
stopThingRegistrationTask :: forall eff. StopThingRegistrationTaskRequest -> Aff (err :: RequestError | eff) StopThingRegistrationTaskResponse
```

<p>Cancels a bulk thing provisioning task.</p>

#### `testAuthorization`

``` purescript
testAuthorization :: forall eff. TestAuthorizationRequest -> Aff (err :: RequestError | eff) TestAuthorizationResponse
```

<p>Test custom authorization.</p>

#### `testInvokeAuthorizer`

``` purescript
testInvokeAuthorizer :: forall eff. TestInvokeAuthorizerRequest -> Aff (err :: RequestError | eff) TestInvokeAuthorizerResponse
```

<p>Invoke the specified custom authorizer for testing purposes.</p>

#### `transferCertificate`

``` purescript
transferCertificate :: forall eff. TransferCertificateRequest -> Aff (err :: RequestError | eff) TransferCertificateResponse
```

<p>Transfers the specified certificate to the specified AWS account.</p> <p>You can cancel the transfer until it is acknowledged by the recipient.</p> <p>No notification is sent to the transfer destination's account. It is up to the caller to notify the transfer target.</p> <p>The certificate being transferred must not be in the ACTIVE state. You can use the UpdateCertificate API to deactivate it.</p> <p>The certificate must not have any policies attached to it. You can use the DetachPrincipalPolicy API to detach them.</p>

#### `updateAuthorizer`

``` purescript
updateAuthorizer :: forall eff. UpdateAuthorizerRequest -> Aff (err :: RequestError | eff) UpdateAuthorizerResponse
```

<p>Updates an authorizer.</p>

#### `updateCACertificate`

``` purescript
updateCACertificate :: forall eff. UpdateCACertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a registered CA certificate.</p>

#### `updateCertificate`

``` purescript
updateCertificate :: forall eff. UpdateCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the status of the specified certificate. This operation is idempotent.</p> <p>Moving a certificate from the ACTIVE state (including REVOKED) will not disconnect currently connected devices, but these devices will be unable to reconnect.</p> <p>The ACTIVE state is required to authenticate devices connecting to AWS IoT using a certificate.</p>

#### `updateEventConfigurations`

``` purescript
updateEventConfigurations :: forall eff. UpdateEventConfigurationsRequest -> Aff (err :: RequestError | eff) UpdateEventConfigurationsResponse
```

<p>Updates the event configurations.</p>

#### `updateIndexingConfiguration`

``` purescript
updateIndexingConfiguration :: forall eff. UpdateIndexingConfigurationRequest -> Aff (err :: RequestError | eff) UpdateIndexingConfigurationResponse
```

<p>Updates the search configuration.</p>

#### `updateRoleAlias`

``` purescript
updateRoleAlias :: forall eff. UpdateRoleAliasRequest -> Aff (err :: RequestError | eff) UpdateRoleAliasResponse
```

<p>Updates a role alias.</p>

#### `updateStream`

``` purescript
updateStream :: forall eff. UpdateStreamRequest -> Aff (err :: RequestError | eff) UpdateStreamResponse
```

<p>Updates an existing stream. The stream version will be incremented by one.</p>

#### `updateThing`

``` purescript
updateThing :: forall eff. UpdateThingRequest -> Aff (err :: RequestError | eff) UpdateThingResponse
```

<p>Updates the data for a thing.</p>

#### `updateThingGroup`

``` purescript
updateThingGroup :: forall eff. UpdateThingGroupRequest -> Aff (err :: RequestError | eff) UpdateThingGroupResponse
```

<p>Update a thing group.</p>

#### `updateThingGroupsForThing`

``` purescript
updateThingGroupsForThing :: forall eff. UpdateThingGroupsForThingRequest -> Aff (err :: RequestError | eff) UpdateThingGroupsForThingResponse
```

<p>Updates the groups to which the thing belongs.</p>

#### `AcceptCertificateTransferRequest`

``` purescript
newtype AcceptCertificateTransferRequest
  = AcceptCertificateTransferRequest { "CertificateId'" :: CertificateId, "SetAsActive'" :: NullOrUndefined (SetAsActive) }
```

<p>The input for the AcceptCertificateTransfer operation.</p>

#### `Action`

``` purescript
newtype Action
  = Action { "DynamoDB'" :: NullOrUndefined (DynamoDBAction), "DynamoDBv2'" :: NullOrUndefined (DynamoDBv2Action), "Lambda'" :: NullOrUndefined (LambdaAction), "Sns'" :: NullOrUndefined (SnsAction), "Sqs'" :: NullOrUndefined (SqsAction), "Kinesis'" :: NullOrUndefined (KinesisAction), "Republish'" :: NullOrUndefined (RepublishAction), "S3'" :: NullOrUndefined (S3Action), "Firehose'" :: NullOrUndefined (FirehoseAction), "CloudwatchMetric'" :: NullOrUndefined (CloudwatchMetricAction), "CloudwatchAlarm'" :: NullOrUndefined (CloudwatchAlarmAction), "Elasticsearch'" :: NullOrUndefined (ElasticsearchAction), "Salesforce'" :: NullOrUndefined (SalesforceAction) }
```

<p>Describes the actions associated with a rule.</p>

#### `ActionList`

``` purescript
newtype ActionList
  = ActionList (Array Action)
```

#### `ActionType`

``` purescript
newtype ActionType
  = ActionType String
```

#### `AddThingToThingGroupRequest`

``` purescript
newtype AddThingToThingGroupRequest
  = AddThingToThingGroupRequest { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn) }
```

#### `AddThingToThingGroupResponse`

``` purescript
newtype AddThingToThingGroupResponse
  = AddThingToThingGroupResponse {  }
```

#### `AdditionalParameterMap`

``` purescript
newtype AdditionalParameterMap
  = AdditionalParameterMap (Map Key Value)
```

#### `AlarmName`

``` purescript
newtype AlarmName
  = AlarmName String
```

#### `AllowAutoRegistration`

``` purescript
newtype AllowAutoRegistration
  = AllowAutoRegistration Boolean
```

#### `Allowed`

``` purescript
newtype Allowed
  = Allowed { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Contains information that allowed the authorization.</p>

#### `AscendingOrder`

``` purescript
newtype AscendingOrder
  = AscendingOrder Boolean
```

#### `AssociateTargetsWithJobRequest`

``` purescript
newtype AssociateTargetsWithJobRequest
  = AssociateTargetsWithJobRequest { "Targets'" :: JobTargets, "JobId'" :: JobId, "Comment'" :: NullOrUndefined (Comment) }
```

#### `AssociateTargetsWithJobResponse`

``` purescript
newtype AssociateTargetsWithJobResponse
  = AssociateTargetsWithJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

#### `AttachPolicyRequest`

``` purescript
newtype AttachPolicyRequest
  = AttachPolicyRequest { "PolicyName'" :: PolicyName, "Target'" :: PolicyTarget }
```

#### `AttachPrincipalPolicyRequest`

``` purescript
newtype AttachPrincipalPolicyRequest
  = AttachPrincipalPolicyRequest { "PolicyName'" :: PolicyName, "Principal'" :: Principal }
```

<p>The input for the AttachPrincipalPolicy operation.</p>

#### `AttachThingPrincipalRequest`

``` purescript
newtype AttachThingPrincipalRequest
  = AttachThingPrincipalRequest { "ThingName'" :: ThingName, "Principal'" :: Principal }
```

<p>The input for the AttachThingPrincipal operation.</p>

#### `AttachThingPrincipalResponse`

``` purescript
newtype AttachThingPrincipalResponse
  = AttachThingPrincipalResponse {  }
```

<p>The output from the AttachThingPrincipal operation.</p>

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

#### `AttributePayload`

``` purescript
newtype AttributePayload
  = AttributePayload { "Attributes'" :: NullOrUndefined (Attributes), "Merge'" :: NullOrUndefined (Flag) }
```

<p>The attribute payload.</p>

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Map AttributeName AttributeValue)
```

#### `AttributesMap`

``` purescript
newtype AttributesMap
  = AttributesMap (Map Key Value)
```

#### `AuthDecision`

``` purescript
newtype AuthDecision
  = AuthDecision String
```

#### `AuthInfo`

``` purescript
newtype AuthInfo
  = AuthInfo { "ActionType'" :: NullOrUndefined (ActionType), "Resources'" :: NullOrUndefined (Resources) }
```

<p>A collection of authorization information.</p>

#### `AuthInfos`

``` purescript
newtype AuthInfos
  = AuthInfos (Array AuthInfo)
```

#### `AuthResult`

``` purescript
newtype AuthResult
  = AuthResult { "AuthInfo'" :: NullOrUndefined (AuthInfo), "Allowed'" :: NullOrUndefined (Allowed), "Denied'" :: NullOrUndefined (Denied), "AuthDecision'" :: NullOrUndefined (AuthDecision), "MissingContextValues'" :: NullOrUndefined (MissingContextValues) }
```

<p>The authorizer result.</p>

#### `AuthResults`

``` purescript
newtype AuthResults
  = AuthResults (Array AuthResult)
```

#### `AuthorizerArn`

``` purescript
newtype AuthorizerArn
  = AuthorizerArn String
```

#### `AuthorizerDescription`

``` purescript
newtype AuthorizerDescription
  = AuthorizerDescription { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn), "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn), "TokenKeyName'" :: NullOrUndefined (TokenKeyName), "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap), "Status'" :: NullOrUndefined (AuthorizerStatus), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType) }
```

<p>The authorizer description.</p>

#### `AuthorizerFunctionArn`

``` purescript
newtype AuthorizerFunctionArn
  = AuthorizerFunctionArn String
```

#### `AuthorizerName`

``` purescript
newtype AuthorizerName
  = AuthorizerName String
```

#### `AuthorizerStatus`

``` purescript
newtype AuthorizerStatus
  = AuthorizerStatus String
```

#### `AuthorizerSummary`

``` purescript
newtype AuthorizerSummary
  = AuthorizerSummary { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

<p>The authorizer summary.</p>

#### `Authorizers`

``` purescript
newtype Authorizers
  = Authorizers (Array AuthorizerSummary)
```

#### `AutoRegistrationStatus`

``` purescript
newtype AutoRegistrationStatus
  = AutoRegistrationStatus String
```

#### `AwsAccountId`

``` purescript
newtype AwsAccountId
  = AwsAccountId String
```

#### `AwsArn`

``` purescript
newtype AwsArn
  = AwsArn String
```

#### `AwsIotJobArn`

``` purescript
newtype AwsIotJobArn
  = AwsIotJobArn String
```

#### `AwsIotJobId`

``` purescript
newtype AwsIotJobId
  = AwsIotJobId String
```

#### `AwsIotSqlVersion`

``` purescript
newtype AwsIotSqlVersion
  = AwsIotSqlVersion String
```

#### `BucketName`

``` purescript
newtype BucketName
  = BucketName String
```

#### `CACertificate`

``` purescript
newtype CACertificate
  = CACertificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CACertificateStatus), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>A CA certificate.</p>

#### `CACertificateDescription`

``` purescript
newtype CACertificateDescription
  = CACertificateDescription { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CACertificateStatus), "CertificatePem'" :: NullOrUndefined (CertificatePem), "OwnedBy'" :: NullOrUndefined (AwsAccountId), "CreationDate'" :: NullOrUndefined (DateType), "AutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus) }
```

<p>Describes a CA certificate.</p>

#### `CACertificateStatus`

``` purescript
newtype CACertificateStatus
  = CACertificateStatus String
```

#### `CACertificates`

``` purescript
newtype CACertificates
  = CACertificates (Array CACertificate)
```

#### `CancelCertificateTransferRequest`

``` purescript
newtype CancelCertificateTransferRequest
  = CancelCertificateTransferRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the CancelCertificateTransfer operation.</p>

#### `CancelJobRequest`

``` purescript
newtype CancelJobRequest
  = CancelJobRequest { "JobId'" :: JobId, "Comment'" :: NullOrUndefined (Comment) }
```

#### `CancelJobResponse`

``` purescript
newtype CancelJobResponse
  = CancelJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

#### `CanceledThings`

``` purescript
newtype CanceledThings
  = CanceledThings Int
```

#### `CannedAccessControlList`

``` purescript
newtype CannedAccessControlList
  = CannedAccessControlList String
```

#### `Certificate`

``` purescript
newtype Certificate
  = Certificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CertificateStatus), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>Information about a certificate.</p>

#### `CertificateArn`

``` purescript
newtype CertificateArn
  = CertificateArn String
```

#### `CertificateConflictException`

``` purescript
newtype CertificateConflictException
  = CertificateConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.</p>

#### `CertificateDescription`

``` purescript
newtype CertificateDescription
  = CertificateDescription { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CaCertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CertificateStatus), "CertificatePem'" :: NullOrUndefined (CertificatePem), "OwnedBy'" :: NullOrUndefined (AwsAccountId), "PreviousOwnedBy'" :: NullOrUndefined (AwsAccountId), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType), "TransferData'" :: NullOrUndefined (TransferData) }
```

<p>Describes a certificate.</p>

#### `CertificateId`

``` purescript
newtype CertificateId
  = CertificateId String
```

#### `CertificateName`

``` purescript
newtype CertificateName
  = CertificateName String
```

#### `CertificatePem`

``` purescript
newtype CertificatePem
  = CertificatePem String
```

<p>The PEM of a certificate.</p>

#### `CertificateSigningRequest`

``` purescript
newtype CertificateSigningRequest
  = CertificateSigningRequest String
```

#### `CertificateStateException`

``` purescript
newtype CertificateStateException
  = CertificateStateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The certificate operation is not allowed.</p>

#### `CertificateStatus`

``` purescript
newtype CertificateStatus
  = CertificateStatus String
```

#### `CertificateValidationException`

``` purescript
newtype CertificateValidationException
  = CertificateValidationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The certificate is invalid.</p>

#### `Certificates`

``` purescript
newtype Certificates
  = Certificates (Array Certificate)
```

#### `ClearDefaultAuthorizerRequest`

``` purescript
newtype ClearDefaultAuthorizerRequest
  = ClearDefaultAuthorizerRequest {  }
```

#### `ClearDefaultAuthorizerResponse`

``` purescript
newtype ClearDefaultAuthorizerResponse
  = ClearDefaultAuthorizerResponse {  }
```

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

#### `CloudwatchAlarmAction`

``` purescript
newtype CloudwatchAlarmAction
  = CloudwatchAlarmAction { "RoleArn'" :: AwsArn, "AlarmName'" :: AlarmName, "StateReason'" :: StateReason, "StateValue'" :: StateValue }
```

<p>Describes an action that updates a CloudWatch alarm.</p>

#### `CloudwatchMetricAction`

``` purescript
newtype CloudwatchMetricAction
  = CloudwatchMetricAction { "RoleArn'" :: AwsArn, "MetricNamespace'" :: MetricNamespace, "MetricName'" :: MetricName, "MetricValue'" :: MetricValue, "MetricUnit'" :: MetricUnit, "MetricTimestamp'" :: NullOrUndefined (MetricTimestamp) }
```

<p>Describes an action that captures a CloudWatch metric.</p>

#### `Code`

``` purescript
newtype Code
  = Code String
```

#### `CodeSigning`

``` purescript
newtype CodeSigning
  = CodeSigning { "AwsSignerJobId'" :: NullOrUndefined (SigningJobId), "CustomCodeSigning'" :: NullOrUndefined (CustomCodeSigning) }
```

<p>Describes the method to use when code signing a file.</p>

#### `CodeSigningCertificateChain`

``` purescript
newtype CodeSigningCertificateChain
  = CodeSigningCertificateChain { "Stream'" :: NullOrUndefined (Stream), "CertificateName'" :: NullOrUndefined (CertificateName), "InlineDocument'" :: NullOrUndefined (InlineDocument) }
```

<p>Describes the certificate chain being used when code signing a file.</p>

#### `CodeSigningSignature`

``` purescript
newtype CodeSigningSignature
  = CodeSigningSignature { "Stream'" :: NullOrUndefined (Stream), "InlineDocument'" :: NullOrUndefined (Signature) }
```

<p>Describes the signature for a file.</p>

#### `CognitoIdentityPoolId`

``` purescript
newtype CognitoIdentityPoolId
  = CognitoIdentityPoolId String
```

#### `Comment`

``` purescript
newtype Comment
  = Comment String
```

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration { "Enabled" :: NullOrUndefined (Enabled) }
```

<p>Configuration.</p>

#### `ConflictingResourceUpdateException`

``` purescript
newtype ConflictingResourceUpdateException
  = ConflictingResourceUpdateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.</p>

#### `Count`

``` purescript
newtype Count
  = Count Int
```

#### `CreateAuthorizerRequest`

``` purescript
newtype CreateAuthorizerRequest
  = CreateAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "AuthorizerFunctionArn'" :: AuthorizerFunctionArn, "TokenKeyName'" :: TokenKeyName, "TokenSigningPublicKeys'" :: PublicKeyMap, "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

#### `CreateAuthorizerResponse`

``` purescript
newtype CreateAuthorizerResponse
  = CreateAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

#### `CreateCertificateFromCsrRequest`

``` purescript
newtype CreateCertificateFromCsrRequest
  = CreateCertificateFromCsrRequest { "CertificateSigningRequest'" :: CertificateSigningRequest, "SetAsActive'" :: NullOrUndefined (SetAsActive) }
```

<p>The input for the CreateCertificateFromCsr operation.</p>

#### `CreateCertificateFromCsrResponse`

``` purescript
newtype CreateCertificateFromCsrResponse
  = CreateCertificateFromCsrResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CertificatePem'" :: NullOrUndefined (CertificatePem) }
```

<p>The output from the CreateCertificateFromCsr operation.</p>

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "JobId'" :: JobId, "Targets'" :: JobTargets, "DocumentSource'" :: NullOrUndefined (JobDocumentSource), "Document'" :: NullOrUndefined (JobDocument), "Description'" :: NullOrUndefined (JobDescription), "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig), "TargetSelection'" :: NullOrUndefined (TargetSelection), "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig), "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters) }
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

#### `CreateKeysAndCertificateRequest`

``` purescript
newtype CreateKeysAndCertificateRequest
  = CreateKeysAndCertificateRequest { "SetAsActive'" :: NullOrUndefined (SetAsActive) }
```

<p>The input for the CreateKeysAndCertificate operation.</p>

#### `CreateKeysAndCertificateResponse`

``` purescript
newtype CreateKeysAndCertificateResponse
  = CreateKeysAndCertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CertificatePem'" :: NullOrUndefined (CertificatePem), "KeyPair'" :: NullOrUndefined (KeyPair) }
```

<p>The output of the CreateKeysAndCertificate operation.</p>

#### `CreateOTAUpdateRequest`

``` purescript
newtype CreateOTAUpdateRequest
  = CreateOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId, "Description'" :: NullOrUndefined (OTAUpdateDescription), "Targets'" :: Targets, "TargetSelection'" :: NullOrUndefined (TargetSelection), "Files'" :: OTAUpdateFiles, "RoleArn'" :: RoleArn, "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap) }
```

#### `CreateOTAUpdateResponse`

``` purescript
newtype CreateOTAUpdateResponse
  = CreateOTAUpdateResponse { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus) }
```

#### `CreatePolicyRequest`

``` purescript
newtype CreatePolicyRequest
  = CreatePolicyRequest { "PolicyName'" :: PolicyName, "PolicyDocument'" :: PolicyDocument }
```

<p>The input for the CreatePolicy operation.</p>

#### `CreatePolicyResponse`

``` purescript
newtype CreatePolicyResponse
  = CreatePolicyResponse { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId) }
```

<p>The output from the CreatePolicy operation.</p>

#### `CreatePolicyVersionRequest`

``` purescript
newtype CreatePolicyVersionRequest
  = CreatePolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyDocument'" :: PolicyDocument, "SetAsDefault'" :: NullOrUndefined (SetAsDefault) }
```

<p>The input for the CreatePolicyVersion operation.</p>

#### `CreatePolicyVersionResponse`

``` purescript
newtype CreatePolicyVersionResponse
  = CreatePolicyVersionResponse { "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion) }
```

<p>The output of the CreatePolicyVersion operation.</p>

#### `CreateRoleAliasRequest`

``` purescript
newtype CreateRoleAliasRequest
  = CreateRoleAliasRequest { "RoleAlias'" :: RoleAlias, "RoleArn'" :: RoleArn, "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds) }
```

#### `CreateRoleAliasResponse`

``` purescript
newtype CreateRoleAliasResponse
  = CreateRoleAliasResponse { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn) }
```

#### `CreateStreamRequest`

``` purescript
newtype CreateStreamRequest
  = CreateStreamRequest { "StreamId'" :: StreamId, "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: StreamFiles, "RoleArn'" :: RoleArn }
```

#### `CreateStreamResponse`

``` purescript
newtype CreateStreamResponse
  = CreateStreamResponse { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "Description'" :: NullOrUndefined (StreamDescription), "StreamVersion'" :: NullOrUndefined (StreamVersion) }
```

#### `CreateThingGroupRequest`

``` purescript
newtype CreateThingGroupRequest
  = CreateThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ParentGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties) }
```

#### `CreateThingGroupResponse`

``` purescript
newtype CreateThingGroupResponse
  = CreateThingGroupResponse { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingGroupId'" :: NullOrUndefined (ThingGroupId) }
```

#### `CreateThingRequest`

``` purescript
newtype CreateThingRequest
  = CreateThingRequest { "ThingName'" :: ThingName, "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "AttributePayload'" :: NullOrUndefined (AttributePayload) }
```

<p>The input for the CreateThing operation.</p>

#### `CreateThingResponse`

``` purescript
newtype CreateThingResponse
  = CreateThingResponse { "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn), "ThingId'" :: NullOrUndefined (ThingId) }
```

<p>The output of the CreateThing operation.</p>

#### `CreateThingTypeRequest`

``` purescript
newtype CreateThingTypeRequest
  = CreateThingTypeRequest { "ThingTypeName'" :: ThingTypeName, "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties) }
```

<p>The input for the CreateThingType operation.</p>

#### `CreateThingTypeResponse`

``` purescript
newtype CreateThingTypeResponse
  = CreateThingTypeResponse { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeId'" :: NullOrUndefined (ThingTypeId) }
```

<p>The output of the CreateThingType operation.</p>

#### `CreateTopicRuleRequest`

``` purescript
newtype CreateTopicRuleRequest
  = CreateTopicRuleRequest { "RuleName'" :: RuleName, "TopicRulePayload'" :: TopicRulePayload }
```

<p>The input for the CreateTopicRule operation.</p>

#### `CreatedAtDate`

``` purescript
newtype CreatedAtDate
  = CreatedAtDate Number
```

#### `CreationDate`

``` purescript
newtype CreationDate
  = CreationDate Number
```

#### `CredentialDurationSeconds`

``` purescript
newtype CredentialDurationSeconds
  = CredentialDurationSeconds Int
```

#### `CustomCodeSigning`

``` purescript
newtype CustomCodeSigning
  = CustomCodeSigning { "Signature'" :: NullOrUndefined (CodeSigningSignature), "CertificateChain'" :: NullOrUndefined (CodeSigningCertificateChain), "HashAlgorithm'" :: NullOrUndefined (HashAlgorithm), "SignatureAlgorithm'" :: NullOrUndefined (SignatureAlgorithm) }
```

<p>Describes a custom method used to code sign a file.</p>

#### `DateType`

``` purescript
newtype DateType
  = DateType Number
```

#### `DeleteAuthorizerRequest`

``` purescript
newtype DeleteAuthorizerRequest
  = DeleteAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

#### `DeleteAuthorizerResponse`

``` purescript
newtype DeleteAuthorizerResponse
  = DeleteAuthorizerResponse {  }
```

#### `DeleteCACertificateRequest`

``` purescript
newtype DeleteCACertificateRequest
  = DeleteCACertificateRequest { "CertificateId'" :: CertificateId }
```

<p>Input for the DeleteCACertificate operation.</p>

#### `DeleteCACertificateResponse`

``` purescript
newtype DeleteCACertificateResponse
  = DeleteCACertificateResponse {  }
```

<p>The output for the DeleteCACertificate operation.</p>

#### `DeleteCertificateRequest`

``` purescript
newtype DeleteCertificateRequest
  = DeleteCertificateRequest { "CertificateId'" :: CertificateId, "ForceDelete'" :: NullOrUndefined (ForceDelete) }
```

<p>The input for the DeleteCertificate operation.</p>

#### `DeleteConflictException`

``` purescript
newtype DeleteConflictException
  = DeleteConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't delete the resource because it is attached to one or more resources.</p>

#### `DeleteOTAUpdateRequest`

``` purescript
newtype DeleteOTAUpdateRequest
  = DeleteOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId }
```

#### `DeleteOTAUpdateResponse`

``` purescript
newtype DeleteOTAUpdateResponse
  = DeleteOTAUpdateResponse {  }
```

#### `DeletePolicyRequest`

``` purescript
newtype DeletePolicyRequest
  = DeletePolicyRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the DeletePolicy operation.</p>

#### `DeletePolicyVersionRequest`

``` purescript
newtype DeletePolicyVersionRequest
  = DeletePolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the DeletePolicyVersion operation.</p>

#### `DeleteRegistrationCodeRequest`

``` purescript
newtype DeleteRegistrationCodeRequest
  = DeleteRegistrationCodeRequest {  }
```

<p>The input for the DeleteRegistrationCode operation.</p>

#### `DeleteRegistrationCodeResponse`

``` purescript
newtype DeleteRegistrationCodeResponse
  = DeleteRegistrationCodeResponse {  }
```

<p>The output for the DeleteRegistrationCode operation.</p>

#### `DeleteRoleAliasRequest`

``` purescript
newtype DeleteRoleAliasRequest
  = DeleteRoleAliasRequest { "RoleAlias'" :: RoleAlias }
```

#### `DeleteRoleAliasResponse`

``` purescript
newtype DeleteRoleAliasResponse
  = DeleteRoleAliasResponse {  }
```

#### `DeleteStreamRequest`

``` purescript
newtype DeleteStreamRequest
  = DeleteStreamRequest { "StreamId'" :: StreamId }
```

#### `DeleteStreamResponse`

``` purescript
newtype DeleteStreamResponse
  = DeleteStreamResponse {  }
```

#### `DeleteThingGroupRequest`

``` purescript
newtype DeleteThingGroupRequest
  = DeleteThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

#### `DeleteThingGroupResponse`

``` purescript
newtype DeleteThingGroupResponse
  = DeleteThingGroupResponse {  }
```

#### `DeleteThingRequest`

``` purescript
newtype DeleteThingRequest
  = DeleteThingRequest { "ThingName'" :: ThingName, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

<p>The input for the DeleteThing operation.</p>

#### `DeleteThingResponse`

``` purescript
newtype DeleteThingResponse
  = DeleteThingResponse {  }
```

<p>The output of the DeleteThing operation.</p>

#### `DeleteThingTypeRequest`

``` purescript
newtype DeleteThingTypeRequest
  = DeleteThingTypeRequest { "ThingTypeName'" :: ThingTypeName }
```

<p>The input for the DeleteThingType operation.</p>

#### `DeleteThingTypeResponse`

``` purescript
newtype DeleteThingTypeResponse
  = DeleteThingTypeResponse {  }
```

<p>The output for the DeleteThingType operation.</p>

#### `DeleteTopicRuleRequest`

``` purescript
newtype DeleteTopicRuleRequest
  = DeleteTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the DeleteTopicRule operation.</p>

#### `DeleteV2LoggingLevelRequest`

``` purescript
newtype DeleteV2LoggingLevelRequest
  = DeleteV2LoggingLevelRequest { "TargetType'" :: LogTargetType, "TargetName'" :: LogTargetName }
```

#### `DeliveryStreamName`

``` purescript
newtype DeliveryStreamName
  = DeliveryStreamName String
```

#### `Denied`

``` purescript
newtype Denied
  = Denied { "ImplicitDeny'" :: NullOrUndefined (ImplicitDeny), "ExplicitDeny'" :: NullOrUndefined (ExplicitDeny) }
```

<p>Contains information that denied the authorization.</p>

#### `DeprecateThingTypeRequest`

``` purescript
newtype DeprecateThingTypeRequest
  = DeprecateThingTypeRequest { "ThingTypeName'" :: ThingTypeName, "UndoDeprecate'" :: NullOrUndefined (UndoDeprecate) }
```

<p>The input for the DeprecateThingType operation.</p>

#### `DeprecateThingTypeResponse`

``` purescript
newtype DeprecateThingTypeResponse
  = DeprecateThingTypeResponse {  }
```

<p>The output for the DeprecateThingType operation.</p>

#### `DeprecationDate`

``` purescript
newtype DeprecationDate
  = DeprecationDate Number
```

#### `DescribeAuthorizerRequest`

``` purescript
newtype DescribeAuthorizerRequest
  = DescribeAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

#### `DescribeAuthorizerResponse`

``` purescript
newtype DescribeAuthorizerResponse
  = DescribeAuthorizerResponse { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription) }
```

#### `DescribeCACertificateRequest`

``` purescript
newtype DescribeCACertificateRequest
  = DescribeCACertificateRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the DescribeCACertificate operation.</p>

#### `DescribeCACertificateResponse`

``` purescript
newtype DescribeCACertificateResponse
  = DescribeCACertificateResponse { "CertificateDescription'" :: NullOrUndefined (CACertificateDescription), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig) }
```

<p>The output from the DescribeCACertificate operation.</p>

#### `DescribeCertificateRequest`

``` purescript
newtype DescribeCertificateRequest
  = DescribeCertificateRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the DescribeCertificate operation.</p>

#### `DescribeCertificateResponse`

``` purescript
newtype DescribeCertificateResponse
  = DescribeCertificateResponse { "CertificateDescription'" :: NullOrUndefined (CertificateDescription) }
```

<p>The output of the DescribeCertificate operation.</p>

#### `DescribeDefaultAuthorizerRequest`

``` purescript
newtype DescribeDefaultAuthorizerRequest
  = DescribeDefaultAuthorizerRequest {  }
```

#### `DescribeDefaultAuthorizerResponse`

``` purescript
newtype DescribeDefaultAuthorizerResponse
  = DescribeDefaultAuthorizerResponse { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription) }
```

#### `DescribeEndpointRequest`

``` purescript
newtype DescribeEndpointRequest
  = DescribeEndpointRequest { "EndpointType'" :: NullOrUndefined (EndpointType) }
```

<p>The input for the DescribeEndpoint operation.</p>

#### `DescribeEndpointResponse`

``` purescript
newtype DescribeEndpointResponse
  = DescribeEndpointResponse { "EndpointAddress'" :: NullOrUndefined (EndpointAddress) }
```

<p>The output from the DescribeEndpoint operation.</p>

#### `DescribeEventConfigurationsRequest`

``` purescript
newtype DescribeEventConfigurationsRequest
  = DescribeEventConfigurationsRequest {  }
```

#### `DescribeEventConfigurationsResponse`

``` purescript
newtype DescribeEventConfigurationsResponse
  = DescribeEventConfigurationsResponse { "EventConfigurations'" :: NullOrUndefined (EventConfigurations), "CreationDate'" :: NullOrUndefined (CreationDate), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate) }
```

#### `DescribeIndexRequest`

``` purescript
newtype DescribeIndexRequest
  = DescribeIndexRequest { "IndexName'" :: IndexName }
```

#### `DescribeIndexResponse`

``` purescript
newtype DescribeIndexResponse
  = DescribeIndexResponse { "IndexName'" :: NullOrUndefined (IndexName), "IndexStatus'" :: NullOrUndefined (IndexStatus), "Schema'" :: NullOrUndefined (IndexSchema) }
```

#### `DescribeJobExecutionRequest`

``` purescript
newtype DescribeJobExecutionRequest
  = DescribeJobExecutionRequest { "JobId'" :: JobId, "ThingName'" :: ThingName, "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

#### `DescribeJobExecutionResponse`

``` purescript
newtype DescribeJobExecutionResponse
  = DescribeJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

#### `DescribeJobRequest`

``` purescript
newtype DescribeJobRequest
  = DescribeJobRequest { "JobId'" :: JobId }
```

#### `DescribeJobResponse`

``` purescript
newtype DescribeJobResponse
  = DescribeJobResponse { "DocumentSource'" :: NullOrUndefined (JobDocumentSource), "Job'" :: NullOrUndefined (Job) }
```

#### `DescribeRoleAliasRequest`

``` purescript
newtype DescribeRoleAliasRequest
  = DescribeRoleAliasRequest { "RoleAlias'" :: RoleAlias }
```

#### `DescribeRoleAliasResponse`

``` purescript
newtype DescribeRoleAliasResponse
  = DescribeRoleAliasResponse { "RoleAliasDescription'" :: NullOrUndefined (RoleAliasDescription) }
```

#### `DescribeStreamRequest`

``` purescript
newtype DescribeStreamRequest
  = DescribeStreamRequest { "StreamId'" :: StreamId }
```

#### `DescribeStreamResponse`

``` purescript
newtype DescribeStreamResponse
  = DescribeStreamResponse { "StreamInfo'" :: NullOrUndefined (StreamInfo) }
```

#### `DescribeThingGroupRequest`

``` purescript
newtype DescribeThingGroupRequest
  = DescribeThingGroupRequest { "ThingGroupName'" :: ThingGroupName }
```

#### `DescribeThingGroupResponse`

``` purescript
newtype DescribeThingGroupResponse
  = DescribeThingGroupResponse { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupId'" :: NullOrUndefined (ThingGroupId), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "Version'" :: NullOrUndefined (Version), "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties), "ThingGroupMetadata'" :: NullOrUndefined (ThingGroupMetadata) }
```

#### `DescribeThingRegistrationTaskRequest`

``` purescript
newtype DescribeThingRegistrationTaskRequest
  = DescribeThingRegistrationTaskRequest { "TaskId'" :: TaskId }
```

#### `DescribeThingRegistrationTaskResponse`

``` purescript
newtype DescribeThingRegistrationTaskResponse
  = DescribeThingRegistrationTaskResponse { "TaskId'" :: NullOrUndefined (TaskId), "CreationDate'" :: NullOrUndefined (CreationDate), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate), "TemplateBody'" :: NullOrUndefined (TemplateBody), "InputFileBucket'" :: NullOrUndefined (RegistryS3BucketName), "InputFileKey'" :: NullOrUndefined (RegistryS3KeyName), "RoleArn'" :: NullOrUndefined (RoleArn), "Status'" :: NullOrUndefined (Status), "Message'" :: NullOrUndefined (ErrorMessage), "SuccessCount'" :: NullOrUndefined (Count), "FailureCount'" :: NullOrUndefined (Count), "PercentageProgress'" :: NullOrUndefined (Percentage) }
```

#### `DescribeThingRequest`

``` purescript
newtype DescribeThingRequest
  = DescribeThingRequest { "ThingName'" :: ThingName }
```

<p>The input for the DescribeThing operation.</p>

#### `DescribeThingResponse`

``` purescript
newtype DescribeThingResponse
  = DescribeThingResponse { "DefaultClientId'" :: NullOrUndefined (ClientId), "ThingName'" :: NullOrUndefined (ThingName), "ThingId'" :: NullOrUndefined (ThingId), "ThingArn'" :: NullOrUndefined (ThingArn), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "Attributes'" :: NullOrUndefined (Attributes), "Version'" :: NullOrUndefined (Version) }
```

<p>The output from the DescribeThing operation.</p>

#### `DescribeThingTypeRequest`

``` purescript
newtype DescribeThingTypeRequest
  = DescribeThingTypeRequest { "ThingTypeName'" :: ThingTypeName }
```

<p>The input for the DescribeThingType operation.</p>

#### `DescribeThingTypeResponse`

``` purescript
newtype DescribeThingTypeResponse
  = DescribeThingTypeResponse { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeId'" :: NullOrUndefined (ThingTypeId), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties), "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata) }
```

<p>The output for the DescribeThingType operation.</p>

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DetachPolicyRequest`

``` purescript
newtype DetachPolicyRequest
  = DetachPolicyRequest { "PolicyName'" :: PolicyName, "Target'" :: PolicyTarget }
```

#### `DetachPrincipalPolicyRequest`

``` purescript
newtype DetachPrincipalPolicyRequest
  = DetachPrincipalPolicyRequest { "PolicyName'" :: PolicyName, "Principal'" :: Principal }
```

<p>The input for the DetachPrincipalPolicy operation.</p>

#### `DetachThingPrincipalRequest`

``` purescript
newtype DetachThingPrincipalRequest
  = DetachThingPrincipalRequest { "ThingName'" :: ThingName, "Principal'" :: Principal }
```

<p>The input for the DetachThingPrincipal operation.</p>

#### `DetachThingPrincipalResponse`

``` purescript
newtype DetachThingPrincipalResponse
  = DetachThingPrincipalResponse {  }
```

<p>The output from the DetachThingPrincipal operation.</p>

#### `DetailsKey`

``` purescript
newtype DetailsKey
  = DetailsKey String
```

#### `DetailsMap`

``` purescript
newtype DetailsMap
  = DetailsMap (Map DetailsKey DetailsValue)
```

#### `DetailsValue`

``` purescript
newtype DetailsValue
  = DetailsValue String
```

#### `DisableAllLogs`

``` purescript
newtype DisableAllLogs
  = DisableAllLogs Boolean
```

#### `DisableTopicRuleRequest`

``` purescript
newtype DisableTopicRuleRequest
  = DisableTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the DisableTopicRuleRequest operation.</p>

#### `DynamoDBAction`

``` purescript
newtype DynamoDBAction
  = DynamoDBAction { "TableName'" :: TableName, "RoleArn'" :: AwsArn, "Operation'" :: NullOrUndefined (DynamoOperation), "HashKeyField'" :: HashKeyField, "HashKeyValue'" :: HashKeyValue, "HashKeyType'" :: NullOrUndefined (DynamoKeyType), "RangeKeyField'" :: NullOrUndefined (RangeKeyField), "RangeKeyValue'" :: NullOrUndefined (RangeKeyValue), "RangeKeyType'" :: NullOrUndefined (DynamoKeyType), "PayloadField'" :: NullOrUndefined (PayloadField) }
```

<p>Describes an action to write to a DynamoDB table.</p> <p>The <code>tableName</code>, <code>hashKeyField</code>, and <code>rangeKeyField</code> values must match the values used when you created the table.</p> <p>The <code>hashKeyValue</code> and <code>rangeKeyvalue</code> fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${<i>sql-expression</i>}.</p> <p>You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:</p> <p> <code>"hashKeyValue": "${topic(3)}"</code> </p> <p>The following field uses the timestamp:</p> <p> <code>"rangeKeyValue": "${timestamp()}"</code> </p>

#### `DynamoDBv2Action`

``` purescript
newtype DynamoDBv2Action
  = DynamoDBv2Action { "RoleArn'" :: NullOrUndefined (AwsArn), "PutItem'" :: NullOrUndefined (PutItemInput) }
```

<p>Describes an action to write to a DynamoDB table.</p> <p>This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.</p>

#### `DynamoKeyType`

``` purescript
newtype DynamoKeyType
  = DynamoKeyType String
```

#### `DynamoOperation`

``` purescript
newtype DynamoOperation
  = DynamoOperation String
```

#### `EffectivePolicies`

``` purescript
newtype EffectivePolicies
  = EffectivePolicies (Array EffectivePolicy)
```

#### `EffectivePolicy`

``` purescript
newtype EffectivePolicy
  = EffectivePolicy { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument) }
```

<p>The policy that has the effect on the authorization results.</p>

#### `ElasticsearchAction`

``` purescript
newtype ElasticsearchAction
  = ElasticsearchAction { "RoleArn'" :: AwsArn, "Endpoint'" :: ElasticsearchEndpoint, "Index'" :: ElasticsearchIndex, "Type'" :: ElasticsearchType, "Id'" :: ElasticsearchId }
```

<p>Describes an action that writes data to an Amazon Elasticsearch Service domain.</p>

#### `ElasticsearchEndpoint`

``` purescript
newtype ElasticsearchEndpoint
  = ElasticsearchEndpoint String
```

#### `ElasticsearchId`

``` purescript
newtype ElasticsearchId
  = ElasticsearchId String
```

#### `ElasticsearchIndex`

``` purescript
newtype ElasticsearchIndex
  = ElasticsearchIndex String
```

#### `ElasticsearchType`

``` purescript
newtype ElasticsearchType
  = ElasticsearchType String
```

#### `EnableTopicRuleRequest`

``` purescript
newtype EnableTopicRuleRequest
  = EnableTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the EnableTopicRuleRequest operation.</p>

#### `Enabled`

``` purescript
newtype Enabled
  = Enabled Boolean
```

#### `EndpointAddress`

``` purescript
newtype EndpointAddress
  = EndpointAddress String
```

#### `EndpointType`

``` purescript
newtype EndpointType
  = EndpointType String
```

#### `ErrorInfo`

``` purescript
newtype ErrorInfo
  = ErrorInfo { "Code'" :: NullOrUndefined (Code), "Message'" :: NullOrUndefined (OTAUpdateErrorMessage) }
```

<p>Error information.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `EventConfigurations`

``` purescript
newtype EventConfigurations
  = EventConfigurations (Map EventType Configuration)
```

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

#### `ExecutionNumber`

``` purescript
newtype ExecutionNumber
  = ExecutionNumber Number
```

#### `ExpiresInSec`

``` purescript
newtype ExpiresInSec
  = ExpiresInSec Number
```

#### `ExplicitDeny`

``` purescript
newtype ExplicitDeny
  = ExplicitDeny { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Information that explicitly denies authorization.</p>

#### `FailedThings`

``` purescript
newtype FailedThings
  = FailedThings Int
```

#### `FileId`

``` purescript
newtype FileId
  = FileId Int
```

#### `FileName`

``` purescript
newtype FileName
  = FileName String
```

#### `FirehoseAction`

``` purescript
newtype FirehoseAction
  = FirehoseAction { "RoleArn'" :: AwsArn, "DeliveryStreamName'" :: DeliveryStreamName, "Separator'" :: NullOrUndefined (FirehoseSeparator) }
```

<p>Describes an action that writes data to an Amazon Kinesis Firehose stream.</p>

#### `FirehoseSeparator`

``` purescript
newtype FirehoseSeparator
  = FirehoseSeparator String
```

#### `Flag`

``` purescript
newtype Flag
  = Flag Boolean
```

#### `ForceDelete`

``` purescript
newtype ForceDelete
  = ForceDelete Boolean
```

#### `FunctionArn`

``` purescript
newtype FunctionArn
  = FunctionArn String
```

#### `GEMaxResults`

``` purescript
newtype GEMaxResults
  = GEMaxResults Int
```

#### `GetEffectivePoliciesRequest`

``` purescript
newtype GetEffectivePoliciesRequest
  = GetEffectivePoliciesRequest { "Principal'" :: NullOrUndefined (Principal), "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId), "ThingName'" :: NullOrUndefined (ThingName) }
```

#### `GetEffectivePoliciesResponse`

``` purescript
newtype GetEffectivePoliciesResponse
  = GetEffectivePoliciesResponse { "EffectivePolicies'" :: NullOrUndefined (EffectivePolicies) }
```

#### `GetIndexingConfigurationRequest`

``` purescript
newtype GetIndexingConfigurationRequest
  = GetIndexingConfigurationRequest {  }
```

#### `GetIndexingConfigurationResponse`

``` purescript
newtype GetIndexingConfigurationResponse
  = GetIndexingConfigurationResponse { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration) }
```

#### `GetJobDocumentRequest`

``` purescript
newtype GetJobDocumentRequest
  = GetJobDocumentRequest { "JobId'" :: JobId }
```

#### `GetJobDocumentResponse`

``` purescript
newtype GetJobDocumentResponse
  = GetJobDocumentResponse { "Document'" :: NullOrUndefined (JobDocument) }
```

#### `GetLoggingOptionsRequest`

``` purescript
newtype GetLoggingOptionsRequest
  = GetLoggingOptionsRequest {  }
```

<p>The input for the GetLoggingOptions operation.</p>

#### `GetLoggingOptionsResponse`

``` purescript
newtype GetLoggingOptionsResponse
  = GetLoggingOptionsResponse { "RoleArn'" :: NullOrUndefined (AwsArn), "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>The output from the GetLoggingOptions operation.</p>

#### `GetOTAUpdateRequest`

``` purescript
newtype GetOTAUpdateRequest
  = GetOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId }
```

#### `GetOTAUpdateResponse`

``` purescript
newtype GetOTAUpdateResponse
  = GetOTAUpdateResponse { "OtaUpdateInfo'" :: NullOrUndefined (OTAUpdateInfo) }
```

#### `GetPolicyRequest`

``` purescript
newtype GetPolicyRequest
  = GetPolicyRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the GetPolicy operation.</p>

#### `GetPolicyResponse`

``` purescript
newtype GetPolicyResponse
  = GetPolicyResponse { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "DefaultVersionId'" :: NullOrUndefined (PolicyVersionId) }
```

<p>The output from the GetPolicy operation.</p>

#### `GetPolicyVersionRequest`

``` purescript
newtype GetPolicyVersionRequest
  = GetPolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the GetPolicyVersion operation.</p>

#### `GetPolicyVersionResponse`

``` purescript
newtype GetPolicyVersionResponse
  = GetPolicyVersionResponse { "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion) }
```

<p>The output from the GetPolicyVersion operation.</p>

#### `GetRegistrationCodeRequest`

``` purescript
newtype GetRegistrationCodeRequest
  = GetRegistrationCodeRequest {  }
```

<p>The input to the GetRegistrationCode operation.</p>

#### `GetRegistrationCodeResponse`

``` purescript
newtype GetRegistrationCodeResponse
  = GetRegistrationCodeResponse { "RegistrationCode'" :: NullOrUndefined (RegistrationCode) }
```

<p>The output from the GetRegistrationCode operation.</p>

#### `GetTopicRuleRequest`

``` purescript
newtype GetTopicRuleRequest
  = GetTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the GetTopicRule operation.</p>

#### `GetTopicRuleResponse`

``` purescript
newtype GetTopicRuleResponse
  = GetTopicRuleResponse { "RuleArn'" :: NullOrUndefined (RuleArn), "Rule'" :: NullOrUndefined (TopicRule) }
```

<p>The output from the GetTopicRule operation.</p>

#### `GetV2LoggingOptionsRequest`

``` purescript
newtype GetV2LoggingOptionsRequest
  = GetV2LoggingOptionsRequest {  }
```

#### `GetV2LoggingOptionsResponse`

``` purescript
newtype GetV2LoggingOptionsResponse
  = GetV2LoggingOptionsResponse { "RoleArn'" :: NullOrUndefined (AwsArn), "DefaultLogLevel'" :: NullOrUndefined (LogLevel), "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs) }
```

#### `GroupNameAndArn`

``` purescript
newtype GroupNameAndArn
  = GroupNameAndArn { "GroupName'" :: NullOrUndefined (ThingGroupName), "GroupArn'" :: NullOrUndefined (ThingGroupArn) }
```

<p>The name and ARN of a group.</p>

#### `HashAlgorithm`

``` purescript
newtype HashAlgorithm
  = HashAlgorithm String
```

#### `HashKeyField`

``` purescript
newtype HashKeyField
  = HashKeyField String
```

#### `HashKeyValue`

``` purescript
newtype HashKeyValue
  = HashKeyValue String
```

#### `ImplicitDeny`

``` purescript
newtype ImplicitDeny
  = ImplicitDeny { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.</p>

#### `InProgressThings`

``` purescript
newtype InProgressThings
  = InProgressThings Int
```

#### `IndexName`

``` purescript
newtype IndexName
  = IndexName String
```

#### `IndexNamesList`

``` purescript
newtype IndexNamesList
  = IndexNamesList (Array IndexName)
```

#### `IndexNotReadyException`

``` purescript
newtype IndexNotReadyException
  = IndexNotReadyException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The index is not ready.</p>

#### `IndexSchema`

``` purescript
newtype IndexSchema
  = IndexSchema String
```

#### `IndexStatus`

``` purescript
newtype IndexStatus
  = IndexStatus String
```

#### `InlineDocument`

``` purescript
newtype InlineDocument
  = InlineDocument String
```

#### `InternalException`

``` purescript
newtype InternalException
  = InternalException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An unexpected error has occurred.</p>

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An unexpected error has occurred.</p>

#### `InvalidQueryException`

``` purescript
newtype InvalidQueryException
  = InvalidQueryException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The query is invalid.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The request is not valid.</p>

#### `InvalidResponseException`

``` purescript
newtype InvalidResponseException
  = InvalidResponseException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The response is invalid.</p>

#### `IsAuthenticated`

``` purescript
newtype IsAuthenticated
  = IsAuthenticated Boolean
```

#### `IsDefaultVersion`

``` purescript
newtype IsDefaultVersion
  = IsDefaultVersion Boolean
```

#### `IsDisabled`

``` purescript
newtype IsDisabled
  = IsDisabled Boolean
```

#### `Job`

``` purescript
newtype Job
  = Job { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "TargetSelection'" :: NullOrUndefined (TargetSelection), "Status'" :: NullOrUndefined (JobStatus), "Comment'" :: NullOrUndefined (Comment), "Targets'" :: NullOrUndefined (JobTargets), "Description'" :: NullOrUndefined (JobDescription), "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig), "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "CompletedAt'" :: NullOrUndefined (DateType), "JobProcessDetails'" :: NullOrUndefined (JobProcessDetails), "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters) }
```

<p>The <code>Job</code> object contains details about a job.</p>

#### `JobArn`

``` purescript
newtype JobArn
  = JobArn String
```

#### `JobDescription`

``` purescript
newtype JobDescription
  = JobDescription String
```

#### `JobDocument`

``` purescript
newtype JobDocument
  = JobDocument String
```

#### `JobDocumentParameters`

``` purescript
newtype JobDocumentParameters
  = JobDocumentParameters (Map ParameterKey ParameterValue)
```

#### `JobDocumentSource`

``` purescript
newtype JobDocumentSource
  = JobDocumentSource String
```

#### `JobExecution`

``` purescript
newtype JobExecution
  = JobExecution { "JobId'" :: NullOrUndefined (JobId), "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (JobExecutionStatusDetails), "ThingArn'" :: NullOrUndefined (ThingArn), "QueuedAt'" :: NullOrUndefined (DateType), "StartedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>The job execution object represents the execution of a job on a particular device.</p>

#### `JobExecutionStatus`

``` purescript
newtype JobExecutionStatus
  = JobExecutionStatus String
```

#### `JobExecutionStatusDetails`

``` purescript
newtype JobExecutionStatusDetails
  = JobExecutionStatusDetails { "DetailsMap'" :: NullOrUndefined (DetailsMap) }
```

<p>Details of the job execution status.</p>

#### `JobExecutionSummary`

``` purescript
newtype JobExecutionSummary
  = JobExecutionSummary { "Status'" :: NullOrUndefined (JobExecutionStatus), "QueuedAt'" :: NullOrUndefined (DateType), "StartedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>The job execution summary.</p>

#### `JobExecutionSummaryForJob`

``` purescript
newtype JobExecutionSummaryForJob
  = JobExecutionSummaryForJob { "ThingArn'" :: NullOrUndefined (ThingArn), "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary) }
```

<p>Contains a summary of information about job executions for a specific job.</p>

#### `JobExecutionSummaryForJobList`

``` purescript
newtype JobExecutionSummaryForJobList
  = JobExecutionSummaryForJobList (Array JobExecutionSummaryForJob)
```

#### `JobExecutionSummaryForThing`

``` purescript
newtype JobExecutionSummaryForThing
  = JobExecutionSummaryForThing { "JobId'" :: NullOrUndefined (JobId), "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary) }
```

<p>The job execution summary for a thing.</p>

#### `JobExecutionSummaryForThingList`

``` purescript
newtype JobExecutionSummaryForThingList
  = JobExecutionSummaryForThingList (Array JobExecutionSummaryForThing)
```

#### `JobExecutionsRolloutConfig`

``` purescript
newtype JobExecutionsRolloutConfig
  = JobExecutionsRolloutConfig { "MaximumPerMinute'" :: NullOrUndefined (MaxJobExecutionsPerMin) }
```

<p>Allows you to create a staged rollout of a job.</p>

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

#### `JobProcessDetails`

``` purescript
newtype JobProcessDetails
  = JobProcessDetails { "ProcessingTargets'" :: NullOrUndefined (ProcessingTargetNameList), "NumberOfCanceledThings'" :: NullOrUndefined (CanceledThings), "NumberOfSucceededThings'" :: NullOrUndefined (SucceededThings), "NumberOfFailedThings'" :: NullOrUndefined (FailedThings), "NumberOfRejectedThings'" :: NullOrUndefined (RejectedThings), "NumberOfQueuedThings'" :: NullOrUndefined (QueuedThings), "NumberOfInProgressThings'" :: NullOrUndefined (InProgressThings), "NumberOfRemovedThings'" :: NullOrUndefined (RemovedThings) }
```

<p>The job process details.</p>

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

#### `JobSummary`

``` purescript
newtype JobSummary
  = JobSummary { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "ThingGroupId'" :: NullOrUndefined (ThingGroupId), "TargetSelection'" :: NullOrUndefined (TargetSelection), "Status'" :: NullOrUndefined (JobStatus), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "CompletedAt'" :: NullOrUndefined (DateType) }
```

<p>The job summary.</p>

#### `JobSummaryList`

``` purescript
newtype JobSummaryList
  = JobSummaryList (Array JobSummary)
```

#### `JobTargets`

``` purescript
newtype JobTargets
  = JobTargets (Array TargetArn)
```

#### `JsonDocument`

``` purescript
newtype JsonDocument
  = JsonDocument String
```

#### `Key`

``` purescript
newtype Key
  = Key String
```

#### `KeyName`

``` purescript
newtype KeyName
  = KeyName String
```

#### `KeyPair`

``` purescript
newtype KeyPair
  = KeyPair { "PublicKey" :: NullOrUndefined (PublicKey), "PrivateKey" :: NullOrUndefined (PrivateKey) }
```

<p>Describes a key pair.</p>

#### `KeyValue`

``` purescript
newtype KeyValue
  = KeyValue String
```

#### `KinesisAction`

``` purescript
newtype KinesisAction
  = KinesisAction { "RoleArn'" :: AwsArn, "StreamName'" :: StreamName, "PartitionKey'" :: NullOrUndefined (PartitionKey) }
```

<p>Describes an action to write data to an Amazon Kinesis stream.</p>

#### `LambdaAction`

``` purescript
newtype LambdaAction
  = LambdaAction { "FunctionArn'" :: FunctionArn }
```

<p>Describes an action to invoke a Lambda function.</p>

#### `LaserMaxResults`

``` purescript
newtype LaserMaxResults
  = LaserMaxResults Int
```

#### `LastModifiedDate`

``` purescript
newtype LastModifiedDate
  = LastModifiedDate Number
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The number of attached entities exceeds the limit.</p>

#### `ListAttachedPoliciesRequest`

``` purescript
newtype ListAttachedPoliciesRequest
  = ListAttachedPoliciesRequest { "Target'" :: PolicyTarget, "Recursive'" :: NullOrUndefined (Recursive), "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize) }
```

#### `ListAttachedPoliciesResponse`

``` purescript
newtype ListAttachedPoliciesResponse
  = ListAttachedPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

#### `ListAuthorizersRequest`

``` purescript
newtype ListAuthorizersRequest
  = ListAuthorizersRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder), "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

#### `ListAuthorizersResponse`

``` purescript
newtype ListAuthorizersResponse
  = ListAuthorizersResponse { "Authorizers'" :: NullOrUndefined (Authorizers), "NextMarker'" :: NullOrUndefined (Marker) }
```

#### `ListCACertificatesRequest`

``` purescript
newtype ListCACertificatesRequest
  = ListCACertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>Input for the ListCACertificates operation.</p>

#### `ListCACertificatesResponse`

``` purescript
newtype ListCACertificatesResponse
  = ListCACertificatesResponse { "Certificates'" :: NullOrUndefined (CACertificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListCACertificates operation.</p>

#### `ListCertificatesByCARequest`

``` purescript
newtype ListCertificatesByCARequest
  = ListCertificatesByCARequest { "CaCertificateId'" :: CertificateId, "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input to the ListCertificatesByCA operation.</p>

#### `ListCertificatesByCAResponse`

``` purescript
newtype ListCertificatesByCAResponse
  = ListCertificatesByCAResponse { "Certificates'" :: NullOrUndefined (Certificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output of the ListCertificatesByCA operation.</p>

#### `ListCertificatesRequest`

``` purescript
newtype ListCertificatesRequest
  = ListCertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListCertificates operation.</p>

#### `ListCertificatesResponse`

``` purescript
newtype ListCertificatesResponse
  = ListCertificatesResponse { "Certificates'" :: NullOrUndefined (Certificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output of the ListCertificates operation.</p>

#### `ListIndicesRequest`

``` purescript
newtype ListIndicesRequest
  = ListIndicesRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (QueryMaxResults) }
```

#### `ListIndicesResponse`

``` purescript
newtype ListIndicesResponse
  = ListIndicesResponse { "IndexNames'" :: NullOrUndefined (IndexNamesList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListJobExecutionsForJobRequest`

``` purescript
newtype ListJobExecutionsForJobRequest
  = ListJobExecutionsForJobRequest { "JobId'" :: JobId, "Status'" :: NullOrUndefined (JobExecutionStatus), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListJobExecutionsForJobResponse`

``` purescript
newtype ListJobExecutionsForJobResponse
  = ListJobExecutionsForJobResponse { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForJobList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListJobExecutionsForThingRequest`

``` purescript
newtype ListJobExecutionsForThingRequest
  = ListJobExecutionsForThingRequest { "ThingName'" :: ThingName, "Status'" :: NullOrUndefined (JobExecutionStatus), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListJobExecutionsForThingResponse`

``` purescript
newtype ListJobExecutionsForThingResponse
  = ListJobExecutionsForThingResponse { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForThingList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "Status'" :: NullOrUndefined (JobStatus), "TargetSelection'" :: NullOrUndefined (TargetSelection), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken), "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupId'" :: NullOrUndefined (ThingGroupId) }
```

#### `ListJobsResponse`

``` purescript
newtype ListJobsResponse
  = ListJobsResponse { "Jobs'" :: NullOrUndefined (JobSummaryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListOTAUpdatesRequest`

``` purescript
newtype ListOTAUpdatesRequest
  = ListOTAUpdatesRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus) }
```

#### `ListOTAUpdatesResponse`

``` purescript
newtype ListOTAUpdatesResponse
  = ListOTAUpdatesResponse { "OtaUpdates'" :: NullOrUndefined (OTAUpdatesSummary), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListOutgoingCertificatesRequest`

``` purescript
newtype ListOutgoingCertificatesRequest
  = ListOutgoingCertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input to the ListOutgoingCertificates operation.</p>

#### `ListOutgoingCertificatesResponse`

``` purescript
newtype ListOutgoingCertificatesResponse
  = ListOutgoingCertificatesResponse { "OutgoingCertificates'" :: NullOrUndefined (OutgoingCertificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListOutgoingCertificates operation.</p>

#### `ListPoliciesRequest`

``` purescript
newtype ListPoliciesRequest
  = ListPoliciesRequest { "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPolicies operation.</p>

#### `ListPoliciesResponse`

``` purescript
newtype ListPoliciesResponse
  = ListPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPolicies operation.</p>

#### `ListPolicyPrincipalsRequest`

``` purescript
newtype ListPolicyPrincipalsRequest
  = ListPolicyPrincipalsRequest { "PolicyName'" :: PolicyName, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPolicyPrincipals operation.</p>

#### `ListPolicyPrincipalsResponse`

``` purescript
newtype ListPolicyPrincipalsResponse
  = ListPolicyPrincipalsResponse { "Principals'" :: NullOrUndefined (Principals), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPolicyPrincipals operation.</p>

#### `ListPolicyVersionsRequest`

``` purescript
newtype ListPolicyVersionsRequest
  = ListPolicyVersionsRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the ListPolicyVersions operation.</p>

#### `ListPolicyVersionsResponse`

``` purescript
newtype ListPolicyVersionsResponse
  = ListPolicyVersionsResponse { "PolicyVersions'" :: NullOrUndefined (PolicyVersions) }
```

<p>The output from the ListPolicyVersions operation.</p>

#### `ListPrincipalPoliciesRequest`

``` purescript
newtype ListPrincipalPoliciesRequest
  = ListPrincipalPoliciesRequest { "Principal'" :: Principal, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPrincipalPolicies operation.</p>

#### `ListPrincipalPoliciesResponse`

``` purescript
newtype ListPrincipalPoliciesResponse
  = ListPrincipalPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPrincipalPolicies operation.</p>

#### `ListPrincipalThingsRequest`

``` purescript
newtype ListPrincipalThingsRequest
  = ListPrincipalThingsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "Principal'" :: Principal }
```

<p>The input for the ListPrincipalThings operation.</p>

#### `ListPrincipalThingsResponse`

``` purescript
newtype ListPrincipalThingsResponse
  = ListPrincipalThingsResponse { "Things'" :: NullOrUndefined (ThingNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListPrincipalThings operation.</p>

#### `ListRoleAliasesRequest`

``` purescript
newtype ListRoleAliasesRequest
  = ListRoleAliasesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

#### `ListRoleAliasesResponse`

``` purescript
newtype ListRoleAliasesResponse
  = ListRoleAliasesResponse { "RoleAliases'" :: NullOrUndefined (RoleAliases), "NextMarker'" :: NullOrUndefined (Marker) }
```

#### `ListStreamsRequest`

``` purescript
newtype ListStreamsRequest
  = ListStreamsRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

#### `ListStreamsResponse`

``` purescript
newtype ListStreamsResponse
  = ListStreamsResponse { "Streams'" :: NullOrUndefined (StreamsSummary), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListTargetsForPolicyRequest`

``` purescript
newtype ListTargetsForPolicyRequest
  = ListTargetsForPolicyRequest { "PolicyName'" :: PolicyName, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize) }
```

#### `ListTargetsForPolicyResponse`

``` purescript
newtype ListTargetsForPolicyResponse
  = ListTargetsForPolicyResponse { "Targets'" :: NullOrUndefined (PolicyTargets), "NextMarker'" :: NullOrUndefined (Marker) }
```

#### `ListThingGroupsForThingRequest`

``` purescript
newtype ListThingGroupsForThingRequest
  = ListThingGroupsForThingRequest { "ThingName'" :: ThingName, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

#### `ListThingGroupsForThingResponse`

``` purescript
newtype ListThingGroupsForThingResponse
  = ListThingGroupsForThingResponse { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListThingGroupsRequest`

``` purescript
newtype ListThingGroupsRequest
  = ListThingGroupsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "ParentGroup'" :: NullOrUndefined (ThingGroupName), "NamePrefixFilter'" :: NullOrUndefined (ThingGroupName), "Recursive'" :: NullOrUndefined (RecursiveWithoutDefault) }
```

#### `ListThingGroupsResponse`

``` purescript
newtype ListThingGroupsResponse
  = ListThingGroupsResponse { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListThingPrincipalsRequest`

``` purescript
newtype ListThingPrincipalsRequest
  = ListThingPrincipalsRequest { "ThingName'" :: ThingName }
```

<p>The input for the ListThingPrincipal operation.</p>

#### `ListThingPrincipalsResponse`

``` purescript
newtype ListThingPrincipalsResponse
  = ListThingPrincipalsResponse { "Principals'" :: NullOrUndefined (Principals) }
```

<p>The output from the ListThingPrincipals operation.</p>

#### `ListThingRegistrationTaskReportsRequest`

``` purescript
newtype ListThingRegistrationTaskReportsRequest
  = ListThingRegistrationTaskReportsRequest { "TaskId'" :: TaskId, "ReportType'" :: ReportType, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

#### `ListThingRegistrationTaskReportsResponse`

``` purescript
newtype ListThingRegistrationTaskReportsResponse
  = ListThingRegistrationTaskReportsResponse { "ResourceLinks'" :: NullOrUndefined (S3FileUrlList), "ReportType'" :: NullOrUndefined (ReportType), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListThingRegistrationTasksRequest`

``` purescript
newtype ListThingRegistrationTasksRequest
  = ListThingRegistrationTasksRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "Status'" :: NullOrUndefined (Status) }
```

#### `ListThingRegistrationTasksResponse`

``` purescript
newtype ListThingRegistrationTasksResponse
  = ListThingRegistrationTasksResponse { "TaskIds'" :: NullOrUndefined (TaskIdList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListThingTypesRequest`

``` purescript
newtype ListThingTypesRequest
  = ListThingTypesRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "ThingTypeName'" :: NullOrUndefined (ThingTypeName) }
```

<p>The input for the ListThingTypes operation.</p>

#### `ListThingTypesResponse`

``` purescript
newtype ListThingTypesResponse
  = ListThingTypesResponse { "ThingTypes'" :: NullOrUndefined (ThingTypeList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output for the ListThingTypes operation.</p>

#### `ListThingsInThingGroupRequest`

``` purescript
newtype ListThingsInThingGroupRequest
  = ListThingsInThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "Recursive'" :: NullOrUndefined (Recursive), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

#### `ListThingsInThingGroupResponse`

``` purescript
newtype ListThingsInThingGroupResponse
  = ListThingsInThingGroupResponse { "Things'" :: NullOrUndefined (ThingNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListThingsRequest`

``` purescript
newtype ListThingsRequest
  = ListThingsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "AttributeName'" :: NullOrUndefined (AttributeName), "AttributeValue'" :: NullOrUndefined (AttributeValue), "ThingTypeName'" :: NullOrUndefined (ThingTypeName) }
```

<p>The input for the ListThings operation.</p>

#### `ListThingsResponse`

``` purescript
newtype ListThingsResponse
  = ListThingsResponse { "Things'" :: NullOrUndefined (ThingAttributeList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListThings operation.</p>

#### `ListTopicRulesRequest`

``` purescript
newtype ListTopicRulesRequest
  = ListTopicRulesRequest { "Topic'" :: NullOrUndefined (Topic), "MaxResults'" :: NullOrUndefined (GEMaxResults), "NextToken'" :: NullOrUndefined (NextToken), "RuleDisabled'" :: NullOrUndefined (IsDisabled) }
```

<p>The input for the ListTopicRules operation.</p>

#### `ListTopicRulesResponse`

``` purescript
newtype ListTopicRulesResponse
  = ListTopicRulesResponse { "Rules'" :: NullOrUndefined (TopicRuleList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListTopicRules operation.</p>

#### `ListV2LoggingLevelsRequest`

``` purescript
newtype ListV2LoggingLevelsRequest
  = ListV2LoggingLevelsRequest { "TargetType'" :: NullOrUndefined (LogTargetType), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (SkyfallMaxResults) }
```

#### `ListV2LoggingLevelsResponse`

``` purescript
newtype ListV2LoggingLevelsResponse
  = ListV2LoggingLevelsResponse { "LogTargetConfigurations'" :: NullOrUndefined (LogTargetConfigurations), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `LogLevel`

``` purescript
newtype LogLevel
  = LogLevel String
```

#### `LogTarget`

``` purescript
newtype LogTarget
  = LogTarget { "TargetType'" :: LogTargetType, "TargetName'" :: NullOrUndefined (LogTargetName) }
```

<p>A log target.</p>

#### `LogTargetConfiguration`

``` purescript
newtype LogTargetConfiguration
  = LogTargetConfiguration { "LogTarget'" :: NullOrUndefined (LogTarget), "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>The target configuration.</p>

#### `LogTargetConfigurations`

``` purescript
newtype LogTargetConfigurations
  = LogTargetConfigurations (Array LogTargetConfiguration)
```

#### `LogTargetName`

``` purescript
newtype LogTargetName
  = LogTargetName String
```

#### `LogTargetType`

``` purescript
newtype LogTargetType
  = LogTargetType String
```

#### `LoggingOptionsPayload`

``` purescript
newtype LoggingOptionsPayload
  = LoggingOptionsPayload { "RoleArn'" :: AwsArn, "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>Describes the logging options payload.</p>

#### `MalformedPolicyException`

``` purescript
newtype MalformedPolicyException
  = MalformedPolicyException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The policy documentation is not valid.</p>

#### `Marker`

``` purescript
newtype Marker
  = Marker String
```

#### `MaxJobExecutionsPerMin`

``` purescript
newtype MaxJobExecutionsPerMin
  = MaxJobExecutionsPerMin Int
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `MessageFormat`

``` purescript
newtype MessageFormat
  = MessageFormat String
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

#### `MetricNamespace`

``` purescript
newtype MetricNamespace
  = MetricNamespace String
```

#### `MetricTimestamp`

``` purescript
newtype MetricTimestamp
  = MetricTimestamp String
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

#### `MetricValue`

``` purescript
newtype MetricValue
  = MetricValue String
```

#### `MissingContextValue`

``` purescript
newtype MissingContextValue
  = MissingContextValue String
```

#### `MissingContextValues`

``` purescript
newtype MissingContextValues
  = MissingContextValues (Array MissingContextValue)
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NotConfiguredException`

``` purescript
newtype NotConfiguredException
  = NotConfiguredException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The resource is not configured.</p>

#### `OTAUpdateArn`

``` purescript
newtype OTAUpdateArn
  = OTAUpdateArn String
```

#### `OTAUpdateDescription`

``` purescript
newtype OTAUpdateDescription
  = OTAUpdateDescription String
```

#### `OTAUpdateErrorMessage`

``` purescript
newtype OTAUpdateErrorMessage
  = OTAUpdateErrorMessage String
```

#### `OTAUpdateFile`

``` purescript
newtype OTAUpdateFile
  = OTAUpdateFile { "FileName'" :: NullOrUndefined (FileName), "FileVersion'" :: NullOrUndefined (OTAUpdateFileVersion), "FileSource'" :: NullOrUndefined (Stream), "CodeSigning'" :: NullOrUndefined (CodeSigning), "Attributes'" :: NullOrUndefined (AttributesMap) }
```

<p>Describes a file to be associated with an OTA update.</p>

#### `OTAUpdateFileVersion`

``` purescript
newtype OTAUpdateFileVersion
  = OTAUpdateFileVersion String
```

#### `OTAUpdateFiles`

``` purescript
newtype OTAUpdateFiles
  = OTAUpdateFiles (Array OTAUpdateFile)
```

#### `OTAUpdateId`

``` purescript
newtype OTAUpdateId
  = OTAUpdateId String
```

#### `OTAUpdateInfo`

``` purescript
newtype OTAUpdateInfo
  = OTAUpdateInfo { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType), "Description'" :: NullOrUndefined (OTAUpdateDescription), "Targets'" :: NullOrUndefined (Targets), "TargetSelection'" :: NullOrUndefined (TargetSelection), "OtaUpdateFiles'" :: NullOrUndefined (OTAUpdateFiles), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus), "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId), "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn), "ErrorInfo'" :: NullOrUndefined (ErrorInfo), "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap) }
```

<p>Information about an OTA update.</p>

#### `OTAUpdateStatus`

``` purescript
newtype OTAUpdateStatus
  = OTAUpdateStatus String
```

#### `OTAUpdateSummary`

``` purescript
newtype OTAUpdateSummary
  = OTAUpdateSummary { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>An OTA update summary.</p>

#### `OTAUpdatesSummary`

``` purescript
newtype OTAUpdatesSummary
  = OTAUpdatesSummary (Array OTAUpdateSummary)
```

#### `OptionalVersion`

``` purescript
newtype OptionalVersion
  = OptionalVersion Number
```

#### `OutgoingCertificate`

``` purescript
newtype OutgoingCertificate
  = OutgoingCertificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "TransferredTo'" :: NullOrUndefined (AwsAccountId), "TransferDate'" :: NullOrUndefined (DateType), "TransferMessage'" :: NullOrUndefined (Message), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>A certificate that has been transferred but not yet accepted.</p>

#### `OutgoingCertificates`

``` purescript
newtype OutgoingCertificates
  = OutgoingCertificates (Array OutgoingCertificate)
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter String
```

#### `ParameterKey`

``` purescript
newtype ParameterKey
  = ParameterKey String
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Map Parameter Value)
```

#### `PartitionKey`

``` purescript
newtype PartitionKey
  = PartitionKey String
```

#### `PayloadField`

``` purescript
newtype PayloadField
  = PayloadField String
```

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

#### `Policies`

``` purescript
newtype Policies
  = Policies (Array Policy)
```

#### `Policy`

``` purescript
newtype Policy
  = Policy { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn) }
```

<p>Describes an AWS IoT policy.</p>

#### `PolicyArn`

``` purescript
newtype PolicyArn
  = PolicyArn String
```

#### `PolicyDocument`

``` purescript
newtype PolicyDocument
  = PolicyDocument String
```

#### `PolicyDocuments`

``` purescript
newtype PolicyDocuments
  = PolicyDocuments (Array PolicyDocument)
```

#### `PolicyName`

``` purescript
newtype PolicyName
  = PolicyName String
```

#### `PolicyNames`

``` purescript
newtype PolicyNames
  = PolicyNames (Array PolicyName)
```

#### `PolicyTarget`

``` purescript
newtype PolicyTarget
  = PolicyTarget String
```

#### `PolicyTargets`

``` purescript
newtype PolicyTargets
  = PolicyTargets (Array PolicyTarget)
```

#### `PolicyVersion`

``` purescript
newtype PolicyVersion
  = PolicyVersion { "VersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion), "CreateDate'" :: NullOrUndefined (DateType) }
```

<p>Describes a policy version.</p>

#### `PolicyVersionId`

``` purescript
newtype PolicyVersionId
  = PolicyVersionId String
```

#### `PolicyVersions`

``` purescript
newtype PolicyVersions
  = PolicyVersions (Array PolicyVersion)
```

#### `PresignedUrlConfig`

``` purescript
newtype PresignedUrlConfig
  = PresignedUrlConfig { "RoleArn'" :: NullOrUndefined (RoleArn), "ExpiresInSec'" :: NullOrUndefined (ExpiresInSec) }
```

<p>Configuration for pre-signed S3 URLs.</p>

#### `Principal`

``` purescript
newtype Principal
  = Principal String
```

#### `PrincipalArn`

``` purescript
newtype PrincipalArn
  = PrincipalArn String
```

#### `PrincipalId`

``` purescript
newtype PrincipalId
  = PrincipalId String
```

#### `Principals`

``` purescript
newtype Principals
  = Principals (Array PrincipalArn)
```

#### `PrivateKey`

``` purescript
newtype PrivateKey
  = PrivateKey String
```

#### `ProcessingTargetName`

``` purescript
newtype ProcessingTargetName
  = ProcessingTargetName String
```

#### `ProcessingTargetNameList`

``` purescript
newtype ProcessingTargetNameList
  = ProcessingTargetNameList (Array ProcessingTargetName)
```

#### `PublicKey`

``` purescript
newtype PublicKey
  = PublicKey String
```

#### `PublicKeyMap`

``` purescript
newtype PublicKeyMap
  = PublicKeyMap (Map KeyName KeyValue)
```

#### `PutItemInput`

``` purescript
newtype PutItemInput
  = PutItemInput { "TableName'" :: TableName }
```

<p>The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.</p>

#### `QueryMaxResults`

``` purescript
newtype QueryMaxResults
  = QueryMaxResults Int
```

#### `QueryString`

``` purescript
newtype QueryString
  = QueryString String
```

#### `QueryVersion`

``` purescript
newtype QueryVersion
  = QueryVersion String
```

#### `QueueUrl`

``` purescript
newtype QueueUrl
  = QueueUrl String
```

#### `QueuedThings`

``` purescript
newtype QueuedThings
  = QueuedThings Int
```

#### `RangeKeyField`

``` purescript
newtype RangeKeyField
  = RangeKeyField String
```

#### `RangeKeyValue`

``` purescript
newtype RangeKeyValue
  = RangeKeyValue String
```

#### `Recursive`

``` purescript
newtype Recursive
  = Recursive Boolean
```

#### `RecursiveWithoutDefault`

``` purescript
newtype RecursiveWithoutDefault
  = RecursiveWithoutDefault Boolean
```

#### `RegisterCACertificateRequest`

``` purescript
newtype RegisterCACertificateRequest
  = RegisterCACertificateRequest { "CaCertificate'" :: CertificatePem, "VerificationCertificate'" :: CertificatePem, "SetAsActive'" :: NullOrUndefined (SetAsActive), "AllowAutoRegistration'" :: NullOrUndefined (AllowAutoRegistration), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig) }
```

<p>The input to the RegisterCACertificate operation.</p>

#### `RegisterCACertificateResponse`

``` purescript
newtype RegisterCACertificateResponse
  = RegisterCACertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId) }
```

<p>The output from the RegisterCACertificateResponse operation.</p>

#### `RegisterCertificateRequest`

``` purescript
newtype RegisterCertificateRequest
  = RegisterCertificateRequest { "CertificatePem'" :: CertificatePem, "CaCertificatePem'" :: NullOrUndefined (CertificatePem), "SetAsActive'" :: NullOrUndefined (SetAsActiveFlag), "Status'" :: NullOrUndefined (CertificateStatus) }
```

<p>The input to the RegisterCertificate operation.</p>

#### `RegisterCertificateResponse`

``` purescript
newtype RegisterCertificateResponse
  = RegisterCertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId) }
```

<p>The output from the RegisterCertificate operation.</p>

#### `RegisterThingRequest`

``` purescript
newtype RegisterThingRequest
  = RegisterThingRequest { "TemplateBody'" :: TemplateBody, "Parameters'" :: NullOrUndefined (Parameters) }
```

#### `RegisterThingResponse`

``` purescript
newtype RegisterThingResponse
  = RegisterThingResponse { "CertificatePem'" :: NullOrUndefined (CertificatePem), "ResourceArns'" :: NullOrUndefined (ResourceArns) }
```

#### `RegistrationCode`

``` purescript
newtype RegistrationCode
  = RegistrationCode String
```

#### `RegistrationCodeValidationException`

``` purescript
newtype RegistrationCodeValidationException
  = RegistrationCodeValidationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The registration code is invalid.</p>

#### `RegistrationConfig`

``` purescript
newtype RegistrationConfig
  = RegistrationConfig { "TemplateBody'" :: NullOrUndefined (TemplateBody), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>The registration configuration.</p>

#### `RegistryMaxResults`

``` purescript
newtype RegistryMaxResults
  = RegistryMaxResults Int
```

#### `RegistryS3BucketName`

``` purescript
newtype RegistryS3BucketName
  = RegistryS3BucketName String
```

#### `RegistryS3KeyName`

``` purescript
newtype RegistryS3KeyName
  = RegistryS3KeyName String
```

#### `RejectCertificateTransferRequest`

``` purescript
newtype RejectCertificateTransferRequest
  = RejectCertificateTransferRequest { "CertificateId'" :: CertificateId, "RejectReason'" :: NullOrUndefined (Message) }
```

<p>The input for the RejectCertificateTransfer operation.</p>

#### `RejectedThings`

``` purescript
newtype RejectedThings
  = RejectedThings Int
```

#### `RemoveAutoRegistration`

``` purescript
newtype RemoveAutoRegistration
  = RemoveAutoRegistration Boolean
```

#### `RemoveThingFromThingGroupRequest`

``` purescript
newtype RemoveThingFromThingGroupRequest
  = RemoveThingFromThingGroupRequest { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn) }
```

#### `RemoveThingFromThingGroupResponse`

``` purescript
newtype RemoveThingFromThingGroupResponse
  = RemoveThingFromThingGroupResponse {  }
```

#### `RemoveThingType`

``` purescript
newtype RemoveThingType
  = RemoveThingType Boolean
```

#### `RemovedThings`

``` purescript
newtype RemovedThings
  = RemovedThings Int
```

#### `ReplaceTopicRuleRequest`

``` purescript
newtype ReplaceTopicRuleRequest
  = ReplaceTopicRuleRequest { "RuleName'" :: RuleName, "TopicRulePayload'" :: TopicRulePayload }
```

<p>The input for the ReplaceTopicRule operation.</p>

#### `ReportType`

``` purescript
newtype ReportType
  = ReportType String
```

#### `RepublishAction`

``` purescript
newtype RepublishAction
  = RepublishAction { "RoleArn'" :: AwsArn, "Topic'" :: TopicPattern }
```

<p>Describes an action to republish to another topic.</p>

#### `Resource`

``` purescript
newtype Resource
  = Resource String
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException { "Message'" :: NullOrUndefined (ErrorMessage'), "ResourceId'" :: NullOrUndefined (ResourceId'), "ResourceArn'" :: NullOrUndefined (ResourceArn') }
```

<p>The resource already exists.</p>

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

#### `ResourceArns`

``` purescript
newtype ResourceArns
  = ResourceArns (Map ResourceLogicalId ResourceArn)
```

#### `ResourceLogicalId`

``` purescript
newtype ResourceLogicalId
  = ResourceLogicalId String
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The specified resource does not exist.</p>

#### `ResourceRegistrationFailureException`

``` purescript
newtype ResourceRegistrationFailureException
  = ResourceRegistrationFailureException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The resource registration failed.</p>

#### `Resources`

``` purescript
newtype Resources
  = Resources (Array Resource)
```

#### `RoleAlias`

``` purescript
newtype RoleAlias
  = RoleAlias String
```

#### `RoleAliasArn`

``` purescript
newtype RoleAliasArn
  = RoleAliasArn String
```

#### `RoleAliasDescription`

``` purescript
newtype RoleAliasDescription
  = RoleAliasDescription { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleArn'" :: NullOrUndefined (RoleArn), "Owner'" :: NullOrUndefined (AwsAccountId), "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType) }
```

<p>Role alias description.</p>

#### `RoleAliases`

``` purescript
newtype RoleAliases
  = RoleAliases (Array RoleAlias)
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

#### `RuleArn`

``` purescript
newtype RuleArn
  = RuleArn String
```

#### `RuleName`

``` purescript
newtype RuleName
  = RuleName String
```

#### `S3Action`

``` purescript
newtype S3Action
  = S3Action { "RoleArn'" :: AwsArn, "BucketName'" :: BucketName, "Key'" :: Key, "CannedAcl'" :: NullOrUndefined (CannedAccessControlList) }
```

<p>Describes an action to write data to an Amazon S3 bucket.</p>

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

#### `S3FileUrl`

``` purescript
newtype S3FileUrl
  = S3FileUrl String
```

#### `S3FileUrlList`

``` purescript
newtype S3FileUrlList
  = S3FileUrlList (Array S3FileUrl)
```

#### `S3Key`

``` purescript
newtype S3Key
  = S3Key String
```

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "Bucket'" :: S3Bucket, "Key'" :: S3Key, "Version'" :: NullOrUndefined (S3Version) }
```

<p>The location in S3 the contains the files to stream.</p>

#### `S3Version`

``` purescript
newtype S3Version
  = S3Version String
```

#### `SQL`

``` purescript
newtype SQL
  = SQL String
```

#### `SalesforceAction`

``` purescript
newtype SalesforceAction
  = SalesforceAction { "Token'" :: SalesforceToken, "Url'" :: SalesforceEndpoint }
```

<p>Describes an action to write a message to a Salesforce IoT Cloud Input Stream.</p>

#### `SalesforceEndpoint`

``` purescript
newtype SalesforceEndpoint
  = SalesforceEndpoint String
```

#### `SalesforceToken`

``` purescript
newtype SalesforceToken
  = SalesforceToken String
```

#### `SearchIndexRequest`

``` purescript
newtype SearchIndexRequest
  = SearchIndexRequest { "IndexName'" :: NullOrUndefined (IndexName), "QueryString'" :: QueryString, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (QueryMaxResults), "QueryVersion'" :: NullOrUndefined (QueryVersion) }
```

#### `SearchIndexResponse`

``` purescript
newtype SearchIndexResponse
  = SearchIndexResponse { "NextToken'" :: NullOrUndefined (NextToken), "Things'" :: NullOrUndefined (ThingDocumentList) }
```

#### `SearchableAttributes`

``` purescript
newtype SearchableAttributes
  = SearchableAttributes (Array AttributeName)
```

#### `Seconds`

``` purescript
newtype Seconds
  = Seconds Int
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The service is temporarily unavailable.</p>

#### `SetAsActive`

``` purescript
newtype SetAsActive
  = SetAsActive Boolean
```

#### `SetAsActiveFlag`

``` purescript
newtype SetAsActiveFlag
  = SetAsActiveFlag Boolean
```

#### `SetAsDefault`

``` purescript
newtype SetAsDefault
  = SetAsDefault Boolean
```

#### `SetDefaultAuthorizerRequest`

``` purescript
newtype SetDefaultAuthorizerRequest
  = SetDefaultAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

#### `SetDefaultAuthorizerResponse`

``` purescript
newtype SetDefaultAuthorizerResponse
  = SetDefaultAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

#### `SetDefaultPolicyVersionRequest`

``` purescript
newtype SetDefaultPolicyVersionRequest
  = SetDefaultPolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the SetDefaultPolicyVersion operation.</p>

#### `SetLoggingOptionsRequest`

``` purescript
newtype SetLoggingOptionsRequest
  = SetLoggingOptionsRequest { "LoggingOptionsPayload'" :: LoggingOptionsPayload }
```

<p>The input for the SetLoggingOptions operation.</p>

#### `SetV2LoggingLevelRequest`

``` purescript
newtype SetV2LoggingLevelRequest
  = SetV2LoggingLevelRequest { "LogTarget'" :: LogTarget, "LogLevel'" :: LogLevel }
```

#### `SetV2LoggingOptionsRequest`

``` purescript
newtype SetV2LoggingOptionsRequest
  = SetV2LoggingOptionsRequest { "RoleArn'" :: NullOrUndefined (AwsArn), "DefaultLogLevel'" :: NullOrUndefined (LogLevel), "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs) }
```

#### `Signature`

``` purescript
newtype Signature
  = Signature String
```

#### `SignatureAlgorithm`

``` purescript
newtype SignatureAlgorithm
  = SignatureAlgorithm String
```

#### `SigningJobId`

``` purescript
newtype SigningJobId
  = SigningJobId String
```

#### `SkyfallMaxResults`

``` purescript
newtype SkyfallMaxResults
  = SkyfallMaxResults Int
```

#### `SnsAction`

``` purescript
newtype SnsAction
  = SnsAction { "TargetArn'" :: AwsArn, "RoleArn'" :: AwsArn, "MessageFormat'" :: NullOrUndefined (MessageFormat) }
```

<p>Describes an action to publish to an Amazon SNS topic.</p>

#### `SqlParseException`

``` purescript
newtype SqlParseException
  = SqlParseException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The Rule-SQL expression can't be parsed correctly.</p>

#### `SqsAction`

``` purescript
newtype SqsAction
  = SqsAction { "RoleArn'" :: AwsArn, "QueueUrl'" :: QueueUrl, "UseBase64'" :: NullOrUndefined (UseBase64) }
```

<p>Describes an action to publish data to an Amazon SQS queue.</p>

#### `StartThingRegistrationTaskRequest`

``` purescript
newtype StartThingRegistrationTaskRequest
  = StartThingRegistrationTaskRequest { "TemplateBody'" :: TemplateBody, "InputFileBucket'" :: RegistryS3BucketName, "InputFileKey'" :: RegistryS3KeyName, "RoleArn'" :: RoleArn }
```

#### `StartThingRegistrationTaskResponse`

``` purescript
newtype StartThingRegistrationTaskResponse
  = StartThingRegistrationTaskResponse { "TaskId'" :: NullOrUndefined (TaskId) }
```

#### `StateReason`

``` purescript
newtype StateReason
  = StateReason String
```

#### `StateValue`

``` purescript
newtype StateValue
  = StateValue String
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StopThingRegistrationTaskRequest`

``` purescript
newtype StopThingRegistrationTaskRequest
  = StopThingRegistrationTaskRequest { "TaskId'" :: TaskId }
```

#### `StopThingRegistrationTaskResponse`

``` purescript
newtype StopThingRegistrationTaskResponse
  = StopThingRegistrationTaskResponse {  }
```

#### `Stream`

``` purescript
newtype Stream
  = Stream { "StreamId'" :: NullOrUndefined (StreamId), "FileId'" :: NullOrUndefined (FileId) }
```

<p>Describes a group of files that can be streamed.</p>

#### `StreamArn`

``` purescript
newtype StreamArn
  = StreamArn String
```

#### `StreamDescription`

``` purescript
newtype StreamDescription
  = StreamDescription String
```

#### `StreamFile`

``` purescript
newtype StreamFile
  = StreamFile { "FileId'" :: NullOrUndefined (FileId), "S3Location'" :: NullOrUndefined (S3Location) }
```

<p>Represents a file to stream.</p>

#### `StreamFiles`

``` purescript
newtype StreamFiles
  = StreamFiles (Array StreamFile)
```

#### `StreamId`

``` purescript
newtype StreamId
  = StreamId String
```

#### `StreamInfo`

``` purescript
newtype StreamInfo
  = StreamInfo { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "StreamVersion'" :: NullOrUndefined (StreamVersion), "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: NullOrUndefined (StreamFiles), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>Information about a stream.</p>

#### `StreamName`

``` purescript
newtype StreamName
  = StreamName String
```

#### `StreamSummary`

``` purescript
newtype StreamSummary
  = StreamSummary { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "StreamVersion'" :: NullOrUndefined (StreamVersion), "Description'" :: NullOrUndefined (StreamDescription) }
```

<p>A summary of a stream.</p>

#### `StreamVersion`

``` purescript
newtype StreamVersion
  = StreamVersion Int
```

#### `StreamsSummary`

``` purescript
newtype StreamsSummary
  = StreamsSummary (Array StreamSummary)
```

#### `SucceededThings`

``` purescript
newtype SucceededThings
  = SucceededThings Int
```

#### `TableName`

``` purescript
newtype TableName
  = TableName String
```

#### `Target`

``` purescript
newtype Target
  = Target String
```

#### `TargetArn`

``` purescript
newtype TargetArn
  = TargetArn String
```

#### `TargetSelection`

``` purescript
newtype TargetSelection
  = TargetSelection String
```

#### `Targets`

``` purescript
newtype Targets
  = Targets (Array Target)
```

#### `TaskId`

``` purescript
newtype TaskId
  = TaskId String
```

#### `TaskIdList`

``` purescript
newtype TaskIdList
  = TaskIdList (Array TaskId)
```

#### `TemplateBody`

``` purescript
newtype TemplateBody
  = TemplateBody String
```

#### `TestAuthorizationRequest`

``` purescript
newtype TestAuthorizationRequest
  = TestAuthorizationRequest { "Principal'" :: NullOrUndefined (Principal), "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId), "AuthInfos'" :: AuthInfos, "ClientId'" :: NullOrUndefined (ClientId), "PolicyNamesToAdd'" :: NullOrUndefined (PolicyNames), "PolicyNamesToSkip'" :: NullOrUndefined (PolicyNames) }
```

#### `TestAuthorizationResponse`

``` purescript
newtype TestAuthorizationResponse
  = TestAuthorizationResponse { "AuthResults'" :: NullOrUndefined (AuthResults) }
```

#### `TestInvokeAuthorizerRequest`

``` purescript
newtype TestInvokeAuthorizerRequest
  = TestInvokeAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "Token'" :: Token, "TokenSignature'" :: TokenSignature }
```

#### `TestInvokeAuthorizerResponse`

``` purescript
newtype TestInvokeAuthorizerResponse
  = TestInvokeAuthorizerResponse { "IsAuthenticated'" :: NullOrUndefined (IsAuthenticated), "PrincipalId'" :: NullOrUndefined (PrincipalId), "PolicyDocuments'" :: NullOrUndefined (PolicyDocuments), "RefreshAfterInSeconds'" :: NullOrUndefined (Seconds), "DisconnectAfterInSeconds'" :: NullOrUndefined (Seconds) }
```

#### `ThingArn`

``` purescript
newtype ThingArn
  = ThingArn String
```

#### `ThingAttribute`

``` purescript
newtype ThingAttribute
  = ThingAttribute { "ThingName'" :: NullOrUndefined (ThingName), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingArn'" :: NullOrUndefined (ThingArn), "Attributes'" :: NullOrUndefined (Attributes), "Version'" :: NullOrUndefined (Version) }
```

<p>The properties of the thing, including thing name, thing type name, and a list of thing attributes.</p>

#### `ThingAttributeList`

``` purescript
newtype ThingAttributeList
  = ThingAttributeList (Array ThingAttribute)
```

#### `ThingDocument`

``` purescript
newtype ThingDocument
  = ThingDocument { "ThingName'" :: NullOrUndefined (ThingName), "ThingId'" :: NullOrUndefined (ThingId), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingGroupNames'" :: NullOrUndefined (ThingGroupNameList), "Attributes'" :: NullOrUndefined (Attributes), "Shadow'" :: NullOrUndefined (JsonDocument) }
```

<p>The thing search index document.</p>

#### `ThingDocumentList`

``` purescript
newtype ThingDocumentList
  = ThingDocumentList (Array ThingDocument)
```

#### `ThingGroupArn`

``` purescript
newtype ThingGroupArn
  = ThingGroupArn String
```

#### `ThingGroupDescription`

``` purescript
newtype ThingGroupDescription
  = ThingGroupDescription String
```

#### `ThingGroupId`

``` purescript
newtype ThingGroupId
  = ThingGroupId String
```

#### `ThingGroupList`

``` purescript
newtype ThingGroupList
  = ThingGroupList (Array ThingGroupName)
```

#### `ThingGroupMetadata`

``` purescript
newtype ThingGroupMetadata
  = ThingGroupMetadata { "ParentGroupName'" :: NullOrUndefined (ThingGroupName), "RootToParentThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "CreationDate'" :: NullOrUndefined (CreationDate) }
```

<p>Thing group metadata.</p>

#### `ThingGroupName`

``` purescript
newtype ThingGroupName
  = ThingGroupName String
```

#### `ThingGroupNameAndArnList`

``` purescript
newtype ThingGroupNameAndArnList
  = ThingGroupNameAndArnList (Array GroupNameAndArn)
```

#### `ThingGroupNameList`

``` purescript
newtype ThingGroupNameList
  = ThingGroupNameList (Array ThingGroupName)
```

#### `ThingGroupProperties`

``` purescript
newtype ThingGroupProperties
  = ThingGroupProperties { "ThingGroupDescription'" :: NullOrUndefined (ThingGroupDescription), "AttributePayload'" :: NullOrUndefined (AttributePayload) }
```

<p>Thing group properties.</p>

#### `ThingId`

``` purescript
newtype ThingId
  = ThingId String
```

#### `ThingIndexingConfiguration`

``` purescript
newtype ThingIndexingConfiguration
  = ThingIndexingConfiguration { "ThingIndexingMode'" :: NullOrUndefined (ThingIndexingMode) }
```

<p>Thing indexing configuration.</p>

#### `ThingIndexingMode`

``` purescript
newtype ThingIndexingMode
  = ThingIndexingMode String
```

#### `ThingName`

``` purescript
newtype ThingName
  = ThingName String
```

#### `ThingNameList`

``` purescript
newtype ThingNameList
  = ThingNameList (Array ThingName)
```

#### `ThingTypeArn`

``` purescript
newtype ThingTypeArn
  = ThingTypeArn String
```

#### `ThingTypeDefinition`

``` purescript
newtype ThingTypeDefinition
  = ThingTypeDefinition { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties), "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata) }
```

<p>The definition of the thing type, including thing type name and description.</p>

#### `ThingTypeDescription`

``` purescript
newtype ThingTypeDescription
  = ThingTypeDescription String
```

#### `ThingTypeId`

``` purescript
newtype ThingTypeId
  = ThingTypeId String
```

#### `ThingTypeList`

``` purescript
newtype ThingTypeList
  = ThingTypeList (Array ThingTypeDefinition)
```

#### `ThingTypeMetadata`

``` purescript
newtype ThingTypeMetadata
  = ThingTypeMetadata { "Deprecated'" :: NullOrUndefined (Boolean), "DeprecationDate'" :: NullOrUndefined (DeprecationDate), "CreationDate'" :: NullOrUndefined (CreationDate) }
```

<p>The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.</p>

#### `ThingTypeName`

``` purescript
newtype ThingTypeName
  = ThingTypeName String
```

#### `ThingTypeProperties`

``` purescript
newtype ThingTypeProperties
  = ThingTypeProperties { "ThingTypeDescription'" :: NullOrUndefined (ThingTypeDescription), "SearchableAttributes'" :: NullOrUndefined (SearchableAttributes) }
```

<p>The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.</p>

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The rate exceeds the limit.</p>

#### `Token`

``` purescript
newtype Token
  = Token String
```

#### `TokenKeyName`

``` purescript
newtype TokenKeyName
  = TokenKeyName String
```

#### `TokenSignature`

``` purescript
newtype TokenSignature
  = TokenSignature String
```

#### `Topic`

``` purescript
newtype Topic
  = Topic String
```

#### `TopicPattern`

``` purescript
newtype TopicPattern
  = TopicPattern String
```

#### `TopicRule`

``` purescript
newtype TopicRule
  = TopicRule { "RuleName'" :: NullOrUndefined (RuleName), "Sql'" :: NullOrUndefined (SQL), "Description'" :: NullOrUndefined (Description), "CreatedAt'" :: NullOrUndefined (CreatedAtDate), "Actions'" :: NullOrUndefined (ActionList), "RuleDisabled'" :: NullOrUndefined (IsDisabled), "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion), "ErrorAction'" :: NullOrUndefined (Action) }
```

<p>Describes a rule.</p>

#### `TopicRuleList`

``` purescript
newtype TopicRuleList
  = TopicRuleList (Array TopicRuleListItem)
```

#### `TopicRuleListItem`

``` purescript
newtype TopicRuleListItem
  = TopicRuleListItem { "RuleArn'" :: NullOrUndefined (RuleArn), "RuleName'" :: NullOrUndefined (RuleName), "TopicPattern'" :: NullOrUndefined (TopicPattern), "CreatedAt'" :: NullOrUndefined (CreatedAtDate), "RuleDisabled'" :: NullOrUndefined (IsDisabled) }
```

<p>Describes a rule.</p>

#### `TopicRulePayload`

``` purescript
newtype TopicRulePayload
  = TopicRulePayload { "Sql'" :: SQL, "Description'" :: NullOrUndefined (Description), "Actions'" :: ActionList, "RuleDisabled'" :: NullOrUndefined (IsDisabled), "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion), "ErrorAction'" :: NullOrUndefined (Action) }
```

<p>Describes a rule.</p>

#### `TransferAlreadyCompletedException`

``` purescript
newtype TransferAlreadyCompletedException
  = TransferAlreadyCompletedException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't revert the certificate transfer because the transfer is already complete.</p>

#### `TransferCertificateRequest`

``` purescript
newtype TransferCertificateRequest
  = TransferCertificateRequest { "CertificateId'" :: CertificateId, "TargetAwsAccount'" :: AwsAccountId, "TransferMessage'" :: NullOrUndefined (Message) }
```

<p>The input for the TransferCertificate operation.</p>

#### `TransferCertificateResponse`

``` purescript
newtype TransferCertificateResponse
  = TransferCertificateResponse { "TransferredCertificateArn'" :: NullOrUndefined (CertificateArn) }
```

<p>The output from the TransferCertificate operation.</p>

#### `TransferConflictException`

``` purescript
newtype TransferConflictException
  = TransferConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't transfer the certificate because authorization policies are still attached.</p>

#### `TransferData`

``` purescript
newtype TransferData
  = TransferData { "TransferMessage'" :: NullOrUndefined (Message), "RejectReason'" :: NullOrUndefined (Message), "TransferDate'" :: NullOrUndefined (DateType), "AcceptDate'" :: NullOrUndefined (DateType), "RejectDate'" :: NullOrUndefined (DateType) }
```

<p>Data used to transfer a certificate to an AWS account.</p>

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You are not authorized to perform this operation.</p>

#### `UndoDeprecate`

``` purescript
newtype UndoDeprecate
  = UndoDeprecate Boolean
```

#### `UpdateAuthorizerRequest`

``` purescript
newtype UpdateAuthorizerRequest
  = UpdateAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn), "TokenKeyName'" :: NullOrUndefined (TokenKeyName), "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap), "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

#### `UpdateAuthorizerResponse`

``` purescript
newtype UpdateAuthorizerResponse
  = UpdateAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

#### `UpdateCACertificateRequest`

``` purescript
newtype UpdateCACertificateRequest
  = UpdateCACertificateRequest { "CertificateId'" :: CertificateId, "NewStatus'" :: NullOrUndefined (CACertificateStatus), "NewAutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig), "RemoveAutoRegistration'" :: NullOrUndefined (RemoveAutoRegistration) }
```

<p>The input to the UpdateCACertificate operation.</p>

#### `UpdateCertificateRequest`

``` purescript
newtype UpdateCertificateRequest
  = UpdateCertificateRequest { "CertificateId'" :: CertificateId, "NewStatus'" :: CertificateStatus }
```

<p>The input for the UpdateCertificate operation.</p>

#### `UpdateEventConfigurationsRequest`

``` purescript
newtype UpdateEventConfigurationsRequest
  = UpdateEventConfigurationsRequest { "EventConfigurations'" :: NullOrUndefined (EventConfigurations) }
```

#### `UpdateEventConfigurationsResponse`

``` purescript
newtype UpdateEventConfigurationsResponse
  = UpdateEventConfigurationsResponse {  }
```

#### `UpdateIndexingConfigurationRequest`

``` purescript
newtype UpdateIndexingConfigurationRequest
  = UpdateIndexingConfigurationRequest { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration) }
```

#### `UpdateIndexingConfigurationResponse`

``` purescript
newtype UpdateIndexingConfigurationResponse
  = UpdateIndexingConfigurationResponse {  }
```

#### `UpdateRoleAliasRequest`

``` purescript
newtype UpdateRoleAliasRequest
  = UpdateRoleAliasRequest { "RoleAlias'" :: RoleAlias, "RoleArn'" :: NullOrUndefined (RoleArn), "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds) }
```

#### `UpdateRoleAliasResponse`

``` purescript
newtype UpdateRoleAliasResponse
  = UpdateRoleAliasResponse { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn) }
```

#### `UpdateStreamRequest`

``` purescript
newtype UpdateStreamRequest
  = UpdateStreamRequest { "StreamId'" :: StreamId, "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: NullOrUndefined (StreamFiles), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

#### `UpdateStreamResponse`

``` purescript
newtype UpdateStreamResponse
  = UpdateStreamResponse { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "Description'" :: NullOrUndefined (StreamDescription), "StreamVersion'" :: NullOrUndefined (StreamVersion) }
```

#### `UpdateThingGroupRequest`

``` purescript
newtype UpdateThingGroupRequest
  = UpdateThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ThingGroupProperties'" :: ThingGroupProperties, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

#### `UpdateThingGroupResponse`

``` purescript
newtype UpdateThingGroupResponse
  = UpdateThingGroupResponse { "Version'" :: NullOrUndefined (Version) }
```

#### `UpdateThingGroupsForThingRequest`

``` purescript
newtype UpdateThingGroupsForThingRequest
  = UpdateThingGroupsForThingRequest { "ThingName'" :: NullOrUndefined (ThingName), "ThingGroupsToAdd'" :: NullOrUndefined (ThingGroupList), "ThingGroupsToRemove'" :: NullOrUndefined (ThingGroupList) }
```

#### `UpdateThingGroupsForThingResponse`

``` purescript
newtype UpdateThingGroupsForThingResponse
  = UpdateThingGroupsForThingResponse {  }
```

#### `UpdateThingRequest`

``` purescript
newtype UpdateThingRequest
  = UpdateThingRequest { "ThingName'" :: ThingName, "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "AttributePayload'" :: NullOrUndefined (AttributePayload), "ExpectedVersion'" :: NullOrUndefined (OptionalVersion), "RemoveThingType'" :: NullOrUndefined (RemoveThingType) }
```

<p>The input for the UpdateThing operation.</p>

#### `UpdateThingResponse`

``` purescript
newtype UpdateThingResponse
  = UpdateThingResponse {  }
```

<p>The output from the UpdateThing operation.</p>

#### `UseBase64`

``` purescript
newtype UseBase64
  = UseBase64 Boolean
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

#### `Version`

``` purescript
newtype Version
  = Version Number
```

#### `VersionConflictException`

``` purescript
newtype VersionConflictException
  = VersionConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.</p>

#### `VersionsLimitExceededException`

``` purescript
newtype VersionsLimitExceededException
  = VersionsLimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The number of policy versions exceeds the limit.</p>

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

#### `ResourceArn'`

``` purescript
newtype ResourceArn'
  = ResourceArn' String
```

#### `ResourceId'`

``` purescript
newtype ResourceId'
  = ResourceId' String
```


