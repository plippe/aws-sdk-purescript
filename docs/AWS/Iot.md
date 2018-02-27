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

##### Instances
``` purescript
Newtype AcceptCertificateTransferRequest _
```

#### `Action`

``` purescript
newtype Action
  = Action { "DynamoDB'" :: NullOrUndefined (DynamoDBAction), "DynamoDBv2'" :: NullOrUndefined (DynamoDBv2Action), "Lambda'" :: NullOrUndefined (LambdaAction), "Sns'" :: NullOrUndefined (SnsAction), "Sqs'" :: NullOrUndefined (SqsAction), "Kinesis'" :: NullOrUndefined (KinesisAction), "Republish'" :: NullOrUndefined (RepublishAction), "S3'" :: NullOrUndefined (S3Action), "Firehose'" :: NullOrUndefined (FirehoseAction), "CloudwatchMetric'" :: NullOrUndefined (CloudwatchMetricAction), "CloudwatchAlarm'" :: NullOrUndefined (CloudwatchAlarmAction), "Elasticsearch'" :: NullOrUndefined (ElasticsearchAction), "Salesforce'" :: NullOrUndefined (SalesforceAction) }
```

<p>Describes the actions associated with a rule.</p>

##### Instances
``` purescript
Newtype Action _
```

#### `ActionList`

``` purescript
newtype ActionList
  = ActionList (Array Action)
```

##### Instances
``` purescript
Newtype ActionList _
```

#### `ActionType`

``` purescript
newtype ActionType
  = ActionType String
```

##### Instances
``` purescript
Newtype ActionType _
```

#### `AddThingToThingGroupRequest`

``` purescript
newtype AddThingToThingGroupRequest
  = AddThingToThingGroupRequest { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn) }
```

##### Instances
``` purescript
Newtype AddThingToThingGroupRequest _
```

#### `AddThingToThingGroupResponse`

``` purescript
newtype AddThingToThingGroupResponse
  = AddThingToThingGroupResponse {  }
```

##### Instances
``` purescript
Newtype AddThingToThingGroupResponse _
```

#### `AdditionalParameterMap`

``` purescript
newtype AdditionalParameterMap
  = AdditionalParameterMap (Map Key Value)
```

##### Instances
``` purescript
Newtype AdditionalParameterMap _
```

#### `AlarmName`

``` purescript
newtype AlarmName
  = AlarmName String
```

##### Instances
``` purescript
Newtype AlarmName _
```

#### `AllowAutoRegistration`

``` purescript
newtype AllowAutoRegistration
  = AllowAutoRegistration Boolean
```

##### Instances
``` purescript
Newtype AllowAutoRegistration _
```

#### `Allowed`

``` purescript
newtype Allowed
  = Allowed { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Contains information that allowed the authorization.</p>

##### Instances
``` purescript
Newtype Allowed _
```

#### `AscendingOrder`

``` purescript
newtype AscendingOrder
  = AscendingOrder Boolean
```

##### Instances
``` purescript
Newtype AscendingOrder _
```

#### `AssociateTargetsWithJobRequest`

``` purescript
newtype AssociateTargetsWithJobRequest
  = AssociateTargetsWithJobRequest { "Targets'" :: JobTargets, "JobId'" :: JobId, "Comment'" :: NullOrUndefined (Comment) }
```

##### Instances
``` purescript
Newtype AssociateTargetsWithJobRequest _
```

#### `AssociateTargetsWithJobResponse`

``` purescript
newtype AssociateTargetsWithJobResponse
  = AssociateTargetsWithJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

##### Instances
``` purescript
Newtype AssociateTargetsWithJobResponse _
```

#### `AttachPolicyRequest`

``` purescript
newtype AttachPolicyRequest
  = AttachPolicyRequest { "PolicyName'" :: PolicyName, "Target'" :: PolicyTarget }
```

##### Instances
``` purescript
Newtype AttachPolicyRequest _
```

#### `AttachPrincipalPolicyRequest`

``` purescript
newtype AttachPrincipalPolicyRequest
  = AttachPrincipalPolicyRequest { "PolicyName'" :: PolicyName, "Principal'" :: Principal }
```

<p>The input for the AttachPrincipalPolicy operation.</p>

##### Instances
``` purescript
Newtype AttachPrincipalPolicyRequest _
```

#### `AttachThingPrincipalRequest`

``` purescript
newtype AttachThingPrincipalRequest
  = AttachThingPrincipalRequest { "ThingName'" :: ThingName, "Principal'" :: Principal }
```

<p>The input for the AttachThingPrincipal operation.</p>

##### Instances
``` purescript
Newtype AttachThingPrincipalRequest _
```

#### `AttachThingPrincipalResponse`

``` purescript
newtype AttachThingPrincipalResponse
  = AttachThingPrincipalResponse {  }
```

<p>The output from the AttachThingPrincipal operation.</p>

##### Instances
``` purescript
Newtype AttachThingPrincipalResponse _
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

#### `AttributePayload`

``` purescript
newtype AttributePayload
  = AttributePayload { "Attributes'" :: NullOrUndefined (Attributes), "Merge'" :: NullOrUndefined (Flag) }
```

<p>The attribute payload.</p>

##### Instances
``` purescript
Newtype AttributePayload _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Map AttributeName AttributeValue)
```

##### Instances
``` purescript
Newtype Attributes _
```

#### `AttributesMap`

``` purescript
newtype AttributesMap
  = AttributesMap (Map Key Value)
```

##### Instances
``` purescript
Newtype AttributesMap _
```

#### `AuthDecision`

``` purescript
newtype AuthDecision
  = AuthDecision String
```

##### Instances
``` purescript
Newtype AuthDecision _
```

#### `AuthInfo`

``` purescript
newtype AuthInfo
  = AuthInfo { "ActionType'" :: NullOrUndefined (ActionType), "Resources'" :: NullOrUndefined (Resources) }
```

<p>A collection of authorization information.</p>

##### Instances
``` purescript
Newtype AuthInfo _
```

#### `AuthInfos`

``` purescript
newtype AuthInfos
  = AuthInfos (Array AuthInfo)
```

##### Instances
``` purescript
Newtype AuthInfos _
```

#### `AuthResult`

``` purescript
newtype AuthResult
  = AuthResult { "AuthInfo'" :: NullOrUndefined (AuthInfo), "Allowed'" :: NullOrUndefined (Allowed), "Denied'" :: NullOrUndefined (Denied), "AuthDecision'" :: NullOrUndefined (AuthDecision), "MissingContextValues'" :: NullOrUndefined (MissingContextValues) }
```

<p>The authorizer result.</p>

##### Instances
``` purescript
Newtype AuthResult _
```

#### `AuthResults`

``` purescript
newtype AuthResults
  = AuthResults (Array AuthResult)
```

##### Instances
``` purescript
Newtype AuthResults _
```

#### `AuthorizerArn`

``` purescript
newtype AuthorizerArn
  = AuthorizerArn String
```

##### Instances
``` purescript
Newtype AuthorizerArn _
```

#### `AuthorizerDescription`

``` purescript
newtype AuthorizerDescription
  = AuthorizerDescription { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn), "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn), "TokenKeyName'" :: NullOrUndefined (TokenKeyName), "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap), "Status'" :: NullOrUndefined (AuthorizerStatus), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType) }
```

<p>The authorizer description.</p>

##### Instances
``` purescript
Newtype AuthorizerDescription _
```

#### `AuthorizerFunctionArn`

``` purescript
newtype AuthorizerFunctionArn
  = AuthorizerFunctionArn String
```

##### Instances
``` purescript
Newtype AuthorizerFunctionArn _
```

#### `AuthorizerName`

``` purescript
newtype AuthorizerName
  = AuthorizerName String
```

##### Instances
``` purescript
Newtype AuthorizerName _
```

#### `AuthorizerStatus`

``` purescript
newtype AuthorizerStatus
  = AuthorizerStatus String
```

##### Instances
``` purescript
Newtype AuthorizerStatus _
```

#### `AuthorizerSummary`

``` purescript
newtype AuthorizerSummary
  = AuthorizerSummary { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

<p>The authorizer summary.</p>

##### Instances
``` purescript
Newtype AuthorizerSummary _
```

#### `Authorizers`

``` purescript
newtype Authorizers
  = Authorizers (Array AuthorizerSummary)
```

##### Instances
``` purescript
Newtype Authorizers _
```

#### `AutoRegistrationStatus`

``` purescript
newtype AutoRegistrationStatus
  = AutoRegistrationStatus String
```

##### Instances
``` purescript
Newtype AutoRegistrationStatus _
```

#### `AwsAccountId`

``` purescript
newtype AwsAccountId
  = AwsAccountId String
```

##### Instances
``` purescript
Newtype AwsAccountId _
```

#### `AwsArn`

``` purescript
newtype AwsArn
  = AwsArn String
```

##### Instances
``` purescript
Newtype AwsArn _
```

#### `AwsIotJobArn`

``` purescript
newtype AwsIotJobArn
  = AwsIotJobArn String
```

##### Instances
``` purescript
Newtype AwsIotJobArn _
```

#### `AwsIotJobId`

``` purescript
newtype AwsIotJobId
  = AwsIotJobId String
```

##### Instances
``` purescript
Newtype AwsIotJobId _
```

#### `AwsIotSqlVersion`

``` purescript
newtype AwsIotSqlVersion
  = AwsIotSqlVersion String
```

##### Instances
``` purescript
Newtype AwsIotSqlVersion _
```

#### `BucketName`

``` purescript
newtype BucketName
  = BucketName String
```

##### Instances
``` purescript
Newtype BucketName _
```

#### `CACertificate`

``` purescript
newtype CACertificate
  = CACertificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CACertificateStatus), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>A CA certificate.</p>

##### Instances
``` purescript
Newtype CACertificate _
```

#### `CACertificateDescription`

``` purescript
newtype CACertificateDescription
  = CACertificateDescription { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CACertificateStatus), "CertificatePem'" :: NullOrUndefined (CertificatePem), "OwnedBy'" :: NullOrUndefined (AwsAccountId), "CreationDate'" :: NullOrUndefined (DateType), "AutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus) }
```

<p>Describes a CA certificate.</p>

##### Instances
``` purescript
Newtype CACertificateDescription _
```

#### `CACertificateStatus`

``` purescript
newtype CACertificateStatus
  = CACertificateStatus String
```

##### Instances
``` purescript
Newtype CACertificateStatus _
```

#### `CACertificates`

``` purescript
newtype CACertificates
  = CACertificates (Array CACertificate)
```

##### Instances
``` purescript
Newtype CACertificates _
```

#### `CancelCertificateTransferRequest`

``` purescript
newtype CancelCertificateTransferRequest
  = CancelCertificateTransferRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the CancelCertificateTransfer operation.</p>

##### Instances
``` purescript
Newtype CancelCertificateTransferRequest _
```

#### `CancelJobRequest`

``` purescript
newtype CancelJobRequest
  = CancelJobRequest { "JobId'" :: JobId, "Comment'" :: NullOrUndefined (Comment) }
```

##### Instances
``` purescript
Newtype CancelJobRequest _
```

#### `CancelJobResponse`

``` purescript
newtype CancelJobResponse
  = CancelJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

##### Instances
``` purescript
Newtype CancelJobResponse _
```

#### `CanceledThings`

``` purescript
newtype CanceledThings
  = CanceledThings Int
```

##### Instances
``` purescript
Newtype CanceledThings _
```

#### `CannedAccessControlList`

``` purescript
newtype CannedAccessControlList
  = CannedAccessControlList String
```

##### Instances
``` purescript
Newtype CannedAccessControlList _
```

#### `Certificate`

``` purescript
newtype Certificate
  = Certificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CertificateStatus), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>Information about a certificate.</p>

##### Instances
``` purescript
Newtype Certificate _
```

#### `CertificateArn`

``` purescript
newtype CertificateArn
  = CertificateArn String
```

##### Instances
``` purescript
Newtype CertificateArn _
```

#### `CertificateConflictException`

``` purescript
newtype CertificateConflictException
  = CertificateConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.</p>

##### Instances
``` purescript
Newtype CertificateConflictException _
```

#### `CertificateDescription`

``` purescript
newtype CertificateDescription
  = CertificateDescription { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CaCertificateId'" :: NullOrUndefined (CertificateId), "Status'" :: NullOrUndefined (CertificateStatus), "CertificatePem'" :: NullOrUndefined (CertificatePem), "OwnedBy'" :: NullOrUndefined (AwsAccountId), "PreviousOwnedBy'" :: NullOrUndefined (AwsAccountId), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType), "TransferData'" :: NullOrUndefined (TransferData) }
```

<p>Describes a certificate.</p>

##### Instances
``` purescript
Newtype CertificateDescription _
```

#### `CertificateId`

``` purescript
newtype CertificateId
  = CertificateId String
```

##### Instances
``` purescript
Newtype CertificateId _
```

#### `CertificateName`

``` purescript
newtype CertificateName
  = CertificateName String
```

##### Instances
``` purescript
Newtype CertificateName _
```

#### `CertificatePem`

``` purescript
newtype CertificatePem
  = CertificatePem String
```

<p>The PEM of a certificate.</p>

##### Instances
``` purescript
Newtype CertificatePem _
```

#### `CertificateSigningRequest`

``` purescript
newtype CertificateSigningRequest
  = CertificateSigningRequest String
```

##### Instances
``` purescript
Newtype CertificateSigningRequest _
```

#### `CertificateStateException`

``` purescript
newtype CertificateStateException
  = CertificateStateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The certificate operation is not allowed.</p>

##### Instances
``` purescript
Newtype CertificateStateException _
```

#### `CertificateStatus`

``` purescript
newtype CertificateStatus
  = CertificateStatus String
```

##### Instances
``` purescript
Newtype CertificateStatus _
```

#### `CertificateValidationException`

``` purescript
newtype CertificateValidationException
  = CertificateValidationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The certificate is invalid.</p>

##### Instances
``` purescript
Newtype CertificateValidationException _
```

#### `Certificates`

``` purescript
newtype Certificates
  = Certificates (Array Certificate)
```

##### Instances
``` purescript
Newtype Certificates _
```

#### `ClearDefaultAuthorizerRequest`

``` purescript
newtype ClearDefaultAuthorizerRequest
  = ClearDefaultAuthorizerRequest {  }
```

##### Instances
``` purescript
Newtype ClearDefaultAuthorizerRequest _
```

#### `ClearDefaultAuthorizerResponse`

``` purescript
newtype ClearDefaultAuthorizerResponse
  = ClearDefaultAuthorizerResponse {  }
```

##### Instances
``` purescript
Newtype ClearDefaultAuthorizerResponse _
```

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

##### Instances
``` purescript
Newtype ClientId _
```

#### `CloudwatchAlarmAction`

``` purescript
newtype CloudwatchAlarmAction
  = CloudwatchAlarmAction { "RoleArn'" :: AwsArn, "AlarmName'" :: AlarmName, "StateReason'" :: StateReason, "StateValue'" :: StateValue }
```

<p>Describes an action that updates a CloudWatch alarm.</p>

##### Instances
``` purescript
Newtype CloudwatchAlarmAction _
```

#### `CloudwatchMetricAction`

``` purescript
newtype CloudwatchMetricAction
  = CloudwatchMetricAction { "RoleArn'" :: AwsArn, "MetricNamespace'" :: MetricNamespace, "MetricName'" :: MetricName, "MetricValue'" :: MetricValue, "MetricUnit'" :: MetricUnit, "MetricTimestamp'" :: NullOrUndefined (MetricTimestamp) }
```

<p>Describes an action that captures a CloudWatch metric.</p>

##### Instances
``` purescript
Newtype CloudwatchMetricAction _
```

#### `Code`

``` purescript
newtype Code
  = Code String
```

##### Instances
``` purescript
Newtype Code _
```

#### `CodeSigning`

``` purescript
newtype CodeSigning
  = CodeSigning { "AwsSignerJobId'" :: NullOrUndefined (SigningJobId), "CustomCodeSigning'" :: NullOrUndefined (CustomCodeSigning) }
```

<p>Describes the method to use when code signing a file.</p>

##### Instances
``` purescript
Newtype CodeSigning _
```

#### `CodeSigningCertificateChain`

``` purescript
newtype CodeSigningCertificateChain
  = CodeSigningCertificateChain { "Stream'" :: NullOrUndefined (Stream), "CertificateName'" :: NullOrUndefined (CertificateName), "InlineDocument'" :: NullOrUndefined (InlineDocument) }
```

<p>Describes the certificate chain being used when code signing a file.</p>

##### Instances
``` purescript
Newtype CodeSigningCertificateChain _
```

#### `CodeSigningSignature`

``` purescript
newtype CodeSigningSignature
  = CodeSigningSignature { "Stream'" :: NullOrUndefined (Stream), "InlineDocument'" :: NullOrUndefined (Signature) }
```

<p>Describes the signature for a file.</p>

##### Instances
``` purescript
Newtype CodeSigningSignature _
```

#### `CognitoIdentityPoolId`

``` purescript
newtype CognitoIdentityPoolId
  = CognitoIdentityPoolId String
```

##### Instances
``` purescript
Newtype CognitoIdentityPoolId _
```

#### `Comment`

``` purescript
newtype Comment
  = Comment String
```

##### Instances
``` purescript
Newtype Comment _
```

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration { "Enabled" :: NullOrUndefined (Enabled) }
```

<p>Configuration.</p>

##### Instances
``` purescript
Newtype Configuration _
```

#### `ConflictingResourceUpdateException`

``` purescript
newtype ConflictingResourceUpdateException
  = ConflictingResourceUpdateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.</p>

##### Instances
``` purescript
Newtype ConflictingResourceUpdateException _
```

#### `Count`

``` purescript
newtype Count
  = Count Int
```

##### Instances
``` purescript
Newtype Count _
```

#### `CreateAuthorizerRequest`

``` purescript
newtype CreateAuthorizerRequest
  = CreateAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "AuthorizerFunctionArn'" :: AuthorizerFunctionArn, "TokenKeyName'" :: TokenKeyName, "TokenSigningPublicKeys'" :: PublicKeyMap, "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

##### Instances
``` purescript
Newtype CreateAuthorizerRequest _
```

#### `CreateAuthorizerResponse`

``` purescript
newtype CreateAuthorizerResponse
  = CreateAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

##### Instances
``` purescript
Newtype CreateAuthorizerResponse _
```

#### `CreateCertificateFromCsrRequest`

``` purescript
newtype CreateCertificateFromCsrRequest
  = CreateCertificateFromCsrRequest { "CertificateSigningRequest'" :: CertificateSigningRequest, "SetAsActive'" :: NullOrUndefined (SetAsActive) }
```

<p>The input for the CreateCertificateFromCsr operation.</p>

##### Instances
``` purescript
Newtype CreateCertificateFromCsrRequest _
```

#### `CreateCertificateFromCsrResponse`

``` purescript
newtype CreateCertificateFromCsrResponse
  = CreateCertificateFromCsrResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CertificatePem'" :: NullOrUndefined (CertificatePem) }
```

<p>The output from the CreateCertificateFromCsr operation.</p>

##### Instances
``` purescript
Newtype CreateCertificateFromCsrResponse _
```

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "JobId'" :: JobId, "Targets'" :: JobTargets, "DocumentSource'" :: NullOrUndefined (JobDocumentSource), "Document'" :: NullOrUndefined (JobDocument), "Description'" :: NullOrUndefined (JobDescription), "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig), "TargetSelection'" :: NullOrUndefined (TargetSelection), "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig), "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters) }
```

##### Instances
``` purescript
Newtype CreateJobRequest _
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "Description'" :: NullOrUndefined (JobDescription) }
```

##### Instances
``` purescript
Newtype CreateJobResponse _
```

#### `CreateKeysAndCertificateRequest`

``` purescript
newtype CreateKeysAndCertificateRequest
  = CreateKeysAndCertificateRequest { "SetAsActive'" :: NullOrUndefined (SetAsActive) }
```

<p>The input for the CreateKeysAndCertificate operation.</p>

##### Instances
``` purescript
Newtype CreateKeysAndCertificateRequest _
```

#### `CreateKeysAndCertificateResponse`

``` purescript
newtype CreateKeysAndCertificateResponse
  = CreateKeysAndCertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "CertificatePem'" :: NullOrUndefined (CertificatePem), "KeyPair'" :: NullOrUndefined (KeyPair) }
```

<p>The output of the CreateKeysAndCertificate operation.</p>

##### Instances
``` purescript
Newtype CreateKeysAndCertificateResponse _
```

#### `CreateOTAUpdateRequest`

``` purescript
newtype CreateOTAUpdateRequest
  = CreateOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId, "Description'" :: NullOrUndefined (OTAUpdateDescription), "Targets'" :: Targets, "TargetSelection'" :: NullOrUndefined (TargetSelection), "Files'" :: OTAUpdateFiles, "RoleArn'" :: RoleArn, "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap) }
```

##### Instances
``` purescript
Newtype CreateOTAUpdateRequest _
```

#### `CreateOTAUpdateResponse`

``` purescript
newtype CreateOTAUpdateResponse
  = CreateOTAUpdateResponse { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus) }
```

##### Instances
``` purescript
Newtype CreateOTAUpdateResponse _
```

#### `CreatePolicyRequest`

``` purescript
newtype CreatePolicyRequest
  = CreatePolicyRequest { "PolicyName'" :: PolicyName, "PolicyDocument'" :: PolicyDocument }
```

<p>The input for the CreatePolicy operation.</p>

##### Instances
``` purescript
Newtype CreatePolicyRequest _
```

#### `CreatePolicyResponse`

``` purescript
newtype CreatePolicyResponse
  = CreatePolicyResponse { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId) }
```

<p>The output from the CreatePolicy operation.</p>

##### Instances
``` purescript
Newtype CreatePolicyResponse _
```

#### `CreatePolicyVersionRequest`

``` purescript
newtype CreatePolicyVersionRequest
  = CreatePolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyDocument'" :: PolicyDocument, "SetAsDefault'" :: NullOrUndefined (SetAsDefault) }
```

<p>The input for the CreatePolicyVersion operation.</p>

##### Instances
``` purescript
Newtype CreatePolicyVersionRequest _
```

#### `CreatePolicyVersionResponse`

``` purescript
newtype CreatePolicyVersionResponse
  = CreatePolicyVersionResponse { "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion) }
```

<p>The output of the CreatePolicyVersion operation.</p>

##### Instances
``` purescript
Newtype CreatePolicyVersionResponse _
```

#### `CreateRoleAliasRequest`

``` purescript
newtype CreateRoleAliasRequest
  = CreateRoleAliasRequest { "RoleAlias'" :: RoleAlias, "RoleArn'" :: RoleArn, "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds) }
```

##### Instances
``` purescript
Newtype CreateRoleAliasRequest _
```

#### `CreateRoleAliasResponse`

``` purescript
newtype CreateRoleAliasResponse
  = CreateRoleAliasResponse { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn) }
```

##### Instances
``` purescript
Newtype CreateRoleAliasResponse _
```

#### `CreateStreamRequest`

``` purescript
newtype CreateStreamRequest
  = CreateStreamRequest { "StreamId'" :: StreamId, "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: StreamFiles, "RoleArn'" :: RoleArn }
```

##### Instances
``` purescript
Newtype CreateStreamRequest _
```

#### `CreateStreamResponse`

``` purescript
newtype CreateStreamResponse
  = CreateStreamResponse { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "Description'" :: NullOrUndefined (StreamDescription), "StreamVersion'" :: NullOrUndefined (StreamVersion) }
```

##### Instances
``` purescript
Newtype CreateStreamResponse _
```

#### `CreateThingGroupRequest`

``` purescript
newtype CreateThingGroupRequest
  = CreateThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ParentGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties) }
```

##### Instances
``` purescript
Newtype CreateThingGroupRequest _
```

#### `CreateThingGroupResponse`

``` purescript
newtype CreateThingGroupResponse
  = CreateThingGroupResponse { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingGroupId'" :: NullOrUndefined (ThingGroupId) }
```

##### Instances
``` purescript
Newtype CreateThingGroupResponse _
```

#### `CreateThingRequest`

``` purescript
newtype CreateThingRequest
  = CreateThingRequest { "ThingName'" :: ThingName, "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "AttributePayload'" :: NullOrUndefined (AttributePayload) }
```

<p>The input for the CreateThing operation.</p>

##### Instances
``` purescript
Newtype CreateThingRequest _
```

#### `CreateThingResponse`

``` purescript
newtype CreateThingResponse
  = CreateThingResponse { "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn), "ThingId'" :: NullOrUndefined (ThingId) }
```

<p>The output of the CreateThing operation.</p>

##### Instances
``` purescript
Newtype CreateThingResponse _
```

#### `CreateThingTypeRequest`

``` purescript
newtype CreateThingTypeRequest
  = CreateThingTypeRequest { "ThingTypeName'" :: ThingTypeName, "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties) }
```

<p>The input for the CreateThingType operation.</p>

##### Instances
``` purescript
Newtype CreateThingTypeRequest _
```

#### `CreateThingTypeResponse`

``` purescript
newtype CreateThingTypeResponse
  = CreateThingTypeResponse { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeId'" :: NullOrUndefined (ThingTypeId) }
```

<p>The output of the CreateThingType operation.</p>

##### Instances
``` purescript
Newtype CreateThingTypeResponse _
```

#### `CreateTopicRuleRequest`

``` purescript
newtype CreateTopicRuleRequest
  = CreateTopicRuleRequest { "RuleName'" :: RuleName, "TopicRulePayload'" :: TopicRulePayload }
```

<p>The input for the CreateTopicRule operation.</p>

##### Instances
``` purescript
Newtype CreateTopicRuleRequest _
```

#### `CreatedAtDate`

``` purescript
newtype CreatedAtDate
  = CreatedAtDate Number
```

##### Instances
``` purescript
Newtype CreatedAtDate _
```

#### `CreationDate`

``` purescript
newtype CreationDate
  = CreationDate Number
```

##### Instances
``` purescript
Newtype CreationDate _
```

#### `CredentialDurationSeconds`

``` purescript
newtype CredentialDurationSeconds
  = CredentialDurationSeconds Int
```

##### Instances
``` purescript
Newtype CredentialDurationSeconds _
```

#### `CustomCodeSigning`

``` purescript
newtype CustomCodeSigning
  = CustomCodeSigning { "Signature'" :: NullOrUndefined (CodeSigningSignature), "CertificateChain'" :: NullOrUndefined (CodeSigningCertificateChain), "HashAlgorithm'" :: NullOrUndefined (HashAlgorithm), "SignatureAlgorithm'" :: NullOrUndefined (SignatureAlgorithm) }
```

<p>Describes a custom method used to code sign a file.</p>

##### Instances
``` purescript
Newtype CustomCodeSigning _
```

#### `DateType`

``` purescript
newtype DateType
  = DateType Number
```

##### Instances
``` purescript
Newtype DateType _
```

#### `DeleteAuthorizerRequest`

``` purescript
newtype DeleteAuthorizerRequest
  = DeleteAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

##### Instances
``` purescript
Newtype DeleteAuthorizerRequest _
```

#### `DeleteAuthorizerResponse`

``` purescript
newtype DeleteAuthorizerResponse
  = DeleteAuthorizerResponse {  }
```

##### Instances
``` purescript
Newtype DeleteAuthorizerResponse _
```

#### `DeleteCACertificateRequest`

``` purescript
newtype DeleteCACertificateRequest
  = DeleteCACertificateRequest { "CertificateId'" :: CertificateId }
```

<p>Input for the DeleteCACertificate operation.</p>

##### Instances
``` purescript
Newtype DeleteCACertificateRequest _
```

#### `DeleteCACertificateResponse`

``` purescript
newtype DeleteCACertificateResponse
  = DeleteCACertificateResponse {  }
```

<p>The output for the DeleteCACertificate operation.</p>

##### Instances
``` purescript
Newtype DeleteCACertificateResponse _
```

#### `DeleteCertificateRequest`

``` purescript
newtype DeleteCertificateRequest
  = DeleteCertificateRequest { "CertificateId'" :: CertificateId, "ForceDelete'" :: NullOrUndefined (ForceDelete) }
```

<p>The input for the DeleteCertificate operation.</p>

##### Instances
``` purescript
Newtype DeleteCertificateRequest _
```

#### `DeleteConflictException`

``` purescript
newtype DeleteConflictException
  = DeleteConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't delete the resource because it is attached to one or more resources.</p>

##### Instances
``` purescript
Newtype DeleteConflictException _
```

#### `DeleteOTAUpdateRequest`

``` purescript
newtype DeleteOTAUpdateRequest
  = DeleteOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId }
```

##### Instances
``` purescript
Newtype DeleteOTAUpdateRequest _
```

#### `DeleteOTAUpdateResponse`

``` purescript
newtype DeleteOTAUpdateResponse
  = DeleteOTAUpdateResponse {  }
```

##### Instances
``` purescript
Newtype DeleteOTAUpdateResponse _
```

#### `DeletePolicyRequest`

``` purescript
newtype DeletePolicyRequest
  = DeletePolicyRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the DeletePolicy operation.</p>

##### Instances
``` purescript
Newtype DeletePolicyRequest _
```

#### `DeletePolicyVersionRequest`

``` purescript
newtype DeletePolicyVersionRequest
  = DeletePolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the DeletePolicyVersion operation.</p>

##### Instances
``` purescript
Newtype DeletePolicyVersionRequest _
```

#### `DeleteRegistrationCodeRequest`

``` purescript
newtype DeleteRegistrationCodeRequest
  = DeleteRegistrationCodeRequest {  }
```

<p>The input for the DeleteRegistrationCode operation.</p>

##### Instances
``` purescript
Newtype DeleteRegistrationCodeRequest _
```

#### `DeleteRegistrationCodeResponse`

``` purescript
newtype DeleteRegistrationCodeResponse
  = DeleteRegistrationCodeResponse {  }
```

<p>The output for the DeleteRegistrationCode operation.</p>

##### Instances
``` purescript
Newtype DeleteRegistrationCodeResponse _
```

#### `DeleteRoleAliasRequest`

``` purescript
newtype DeleteRoleAliasRequest
  = DeleteRoleAliasRequest { "RoleAlias'" :: RoleAlias }
```

##### Instances
``` purescript
Newtype DeleteRoleAliasRequest _
```

#### `DeleteRoleAliasResponse`

``` purescript
newtype DeleteRoleAliasResponse
  = DeleteRoleAliasResponse {  }
```

##### Instances
``` purescript
Newtype DeleteRoleAliasResponse _
```

#### `DeleteStreamRequest`

``` purescript
newtype DeleteStreamRequest
  = DeleteStreamRequest { "StreamId'" :: StreamId }
```

##### Instances
``` purescript
Newtype DeleteStreamRequest _
```

#### `DeleteStreamResponse`

``` purescript
newtype DeleteStreamResponse
  = DeleteStreamResponse {  }
```

##### Instances
``` purescript
Newtype DeleteStreamResponse _
```

#### `DeleteThingGroupRequest`

``` purescript
newtype DeleteThingGroupRequest
  = DeleteThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

##### Instances
``` purescript
Newtype DeleteThingGroupRequest _
```

#### `DeleteThingGroupResponse`

``` purescript
newtype DeleteThingGroupResponse
  = DeleteThingGroupResponse {  }
```

##### Instances
``` purescript
Newtype DeleteThingGroupResponse _
```

#### `DeleteThingRequest`

``` purescript
newtype DeleteThingRequest
  = DeleteThingRequest { "ThingName'" :: ThingName, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

<p>The input for the DeleteThing operation.</p>

##### Instances
``` purescript
Newtype DeleteThingRequest _
```

#### `DeleteThingResponse`

``` purescript
newtype DeleteThingResponse
  = DeleteThingResponse {  }
```

<p>The output of the DeleteThing operation.</p>

##### Instances
``` purescript
Newtype DeleteThingResponse _
```

#### `DeleteThingTypeRequest`

``` purescript
newtype DeleteThingTypeRequest
  = DeleteThingTypeRequest { "ThingTypeName'" :: ThingTypeName }
```

<p>The input for the DeleteThingType operation.</p>

##### Instances
``` purescript
Newtype DeleteThingTypeRequest _
```

#### `DeleteThingTypeResponse`

``` purescript
newtype DeleteThingTypeResponse
  = DeleteThingTypeResponse {  }
```

<p>The output for the DeleteThingType operation.</p>

##### Instances
``` purescript
Newtype DeleteThingTypeResponse _
```

#### `DeleteTopicRuleRequest`

``` purescript
newtype DeleteTopicRuleRequest
  = DeleteTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the DeleteTopicRule operation.</p>

##### Instances
``` purescript
Newtype DeleteTopicRuleRequest _
```

#### `DeleteV2LoggingLevelRequest`

``` purescript
newtype DeleteV2LoggingLevelRequest
  = DeleteV2LoggingLevelRequest { "TargetType'" :: LogTargetType, "TargetName'" :: LogTargetName }
```

##### Instances
``` purescript
Newtype DeleteV2LoggingLevelRequest _
```

#### `DeliveryStreamName`

``` purescript
newtype DeliveryStreamName
  = DeliveryStreamName String
```

##### Instances
``` purescript
Newtype DeliveryStreamName _
```

#### `Denied`

``` purescript
newtype Denied
  = Denied { "ImplicitDeny'" :: NullOrUndefined (ImplicitDeny), "ExplicitDeny'" :: NullOrUndefined (ExplicitDeny) }
```

<p>Contains information that denied the authorization.</p>

##### Instances
``` purescript
Newtype Denied _
```

#### `DeprecateThingTypeRequest`

``` purescript
newtype DeprecateThingTypeRequest
  = DeprecateThingTypeRequest { "ThingTypeName'" :: ThingTypeName, "UndoDeprecate'" :: NullOrUndefined (UndoDeprecate) }
```

<p>The input for the DeprecateThingType operation.</p>

##### Instances
``` purescript
Newtype DeprecateThingTypeRequest _
```

#### `DeprecateThingTypeResponse`

``` purescript
newtype DeprecateThingTypeResponse
  = DeprecateThingTypeResponse {  }
```

<p>The output for the DeprecateThingType operation.</p>

##### Instances
``` purescript
Newtype DeprecateThingTypeResponse _
```

#### `DeprecationDate`

``` purescript
newtype DeprecationDate
  = DeprecationDate Number
```

##### Instances
``` purescript
Newtype DeprecationDate _
```

#### `DescribeAuthorizerRequest`

``` purescript
newtype DescribeAuthorizerRequest
  = DescribeAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

##### Instances
``` purescript
Newtype DescribeAuthorizerRequest _
```

#### `DescribeAuthorizerResponse`

``` purescript
newtype DescribeAuthorizerResponse
  = DescribeAuthorizerResponse { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription) }
```

##### Instances
``` purescript
Newtype DescribeAuthorizerResponse _
```

#### `DescribeCACertificateRequest`

``` purescript
newtype DescribeCACertificateRequest
  = DescribeCACertificateRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the DescribeCACertificate operation.</p>

##### Instances
``` purescript
Newtype DescribeCACertificateRequest _
```

#### `DescribeCACertificateResponse`

``` purescript
newtype DescribeCACertificateResponse
  = DescribeCACertificateResponse { "CertificateDescription'" :: NullOrUndefined (CACertificateDescription), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig) }
```

<p>The output from the DescribeCACertificate operation.</p>

##### Instances
``` purescript
Newtype DescribeCACertificateResponse _
```

#### `DescribeCertificateRequest`

``` purescript
newtype DescribeCertificateRequest
  = DescribeCertificateRequest { "CertificateId'" :: CertificateId }
```

<p>The input for the DescribeCertificate operation.</p>

##### Instances
``` purescript
Newtype DescribeCertificateRequest _
```

#### `DescribeCertificateResponse`

``` purescript
newtype DescribeCertificateResponse
  = DescribeCertificateResponse { "CertificateDescription'" :: NullOrUndefined (CertificateDescription) }
```

<p>The output of the DescribeCertificate operation.</p>

##### Instances
``` purescript
Newtype DescribeCertificateResponse _
```

#### `DescribeDefaultAuthorizerRequest`

``` purescript
newtype DescribeDefaultAuthorizerRequest
  = DescribeDefaultAuthorizerRequest {  }
```

##### Instances
``` purescript
Newtype DescribeDefaultAuthorizerRequest _
```

#### `DescribeDefaultAuthorizerResponse`

``` purescript
newtype DescribeDefaultAuthorizerResponse
  = DescribeDefaultAuthorizerResponse { "AuthorizerDescription'" :: NullOrUndefined (AuthorizerDescription) }
```

##### Instances
``` purescript
Newtype DescribeDefaultAuthorizerResponse _
```

#### `DescribeEndpointRequest`

``` purescript
newtype DescribeEndpointRequest
  = DescribeEndpointRequest { "EndpointType'" :: NullOrUndefined (EndpointType) }
```

<p>The input for the DescribeEndpoint operation.</p>

##### Instances
``` purescript
Newtype DescribeEndpointRequest _
```

#### `DescribeEndpointResponse`

``` purescript
newtype DescribeEndpointResponse
  = DescribeEndpointResponse { "EndpointAddress'" :: NullOrUndefined (EndpointAddress) }
```

<p>The output from the DescribeEndpoint operation.</p>

##### Instances
``` purescript
Newtype DescribeEndpointResponse _
```

#### `DescribeEventConfigurationsRequest`

``` purescript
newtype DescribeEventConfigurationsRequest
  = DescribeEventConfigurationsRequest {  }
```

##### Instances
``` purescript
Newtype DescribeEventConfigurationsRequest _
```

#### `DescribeEventConfigurationsResponse`

``` purescript
newtype DescribeEventConfigurationsResponse
  = DescribeEventConfigurationsResponse { "EventConfigurations'" :: NullOrUndefined (EventConfigurations), "CreationDate'" :: NullOrUndefined (CreationDate), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate) }
```

##### Instances
``` purescript
Newtype DescribeEventConfigurationsResponse _
```

#### `DescribeIndexRequest`

``` purescript
newtype DescribeIndexRequest
  = DescribeIndexRequest { "IndexName'" :: IndexName }
```

##### Instances
``` purescript
Newtype DescribeIndexRequest _
```

#### `DescribeIndexResponse`

``` purescript
newtype DescribeIndexResponse
  = DescribeIndexResponse { "IndexName'" :: NullOrUndefined (IndexName), "IndexStatus'" :: NullOrUndefined (IndexStatus), "Schema'" :: NullOrUndefined (IndexSchema) }
```

##### Instances
``` purescript
Newtype DescribeIndexResponse _
```

#### `DescribeJobExecutionRequest`

``` purescript
newtype DescribeJobExecutionRequest
  = DescribeJobExecutionRequest { "JobId'" :: JobId, "ThingName'" :: ThingName, "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

##### Instances
``` purescript
Newtype DescribeJobExecutionRequest _
```

#### `DescribeJobExecutionResponse`

``` purescript
newtype DescribeJobExecutionResponse
  = DescribeJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

##### Instances
``` purescript
Newtype DescribeJobExecutionResponse _
```

#### `DescribeJobRequest`

``` purescript
newtype DescribeJobRequest
  = DescribeJobRequest { "JobId'" :: JobId }
```

##### Instances
``` purescript
Newtype DescribeJobRequest _
```

#### `DescribeJobResponse`

``` purescript
newtype DescribeJobResponse
  = DescribeJobResponse { "DocumentSource'" :: NullOrUndefined (JobDocumentSource), "Job'" :: NullOrUndefined (Job) }
```

##### Instances
``` purescript
Newtype DescribeJobResponse _
```

#### `DescribeRoleAliasRequest`

``` purescript
newtype DescribeRoleAliasRequest
  = DescribeRoleAliasRequest { "RoleAlias'" :: RoleAlias }
```

##### Instances
``` purescript
Newtype DescribeRoleAliasRequest _
```

#### `DescribeRoleAliasResponse`

``` purescript
newtype DescribeRoleAliasResponse
  = DescribeRoleAliasResponse { "RoleAliasDescription'" :: NullOrUndefined (RoleAliasDescription) }
```

##### Instances
``` purescript
Newtype DescribeRoleAliasResponse _
```

#### `DescribeStreamRequest`

``` purescript
newtype DescribeStreamRequest
  = DescribeStreamRequest { "StreamId'" :: StreamId }
```

##### Instances
``` purescript
Newtype DescribeStreamRequest _
```

#### `DescribeStreamResponse`

``` purescript
newtype DescribeStreamResponse
  = DescribeStreamResponse { "StreamInfo'" :: NullOrUndefined (StreamInfo) }
```

##### Instances
``` purescript
Newtype DescribeStreamResponse _
```

#### `DescribeThingGroupRequest`

``` purescript
newtype DescribeThingGroupRequest
  = DescribeThingGroupRequest { "ThingGroupName'" :: ThingGroupName }
```

##### Instances
``` purescript
Newtype DescribeThingGroupRequest _
```

#### `DescribeThingGroupResponse`

``` purescript
newtype DescribeThingGroupResponse
  = DescribeThingGroupResponse { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupId'" :: NullOrUndefined (ThingGroupId), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "Version'" :: NullOrUndefined (Version), "ThingGroupProperties'" :: NullOrUndefined (ThingGroupProperties), "ThingGroupMetadata'" :: NullOrUndefined (ThingGroupMetadata) }
```

##### Instances
``` purescript
Newtype DescribeThingGroupResponse _
```

#### `DescribeThingRegistrationTaskRequest`

``` purescript
newtype DescribeThingRegistrationTaskRequest
  = DescribeThingRegistrationTaskRequest { "TaskId'" :: TaskId }
```

##### Instances
``` purescript
Newtype DescribeThingRegistrationTaskRequest _
```

#### `DescribeThingRegistrationTaskResponse`

``` purescript
newtype DescribeThingRegistrationTaskResponse
  = DescribeThingRegistrationTaskResponse { "TaskId'" :: NullOrUndefined (TaskId), "CreationDate'" :: NullOrUndefined (CreationDate), "LastModifiedDate'" :: NullOrUndefined (LastModifiedDate), "TemplateBody'" :: NullOrUndefined (TemplateBody), "InputFileBucket'" :: NullOrUndefined (RegistryS3BucketName), "InputFileKey'" :: NullOrUndefined (RegistryS3KeyName), "RoleArn'" :: NullOrUndefined (RoleArn), "Status'" :: NullOrUndefined (Status), "Message'" :: NullOrUndefined (ErrorMessage), "SuccessCount'" :: NullOrUndefined (Count), "FailureCount'" :: NullOrUndefined (Count), "PercentageProgress'" :: NullOrUndefined (Percentage) }
```

##### Instances
``` purescript
Newtype DescribeThingRegistrationTaskResponse _
```

#### `DescribeThingRequest`

``` purescript
newtype DescribeThingRequest
  = DescribeThingRequest { "ThingName'" :: ThingName }
```

<p>The input for the DescribeThing operation.</p>

##### Instances
``` purescript
Newtype DescribeThingRequest _
```

#### `DescribeThingResponse`

``` purescript
newtype DescribeThingResponse
  = DescribeThingResponse { "DefaultClientId'" :: NullOrUndefined (ClientId), "ThingName'" :: NullOrUndefined (ThingName), "ThingId'" :: NullOrUndefined (ThingId), "ThingArn'" :: NullOrUndefined (ThingArn), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "Attributes'" :: NullOrUndefined (Attributes), "Version'" :: NullOrUndefined (Version) }
```

<p>The output from the DescribeThing operation.</p>

##### Instances
``` purescript
Newtype DescribeThingResponse _
```

#### `DescribeThingTypeRequest`

``` purescript
newtype DescribeThingTypeRequest
  = DescribeThingTypeRequest { "ThingTypeName'" :: ThingTypeName }
```

<p>The input for the DescribeThingType operation.</p>

##### Instances
``` purescript
Newtype DescribeThingTypeRequest _
```

#### `DescribeThingTypeResponse`

``` purescript
newtype DescribeThingTypeResponse
  = DescribeThingTypeResponse { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeId'" :: NullOrUndefined (ThingTypeId), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties), "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata) }
```

<p>The output for the DescribeThingType operation.</p>

##### Instances
``` purescript
Newtype DescribeThingTypeResponse _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DetachPolicyRequest`

``` purescript
newtype DetachPolicyRequest
  = DetachPolicyRequest { "PolicyName'" :: PolicyName, "Target'" :: PolicyTarget }
```

##### Instances
``` purescript
Newtype DetachPolicyRequest _
```

#### `DetachPrincipalPolicyRequest`

``` purescript
newtype DetachPrincipalPolicyRequest
  = DetachPrincipalPolicyRequest { "PolicyName'" :: PolicyName, "Principal'" :: Principal }
```

<p>The input for the DetachPrincipalPolicy operation.</p>

##### Instances
``` purescript
Newtype DetachPrincipalPolicyRequest _
```

#### `DetachThingPrincipalRequest`

``` purescript
newtype DetachThingPrincipalRequest
  = DetachThingPrincipalRequest { "ThingName'" :: ThingName, "Principal'" :: Principal }
```

<p>The input for the DetachThingPrincipal operation.</p>

##### Instances
``` purescript
Newtype DetachThingPrincipalRequest _
```

#### `DetachThingPrincipalResponse`

``` purescript
newtype DetachThingPrincipalResponse
  = DetachThingPrincipalResponse {  }
```

<p>The output from the DetachThingPrincipal operation.</p>

##### Instances
``` purescript
Newtype DetachThingPrincipalResponse _
```

#### `DetailsKey`

``` purescript
newtype DetailsKey
  = DetailsKey String
```

##### Instances
``` purescript
Newtype DetailsKey _
```

#### `DetailsMap`

``` purescript
newtype DetailsMap
  = DetailsMap (Map DetailsKey DetailsValue)
```

##### Instances
``` purescript
Newtype DetailsMap _
```

#### `DetailsValue`

``` purescript
newtype DetailsValue
  = DetailsValue String
```

##### Instances
``` purescript
Newtype DetailsValue _
```

#### `DisableAllLogs`

``` purescript
newtype DisableAllLogs
  = DisableAllLogs Boolean
```

##### Instances
``` purescript
Newtype DisableAllLogs _
```

#### `DisableTopicRuleRequest`

``` purescript
newtype DisableTopicRuleRequest
  = DisableTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the DisableTopicRuleRequest operation.</p>

##### Instances
``` purescript
Newtype DisableTopicRuleRequest _
```

#### `DynamoDBAction`

``` purescript
newtype DynamoDBAction
  = DynamoDBAction { "TableName'" :: TableName, "RoleArn'" :: AwsArn, "Operation'" :: NullOrUndefined (DynamoOperation), "HashKeyField'" :: HashKeyField, "HashKeyValue'" :: HashKeyValue, "HashKeyType'" :: NullOrUndefined (DynamoKeyType), "RangeKeyField'" :: NullOrUndefined (RangeKeyField), "RangeKeyValue'" :: NullOrUndefined (RangeKeyValue), "RangeKeyType'" :: NullOrUndefined (DynamoKeyType), "PayloadField'" :: NullOrUndefined (PayloadField) }
```

<p>Describes an action to write to a DynamoDB table.</p> <p>The <code>tableName</code>, <code>hashKeyField</code>, and <code>rangeKeyField</code> values must match the values used when you created the table.</p> <p>The <code>hashKeyValue</code> and <code>rangeKeyvalue</code> fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${<i>sql-expression</i>}.</p> <p>You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:</p> <p> <code>"hashKeyValue": "${topic(3)}"</code> </p> <p>The following field uses the timestamp:</p> <p> <code>"rangeKeyValue": "${timestamp()}"</code> </p>

##### Instances
``` purescript
Newtype DynamoDBAction _
```

#### `DynamoDBv2Action`

``` purescript
newtype DynamoDBv2Action
  = DynamoDBv2Action { "RoleArn'" :: NullOrUndefined (AwsArn), "PutItem'" :: NullOrUndefined (PutItemInput) }
```

<p>Describes an action to write to a DynamoDB table.</p> <p>This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.</p>

##### Instances
``` purescript
Newtype DynamoDBv2Action _
```

#### `DynamoKeyType`

``` purescript
newtype DynamoKeyType
  = DynamoKeyType String
```

##### Instances
``` purescript
Newtype DynamoKeyType _
```

#### `DynamoOperation`

``` purescript
newtype DynamoOperation
  = DynamoOperation String
```

##### Instances
``` purescript
Newtype DynamoOperation _
```

#### `EffectivePolicies`

``` purescript
newtype EffectivePolicies
  = EffectivePolicies (Array EffectivePolicy)
```

##### Instances
``` purescript
Newtype EffectivePolicies _
```

#### `EffectivePolicy`

``` purescript
newtype EffectivePolicy
  = EffectivePolicy { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument) }
```

<p>The policy that has the effect on the authorization results.</p>

##### Instances
``` purescript
Newtype EffectivePolicy _
```

#### `ElasticsearchAction`

``` purescript
newtype ElasticsearchAction
  = ElasticsearchAction { "RoleArn'" :: AwsArn, "Endpoint'" :: ElasticsearchEndpoint, "Index'" :: ElasticsearchIndex, "Type'" :: ElasticsearchType, "Id'" :: ElasticsearchId }
```

<p>Describes an action that writes data to an Amazon Elasticsearch Service domain.</p>

##### Instances
``` purescript
Newtype ElasticsearchAction _
```

#### `ElasticsearchEndpoint`

``` purescript
newtype ElasticsearchEndpoint
  = ElasticsearchEndpoint String
```

##### Instances
``` purescript
Newtype ElasticsearchEndpoint _
```

#### `ElasticsearchId`

``` purescript
newtype ElasticsearchId
  = ElasticsearchId String
```

##### Instances
``` purescript
Newtype ElasticsearchId _
```

#### `ElasticsearchIndex`

``` purescript
newtype ElasticsearchIndex
  = ElasticsearchIndex String
```

##### Instances
``` purescript
Newtype ElasticsearchIndex _
```

#### `ElasticsearchType`

``` purescript
newtype ElasticsearchType
  = ElasticsearchType String
```

##### Instances
``` purescript
Newtype ElasticsearchType _
```

#### `EnableTopicRuleRequest`

``` purescript
newtype EnableTopicRuleRequest
  = EnableTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the EnableTopicRuleRequest operation.</p>

##### Instances
``` purescript
Newtype EnableTopicRuleRequest _
```

#### `Enabled`

``` purescript
newtype Enabled
  = Enabled Boolean
```

##### Instances
``` purescript
Newtype Enabled _
```

#### `EndpointAddress`

``` purescript
newtype EndpointAddress
  = EndpointAddress String
```

##### Instances
``` purescript
Newtype EndpointAddress _
```

#### `EndpointType`

``` purescript
newtype EndpointType
  = EndpointType String
```

##### Instances
``` purescript
Newtype EndpointType _
```

#### `ErrorInfo`

``` purescript
newtype ErrorInfo
  = ErrorInfo { "Code'" :: NullOrUndefined (Code), "Message'" :: NullOrUndefined (OTAUpdateErrorMessage) }
```

<p>Error information.</p>

##### Instances
``` purescript
Newtype ErrorInfo _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `EventConfigurations`

``` purescript
newtype EventConfigurations
  = EventConfigurations (Map EventType Configuration)
```

##### Instances
``` purescript
Newtype EventConfigurations _
```

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

##### Instances
``` purescript
Newtype EventType _
```

#### `ExecutionNumber`

``` purescript
newtype ExecutionNumber
  = ExecutionNumber Number
```

##### Instances
``` purescript
Newtype ExecutionNumber _
```

#### `ExpiresInSec`

``` purescript
newtype ExpiresInSec
  = ExpiresInSec Number
```

##### Instances
``` purescript
Newtype ExpiresInSec _
```

#### `ExplicitDeny`

``` purescript
newtype ExplicitDeny
  = ExplicitDeny { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Information that explicitly denies authorization.</p>

##### Instances
``` purescript
Newtype ExplicitDeny _
```

#### `FailedThings`

``` purescript
newtype FailedThings
  = FailedThings Int
```

##### Instances
``` purescript
Newtype FailedThings _
```

#### `FileId`

``` purescript
newtype FileId
  = FileId Int
```

##### Instances
``` purescript
Newtype FileId _
```

#### `FileName`

``` purescript
newtype FileName
  = FileName String
```

##### Instances
``` purescript
Newtype FileName _
```

#### `FirehoseAction`

``` purescript
newtype FirehoseAction
  = FirehoseAction { "RoleArn'" :: AwsArn, "DeliveryStreamName'" :: DeliveryStreamName, "Separator'" :: NullOrUndefined (FirehoseSeparator) }
```

<p>Describes an action that writes data to an Amazon Kinesis Firehose stream.</p>

##### Instances
``` purescript
Newtype FirehoseAction _
```

#### `FirehoseSeparator`

``` purescript
newtype FirehoseSeparator
  = FirehoseSeparator String
```

##### Instances
``` purescript
Newtype FirehoseSeparator _
```

#### `Flag`

``` purescript
newtype Flag
  = Flag Boolean
```

##### Instances
``` purescript
Newtype Flag _
```

#### `ForceDelete`

``` purescript
newtype ForceDelete
  = ForceDelete Boolean
```

##### Instances
``` purescript
Newtype ForceDelete _
```

#### `FunctionArn`

``` purescript
newtype FunctionArn
  = FunctionArn String
```

##### Instances
``` purescript
Newtype FunctionArn _
```

#### `GEMaxResults`

``` purescript
newtype GEMaxResults
  = GEMaxResults Int
```

##### Instances
``` purescript
Newtype GEMaxResults _
```

#### `GetEffectivePoliciesRequest`

``` purescript
newtype GetEffectivePoliciesRequest
  = GetEffectivePoliciesRequest { "Principal'" :: NullOrUndefined (Principal), "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId), "ThingName'" :: NullOrUndefined (ThingName) }
```

##### Instances
``` purescript
Newtype GetEffectivePoliciesRequest _
```

#### `GetEffectivePoliciesResponse`

``` purescript
newtype GetEffectivePoliciesResponse
  = GetEffectivePoliciesResponse { "EffectivePolicies'" :: NullOrUndefined (EffectivePolicies) }
```

##### Instances
``` purescript
Newtype GetEffectivePoliciesResponse _
```

#### `GetIndexingConfigurationRequest`

``` purescript
newtype GetIndexingConfigurationRequest
  = GetIndexingConfigurationRequest {  }
```

##### Instances
``` purescript
Newtype GetIndexingConfigurationRequest _
```

#### `GetIndexingConfigurationResponse`

``` purescript
newtype GetIndexingConfigurationResponse
  = GetIndexingConfigurationResponse { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration) }
```

##### Instances
``` purescript
Newtype GetIndexingConfigurationResponse _
```

#### `GetJobDocumentRequest`

``` purescript
newtype GetJobDocumentRequest
  = GetJobDocumentRequest { "JobId'" :: JobId }
```

##### Instances
``` purescript
Newtype GetJobDocumentRequest _
```

#### `GetJobDocumentResponse`

``` purescript
newtype GetJobDocumentResponse
  = GetJobDocumentResponse { "Document'" :: NullOrUndefined (JobDocument) }
```

##### Instances
``` purescript
Newtype GetJobDocumentResponse _
```

#### `GetLoggingOptionsRequest`

``` purescript
newtype GetLoggingOptionsRequest
  = GetLoggingOptionsRequest {  }
```

<p>The input for the GetLoggingOptions operation.</p>

##### Instances
``` purescript
Newtype GetLoggingOptionsRequest _
```

#### `GetLoggingOptionsResponse`

``` purescript
newtype GetLoggingOptionsResponse
  = GetLoggingOptionsResponse { "RoleArn'" :: NullOrUndefined (AwsArn), "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>The output from the GetLoggingOptions operation.</p>

##### Instances
``` purescript
Newtype GetLoggingOptionsResponse _
```

#### `GetOTAUpdateRequest`

``` purescript
newtype GetOTAUpdateRequest
  = GetOTAUpdateRequest { "OtaUpdateId'" :: OTAUpdateId }
```

##### Instances
``` purescript
Newtype GetOTAUpdateRequest _
```

#### `GetOTAUpdateResponse`

``` purescript
newtype GetOTAUpdateResponse
  = GetOTAUpdateResponse { "OtaUpdateInfo'" :: NullOrUndefined (OTAUpdateInfo) }
```

##### Instances
``` purescript
Newtype GetOTAUpdateResponse _
```

#### `GetPolicyRequest`

``` purescript
newtype GetPolicyRequest
  = GetPolicyRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the GetPolicy operation.</p>

##### Instances
``` purescript
Newtype GetPolicyRequest _
```

#### `GetPolicyResponse`

``` purescript
newtype GetPolicyResponse
  = GetPolicyResponse { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "DefaultVersionId'" :: NullOrUndefined (PolicyVersionId) }
```

<p>The output from the GetPolicy operation.</p>

##### Instances
``` purescript
Newtype GetPolicyResponse _
```

#### `GetPolicyVersionRequest`

``` purescript
newtype GetPolicyVersionRequest
  = GetPolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the GetPolicyVersion operation.</p>

##### Instances
``` purescript
Newtype GetPolicyVersionRequest _
```

#### `GetPolicyVersionResponse`

``` purescript
newtype GetPolicyVersionResponse
  = GetPolicyVersionResponse { "PolicyArn'" :: NullOrUndefined (PolicyArn), "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "PolicyVersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion) }
```

<p>The output from the GetPolicyVersion operation.</p>

##### Instances
``` purescript
Newtype GetPolicyVersionResponse _
```

#### `GetRegistrationCodeRequest`

``` purescript
newtype GetRegistrationCodeRequest
  = GetRegistrationCodeRequest {  }
```

<p>The input to the GetRegistrationCode operation.</p>

##### Instances
``` purescript
Newtype GetRegistrationCodeRequest _
```

#### `GetRegistrationCodeResponse`

``` purescript
newtype GetRegistrationCodeResponse
  = GetRegistrationCodeResponse { "RegistrationCode'" :: NullOrUndefined (RegistrationCode) }
```

<p>The output from the GetRegistrationCode operation.</p>

##### Instances
``` purescript
Newtype GetRegistrationCodeResponse _
```

#### `GetTopicRuleRequest`

``` purescript
newtype GetTopicRuleRequest
  = GetTopicRuleRequest { "RuleName'" :: RuleName }
```

<p>The input for the GetTopicRule operation.</p>

##### Instances
``` purescript
Newtype GetTopicRuleRequest _
```

#### `GetTopicRuleResponse`

``` purescript
newtype GetTopicRuleResponse
  = GetTopicRuleResponse { "RuleArn'" :: NullOrUndefined (RuleArn), "Rule'" :: NullOrUndefined (TopicRule) }
```

<p>The output from the GetTopicRule operation.</p>

##### Instances
``` purescript
Newtype GetTopicRuleResponse _
```

#### `GetV2LoggingOptionsRequest`

``` purescript
newtype GetV2LoggingOptionsRequest
  = GetV2LoggingOptionsRequest {  }
```

##### Instances
``` purescript
Newtype GetV2LoggingOptionsRequest _
```

#### `GetV2LoggingOptionsResponse`

``` purescript
newtype GetV2LoggingOptionsResponse
  = GetV2LoggingOptionsResponse { "RoleArn'" :: NullOrUndefined (AwsArn), "DefaultLogLevel'" :: NullOrUndefined (LogLevel), "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs) }
```

##### Instances
``` purescript
Newtype GetV2LoggingOptionsResponse _
```

#### `GroupNameAndArn`

``` purescript
newtype GroupNameAndArn
  = GroupNameAndArn { "GroupName'" :: NullOrUndefined (ThingGroupName), "GroupArn'" :: NullOrUndefined (ThingGroupArn) }
```

<p>The name and ARN of a group.</p>

##### Instances
``` purescript
Newtype GroupNameAndArn _
```

#### `HashAlgorithm`

``` purescript
newtype HashAlgorithm
  = HashAlgorithm String
```

##### Instances
``` purescript
Newtype HashAlgorithm _
```

#### `HashKeyField`

``` purescript
newtype HashKeyField
  = HashKeyField String
```

##### Instances
``` purescript
Newtype HashKeyField _
```

#### `HashKeyValue`

``` purescript
newtype HashKeyValue
  = HashKeyValue String
```

##### Instances
``` purescript
Newtype HashKeyValue _
```

#### `ImplicitDeny`

``` purescript
newtype ImplicitDeny
  = ImplicitDeny { "Policies'" :: NullOrUndefined (Policies) }
```

<p>Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.</p>

##### Instances
``` purescript
Newtype ImplicitDeny _
```

#### `InProgressThings`

``` purescript
newtype InProgressThings
  = InProgressThings Int
```

##### Instances
``` purescript
Newtype InProgressThings _
```

#### `IndexName`

``` purescript
newtype IndexName
  = IndexName String
```

##### Instances
``` purescript
Newtype IndexName _
```

#### `IndexNamesList`

``` purescript
newtype IndexNamesList
  = IndexNamesList (Array IndexName)
```

##### Instances
``` purescript
Newtype IndexNamesList _
```

#### `IndexNotReadyException`

``` purescript
newtype IndexNotReadyException
  = IndexNotReadyException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The index is not ready.</p>

##### Instances
``` purescript
Newtype IndexNotReadyException _
```

#### `IndexSchema`

``` purescript
newtype IndexSchema
  = IndexSchema String
```

##### Instances
``` purescript
Newtype IndexSchema _
```

#### `IndexStatus`

``` purescript
newtype IndexStatus
  = IndexStatus String
```

##### Instances
``` purescript
Newtype IndexStatus _
```

#### `InlineDocument`

``` purescript
newtype InlineDocument
  = InlineDocument String
```

##### Instances
``` purescript
Newtype InlineDocument _
```

#### `InternalException`

``` purescript
newtype InternalException
  = InternalException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An unexpected error has occurred.</p>

##### Instances
``` purescript
Newtype InternalException _
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An unexpected error has occurred.</p>

##### Instances
``` purescript
Newtype InternalFailureException _
```

#### `InvalidQueryException`

``` purescript
newtype InvalidQueryException
  = InvalidQueryException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The query is invalid.</p>

##### Instances
``` purescript
Newtype InvalidQueryException _
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The request is not valid.</p>

##### Instances
``` purescript
Newtype InvalidRequestException _
```

#### `InvalidResponseException`

``` purescript
newtype InvalidResponseException
  = InvalidResponseException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The response is invalid.</p>

##### Instances
``` purescript
Newtype InvalidResponseException _
```

#### `IsAuthenticated`

``` purescript
newtype IsAuthenticated
  = IsAuthenticated Boolean
```

##### Instances
``` purescript
Newtype IsAuthenticated _
```

#### `IsDefaultVersion`

``` purescript
newtype IsDefaultVersion
  = IsDefaultVersion Boolean
```

##### Instances
``` purescript
Newtype IsDefaultVersion _
```

#### `IsDisabled`

``` purescript
newtype IsDisabled
  = IsDisabled Boolean
```

##### Instances
``` purescript
Newtype IsDisabled _
```

#### `Job`

``` purescript
newtype Job
  = Job { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "TargetSelection'" :: NullOrUndefined (TargetSelection), "Status'" :: NullOrUndefined (JobStatus), "Comment'" :: NullOrUndefined (Comment), "Targets'" :: NullOrUndefined (JobTargets), "Description'" :: NullOrUndefined (JobDescription), "PresignedUrlConfig'" :: NullOrUndefined (PresignedUrlConfig), "JobExecutionsRolloutConfig'" :: NullOrUndefined (JobExecutionsRolloutConfig), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "CompletedAt'" :: NullOrUndefined (DateType), "JobProcessDetails'" :: NullOrUndefined (JobProcessDetails), "DocumentParameters'" :: NullOrUndefined (JobDocumentParameters) }
```

<p>The <code>Job</code> object contains details about a job.</p>

##### Instances
``` purescript
Newtype Job _
```

#### `JobArn`

``` purescript
newtype JobArn
  = JobArn String
```

##### Instances
``` purescript
Newtype JobArn _
```

#### `JobDescription`

``` purescript
newtype JobDescription
  = JobDescription String
```

##### Instances
``` purescript
Newtype JobDescription _
```

#### `JobDocument`

``` purescript
newtype JobDocument
  = JobDocument String
```

##### Instances
``` purescript
Newtype JobDocument _
```

#### `JobDocumentParameters`

``` purescript
newtype JobDocumentParameters
  = JobDocumentParameters (Map ParameterKey ParameterValue)
```

##### Instances
``` purescript
Newtype JobDocumentParameters _
```

#### `JobDocumentSource`

``` purescript
newtype JobDocumentSource
  = JobDocumentSource String
```

##### Instances
``` purescript
Newtype JobDocumentSource _
```

#### `JobExecution`

``` purescript
newtype JobExecution
  = JobExecution { "JobId'" :: NullOrUndefined (JobId), "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (JobExecutionStatusDetails), "ThingArn'" :: NullOrUndefined (ThingArn), "QueuedAt'" :: NullOrUndefined (DateType), "StartedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>The job execution object represents the execution of a job on a particular device.</p>

##### Instances
``` purescript
Newtype JobExecution _
```

#### `JobExecutionStatus`

``` purescript
newtype JobExecutionStatus
  = JobExecutionStatus String
```

##### Instances
``` purescript
Newtype JobExecutionStatus _
```

#### `JobExecutionStatusDetails`

``` purescript
newtype JobExecutionStatusDetails
  = JobExecutionStatusDetails { "DetailsMap'" :: NullOrUndefined (DetailsMap) }
```

<p>Details of the job execution status.</p>

##### Instances
``` purescript
Newtype JobExecutionStatusDetails _
```

#### `JobExecutionSummary`

``` purescript
newtype JobExecutionSummary
  = JobExecutionSummary { "Status'" :: NullOrUndefined (JobExecutionStatus), "QueuedAt'" :: NullOrUndefined (DateType), "StartedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>The job execution summary.</p>

##### Instances
``` purescript
Newtype JobExecutionSummary _
```

#### `JobExecutionSummaryForJob`

``` purescript
newtype JobExecutionSummaryForJob
  = JobExecutionSummaryForJob { "ThingArn'" :: NullOrUndefined (ThingArn), "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary) }
```

<p>Contains a summary of information about job executions for a specific job.</p>

##### Instances
``` purescript
Newtype JobExecutionSummaryForJob _
```

#### `JobExecutionSummaryForJobList`

``` purescript
newtype JobExecutionSummaryForJobList
  = JobExecutionSummaryForJobList (Array JobExecutionSummaryForJob)
```

##### Instances
``` purescript
Newtype JobExecutionSummaryForJobList _
```

#### `JobExecutionSummaryForThing`

``` purescript
newtype JobExecutionSummaryForThing
  = JobExecutionSummaryForThing { "JobId'" :: NullOrUndefined (JobId), "JobExecutionSummary'" :: NullOrUndefined (JobExecutionSummary) }
```

<p>The job execution summary for a thing.</p>

##### Instances
``` purescript
Newtype JobExecutionSummaryForThing _
```

#### `JobExecutionSummaryForThingList`

``` purescript
newtype JobExecutionSummaryForThingList
  = JobExecutionSummaryForThingList (Array JobExecutionSummaryForThing)
```

##### Instances
``` purescript
Newtype JobExecutionSummaryForThingList _
```

#### `JobExecutionsRolloutConfig`

``` purescript
newtype JobExecutionsRolloutConfig
  = JobExecutionsRolloutConfig { "MaximumPerMinute'" :: NullOrUndefined (MaxJobExecutionsPerMin) }
```

<p>Allows you to create a staged rollout of a job.</p>

##### Instances
``` purescript
Newtype JobExecutionsRolloutConfig _
```

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

##### Instances
``` purescript
Newtype JobId _
```

#### `JobProcessDetails`

``` purescript
newtype JobProcessDetails
  = JobProcessDetails { "ProcessingTargets'" :: NullOrUndefined (ProcessingTargetNameList), "NumberOfCanceledThings'" :: NullOrUndefined (CanceledThings), "NumberOfSucceededThings'" :: NullOrUndefined (SucceededThings), "NumberOfFailedThings'" :: NullOrUndefined (FailedThings), "NumberOfRejectedThings'" :: NullOrUndefined (RejectedThings), "NumberOfQueuedThings'" :: NullOrUndefined (QueuedThings), "NumberOfInProgressThings'" :: NullOrUndefined (InProgressThings), "NumberOfRemovedThings'" :: NullOrUndefined (RemovedThings) }
```

<p>The job process details.</p>

##### Instances
``` purescript
Newtype JobProcessDetails _
```

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

##### Instances
``` purescript
Newtype JobStatus _
```

#### `JobSummary`

``` purescript
newtype JobSummary
  = JobSummary { "JobArn'" :: NullOrUndefined (JobArn), "JobId'" :: NullOrUndefined (JobId), "ThingGroupId'" :: NullOrUndefined (ThingGroupId), "TargetSelection'" :: NullOrUndefined (TargetSelection), "Status'" :: NullOrUndefined (JobStatus), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "CompletedAt'" :: NullOrUndefined (DateType) }
```

<p>The job summary.</p>

##### Instances
``` purescript
Newtype JobSummary _
```

#### `JobSummaryList`

``` purescript
newtype JobSummaryList
  = JobSummaryList (Array JobSummary)
```

##### Instances
``` purescript
Newtype JobSummaryList _
```

#### `JobTargets`

``` purescript
newtype JobTargets
  = JobTargets (Array TargetArn)
```

##### Instances
``` purescript
Newtype JobTargets _
```

#### `JsonDocument`

``` purescript
newtype JsonDocument
  = JsonDocument String
```

##### Instances
``` purescript
Newtype JsonDocument _
```

#### `Key`

``` purescript
newtype Key
  = Key String
```

##### Instances
``` purescript
Newtype Key _
```

#### `KeyName`

``` purescript
newtype KeyName
  = KeyName String
```

##### Instances
``` purescript
Newtype KeyName _
```

#### `KeyPair`

``` purescript
newtype KeyPair
  = KeyPair { "PublicKey" :: NullOrUndefined (PublicKey), "PrivateKey" :: NullOrUndefined (PrivateKey) }
```

<p>Describes a key pair.</p>

##### Instances
``` purescript
Newtype KeyPair _
```

#### `KeyValue`

``` purescript
newtype KeyValue
  = KeyValue String
```

##### Instances
``` purescript
Newtype KeyValue _
```

#### `KinesisAction`

``` purescript
newtype KinesisAction
  = KinesisAction { "RoleArn'" :: AwsArn, "StreamName'" :: StreamName, "PartitionKey'" :: NullOrUndefined (PartitionKey) }
```

<p>Describes an action to write data to an Amazon Kinesis stream.</p>

##### Instances
``` purescript
Newtype KinesisAction _
```

#### `LambdaAction`

``` purescript
newtype LambdaAction
  = LambdaAction { "FunctionArn'" :: FunctionArn }
```

<p>Describes an action to invoke a Lambda function.</p>

##### Instances
``` purescript
Newtype LambdaAction _
```

#### `LaserMaxResults`

``` purescript
newtype LaserMaxResults
  = LaserMaxResults Int
```

##### Instances
``` purescript
Newtype LaserMaxResults _
```

#### `LastModifiedDate`

``` purescript
newtype LastModifiedDate
  = LastModifiedDate Number
```

##### Instances
``` purescript
Newtype LastModifiedDate _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The number of attached entities exceeds the limit.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListAttachedPoliciesRequest`

``` purescript
newtype ListAttachedPoliciesRequest
  = ListAttachedPoliciesRequest { "Target'" :: PolicyTarget, "Recursive'" :: NullOrUndefined (Recursive), "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype ListAttachedPoliciesRequest _
```

#### `ListAttachedPoliciesResponse`

``` purescript
newtype ListAttachedPoliciesResponse
  = ListAttachedPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype ListAttachedPoliciesResponse _
```

#### `ListAuthorizersRequest`

``` purescript
newtype ListAuthorizersRequest
  = ListAuthorizersRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder), "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

##### Instances
``` purescript
Newtype ListAuthorizersRequest _
```

#### `ListAuthorizersResponse`

``` purescript
newtype ListAuthorizersResponse
  = ListAuthorizersResponse { "Authorizers'" :: NullOrUndefined (Authorizers), "NextMarker'" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype ListAuthorizersResponse _
```

#### `ListCACertificatesRequest`

``` purescript
newtype ListCACertificatesRequest
  = ListCACertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>Input for the ListCACertificates operation.</p>

##### Instances
``` purescript
Newtype ListCACertificatesRequest _
```

#### `ListCACertificatesResponse`

``` purescript
newtype ListCACertificatesResponse
  = ListCACertificatesResponse { "Certificates'" :: NullOrUndefined (CACertificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListCACertificates operation.</p>

##### Instances
``` purescript
Newtype ListCACertificatesResponse _
```

#### `ListCertificatesByCARequest`

``` purescript
newtype ListCertificatesByCARequest
  = ListCertificatesByCARequest { "CaCertificateId'" :: CertificateId, "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input to the ListCertificatesByCA operation.</p>

##### Instances
``` purescript
Newtype ListCertificatesByCARequest _
```

#### `ListCertificatesByCAResponse`

``` purescript
newtype ListCertificatesByCAResponse
  = ListCertificatesByCAResponse { "Certificates'" :: NullOrUndefined (Certificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output of the ListCertificatesByCA operation.</p>

##### Instances
``` purescript
Newtype ListCertificatesByCAResponse _
```

#### `ListCertificatesRequest`

``` purescript
newtype ListCertificatesRequest
  = ListCertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListCertificates operation.</p>

##### Instances
``` purescript
Newtype ListCertificatesRequest _
```

#### `ListCertificatesResponse`

``` purescript
newtype ListCertificatesResponse
  = ListCertificatesResponse { "Certificates'" :: NullOrUndefined (Certificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output of the ListCertificates operation.</p>

##### Instances
``` purescript
Newtype ListCertificatesResponse _
```

#### `ListIndicesRequest`

``` purescript
newtype ListIndicesRequest
  = ListIndicesRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (QueryMaxResults) }
```

##### Instances
``` purescript
Newtype ListIndicesRequest _
```

#### `ListIndicesResponse`

``` purescript
newtype ListIndicesResponse
  = ListIndicesResponse { "IndexNames'" :: NullOrUndefined (IndexNamesList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListIndicesResponse _
```

#### `ListJobExecutionsForJobRequest`

``` purescript
newtype ListJobExecutionsForJobRequest
  = ListJobExecutionsForJobRequest { "JobId'" :: JobId, "Status'" :: NullOrUndefined (JobExecutionStatus), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListJobExecutionsForJobRequest _
```

#### `ListJobExecutionsForJobResponse`

``` purescript
newtype ListJobExecutionsForJobResponse
  = ListJobExecutionsForJobResponse { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForJobList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListJobExecutionsForJobResponse _
```

#### `ListJobExecutionsForThingRequest`

``` purescript
newtype ListJobExecutionsForThingRequest
  = ListJobExecutionsForThingRequest { "ThingName'" :: ThingName, "Status'" :: NullOrUndefined (JobExecutionStatus), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListJobExecutionsForThingRequest _
```

#### `ListJobExecutionsForThingResponse`

``` purescript
newtype ListJobExecutionsForThingResponse
  = ListJobExecutionsForThingResponse { "ExecutionSummaries'" :: NullOrUndefined (JobExecutionSummaryForThingList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListJobExecutionsForThingResponse _
```

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "Status'" :: NullOrUndefined (JobStatus), "TargetSelection'" :: NullOrUndefined (TargetSelection), "MaxResults'" :: NullOrUndefined (LaserMaxResults), "NextToken'" :: NullOrUndefined (NextToken), "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupId'" :: NullOrUndefined (ThingGroupId) }
```

##### Instances
``` purescript
Newtype ListJobsRequest _
```

#### `ListJobsResponse`

``` purescript
newtype ListJobsResponse
  = ListJobsResponse { "Jobs'" :: NullOrUndefined (JobSummaryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListJobsResponse _
```

#### `ListOTAUpdatesRequest`

``` purescript
newtype ListOTAUpdatesRequest
  = ListOTAUpdatesRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus) }
```

##### Instances
``` purescript
Newtype ListOTAUpdatesRequest _
```

#### `ListOTAUpdatesResponse`

``` purescript
newtype ListOTAUpdatesResponse
  = ListOTAUpdatesResponse { "OtaUpdates'" :: NullOrUndefined (OTAUpdatesSummary), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListOTAUpdatesResponse _
```

#### `ListOutgoingCertificatesRequest`

``` purescript
newtype ListOutgoingCertificatesRequest
  = ListOutgoingCertificatesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input to the ListOutgoingCertificates operation.</p>

##### Instances
``` purescript
Newtype ListOutgoingCertificatesRequest _
```

#### `ListOutgoingCertificatesResponse`

``` purescript
newtype ListOutgoingCertificatesResponse
  = ListOutgoingCertificatesResponse { "OutgoingCertificates'" :: NullOrUndefined (OutgoingCertificates), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListOutgoingCertificates operation.</p>

##### Instances
``` purescript
Newtype ListOutgoingCertificatesResponse _
```

#### `ListPoliciesRequest`

``` purescript
newtype ListPoliciesRequest
  = ListPoliciesRequest { "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPolicies operation.</p>

##### Instances
``` purescript
Newtype ListPoliciesRequest _
```

#### `ListPoliciesResponse`

``` purescript
newtype ListPoliciesResponse
  = ListPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPolicies operation.</p>

##### Instances
``` purescript
Newtype ListPoliciesResponse _
```

#### `ListPolicyPrincipalsRequest`

``` purescript
newtype ListPolicyPrincipalsRequest
  = ListPolicyPrincipalsRequest { "PolicyName'" :: PolicyName, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPolicyPrincipals operation.</p>

##### Instances
``` purescript
Newtype ListPolicyPrincipalsRequest _
```

#### `ListPolicyPrincipalsResponse`

``` purescript
newtype ListPolicyPrincipalsResponse
  = ListPolicyPrincipalsResponse { "Principals'" :: NullOrUndefined (Principals), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPolicyPrincipals operation.</p>

##### Instances
``` purescript
Newtype ListPolicyPrincipalsResponse _
```

#### `ListPolicyVersionsRequest`

``` purescript
newtype ListPolicyVersionsRequest
  = ListPolicyVersionsRequest { "PolicyName'" :: PolicyName }
```

<p>The input for the ListPolicyVersions operation.</p>

##### Instances
``` purescript
Newtype ListPolicyVersionsRequest _
```

#### `ListPolicyVersionsResponse`

``` purescript
newtype ListPolicyVersionsResponse
  = ListPolicyVersionsResponse { "PolicyVersions'" :: NullOrUndefined (PolicyVersions) }
```

<p>The output from the ListPolicyVersions operation.</p>

##### Instances
``` purescript
Newtype ListPolicyVersionsResponse _
```

#### `ListPrincipalPoliciesRequest`

``` purescript
newtype ListPrincipalPoliciesRequest
  = ListPrincipalPoliciesRequest { "Principal'" :: Principal, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

<p>The input for the ListPrincipalPolicies operation.</p>

##### Instances
``` purescript
Newtype ListPrincipalPoliciesRequest _
```

#### `ListPrincipalPoliciesResponse`

``` purescript
newtype ListPrincipalPoliciesResponse
  = ListPrincipalPoliciesResponse { "Policies'" :: NullOrUndefined (Policies), "NextMarker'" :: NullOrUndefined (Marker) }
```

<p>The output from the ListPrincipalPolicies operation.</p>

##### Instances
``` purescript
Newtype ListPrincipalPoliciesResponse _
```

#### `ListPrincipalThingsRequest`

``` purescript
newtype ListPrincipalThingsRequest
  = ListPrincipalThingsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "Principal'" :: Principal }
```

<p>The input for the ListPrincipalThings operation.</p>

##### Instances
``` purescript
Newtype ListPrincipalThingsRequest _
```

#### `ListPrincipalThingsResponse`

``` purescript
newtype ListPrincipalThingsResponse
  = ListPrincipalThingsResponse { "Things'" :: NullOrUndefined (ThingNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListPrincipalThings operation.</p>

##### Instances
``` purescript
Newtype ListPrincipalThingsResponse _
```

#### `ListRoleAliasesRequest`

``` purescript
newtype ListRoleAliasesRequest
  = ListRoleAliasesRequest { "PageSize'" :: NullOrUndefined (PageSize), "Marker'" :: NullOrUndefined (Marker), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

##### Instances
``` purescript
Newtype ListRoleAliasesRequest _
```

#### `ListRoleAliasesResponse`

``` purescript
newtype ListRoleAliasesResponse
  = ListRoleAliasesResponse { "RoleAliases'" :: NullOrUndefined (RoleAliases), "NextMarker'" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype ListRoleAliasesResponse _
```

#### `ListStreamsRequest`

``` purescript
newtype ListStreamsRequest
  = ListStreamsRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken), "AscendingOrder'" :: NullOrUndefined (AscendingOrder) }
```

##### Instances
``` purescript
Newtype ListStreamsRequest _
```

#### `ListStreamsResponse`

``` purescript
newtype ListStreamsResponse
  = ListStreamsResponse { "Streams'" :: NullOrUndefined (StreamsSummary), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListStreamsResponse _
```

#### `ListTargetsForPolicyRequest`

``` purescript
newtype ListTargetsForPolicyRequest
  = ListTargetsForPolicyRequest { "PolicyName'" :: PolicyName, "Marker'" :: NullOrUndefined (Marker), "PageSize'" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype ListTargetsForPolicyRequest _
```

#### `ListTargetsForPolicyResponse`

``` purescript
newtype ListTargetsForPolicyResponse
  = ListTargetsForPolicyResponse { "Targets'" :: NullOrUndefined (PolicyTargets), "NextMarker'" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype ListTargetsForPolicyResponse _
```

#### `ListThingGroupsForThingRequest`

``` purescript
newtype ListThingGroupsForThingRequest
  = ListThingGroupsForThingRequest { "ThingName'" :: ThingName, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

##### Instances
``` purescript
Newtype ListThingGroupsForThingRequest _
```

#### `ListThingGroupsForThingResponse`

``` purescript
newtype ListThingGroupsForThingResponse
  = ListThingGroupsForThingResponse { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListThingGroupsForThingResponse _
```

#### `ListThingGroupsRequest`

``` purescript
newtype ListThingGroupsRequest
  = ListThingGroupsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "ParentGroup'" :: NullOrUndefined (ThingGroupName), "NamePrefixFilter'" :: NullOrUndefined (ThingGroupName), "Recursive'" :: NullOrUndefined (RecursiveWithoutDefault) }
```

##### Instances
``` purescript
Newtype ListThingGroupsRequest _
```

#### `ListThingGroupsResponse`

``` purescript
newtype ListThingGroupsResponse
  = ListThingGroupsResponse { "ThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListThingGroupsResponse _
```

#### `ListThingPrincipalsRequest`

``` purescript
newtype ListThingPrincipalsRequest
  = ListThingPrincipalsRequest { "ThingName'" :: ThingName }
```

<p>The input for the ListThingPrincipal operation.</p>

##### Instances
``` purescript
Newtype ListThingPrincipalsRequest _
```

#### `ListThingPrincipalsResponse`

``` purescript
newtype ListThingPrincipalsResponse
  = ListThingPrincipalsResponse { "Principals'" :: NullOrUndefined (Principals) }
```

<p>The output from the ListThingPrincipals operation.</p>

##### Instances
``` purescript
Newtype ListThingPrincipalsResponse _
```

#### `ListThingRegistrationTaskReportsRequest`

``` purescript
newtype ListThingRegistrationTaskReportsRequest
  = ListThingRegistrationTaskReportsRequest { "TaskId'" :: TaskId, "ReportType'" :: ReportType, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

##### Instances
``` purescript
Newtype ListThingRegistrationTaskReportsRequest _
```

#### `ListThingRegistrationTaskReportsResponse`

``` purescript
newtype ListThingRegistrationTaskReportsResponse
  = ListThingRegistrationTaskReportsResponse { "ResourceLinks'" :: NullOrUndefined (S3FileUrlList), "ReportType'" :: NullOrUndefined (ReportType), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListThingRegistrationTaskReportsResponse _
```

#### `ListThingRegistrationTasksRequest`

``` purescript
newtype ListThingRegistrationTasksRequest
  = ListThingRegistrationTasksRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "Status'" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype ListThingRegistrationTasksRequest _
```

#### `ListThingRegistrationTasksResponse`

``` purescript
newtype ListThingRegistrationTasksResponse
  = ListThingRegistrationTasksResponse { "TaskIds'" :: NullOrUndefined (TaskIdList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListThingRegistrationTasksResponse _
```

#### `ListThingTypesRequest`

``` purescript
newtype ListThingTypesRequest
  = ListThingTypesRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "ThingTypeName'" :: NullOrUndefined (ThingTypeName) }
```

<p>The input for the ListThingTypes operation.</p>

##### Instances
``` purescript
Newtype ListThingTypesRequest _
```

#### `ListThingTypesResponse`

``` purescript
newtype ListThingTypesResponse
  = ListThingTypesResponse { "ThingTypes'" :: NullOrUndefined (ThingTypeList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output for the ListThingTypes operation.</p>

##### Instances
``` purescript
Newtype ListThingTypesResponse _
```

#### `ListThingsInThingGroupRequest`

``` purescript
newtype ListThingsInThingGroupRequest
  = ListThingsInThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "Recursive'" :: NullOrUndefined (Recursive), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults) }
```

##### Instances
``` purescript
Newtype ListThingsInThingGroupRequest _
```

#### `ListThingsInThingGroupResponse`

``` purescript
newtype ListThingsInThingGroupResponse
  = ListThingsInThingGroupResponse { "Things'" :: NullOrUndefined (ThingNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListThingsInThingGroupResponse _
```

#### `ListThingsRequest`

``` purescript
newtype ListThingsRequest
  = ListThingsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (RegistryMaxResults), "AttributeName'" :: NullOrUndefined (AttributeName), "AttributeValue'" :: NullOrUndefined (AttributeValue), "ThingTypeName'" :: NullOrUndefined (ThingTypeName) }
```

<p>The input for the ListThings operation.</p>

##### Instances
``` purescript
Newtype ListThingsRequest _
```

#### `ListThingsResponse`

``` purescript
newtype ListThingsResponse
  = ListThingsResponse { "Things'" :: NullOrUndefined (ThingAttributeList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListThings operation.</p>

##### Instances
``` purescript
Newtype ListThingsResponse _
```

#### `ListTopicRulesRequest`

``` purescript
newtype ListTopicRulesRequest
  = ListTopicRulesRequest { "Topic'" :: NullOrUndefined (Topic), "MaxResults'" :: NullOrUndefined (GEMaxResults), "NextToken'" :: NullOrUndefined (NextToken), "RuleDisabled'" :: NullOrUndefined (IsDisabled) }
```

<p>The input for the ListTopicRules operation.</p>

##### Instances
``` purescript
Newtype ListTopicRulesRequest _
```

#### `ListTopicRulesResponse`

``` purescript
newtype ListTopicRulesResponse
  = ListTopicRulesResponse { "Rules'" :: NullOrUndefined (TopicRuleList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output from the ListTopicRules operation.</p>

##### Instances
``` purescript
Newtype ListTopicRulesResponse _
```

#### `ListV2LoggingLevelsRequest`

``` purescript
newtype ListV2LoggingLevelsRequest
  = ListV2LoggingLevelsRequest { "TargetType'" :: NullOrUndefined (LogTargetType), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (SkyfallMaxResults) }
```

##### Instances
``` purescript
Newtype ListV2LoggingLevelsRequest _
```

#### `ListV2LoggingLevelsResponse`

``` purescript
newtype ListV2LoggingLevelsResponse
  = ListV2LoggingLevelsResponse { "LogTargetConfigurations'" :: NullOrUndefined (LogTargetConfigurations), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListV2LoggingLevelsResponse _
```

#### `LogLevel`

``` purescript
newtype LogLevel
  = LogLevel String
```

##### Instances
``` purescript
Newtype LogLevel _
```

#### `LogTarget`

``` purescript
newtype LogTarget
  = LogTarget { "TargetType'" :: LogTargetType, "TargetName'" :: NullOrUndefined (LogTargetName) }
```

<p>A log target.</p>

##### Instances
``` purescript
Newtype LogTarget _
```

#### `LogTargetConfiguration`

``` purescript
newtype LogTargetConfiguration
  = LogTargetConfiguration { "LogTarget'" :: NullOrUndefined (LogTarget), "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>The target configuration.</p>

##### Instances
``` purescript
Newtype LogTargetConfiguration _
```

#### `LogTargetConfigurations`

``` purescript
newtype LogTargetConfigurations
  = LogTargetConfigurations (Array LogTargetConfiguration)
```

##### Instances
``` purescript
Newtype LogTargetConfigurations _
```

#### `LogTargetName`

``` purescript
newtype LogTargetName
  = LogTargetName String
```

##### Instances
``` purescript
Newtype LogTargetName _
```

#### `LogTargetType`

``` purescript
newtype LogTargetType
  = LogTargetType String
```

##### Instances
``` purescript
Newtype LogTargetType _
```

#### `LoggingOptionsPayload`

``` purescript
newtype LoggingOptionsPayload
  = LoggingOptionsPayload { "RoleArn'" :: AwsArn, "LogLevel'" :: NullOrUndefined (LogLevel) }
```

<p>Describes the logging options payload.</p>

##### Instances
``` purescript
Newtype LoggingOptionsPayload _
```

#### `MalformedPolicyException`

``` purescript
newtype MalformedPolicyException
  = MalformedPolicyException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The policy documentation is not valid.</p>

##### Instances
``` purescript
Newtype MalformedPolicyException _
```

#### `Marker`

``` purescript
newtype Marker
  = Marker String
```

##### Instances
``` purescript
Newtype Marker _
```

#### `MaxJobExecutionsPerMin`

``` purescript
newtype MaxJobExecutionsPerMin
  = MaxJobExecutionsPerMin Int
```

##### Instances
``` purescript
Newtype MaxJobExecutionsPerMin _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `MessageFormat`

``` purescript
newtype MessageFormat
  = MessageFormat String
```

##### Instances
``` purescript
Newtype MessageFormat _
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

##### Instances
``` purescript
Newtype MetricName _
```

#### `MetricNamespace`

``` purescript
newtype MetricNamespace
  = MetricNamespace String
```

##### Instances
``` purescript
Newtype MetricNamespace _
```

#### `MetricTimestamp`

``` purescript
newtype MetricTimestamp
  = MetricTimestamp String
```

##### Instances
``` purescript
Newtype MetricTimestamp _
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

##### Instances
``` purescript
Newtype MetricUnit _
```

#### `MetricValue`

``` purescript
newtype MetricValue
  = MetricValue String
```

##### Instances
``` purescript
Newtype MetricValue _
```

#### `MissingContextValue`

``` purescript
newtype MissingContextValue
  = MissingContextValue String
```

##### Instances
``` purescript
Newtype MissingContextValue _
```

#### `MissingContextValues`

``` purescript
newtype MissingContextValues
  = MissingContextValues (Array MissingContextValue)
```

##### Instances
``` purescript
Newtype MissingContextValues _
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

#### `NotConfiguredException`

``` purescript
newtype NotConfiguredException
  = NotConfiguredException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The resource is not configured.</p>

##### Instances
``` purescript
Newtype NotConfiguredException _
```

#### `OTAUpdateArn`

``` purescript
newtype OTAUpdateArn
  = OTAUpdateArn String
```

##### Instances
``` purescript
Newtype OTAUpdateArn _
```

#### `OTAUpdateDescription`

``` purescript
newtype OTAUpdateDescription
  = OTAUpdateDescription String
```

##### Instances
``` purescript
Newtype OTAUpdateDescription _
```

#### `OTAUpdateErrorMessage`

``` purescript
newtype OTAUpdateErrorMessage
  = OTAUpdateErrorMessage String
```

##### Instances
``` purescript
Newtype OTAUpdateErrorMessage _
```

#### `OTAUpdateFile`

``` purescript
newtype OTAUpdateFile
  = OTAUpdateFile { "FileName'" :: NullOrUndefined (FileName), "FileVersion'" :: NullOrUndefined (OTAUpdateFileVersion), "FileSource'" :: NullOrUndefined (Stream), "CodeSigning'" :: NullOrUndefined (CodeSigning), "Attributes'" :: NullOrUndefined (AttributesMap) }
```

<p>Describes a file to be associated with an OTA update.</p>

##### Instances
``` purescript
Newtype OTAUpdateFile _
```

#### `OTAUpdateFileVersion`

``` purescript
newtype OTAUpdateFileVersion
  = OTAUpdateFileVersion String
```

##### Instances
``` purescript
Newtype OTAUpdateFileVersion _
```

#### `OTAUpdateFiles`

``` purescript
newtype OTAUpdateFiles
  = OTAUpdateFiles (Array OTAUpdateFile)
```

##### Instances
``` purescript
Newtype OTAUpdateFiles _
```

#### `OTAUpdateId`

``` purescript
newtype OTAUpdateId
  = OTAUpdateId String
```

##### Instances
``` purescript
Newtype OTAUpdateId _
```

#### `OTAUpdateInfo`

``` purescript
newtype OTAUpdateInfo
  = OTAUpdateInfo { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType), "Description'" :: NullOrUndefined (OTAUpdateDescription), "Targets'" :: NullOrUndefined (Targets), "TargetSelection'" :: NullOrUndefined (TargetSelection), "OtaUpdateFiles'" :: NullOrUndefined (OTAUpdateFiles), "OtaUpdateStatus'" :: NullOrUndefined (OTAUpdateStatus), "AwsIotJobId'" :: NullOrUndefined (AwsIotJobId), "AwsIotJobArn'" :: NullOrUndefined (AwsIotJobArn), "ErrorInfo'" :: NullOrUndefined (ErrorInfo), "AdditionalParameters'" :: NullOrUndefined (AdditionalParameterMap) }
```

<p>Information about an OTA update.</p>

##### Instances
``` purescript
Newtype OTAUpdateInfo _
```

#### `OTAUpdateStatus`

``` purescript
newtype OTAUpdateStatus
  = OTAUpdateStatus String
```

##### Instances
``` purescript
Newtype OTAUpdateStatus _
```

#### `OTAUpdateSummary`

``` purescript
newtype OTAUpdateSummary
  = OTAUpdateSummary { "OtaUpdateId'" :: NullOrUndefined (OTAUpdateId), "OtaUpdateArn'" :: NullOrUndefined (OTAUpdateArn), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>An OTA update summary.</p>

##### Instances
``` purescript
Newtype OTAUpdateSummary _
```

#### `OTAUpdatesSummary`

``` purescript
newtype OTAUpdatesSummary
  = OTAUpdatesSummary (Array OTAUpdateSummary)
```

##### Instances
``` purescript
Newtype OTAUpdatesSummary _
```

#### `OptionalVersion`

``` purescript
newtype OptionalVersion
  = OptionalVersion Number
```

##### Instances
``` purescript
Newtype OptionalVersion _
```

#### `OutgoingCertificate`

``` purescript
newtype OutgoingCertificate
  = OutgoingCertificate { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId), "TransferredTo'" :: NullOrUndefined (AwsAccountId), "TransferDate'" :: NullOrUndefined (DateType), "TransferMessage'" :: NullOrUndefined (Message), "CreationDate'" :: NullOrUndefined (DateType) }
```

<p>A certificate that has been transferred but not yet accepted.</p>

##### Instances
``` purescript
Newtype OutgoingCertificate _
```

#### `OutgoingCertificates`

``` purescript
newtype OutgoingCertificates
  = OutgoingCertificates (Array OutgoingCertificate)
```

##### Instances
``` purescript
Newtype OutgoingCertificates _
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

##### Instances
``` purescript
Newtype PageSize _
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter String
```

##### Instances
``` purescript
Newtype Parameter _
```

#### `ParameterKey`

``` purescript
newtype ParameterKey
  = ParameterKey String
```

##### Instances
``` purescript
Newtype ParameterKey _
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

##### Instances
``` purescript
Newtype ParameterValue _
```

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Map Parameter Value)
```

##### Instances
``` purescript
Newtype Parameters _
```

#### `PartitionKey`

``` purescript
newtype PartitionKey
  = PartitionKey String
```

##### Instances
``` purescript
Newtype PartitionKey _
```

#### `PayloadField`

``` purescript
newtype PayloadField
  = PayloadField String
```

##### Instances
``` purescript
Newtype PayloadField _
```

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

##### Instances
``` purescript
Newtype Percentage _
```

#### `Policies`

``` purescript
newtype Policies
  = Policies (Array Policy)
```

##### Instances
``` purescript
Newtype Policies _
```

#### `Policy`

``` purescript
newtype Policy
  = Policy { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyArn'" :: NullOrUndefined (PolicyArn) }
```

<p>Describes an AWS IoT policy.</p>

##### Instances
``` purescript
Newtype Policy _
```

#### `PolicyArn`

``` purescript
newtype PolicyArn
  = PolicyArn String
```

##### Instances
``` purescript
Newtype PolicyArn _
```

#### `PolicyDocument`

``` purescript
newtype PolicyDocument
  = PolicyDocument String
```

##### Instances
``` purescript
Newtype PolicyDocument _
```

#### `PolicyDocuments`

``` purescript
newtype PolicyDocuments
  = PolicyDocuments (Array PolicyDocument)
```

##### Instances
``` purescript
Newtype PolicyDocuments _
```

#### `PolicyName`

``` purescript
newtype PolicyName
  = PolicyName String
```

##### Instances
``` purescript
Newtype PolicyName _
```

#### `PolicyNames`

``` purescript
newtype PolicyNames
  = PolicyNames (Array PolicyName)
```

##### Instances
``` purescript
Newtype PolicyNames _
```

#### `PolicyTarget`

``` purescript
newtype PolicyTarget
  = PolicyTarget String
```

##### Instances
``` purescript
Newtype PolicyTarget _
```

#### `PolicyTargets`

``` purescript
newtype PolicyTargets
  = PolicyTargets (Array PolicyTarget)
```

##### Instances
``` purescript
Newtype PolicyTargets _
```

#### `PolicyVersion`

``` purescript
newtype PolicyVersion
  = PolicyVersion { "VersionId'" :: NullOrUndefined (PolicyVersionId), "IsDefaultVersion'" :: NullOrUndefined (IsDefaultVersion), "CreateDate'" :: NullOrUndefined (DateType) }
```

<p>Describes a policy version.</p>

##### Instances
``` purescript
Newtype PolicyVersion _
```

#### `PolicyVersionId`

``` purescript
newtype PolicyVersionId
  = PolicyVersionId String
```

##### Instances
``` purescript
Newtype PolicyVersionId _
```

#### `PolicyVersions`

``` purescript
newtype PolicyVersions
  = PolicyVersions (Array PolicyVersion)
```

##### Instances
``` purescript
Newtype PolicyVersions _
```

#### `PresignedUrlConfig`

``` purescript
newtype PresignedUrlConfig
  = PresignedUrlConfig { "RoleArn'" :: NullOrUndefined (RoleArn), "ExpiresInSec'" :: NullOrUndefined (ExpiresInSec) }
```

<p>Configuration for pre-signed S3 URLs.</p>

##### Instances
``` purescript
Newtype PresignedUrlConfig _
```

#### `Principal`

``` purescript
newtype Principal
  = Principal String
```

##### Instances
``` purescript
Newtype Principal _
```

#### `PrincipalArn`

``` purescript
newtype PrincipalArn
  = PrincipalArn String
```

##### Instances
``` purescript
Newtype PrincipalArn _
```

#### `PrincipalId`

``` purescript
newtype PrincipalId
  = PrincipalId String
```

##### Instances
``` purescript
Newtype PrincipalId _
```

#### `Principals`

``` purescript
newtype Principals
  = Principals (Array PrincipalArn)
```

##### Instances
``` purescript
Newtype Principals _
```

#### `PrivateKey`

``` purescript
newtype PrivateKey
  = PrivateKey String
```

##### Instances
``` purescript
Newtype PrivateKey _
```

#### `ProcessingTargetName`

``` purescript
newtype ProcessingTargetName
  = ProcessingTargetName String
```

##### Instances
``` purescript
Newtype ProcessingTargetName _
```

#### `ProcessingTargetNameList`

``` purescript
newtype ProcessingTargetNameList
  = ProcessingTargetNameList (Array ProcessingTargetName)
```

##### Instances
``` purescript
Newtype ProcessingTargetNameList _
```

#### `PublicKey`

``` purescript
newtype PublicKey
  = PublicKey String
```

##### Instances
``` purescript
Newtype PublicKey _
```

#### `PublicKeyMap`

``` purescript
newtype PublicKeyMap
  = PublicKeyMap (Map KeyName KeyValue)
```

##### Instances
``` purescript
Newtype PublicKeyMap _
```

#### `PutItemInput`

``` purescript
newtype PutItemInput
  = PutItemInput { "TableName'" :: TableName }
```

<p>The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.</p>

##### Instances
``` purescript
Newtype PutItemInput _
```

#### `QueryMaxResults`

``` purescript
newtype QueryMaxResults
  = QueryMaxResults Int
```

##### Instances
``` purescript
Newtype QueryMaxResults _
```

#### `QueryString`

``` purescript
newtype QueryString
  = QueryString String
```

##### Instances
``` purescript
Newtype QueryString _
```

#### `QueryVersion`

``` purescript
newtype QueryVersion
  = QueryVersion String
```

##### Instances
``` purescript
Newtype QueryVersion _
```

#### `QueueUrl`

``` purescript
newtype QueueUrl
  = QueueUrl String
```

##### Instances
``` purescript
Newtype QueueUrl _
```

#### `QueuedThings`

``` purescript
newtype QueuedThings
  = QueuedThings Int
```

##### Instances
``` purescript
Newtype QueuedThings _
```

#### `RangeKeyField`

``` purescript
newtype RangeKeyField
  = RangeKeyField String
```

##### Instances
``` purescript
Newtype RangeKeyField _
```

#### `RangeKeyValue`

``` purescript
newtype RangeKeyValue
  = RangeKeyValue String
```

##### Instances
``` purescript
Newtype RangeKeyValue _
```

#### `Recursive`

``` purescript
newtype Recursive
  = Recursive Boolean
```

##### Instances
``` purescript
Newtype Recursive _
```

#### `RecursiveWithoutDefault`

``` purescript
newtype RecursiveWithoutDefault
  = RecursiveWithoutDefault Boolean
```

##### Instances
``` purescript
Newtype RecursiveWithoutDefault _
```

#### `RegisterCACertificateRequest`

``` purescript
newtype RegisterCACertificateRequest
  = RegisterCACertificateRequest { "CaCertificate'" :: CertificatePem, "VerificationCertificate'" :: CertificatePem, "SetAsActive'" :: NullOrUndefined (SetAsActive), "AllowAutoRegistration'" :: NullOrUndefined (AllowAutoRegistration), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig) }
```

<p>The input to the RegisterCACertificate operation.</p>

##### Instances
``` purescript
Newtype RegisterCACertificateRequest _
```

#### `RegisterCACertificateResponse`

``` purescript
newtype RegisterCACertificateResponse
  = RegisterCACertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId) }
```

<p>The output from the RegisterCACertificateResponse operation.</p>

##### Instances
``` purescript
Newtype RegisterCACertificateResponse _
```

#### `RegisterCertificateRequest`

``` purescript
newtype RegisterCertificateRequest
  = RegisterCertificateRequest { "CertificatePem'" :: CertificatePem, "CaCertificatePem'" :: NullOrUndefined (CertificatePem), "SetAsActive'" :: NullOrUndefined (SetAsActiveFlag), "Status'" :: NullOrUndefined (CertificateStatus) }
```

<p>The input to the RegisterCertificate operation.</p>

##### Instances
``` purescript
Newtype RegisterCertificateRequest _
```

#### `RegisterCertificateResponse`

``` purescript
newtype RegisterCertificateResponse
  = RegisterCertificateResponse { "CertificateArn'" :: NullOrUndefined (CertificateArn), "CertificateId'" :: NullOrUndefined (CertificateId) }
```

<p>The output from the RegisterCertificate operation.</p>

##### Instances
``` purescript
Newtype RegisterCertificateResponse _
```

#### `RegisterThingRequest`

``` purescript
newtype RegisterThingRequest
  = RegisterThingRequest { "TemplateBody'" :: TemplateBody, "Parameters'" :: NullOrUndefined (Parameters) }
```

##### Instances
``` purescript
Newtype RegisterThingRequest _
```

#### `RegisterThingResponse`

``` purescript
newtype RegisterThingResponse
  = RegisterThingResponse { "CertificatePem'" :: NullOrUndefined (CertificatePem), "ResourceArns'" :: NullOrUndefined (ResourceArns) }
```

##### Instances
``` purescript
Newtype RegisterThingResponse _
```

#### `RegistrationCode`

``` purescript
newtype RegistrationCode
  = RegistrationCode String
```

##### Instances
``` purescript
Newtype RegistrationCode _
```

#### `RegistrationCodeValidationException`

``` purescript
newtype RegistrationCodeValidationException
  = RegistrationCodeValidationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The registration code is invalid.</p>

##### Instances
``` purescript
Newtype RegistrationCodeValidationException _
```

#### `RegistrationConfig`

``` purescript
newtype RegistrationConfig
  = RegistrationConfig { "TemplateBody'" :: NullOrUndefined (TemplateBody), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>The registration configuration.</p>

##### Instances
``` purescript
Newtype RegistrationConfig _
```

#### `RegistryMaxResults`

``` purescript
newtype RegistryMaxResults
  = RegistryMaxResults Int
```

##### Instances
``` purescript
Newtype RegistryMaxResults _
```

#### `RegistryS3BucketName`

``` purescript
newtype RegistryS3BucketName
  = RegistryS3BucketName String
```

##### Instances
``` purescript
Newtype RegistryS3BucketName _
```

#### `RegistryS3KeyName`

``` purescript
newtype RegistryS3KeyName
  = RegistryS3KeyName String
```

##### Instances
``` purescript
Newtype RegistryS3KeyName _
```

#### `RejectCertificateTransferRequest`

``` purescript
newtype RejectCertificateTransferRequest
  = RejectCertificateTransferRequest { "CertificateId'" :: CertificateId, "RejectReason'" :: NullOrUndefined (Message) }
```

<p>The input for the RejectCertificateTransfer operation.</p>

##### Instances
``` purescript
Newtype RejectCertificateTransferRequest _
```

#### `RejectedThings`

``` purescript
newtype RejectedThings
  = RejectedThings Int
```

##### Instances
``` purescript
Newtype RejectedThings _
```

#### `RemoveAutoRegistration`

``` purescript
newtype RemoveAutoRegistration
  = RemoveAutoRegistration Boolean
```

##### Instances
``` purescript
Newtype RemoveAutoRegistration _
```

#### `RemoveThingFromThingGroupRequest`

``` purescript
newtype RemoveThingFromThingGroupRequest
  = RemoveThingFromThingGroupRequest { "ThingGroupName'" :: NullOrUndefined (ThingGroupName), "ThingGroupArn'" :: NullOrUndefined (ThingGroupArn), "ThingName'" :: NullOrUndefined (ThingName), "ThingArn'" :: NullOrUndefined (ThingArn) }
```

##### Instances
``` purescript
Newtype RemoveThingFromThingGroupRequest _
```

#### `RemoveThingFromThingGroupResponse`

``` purescript
newtype RemoveThingFromThingGroupResponse
  = RemoveThingFromThingGroupResponse {  }
```

##### Instances
``` purescript
Newtype RemoveThingFromThingGroupResponse _
```

#### `RemoveThingType`

``` purescript
newtype RemoveThingType
  = RemoveThingType Boolean
```

##### Instances
``` purescript
Newtype RemoveThingType _
```

#### `RemovedThings`

``` purescript
newtype RemovedThings
  = RemovedThings Int
```

##### Instances
``` purescript
Newtype RemovedThings _
```

#### `ReplaceTopicRuleRequest`

``` purescript
newtype ReplaceTopicRuleRequest
  = ReplaceTopicRuleRequest { "RuleName'" :: RuleName, "TopicRulePayload'" :: TopicRulePayload }
```

<p>The input for the ReplaceTopicRule operation.</p>

##### Instances
``` purescript
Newtype ReplaceTopicRuleRequest _
```

#### `ReportType`

``` purescript
newtype ReportType
  = ReportType String
```

##### Instances
``` purescript
Newtype ReportType _
```

#### `RepublishAction`

``` purescript
newtype RepublishAction
  = RepublishAction { "RoleArn'" :: AwsArn, "Topic'" :: TopicPattern }
```

<p>Describes an action to republish to another topic.</p>

##### Instances
``` purescript
Newtype RepublishAction _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource String
```

##### Instances
``` purescript
Newtype Resource _
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException { "Message'" :: NullOrUndefined (ErrorMessage'), "ResourceId'" :: NullOrUndefined (ResourceId'), "ResourceArn'" :: NullOrUndefined (ResourceArn') }
```

<p>The resource already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsException _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceArns`

``` purescript
newtype ResourceArns
  = ResourceArns (Map ResourceLogicalId ResourceArn)
```

##### Instances
``` purescript
Newtype ResourceArns _
```

#### `ResourceLogicalId`

``` purescript
newtype ResourceLogicalId
  = ResourceLogicalId String
```

##### Instances
``` purescript
Newtype ResourceLogicalId _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The specified resource does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceRegistrationFailureException`

``` purescript
newtype ResourceRegistrationFailureException
  = ResourceRegistrationFailureException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The resource registration failed.</p>

##### Instances
``` purescript
Newtype ResourceRegistrationFailureException _
```

#### `Resources`

``` purescript
newtype Resources
  = Resources (Array Resource)
```

##### Instances
``` purescript
Newtype Resources _
```

#### `RoleAlias`

``` purescript
newtype RoleAlias
  = RoleAlias String
```

##### Instances
``` purescript
Newtype RoleAlias _
```

#### `RoleAliasArn`

``` purescript
newtype RoleAliasArn
  = RoleAliasArn String
```

##### Instances
``` purescript
Newtype RoleAliasArn _
```

#### `RoleAliasDescription`

``` purescript
newtype RoleAliasDescription
  = RoleAliasDescription { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleArn'" :: NullOrUndefined (RoleArn), "Owner'" :: NullOrUndefined (AwsAccountId), "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds), "CreationDate'" :: NullOrUndefined (DateType), "LastModifiedDate'" :: NullOrUndefined (DateType) }
```

<p>Role alias description.</p>

##### Instances
``` purescript
Newtype RoleAliasDescription _
```

#### `RoleAliases`

``` purescript
newtype RoleAliases
  = RoleAliases (Array RoleAlias)
```

##### Instances
``` purescript
Newtype RoleAliases _
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

##### Instances
``` purescript
Newtype RoleArn _
```

#### `RuleArn`

``` purescript
newtype RuleArn
  = RuleArn String
```

##### Instances
``` purescript
Newtype RuleArn _
```

#### `RuleName`

``` purescript
newtype RuleName
  = RuleName String
```

##### Instances
``` purescript
Newtype RuleName _
```

#### `S3Action`

``` purescript
newtype S3Action
  = S3Action { "RoleArn'" :: AwsArn, "BucketName'" :: BucketName, "Key'" :: Key, "CannedAcl'" :: NullOrUndefined (CannedAccessControlList) }
```

<p>Describes an action to write data to an Amazon S3 bucket.</p>

##### Instances
``` purescript
Newtype S3Action _
```

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

##### Instances
``` purescript
Newtype S3Bucket _
```

#### `S3FileUrl`

``` purescript
newtype S3FileUrl
  = S3FileUrl String
```

##### Instances
``` purescript
Newtype S3FileUrl _
```

#### `S3FileUrlList`

``` purescript
newtype S3FileUrlList
  = S3FileUrlList (Array S3FileUrl)
```

##### Instances
``` purescript
Newtype S3FileUrlList _
```

#### `S3Key`

``` purescript
newtype S3Key
  = S3Key String
```

##### Instances
``` purescript
Newtype S3Key _
```

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "Bucket'" :: S3Bucket, "Key'" :: S3Key, "Version'" :: NullOrUndefined (S3Version) }
```

<p>The location in S3 the contains the files to stream.</p>

##### Instances
``` purescript
Newtype S3Location _
```

#### `S3Version`

``` purescript
newtype S3Version
  = S3Version String
```

##### Instances
``` purescript
Newtype S3Version _
```

#### `SQL`

``` purescript
newtype SQL
  = SQL String
```

##### Instances
``` purescript
Newtype SQL _
```

#### `SalesforceAction`

``` purescript
newtype SalesforceAction
  = SalesforceAction { "Token'" :: SalesforceToken, "Url'" :: SalesforceEndpoint }
```

<p>Describes an action to write a message to a Salesforce IoT Cloud Input Stream.</p>

##### Instances
``` purescript
Newtype SalesforceAction _
```

#### `SalesforceEndpoint`

``` purescript
newtype SalesforceEndpoint
  = SalesforceEndpoint String
```

##### Instances
``` purescript
Newtype SalesforceEndpoint _
```

#### `SalesforceToken`

``` purescript
newtype SalesforceToken
  = SalesforceToken String
```

##### Instances
``` purescript
Newtype SalesforceToken _
```

#### `SearchIndexRequest`

``` purescript
newtype SearchIndexRequest
  = SearchIndexRequest { "IndexName'" :: NullOrUndefined (IndexName), "QueryString'" :: QueryString, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (QueryMaxResults), "QueryVersion'" :: NullOrUndefined (QueryVersion) }
```

##### Instances
``` purescript
Newtype SearchIndexRequest _
```

#### `SearchIndexResponse`

``` purescript
newtype SearchIndexResponse
  = SearchIndexResponse { "NextToken'" :: NullOrUndefined (NextToken), "Things'" :: NullOrUndefined (ThingDocumentList) }
```

##### Instances
``` purescript
Newtype SearchIndexResponse _
```

#### `SearchableAttributes`

``` purescript
newtype SearchableAttributes
  = SearchableAttributes (Array AttributeName)
```

##### Instances
``` purescript
Newtype SearchableAttributes _
```

#### `Seconds`

``` purescript
newtype Seconds
  = Seconds Int
```

##### Instances
``` purescript
Newtype Seconds _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The service is temporarily unavailable.</p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `SetAsActive`

``` purescript
newtype SetAsActive
  = SetAsActive Boolean
```

##### Instances
``` purescript
Newtype SetAsActive _
```

#### `SetAsActiveFlag`

``` purescript
newtype SetAsActiveFlag
  = SetAsActiveFlag Boolean
```

##### Instances
``` purescript
Newtype SetAsActiveFlag _
```

#### `SetAsDefault`

``` purescript
newtype SetAsDefault
  = SetAsDefault Boolean
```

##### Instances
``` purescript
Newtype SetAsDefault _
```

#### `SetDefaultAuthorizerRequest`

``` purescript
newtype SetDefaultAuthorizerRequest
  = SetDefaultAuthorizerRequest { "AuthorizerName'" :: AuthorizerName }
```

##### Instances
``` purescript
Newtype SetDefaultAuthorizerRequest _
```

#### `SetDefaultAuthorizerResponse`

``` purescript
newtype SetDefaultAuthorizerResponse
  = SetDefaultAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

##### Instances
``` purescript
Newtype SetDefaultAuthorizerResponse _
```

#### `SetDefaultPolicyVersionRequest`

``` purescript
newtype SetDefaultPolicyVersionRequest
  = SetDefaultPolicyVersionRequest { "PolicyName'" :: PolicyName, "PolicyVersionId'" :: PolicyVersionId }
```

<p>The input for the SetDefaultPolicyVersion operation.</p>

##### Instances
``` purescript
Newtype SetDefaultPolicyVersionRequest _
```

#### `SetLoggingOptionsRequest`

``` purescript
newtype SetLoggingOptionsRequest
  = SetLoggingOptionsRequest { "LoggingOptionsPayload'" :: LoggingOptionsPayload }
```

<p>The input for the SetLoggingOptions operation.</p>

##### Instances
``` purescript
Newtype SetLoggingOptionsRequest _
```

#### `SetV2LoggingLevelRequest`

``` purescript
newtype SetV2LoggingLevelRequest
  = SetV2LoggingLevelRequest { "LogTarget'" :: LogTarget, "LogLevel'" :: LogLevel }
```

##### Instances
``` purescript
Newtype SetV2LoggingLevelRequest _
```

#### `SetV2LoggingOptionsRequest`

``` purescript
newtype SetV2LoggingOptionsRequest
  = SetV2LoggingOptionsRequest { "RoleArn'" :: NullOrUndefined (AwsArn), "DefaultLogLevel'" :: NullOrUndefined (LogLevel), "DisableAllLogs'" :: NullOrUndefined (DisableAllLogs) }
```

##### Instances
``` purescript
Newtype SetV2LoggingOptionsRequest _
```

#### `Signature`

``` purescript
newtype Signature
  = Signature String
```

##### Instances
``` purescript
Newtype Signature _
```

#### `SignatureAlgorithm`

``` purescript
newtype SignatureAlgorithm
  = SignatureAlgorithm String
```

##### Instances
``` purescript
Newtype SignatureAlgorithm _
```

#### `SigningJobId`

``` purescript
newtype SigningJobId
  = SigningJobId String
```

##### Instances
``` purescript
Newtype SigningJobId _
```

#### `SkyfallMaxResults`

``` purescript
newtype SkyfallMaxResults
  = SkyfallMaxResults Int
```

##### Instances
``` purescript
Newtype SkyfallMaxResults _
```

#### `SnsAction`

``` purescript
newtype SnsAction
  = SnsAction { "TargetArn'" :: AwsArn, "RoleArn'" :: AwsArn, "MessageFormat'" :: NullOrUndefined (MessageFormat) }
```

<p>Describes an action to publish to an Amazon SNS topic.</p>

##### Instances
``` purescript
Newtype SnsAction _
```

#### `SqlParseException`

``` purescript
newtype SqlParseException
  = SqlParseException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The Rule-SQL expression can't be parsed correctly.</p>

##### Instances
``` purescript
Newtype SqlParseException _
```

#### `SqsAction`

``` purescript
newtype SqsAction
  = SqsAction { "RoleArn'" :: AwsArn, "QueueUrl'" :: QueueUrl, "UseBase64'" :: NullOrUndefined (UseBase64) }
```

<p>Describes an action to publish data to an Amazon SQS queue.</p>

##### Instances
``` purescript
Newtype SqsAction _
```

#### `StartThingRegistrationTaskRequest`

``` purescript
newtype StartThingRegistrationTaskRequest
  = StartThingRegistrationTaskRequest { "TemplateBody'" :: TemplateBody, "InputFileBucket'" :: RegistryS3BucketName, "InputFileKey'" :: RegistryS3KeyName, "RoleArn'" :: RoleArn }
```

##### Instances
``` purescript
Newtype StartThingRegistrationTaskRequest _
```

#### `StartThingRegistrationTaskResponse`

``` purescript
newtype StartThingRegistrationTaskResponse
  = StartThingRegistrationTaskResponse { "TaskId'" :: NullOrUndefined (TaskId) }
```

##### Instances
``` purescript
Newtype StartThingRegistrationTaskResponse _
```

#### `StateReason`

``` purescript
newtype StateReason
  = StateReason String
```

##### Instances
``` purescript
Newtype StateReason _
```

#### `StateValue`

``` purescript
newtype StateValue
  = StateValue String
```

##### Instances
``` purescript
Newtype StateValue _
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

##### Instances
``` purescript
Newtype Status _
```

#### `StopThingRegistrationTaskRequest`

``` purescript
newtype StopThingRegistrationTaskRequest
  = StopThingRegistrationTaskRequest { "TaskId'" :: TaskId }
```

##### Instances
``` purescript
Newtype StopThingRegistrationTaskRequest _
```

#### `StopThingRegistrationTaskResponse`

``` purescript
newtype StopThingRegistrationTaskResponse
  = StopThingRegistrationTaskResponse {  }
```

##### Instances
``` purescript
Newtype StopThingRegistrationTaskResponse _
```

#### `Stream`

``` purescript
newtype Stream
  = Stream { "StreamId'" :: NullOrUndefined (StreamId), "FileId'" :: NullOrUndefined (FileId) }
```

<p>Describes a group of files that can be streamed.</p>

##### Instances
``` purescript
Newtype Stream _
```

#### `StreamArn`

``` purescript
newtype StreamArn
  = StreamArn String
```

##### Instances
``` purescript
Newtype StreamArn _
```

#### `StreamDescription`

``` purescript
newtype StreamDescription
  = StreamDescription String
```

##### Instances
``` purescript
Newtype StreamDescription _
```

#### `StreamFile`

``` purescript
newtype StreamFile
  = StreamFile { "FileId'" :: NullOrUndefined (FileId), "S3Location'" :: NullOrUndefined (S3Location) }
```

<p>Represents a file to stream.</p>

##### Instances
``` purescript
Newtype StreamFile _
```

#### `StreamFiles`

``` purescript
newtype StreamFiles
  = StreamFiles (Array StreamFile)
```

##### Instances
``` purescript
Newtype StreamFiles _
```

#### `StreamId`

``` purescript
newtype StreamId
  = StreamId String
```

##### Instances
``` purescript
Newtype StreamId _
```

#### `StreamInfo`

``` purescript
newtype StreamInfo
  = StreamInfo { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "StreamVersion'" :: NullOrUndefined (StreamVersion), "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: NullOrUndefined (StreamFiles), "CreatedAt'" :: NullOrUndefined (DateType), "LastUpdatedAt'" :: NullOrUndefined (DateType), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>Information about a stream.</p>

##### Instances
``` purescript
Newtype StreamInfo _
```

#### `StreamName`

``` purescript
newtype StreamName
  = StreamName String
```

##### Instances
``` purescript
Newtype StreamName _
```

#### `StreamSummary`

``` purescript
newtype StreamSummary
  = StreamSummary { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "StreamVersion'" :: NullOrUndefined (StreamVersion), "Description'" :: NullOrUndefined (StreamDescription) }
```

<p>A summary of a stream.</p>

##### Instances
``` purescript
Newtype StreamSummary _
```

#### `StreamVersion`

``` purescript
newtype StreamVersion
  = StreamVersion Int
```

##### Instances
``` purescript
Newtype StreamVersion _
```

#### `StreamsSummary`

``` purescript
newtype StreamsSummary
  = StreamsSummary (Array StreamSummary)
```

##### Instances
``` purescript
Newtype StreamsSummary _
```

#### `SucceededThings`

``` purescript
newtype SucceededThings
  = SucceededThings Int
```

##### Instances
``` purescript
Newtype SucceededThings _
```

#### `TableName`

``` purescript
newtype TableName
  = TableName String
```

##### Instances
``` purescript
Newtype TableName _
```

#### `Target`

``` purescript
newtype Target
  = Target String
```

##### Instances
``` purescript
Newtype Target _
```

#### `TargetArn`

``` purescript
newtype TargetArn
  = TargetArn String
```

##### Instances
``` purescript
Newtype TargetArn _
```

#### `TargetSelection`

``` purescript
newtype TargetSelection
  = TargetSelection String
```

##### Instances
``` purescript
Newtype TargetSelection _
```

#### `Targets`

``` purescript
newtype Targets
  = Targets (Array Target)
```

##### Instances
``` purescript
Newtype Targets _
```

#### `TaskId`

``` purescript
newtype TaskId
  = TaskId String
```

##### Instances
``` purescript
Newtype TaskId _
```

#### `TaskIdList`

``` purescript
newtype TaskIdList
  = TaskIdList (Array TaskId)
```

##### Instances
``` purescript
Newtype TaskIdList _
```

#### `TemplateBody`

``` purescript
newtype TemplateBody
  = TemplateBody String
```

##### Instances
``` purescript
Newtype TemplateBody _
```

#### `TestAuthorizationRequest`

``` purescript
newtype TestAuthorizationRequest
  = TestAuthorizationRequest { "Principal'" :: NullOrUndefined (Principal), "CognitoIdentityPoolId'" :: NullOrUndefined (CognitoIdentityPoolId), "AuthInfos'" :: AuthInfos, "ClientId'" :: NullOrUndefined (ClientId), "PolicyNamesToAdd'" :: NullOrUndefined (PolicyNames), "PolicyNamesToSkip'" :: NullOrUndefined (PolicyNames) }
```

##### Instances
``` purescript
Newtype TestAuthorizationRequest _
```

#### `TestAuthorizationResponse`

``` purescript
newtype TestAuthorizationResponse
  = TestAuthorizationResponse { "AuthResults'" :: NullOrUndefined (AuthResults) }
```

##### Instances
``` purescript
Newtype TestAuthorizationResponse _
```

#### `TestInvokeAuthorizerRequest`

``` purescript
newtype TestInvokeAuthorizerRequest
  = TestInvokeAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "Token'" :: Token, "TokenSignature'" :: TokenSignature }
```

##### Instances
``` purescript
Newtype TestInvokeAuthorizerRequest _
```

#### `TestInvokeAuthorizerResponse`

``` purescript
newtype TestInvokeAuthorizerResponse
  = TestInvokeAuthorizerResponse { "IsAuthenticated'" :: NullOrUndefined (IsAuthenticated), "PrincipalId'" :: NullOrUndefined (PrincipalId), "PolicyDocuments'" :: NullOrUndefined (PolicyDocuments), "RefreshAfterInSeconds'" :: NullOrUndefined (Seconds), "DisconnectAfterInSeconds'" :: NullOrUndefined (Seconds) }
```

##### Instances
``` purescript
Newtype TestInvokeAuthorizerResponse _
```

#### `ThingArn`

``` purescript
newtype ThingArn
  = ThingArn String
```

##### Instances
``` purescript
Newtype ThingArn _
```

#### `ThingAttribute`

``` purescript
newtype ThingAttribute
  = ThingAttribute { "ThingName'" :: NullOrUndefined (ThingName), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingArn'" :: NullOrUndefined (ThingArn), "Attributes'" :: NullOrUndefined (Attributes), "Version'" :: NullOrUndefined (Version) }
```

<p>The properties of the thing, including thing name, thing type name, and a list of thing attributes.</p>

##### Instances
``` purescript
Newtype ThingAttribute _
```

#### `ThingAttributeList`

``` purescript
newtype ThingAttributeList
  = ThingAttributeList (Array ThingAttribute)
```

##### Instances
``` purescript
Newtype ThingAttributeList _
```

#### `ThingDocument`

``` purescript
newtype ThingDocument
  = ThingDocument { "ThingName'" :: NullOrUndefined (ThingName), "ThingId'" :: NullOrUndefined (ThingId), "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingGroupNames'" :: NullOrUndefined (ThingGroupNameList), "Attributes'" :: NullOrUndefined (Attributes), "Shadow'" :: NullOrUndefined (JsonDocument) }
```

<p>The thing search index document.</p>

##### Instances
``` purescript
Newtype ThingDocument _
```

#### `ThingDocumentList`

``` purescript
newtype ThingDocumentList
  = ThingDocumentList (Array ThingDocument)
```

##### Instances
``` purescript
Newtype ThingDocumentList _
```

#### `ThingGroupArn`

``` purescript
newtype ThingGroupArn
  = ThingGroupArn String
```

##### Instances
``` purescript
Newtype ThingGroupArn _
```

#### `ThingGroupDescription`

``` purescript
newtype ThingGroupDescription
  = ThingGroupDescription String
```

##### Instances
``` purescript
Newtype ThingGroupDescription _
```

#### `ThingGroupId`

``` purescript
newtype ThingGroupId
  = ThingGroupId String
```

##### Instances
``` purescript
Newtype ThingGroupId _
```

#### `ThingGroupList`

``` purescript
newtype ThingGroupList
  = ThingGroupList (Array ThingGroupName)
```

##### Instances
``` purescript
Newtype ThingGroupList _
```

#### `ThingGroupMetadata`

``` purescript
newtype ThingGroupMetadata
  = ThingGroupMetadata { "ParentGroupName'" :: NullOrUndefined (ThingGroupName), "RootToParentThingGroups'" :: NullOrUndefined (ThingGroupNameAndArnList), "CreationDate'" :: NullOrUndefined (CreationDate) }
```

<p>Thing group metadata.</p>

##### Instances
``` purescript
Newtype ThingGroupMetadata _
```

#### `ThingGroupName`

``` purescript
newtype ThingGroupName
  = ThingGroupName String
```

##### Instances
``` purescript
Newtype ThingGroupName _
```

#### `ThingGroupNameAndArnList`

``` purescript
newtype ThingGroupNameAndArnList
  = ThingGroupNameAndArnList (Array GroupNameAndArn)
```

##### Instances
``` purescript
Newtype ThingGroupNameAndArnList _
```

#### `ThingGroupNameList`

``` purescript
newtype ThingGroupNameList
  = ThingGroupNameList (Array ThingGroupName)
```

##### Instances
``` purescript
Newtype ThingGroupNameList _
```

#### `ThingGroupProperties`

``` purescript
newtype ThingGroupProperties
  = ThingGroupProperties { "ThingGroupDescription'" :: NullOrUndefined (ThingGroupDescription), "AttributePayload'" :: NullOrUndefined (AttributePayload) }
```

<p>Thing group properties.</p>

##### Instances
``` purescript
Newtype ThingGroupProperties _
```

#### `ThingId`

``` purescript
newtype ThingId
  = ThingId String
```

##### Instances
``` purescript
Newtype ThingId _
```

#### `ThingIndexingConfiguration`

``` purescript
newtype ThingIndexingConfiguration
  = ThingIndexingConfiguration { "ThingIndexingMode'" :: NullOrUndefined (ThingIndexingMode) }
```

<p>Thing indexing configuration.</p>

##### Instances
``` purescript
Newtype ThingIndexingConfiguration _
```

#### `ThingIndexingMode`

``` purescript
newtype ThingIndexingMode
  = ThingIndexingMode String
```

##### Instances
``` purescript
Newtype ThingIndexingMode _
```

#### `ThingName`

``` purescript
newtype ThingName
  = ThingName String
```

##### Instances
``` purescript
Newtype ThingName _
```

#### `ThingNameList`

``` purescript
newtype ThingNameList
  = ThingNameList (Array ThingName)
```

##### Instances
``` purescript
Newtype ThingNameList _
```

#### `ThingTypeArn`

``` purescript
newtype ThingTypeArn
  = ThingTypeArn String
```

##### Instances
``` purescript
Newtype ThingTypeArn _
```

#### `ThingTypeDefinition`

``` purescript
newtype ThingTypeDefinition
  = ThingTypeDefinition { "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "ThingTypeArn'" :: NullOrUndefined (ThingTypeArn), "ThingTypeProperties'" :: NullOrUndefined (ThingTypeProperties), "ThingTypeMetadata'" :: NullOrUndefined (ThingTypeMetadata) }
```

<p>The definition of the thing type, including thing type name and description.</p>

##### Instances
``` purescript
Newtype ThingTypeDefinition _
```

#### `ThingTypeDescription`

``` purescript
newtype ThingTypeDescription
  = ThingTypeDescription String
```

##### Instances
``` purescript
Newtype ThingTypeDescription _
```

#### `ThingTypeId`

``` purescript
newtype ThingTypeId
  = ThingTypeId String
```

##### Instances
``` purescript
Newtype ThingTypeId _
```

#### `ThingTypeList`

``` purescript
newtype ThingTypeList
  = ThingTypeList (Array ThingTypeDefinition)
```

##### Instances
``` purescript
Newtype ThingTypeList _
```

#### `ThingTypeMetadata`

``` purescript
newtype ThingTypeMetadata
  = ThingTypeMetadata { "Deprecated'" :: NullOrUndefined (Boolean), "DeprecationDate'" :: NullOrUndefined (DeprecationDate), "CreationDate'" :: NullOrUndefined (CreationDate) }
```

<p>The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.</p>

##### Instances
``` purescript
Newtype ThingTypeMetadata _
```

#### `ThingTypeName`

``` purescript
newtype ThingTypeName
  = ThingTypeName String
```

##### Instances
``` purescript
Newtype ThingTypeName _
```

#### `ThingTypeProperties`

``` purescript
newtype ThingTypeProperties
  = ThingTypeProperties { "ThingTypeDescription'" :: NullOrUndefined (ThingTypeDescription), "SearchableAttributes'" :: NullOrUndefined (SearchableAttributes) }
```

<p>The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.</p>

##### Instances
``` purescript
Newtype ThingTypeProperties _
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The rate exceeds the limit.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

##### Instances
``` purescript
Newtype Token _
```

#### `TokenKeyName`

``` purescript
newtype TokenKeyName
  = TokenKeyName String
```

##### Instances
``` purescript
Newtype TokenKeyName _
```

#### `TokenSignature`

``` purescript
newtype TokenSignature
  = TokenSignature String
```

##### Instances
``` purescript
Newtype TokenSignature _
```

#### `Topic`

``` purescript
newtype Topic
  = Topic String
```

##### Instances
``` purescript
Newtype Topic _
```

#### `TopicPattern`

``` purescript
newtype TopicPattern
  = TopicPattern String
```

##### Instances
``` purescript
Newtype TopicPattern _
```

#### `TopicRule`

``` purescript
newtype TopicRule
  = TopicRule { "RuleName'" :: NullOrUndefined (RuleName), "Sql'" :: NullOrUndefined (SQL), "Description'" :: NullOrUndefined (Description), "CreatedAt'" :: NullOrUndefined (CreatedAtDate), "Actions'" :: NullOrUndefined (ActionList), "RuleDisabled'" :: NullOrUndefined (IsDisabled), "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion), "ErrorAction'" :: NullOrUndefined (Action) }
```

<p>Describes a rule.</p>

##### Instances
``` purescript
Newtype TopicRule _
```

#### `TopicRuleList`

``` purescript
newtype TopicRuleList
  = TopicRuleList (Array TopicRuleListItem)
```

##### Instances
``` purescript
Newtype TopicRuleList _
```

#### `TopicRuleListItem`

``` purescript
newtype TopicRuleListItem
  = TopicRuleListItem { "RuleArn'" :: NullOrUndefined (RuleArn), "RuleName'" :: NullOrUndefined (RuleName), "TopicPattern'" :: NullOrUndefined (TopicPattern), "CreatedAt'" :: NullOrUndefined (CreatedAtDate), "RuleDisabled'" :: NullOrUndefined (IsDisabled) }
```

<p>Describes a rule.</p>

##### Instances
``` purescript
Newtype TopicRuleListItem _
```

#### `TopicRulePayload`

``` purescript
newtype TopicRulePayload
  = TopicRulePayload { "Sql'" :: SQL, "Description'" :: NullOrUndefined (Description), "Actions'" :: ActionList, "RuleDisabled'" :: NullOrUndefined (IsDisabled), "AwsIotSqlVersion'" :: NullOrUndefined (AwsIotSqlVersion), "ErrorAction'" :: NullOrUndefined (Action) }
```

<p>Describes a rule.</p>

##### Instances
``` purescript
Newtype TopicRulePayload _
```

#### `TransferAlreadyCompletedException`

``` purescript
newtype TransferAlreadyCompletedException
  = TransferAlreadyCompletedException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't revert the certificate transfer because the transfer is already complete.</p>

##### Instances
``` purescript
Newtype TransferAlreadyCompletedException _
```

#### `TransferCertificateRequest`

``` purescript
newtype TransferCertificateRequest
  = TransferCertificateRequest { "CertificateId'" :: CertificateId, "TargetAwsAccount'" :: AwsAccountId, "TransferMessage'" :: NullOrUndefined (Message) }
```

<p>The input for the TransferCertificate operation.</p>

##### Instances
``` purescript
Newtype TransferCertificateRequest _
```

#### `TransferCertificateResponse`

``` purescript
newtype TransferCertificateResponse
  = TransferCertificateResponse { "TransferredCertificateArn'" :: NullOrUndefined (CertificateArn) }
```

<p>The output from the TransferCertificate operation.</p>

##### Instances
``` purescript
Newtype TransferCertificateResponse _
```

#### `TransferConflictException`

``` purescript
newtype TransferConflictException
  = TransferConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You can't transfer the certificate because authorization policies are still attached.</p>

##### Instances
``` purescript
Newtype TransferConflictException _
```

#### `TransferData`

``` purescript
newtype TransferData
  = TransferData { "TransferMessage'" :: NullOrUndefined (Message), "RejectReason'" :: NullOrUndefined (Message), "TransferDate'" :: NullOrUndefined (DateType), "AcceptDate'" :: NullOrUndefined (DateType), "RejectDate'" :: NullOrUndefined (DateType) }
```

<p>Data used to transfer a certificate to an AWS account.</p>

##### Instances
``` purescript
Newtype TransferData _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You are not authorized to perform this operation.</p>

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UndoDeprecate`

``` purescript
newtype UndoDeprecate
  = UndoDeprecate Boolean
```

##### Instances
``` purescript
Newtype UndoDeprecate _
```

#### `UpdateAuthorizerRequest`

``` purescript
newtype UpdateAuthorizerRequest
  = UpdateAuthorizerRequest { "AuthorizerName'" :: AuthorizerName, "AuthorizerFunctionArn'" :: NullOrUndefined (AuthorizerFunctionArn), "TokenKeyName'" :: NullOrUndefined (TokenKeyName), "TokenSigningPublicKeys'" :: NullOrUndefined (PublicKeyMap), "Status'" :: NullOrUndefined (AuthorizerStatus) }
```

##### Instances
``` purescript
Newtype UpdateAuthorizerRequest _
```

#### `UpdateAuthorizerResponse`

``` purescript
newtype UpdateAuthorizerResponse
  = UpdateAuthorizerResponse { "AuthorizerName'" :: NullOrUndefined (AuthorizerName), "AuthorizerArn'" :: NullOrUndefined (AuthorizerArn) }
```

##### Instances
``` purescript
Newtype UpdateAuthorizerResponse _
```

#### `UpdateCACertificateRequest`

``` purescript
newtype UpdateCACertificateRequest
  = UpdateCACertificateRequest { "CertificateId'" :: CertificateId, "NewStatus'" :: NullOrUndefined (CACertificateStatus), "NewAutoRegistrationStatus'" :: NullOrUndefined (AutoRegistrationStatus), "RegistrationConfig'" :: NullOrUndefined (RegistrationConfig), "RemoveAutoRegistration'" :: NullOrUndefined (RemoveAutoRegistration) }
```

<p>The input to the UpdateCACertificate operation.</p>

##### Instances
``` purescript
Newtype UpdateCACertificateRequest _
```

#### `UpdateCertificateRequest`

``` purescript
newtype UpdateCertificateRequest
  = UpdateCertificateRequest { "CertificateId'" :: CertificateId, "NewStatus'" :: CertificateStatus }
```

<p>The input for the UpdateCertificate operation.</p>

##### Instances
``` purescript
Newtype UpdateCertificateRequest _
```

#### `UpdateEventConfigurationsRequest`

``` purescript
newtype UpdateEventConfigurationsRequest
  = UpdateEventConfigurationsRequest { "EventConfigurations'" :: NullOrUndefined (EventConfigurations) }
```

##### Instances
``` purescript
Newtype UpdateEventConfigurationsRequest _
```

#### `UpdateEventConfigurationsResponse`

``` purescript
newtype UpdateEventConfigurationsResponse
  = UpdateEventConfigurationsResponse {  }
```

##### Instances
``` purescript
Newtype UpdateEventConfigurationsResponse _
```

#### `UpdateIndexingConfigurationRequest`

``` purescript
newtype UpdateIndexingConfigurationRequest
  = UpdateIndexingConfigurationRequest { "ThingIndexingConfiguration'" :: NullOrUndefined (ThingIndexingConfiguration) }
```

##### Instances
``` purescript
Newtype UpdateIndexingConfigurationRequest _
```

#### `UpdateIndexingConfigurationResponse`

``` purescript
newtype UpdateIndexingConfigurationResponse
  = UpdateIndexingConfigurationResponse {  }
```

##### Instances
``` purescript
Newtype UpdateIndexingConfigurationResponse _
```

#### `UpdateRoleAliasRequest`

``` purescript
newtype UpdateRoleAliasRequest
  = UpdateRoleAliasRequest { "RoleAlias'" :: RoleAlias, "RoleArn'" :: NullOrUndefined (RoleArn), "CredentialDurationSeconds'" :: NullOrUndefined (CredentialDurationSeconds) }
```

##### Instances
``` purescript
Newtype UpdateRoleAliasRequest _
```

#### `UpdateRoleAliasResponse`

``` purescript
newtype UpdateRoleAliasResponse
  = UpdateRoleAliasResponse { "RoleAlias'" :: NullOrUndefined (RoleAlias), "RoleAliasArn'" :: NullOrUndefined (RoleAliasArn) }
```

##### Instances
``` purescript
Newtype UpdateRoleAliasResponse _
```

#### `UpdateStreamRequest`

``` purescript
newtype UpdateStreamRequest
  = UpdateStreamRequest { "StreamId'" :: StreamId, "Description'" :: NullOrUndefined (StreamDescription), "Files'" :: NullOrUndefined (StreamFiles), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

##### Instances
``` purescript
Newtype UpdateStreamRequest _
```

#### `UpdateStreamResponse`

``` purescript
newtype UpdateStreamResponse
  = UpdateStreamResponse { "StreamId'" :: NullOrUndefined (StreamId), "StreamArn'" :: NullOrUndefined (StreamArn), "Description'" :: NullOrUndefined (StreamDescription), "StreamVersion'" :: NullOrUndefined (StreamVersion) }
```

##### Instances
``` purescript
Newtype UpdateStreamResponse _
```

#### `UpdateThingGroupRequest`

``` purescript
newtype UpdateThingGroupRequest
  = UpdateThingGroupRequest { "ThingGroupName'" :: ThingGroupName, "ThingGroupProperties'" :: ThingGroupProperties, "ExpectedVersion'" :: NullOrUndefined (OptionalVersion) }
```

##### Instances
``` purescript
Newtype UpdateThingGroupRequest _
```

#### `UpdateThingGroupResponse`

``` purescript
newtype UpdateThingGroupResponse
  = UpdateThingGroupResponse { "Version'" :: NullOrUndefined (Version) }
```

##### Instances
``` purescript
Newtype UpdateThingGroupResponse _
```

#### `UpdateThingGroupsForThingRequest`

``` purescript
newtype UpdateThingGroupsForThingRequest
  = UpdateThingGroupsForThingRequest { "ThingName'" :: NullOrUndefined (ThingName), "ThingGroupsToAdd'" :: NullOrUndefined (ThingGroupList), "ThingGroupsToRemove'" :: NullOrUndefined (ThingGroupList) }
```

##### Instances
``` purescript
Newtype UpdateThingGroupsForThingRequest _
```

#### `UpdateThingGroupsForThingResponse`

``` purescript
newtype UpdateThingGroupsForThingResponse
  = UpdateThingGroupsForThingResponse {  }
```

##### Instances
``` purescript
Newtype UpdateThingGroupsForThingResponse _
```

#### `UpdateThingRequest`

``` purescript
newtype UpdateThingRequest
  = UpdateThingRequest { "ThingName'" :: ThingName, "ThingTypeName'" :: NullOrUndefined (ThingTypeName), "AttributePayload'" :: NullOrUndefined (AttributePayload), "ExpectedVersion'" :: NullOrUndefined (OptionalVersion), "RemoveThingType'" :: NullOrUndefined (RemoveThingType) }
```

<p>The input for the UpdateThing operation.</p>

##### Instances
``` purescript
Newtype UpdateThingRequest _
```

#### `UpdateThingResponse`

``` purescript
newtype UpdateThingResponse
  = UpdateThingResponse {  }
```

<p>The output from the UpdateThing operation.</p>

##### Instances
``` purescript
Newtype UpdateThingResponse _
```

#### `UseBase64`

``` purescript
newtype UseBase64
  = UseBase64 Boolean
```

##### Instances
``` purescript
Newtype UseBase64 _
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

##### Instances
``` purescript
Newtype Value _
```

#### `Version`

``` purescript
newtype Version
  = Version Number
```

##### Instances
``` purescript
Newtype Version _
```

#### `VersionConflictException`

``` purescript
newtype VersionConflictException
  = VersionConflictException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.</p>

##### Instances
``` purescript
Newtype VersionConflictException _
```

#### `VersionsLimitExceededException`

``` purescript
newtype VersionsLimitExceededException
  = VersionsLimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The number of policy versions exceeds the limit.</p>

##### Instances
``` purescript
Newtype VersionsLimitExceededException _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

##### Instances
``` purescript
Newtype ErrorMessage' _
```

#### `ResourceArn'`

``` purescript
newtype ResourceArn'
  = ResourceArn' String
```

##### Instances
``` purescript
Newtype ResourceArn' _
```

#### `ResourceId'`

``` purescript
newtype ResourceId'
  = ResourceId' String
```

##### Instances
``` purescript
Newtype ResourceId' _
```


