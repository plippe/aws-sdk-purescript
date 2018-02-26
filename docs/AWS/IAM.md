## Module AWS.IAM

<fullname>AWS Identity and Access Management</fullname> <p>AWS Identity and Access Management (IAM) is a web service that you can use to manage users and user permissions under your AWS account. This guide provides descriptions of IAM actions that you can call programmatically. For general information about IAM, see <a href="http://aws.amazon.com/iam/">AWS Identity and Access Management (IAM)</a>. For the user guide for IAM, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/">Using IAM</a>. </p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to IAM and AWS. For example, the SDKs take care of tasks such as cryptographically signing requests (see below), managing errors, and retrying requests automatically. For information about the AWS SDKs, including how to download and install them, see the <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a> page. </p> </note> <p>We recommend that you use the AWS SDKs to make programmatic API calls to IAM. However, you can also use the IAM Query API to make direct calls to the IAM web service. To learn more about the IAM Query API, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>Using IAM</i> guide. IAM supports GET and POST requests for all actions. That is, the API does not require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request. </p> <p> <b>Signing Requests</b> </p> <p>Requests must be signed using an access key ID and a secret access key. We strongly recommend that you do not use your AWS account access key ID and secret access key for everyday work with IAM. You can use the access key ID and secret access key for an IAM user or you can use the AWS Security Token Service to generate temporary security credentials and use those to sign requests.</p> <p>To sign requests, we recommend that you use <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>. If you have an existing application that uses Signature Version 2, you do not have to update it to use Signature Version 4. However, some operations now require Signature Version 4. The documentation for operations that require version 4 indicate this requirement. </p> <p> <b>Additional Resources</b> </p> <p>For more information, see the following:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html">AWS Security Credentials</a>. This topic provides general information about the types of credentials used for accessing AWS. </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html">IAM Best Practices</a>. This topic presents a list of suggestions for using the IAM service to help secure your AWS resources. </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a>. This set of topics walk you through the process of signing a request using an access key ID and secret access key. </p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addClientIDToOpenIDConnectProvider`

``` purescript
addClientIDToOpenIDConnectProvider :: forall eff. AddClientIDToOpenIDConnectProviderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds a new client ID (also known as audience) to the list of client IDs already registered for the specified IAM OpenID Connect (OIDC) provider resource.</p> <p>This action is idempotent; it does not fail or return an error if you add an existing client ID to the provider.</p>

#### `addRoleToInstanceProfile`

``` purescript
addRoleToInstanceProfile :: forall eff. AddRoleToInstanceProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds the specified IAM role to the specified instance profile. An instance profile can contain only one role, and this limit cannot be increased.</p> <note> <p>The caller of this API must be granted the <code>PassRole</code> permission on the IAM role by a permission policy.</p> </note> <p>For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>

#### `addUserToGroup`

``` purescript
addUserToGroup :: forall eff. AddUserToGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds the specified user to the specified group.</p>

#### `attachGroupPolicy`

``` purescript
attachGroupPolicy :: forall eff. AttachGroupPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches the specified managed policy to the specified IAM group.</p> <p>You use this API to attach a managed policy to a group. To embed an inline policy in a group, use <a>PutGroupPolicy</a>.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `attachRolePolicy`

``` purescript
attachRolePolicy :: forall eff. AttachRolePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches the specified managed policy to the specified IAM role. When you attach a managed policy to a role, the managed policy becomes part of the role's permission (access) policy.</p> <note> <p>You cannot use a managed policy as the role's trust policy. The role's trust policy is created at the same time as the role, using <a>CreateRole</a>. You can update a role's trust policy using <a>UpdateAssumeRolePolicy</a>.</p> </note> <p>Use this API to attach a <i>managed</i> policy to a role. To embed an inline policy in a role, use <a>PutRolePolicy</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `attachUserPolicy`

``` purescript
attachUserPolicy :: forall eff. AttachUserPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches the specified managed policy to the specified user.</p> <p>You use this API to attach a <i>managed</i> policy to a user. To embed an inline policy in a user, use <a>PutUserPolicy</a>.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `changePassword`

``` purescript
changePassword :: forall eff. ChangePasswordRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the password of the IAM user who is calling this action. The root account password is not affected by this action.</p> <p>To change the password for a different user, see <a>UpdateLoginProfile</a>. For more information about modifying passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>

#### `createAccessKey`

``` purescript
createAccessKey :: forall eff. CreateAccessKeyRequest -> Aff (err :: RequestError | eff) CreateAccessKeyResponse
```

<p> Creates a new AWS secret access key and corresponding AWS access key ID for the specified user. The default status for new keys is <code>Active</code>.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <p> For information about limits on the number of keys you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <important> <p>To ensure the security of your AWS account, the secret access key is accessible only during key and user creation. You must save the key (for example, in a text file) if you want to be able to access it again. If a secret key is lost, you can delete the access keys for the associated user and then create new keys.</p> </important>

#### `createAccountAlias`

``` purescript
createAccountAlias :: forall eff. CreateAccountAliasRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates an alias for your AWS account. For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>

#### `createGroup`

``` purescript
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: RequestError | eff) CreateGroupResponse
```

<p>Creates a new group.</p> <p> For information about the number of groups you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>

#### `createInstanceProfile`

``` purescript
createInstanceProfile :: forall eff. CreateInstanceProfileRequest -> Aff (err :: RequestError | eff) CreateInstanceProfileResponse
```

<p> Creates a new instance profile. For information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p> For information about the number of instance profiles you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>

#### `createLoginProfile`

``` purescript
createLoginProfile :: forall eff. CreateLoginProfileRequest -> Aff (err :: RequestError | eff) CreateLoginProfileResponse
```

<p> Creates a password for the specified user, giving the user the ability to access AWS services through the AWS Management Console. For more information about managing passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>

#### `createOpenIDConnectProvider`

``` purescript
createOpenIDConnectProvider :: forall eff. CreateOpenIDConnectProviderRequest -> Aff (err :: RequestError | eff) CreateOpenIDConnectProviderResponse
```

<p>Creates an IAM entity to describe an identity provider (IdP) that supports <a href="http://openid.net/connect/">OpenID Connect (OIDC)</a>.</p> <p>The OIDC provider that you create with this operation can be used as a principal in a role's trust policy to establish a trust relationship between AWS and the OIDC provider.</p> <p>When you create the IAM OIDC provider, you specify the URL of the OIDC identity provider (IdP) to trust, a list of client IDs (also known as audiences) that identify the application or applications that are allowed to authenticate using the OIDC provider, and a list of thumbprints of the server certificate(s) that the IdP uses. You get all of this information from the OIDC IdP that you want to use for access to AWS.</p> <note> <p>Because trust for the OIDC provider is ultimately derived from the IAM provider that this action creates, it is a best practice to limit access to the <a>CreateOpenIDConnectProvider</a> action to highly-privileged users.</p> </note>

#### `createPolicy`

``` purescript
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: RequestError | eff) CreatePolicyResponse
```

<p>Creates a new managed policy for your AWS account.</p> <p>This operation creates a policy version with a version identifier of <code>v1</code> and sets v1 as the policy's default version. For more information about policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about managed policies in general, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `createPolicyVersion`

``` purescript
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (err :: RequestError | eff) CreatePolicyVersionResponse
```

<p>Creates a new version of the specified managed policy. To update a managed policy, you create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must delete an existing version using <a>DeletePolicyVersion</a> before you create a new version.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the version that is in effect for the IAM users, groups, and roles to which the policy is attached.</p> <p>For more information about managed policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>

#### `createRole`

``` purescript
createRole :: forall eff. CreateRoleRequest -> Aff (err :: RequestError | eff) CreateRoleResponse
```

<p>Creates a new role for your AWS account. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For information about limitations on role names and the number of roles you can create, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>

#### `createSAMLProvider`

``` purescript
createSAMLProvider :: forall eff. CreateSAMLProviderRequest -> Aff (err :: RequestError | eff) CreateSAMLProviderResponse
```

<p>Creates an IAM resource that describes an identity provider (IdP) that supports SAML 2.0.</p> <p>The SAML provider resource that you create with this operation can be used as a principal in an IAM role's trust policy to enable federated users who sign-in using the SAML IdP to assume the role. You can create an IAM role that supports Web-based single sign-on (SSO) to the AWS Management Console or one that supports API access to AWS.</p> <p>When you create the SAML provider resource, you upload an a SAML metadata document that you get from your IdP and that includes the issuer's name, expiration information, and keys that can be used to validate the SAML authentication response (assertions) that the IdP sends. You must generate the metadata document using the identity management software that is used as your organization's IdP.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note> <p> For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-saml.html">Enabling SAML 2.0 Federated Users to Access the AWS Management Console</a> and <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html">About SAML 2.0-based Federation</a> in the <i>IAM User Guide</i>.</p>

#### `createServiceLinkedRole`

``` purescript
createServiceLinkedRole :: forall eff. CreateServiceLinkedRoleRequest -> Aff (err :: RequestError | eff) CreateServiceLinkedRoleResponse
```

<p>Creates an IAM role that is linked to a specific AWS service. The service controls the attached policies and when the role can be deleted. This helps ensure that the service is not broken by an unexpectedly changed or deleted role, which could put your AWS resources into an unknown state. Allowing the service to control the role helps improve service stability and proper cleanup when a service and its role are no longer needed.</p> <p>The name of the role is autogenerated by combining the string that you specify for the <code>AWSServiceName</code> parameter with the string that you specify for the <code>CustomSuffix</code> parameter. The resulting name must be unique in your account or the request fails.</p> <p>To attach a policy to this service-linked role, you must make the request using the AWS service that depends on this role.</p>

#### `createServiceSpecificCredential`

``` purescript
createServiceSpecificCredential :: forall eff. CreateServiceSpecificCredentialRequest -> Aff (err :: RequestError | eff) CreateServiceSpecificCredentialResponse
```

<p>Generates a set of credentials consisting of a user name and password that can be used to access the service specified in the request. These credentials are generated by IAM, and can be used only for the specified service. </p> <p>You can have a maximum of two sets of service-specific credentials for each supported service per user.</p> <p>The only supported service at this time is AWS CodeCommit.</p> <p>You can reset the password to a new service-generated value by calling <a>ResetServiceSpecificCredential</a>.</p> <p>For more information about service-specific credentials, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_ssh-keys.html">Using IAM with AWS CodeCommit: Git Credentials, SSH Keys, and AWS Access Keys</a> in the <i>IAM User Guide</i>.</p>

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

<p>Creates a new IAM user for your AWS account.</p> <p> For information about limitations on the number of IAM users you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>

#### `createVirtualMFADevice`

``` purescript
createVirtualMFADevice :: forall eff. CreateVirtualMFADeviceRequest -> Aff (err :: RequestError | eff) CreateVirtualMFADeviceResponse
```

<p>Creates a new virtual MFA device for the AWS account. After creating the virtual MFA, use <a>EnableMFADevice</a> to attach the MFA device to an IAM user. For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of MFA devices you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on Entities</a> in the <i>IAM User Guide</i>.</p> <important> <p>The seed information contained in the QR code and the Base32 string should be treated like any other secret access information, such as your AWS access keys or your passwords. After you provision your virtual device, you should ensure that the information is destroyed following secure procedures.</p> </important>

#### `deactivateMFADevice`

``` purescript
deactivateMFADevice :: forall eff. DeactivateMFADeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deactivates the specified MFA device and removes it from association with the user name for which it was originally enabled.</p> <p>For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p>

#### `deleteAccessKey`

``` purescript
deleteAccessKey :: forall eff. DeleteAccessKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the access key pair associated with the specified IAM user.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>

#### `deleteAccountAlias`

``` purescript
deleteAccountAlias :: forall eff. DeleteAccountAliasRequest -> Aff (err :: RequestError | eff) Unit
```

<p> Deletes the specified AWS account alias. For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>

#### `deleteAccountPasswordPolicy`

``` purescript
deleteAccountPasswordPolicy :: forall eff. Aff (err :: RequestError | eff) Unit
```

<p>Deletes the password policy for the AWS account. There are no parameters.</p>

#### `deleteGroup`

``` purescript
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified IAM group. The group must not contain any users or have any attached policies.</p>

#### `deleteGroupPolicy`

``` purescript
deleteGroupPolicy :: forall eff. DeleteGroupPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified inline policy that is embedded in the specified IAM group.</p> <p>A group can also have managed policies attached to it. To detach a managed policy from a group, use <a>DetachGroupPolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `deleteInstanceProfile`

``` purescript
deleteInstanceProfile :: forall eff. DeleteInstanceProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified instance profile. The instance profile must not have an associated role.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the instance profile you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.</p> </important> <p>For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>

#### `deleteLoginProfile`

``` purescript
deleteLoginProfile :: forall eff. DeleteLoginProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the password for the specified IAM user, which terminates the user's ability to access AWS services through the AWS Management Console.</p> <important> <p> Deleting a user's password does not prevent a user from accessing AWS through the command line interface or the API. To prevent all user access you must also either make any access keys inactive or delete them. For more information about making keys inactive or deleting them, see <a>UpdateAccessKey</a> and <a>DeleteAccessKey</a>. </p> </important>

#### `deleteOpenIDConnectProvider`

``` purescript
deleteOpenIDConnectProvider :: forall eff. DeleteOpenIDConnectProviderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an OpenID Connect identity provider (IdP) resource object in IAM.</p> <p>Deleting an IAM OIDC provider resource does not update any roles that reference the provider as a principal in their trust policies. Any attempt to assume a role that references a deleted provider fails.</p> <p>This action is idempotent; it does not fail or return an error if you call the action for a provider that does not exist.</p>

#### `deletePolicy`

``` purescript
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified managed policy.</p> <p>Before you can delete a managed policy, you must first detach the policy from all users, groups, and roles that it is attached to, and you must delete all of the policy's versions. The following steps describe the process for deleting a managed policy:</p> <ul> <li> <p>Detach the policy from all users, groups, and roles that the policy is attached to, using the <a>DetachUserPolicy</a>, <a>DetachGroupPolicy</a>, or <a>DetachRolePolicy</a> APIs. To list all the users, groups, and roles that a policy is attached to, use <a>ListEntitiesForPolicy</a>.</p> </li> <li> <p>Delete all versions of the policy using <a>DeletePolicyVersion</a>. To list the policy's versions, use <a>ListPolicyVersions</a>. You cannot use <a>DeletePolicyVersion</a> to delete the version that is marked as the default version. You delete the policy's default version in the next step of the process.</p> </li> <li> <p>Delete the policy (this automatically deletes the policy's default version) using this API.</p> </li> </ul> <p>For information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `deletePolicyVersion`

``` purescript
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified version from the specified managed policy.</p> <p>You cannot delete the default version from a policy using this API. To delete the default version from a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use <a>ListPolicyVersions</a>.</p> <p>For information about versions for managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>

#### `deleteRole`

``` purescript
deleteRole :: forall eff. DeleteRoleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified role. The role must not have any policies attached. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the role you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.</p> </important>

#### `deleteRolePolicy`

``` purescript
deleteRolePolicy :: forall eff. DeleteRolePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified inline policy that is embedded in the specified IAM role.</p> <p>A role can also have managed policies attached to it. To detach a managed policy from a role, use <a>DetachRolePolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `deleteSAMLProvider`

``` purescript
deleteSAMLProvider :: forall eff. DeleteSAMLProviderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a SAML provider resource in IAM.</p> <p>Deleting the provider resource from IAM does not update any roles that reference the SAML provider resource's ARN as a principal in their trust policies. Any attempt to assume a role that references a non-existent provider resource ARN fails.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>

#### `deleteSSHPublicKey`

``` purescript
deleteSSHPublicKey :: forall eff. DeleteSSHPublicKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified SSH public key.</p> <p>The SSH public key deleted by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>

#### `deleteServerCertificate`

``` purescript
deleteServerCertificate :: forall eff. DeleteServerCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified server certificate.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <important> <p> If you are using a server certificate with Elastic Load Balancing, deleting the certificate could have implications for your application. If Elastic Load Balancing doesn't detect the deletion of bound certificates, it may continue to use the certificates. This could cause Elastic Load Balancing to stop accepting traffic. We recommend that you remove the reference to the certificate from Elastic Load Balancing before using this command to delete the certificate. For more information, go to <a href="http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html">DeleteLoadBalancerListeners</a> in the <i>Elastic Load Balancing API Reference</i>.</p> </important>

#### `deleteServiceLinkedRole`

``` purescript
deleteServiceLinkedRole :: forall eff. DeleteServiceLinkedRoleRequest -> Aff (err :: RequestError | eff) DeleteServiceLinkedRoleResponse
```

<p>Submits a service-linked role deletion request and returns a <code>DeletionTaskId</code>, which you can use to check the status of the deletion. Before you call this operation, confirm that the role has no active sessions and that any resources used by the role in the linked service are deleted. If you call this operation more than once for the same service-linked role and an earlier deletion task is not complete, then the <code>DeletionTaskId</code> of the earlier request is returned.</p> <p>If you submit a deletion request for a service-linked role whose linked service is still accessing a resource, then the deletion task fails. If it fails, the <a>GetServiceLinkedRoleDeletionStatus</a> API operation returns the reason for the failure, including the resources that must be deleted. To delete the service-linked role, you must first remove those resources from the linked service and then submit the deletion request again. Resources are specific to the service that is linked to the role. For more information about removing resources from a service, see the <a href="http://docs.aws.amazon.com/">AWS documentation</a> for your service.</p> <p>For more information about service-linked roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role">Roles Terms and Concepts: AWS Service-Linked Role</a> in the <i>IAM User Guide</i>.</p>

#### `deleteServiceSpecificCredential`

``` purescript
deleteServiceSpecificCredential :: forall eff. DeleteServiceSpecificCredentialRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified service-specific credential.</p>

#### `deleteSigningCertificate`

``` purescript
deleteSigningCertificate :: forall eff. DeleteSigningCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a signing certificate associated with the specified IAM user.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated IAM users.</p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified IAM user. The user must not belong to any groups or have any access keys, signing certificates, or attached policies.</p>

#### `deleteUserPolicy`

``` purescript
deleteUserPolicy :: forall eff. DeleteUserPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified inline policy that is embedded in the specified IAM user.</p> <p>A user can also have managed policies attached to it. To detach a managed policy from a user, use <a>DetachUserPolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `deleteVirtualMFADevice`

``` purescript
deleteVirtualMFADevice :: forall eff. DeleteVirtualMFADeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a virtual MFA device.</p> <note> <p> You must deactivate a user's virtual MFA device before you can delete it. For information about deactivating MFA devices, see <a>DeactivateMFADevice</a>. </p> </note>

#### `detachGroupPolicy`

``` purescript
detachGroupPolicy :: forall eff. DetachGroupPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified managed policy from the specified IAM group.</p> <p>A group can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteGroupPolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `detachRolePolicy`

``` purescript
detachRolePolicy :: forall eff. DetachRolePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified managed policy from the specified role.</p> <p>A role can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteRolePolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `detachUserPolicy`

``` purescript
detachUserPolicy :: forall eff. DetachUserPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified managed policy from the specified user.</p> <p>A user can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteUserPolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `enableMFADevice`

``` purescript
enableMFADevice :: forall eff. EnableMFADeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables the specified MFA device and associates it with the specified IAM user. When enabled, the MFA device is required for every subsequent login by the IAM user associated with the device.</p>

#### `generateCredentialReport`

``` purescript
generateCredentialReport :: forall eff. Aff (err :: RequestError | eff) GenerateCredentialReportResponse
```

<p> Generates a credential report for the AWS account. For more information about the credential report, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>

#### `getAccessKeyLastUsed`

``` purescript
getAccessKeyLastUsed :: forall eff. GetAccessKeyLastUsedRequest -> Aff (err :: RequestError | eff) GetAccessKeyLastUsedResponse
```

<p>Retrieves information about when the specified access key was last used. The information includes the date and time of last use, along with the AWS service and region that were specified in the last request made with that key.</p>

#### `getAccountAuthorizationDetails`

``` purescript
getAccountAuthorizationDetails :: forall eff. GetAccountAuthorizationDetailsRequest -> Aff (err :: RequestError | eff) GetAccountAuthorizationDetailsResponse
```

<p>Retrieves information about all IAM users, groups, roles, and policies in your AWS account, including their relationships to one another. Use this API to obtain a snapshot of the configuration of IAM permissions (users, groups, roles, and policies) in your account.</p> <p>You can optionally filter the results using the <code>Filter</code> parameter. You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `getAccountPasswordPolicy`

``` purescript
getAccountPasswordPolicy :: forall eff. Aff (err :: RequestError | eff) GetAccountPasswordPolicyResponse
```

<p>Retrieves the password policy for the AWS account. For more information about using a password policy, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html">Managing an IAM Password Policy</a>.</p>

#### `getAccountSummary`

``` purescript
getAccountSummary :: forall eff. Aff (err :: RequestError | eff) GetAccountSummaryResponse
```

<p>Retrieves information about IAM entity usage and IAM quotas in the AWS account.</p> <p> For information about limitations on IAM entities, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>

#### `getContextKeysForCustomPolicy`

``` purescript
getContextKeysForCustomPolicy :: forall eff. GetContextKeysForCustomPolicyRequest -> Aff (err :: RequestError | eff) GetContextKeysForPolicyResponse
```

<p>Gets a list of all of the context keys referenced in the input policies. The policies are supplied as a list of one or more strings. To get the context keys from policies associated with an IAM user, group, or role, use <a>GetContextKeysForPrincipalPolicy</a>.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request, and can be evaluated by testing against a value specified in an IAM policy. Use GetContextKeysForCustomPolicy to understand what key names and values you must supply when you call <a>SimulateCustomPolicy</a>. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.</p>

#### `getContextKeysForPrincipalPolicy`

``` purescript
getContextKeysForPrincipalPolicy :: forall eff. GetContextKeysForPrincipalPolicyRequest -> Aff (err :: RequestError | eff) GetContextKeysForPolicyResponse
```

<p>Gets a list of all of the context keys referenced in all of the IAM policies attached to the specified IAM entity. The entity can be an IAM user, group, or role. If you specify a user, then the request also includes all of the policies attached to groups that the user is a member of.</p> <p>You can optionally include a list of one or more additional policies, specified as strings. If you want to include <i>only</i> a list of policies by string, use <a>GetContextKeysForCustomPolicy</a> instead.</p> <p> <b>Note:</b> This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use <a>GetContextKeysForCustomPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request, and can be evaluated by testing against a value in an IAM policy. Use <a>GetContextKeysForPrincipalPolicy</a> to understand what key names and values you must supply when you call <a>SimulatePrincipalPolicy</a>.</p>

#### `getCredentialReport`

``` purescript
getCredentialReport :: forall eff. Aff (err :: RequestError | eff) GetCredentialReportResponse
```

<p> Retrieves a credential report for the AWS account. For more information about the credential report, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>

#### `getGroup`

``` purescript
getGroup :: forall eff. GetGroupRequest -> Aff (err :: RequestError | eff) GetGroupResponse
```

<p> Returns a list of IAM users that are in the specified IAM group. You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `getGroupPolicy`

``` purescript
getGroupPolicy :: forall eff. GetGroupPolicyRequest -> Aff (err :: RequestError | eff) GetGroupPolicyResponse
```

<p>Retrieves the specified inline policy document that is embedded in the specified IAM group.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM group can also have managed policies attached to it. To retrieve a managed policy document that is attached to a group, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `getInstanceProfile`

``` purescript
getInstanceProfile :: forall eff. GetInstanceProfileRequest -> Aff (err :: RequestError | eff) GetInstanceProfileResponse
```

<p> Retrieves information about the specified instance profile, including the instance profile's path, GUID, ARN, and role. For more information about instance profiles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a> in the <i>IAM User Guide</i>.</p>

#### `getLoginProfile`

``` purescript
getLoginProfile :: forall eff. GetLoginProfileRequest -> Aff (err :: RequestError | eff) GetLoginProfileResponse
```

<p>Retrieves the user name and password-creation date for the specified IAM user. If the user has not been assigned a password, the action returns a 404 (<code>NoSuchEntity</code>) error.</p>

#### `getOpenIDConnectProvider`

``` purescript
getOpenIDConnectProvider :: forall eff. GetOpenIDConnectProviderRequest -> Aff (err :: RequestError | eff) GetOpenIDConnectProviderResponse
```

<p>Returns information about the specified OpenID Connect (OIDC) provider resource object in IAM.</p>

#### `getPolicy`

``` purescript
getPolicy :: forall eff. GetPolicyRequest -> Aff (err :: RequestError | eff) GetPolicyResponse
```

<p>Retrieves information about the specified managed policy, including the policy's default version and the total number of IAM users, groups, and roles to which the policy is attached. To retrieve the list of the specific users, groups, and roles that the policy is attached to, use the <a>ListEntitiesForPolicy</a> API. This API returns metadata about the policy. To retrieve the actual policy document for a specific version of the policy, use <a>GetPolicyVersion</a>.</p> <p>This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded with an IAM user, group, or role, use the <a>GetUserPolicy</a>, <a>GetGroupPolicy</a>, or <a>GetRolePolicy</a> API.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `getPolicyVersion`

``` purescript
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (err :: RequestError | eff) GetPolicyVersionResponse
```

<p>Retrieves information about the specified version of the specified managed policy, including the policy document.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>To list the available versions for a policy, use <a>ListPolicyVersions</a>.</p> <p>This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded in a user, group, or role, use the <a>GetUserPolicy</a>, <a>GetGroupPolicy</a>, or <a>GetRolePolicy</a> API.</p> <p>For more information about the types of policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about managed policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>

#### `getRole`

``` purescript
getRole :: forall eff. GetRoleRequest -> Aff (err :: RequestError | eff) GetRoleResponse
```

<p>Retrieves information about the specified role, including the role's path, GUID, ARN, and the role's trust policy that grants permission to assume the role. For more information about roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note>

#### `getRolePolicy`

``` purescript
getRolePolicy :: forall eff. GetRolePolicyRequest -> Aff (err :: RequestError | eff) GetRolePolicyResponse
```

<p>Retrieves the specified inline policy document that is embedded with the specified IAM role.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM role can also have managed policies attached to it. To retrieve a managed policy document that is attached to a role, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p>

#### `getSAMLProvider`

``` purescript
getSAMLProvider :: forall eff. GetSAMLProviderRequest -> Aff (err :: RequestError | eff) GetSAMLProviderResponse
```

<p>Returns the SAML provider metadocument that was uploaded when the IAM SAML provider resource object was created or updated.</p> <note> <p>This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>

#### `getSSHPublicKey`

``` purescript
getSSHPublicKey :: forall eff. GetSSHPublicKeyRequest -> Aff (err :: RequestError | eff) GetSSHPublicKeyResponse
```

<p>Retrieves the specified SSH public key, including metadata about the key.</p> <p>The SSH public key retrieved by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>

#### `getServerCertificate`

``` purescript
getServerCertificate :: forall eff. GetServerCertificateRequest -> Aff (err :: RequestError | eff) GetServerCertificateResponse
```

<p>Retrieves information about the specified server certificate stored in IAM.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p>

#### `getServiceLinkedRoleDeletionStatus`

``` purescript
getServiceLinkedRoleDeletionStatus :: forall eff. GetServiceLinkedRoleDeletionStatusRequest -> Aff (err :: RequestError | eff) GetServiceLinkedRoleDeletionStatusResponse
```

<p>Retrieves the status of your service-linked role deletion. After you use the <a>DeleteServiceLinkedRole</a> API operation to submit a service-linked role for deletion, you can use the <code>DeletionTaskId</code> parameter in <code>GetServiceLinkedRoleDeletionStatus</code> to check the status of the deletion. If the deletion fails, this operation returns the reason that it failed.</p>

#### `getUser`

``` purescript
getUser :: forall eff. GetUserRequest -> Aff (err :: RequestError | eff) GetUserResponse
```

<p>Retrieves information about the specified IAM user, including the user's creation date, path, unique ID, and ARN.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID used to sign the request to this API.</p>

#### `getUserPolicy`

``` purescript
getUserPolicy :: forall eff. GetUserPolicyRequest -> Aff (err :: RequestError | eff) GetUserPolicyResponse
```

<p>Retrieves the specified inline policy document that is embedded in the specified IAM user.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM user can also have managed policies attached to it. To retrieve a managed policy document that is attached to a user, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `listAccessKeys`

``` purescript
listAccessKeys :: forall eff. ListAccessKeysRequest -> Aff (err :: RequestError | eff) ListAccessKeysResponse
```

<p>Returns information about the access key IDs associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>Although each user is limited to a small number of keys, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <note> <p>To ensure the security of your AWS account, the secret access key is accessible only during key and user creation.</p> </note>

#### `listAccountAliases`

``` purescript
listAccountAliases :: forall eff. ListAccountAliasesRequest -> Aff (err :: RequestError | eff) ListAccountAliasesResponse
```

<p>Lists the account alias associated with the AWS account (Note: you can have only one). For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>

#### `listAttachedGroupPolicies`

``` purescript
listAttachedGroupPolicies :: forall eff. ListAttachedGroupPoliciesRequest -> Aff (err :: RequestError | eff) ListAttachedGroupPoliciesResponse
```

<p>Lists all managed policies that are attached to the specified IAM group.</p> <p>An IAM group can also have inline policies embedded with it. To list the inline policies for a group, use the <a>ListGroupPolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the action returns an empty list.</p>

#### `listAttachedRolePolicies`

``` purescript
listAttachedRolePolicies :: forall eff. ListAttachedRolePoliciesRequest -> Aff (err :: RequestError | eff) ListAttachedRolePoliciesResponse
```

<p>Lists all managed policies that are attached to the specified IAM role.</p> <p>An IAM role can also have inline policies embedded with it. To list the inline policies for a role, use the <a>ListRolePolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified role (or none that match the specified path prefix), the action returns an empty list.</p>

#### `listAttachedUserPolicies`

``` purescript
listAttachedUserPolicies :: forall eff. ListAttachedUserPoliciesRequest -> Aff (err :: RequestError | eff) ListAttachedUserPoliciesResponse
```

<p>Lists all managed policies that are attached to the specified IAM user.</p> <p>An IAM user can also have inline policies embedded with it. To list the inline policies for a user, use the <a>ListUserPolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the action returns an empty list.</p>

#### `listEntitiesForPolicy`

``` purescript
listEntitiesForPolicy :: forall eff. ListEntitiesForPolicyRequest -> Aff (err :: RequestError | eff) ListEntitiesForPolicyResponse
```

<p>Lists all IAM users, groups, and roles that the specified managed policy is attached to.</p> <p>You can use the optional <code>EntityFilter</code> parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set <code>EntityFilter</code> to <code>Role</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listGroupPolicies`

``` purescript
listGroupPolicies :: forall eff. ListGroupPoliciesRequest -> Aff (err :: RequestError | eff) ListGroupPoliciesResponse
```

<p>Lists the names of the inline policies that are embedded in the specified IAM group.</p> <p>An IAM group can also have managed policies attached to it. To list the managed policies that are attached to a group, use <a>ListAttachedGroupPolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified group, the action returns an empty list.</p>

#### `listGroups`

``` purescript
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: RequestError | eff) ListGroupsResponse
```

<p>Lists the IAM groups that have the specified path prefix.</p> <p> You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listGroupsForUser`

``` purescript
listGroupsForUser :: forall eff. ListGroupsForUserRequest -> Aff (err :: RequestError | eff) ListGroupsForUserResponse
```

<p>Lists the IAM groups that the specified IAM user belongs to.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listInstanceProfiles`

``` purescript
listInstanceProfiles :: forall eff. ListInstanceProfilesRequest -> Aff (err :: RequestError | eff) ListInstanceProfilesResponse
```

<p>Lists the instance profiles that have the specified path prefix. If there are none, the action returns an empty list. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listInstanceProfilesForRole`

``` purescript
listInstanceProfilesForRole :: forall eff. ListInstanceProfilesForRoleRequest -> Aff (err :: RequestError | eff) ListInstanceProfilesForRoleResponse
```

<p>Lists the instance profiles that have the specified associated IAM role. If there are none, the action returns an empty list. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listMFADevices`

``` purescript
listMFADevices :: forall eff. ListMFADevicesRequest -> Aff (err :: RequestError | eff) ListMFADevicesResponse
```

<p>Lists the MFA devices for an IAM user. If the request includes a IAM user name, then this action lists all the MFA devices associated with the specified user. If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request for this API.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listOpenIDConnectProviders`

``` purescript
listOpenIDConnectProviders :: forall eff. ListOpenIDConnectProvidersRequest -> Aff (err :: RequestError | eff) ListOpenIDConnectProvidersResponse
```

<p>Lists information about the IAM OpenID Connect (OIDC) provider resource objects defined in the AWS account.</p>

#### `listPolicies`

``` purescript
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: RequestError | eff) ListPoliciesResponse
```

<p>Lists all the managed policies that are available in your AWS account, including your own customer-defined managed policies and all AWS managed policies.</p> <p>You can filter the list of policies that is returned using the optional <code>OnlyAttached</code>, <code>Scope</code>, and <code>PathPrefix</code> parameters. For example, to list only the customer managed policies in your AWS account, set <code>Scope</code> to <code>Local</code>. To list only AWS managed policies, set <code>Scope</code> to <code>AWS</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `listPolicyVersions`

``` purescript
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (err :: RequestError | eff) ListPolicyVersionsResponse
```

<p>Lists information about the versions of the specified managed policy, including the version that is currently set as the policy's default version.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `listRolePolicies`

``` purescript
listRolePolicies :: forall eff. ListRolePoliciesRequest -> Aff (err :: RequestError | eff) ListRolePoliciesResponse
```

<p>Lists the names of the inline policies that are embedded in the specified IAM role.</p> <p>An IAM role can also have managed policies attached to it. To list the managed policies that are attached to a role, use <a>ListAttachedRolePolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified role, the action returns an empty list.</p>

#### `listRoles`

``` purescript
listRoles :: forall eff. ListRolesRequest -> Aff (err :: RequestError | eff) ListRolesResponse
```

<p>Lists the IAM roles that have the specified path prefix. If there are none, the action returns an empty list. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listSAMLProviders`

``` purescript
listSAMLProviders :: forall eff. ListSAMLProvidersRequest -> Aff (err :: RequestError | eff) ListSAMLProvidersResponse
```

<p>Lists the SAML provider resource objects defined in IAM in the account.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>

#### `listSSHPublicKeys`

``` purescript
listSSHPublicKeys :: forall eff. ListSSHPublicKeysRequest -> Aff (err :: RequestError | eff) ListSSHPublicKeysResponse
```

<p>Returns information about the SSH public keys associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>The SSH public keys returned by this action are used only for authenticating the IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p> <p>Although each user is limited to a small number of keys, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listServerCertificates`

``` purescript
listServerCertificates :: forall eff. ListServerCertificatesRequest -> Aff (err :: RequestError | eff) ListServerCertificatesResponse
```

<p>Lists the server certificates stored in IAM that have the specified path prefix. If none exist, the action returns an empty list.</p> <p> You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p>

#### `listServiceSpecificCredentials`

``` purescript
listServiceSpecificCredentials :: forall eff. ListServiceSpecificCredentialsRequest -> Aff (err :: RequestError | eff) ListServiceSpecificCredentialsResponse
```

<p>Returns information about the service-specific credentials associated with the specified IAM user. If there are none, the action returns an empty list. The service-specific credentials returned by this action are used only for authenticating the IAM user to a specific service. For more information about using service-specific credentials to authenticate to an AWS service, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html">Set Up service-specific credentials</a> in the AWS CodeCommit User Guide.</p>

#### `listSigningCertificates`

``` purescript
listSigningCertificates :: forall eff. ListSigningCertificatesRequest -> Aff (err :: RequestError | eff) ListSigningCertificatesResponse
```

<p>Returns information about the signing certificates associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>Although each user is limited to a small number of signing certificates, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>If the <code>UserName</code> field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request for this API. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>

#### `listUserPolicies`

``` purescript
listUserPolicies :: forall eff. ListUserPoliciesRequest -> Aff (err :: RequestError | eff) ListUserPoliciesResponse
```

<p>Lists the names of the inline policies embedded in the specified IAM user.</p> <p>An IAM user can also have managed policies attached to it. To list the managed policies that are attached to a user, use <a>ListAttachedUserPolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified user, the action returns an empty list.</p>

#### `listUsers`

``` purescript
listUsers :: forall eff. ListUsersRequest -> Aff (err :: RequestError | eff) ListUsersResponse
```

<p>Lists the IAM users that have the specified path prefix. If no path prefix is specified, the action returns all users in the AWS account. If there are none, the action returns an empty list.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `listVirtualMFADevices`

``` purescript
listVirtualMFADevices :: forall eff. ListVirtualMFADevicesRequest -> Aff (err :: RequestError | eff) ListVirtualMFADevicesResponse
```

<p>Lists the virtual MFA devices defined in the AWS account by assignment status. If you do not specify an assignment status, the action returns a list of all virtual MFA devices. Assignment status can be <code>Assigned</code>, <code>Unassigned</code>, or <code>Any</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>

#### `putGroupPolicy`

``` purescript
putGroupPolicy :: forall eff. PutGroupPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates an inline policy document that is embedded in the specified IAM group.</p> <p>A user can also have managed policies attached to it. To attach a managed policy to a group, use <a>AttachGroupPolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed in a group, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutGroupPolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>

#### `putRolePolicy`

``` purescript
putRolePolicy :: forall eff. PutRolePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates an inline policy document that is embedded in the specified IAM role.</p> <p>When you embed an inline policy in a role, the inline policy is used as part of the role's access (permissions) policy. The role's trust policy is created at the same time as the role, using <a>CreateRole</a>. You can update a role's trust policy using <a>UpdateAssumeRolePolicy</a>. For more information about IAM roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p> <p>A role can also have a managed policy attached to it. To attach a managed policy to a role, use <a>AttachRolePolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed with a role, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutRolePolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>

#### `putUserPolicy`

``` purescript
putUserPolicy :: forall eff. PutUserPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates an inline policy document that is embedded in the specified IAM user.</p> <p>An IAM user can also have a managed policy attached to it. To attach a managed policy to a user, use <a>AttachUserPolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed in a user, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutUserPolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>

#### `removeClientIDFromOpenIDConnectProvider`

``` purescript
removeClientIDFromOpenIDConnectProvider :: forall eff. RemoveClientIDFromOpenIDConnectProviderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified client ID (also known as audience) from the list of client IDs registered for the specified IAM OpenID Connect (OIDC) provider resource object.</p> <p>This action is idempotent; it does not fail or return an error if you try to remove a client ID that does not exist.</p>

#### `removeRoleFromInstanceProfile`

``` purescript
removeRoleFromInstanceProfile :: forall eff. RemoveRoleFromInstanceProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified IAM role from the specified EC2 instance profile.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the role you are about to remove from the instance profile. Removing a role from an instance profile that is associated with a running instance might break any applications running on the instance.</p> </important> <p> For more information about IAM roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>

#### `removeUserFromGroup`

``` purescript
removeUserFromGroup :: forall eff. RemoveUserFromGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified user from the specified group.</p>

#### `resetServiceSpecificCredential`

``` purescript
resetServiceSpecificCredential :: forall eff. ResetServiceSpecificCredentialRequest -> Aff (err :: RequestError | eff) ResetServiceSpecificCredentialResponse
```

<p>Resets the password for a service-specific credential. The new password is AWS generated and cryptographically strong. It cannot be configured by the user. Resetting the password immediately invalidates the previous password associated with this user.</p>

#### `resyncMFADevice`

``` purescript
resyncMFADevice :: forall eff. ResyncMFADeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Synchronizes the specified MFA device with its IAM resource object on the AWS servers.</p> <p>For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p>

#### `setDefaultPolicyVersion`

``` purescript
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the specified version of the specified policy as the policy's default (operative) version.</p> <p>This action affects all users, groups, and roles that the policy is attached to. To list the users, groups, and roles that the policy is attached to, use the <a>ListEntitiesForPolicy</a> API.</p> <p>For information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>

#### `simulateCustomPolicy`

``` purescript
simulateCustomPolicy :: forall eff. SimulateCustomPolicyRequest -> Aff (err :: RequestError | eff) SimulatePolicyResponse
```

<p>Simulate how a set of IAM policies and optionally a resource-based policy works with a list of API actions and AWS resources to determine the policies' effective permissions. The policies are provided as strings.</p> <p>The simulation does not perform the API actions; it only checks the authorization to determine if the simulated policies allow or deny the actions.</p> <p>If you want to simulate existing policies attached to an IAM user, group, or role, use <a>SimulatePrincipalPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the <code>Condition</code> element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use <a>GetContextKeysForCustomPolicy</a>.</p> <p>If the output is long, you can use <code>MaxItems</code> and <code>Marker</code> parameters to paginate the results.</p>

#### `simulatePrincipalPolicy`

``` purescript
simulatePrincipalPolicy :: forall eff. SimulatePrincipalPolicyRequest -> Aff (err :: RequestError | eff) SimulatePolicyResponse
```

<p>Simulate how a set of IAM policies attached to an IAM entity works with a list of API actions and AWS resources to determine the policies' effective permissions. The entity can be an IAM user, group, or role. If you specify a user, then the simulation also includes all of the policies that are attached to groups that the user belongs to .</p> <p>You can optionally include a list of one or more additional policies specified as strings to include in the simulation. If you want to simulate only policies specified as strings, use <a>SimulateCustomPolicy</a> instead.</p> <p>You can also optionally include one resource-based policy to be evaluated with each of the resources included in the simulation.</p> <p>The simulation does not perform the API actions, it only checks the authorization to determine if the simulated policies allow or deny the actions.</p> <p> <b>Note:</b> This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use <a>SimulateCustomPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the <code>Condition</code> element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use <a>GetContextKeysForPrincipalPolicy</a>.</p> <p>If the output is long, you can use the <code>MaxItems</code> and <code>Marker</code> parameters to paginate the results.</p>

#### `updateAccessKey`

``` purescript
updateAccessKey :: forall eff. UpdateAccessKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the status of the specified access key from Active to Inactive, or vice versa. This action can be used to disable a user's key as part of a key rotation work flow.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <p>For information about rotating keys, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html">Managing Keys and Certificates</a> in the <i>IAM User Guide</i>.</p>

#### `updateAccountPasswordPolicy`

``` purescript
updateAccountPasswordPolicy :: forall eff. UpdateAccountPasswordPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the password policy settings for the AWS account.</p> <note> <p>This action does not support partial updates. No parameters are required, but if you do not specify a parameter, that parameter's value reverts to its default value. See the <b>Request Parameters</b> section for each parameter's default value.</p> </note> <p> For more information about using a password policy, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html">Managing an IAM Password Policy</a> in the <i>IAM User Guide</i>.</p>

#### `updateAssumeRolePolicy`

``` purescript
updateAssumeRolePolicy :: forall eff. UpdateAssumeRolePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the policy that grants an IAM entity permission to assume a role. This is typically referred to as the "role trust policy". For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p>

#### `updateGroup`

``` purescript
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the name and/or the path of the specified IAM group.</p> <important> <p> You should understand the implications of changing a group's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html">Renaming Users and Groups</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p>To change an IAM group name the requester must have appropriate permissions on both the source object and the target object. For example, to change "Managers" to "MGRs", the entity making the request must have permission on both "Managers" and "MGRs", or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html">Permissions and Policies</a>. </p> </note>

#### `updateLoginProfile`

``` purescript
updateLoginProfile :: forall eff. UpdateLoginProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the password for the specified IAM user.</p> <p>IAM users can change their own passwords by calling <a>ChangePassword</a>. For more information about modifying passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>

#### `updateOpenIDConnectProviderThumbprint`

``` purescript
updateOpenIDConnectProviderThumbprint :: forall eff. UpdateOpenIDConnectProviderThumbprintRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Replaces the existing list of server certificate thumbprints associated with an OpenID Connect (OIDC) provider resource object with a new list of thumbprints.</p> <p>The list that you pass with this action completely replaces the existing list of thumbprints. (The lists are not merged.)</p> <p>Typically, you need to update a thumbprint only when the identity provider's certificate changes, which occurs rarely. However, if the provider's certificate <i>does</i> change, any attempt to assume an IAM role that specifies the OIDC provider as a principal fails until the certificate thumbprint is updated.</p> <note> <p>Because trust for the OIDC provider is ultimately derived from the provider's certificate and is validated by the thumbprint, it is a best practice to limit access to the <code>UpdateOpenIDConnectProviderThumbprint</code> action to highly-privileged users.</p> </note>

#### `updateRoleDescription`

``` purescript
updateRoleDescription :: forall eff. UpdateRoleDescriptionRequest -> Aff (err :: RequestError | eff) UpdateRoleDescriptionResponse
```

<p>Modifies the description of a role.</p>

#### `updateSAMLProvider`

``` purescript
updateSAMLProvider :: forall eff. UpdateSAMLProviderRequest -> Aff (err :: RequestError | eff) UpdateSAMLProviderResponse
```

<p>Updates the metadata document for an existing SAML provider resource object.</p> <note> <p>This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>

#### `updateSSHPublicKey`

``` purescript
updateSSHPublicKey :: forall eff. UpdateSSHPublicKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the status of an IAM user's SSH public key to active or inactive. SSH public keys that are inactive cannot be used for authentication. This action can be used to disable a user's SSH public key as part of a key rotation work flow.</p> <p>The SSH public key affected by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>

#### `updateServerCertificate`

``` purescript
updateServerCertificate :: forall eff. UpdateServerCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the name and/or the path of the specified server certificate stored in IAM.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <important> <p>You should understand the implications of changing a server certificate's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts">Renaming a Server Certificate</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p>To change a server certificate name the requester must have appropriate permissions on both the source object and the target object. For example, to change the name from "ProductionCert" to "ProdCert", the entity making the request must have permission on "ProductionCert" and "ProdCert", or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html">Access Management</a> in the <i>IAM User Guide</i>.</p> </note>

#### `updateServiceSpecificCredential`

``` purescript
updateServiceSpecificCredential :: forall eff. UpdateServiceSpecificCredentialRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the status of a service-specific credential to <code>Active</code> or <code>Inactive</code>. Service-specific credentials that are inactive cannot be used for authentication to the service. This action can be used to disable a user’s service-specific credential as part of a credential rotation work flow.</p>

#### `updateSigningCertificate`

``` purescript
updateSigningCertificate :: forall eff. UpdateSigningCertificateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the status of the specified user signing certificate from active to disabled, or vice versa. This action can be used to disable an IAM user's signing certificate as part of a certificate rotation work flow.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>

#### `updateUser`

``` purescript
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates the name and/or the path of the specified IAM user.</p> <important> <p> You should understand the implications of changing an IAM user's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_renaming">Renaming an IAM User</a> and <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_groups_manage_rename.html">Renaming an IAM Group</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p> To change a user name the requester must have appropriate permissions on both the source object and the target object. For example, to change Bob to Robert, the entity making the request must have permission on Bob and Robert, or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html">Permissions and Policies</a>. </p> </note>

#### `uploadSSHPublicKey`

``` purescript
uploadSSHPublicKey :: forall eff. UploadSSHPublicKeyRequest -> Aff (err :: RequestError | eff) UploadSSHPublicKeyResponse
```

<p>Uploads an SSH public key and associates it with the specified IAM user.</p> <p>The SSH public key uploaded by this action can be used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>

#### `uploadServerCertificate`

``` purescript
uploadServerCertificate :: forall eff. UploadServerCertificateRequest -> Aff (err :: RequestError | eff) UploadServerCertificateResponse
```

<p>Uploads a server certificate entity for the AWS account. The server certificate entity includes a public key certificate, a private key, and an optional certificate chain, which should all be PEM-encoded.</p> <p>We recommend that you use <a href="https://aws.amazon.com/certificate-manager/">AWS Certificate Manager</a> to provision, manage, and deploy your server certificates. With ACM you can request a certificate, deploy it to AWS resources, and let ACM handle certificate renewals for you. Certificates provided by ACM are free. For more information about using ACM, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/">AWS Certificate Manager User Guide</a>.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <p>For information about the number of server certificates you can upload, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html">Limitations on IAM Entities and Objects</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because the body of the public key certificate, private key, and the certificate chain can be large, you should use POST rather than GET when calling <code>UploadServerCertificate</code>. For information about setting up signatures and authorization through the API, go to <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a> in the <i>AWS General Reference</i>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/programming.html">Calling the API by Making HTTP Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>

#### `uploadSigningCertificate`

``` purescript
uploadSigningCertificate :: forall eff. UploadSigningCertificateRequest -> Aff (err :: RequestError | eff) UploadSigningCertificateResponse
```

<p>Uploads an X.509 signing certificate and associates it with the specified IAM user. Some AWS services use X.509 signing certificates to validate requests that are signed with a corresponding private key. When you upload the certificate, its default status is <code>Active</code>.</p> <p>If the <code>UserName</code> field is not specified, the IAM user name is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <note> <p>Because the body of a X.509 certificate can be large, you should use POST rather than GET when calling <code>UploadSigningCertificate</code>. For information about setting up signatures and authorization through the API, go to <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a> in the <i>AWS General Reference</i>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>

#### `AccessKey`

``` purescript
newtype AccessKey
  = AccessKey { "UserName" :: UserNameType', "AccessKeyId" :: AccessKeyIdType', "Status" :: StatusType', "SecretAccessKey" :: AccessKeySecretType', "CreateDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about an AWS access key.</p> <p> This data type is used as a response element in the <a>CreateAccessKey</a> and <a>ListAccessKeys</a> actions. </p> <note> <p>The <code>SecretAccessKey</code> value is returned only in response to <a>CreateAccessKey</a>. You can get a secret access key only when you first create an access key; you cannot recover the secret access key later. If you lose a secret access key, you must create a new access key.</p> </note>

#### `AccessKeyLastUsed`

``` purescript
newtype AccessKeyLastUsed
  = AccessKeyLastUsed { "LastUsedDate" :: DateType', "ServiceName" :: StringType', "Region" :: StringType' }
```

<p>Contains information about the last time an AWS access key was used.</p> <p>This data type is used as a response element in the <a>GetAccessKeyLastUsed</a> action.</p>

#### `AccessKeyMetadata`

``` purescript
newtype AccessKeyMetadata
  = AccessKeyMetadata { "UserName" :: NullOrUndefined (UserNameType'), "AccessKeyId" :: NullOrUndefined (AccessKeyIdType'), "Status" :: NullOrUndefined (StatusType'), "CreateDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about an AWS access key, without its secret key.</p> <p>This data type is used as a response element in the <a>ListAccessKeys</a> action.</p>

#### `ActionNameListType`

``` purescript
newtype ActionNameListType
  = ActionNameListType (Array ActionNameType)
```

#### `ActionNameType`

``` purescript
newtype ActionNameType
  = ActionNameType String
```

#### `AddClientIDToOpenIDConnectProviderRequest`

``` purescript
newtype AddClientIDToOpenIDConnectProviderRequest
  = AddClientIDToOpenIDConnectProviderRequest { "OpenIDConnectProviderArn" :: ArnType', "ClientID" :: ClientIDType' }
```

#### `AddRoleToInstanceProfileRequest`

``` purescript
newtype AddRoleToInstanceProfileRequest
  = AddRoleToInstanceProfileRequest { "InstanceProfileName" :: InstanceProfileNameType', "RoleName" :: RoleNameType' }
```

#### `AddUserToGroupRequest`

``` purescript
newtype AddUserToGroupRequest
  = AddUserToGroupRequest { "GroupName" :: GroupNameType', "UserName" :: ExistingUserNameType' }
```

#### `ArnListType`

``` purescript
newtype ArnListType
  = ArnListType (Array ArnType')
```

#### `AttachGroupPolicyRequest`

``` purescript
newtype AttachGroupPolicyRequest
  = AttachGroupPolicyRequest { "GroupName" :: GroupNameType', "PolicyArn" :: ArnType' }
```

#### `AttachRolePolicyRequest`

``` purescript
newtype AttachRolePolicyRequest
  = AttachRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyArn" :: ArnType' }
```

#### `AttachUserPolicyRequest`

``` purescript
newtype AttachUserPolicyRequest
  = AttachUserPolicyRequest { "UserName" :: UserNameType', "PolicyArn" :: ArnType' }
```

#### `AttachedPolicy`

``` purescript
newtype AttachedPolicy
  = AttachedPolicy { "PolicyName" :: NullOrUndefined (PolicyNameType'), "PolicyArn" :: NullOrUndefined (ArnType') }
```

<p>Contains information about an attached policy.</p> <p>An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the <a>ListAttachedGroupPolicies</a>, <a>ListAttachedRolePolicies</a>, <a>ListAttachedUserPolicies</a>, and <a>GetAccountAuthorizationDetails</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `BootstrapDatum`

``` purescript
newtype BootstrapDatum
  = BootstrapDatum String
```

#### `ChangePasswordRequest`

``` purescript
newtype ChangePasswordRequest
  = ChangePasswordRequest { "OldPassword" :: PasswordType', "NewPassword" :: PasswordType' }
```

#### `ColumnNumber`

``` purescript
newtype ColumnNumber
  = ColumnNumber Int
```

#### `ContextEntry`

``` purescript
newtype ContextEntry
  = ContextEntry { "ContextKeyName" :: NullOrUndefined (ContextKeyNameType), "ContextKeyValues" :: NullOrUndefined (ContextKeyValueListType), "ContextKeyType" :: NullOrUndefined (ContextKeyTypeEnum) }
```

<p>Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the <code>Condition</code> elements of the input policies.</p> <p>This data type is used as an input parameter to <code> <a>SimulateCustomPolicy</a> </code> and <code> <a>SimulateCustomPolicy</a> </code>.</p>

#### `ContextEntryListType`

``` purescript
newtype ContextEntryListType
  = ContextEntryListType (Array ContextEntry)
```

#### `ContextKeyNameType`

``` purescript
newtype ContextKeyNameType
  = ContextKeyNameType String
```

#### `ContextKeyNamesResultListType`

``` purescript
newtype ContextKeyNamesResultListType
  = ContextKeyNamesResultListType (Array ContextKeyNameType)
```

#### `ContextKeyTypeEnum`

``` purescript
newtype ContextKeyTypeEnum
  = ContextKeyTypeEnum String
```

#### `ContextKeyValueListType`

``` purescript
newtype ContextKeyValueListType
  = ContextKeyValueListType (Array ContextKeyValueType)
```

#### `ContextKeyValueType`

``` purescript
newtype ContextKeyValueType
  = ContextKeyValueType String
```

#### `CreateAccessKeyRequest`

``` purescript
newtype CreateAccessKeyRequest
  = CreateAccessKeyRequest { "UserName" :: NullOrUndefined (ExistingUserNameType') }
```

#### `CreateAccessKeyResponse`

``` purescript
newtype CreateAccessKeyResponse
  = CreateAccessKeyResponse { "AccessKey" :: AccessKey }
```

<p>Contains the response to a successful <a>CreateAccessKey</a> request. </p>

#### `CreateAccountAliasRequest`

``` purescript
newtype CreateAccountAliasRequest
  = CreateAccountAliasRequest { "AccountAlias" :: AccountAliasType' }
```

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "Path" :: NullOrUndefined (PathType'), "GroupName" :: GroupNameType' }
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "Group" :: Group }
```

<p>Contains the response to a successful <a>CreateGroup</a> request. </p>

#### `CreateInstanceProfileRequest`

``` purescript
newtype CreateInstanceProfileRequest
  = CreateInstanceProfileRequest { "InstanceProfileName" :: InstanceProfileNameType', "Path" :: NullOrUndefined (PathType') }
```

#### `CreateInstanceProfileResponse`

``` purescript
newtype CreateInstanceProfileResponse
  = CreateInstanceProfileResponse { "InstanceProfile" :: InstanceProfile }
```

<p>Contains the response to a successful <a>CreateInstanceProfile</a> request. </p>

#### `CreateLoginProfileRequest`

``` purescript
newtype CreateLoginProfileRequest
  = CreateLoginProfileRequest { "UserName" :: UserNameType', "Password" :: PasswordType', "PasswordResetRequired" :: NullOrUndefined (BooleanType') }
```

#### `CreateLoginProfileResponse`

``` purescript
newtype CreateLoginProfileResponse
  = CreateLoginProfileResponse { "LoginProfile" :: LoginProfile }
```

<p>Contains the response to a successful <a>CreateLoginProfile</a> request. </p>

#### `CreateOpenIDConnectProviderRequest`

``` purescript
newtype CreateOpenIDConnectProviderRequest
  = CreateOpenIDConnectProviderRequest { "Url" :: OpenIDConnectProviderUrlType, "ClientIDList" :: NullOrUndefined (ClientIDListType'), "ThumbprintList" :: ThumbprintListType' }
```

#### `CreateOpenIDConnectProviderResponse`

``` purescript
newtype CreateOpenIDConnectProviderResponse
  = CreateOpenIDConnectProviderResponse { "OpenIDConnectProviderArn" :: NullOrUndefined (ArnType') }
```

<p>Contains the response to a successful <a>CreateOpenIDConnectProvider</a> request. </p>

#### `CreatePolicyRequest`

``` purescript
newtype CreatePolicyRequest
  = CreatePolicyRequest { "PolicyName" :: PolicyNameType', "Path" :: NullOrUndefined (PolicyPathType'), "PolicyDocument" :: PolicyDocumentType', "Description" :: NullOrUndefined (PolicyDescriptionType') }
```

#### `CreatePolicyResponse`

``` purescript
newtype CreatePolicyResponse
  = CreatePolicyResponse { "Policy" :: NullOrUndefined (Policy) }
```

<p>Contains the response to a successful <a>CreatePolicy</a> request. </p>

#### `CreatePolicyVersionRequest`

``` purescript
newtype CreatePolicyVersionRequest
  = CreatePolicyVersionRequest { "PolicyArn" :: ArnType', "PolicyDocument" :: PolicyDocumentType', "SetAsDefault" :: NullOrUndefined (BooleanType') }
```

#### `CreatePolicyVersionResponse`

``` purescript
newtype CreatePolicyVersionResponse
  = CreatePolicyVersionResponse { "PolicyVersion" :: NullOrUndefined (PolicyVersion) }
```

<p>Contains the response to a successful <a>CreatePolicyVersion</a> request. </p>

#### `CreateRoleRequest`

``` purescript
newtype CreateRoleRequest
  = CreateRoleRequest { "Path" :: NullOrUndefined (PathType'), "RoleName" :: RoleNameType', "AssumeRolePolicyDocument" :: PolicyDocumentType', "Description" :: NullOrUndefined (RoleDescriptionType') }
```

#### `CreateRoleResponse`

``` purescript
newtype CreateRoleResponse
  = CreateRoleResponse { "Role" :: Role }
```

<p>Contains the response to a successful <a>CreateRole</a> request. </p>

#### `CreateSAMLProviderRequest`

``` purescript
newtype CreateSAMLProviderRequest
  = CreateSAMLProviderRequest { "SAMLMetadataDocument" :: SAMLMetadataDocumentType, "Name" :: SAMLProviderNameType }
```

#### `CreateSAMLProviderResponse`

``` purescript
newtype CreateSAMLProviderResponse
  = CreateSAMLProviderResponse { "SAMLProviderArn" :: NullOrUndefined (ArnType') }
```

<p>Contains the response to a successful <a>CreateSAMLProvider</a> request. </p>

#### `CreateServiceLinkedRoleRequest`

``` purescript
newtype CreateServiceLinkedRoleRequest
  = CreateServiceLinkedRoleRequest { "AWSServiceName" :: GroupNameType', "Description" :: NullOrUndefined (RoleDescriptionType'), "CustomSuffix" :: NullOrUndefined (CustomSuffixType') }
```

#### `CreateServiceLinkedRoleResponse`

``` purescript
newtype CreateServiceLinkedRoleResponse
  = CreateServiceLinkedRoleResponse { "Role" :: NullOrUndefined (Role) }
```

#### `CreateServiceSpecificCredentialRequest`

``` purescript
newtype CreateServiceSpecificCredentialRequest
  = CreateServiceSpecificCredentialRequest { "UserName" :: UserNameType', "ServiceName" :: ServiceName' }
```

#### `CreateServiceSpecificCredentialResponse`

``` purescript
newtype CreateServiceSpecificCredentialResponse
  = CreateServiceSpecificCredentialResponse { "ServiceSpecificCredential" :: NullOrUndefined (ServiceSpecificCredential) }
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "Path" :: NullOrUndefined (PathType'), "UserName" :: UserNameType' }
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse { "User" :: NullOrUndefined (User) }
```

<p>Contains the response to a successful <a>CreateUser</a> request. </p>

#### `CreateVirtualMFADeviceRequest`

``` purescript
newtype CreateVirtualMFADeviceRequest
  = CreateVirtualMFADeviceRequest { "Path" :: NullOrUndefined (PathType'), "VirtualMFADeviceName" :: VirtualMFADeviceName' }
```

#### `CreateVirtualMFADeviceResponse`

``` purescript
newtype CreateVirtualMFADeviceResponse
  = CreateVirtualMFADeviceResponse { "VirtualMFADevice" :: VirtualMFADevice }
```

<p>Contains the response to a successful <a>CreateVirtualMFADevice</a> request. </p>

#### `CredentialReportExpiredException`

``` purescript
newtype CredentialReportExpiredException
  = CredentialReportExpiredException { "Message'" :: NullOrUndefined (CredentialReportExpiredExceptionMessage') }
```

<p>The request was rejected because the most recent credential report has expired. To generate a new credential report, use <a>GenerateCredentialReport</a>. For more information about credential report expiration, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>

#### `CredentialReportNotPresentException`

``` purescript
newtype CredentialReportNotPresentException
  = CredentialReportNotPresentException { "Message'" :: NullOrUndefined (CredentialReportNotPresentExceptionMessage') }
```

<p>The request was rejected because the credential report does not exist. To generate a credential report, use <a>GenerateCredentialReport</a>.</p>

#### `CredentialReportNotReadyException`

``` purescript
newtype CredentialReportNotReadyException
  = CredentialReportNotReadyException { "Message'" :: NullOrUndefined (CredentialReportNotReadyExceptionMessage') }
```

<p>The request was rejected because the credential report is still being generated.</p>

#### `DeactivateMFADeviceRequest`

``` purescript
newtype DeactivateMFADeviceRequest
  = DeactivateMFADeviceRequest { "UserName" :: ExistingUserNameType', "SerialNumber" :: SerialNumberType' }
```

#### `DeleteAccessKeyRequest`

``` purescript
newtype DeleteAccessKeyRequest
  = DeleteAccessKeyRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "AccessKeyId" :: AccessKeyIdType' }
```

#### `DeleteAccountAliasRequest`

``` purescript
newtype DeleteAccountAliasRequest
  = DeleteAccountAliasRequest { "AccountAlias" :: AccountAliasType' }
```

#### `DeleteConflictException`

``` purescript
newtype DeleteConflictException
  = DeleteConflictException { "Message'" :: NullOrUndefined (DeleteConflictMessage') }
```

<p>The request was rejected because it attempted to delete a resource that has attached subordinate entities. The error message describes these entities.</p>

#### `DeleteGroupPolicyRequest`

``` purescript
newtype DeleteGroupPolicyRequest
  = DeleteGroupPolicyRequest { "GroupName" :: GroupNameType', "PolicyName" :: PolicyNameType' }
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "GroupName" :: GroupNameType' }
```

#### `DeleteInstanceProfileRequest`

``` purescript
newtype DeleteInstanceProfileRequest
  = DeleteInstanceProfileRequest { "InstanceProfileName" :: InstanceProfileNameType' }
```

#### `DeleteLoginProfileRequest`

``` purescript
newtype DeleteLoginProfileRequest
  = DeleteLoginProfileRequest { "UserName" :: UserNameType' }
```

#### `DeleteOpenIDConnectProviderRequest`

``` purescript
newtype DeleteOpenIDConnectProviderRequest
  = DeleteOpenIDConnectProviderRequest { "OpenIDConnectProviderArn" :: ArnType' }
```

#### `DeletePolicyRequest`

``` purescript
newtype DeletePolicyRequest
  = DeletePolicyRequest { "PolicyArn" :: ArnType' }
```

#### `DeletePolicyVersionRequest`

``` purescript
newtype DeletePolicyVersionRequest
  = DeletePolicyVersionRequest { "PolicyArn" :: ArnType', "VersionId" :: PolicyVersionIdType' }
```

#### `DeleteRolePolicyRequest`

``` purescript
newtype DeleteRolePolicyRequest
  = DeleteRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyName" :: PolicyNameType' }
```

#### `DeleteRoleRequest`

``` purescript
newtype DeleteRoleRequest
  = DeleteRoleRequest { "RoleName" :: RoleNameType' }
```

#### `DeleteSAMLProviderRequest`

``` purescript
newtype DeleteSAMLProviderRequest
  = DeleteSAMLProviderRequest { "SAMLProviderArn" :: ArnType' }
```

#### `DeleteSSHPublicKeyRequest`

``` purescript
newtype DeleteSSHPublicKeyRequest
  = DeleteSSHPublicKeyRequest { "UserName" :: UserNameType', "SSHPublicKeyId" :: PublicKeyIdType' }
```

#### `DeleteServerCertificateRequest`

``` purescript
newtype DeleteServerCertificateRequest
  = DeleteServerCertificateRequest { "ServerCertificateName" :: ServerCertificateNameType' }
```

#### `DeleteServiceLinkedRoleRequest`

``` purescript
newtype DeleteServiceLinkedRoleRequest
  = DeleteServiceLinkedRoleRequest { "RoleName" :: RoleNameType' }
```

#### `DeleteServiceLinkedRoleResponse`

``` purescript
newtype DeleteServiceLinkedRoleResponse
  = DeleteServiceLinkedRoleResponse { "DeletionTaskId" :: DeletionTaskIdType }
```

#### `DeleteServiceSpecificCredentialRequest`

``` purescript
newtype DeleteServiceSpecificCredentialRequest
  = DeleteServiceSpecificCredentialRequest { "UserName" :: NullOrUndefined (UserNameType'), "ServiceSpecificCredentialId" :: ServiceSpecificCredentialId' }
```

#### `DeleteSigningCertificateRequest`

``` purescript
newtype DeleteSigningCertificateRequest
  = DeleteSigningCertificateRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "CertificateId" :: CertificateIdType' }
```

#### `DeleteUserPolicyRequest`

``` purescript
newtype DeleteUserPolicyRequest
  = DeleteUserPolicyRequest { "UserName" :: ExistingUserNameType', "PolicyName" :: PolicyNameType' }
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "UserName" :: ExistingUserNameType' }
```

#### `DeleteVirtualMFADeviceRequest`

``` purescript
newtype DeleteVirtualMFADeviceRequest
  = DeleteVirtualMFADeviceRequest { "SerialNumber" :: SerialNumberType' }
```

#### `DeletionTaskFailureReasonType`

``` purescript
newtype DeletionTaskFailureReasonType
  = DeletionTaskFailureReasonType { "Reason" :: NullOrUndefined (ReasonType), "RoleUsageList" :: NullOrUndefined (RoleUsageListType) }
```

<p>The reason that the service-linked role deletion failed.</p> <p>This data type is used as a response element in the <a>GetServiceLinkedRoleDeletionStatus</a> operation.</p>

#### `DeletionTaskIdType`

``` purescript
newtype DeletionTaskIdType
  = DeletionTaskIdType String
```

#### `DeletionTaskStatusType`

``` purescript
newtype DeletionTaskStatusType
  = DeletionTaskStatusType String
```

#### `DetachGroupPolicyRequest`

``` purescript
newtype DetachGroupPolicyRequest
  = DetachGroupPolicyRequest { "GroupName" :: GroupNameType', "PolicyArn" :: ArnType' }
```

#### `DetachRolePolicyRequest`

``` purescript
newtype DetachRolePolicyRequest
  = DetachRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyArn" :: ArnType' }
```

#### `DetachUserPolicyRequest`

``` purescript
newtype DetachUserPolicyRequest
  = DetachUserPolicyRequest { "UserName" :: UserNameType', "PolicyArn" :: ArnType' }
```

#### `DuplicateCertificateException`

``` purescript
newtype DuplicateCertificateException
  = DuplicateCertificateException { "Message'" :: NullOrUndefined (DuplicateCertificateMessage') }
```

<p>The request was rejected because the same certificate is associated with an IAM user in the account.</p>

#### `DuplicateSSHPublicKeyException`

``` purescript
newtype DuplicateSSHPublicKeyException
  = DuplicateSSHPublicKeyException { "Message'" :: NullOrUndefined (DuplicateSSHPublicKeyMessage') }
```

<p>The request was rejected because the SSH public key is already associated with the specified IAM user.</p>

#### `EnableMFADeviceRequest`

``` purescript
newtype EnableMFADeviceRequest
  = EnableMFADeviceRequest { "UserName" :: ExistingUserNameType', "SerialNumber" :: SerialNumberType', "AuthenticationCode1" :: AuthenticationCodeType', "AuthenticationCode2" :: AuthenticationCodeType' }
```

#### `EntityAlreadyExistsException`

``` purescript
newtype EntityAlreadyExistsException
  = EntityAlreadyExistsException { "Message'" :: NullOrUndefined (EntityAlreadyExistsMessage') }
```

<p>The request was rejected because it attempted to create a resource that already exists.</p>

#### `EntityTemporarilyUnmodifiableException`

``` purescript
newtype EntityTemporarilyUnmodifiableException
  = EntityTemporarilyUnmodifiableException { "Message'" :: NullOrUndefined (EntityTemporarilyUnmodifiableMessage') }
```

<p>The request was rejected because it referenced an entity that is temporarily unmodifiable, such as a user name that was deleted and then recreated. The error indicates that the request is likely to succeed if you try again after waiting several minutes. The error message describes the entity.</p>

#### `EntityType`

``` purescript
newtype EntityType
  = EntityType String
```

#### `EvalDecisionDetailsType`

``` purescript
newtype EvalDecisionDetailsType
  = EvalDecisionDetailsType (Map EvalDecisionSourceType PolicyEvaluationDecisionType)
```

#### `EvalDecisionSourceType`

``` purescript
newtype EvalDecisionSourceType
  = EvalDecisionSourceType String
```

#### `EvaluationResult`

``` purescript
newtype EvaluationResult
  = EvaluationResult { "EvalActionName" :: ActionNameType, "EvalResourceName" :: NullOrUndefined (ResourceNameType), "EvalDecision" :: PolicyEvaluationDecisionType, "MatchedStatements" :: NullOrUndefined (StatementListType), "MissingContextValues" :: NullOrUndefined (ContextKeyNamesResultListType), "OrganizationsDecisionDetail" :: NullOrUndefined (OrganizationsDecisionDetail), "EvalDecisionDetails" :: NullOrUndefined (EvalDecisionDetailsType), "ResourceSpecificResults" :: NullOrUndefined (ResourceSpecificResultListType) }
```

<p>Contains the results of a simulation.</p> <p>This data type is used by the return parameter of <code> <a>SimulateCustomPolicy</a> </code> and <code> <a>SimulatePrincipalPolicy</a> </code>.</p>

#### `EvaluationResultsListType`

``` purescript
newtype EvaluationResultsListType
  = EvaluationResultsListType (Array EvaluationResult)
```

#### `GenerateCredentialReportResponse`

``` purescript
newtype GenerateCredentialReportResponse
  = GenerateCredentialReportResponse { "State" :: NullOrUndefined (ReportStateType), "Description" :: NullOrUndefined (ReportStateDescriptionType) }
```

<p>Contains the response to a successful <a>GenerateCredentialReport</a> request. </p>

#### `GetAccessKeyLastUsedRequest`

``` purescript
newtype GetAccessKeyLastUsedRequest
  = GetAccessKeyLastUsedRequest { "AccessKeyId" :: AccessKeyIdType' }
```

#### `GetAccessKeyLastUsedResponse`

``` purescript
newtype GetAccessKeyLastUsedResponse
  = GetAccessKeyLastUsedResponse { "UserName" :: NullOrUndefined (ExistingUserNameType'), "AccessKeyLastUsed" :: NullOrUndefined (AccessKeyLastUsed) }
```

<p>Contains the response to a successful <a>GetAccessKeyLastUsed</a> request. It is also returned as a member of the <a>AccessKeyMetaData</a> structure returned by the <a>ListAccessKeys</a> action.</p>

#### `GetAccountAuthorizationDetailsRequest`

``` purescript
newtype GetAccountAuthorizationDetailsRequest
  = GetAccountAuthorizationDetailsRequest { "Filter" :: NullOrUndefined (EntityListType'), "MaxItems" :: NullOrUndefined (MaxItemsType'), "Marker" :: NullOrUndefined (MarkerType') }
```

#### `GetAccountAuthorizationDetailsResponse`

``` purescript
newtype GetAccountAuthorizationDetailsResponse
  = GetAccountAuthorizationDetailsResponse { "UserDetailList" :: NullOrUndefined (UserDetailListType'), "GroupDetailList" :: NullOrUndefined (GroupDetailListType'), "RoleDetailList" :: NullOrUndefined (RoleDetailListType'), "Policies" :: NullOrUndefined (ManagedPolicyDetailListType), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>GetAccountAuthorizationDetails</a> request. </p>

#### `GetAccountPasswordPolicyResponse`

``` purescript
newtype GetAccountPasswordPolicyResponse
  = GetAccountPasswordPolicyResponse { "PasswordPolicy" :: PasswordPolicy }
```

<p>Contains the response to a successful <a>GetAccountPasswordPolicy</a> request. </p>

#### `GetAccountSummaryResponse`

``` purescript
newtype GetAccountSummaryResponse
  = GetAccountSummaryResponse { "SummaryMap" :: NullOrUndefined (SummaryMapType') }
```

<p>Contains the response to a successful <a>GetAccountSummary</a> request. </p>

#### `GetContextKeysForCustomPolicyRequest`

``` purescript
newtype GetContextKeysForCustomPolicyRequest
  = GetContextKeysForCustomPolicyRequest { "PolicyInputList" :: SimulationPolicyListType }
```

#### `GetContextKeysForPolicyResponse`

``` purescript
newtype GetContextKeysForPolicyResponse
  = GetContextKeysForPolicyResponse { "ContextKeyNames" :: NullOrUndefined (ContextKeyNamesResultListType) }
```

<p>Contains the response to a successful <a>GetContextKeysForPrincipalPolicy</a> or <a>GetContextKeysForCustomPolicy</a> request. </p>

#### `GetContextKeysForPrincipalPolicyRequest`

``` purescript
newtype GetContextKeysForPrincipalPolicyRequest
  = GetContextKeysForPrincipalPolicyRequest { "PolicySourceArn" :: ArnType', "PolicyInputList" :: NullOrUndefined (SimulationPolicyListType) }
```

#### `GetCredentialReportResponse`

``` purescript
newtype GetCredentialReportResponse
  = GetCredentialReportResponse { "Content" :: NullOrUndefined (ReportContentType), "ReportFormat" :: NullOrUndefined (ReportFormatType), "GeneratedTime" :: NullOrUndefined (DateType') }
```

<p>Contains the response to a successful <a>GetCredentialReport</a> request. </p>

#### `GetGroupPolicyRequest`

``` purescript
newtype GetGroupPolicyRequest
  = GetGroupPolicyRequest { "GroupName" :: GroupNameType', "PolicyName" :: PolicyNameType' }
```

#### `GetGroupPolicyResponse`

``` purescript
newtype GetGroupPolicyResponse
  = GetGroupPolicyResponse { "GroupName" :: GroupNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

<p>Contains the response to a successful <a>GetGroupPolicy</a> request. </p>

#### `GetGroupRequest`

``` purescript
newtype GetGroupRequest
  = GetGroupRequest { "GroupName" :: GroupNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `GetGroupResponse`

``` purescript
newtype GetGroupResponse
  = GetGroupResponse { "Group" :: Group, "Users" :: UserListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>GetGroup</a> request. </p>

#### `GetInstanceProfileRequest`

``` purescript
newtype GetInstanceProfileRequest
  = GetInstanceProfileRequest { "InstanceProfileName" :: InstanceProfileNameType' }
```

#### `GetInstanceProfileResponse`

``` purescript
newtype GetInstanceProfileResponse
  = GetInstanceProfileResponse { "InstanceProfile" :: InstanceProfile }
```

<p>Contains the response to a successful <a>GetInstanceProfile</a> request. </p>

#### `GetLoginProfileRequest`

``` purescript
newtype GetLoginProfileRequest
  = GetLoginProfileRequest { "UserName" :: UserNameType' }
```

#### `GetLoginProfileResponse`

``` purescript
newtype GetLoginProfileResponse
  = GetLoginProfileResponse { "LoginProfile" :: LoginProfile }
```

<p>Contains the response to a successful <a>GetLoginProfile</a> request. </p>

#### `GetOpenIDConnectProviderRequest`

``` purescript
newtype GetOpenIDConnectProviderRequest
  = GetOpenIDConnectProviderRequest { "OpenIDConnectProviderArn" :: ArnType' }
```

#### `GetOpenIDConnectProviderResponse`

``` purescript
newtype GetOpenIDConnectProviderResponse
  = GetOpenIDConnectProviderResponse { "Url" :: NullOrUndefined (OpenIDConnectProviderUrlType), "ClientIDList" :: NullOrUndefined (ClientIDListType'), "ThumbprintList" :: NullOrUndefined (ThumbprintListType'), "CreateDate" :: NullOrUndefined (DateType') }
```

<p>Contains the response to a successful <a>GetOpenIDConnectProvider</a> request. </p>

#### `GetPolicyRequest`

``` purescript
newtype GetPolicyRequest
  = GetPolicyRequest { "PolicyArn" :: ArnType' }
```

#### `GetPolicyResponse`

``` purescript
newtype GetPolicyResponse
  = GetPolicyResponse { "Policy" :: NullOrUndefined (Policy) }
```

<p>Contains the response to a successful <a>GetPolicy</a> request. </p>

#### `GetPolicyVersionRequest`

``` purescript
newtype GetPolicyVersionRequest
  = GetPolicyVersionRequest { "PolicyArn" :: ArnType', "VersionId" :: PolicyVersionIdType' }
```

#### `GetPolicyVersionResponse`

``` purescript
newtype GetPolicyVersionResponse
  = GetPolicyVersionResponse { "PolicyVersion" :: NullOrUndefined (PolicyVersion) }
```

<p>Contains the response to a successful <a>GetPolicyVersion</a> request. </p>

#### `GetRolePolicyRequest`

``` purescript
newtype GetRolePolicyRequest
  = GetRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyName" :: PolicyNameType' }
```

#### `GetRolePolicyResponse`

``` purescript
newtype GetRolePolicyResponse
  = GetRolePolicyResponse { "RoleName" :: RoleNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

<p>Contains the response to a successful <a>GetRolePolicy</a> request. </p>

#### `GetRoleRequest`

``` purescript
newtype GetRoleRequest
  = GetRoleRequest { "RoleName" :: RoleNameType' }
```

#### `GetRoleResponse`

``` purescript
newtype GetRoleResponse
  = GetRoleResponse { "Role" :: Role }
```

<p>Contains the response to a successful <a>GetRole</a> request. </p>

#### `GetSAMLProviderRequest`

``` purescript
newtype GetSAMLProviderRequest
  = GetSAMLProviderRequest { "SAMLProviderArn" :: ArnType' }
```

#### `GetSAMLProviderResponse`

``` purescript
newtype GetSAMLProviderResponse
  = GetSAMLProviderResponse { "SAMLMetadataDocument" :: NullOrUndefined (SAMLMetadataDocumentType), "CreateDate" :: NullOrUndefined (DateType'), "ValidUntil" :: NullOrUndefined (DateType') }
```

<p>Contains the response to a successful <a>GetSAMLProvider</a> request. </p>

#### `GetSSHPublicKeyRequest`

``` purescript
newtype GetSSHPublicKeyRequest
  = GetSSHPublicKeyRequest { "UserName" :: UserNameType', "SSHPublicKeyId" :: PublicKeyIdType', "Encoding" :: EncodingType' }
```

#### `GetSSHPublicKeyResponse`

``` purescript
newtype GetSSHPublicKeyResponse
  = GetSSHPublicKeyResponse { "SSHPublicKey" :: NullOrUndefined (SSHPublicKey) }
```

<p>Contains the response to a successful <a>GetSSHPublicKey</a> request.</p>

#### `GetServerCertificateRequest`

``` purescript
newtype GetServerCertificateRequest
  = GetServerCertificateRequest { "ServerCertificateName" :: ServerCertificateNameType' }
```

#### `GetServerCertificateResponse`

``` purescript
newtype GetServerCertificateResponse
  = GetServerCertificateResponse { "ServerCertificate" :: ServerCertificate }
```

<p>Contains the response to a successful <a>GetServerCertificate</a> request. </p>

#### `GetServiceLinkedRoleDeletionStatusRequest`

``` purescript
newtype GetServiceLinkedRoleDeletionStatusRequest
  = GetServiceLinkedRoleDeletionStatusRequest { "DeletionTaskId" :: DeletionTaskIdType }
```

#### `GetServiceLinkedRoleDeletionStatusResponse`

``` purescript
newtype GetServiceLinkedRoleDeletionStatusResponse
  = GetServiceLinkedRoleDeletionStatusResponse { "Status" :: DeletionTaskStatusType, "Reason" :: NullOrUndefined (DeletionTaskFailureReasonType) }
```

#### `GetUserPolicyRequest`

``` purescript
newtype GetUserPolicyRequest
  = GetUserPolicyRequest { "UserName" :: ExistingUserNameType', "PolicyName" :: PolicyNameType' }
```

#### `GetUserPolicyResponse`

``` purescript
newtype GetUserPolicyResponse
  = GetUserPolicyResponse { "UserName" :: ExistingUserNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

<p>Contains the response to a successful <a>GetUserPolicy</a> request. </p>

#### `GetUserRequest`

``` purescript
newtype GetUserRequest
  = GetUserRequest { "UserName" :: NullOrUndefined (ExistingUserNameType') }
```

#### `GetUserResponse`

``` purescript
newtype GetUserResponse
  = GetUserResponse { "User" :: User }
```

<p>Contains the response to a successful <a>GetUser</a> request. </p>

#### `Group`

``` purescript
newtype Group
  = Group { "Path" :: PathType', "GroupName" :: GroupNameType', "GroupId" :: IdType', "Arn" :: ArnType', "CreateDate" :: DateType' }
```

<p>Contains information about an IAM group entity.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateGroup</a> </p> </li> <li> <p> <a>GetGroup</a> </p> </li> <li> <p> <a>ListGroups</a> </p> </li> </ul>

#### `GroupDetail`

``` purescript
newtype GroupDetail
  = GroupDetail { "Path" :: NullOrUndefined (PathType'), "GroupName" :: NullOrUndefined (GroupNameType'), "GroupId" :: NullOrUndefined (IdType'), "Arn" :: NullOrUndefined (ArnType'), "CreateDate" :: NullOrUndefined (DateType'), "GroupPolicyList" :: NullOrUndefined (PolicyDetailListType'), "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType') }
```

<p>Contains information about an IAM group, including all of the group's policies.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>

#### `InstanceProfile`

``` purescript
newtype InstanceProfile
  = InstanceProfile { "Path" :: PathType', "InstanceProfileName" :: InstanceProfileNameType', "InstanceProfileId" :: IdType', "Arn" :: ArnType', "CreateDate" :: DateType', "Roles" :: RoleListType' }
```

<p>Contains information about an instance profile.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateInstanceProfile</a> </p> </li> <li> <p> <a>GetInstanceProfile</a> </p> </li> <li> <p> <a>ListInstanceProfiles</a> </p> </li> <li> <p> <a>ListInstanceProfilesForRole</a> </p> </li> </ul>

#### `InvalidAuthenticationCodeException`

``` purescript
newtype InvalidAuthenticationCodeException
  = InvalidAuthenticationCodeException { "Message'" :: NullOrUndefined (InvalidAuthenticationCodeMessage') }
```

<p>The request was rejected because the authentication code was not recognized. The error message describes the specific error.</p>

#### `InvalidCertificateException`

``` purescript
newtype InvalidCertificateException
  = InvalidCertificateException { "Message'" :: NullOrUndefined (InvalidCertificateMessage') }
```

<p>The request was rejected because the certificate is invalid.</p>

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message'" :: NullOrUndefined (InvalidInputMessage') }
```

<p>The request was rejected because an invalid or out-of-range value was supplied for an input parameter.</p>

#### `InvalidPublicKeyException`

``` purescript
newtype InvalidPublicKeyException
  = InvalidPublicKeyException { "Message'" :: NullOrUndefined (InvalidPublicKeyMessage') }
```

<p>The request was rejected because the public key is malformed or otherwise invalid.</p>

#### `InvalidUserTypeException`

``` purescript
newtype InvalidUserTypeException
  = InvalidUserTypeException { "Message'" :: NullOrUndefined (InvalidUserTypeMessage') }
```

<p>The request was rejected because the type of user for the transaction was incorrect.</p>

#### `KeyPairMismatchException`

``` purescript
newtype KeyPairMismatchException
  = KeyPairMismatchException { "Message'" :: NullOrUndefined (KeyPairMismatchMessage') }
```

<p>The request was rejected because the public key certificate and the private key do not match.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (LimitExceededMessage') }
```

<p>The request was rejected because it attempted to create resources beyond the current AWS account limits. The error message describes the limit exceeded.</p>

#### `LineNumber`

``` purescript
newtype LineNumber
  = LineNumber Int
```

#### `ListAccessKeysRequest`

``` purescript
newtype ListAccessKeysRequest
  = ListAccessKeysRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListAccessKeysResponse`

``` purescript
newtype ListAccessKeysResponse
  = ListAccessKeysResponse { "AccessKeyMetadata" :: AccessKeyMetadataListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListAccessKeys</a> request. </p>

#### `ListAccountAliasesRequest`

``` purescript
newtype ListAccountAliasesRequest
  = ListAccountAliasesRequest { "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListAccountAliasesResponse`

``` purescript
newtype ListAccountAliasesResponse
  = ListAccountAliasesResponse { "AccountAliases" :: AccountAliasListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListAccountAliases</a> request. </p>

#### `ListAttachedGroupPoliciesRequest`

``` purescript
newtype ListAttachedGroupPoliciesRequest
  = ListAttachedGroupPoliciesRequest { "GroupName" :: GroupNameType', "PathPrefix" :: NullOrUndefined (PolicyPathType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListAttachedGroupPoliciesResponse`

``` purescript
newtype ListAttachedGroupPoliciesResponse
  = ListAttachedGroupPoliciesResponse { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType'), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListAttachedGroupPolicies</a> request. </p>

#### `ListAttachedRolePoliciesRequest`

``` purescript
newtype ListAttachedRolePoliciesRequest
  = ListAttachedRolePoliciesRequest { "RoleName" :: RoleNameType', "PathPrefix" :: NullOrUndefined (PolicyPathType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListAttachedRolePoliciesResponse`

``` purescript
newtype ListAttachedRolePoliciesResponse
  = ListAttachedRolePoliciesResponse { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType'), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListAttachedRolePolicies</a> request. </p>

#### `ListAttachedUserPoliciesRequest`

``` purescript
newtype ListAttachedUserPoliciesRequest
  = ListAttachedUserPoliciesRequest { "UserName" :: UserNameType', "PathPrefix" :: NullOrUndefined (PolicyPathType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListAttachedUserPoliciesResponse`

``` purescript
newtype ListAttachedUserPoliciesResponse
  = ListAttachedUserPoliciesResponse { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType'), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListAttachedUserPolicies</a> request. </p>

#### `ListEntitiesForPolicyRequest`

``` purescript
newtype ListEntitiesForPolicyRequest
  = ListEntitiesForPolicyRequest { "PolicyArn" :: ArnType', "EntityFilter" :: NullOrUndefined (EntityType), "PathPrefix" :: NullOrUndefined (PathType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListEntitiesForPolicyResponse`

``` purescript
newtype ListEntitiesForPolicyResponse
  = ListEntitiesForPolicyResponse { "PolicyGroups" :: NullOrUndefined (PolicyGroupListType), "PolicyUsers" :: NullOrUndefined (PolicyUserListType), "PolicyRoles" :: NullOrUndefined (PolicyRoleListType), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListEntitiesForPolicy</a> request. </p>

#### `ListGroupPoliciesRequest`

``` purescript
newtype ListGroupPoliciesRequest
  = ListGroupPoliciesRequest { "GroupName" :: GroupNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListGroupPoliciesResponse`

``` purescript
newtype ListGroupPoliciesResponse
  = ListGroupPoliciesResponse { "PolicyNames" :: PolicyNameListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListGroupPolicies</a> request. </p>

#### `ListGroupsForUserRequest`

``` purescript
newtype ListGroupsForUserRequest
  = ListGroupsForUserRequest { "UserName" :: ExistingUserNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListGroupsForUserResponse`

``` purescript
newtype ListGroupsForUserResponse
  = ListGroupsForUserResponse { "Groups" :: GroupListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListGroupsForUser</a> request. </p>

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "PathPrefix" :: NullOrUndefined (PathPrefixType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: GroupListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListGroups</a> request. </p>

#### `ListInstanceProfilesForRoleRequest`

``` purescript
newtype ListInstanceProfilesForRoleRequest
  = ListInstanceProfilesForRoleRequest { "RoleName" :: RoleNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListInstanceProfilesForRoleResponse`

``` purescript
newtype ListInstanceProfilesForRoleResponse
  = ListInstanceProfilesForRoleResponse { "InstanceProfiles" :: InstanceProfileListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListInstanceProfilesForRole</a> request. </p>

#### `ListInstanceProfilesRequest`

``` purescript
newtype ListInstanceProfilesRequest
  = ListInstanceProfilesRequest { "PathPrefix" :: NullOrUndefined (PathPrefixType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListInstanceProfilesResponse`

``` purescript
newtype ListInstanceProfilesResponse
  = ListInstanceProfilesResponse { "InstanceProfiles" :: InstanceProfileListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListInstanceProfiles</a> request. </p>

#### `ListMFADevicesRequest`

``` purescript
newtype ListMFADevicesRequest
  = ListMFADevicesRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListMFADevicesResponse`

``` purescript
newtype ListMFADevicesResponse
  = ListMFADevicesResponse { "MFADevices" :: MfaDeviceListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListMFADevices</a> request. </p>

#### `ListOpenIDConnectProvidersRequest`

``` purescript
newtype ListOpenIDConnectProvidersRequest
  = ListOpenIDConnectProvidersRequest {  }
```

#### `ListOpenIDConnectProvidersResponse`

``` purescript
newtype ListOpenIDConnectProvidersResponse
  = ListOpenIDConnectProvidersResponse { "OpenIDConnectProviderList" :: NullOrUndefined (OpenIDConnectProviderListType) }
```

<p>Contains the response to a successful <a>ListOpenIDConnectProviders</a> request. </p>

#### `ListPoliciesRequest`

``` purescript
newtype ListPoliciesRequest
  = ListPoliciesRequest { "Scope" :: NullOrUndefined (PolicyScopeType'), "OnlyAttached" :: NullOrUndefined (BooleanType'), "PathPrefix" :: NullOrUndefined (PolicyPathType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListPoliciesResponse`

``` purescript
newtype ListPoliciesResponse
  = ListPoliciesResponse { "Policies" :: NullOrUndefined (PolicyListType'), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListPolicies</a> request. </p>

#### `ListPolicyVersionsRequest`

``` purescript
newtype ListPolicyVersionsRequest
  = ListPolicyVersionsRequest { "PolicyArn" :: ArnType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListPolicyVersionsResponse`

``` purescript
newtype ListPolicyVersionsResponse
  = ListPolicyVersionsResponse { "Versions" :: NullOrUndefined (PolicyDocumentVersionListType'), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListPolicyVersions</a> request. </p>

#### `ListRolePoliciesRequest`

``` purescript
newtype ListRolePoliciesRequest
  = ListRolePoliciesRequest { "RoleName" :: RoleNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListRolePoliciesResponse`

``` purescript
newtype ListRolePoliciesResponse
  = ListRolePoliciesResponse { "PolicyNames" :: PolicyNameListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListRolePolicies</a> request. </p>

#### `ListRolesRequest`

``` purescript
newtype ListRolesRequest
  = ListRolesRequest { "PathPrefix" :: NullOrUndefined (PathPrefixType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListRolesResponse`

``` purescript
newtype ListRolesResponse
  = ListRolesResponse { "Roles" :: RoleListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListRoles</a> request. </p>

#### `ListSAMLProvidersRequest`

``` purescript
newtype ListSAMLProvidersRequest
  = ListSAMLProvidersRequest {  }
```

#### `ListSAMLProvidersResponse`

``` purescript
newtype ListSAMLProvidersResponse
  = ListSAMLProvidersResponse { "SAMLProviderList" :: NullOrUndefined (SAMLProviderListType) }
```

<p>Contains the response to a successful <a>ListSAMLProviders</a> request. </p>

#### `ListSSHPublicKeysRequest`

``` purescript
newtype ListSSHPublicKeysRequest
  = ListSSHPublicKeysRequest { "UserName" :: NullOrUndefined (UserNameType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListSSHPublicKeysResponse`

``` purescript
newtype ListSSHPublicKeysResponse
  = ListSSHPublicKeysResponse { "SSHPublicKeys" :: NullOrUndefined (SSHPublicKeyListType), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListSSHPublicKeys</a> request.</p>

#### `ListServerCertificatesRequest`

``` purescript
newtype ListServerCertificatesRequest
  = ListServerCertificatesRequest { "PathPrefix" :: NullOrUndefined (PathPrefixType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListServerCertificatesResponse`

``` purescript
newtype ListServerCertificatesResponse
  = ListServerCertificatesResponse { "ServerCertificateMetadataList" :: ServerCertificateMetadataListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListServerCertificates</a> request. </p>

#### `ListServiceSpecificCredentialsRequest`

``` purescript
newtype ListServiceSpecificCredentialsRequest
  = ListServiceSpecificCredentialsRequest { "UserName" :: NullOrUndefined (UserNameType'), "ServiceName" :: NullOrUndefined (ServiceName') }
```

#### `ListServiceSpecificCredentialsResponse`

``` purescript
newtype ListServiceSpecificCredentialsResponse
  = ListServiceSpecificCredentialsResponse { "ServiceSpecificCredentials" :: NullOrUndefined (ServiceSpecificCredentialsListType) }
```

#### `ListSigningCertificatesRequest`

``` purescript
newtype ListSigningCertificatesRequest
  = ListSigningCertificatesRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListSigningCertificatesResponse`

``` purescript
newtype ListSigningCertificatesResponse
  = ListSigningCertificatesResponse { "Certificates" :: CertificateListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListSigningCertificates</a> request. </p>

#### `ListUserPoliciesRequest`

``` purescript
newtype ListUserPoliciesRequest
  = ListUserPoliciesRequest { "UserName" :: ExistingUserNameType', "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListUserPoliciesResponse`

``` purescript
newtype ListUserPoliciesResponse
  = ListUserPoliciesResponse { "PolicyNames" :: PolicyNameListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListUserPolicies</a> request. </p>

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "PathPrefix" :: NullOrUndefined (PathPrefixType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "Users" :: UserListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListUsers</a> request. </p>

#### `ListVirtualMFADevicesRequest`

``` purescript
newtype ListVirtualMFADevicesRequest
  = ListVirtualMFADevicesRequest { "AssignmentStatus" :: NullOrUndefined (AssignmentStatusType'), "Marker" :: NullOrUndefined (MarkerType'), "MaxItems" :: NullOrUndefined (MaxItemsType') }
```

#### `ListVirtualMFADevicesResponse`

``` purescript
newtype ListVirtualMFADevicesResponse
  = ListVirtualMFADevicesResponse { "VirtualMFADevices" :: VirtualMFADeviceListType', "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>ListVirtualMFADevices</a> request. </p>

#### `LoginProfile`

``` purescript
newtype LoginProfile
  = LoginProfile { "UserName" :: UserNameType', "CreateDate" :: DateType', "PasswordResetRequired" :: NullOrUndefined (BooleanType') }
```

<p>Contains the user name and password create date for a user.</p> <p> This data type is used as a response element in the <a>CreateLoginProfile</a> and <a>GetLoginProfile</a> actions. </p>

#### `MFADevice`

``` purescript
newtype MFADevice
  = MFADevice { "UserName" :: UserNameType', "SerialNumber" :: SerialNumberType', "EnableDate" :: DateType' }
```

<p>Contains information about an MFA device.</p> <p>This data type is used as a response element in the <a>ListMFADevices</a> action.</p>

#### `MalformedCertificateException`

``` purescript
newtype MalformedCertificateException
  = MalformedCertificateException { "Message'" :: NullOrUndefined (MalformedCertificateMessage') }
```

<p>The request was rejected because the certificate was malformed or expired. The error message describes the specific error.</p>

#### `MalformedPolicyDocumentException`

``` purescript
newtype MalformedPolicyDocumentException
  = MalformedPolicyDocumentException { "Message'" :: NullOrUndefined (MalformedPolicyDocumentMessage') }
```

<p>The request was rejected because the policy document was malformed. The error message describes the specific error.</p>

#### `ManagedPolicyDetail`

``` purescript
newtype ManagedPolicyDetail
  = ManagedPolicyDetail { "PolicyName" :: NullOrUndefined (PolicyNameType'), "PolicyId" :: NullOrUndefined (IdType'), "Arn" :: NullOrUndefined (ArnType'), "Path" :: NullOrUndefined (PolicyPathType'), "DefaultVersionId" :: NullOrUndefined (PolicyVersionIdType'), "AttachmentCount" :: NullOrUndefined (AttachmentCountType'), "IsAttachable" :: NullOrUndefined (BooleanType'), "Description" :: NullOrUndefined (PolicyDescriptionType'), "CreateDate" :: NullOrUndefined (DateType'), "UpdateDate" :: NullOrUndefined (DateType'), "PolicyVersionList" :: NullOrUndefined (PolicyDocumentVersionListType') }
```

<p>Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `ManagedPolicyDetailListType`

``` purescript
newtype ManagedPolicyDetailListType
  = ManagedPolicyDetailListType (Array ManagedPolicyDetail)
```

#### `NoSuchEntityException`

``` purescript
newtype NoSuchEntityException
  = NoSuchEntityException { "Message'" :: NullOrUndefined (NoSuchEntityMessage') }
```

<p>The request was rejected because it referenced an entity that does not exist. The error message describes the entity.</p>

#### `OpenIDConnectProviderListEntry`

``` purescript
newtype OpenIDConnectProviderListEntry
  = OpenIDConnectProviderListEntry { "Arn" :: NullOrUndefined (ArnType') }
```

<p>Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.</p>

#### `OpenIDConnectProviderListType`

``` purescript
newtype OpenIDConnectProviderListType
  = OpenIDConnectProviderListType (Array OpenIDConnectProviderListEntry)
```

<p>Contains a list of IAM OpenID Connect providers.</p>

#### `OpenIDConnectProviderUrlType`

``` purescript
newtype OpenIDConnectProviderUrlType
  = OpenIDConnectProviderUrlType String
```

<p>Contains a URL that specifies the endpoint for an OpenID Connect provider.</p>

#### `OrganizationsDecisionDetail`

``` purescript
newtype OrganizationsDecisionDetail
  = OrganizationsDecisionDetail { "AllowedByOrganizations" :: NullOrUndefined (BooleanType') }
```

<p>Contains information about AWS Organizations's affect on a policy simulation.</p>

#### `PasswordPolicy`

``` purescript
newtype PasswordPolicy
  = PasswordPolicy { "MinimumPasswordLength" :: NullOrUndefined (MinimumPasswordLengthType'), "RequireSymbols" :: NullOrUndefined (BooleanType'), "RequireNumbers" :: NullOrUndefined (BooleanType'), "RequireUppercaseCharacters" :: NullOrUndefined (BooleanType'), "RequireLowercaseCharacters" :: NullOrUndefined (BooleanType'), "AllowUsersToChangePassword" :: NullOrUndefined (BooleanType'), "ExpirePasswords" :: NullOrUndefined (BooleanType'), "MaxPasswordAge" :: NullOrUndefined (MaxPasswordAgeType'), "PasswordReusePrevention" :: NullOrUndefined (PasswordReusePreventionType'), "HardExpiry" :: NullOrUndefined (BooleanObjectType') }
```

<p>Contains information about the account password policy.</p> <p> This data type is used as a response element in the <a>GetAccountPasswordPolicy</a> action. </p>

#### `PasswordPolicyViolationException`

``` purescript
newtype PasswordPolicyViolationException
  = PasswordPolicyViolationException { "Message'" :: NullOrUndefined (PasswordPolicyViolationMessage') }
```

<p>The request was rejected because the provided password did not meet the requirements imposed by the account password policy.</p>

#### `Policy`

``` purescript
newtype Policy
  = Policy { "PolicyName" :: NullOrUndefined (PolicyNameType'), "PolicyId" :: NullOrUndefined (IdType'), "Arn" :: NullOrUndefined (ArnType'), "Path" :: NullOrUndefined (PolicyPathType'), "DefaultVersionId" :: NullOrUndefined (PolicyVersionIdType'), "AttachmentCount" :: NullOrUndefined (AttachmentCountType'), "IsAttachable" :: NullOrUndefined (BooleanType'), "Description" :: NullOrUndefined (PolicyDescriptionType'), "CreateDate" :: NullOrUndefined (DateType'), "UpdateDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about a managed policy.</p> <p>This data type is used as a response element in the <a>CreatePolicy</a>, <a>GetPolicy</a>, and <a>ListPolicies</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `PolicyDetail`

``` purescript
newtype PolicyDetail
  = PolicyDetail { "PolicyName" :: NullOrUndefined (PolicyNameType'), "PolicyDocument" :: NullOrUndefined (PolicyDocumentType') }
```

<p>Contains information about an IAM policy, including the policy document.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>

#### `PolicyEvaluationDecisionType`

``` purescript
newtype PolicyEvaluationDecisionType
  = PolicyEvaluationDecisionType String
```

#### `PolicyEvaluationException`

``` purescript
newtype PolicyEvaluationException
  = PolicyEvaluationException { "Message'" :: NullOrUndefined (PolicyEvaluationErrorMessage') }
```

<p>The request failed because a provided policy could not be successfully evaluated. An additional detailed message indicates the source of the failure.</p>

#### `PolicyGroup`

``` purescript
newtype PolicyGroup
  = PolicyGroup { "GroupName" :: NullOrUndefined (GroupNameType'), "GroupId" :: NullOrUndefined (IdType') }
```

<p>Contains information about a group that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `PolicyGroupListType`

``` purescript
newtype PolicyGroupListType
  = PolicyGroupListType (Array PolicyGroup)
```

#### `PolicyIdentifierType`

``` purescript
newtype PolicyIdentifierType
  = PolicyIdentifierType String
```

#### `PolicyNotAttachableException`

``` purescript
newtype PolicyNotAttachableException
  = PolicyNotAttachableException { "Message'" :: NullOrUndefined (PolicyNotAttachableMessage') }
```

<p>The request failed because AWS service role policies can only be attached to the service-linked role for that service.</p>

#### `PolicyRole`

``` purescript
newtype PolicyRole
  = PolicyRole { "RoleName" :: NullOrUndefined (RoleNameType'), "RoleId" :: NullOrUndefined (IdType') }
```

<p>Contains information about a role that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `PolicyRoleListType`

``` purescript
newtype PolicyRoleListType
  = PolicyRoleListType (Array PolicyRole)
```

#### `PolicySourceType`

``` purescript
newtype PolicySourceType
  = PolicySourceType String
```

#### `PolicyUser`

``` purescript
newtype PolicyUser
  = PolicyUser { "UserName" :: NullOrUndefined (UserNameType'), "UserId" :: NullOrUndefined (IdType') }
```

<p>Contains information about a user that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `PolicyUserListType`

``` purescript
newtype PolicyUserListType
  = PolicyUserListType (Array PolicyUser)
```

#### `PolicyVersion`

``` purescript
newtype PolicyVersion
  = PolicyVersion { "Document" :: NullOrUndefined (PolicyDocumentType'), "VersionId" :: NullOrUndefined (PolicyVersionIdType'), "IsDefaultVersion" :: NullOrUndefined (BooleanType'), "CreateDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about a version of a managed policy.</p> <p>This data type is used as a response element in the <a>CreatePolicyVersion</a>, <a>GetPolicyVersion</a>, <a>ListPolicyVersions</a>, and <a>GetAccountAuthorizationDetails</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>

#### `Position`

``` purescript
newtype Position
  = Position { "Line" :: NullOrUndefined (LineNumber), "Column" :: NullOrUndefined (ColumnNumber) }
```

<p>Contains the row and column of a location of a <code>Statement</code> element in a policy document.</p> <p>This data type is used as a member of the <code> <a>Statement</a> </code> type.</p>

#### `PutGroupPolicyRequest`

``` purescript
newtype PutGroupPolicyRequest
  = PutGroupPolicyRequest { "GroupName" :: GroupNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

#### `PutRolePolicyRequest`

``` purescript
newtype PutRolePolicyRequest
  = PutRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

#### `PutUserPolicyRequest`

``` purescript
newtype PutUserPolicyRequest
  = PutUserPolicyRequest { "UserName" :: ExistingUserNameType', "PolicyName" :: PolicyNameType', "PolicyDocument" :: PolicyDocumentType' }
```

#### `ReasonType`

``` purescript
newtype ReasonType
  = ReasonType String
```

#### `RegionNameType`

``` purescript
newtype RegionNameType
  = RegionNameType String
```

#### `RemoveClientIDFromOpenIDConnectProviderRequest`

``` purescript
newtype RemoveClientIDFromOpenIDConnectProviderRequest
  = RemoveClientIDFromOpenIDConnectProviderRequest { "OpenIDConnectProviderArn" :: ArnType', "ClientID" :: ClientIDType' }
```

#### `RemoveRoleFromInstanceProfileRequest`

``` purescript
newtype RemoveRoleFromInstanceProfileRequest
  = RemoveRoleFromInstanceProfileRequest { "InstanceProfileName" :: InstanceProfileNameType', "RoleName" :: RoleNameType' }
```

#### `RemoveUserFromGroupRequest`

``` purescript
newtype RemoveUserFromGroupRequest
  = RemoveUserFromGroupRequest { "GroupName" :: GroupNameType', "UserName" :: ExistingUserNameType' }
```

#### `ReportContentType`

``` purescript
newtype ReportContentType
  = ReportContentType String
```

#### `ReportFormatType`

``` purescript
newtype ReportFormatType
  = ReportFormatType String
```

#### `ReportStateDescriptionType`

``` purescript
newtype ReportStateDescriptionType
  = ReportStateDescriptionType String
```

#### `ReportStateType`

``` purescript
newtype ReportStateType
  = ReportStateType String
```

#### `ResetServiceSpecificCredentialRequest`

``` purescript
newtype ResetServiceSpecificCredentialRequest
  = ResetServiceSpecificCredentialRequest { "UserName" :: NullOrUndefined (UserNameType'), "ServiceSpecificCredentialId" :: ServiceSpecificCredentialId' }
```

#### `ResetServiceSpecificCredentialResponse`

``` purescript
newtype ResetServiceSpecificCredentialResponse
  = ResetServiceSpecificCredentialResponse { "ServiceSpecificCredential" :: NullOrUndefined (ServiceSpecificCredential) }
```

#### `ResourceHandlingOptionType`

``` purescript
newtype ResourceHandlingOptionType
  = ResourceHandlingOptionType String
```

#### `ResourceNameListType`

``` purescript
newtype ResourceNameListType
  = ResourceNameListType (Array ResourceNameType)
```

#### `ResourceNameType`

``` purescript
newtype ResourceNameType
  = ResourceNameType String
```

#### `ResourceSpecificResult`

``` purescript
newtype ResourceSpecificResult
  = ResourceSpecificResult { "EvalResourceName" :: ResourceNameType, "EvalResourceDecision" :: PolicyEvaluationDecisionType, "MatchedStatements" :: NullOrUndefined (StatementListType), "MissingContextValues" :: NullOrUndefined (ContextKeyNamesResultListType), "EvalDecisionDetails" :: NullOrUndefined (EvalDecisionDetailsType) }
```

<p>Contains the result of the simulation of a single API action call on a single resource.</p> <p>This data type is used by a member of the <a>EvaluationResult</a> data type.</p>

#### `ResourceSpecificResultListType`

``` purescript
newtype ResourceSpecificResultListType
  = ResourceSpecificResultListType (Array ResourceSpecificResult)
```

#### `ResyncMFADeviceRequest`

``` purescript
newtype ResyncMFADeviceRequest
  = ResyncMFADeviceRequest { "UserName" :: ExistingUserNameType', "SerialNumber" :: SerialNumberType', "AuthenticationCode1" :: AuthenticationCodeType', "AuthenticationCode2" :: AuthenticationCodeType' }
```

#### `Role`

``` purescript
newtype Role
  = Role { "Path" :: PathType', "RoleName" :: RoleNameType', "RoleId" :: IdType', "Arn" :: ArnType', "CreateDate" :: DateType', "AssumeRolePolicyDocument" :: NullOrUndefined (PolicyDocumentType'), "Description" :: NullOrUndefined (RoleDescriptionType') }
```

<p>Contains information about an IAM role. This structure is returned as a response element in several APIs that interact with roles.</p>

#### `RoleDetail`

``` purescript
newtype RoleDetail
  = RoleDetail { "Path" :: NullOrUndefined (PathType'), "RoleName" :: NullOrUndefined (RoleNameType'), "RoleId" :: NullOrUndefined (IdType'), "Arn" :: NullOrUndefined (ArnType'), "CreateDate" :: NullOrUndefined (DateType'), "AssumeRolePolicyDocument" :: NullOrUndefined (PolicyDocumentType'), "InstanceProfileList" :: NullOrUndefined (InstanceProfileListType'), "RolePolicyList" :: NullOrUndefined (PolicyDetailListType'), "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType') }
```

<p>Contains information about an IAM role, including all of the role's policies.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>

#### `RoleUsageListType`

``` purescript
newtype RoleUsageListType
  = RoleUsageListType (Array RoleUsageType)
```

#### `RoleUsageType`

``` purescript
newtype RoleUsageType
  = RoleUsageType { "Region" :: NullOrUndefined (RegionNameType), "Resources" :: NullOrUndefined (ArnListType) }
```

<p>An object that contains details about how a service-linked role is used.</p> <p>This data type is used as a response element in the <a>GetServiceLinkedRoleDeletionStatus</a> operation.</p>

#### `SAMLMetadataDocumentType`

``` purescript
newtype SAMLMetadataDocumentType
  = SAMLMetadataDocumentType String
```

#### `SAMLProviderListEntry`

``` purescript
newtype SAMLProviderListEntry
  = SAMLProviderListEntry { "Arn" :: NullOrUndefined (ArnType'), "ValidUntil" :: NullOrUndefined (DateType'), "CreateDate" :: NullOrUndefined (DateType') }
```

<p>Contains the list of SAML providers for this account.</p>

#### `SAMLProviderListType`

``` purescript
newtype SAMLProviderListType
  = SAMLProviderListType (Array SAMLProviderListEntry)
```

#### `SAMLProviderNameType`

``` purescript
newtype SAMLProviderNameType
  = SAMLProviderNameType String
```

#### `SSHPublicKey`

``` purescript
newtype SSHPublicKey
  = SSHPublicKey { "UserName" :: UserNameType', "SSHPublicKeyId" :: PublicKeyIdType', "Fingerprint" :: PublicKeyFingerprintType', "SSHPublicKeyBody" :: PublicKeyMaterialType', "Status" :: StatusType', "UploadDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about an SSH public key.</p> <p>This data type is used as a response element in the <a>GetSSHPublicKey</a> and <a>UploadSSHPublicKey</a> actions. </p>

#### `SSHPublicKeyListType`

``` purescript
newtype SSHPublicKeyListType
  = SSHPublicKeyListType (Array SSHPublicKeyMetadata)
```

#### `SSHPublicKeyMetadata`

``` purescript
newtype SSHPublicKeyMetadata
  = SSHPublicKeyMetadata { "UserName" :: UserNameType', "SSHPublicKeyId" :: PublicKeyIdType', "Status" :: StatusType', "UploadDate" :: DateType' }
```

<p>Contains information about an SSH public key, without the key's body or fingerprint.</p> <p>This data type is used as a response element in the <a>ListSSHPublicKeys</a> action.</p>

#### `ServerCertificate`

``` purescript
newtype ServerCertificate
  = ServerCertificate { "ServerCertificateMetadata" :: ServerCertificateMetadata, "CertificateBody" :: CertificateBodyType', "CertificateChain" :: NullOrUndefined (CertificateChainType') }
```

<p>Contains information about a server certificate.</p> <p> This data type is used as a response element in the <a>GetServerCertificate</a> action. </p>

#### `ServerCertificateMetadata`

``` purescript
newtype ServerCertificateMetadata
  = ServerCertificateMetadata { "Path" :: PathType', "ServerCertificateName" :: ServerCertificateNameType', "ServerCertificateId" :: IdType', "Arn" :: ArnType', "UploadDate" :: NullOrUndefined (DateType'), "Expiration" :: NullOrUndefined (DateType') }
```

<p>Contains information about a server certificate without its certificate body, certificate chain, and private key.</p> <p> This data type is used as a response element in the <a>UploadServerCertificate</a> and <a>ListServerCertificates</a> actions. </p>

#### `ServiceFailureException`

``` purescript
newtype ServiceFailureException
  = ServiceFailureException { "Message'" :: NullOrUndefined (ServiceFailureExceptionMessage') }
```

<p>The request processing has failed because of an unknown error, exception or failure.</p>

#### `ServiceNotSupportedException`

``` purescript
newtype ServiceNotSupportedException
  = ServiceNotSupportedException { "Message'" :: NullOrUndefined (ServiceNotSupportedMessage') }
```

<p>The specified service does not support service-specific credentials.</p>

#### `ServiceSpecificCredential`

``` purescript
newtype ServiceSpecificCredential
  = ServiceSpecificCredential { "CreateDate" :: DateType', "ServiceName" :: ServiceName', "ServiceUserName" :: ServiceUserName', "ServicePassword" :: ServicePassword', "ServiceSpecificCredentialId" :: ServiceSpecificCredentialId', "UserName" :: UserNameType', "Status" :: StatusType' }
```

<p>Contains the details of a service specific credential.</p>

#### `ServiceSpecificCredentialMetadata`

``` purescript
newtype ServiceSpecificCredentialMetadata
  = ServiceSpecificCredentialMetadata { "UserName" :: UserNameType', "Status" :: StatusType', "ServiceUserName" :: ServiceUserName', "CreateDate" :: DateType', "ServiceSpecificCredentialId" :: ServiceSpecificCredentialId', "ServiceName" :: ServiceName' }
```

<p>Contains additional details about a service-specific credential.</p>

#### `ServiceSpecificCredentialsListType`

``` purescript
newtype ServiceSpecificCredentialsListType
  = ServiceSpecificCredentialsListType (Array ServiceSpecificCredentialMetadata)
```

#### `SetDefaultPolicyVersionRequest`

``` purescript
newtype SetDefaultPolicyVersionRequest
  = SetDefaultPolicyVersionRequest { "PolicyArn" :: ArnType', "VersionId" :: PolicyVersionIdType' }
```

#### `SigningCertificate`

``` purescript
newtype SigningCertificate
  = SigningCertificate { "UserName" :: UserNameType', "CertificateId" :: CertificateIdType', "CertificateBody" :: CertificateBodyType', "Status" :: StatusType', "UploadDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about an X.509 signing certificate.</p> <p>This data type is used as a response element in the <a>UploadSigningCertificate</a> and <a>ListSigningCertificates</a> actions. </p>

#### `SimulateCustomPolicyRequest`

``` purescript
newtype SimulateCustomPolicyRequest
  = SimulateCustomPolicyRequest { "PolicyInputList" :: SimulationPolicyListType, "ActionNames" :: ActionNameListType, "ResourceArns" :: NullOrUndefined (ResourceNameListType), "ResourcePolicy" :: NullOrUndefined (PolicyDocumentType'), "ResourceOwner" :: NullOrUndefined (ResourceNameType), "CallerArn" :: NullOrUndefined (ResourceNameType), "ContextEntries" :: NullOrUndefined (ContextEntryListType), "ResourceHandlingOption" :: NullOrUndefined (ResourceHandlingOptionType), "MaxItems" :: NullOrUndefined (MaxItemsType'), "Marker" :: NullOrUndefined (MarkerType') }
```

#### `SimulatePolicyResponse`

``` purescript
newtype SimulatePolicyResponse
  = SimulatePolicyResponse { "EvaluationResults" :: NullOrUndefined (EvaluationResultsListType), "IsTruncated" :: NullOrUndefined (BooleanType'), "Marker" :: NullOrUndefined (MarkerType') }
```

<p>Contains the response to a successful <a>SimulatePrincipalPolicy</a> or <a>SimulateCustomPolicy</a> request.</p>

#### `SimulatePrincipalPolicyRequest`

``` purescript
newtype SimulatePrincipalPolicyRequest
  = SimulatePrincipalPolicyRequest { "PolicySourceArn" :: ArnType', "PolicyInputList" :: NullOrUndefined (SimulationPolicyListType), "ActionNames" :: ActionNameListType, "ResourceArns" :: NullOrUndefined (ResourceNameListType), "ResourcePolicy" :: NullOrUndefined (PolicyDocumentType'), "ResourceOwner" :: NullOrUndefined (ResourceNameType), "CallerArn" :: NullOrUndefined (ResourceNameType), "ContextEntries" :: NullOrUndefined (ContextEntryListType), "ResourceHandlingOption" :: NullOrUndefined (ResourceHandlingOptionType), "MaxItems" :: NullOrUndefined (MaxItemsType'), "Marker" :: NullOrUndefined (MarkerType') }
```

#### `SimulationPolicyListType`

``` purescript
newtype SimulationPolicyListType
  = SimulationPolicyListType (Array PolicyDocumentType')
```

#### `Statement`

``` purescript
newtype Statement
  = Statement { "SourcePolicyId" :: NullOrUndefined (PolicyIdentifierType), "SourcePolicyType" :: NullOrUndefined (PolicySourceType), "StartPosition" :: NullOrUndefined (Position), "EndPosition" :: NullOrUndefined (Position) }
```

<p>Contains a reference to a <code>Statement</code> element in a policy document that determines the result of the simulation.</p> <p>This data type is used by the <code>MatchedStatements</code> member of the <code> <a>EvaluationResult</a> </code> type.</p>

#### `StatementListType`

``` purescript
newtype StatementListType
  = StatementListType (Array Statement)
```

#### `UnmodifiableEntityException`

``` purescript
newtype UnmodifiableEntityException
  = UnmodifiableEntityException { "Message'" :: NullOrUndefined (UnmodifiableEntityMessage') }
```

<p>The request was rejected because only the service that depends on the service-linked role can modify or delete the role on your behalf. The error message includes the name of the service that depends on this service-linked role. You must request the change through that service.</p>

#### `UnrecognizedPublicKeyEncodingException`

``` purescript
newtype UnrecognizedPublicKeyEncodingException
  = UnrecognizedPublicKeyEncodingException { "Message'" :: NullOrUndefined (UnrecognizedPublicKeyEncodingMessage') }
```

<p>The request was rejected because the public key encoding format is unsupported or unrecognized.</p>

#### `UpdateAccessKeyRequest`

``` purescript
newtype UpdateAccessKeyRequest
  = UpdateAccessKeyRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "AccessKeyId" :: AccessKeyIdType', "Status" :: StatusType' }
```

#### `UpdateAccountPasswordPolicyRequest`

``` purescript
newtype UpdateAccountPasswordPolicyRequest
  = UpdateAccountPasswordPolicyRequest { "MinimumPasswordLength" :: NullOrUndefined (MinimumPasswordLengthType'), "RequireSymbols" :: NullOrUndefined (BooleanType'), "RequireNumbers" :: NullOrUndefined (BooleanType'), "RequireUppercaseCharacters" :: NullOrUndefined (BooleanType'), "RequireLowercaseCharacters" :: NullOrUndefined (BooleanType'), "AllowUsersToChangePassword" :: NullOrUndefined (BooleanType'), "MaxPasswordAge" :: NullOrUndefined (MaxPasswordAgeType'), "PasswordReusePrevention" :: NullOrUndefined (PasswordReusePreventionType'), "HardExpiry" :: NullOrUndefined (BooleanObjectType') }
```

#### `UpdateAssumeRolePolicyRequest`

``` purescript
newtype UpdateAssumeRolePolicyRequest
  = UpdateAssumeRolePolicyRequest { "RoleName" :: RoleNameType', "PolicyDocument" :: PolicyDocumentType' }
```

#### `UpdateGroupRequest`

``` purescript
newtype UpdateGroupRequest
  = UpdateGroupRequest { "GroupName" :: GroupNameType', "NewPath" :: NullOrUndefined (PathType'), "NewGroupName" :: NullOrUndefined (GroupNameType') }
```

#### `UpdateLoginProfileRequest`

``` purescript
newtype UpdateLoginProfileRequest
  = UpdateLoginProfileRequest { "UserName" :: UserNameType', "Password" :: NullOrUndefined (PasswordType'), "PasswordResetRequired" :: NullOrUndefined (BooleanObjectType') }
```

#### `UpdateOpenIDConnectProviderThumbprintRequest`

``` purescript
newtype UpdateOpenIDConnectProviderThumbprintRequest
  = UpdateOpenIDConnectProviderThumbprintRequest { "OpenIDConnectProviderArn" :: ArnType', "ThumbprintList" :: ThumbprintListType' }
```

#### `UpdateRoleDescriptionRequest`

``` purescript
newtype UpdateRoleDescriptionRequest
  = UpdateRoleDescriptionRequest { "RoleName" :: RoleNameType', "Description" :: RoleDescriptionType' }
```

#### `UpdateRoleDescriptionResponse`

``` purescript
newtype UpdateRoleDescriptionResponse
  = UpdateRoleDescriptionResponse { "Role" :: NullOrUndefined (Role) }
```

#### `UpdateSAMLProviderRequest`

``` purescript
newtype UpdateSAMLProviderRequest
  = UpdateSAMLProviderRequest { "SAMLMetadataDocument" :: SAMLMetadataDocumentType, "SAMLProviderArn" :: ArnType' }
```

#### `UpdateSAMLProviderResponse`

``` purescript
newtype UpdateSAMLProviderResponse
  = UpdateSAMLProviderResponse { "SAMLProviderArn" :: NullOrUndefined (ArnType') }
```

<p>Contains the response to a successful <a>UpdateSAMLProvider</a> request. </p>

#### `UpdateSSHPublicKeyRequest`

``` purescript
newtype UpdateSSHPublicKeyRequest
  = UpdateSSHPublicKeyRequest { "UserName" :: UserNameType', "SSHPublicKeyId" :: PublicKeyIdType', "Status" :: StatusType' }
```

#### `UpdateServerCertificateRequest`

``` purescript
newtype UpdateServerCertificateRequest
  = UpdateServerCertificateRequest { "ServerCertificateName" :: ServerCertificateNameType', "NewPath" :: NullOrUndefined (PathType'), "NewServerCertificateName" :: NullOrUndefined (ServerCertificateNameType') }
```

#### `UpdateServiceSpecificCredentialRequest`

``` purescript
newtype UpdateServiceSpecificCredentialRequest
  = UpdateServiceSpecificCredentialRequest { "UserName" :: NullOrUndefined (UserNameType'), "ServiceSpecificCredentialId" :: ServiceSpecificCredentialId', "Status" :: StatusType' }
```

#### `UpdateSigningCertificateRequest`

``` purescript
newtype UpdateSigningCertificateRequest
  = UpdateSigningCertificateRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "CertificateId" :: CertificateIdType', "Status" :: StatusType' }
```

#### `UpdateUserRequest`

``` purescript
newtype UpdateUserRequest
  = UpdateUserRequest { "UserName" :: ExistingUserNameType', "NewPath" :: NullOrUndefined (PathType'), "NewUserName" :: NullOrUndefined (UserNameType') }
```

#### `UploadSSHPublicKeyRequest`

``` purescript
newtype UploadSSHPublicKeyRequest
  = UploadSSHPublicKeyRequest { "UserName" :: UserNameType', "SSHPublicKeyBody" :: PublicKeyMaterialType' }
```

#### `UploadSSHPublicKeyResponse`

``` purescript
newtype UploadSSHPublicKeyResponse
  = UploadSSHPublicKeyResponse { "SSHPublicKey" :: NullOrUndefined (SSHPublicKey) }
```

<p>Contains the response to a successful <a>UploadSSHPublicKey</a> request.</p>

#### `UploadServerCertificateRequest`

``` purescript
newtype UploadServerCertificateRequest
  = UploadServerCertificateRequest { "Path" :: NullOrUndefined (PathType'), "ServerCertificateName" :: ServerCertificateNameType', "CertificateBody" :: CertificateBodyType', "PrivateKey" :: PrivateKeyType', "CertificateChain" :: NullOrUndefined (CertificateChainType') }
```

#### `UploadServerCertificateResponse`

``` purescript
newtype UploadServerCertificateResponse
  = UploadServerCertificateResponse { "ServerCertificateMetadata" :: NullOrUndefined (ServerCertificateMetadata) }
```

<p>Contains the response to a successful <a>UploadServerCertificate</a> request. </p>

#### `UploadSigningCertificateRequest`

``` purescript
newtype UploadSigningCertificateRequest
  = UploadSigningCertificateRequest { "UserName" :: NullOrUndefined (ExistingUserNameType'), "CertificateBody" :: CertificateBodyType' }
```

#### `UploadSigningCertificateResponse`

``` purescript
newtype UploadSigningCertificateResponse
  = UploadSigningCertificateResponse { "Certificate" :: SigningCertificate }
```

<p>Contains the response to a successful <a>UploadSigningCertificate</a> request. </p>

#### `User`

``` purescript
newtype User
  = User { "Path" :: PathType', "UserName" :: UserNameType', "UserId" :: IdType', "Arn" :: ArnType', "CreateDate" :: DateType', "PasswordLastUsed" :: NullOrUndefined (DateType') }
```

<p>Contains information about an IAM user entity.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateUser</a> </p> </li> <li> <p> <a>GetUser</a> </p> </li> <li> <p> <a>ListUsers</a> </p> </li> </ul>

#### `UserDetail`

``` purescript
newtype UserDetail
  = UserDetail { "Path" :: NullOrUndefined (PathType'), "UserName" :: NullOrUndefined (UserNameType'), "UserId" :: NullOrUndefined (IdType'), "Arn" :: NullOrUndefined (ArnType'), "CreateDate" :: NullOrUndefined (DateType'), "UserPolicyList" :: NullOrUndefined (PolicyDetailListType'), "GroupList" :: NullOrUndefined (GroupNameListType'), "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType') }
```

<p>Contains information about an IAM user, including all the user's policies and all the IAM groups the user is in.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>

#### `VirtualMFADevice`

``` purescript
newtype VirtualMFADevice
  = VirtualMFADevice { "SerialNumber" :: SerialNumberType', "Base32StringSeed" :: NullOrUndefined (BootstrapDatum), "QRCodePNG" :: NullOrUndefined (BootstrapDatum), "User" :: NullOrUndefined (User), "EnableDate" :: NullOrUndefined (DateType') }
```

<p>Contains information about a virtual MFA device.</p>

#### `AccessKeyIdType'`

``` purescript
newtype AccessKeyIdType'
  = AccessKeyIdType' String
```

#### `AccessKeyMetadataListType'`

``` purescript
newtype AccessKeyMetadataListType'
  = AccessKeyMetadataListType' (Array AccessKeyMetadata)
```

<p>Contains a list of access key metadata.</p> <p>This data type is used as a response element in the <a>ListAccessKeys</a> action.</p>

#### `AccessKeySecretType'`

``` purescript
newtype AccessKeySecretType'
  = AccessKeySecretType' String
```

#### `AccountAliasListType'`

``` purescript
newtype AccountAliasListType'
  = AccountAliasListType' (Array AccountAliasType')
```

#### `AccountAliasType'`

``` purescript
newtype AccountAliasType'
  = AccountAliasType' String
```

#### `ArnType'`

``` purescript
newtype ArnType'
  = ArnType' String
```

<p>The Amazon Resource Name (ARN). ARNs are unique identifiers for AWS resources.</p> <p>For more information about ARNs, go to <a href="http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html">Amazon Resource Names (ARNs) and AWS Service Namespaces</a> in the <i>AWS General Reference</i>. </p>

#### `AssignmentStatusType'`

``` purescript
newtype AssignmentStatusType'
  = AssignmentStatusType' String
```

#### `AttachedPoliciesListType'`

``` purescript
newtype AttachedPoliciesListType'
  = AttachedPoliciesListType' (Array AttachedPolicy)
```

#### `AttachmentCountType'`

``` purescript
newtype AttachmentCountType'
  = AttachmentCountType' Int
```

#### `AuthenticationCodeType'`

``` purescript
newtype AuthenticationCodeType'
  = AuthenticationCodeType' String
```

#### `BooleanObjectType'`

``` purescript
newtype BooleanObjectType'
  = BooleanObjectType' Boolean
```

#### `BooleanType'`

``` purescript
newtype BooleanType'
  = BooleanType' Boolean
```

#### `CertificateBodyType'`

``` purescript
newtype CertificateBodyType'
  = CertificateBodyType' String
```

#### `CertificateChainType'`

``` purescript
newtype CertificateChainType'
  = CertificateChainType' String
```

#### `CertificateIdType'`

``` purescript
newtype CertificateIdType'
  = CertificateIdType' String
```

#### `CertificateListType'`

``` purescript
newtype CertificateListType'
  = CertificateListType' (Array SigningCertificate)
```

<p>Contains a list of signing certificates.</p> <p>This data type is used as a response element in the <a>ListSigningCertificates</a> action.</p>

#### `ClientIDListType'`

``` purescript
newtype ClientIDListType'
  = ClientIDListType' (Array ClientIDType')
```

#### `ClientIDType'`

``` purescript
newtype ClientIDType'
  = ClientIDType' String
```

#### `CredentialReportExpiredExceptionMessage'`

``` purescript
newtype CredentialReportExpiredExceptionMessage'
  = CredentialReportExpiredExceptionMessage' String
```

#### `CredentialReportNotPresentExceptionMessage'`

``` purescript
newtype CredentialReportNotPresentExceptionMessage'
  = CredentialReportNotPresentExceptionMessage' String
```

#### `CredentialReportNotReadyExceptionMessage'`

``` purescript
newtype CredentialReportNotReadyExceptionMessage'
  = CredentialReportNotReadyExceptionMessage' String
```

#### `CustomSuffixType'`

``` purescript
newtype CustomSuffixType'
  = CustomSuffixType' String
```

#### `DateType'`

``` purescript
newtype DateType'
  = DateType' Number
```

#### `DeleteConflictMessage'`

``` purescript
newtype DeleteConflictMessage'
  = DeleteConflictMessage' String
```

#### `DuplicateCertificateMessage'`

``` purescript
newtype DuplicateCertificateMessage'
  = DuplicateCertificateMessage' String
```

#### `DuplicateSSHPublicKeyMessage'`

``` purescript
newtype DuplicateSSHPublicKeyMessage'
  = DuplicateSSHPublicKeyMessage' String
```

#### `EncodingType'`

``` purescript
newtype EncodingType'
  = EncodingType' String
```

#### `EntityAlreadyExistsMessage'`

``` purescript
newtype EntityAlreadyExistsMessage'
  = EntityAlreadyExistsMessage' String
```

#### `EntityListType'`

``` purescript
newtype EntityListType'
  = EntityListType' (Array EntityType)
```

#### `EntityTemporarilyUnmodifiableMessage'`

``` purescript
newtype EntityTemporarilyUnmodifiableMessage'
  = EntityTemporarilyUnmodifiableMessage' String
```

#### `ExistingUserNameType'`

``` purescript
newtype ExistingUserNameType'
  = ExistingUserNameType' String
```

#### `GroupDetailListType'`

``` purescript
newtype GroupDetailListType'
  = GroupDetailListType' (Array GroupDetail)
```

#### `GroupListType'`

``` purescript
newtype GroupListType'
  = GroupListType' (Array Group)
```

<p>Contains a list of IAM groups.</p> <p>This data type is used as a response element in the <a>ListGroups</a> action.</p>

#### `GroupNameListType'`

``` purescript
newtype GroupNameListType'
  = GroupNameListType' (Array GroupNameType')
```

#### `GroupNameType'`

``` purescript
newtype GroupNameType'
  = GroupNameType' String
```

#### `IdType'`

``` purescript
newtype IdType'
  = IdType' String
```

#### `InstanceProfileListType'`

``` purescript
newtype InstanceProfileListType'
  = InstanceProfileListType' (Array InstanceProfile)
```

<p>Contains a list of instance profiles.</p>

#### `InstanceProfileNameType'`

``` purescript
newtype InstanceProfileNameType'
  = InstanceProfileNameType' String
```

#### `InvalidAuthenticationCodeMessage'`

``` purescript
newtype InvalidAuthenticationCodeMessage'
  = InvalidAuthenticationCodeMessage' String
```

#### `InvalidCertificateMessage'`

``` purescript
newtype InvalidCertificateMessage'
  = InvalidCertificateMessage' String
```

#### `InvalidInputMessage'`

``` purescript
newtype InvalidInputMessage'
  = InvalidInputMessage' String
```

#### `InvalidPublicKeyMessage'`

``` purescript
newtype InvalidPublicKeyMessage'
  = InvalidPublicKeyMessage' String
```

#### `InvalidUserTypeMessage'`

``` purescript
newtype InvalidUserTypeMessage'
  = InvalidUserTypeMessage' String
```

#### `KeyPairMismatchMessage'`

``` purescript
newtype KeyPairMismatchMessage'
  = KeyPairMismatchMessage' String
```

#### `LimitExceededMessage'`

``` purescript
newtype LimitExceededMessage'
  = LimitExceededMessage' String
```

#### `MalformedCertificateMessage'`

``` purescript
newtype MalformedCertificateMessage'
  = MalformedCertificateMessage' String
```

#### `MalformedPolicyDocumentMessage'`

``` purescript
newtype MalformedPolicyDocumentMessage'
  = MalformedPolicyDocumentMessage' String
```

#### `MarkerType'`

``` purescript
newtype MarkerType'
  = MarkerType' String
```

#### `MaxItemsType'`

``` purescript
newtype MaxItemsType'
  = MaxItemsType' Int
```

#### `MaxPasswordAgeType'`

``` purescript
newtype MaxPasswordAgeType'
  = MaxPasswordAgeType' Int
```

#### `MfaDeviceListType'`

``` purescript
newtype MfaDeviceListType'
  = MfaDeviceListType' (Array MFADevice)
```

<p>Contains a list of MFA devices.</p> <p>This data type is used as a response element in the <a>ListMFADevices</a> and <a>ListVirtualMFADevices</a> actions. </p>

#### `MinimumPasswordLengthType'`

``` purescript
newtype MinimumPasswordLengthType'
  = MinimumPasswordLengthType' Int
```

#### `NoSuchEntityMessage'`

``` purescript
newtype NoSuchEntityMessage'
  = NoSuchEntityMessage' String
```

#### `PasswordPolicyViolationMessage'`

``` purescript
newtype PasswordPolicyViolationMessage'
  = PasswordPolicyViolationMessage' String
```

#### `PasswordReusePreventionType'`

``` purescript
newtype PasswordReusePreventionType'
  = PasswordReusePreventionType' Int
```

#### `PasswordType'`

``` purescript
newtype PasswordType'
  = PasswordType' String
```

#### `PathPrefixType'`

``` purescript
newtype PathPrefixType'
  = PathPrefixType' String
```

#### `PathType'`

``` purescript
newtype PathType'
  = PathType' String
```

#### `PolicyDescriptionType'`

``` purescript
newtype PolicyDescriptionType'
  = PolicyDescriptionType' String
```

#### `PolicyDetailListType'`

``` purescript
newtype PolicyDetailListType'
  = PolicyDetailListType' (Array PolicyDetail)
```

#### `PolicyDocumentType'`

``` purescript
newtype PolicyDocumentType'
  = PolicyDocumentType' String
```

#### `PolicyDocumentVersionListType'`

``` purescript
newtype PolicyDocumentVersionListType'
  = PolicyDocumentVersionListType' (Array PolicyVersion)
```

#### `PolicyEvaluationErrorMessage'`

``` purescript
newtype PolicyEvaluationErrorMessage'
  = PolicyEvaluationErrorMessage' String
```

#### `PolicyListType'`

``` purescript
newtype PolicyListType'
  = PolicyListType' (Array Policy)
```

#### `PolicyNameListType'`

``` purescript
newtype PolicyNameListType'
  = PolicyNameListType' (Array PolicyNameType')
```

<p>Contains a list of policy names.</p> <p>This data type is used as a response element in the <a>ListPolicies</a> action.</p>

#### `PolicyNameType'`

``` purescript
newtype PolicyNameType'
  = PolicyNameType' String
```

#### `PolicyNotAttachableMessage'`

``` purescript
newtype PolicyNotAttachableMessage'
  = PolicyNotAttachableMessage' String
```

#### `PolicyPathType'`

``` purescript
newtype PolicyPathType'
  = PolicyPathType' String
```

#### `PolicyScopeType'`

``` purescript
newtype PolicyScopeType'
  = PolicyScopeType' String
```

#### `PolicyVersionIdType'`

``` purescript
newtype PolicyVersionIdType'
  = PolicyVersionIdType' String
```

#### `PrivateKeyType'`

``` purescript
newtype PrivateKeyType'
  = PrivateKeyType' String
```

#### `PublicKeyFingerprintType'`

``` purescript
newtype PublicKeyFingerprintType'
  = PublicKeyFingerprintType' String
```

#### `PublicKeyIdType'`

``` purescript
newtype PublicKeyIdType'
  = PublicKeyIdType' String
```

#### `PublicKeyMaterialType'`

``` purescript
newtype PublicKeyMaterialType'
  = PublicKeyMaterialType' String
```

#### `RoleDescriptionType'`

``` purescript
newtype RoleDescriptionType'
  = RoleDescriptionType' String
```

#### `RoleDetailListType'`

``` purescript
newtype RoleDetailListType'
  = RoleDetailListType' (Array RoleDetail)
```

#### `RoleListType'`

``` purescript
newtype RoleListType'
  = RoleListType' (Array Role)
```

<p>Contains a list of IAM roles.</p> <p>This data type is used as a response element in the <a>ListRoles</a> action.</p>

#### `RoleNameType'`

``` purescript
newtype RoleNameType'
  = RoleNameType' String
```

#### `SerialNumberType'`

``` purescript
newtype SerialNumberType'
  = SerialNumberType' String
```

#### `ServerCertificateMetadataListType'`

``` purescript
newtype ServerCertificateMetadataListType'
  = ServerCertificateMetadataListType' (Array ServerCertificateMetadata)
```

#### `ServerCertificateNameType'`

``` purescript
newtype ServerCertificateNameType'
  = ServerCertificateNameType' String
```

#### `ServiceFailureExceptionMessage'`

``` purescript
newtype ServiceFailureExceptionMessage'
  = ServiceFailureExceptionMessage' String
```

#### `ServiceName'`

``` purescript
newtype ServiceName'
  = ServiceName' String
```

#### `ServiceNotSupportedMessage'`

``` purescript
newtype ServiceNotSupportedMessage'
  = ServiceNotSupportedMessage' String
```

#### `ServicePassword'`

``` purescript
newtype ServicePassword'
  = ServicePassword' String
```

#### `ServiceSpecificCredentialId'`

``` purescript
newtype ServiceSpecificCredentialId'
  = ServiceSpecificCredentialId' String
```

#### `ServiceUserName'`

``` purescript
newtype ServiceUserName'
  = ServiceUserName' String
```

#### `StatusType'`

``` purescript
newtype StatusType'
  = StatusType' String
```

#### `StringType'`

``` purescript
newtype StringType'
  = StringType' String
```

#### `SummaryKeyType'`

``` purescript
newtype SummaryKeyType'
  = SummaryKeyType' String
```

#### `SummaryMapType'`

``` purescript
newtype SummaryMapType'
  = SummaryMapType' (Map SummaryKeyType' SummaryValueType')
```

#### `SummaryValueType'`

``` purescript
newtype SummaryValueType'
  = SummaryValueType' Int
```

#### `ThumbprintListType'`

``` purescript
newtype ThumbprintListType'
  = ThumbprintListType' (Array ThumbprintType')
```

<p>Contains a list of thumbprints of identity provider server certificates.</p>

#### `ThumbprintType'`

``` purescript
newtype ThumbprintType'
  = ThumbprintType' String
```

<p>Contains a thumbprint for an identity provider's server certificate.</p> <p>The identity provider's server certificate thumbprint is the hex-encoded SHA-1 hash value of the self-signed X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.</p>

#### `UnmodifiableEntityMessage'`

``` purescript
newtype UnmodifiableEntityMessage'
  = UnmodifiableEntityMessage' String
```

#### `UnrecognizedPublicKeyEncodingMessage'`

``` purescript
newtype UnrecognizedPublicKeyEncodingMessage'
  = UnrecognizedPublicKeyEncodingMessage' String
```

#### `UserDetailListType'`

``` purescript
newtype UserDetailListType'
  = UserDetailListType' (Array UserDetail)
```

#### `UserListType'`

``` purescript
newtype UserListType'
  = UserListType' (Array User)
```

<p>Contains a list of users.</p> <p>This data type is used as a response element in the <a>GetGroup</a> and <a>ListUsers</a> actions. </p>

#### `UserNameType'`

``` purescript
newtype UserNameType'
  = UserNameType' String
```

#### `VirtualMFADeviceListType'`

``` purescript
newtype VirtualMFADeviceListType'
  = VirtualMFADeviceListType' (Array VirtualMFADevice)
```

#### `VirtualMFADeviceName'`

``` purescript
newtype VirtualMFADeviceName'
  = VirtualMFADeviceName' String
```


