

-- | <fullname>AWS Identity and Access Management</fullname> <p>AWS Identity and Access Management (IAM) is a web service that you can use to manage users and user permissions under your AWS account. This guide provides descriptions of IAM actions that you can call programmatically. For general information about IAM, see <a href="http://aws.amazon.com/iam/">AWS Identity and Access Management (IAM)</a>. For the user guide for IAM, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/">Using IAM</a>. </p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to IAM and AWS. For example, the SDKs take care of tasks such as cryptographically signing requests (see below), managing errors, and retrying requests automatically. For information about the AWS SDKs, including how to download and install them, see the <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a> page. </p> </note> <p>We recommend that you use the AWS SDKs to make programmatic API calls to IAM. However, you can also use the IAM Query API to make direct calls to the IAM web service. To learn more about the IAM Query API, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>Using IAM</i> guide. IAM supports GET and POST requests for all actions. That is, the API does not require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request. </p> <p> <b>Signing Requests</b> </p> <p>Requests must be signed using an access key ID and a secret access key. We strongly recommend that you do not use your AWS account access key ID and secret access key for everyday work with IAM. You can use the access key ID and secret access key for an IAM user or you can use the AWS Security Token Service to generate temporary security credentials and use those to sign requests.</p> <p>To sign requests, we recommend that you use <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>. If you have an existing application that uses Signature Version 2, you do not have to update it to use Signature Version 4. However, some operations now require Signature Version 4. The documentation for operations that require version 4 indicate this requirement. </p> <p> <b>Additional Resources</b> </p> <p>For more information, see the following:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html">AWS Security Credentials</a>. This topic provides general information about the types of credentials used for accessing AWS. </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html">IAM Best Practices</a>. This topic presents a list of suggestions for using the IAM service to help secure your AWS resources. </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a>. This set of topics walk you through the process of signing a request using an access key ID and secret access key. </p> </li> </ul>
module AWS.IAM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "IAM" :: String


-- | <p>Adds a new client ID (also known as audience) to the list of client IDs already registered for the specified IAM OpenID Connect (OIDC) provider resource.</p> <p>This action is idempotent; it does not fail or return an error if you add an existing client ID to the provider.</p>
addClientIDToOpenIDConnectProvider :: forall eff. AddClientIDToOpenIDConnectProviderRequest -> Aff (err :: AWS.RequestError | eff) Unit
addClientIDToOpenIDConnectProvider = AWS.request serviceName "AddClientIDToOpenIDConnectProvider" 


-- | <p>Adds the specified IAM role to the specified instance profile. An instance profile can contain only one role, and this limit cannot be increased.</p> <note> <p>The caller of this API must be granted the <code>PassRole</code> permission on the IAM role by a permission policy.</p> </note> <p>For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>
addRoleToInstanceProfile :: forall eff. AddRoleToInstanceProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
addRoleToInstanceProfile = AWS.request serviceName "AddRoleToInstanceProfile" 


-- | <p>Adds the specified user to the specified group.</p>
addUserToGroup :: forall eff. AddUserToGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
addUserToGroup = AWS.request serviceName "AddUserToGroup" 


-- | <p>Attaches the specified managed policy to the specified IAM group.</p> <p>You use this API to attach a managed policy to a group. To embed an inline policy in a group, use <a>PutGroupPolicy</a>.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
attachGroupPolicy :: forall eff. AttachGroupPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachGroupPolicy = AWS.request serviceName "AttachGroupPolicy" 


-- | <p>Attaches the specified managed policy to the specified IAM role. When you attach a managed policy to a role, the managed policy becomes part of the role's permission (access) policy.</p> <note> <p>You cannot use a managed policy as the role's trust policy. The role's trust policy is created at the same time as the role, using <a>CreateRole</a>. You can update a role's trust policy using <a>UpdateAssumeRolePolicy</a>.</p> </note> <p>Use this API to attach a <i>managed</i> policy to a role. To embed an inline policy in a role, use <a>PutRolePolicy</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
attachRolePolicy :: forall eff. AttachRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachRolePolicy = AWS.request serviceName "AttachRolePolicy" 


-- | <p>Attaches the specified managed policy to the specified user.</p> <p>You use this API to attach a <i>managed</i> policy to a user. To embed an inline policy in a user, use <a>PutUserPolicy</a>.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
attachUserPolicy :: forall eff. AttachUserPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachUserPolicy = AWS.request serviceName "AttachUserPolicy" 


-- | <p>Changes the password of the IAM user who is calling this action. The root account password is not affected by this action.</p> <p>To change the password for a different user, see <a>UpdateLoginProfile</a>. For more information about modifying passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>
changePassword :: forall eff. ChangePasswordRequest -> Aff (err :: AWS.RequestError | eff) Unit
changePassword = AWS.request serviceName "ChangePassword" 


-- | <p> Creates a new AWS secret access key and corresponding AWS access key ID for the specified user. The default status for new keys is <code>Active</code>.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <p> For information about limits on the number of keys you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <important> <p>To ensure the security of your AWS account, the secret access key is accessible only during key and user creation. You must save the key (for example, in a text file) if you want to be able to access it again. If a secret key is lost, you can delete the access keys for the associated user and then create new keys.</p> </important>
createAccessKey :: forall eff. CreateAccessKeyRequest -> Aff (err :: AWS.RequestError | eff) CreateAccessKeyResponse
createAccessKey = AWS.request serviceName "CreateAccessKey" 


-- | <p>Creates an alias for your AWS account. For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>
createAccountAlias :: forall eff. CreateAccountAliasRequest -> Aff (err :: AWS.RequestError | eff) Unit
createAccountAlias = AWS.request serviceName "CreateAccountAlias" 


-- | <p>Creates a new group.</p> <p> For information about the number of groups you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateGroupResponse
createGroup = AWS.request serviceName "CreateGroup" 


-- | <p> Creates a new instance profile. For information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p> For information about the number of instance profiles you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>
createInstanceProfile :: forall eff. CreateInstanceProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateInstanceProfileResponse
createInstanceProfile = AWS.request serviceName "CreateInstanceProfile" 


-- | <p> Creates a password for the specified user, giving the user the ability to access AWS services through the AWS Management Console. For more information about managing passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>
createLoginProfile :: forall eff. CreateLoginProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateLoginProfileResponse
createLoginProfile = AWS.request serviceName "CreateLoginProfile" 


-- | <p>Creates an IAM entity to describe an identity provider (IdP) that supports <a href="http://openid.net/connect/">OpenID Connect (OIDC)</a>.</p> <p>The OIDC provider that you create with this operation can be used as a principal in a role's trust policy to establish a trust relationship between AWS and the OIDC provider.</p> <p>When you create the IAM OIDC provider, you specify the URL of the OIDC identity provider (IdP) to trust, a list of client IDs (also known as audiences) that identify the application or applications that are allowed to authenticate using the OIDC provider, and a list of thumbprints of the server certificate(s) that the IdP uses. You get all of this information from the OIDC IdP that you want to use for access to AWS.</p> <note> <p>Because trust for the OIDC provider is ultimately derived from the IAM provider that this action creates, it is a best practice to limit access to the <a>CreateOpenIDConnectProvider</a> action to highly-privileged users.</p> </note>
createOpenIDConnectProvider :: forall eff. CreateOpenIDConnectProviderRequest -> Aff (err :: AWS.RequestError | eff) CreateOpenIDConnectProviderResponse
createOpenIDConnectProvider = AWS.request serviceName "CreateOpenIDConnectProvider" 


-- | <p>Creates a new managed policy for your AWS account.</p> <p>This operation creates a policy version with a version identifier of <code>v1</code> and sets v1 as the policy's default version. For more information about policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about managed policies in general, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyResponse
createPolicy = AWS.request serviceName "CreatePolicy" 


-- | <p>Creates a new version of the specified managed policy. To update a managed policy, you create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must delete an existing version using <a>DeletePolicyVersion</a> before you create a new version.</p> <p>Optionally, you can set the new version as the policy's default version. The default version is the version that is in effect for the IAM users, groups, and roles to which the policy is attached.</p> <p>For more information about managed policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>
createPolicyVersion :: forall eff. CreatePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyVersionResponse
createPolicyVersion = AWS.request serviceName "CreatePolicyVersion" 


-- | <p>Creates a new role for your AWS account. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For information about limitations on role names and the number of roles you can create, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>
createRole :: forall eff. CreateRoleRequest -> Aff (err :: AWS.RequestError | eff) CreateRoleResponse
createRole = AWS.request serviceName "CreateRole" 


-- | <p>Creates an IAM resource that describes an identity provider (IdP) that supports SAML 2.0.</p> <p>The SAML provider resource that you create with this operation can be used as a principal in an IAM role's trust policy to enable federated users who sign-in using the SAML IdP to assume the role. You can create an IAM role that supports Web-based single sign-on (SSO) to the AWS Management Console or one that supports API access to AWS.</p> <p>When you create the SAML provider resource, you upload an a SAML metadata document that you get from your IdP and that includes the issuer's name, expiration information, and keys that can be used to validate the SAML authentication response (assertions) that the IdP sends. You must generate the metadata document using the identity management software that is used as your organization's IdP.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note> <p> For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-saml.html">Enabling SAML 2.0 Federated Users to Access the AWS Management Console</a> and <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html">About SAML 2.0-based Federation</a> in the <i>IAM User Guide</i>.</p>
createSAMLProvider :: forall eff. CreateSAMLProviderRequest -> Aff (err :: AWS.RequestError | eff) CreateSAMLProviderResponse
createSAMLProvider = AWS.request serviceName "CreateSAMLProvider" 


-- | <p>Creates an IAM role that is linked to a specific AWS service. The service controls the attached policies and when the role can be deleted. This helps ensure that the service is not broken by an unexpectedly changed or deleted role, which could put your AWS resources into an unknown state. Allowing the service to control the role helps improve service stability and proper cleanup when a service and its role are no longer needed.</p> <p>The name of the role is autogenerated by combining the string that you specify for the <code>AWSServiceName</code> parameter with the string that you specify for the <code>CustomSuffix</code> parameter. The resulting name must be unique in your account or the request fails.</p> <p>To attach a policy to this service-linked role, you must make the request using the AWS service that depends on this role.</p>
createServiceLinkedRole :: forall eff. CreateServiceLinkedRoleRequest -> Aff (err :: AWS.RequestError | eff) CreateServiceLinkedRoleResponse
createServiceLinkedRole = AWS.request serviceName "CreateServiceLinkedRole" 


-- | <p>Generates a set of credentials consisting of a user name and password that can be used to access the service specified in the request. These credentials are generated by IAM, and can be used only for the specified service. </p> <p>You can have a maximum of two sets of service-specific credentials for each supported service per user.</p> <p>The only supported service at this time is AWS CodeCommit.</p> <p>You can reset the password to a new service-generated value by calling <a>ResetServiceSpecificCredential</a>.</p> <p>For more information about service-specific credentials, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_ssh-keys.html">Using IAM with AWS CodeCommit: Git Credentials, SSH Keys, and AWS Access Keys</a> in the <i>IAM User Guide</i>.</p>
createServiceSpecificCredential :: forall eff. CreateServiceSpecificCredentialRequest -> Aff (err :: AWS.RequestError | eff) CreateServiceSpecificCredentialResponse
createServiceSpecificCredential = AWS.request serviceName "CreateServiceSpecificCredential" 


-- | <p>Creates a new IAM user for your AWS account.</p> <p> For information about limitations on the number of IAM users you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "CreateUser" 


-- | <p>Creates a new virtual MFA device for the AWS account. After creating the virtual MFA, use <a>EnableMFADevice</a> to attach the MFA device to an IAM user. For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of MFA devices you can create, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on Entities</a> in the <i>IAM User Guide</i>.</p> <important> <p>The seed information contained in the QR code and the Base32 string should be treated like any other secret access information, such as your AWS access keys or your passwords. After you provision your virtual device, you should ensure that the information is destroyed following secure procedures.</p> </important>
createVirtualMFADevice :: forall eff. CreateVirtualMFADeviceRequest -> Aff (err :: AWS.RequestError | eff) CreateVirtualMFADeviceResponse
createVirtualMFADevice = AWS.request serviceName "CreateVirtualMFADevice" 


-- | <p>Deactivates the specified MFA device and removes it from association with the user name for which it was originally enabled.</p> <p>For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p>
deactivateMFADevice :: forall eff. DeactivateMFADeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
deactivateMFADevice = AWS.request serviceName "DeactivateMFADevice" 


-- | <p>Deletes the access key pair associated with the specified IAM user.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>
deleteAccessKey :: forall eff. DeleteAccessKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteAccessKey = AWS.request serviceName "DeleteAccessKey" 


-- | <p> Deletes the specified AWS account alias. For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>
deleteAccountAlias :: forall eff. DeleteAccountAliasRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteAccountAlias = AWS.request serviceName "DeleteAccountAlias" 


-- | <p>Deletes the password policy for the AWS account. There are no parameters.</p>
deleteAccountPasswordPolicy :: forall eff.  Aff (err :: AWS.RequestError | eff) Unit
deleteAccountPasswordPolicy = AWS.request serviceName "DeleteAccountPasswordPolicy" unit


-- | <p>Deletes the specified IAM group. The group must not contain any users or have any attached policies.</p>
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteGroup = AWS.request serviceName "DeleteGroup" 


-- | <p>Deletes the specified inline policy that is embedded in the specified IAM group.</p> <p>A group can also have managed policies attached to it. To detach a managed policy from a group, use <a>DetachGroupPolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
deleteGroupPolicy :: forall eff. DeleteGroupPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteGroupPolicy = AWS.request serviceName "DeleteGroupPolicy" 


-- | <p>Deletes the specified instance profile. The instance profile must not have an associated role.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the instance profile you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.</p> </important> <p>For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>
deleteInstanceProfile :: forall eff. DeleteInstanceProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteInstanceProfile = AWS.request serviceName "DeleteInstanceProfile" 


-- | <p>Deletes the password for the specified IAM user, which terminates the user's ability to access AWS services through the AWS Management Console.</p> <important> <p> Deleting a user's password does not prevent a user from accessing AWS through the command line interface or the API. To prevent all user access you must also either make any access keys inactive or delete them. For more information about making keys inactive or deleting them, see <a>UpdateAccessKey</a> and <a>DeleteAccessKey</a>. </p> </important>
deleteLoginProfile :: forall eff. DeleteLoginProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteLoginProfile = AWS.request serviceName "DeleteLoginProfile" 


-- | <p>Deletes an OpenID Connect identity provider (IdP) resource object in IAM.</p> <p>Deleting an IAM OIDC provider resource does not update any roles that reference the provider as a principal in their trust policies. Any attempt to assume a role that references a deleted provider fails.</p> <p>This action is idempotent; it does not fail or return an error if you call the action for a provider that does not exist.</p>
deleteOpenIDConnectProvider :: forall eff. DeleteOpenIDConnectProviderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteOpenIDConnectProvider = AWS.request serviceName "DeleteOpenIDConnectProvider" 


-- | <p>Deletes the specified managed policy.</p> <p>Before you can delete a managed policy, you must first detach the policy from all users, groups, and roles that it is attached to, and you must delete all of the policy's versions. The following steps describe the process for deleting a managed policy:</p> <ul> <li> <p>Detach the policy from all users, groups, and roles that the policy is attached to, using the <a>DetachUserPolicy</a>, <a>DetachGroupPolicy</a>, or <a>DetachRolePolicy</a> APIs. To list all the users, groups, and roles that a policy is attached to, use <a>ListEntitiesForPolicy</a>.</p> </li> <li> <p>Delete all versions of the policy using <a>DeletePolicyVersion</a>. To list the policy's versions, use <a>ListPolicyVersions</a>. You cannot use <a>DeletePolicyVersion</a> to delete the version that is marked as the default version. You delete the policy's default version in the next step of the process.</p> </li> <li> <p>Delete the policy (this automatically deletes the policy's default version) using this API.</p> </li> </ul> <p>For information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicy = AWS.request serviceName "DeletePolicy" 


-- | <p>Deletes the specified version from the specified managed policy.</p> <p>You cannot delete the default version from a policy using this API. To delete the default version from a policy, use <a>DeletePolicy</a>. To find out which version of a policy is marked as the default version, use <a>ListPolicyVersions</a>.</p> <p>For information about versions for managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>
deletePolicyVersion :: forall eff. DeletePolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicyVersion = AWS.request serviceName "DeletePolicyVersion" 


-- | <p>Deletes the specified role. The role must not have any policies attached. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the role you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.</p> </important>
deleteRole :: forall eff. DeleteRoleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteRole = AWS.request serviceName "DeleteRole" 


-- | <p>Deletes the specified inline policy that is embedded in the specified IAM role.</p> <p>A role can also have managed policies attached to it. To detach a managed policy from a role, use <a>DetachRolePolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
deleteRolePolicy :: forall eff. DeleteRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteRolePolicy = AWS.request serviceName "DeleteRolePolicy" 


-- | <p>Deletes a SAML provider resource in IAM.</p> <p>Deleting the provider resource from IAM does not update any roles that reference the SAML provider resource's ARN as a principal in their trust policies. Any attempt to assume a role that references a non-existent provider resource ARN fails.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>
deleteSAMLProvider :: forall eff. DeleteSAMLProviderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteSAMLProvider = AWS.request serviceName "DeleteSAMLProvider" 


-- | <p>Deletes the specified SSH public key.</p> <p>The SSH public key deleted by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>
deleteSSHPublicKey :: forall eff. DeleteSSHPublicKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteSSHPublicKey = AWS.request serviceName "DeleteSSHPublicKey" 


-- | <p>Deletes the specified server certificate.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <important> <p> If you are using a server certificate with Elastic Load Balancing, deleting the certificate could have implications for your application. If Elastic Load Balancing doesn't detect the deletion of bound certificates, it may continue to use the certificates. This could cause Elastic Load Balancing to stop accepting traffic. We recommend that you remove the reference to the certificate from Elastic Load Balancing before using this command to delete the certificate. For more information, go to <a href="http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeleteLoadBalancerListeners.html">DeleteLoadBalancerListeners</a> in the <i>Elastic Load Balancing API Reference</i>.</p> </important>
deleteServerCertificate :: forall eff. DeleteServerCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteServerCertificate = AWS.request serviceName "DeleteServerCertificate" 


-- | <p>Submits a service-linked role deletion request and returns a <code>DeletionTaskId</code>, which you can use to check the status of the deletion. Before you call this operation, confirm that the role has no active sessions and that any resources used by the role in the linked service are deleted. If you call this operation more than once for the same service-linked role and an earlier deletion task is not complete, then the <code>DeletionTaskId</code> of the earlier request is returned.</p> <p>If you submit a deletion request for a service-linked role whose linked service is still accessing a resource, then the deletion task fails. If it fails, the <a>GetServiceLinkedRoleDeletionStatus</a> API operation returns the reason for the failure, including the resources that must be deleted. To delete the service-linked role, you must first remove those resources from the linked service and then submit the deletion request again. Resources are specific to the service that is linked to the role. For more information about removing resources from a service, see the <a href="http://docs.aws.amazon.com/">AWS documentation</a> for your service.</p> <p>For more information about service-linked roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role">Roles Terms and Concepts: AWS Service-Linked Role</a> in the <i>IAM User Guide</i>.</p>
deleteServiceLinkedRole :: forall eff. DeleteServiceLinkedRoleRequest -> Aff (err :: AWS.RequestError | eff) DeleteServiceLinkedRoleResponse
deleteServiceLinkedRole = AWS.request serviceName "DeleteServiceLinkedRole" 


-- | <p>Deletes the specified service-specific credential.</p>
deleteServiceSpecificCredential :: forall eff. DeleteServiceSpecificCredentialRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteServiceSpecificCredential = AWS.request serviceName "DeleteServiceSpecificCredential" 


-- | <p>Deletes a signing certificate associated with the specified IAM user.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated IAM users.</p>
deleteSigningCertificate :: forall eff. DeleteSigningCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteSigningCertificate = AWS.request serviceName "DeleteSigningCertificate" 


-- | <p>Deletes the specified IAM user. The user must not belong to any groups or have any access keys, signing certificates, or attached policies.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUser = AWS.request serviceName "DeleteUser" 


-- | <p>Deletes the specified inline policy that is embedded in the specified IAM user.</p> <p>A user can also have managed policies attached to it. To detach a managed policy from a user, use <a>DetachUserPolicy</a>. For more information about policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
deleteUserPolicy :: forall eff. DeleteUserPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUserPolicy = AWS.request serviceName "DeleteUserPolicy" 


-- | <p>Deletes a virtual MFA device.</p> <note> <p> You must deactivate a user's virtual MFA device before you can delete it. For information about deactivating MFA devices, see <a>DeactivateMFADevice</a>. </p> </note>
deleteVirtualMFADevice :: forall eff. DeleteVirtualMFADeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteVirtualMFADevice = AWS.request serviceName "DeleteVirtualMFADevice" 


-- | <p>Removes the specified managed policy from the specified IAM group.</p> <p>A group can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteGroupPolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
detachGroupPolicy :: forall eff. DetachGroupPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachGroupPolicy = AWS.request serviceName "DetachGroupPolicy" 


-- | <p>Removes the specified managed policy from the specified role.</p> <p>A role can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteRolePolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
detachRolePolicy :: forall eff. DetachRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachRolePolicy = AWS.request serviceName "DetachRolePolicy" 


-- | <p>Removes the specified managed policy from the specified user.</p> <p>A user can also have inline policies embedded with it. To delete an inline policy, use the <a>DeleteUserPolicy</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
detachUserPolicy :: forall eff. DetachUserPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachUserPolicy = AWS.request serviceName "DetachUserPolicy" 


-- | <p>Enables the specified MFA device and associates it with the specified IAM user. When enabled, the MFA device is required for every subsequent login by the IAM user associated with the device.</p>
enableMFADevice :: forall eff. EnableMFADeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
enableMFADevice = AWS.request serviceName "EnableMFADevice" 


-- | <p> Generates a credential report for the AWS account. For more information about the credential report, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>
generateCredentialReport :: forall eff.  Aff (err :: AWS.RequestError | eff) GenerateCredentialReportResponse
generateCredentialReport = AWS.request serviceName "GenerateCredentialReport" unit


-- | <p>Retrieves information about when the specified access key was last used. The information includes the date and time of last use, along with the AWS service and region that were specified in the last request made with that key.</p>
getAccessKeyLastUsed :: forall eff. GetAccessKeyLastUsedRequest -> Aff (err :: AWS.RequestError | eff) GetAccessKeyLastUsedResponse
getAccessKeyLastUsed = AWS.request serviceName "GetAccessKeyLastUsed" 


-- | <p>Retrieves information about all IAM users, groups, roles, and policies in your AWS account, including their relationships to one another. Use this API to obtain a snapshot of the configuration of IAM permissions (users, groups, roles, and policies) in your account.</p> <p>You can optionally filter the results using the <code>Filter</code> parameter. You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
getAccountAuthorizationDetails :: forall eff. GetAccountAuthorizationDetailsRequest -> Aff (err :: AWS.RequestError | eff) GetAccountAuthorizationDetailsResponse
getAccountAuthorizationDetails = AWS.request serviceName "GetAccountAuthorizationDetails" 


-- | <p>Retrieves the password policy for the AWS account. For more information about using a password policy, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html">Managing an IAM Password Policy</a>.</p>
getAccountPasswordPolicy :: forall eff.  Aff (err :: AWS.RequestError | eff) GetAccountPasswordPolicyResponse
getAccountPasswordPolicy = AWS.request serviceName "GetAccountPasswordPolicy" unit


-- | <p>Retrieves information about IAM entity usage and IAM quotas in the AWS account.</p> <p> For information about limitations on IAM entities, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p>
getAccountSummary :: forall eff.  Aff (err :: AWS.RequestError | eff) GetAccountSummaryResponse
getAccountSummary = AWS.request serviceName "GetAccountSummary" unit


-- | <p>Gets a list of all of the context keys referenced in the input policies. The policies are supplied as a list of one or more strings. To get the context keys from policies associated with an IAM user, group, or role, use <a>GetContextKeysForPrincipalPolicy</a>.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request, and can be evaluated by testing against a value specified in an IAM policy. Use GetContextKeysForCustomPolicy to understand what key names and values you must supply when you call <a>SimulateCustomPolicy</a>. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.</p>
getContextKeysForCustomPolicy :: forall eff. GetContextKeysForCustomPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetContextKeysForPolicyResponse
getContextKeysForCustomPolicy = AWS.request serviceName "GetContextKeysForCustomPolicy" 


-- | <p>Gets a list of all of the context keys referenced in all of the IAM policies attached to the specified IAM entity. The entity can be an IAM user, group, or role. If you specify a user, then the request also includes all of the policies attached to groups that the user is a member of.</p> <p>You can optionally include a list of one or more additional policies, specified as strings. If you want to include <i>only</i> a list of policies by string, use <a>GetContextKeysForCustomPolicy</a> instead.</p> <p> <b>Note:</b> This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use <a>GetContextKeysForCustomPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request, and can be evaluated by testing against a value in an IAM policy. Use <a>GetContextKeysForPrincipalPolicy</a> to understand what key names and values you must supply when you call <a>SimulatePrincipalPolicy</a>.</p>
getContextKeysForPrincipalPolicy :: forall eff. GetContextKeysForPrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetContextKeysForPolicyResponse
getContextKeysForPrincipalPolicy = AWS.request serviceName "GetContextKeysForPrincipalPolicy" 


-- | <p> Retrieves a credential report for the AWS account. For more information about the credential report, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>
getCredentialReport :: forall eff.  Aff (err :: AWS.RequestError | eff) GetCredentialReportResponse
getCredentialReport = AWS.request serviceName "GetCredentialReport" unit


-- | <p> Returns a list of IAM users that are in the specified IAM group. You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
getGroup :: forall eff. GetGroupRequest -> Aff (err :: AWS.RequestError | eff) GetGroupResponse
getGroup = AWS.request serviceName "GetGroup" 


-- | <p>Retrieves the specified inline policy document that is embedded in the specified IAM group.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM group can also have managed policies attached to it. To retrieve a managed policy document that is attached to a group, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
getGroupPolicy :: forall eff. GetGroupPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetGroupPolicyResponse
getGroupPolicy = AWS.request serviceName "GetGroupPolicy" 


-- | <p> Retrieves information about the specified instance profile, including the instance profile's path, GUID, ARN, and role. For more information about instance profiles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a> in the <i>IAM User Guide</i>.</p>
getInstanceProfile :: forall eff. GetInstanceProfileRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceProfileResponse
getInstanceProfile = AWS.request serviceName "GetInstanceProfile" 


-- | <p>Retrieves the user name and password-creation date for the specified IAM user. If the user has not been assigned a password, the action returns a 404 (<code>NoSuchEntity</code>) error.</p>
getLoginProfile :: forall eff. GetLoginProfileRequest -> Aff (err :: AWS.RequestError | eff) GetLoginProfileResponse
getLoginProfile = AWS.request serviceName "GetLoginProfile" 


-- | <p>Returns information about the specified OpenID Connect (OIDC) provider resource object in IAM.</p>
getOpenIDConnectProvider :: forall eff. GetOpenIDConnectProviderRequest -> Aff (err :: AWS.RequestError | eff) GetOpenIDConnectProviderResponse
getOpenIDConnectProvider = AWS.request serviceName "GetOpenIDConnectProvider" 


-- | <p>Retrieves information about the specified managed policy, including the policy's default version and the total number of IAM users, groups, and roles to which the policy is attached. To retrieve the list of the specific users, groups, and roles that the policy is attached to, use the <a>ListEntitiesForPolicy</a> API. This API returns metadata about the policy. To retrieve the actual policy document for a specific version of the policy, use <a>GetPolicyVersion</a>.</p> <p>This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded with an IAM user, group, or role, use the <a>GetUserPolicy</a>, <a>GetGroupPolicy</a>, or <a>GetRolePolicy</a> API.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
getPolicy :: forall eff. GetPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyResponse
getPolicy = AWS.request serviceName "GetPolicy" 


-- | <p>Retrieves information about the specified version of the specified managed policy, including the policy document.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>To list the available versions for a policy, use <a>ListPolicyVersions</a>.</p> <p>This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded in a user, group, or role, use the <a>GetUserPolicy</a>, <a>GetGroupPolicy</a>, or <a>GetRolePolicy</a> API.</p> <p>For more information about the types of policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about managed policy versions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html">Versioning for Managed Policies</a> in the <i>IAM User Guide</i>.</p>
getPolicyVersion :: forall eff. GetPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) GetPolicyVersionResponse
getPolicyVersion = AWS.request serviceName "GetPolicyVersion" 


-- | <p>Retrieves information about the specified role, including the role's path, GUID, ARN, and the role's trust policy that grants permission to assume the role. For more information about roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note>
getRole :: forall eff. GetRoleRequest -> Aff (err :: AWS.RequestError | eff) GetRoleResponse
getRole = AWS.request serviceName "GetRole" 


-- | <p>Retrieves the specified inline policy document that is embedded with the specified IAM role.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM role can also have managed policies attached to it. To retrieve a managed policy document that is attached to a role, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For more information about roles, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p>
getRolePolicy :: forall eff. GetRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) GetRolePolicyResponse
getRolePolicy = AWS.request serviceName "GetRolePolicy" 


-- | <p>Returns the SAML provider metadocument that was uploaded when the IAM SAML provider resource object was created or updated.</p> <note> <p>This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>
getSAMLProvider :: forall eff. GetSAMLProviderRequest -> Aff (err :: AWS.RequestError | eff) GetSAMLProviderResponse
getSAMLProvider = AWS.request serviceName "GetSAMLProvider" 


-- | <p>Retrieves the specified SSH public key, including metadata about the key.</p> <p>The SSH public key retrieved by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>
getSSHPublicKey :: forall eff. GetSSHPublicKeyRequest -> Aff (err :: AWS.RequestError | eff) GetSSHPublicKeyResponse
getSSHPublicKey = AWS.request serviceName "GetSSHPublicKey" 


-- | <p>Retrieves information about the specified server certificate stored in IAM.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p>
getServerCertificate :: forall eff. GetServerCertificateRequest -> Aff (err :: AWS.RequestError | eff) GetServerCertificateResponse
getServerCertificate = AWS.request serviceName "GetServerCertificate" 


-- | <p>Retrieves the status of your service-linked role deletion. After you use the <a>DeleteServiceLinkedRole</a> API operation to submit a service-linked role for deletion, you can use the <code>DeletionTaskId</code> parameter in <code>GetServiceLinkedRoleDeletionStatus</code> to check the status of the deletion. If the deletion fails, this operation returns the reason that it failed.</p>
getServiceLinkedRoleDeletionStatus :: forall eff. GetServiceLinkedRoleDeletionStatusRequest -> Aff (err :: AWS.RequestError | eff) GetServiceLinkedRoleDeletionStatusResponse
getServiceLinkedRoleDeletionStatus = AWS.request serviceName "GetServiceLinkedRoleDeletionStatus" 


-- | <p>Retrieves information about the specified IAM user, including the user's creation date, path, unique ID, and ARN.</p> <p>If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID used to sign the request to this API.</p>
getUser :: forall eff. GetUserRequest -> Aff (err :: AWS.RequestError | eff) GetUserResponse
getUser = AWS.request serviceName "GetUser" 


-- | <p>Retrieves the specified inline policy document that is embedded in the specified IAM user.</p> <note> <p>Policies returned by this API are URL-encoded compliant with <a href="https://tools.ietf.org/html/rfc3986">RFC 3986</a>. You can use a URL decoding method to convert the policy back to plain JSON text. For example, if you use Java, you can use the <code>decode</code> method of the <code>java.net.URLDecoder</code> utility class in the Java SDK. Other languages and SDKs provide similar functionality.</p> </note> <p>An IAM user can also have managed policies attached to it. To retrieve a managed policy document that is attached to a user, use <a>GetPolicy</a> to determine the policy's default version, then use <a>GetPolicyVersion</a> to retrieve the policy document.</p> <p>For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
getUserPolicy :: forall eff. GetUserPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetUserPolicyResponse
getUserPolicy = AWS.request serviceName "GetUserPolicy" 


-- | <p>Returns information about the access key IDs associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>Although each user is limited to a small number of keys, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <note> <p>To ensure the security of your AWS account, the secret access key is accessible only during key and user creation.</p> </note>
listAccessKeys :: forall eff. ListAccessKeysRequest -> Aff (err :: AWS.RequestError | eff) ListAccessKeysResponse
listAccessKeys = AWS.request serviceName "ListAccessKeys" 


-- | <p>Lists the account alias associated with the AWS account (Note: you can have only one). For information about using an AWS account alias, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html">Using an Alias for Your AWS Account ID</a> in the <i>IAM User Guide</i>.</p>
listAccountAliases :: forall eff. ListAccountAliasesRequest -> Aff (err :: AWS.RequestError | eff) ListAccountAliasesResponse
listAccountAliases = AWS.request serviceName "ListAccountAliases" 


-- | <p>Lists all managed policies that are attached to the specified IAM group.</p> <p>An IAM group can also have inline policies embedded with it. To list the inline policies for a group, use the <a>ListGroupPolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the action returns an empty list.</p>
listAttachedGroupPolicies :: forall eff. ListAttachedGroupPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedGroupPoliciesResponse
listAttachedGroupPolicies = AWS.request serviceName "ListAttachedGroupPolicies" 


-- | <p>Lists all managed policies that are attached to the specified IAM role.</p> <p>An IAM role can also have inline policies embedded with it. To list the inline policies for a role, use the <a>ListRolePolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified role (or none that match the specified path prefix), the action returns an empty list.</p>
listAttachedRolePolicies :: forall eff. ListAttachedRolePoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedRolePoliciesResponse
listAttachedRolePolicies = AWS.request serviceName "ListAttachedRolePolicies" 


-- | <p>Lists all managed policies that are attached to the specified IAM user.</p> <p>An IAM user can also have inline policies embedded with it. To list the inline policies for a user, use the <a>ListUserPolicies</a> API. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. You can use the <code>PathPrefix</code> parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the action returns an empty list.</p>
listAttachedUserPolicies :: forall eff. ListAttachedUserPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListAttachedUserPoliciesResponse
listAttachedUserPolicies = AWS.request serviceName "ListAttachedUserPolicies" 


-- | <p>Lists all IAM users, groups, and roles that the specified managed policy is attached to.</p> <p>You can use the optional <code>EntityFilter</code> parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set <code>EntityFilter</code> to <code>Role</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listEntitiesForPolicy :: forall eff. ListEntitiesForPolicyRequest -> Aff (err :: AWS.RequestError | eff) ListEntitiesForPolicyResponse
listEntitiesForPolicy = AWS.request serviceName "ListEntitiesForPolicy" 


-- | <p>Lists the names of the inline policies that are embedded in the specified IAM group.</p> <p>An IAM group can also have managed policies attached to it. To list the managed policies that are attached to a group, use <a>ListAttachedGroupPolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified group, the action returns an empty list.</p>
listGroupPolicies :: forall eff. ListGroupPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListGroupPoliciesResponse
listGroupPolicies = AWS.request serviceName "ListGroupPolicies" 


-- | <p>Lists the IAM groups that have the specified path prefix.</p> <p> You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListGroupsResponse
listGroups = AWS.request serviceName "ListGroups" 


-- | <p>Lists the IAM groups that the specified IAM user belongs to.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listGroupsForUser :: forall eff. ListGroupsForUserRequest -> Aff (err :: AWS.RequestError | eff) ListGroupsForUserResponse
listGroupsForUser = AWS.request serviceName "ListGroupsForUser" 


-- | <p>Lists the instance profiles that have the specified path prefix. If there are none, the action returns an empty list. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listInstanceProfiles :: forall eff. ListInstanceProfilesRequest -> Aff (err :: AWS.RequestError | eff) ListInstanceProfilesResponse
listInstanceProfiles = AWS.request serviceName "ListInstanceProfiles" 


-- | <p>Lists the instance profiles that have the specified associated IAM role. If there are none, the action returns an empty list. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listInstanceProfilesForRole :: forall eff. ListInstanceProfilesForRoleRequest -> Aff (err :: AWS.RequestError | eff) ListInstanceProfilesForRoleResponse
listInstanceProfilesForRole = AWS.request serviceName "ListInstanceProfilesForRole" 


-- | <p>Lists the MFA devices for an IAM user. If the request includes a IAM user name, then this action lists all the MFA devices associated with the specified user. If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request for this API.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listMFADevices :: forall eff. ListMFADevicesRequest -> Aff (err :: AWS.RequestError | eff) ListMFADevicesResponse
listMFADevices = AWS.request serviceName "ListMFADevices" 


-- | <p>Lists information about the IAM OpenID Connect (OIDC) provider resource objects defined in the AWS account.</p>
listOpenIDConnectProviders :: forall eff. ListOpenIDConnectProvidersRequest -> Aff (err :: AWS.RequestError | eff) ListOpenIDConnectProvidersResponse
listOpenIDConnectProviders = AWS.request serviceName "ListOpenIDConnectProviders" 


-- | <p>Lists all the managed policies that are available in your AWS account, including your own customer-defined managed policies and all AWS managed policies.</p> <p>You can filter the list of policies that is returned using the optional <code>OnlyAttached</code>, <code>Scope</code>, and <code>PathPrefix</code> parameters. For example, to list only the customer managed policies in your AWS account, set <code>Scope</code> to <code>Local</code>. To list only AWS managed policies, set <code>Scope</code> to <code>AWS</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPoliciesResponse
listPolicies = AWS.request serviceName "ListPolicies" 


-- | <p>Lists information about the versions of the specified managed policy, including the version that is currently set as the policy's default version.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
listPolicyVersions :: forall eff. ListPolicyVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListPolicyVersionsResponse
listPolicyVersions = AWS.request serviceName "ListPolicyVersions" 


-- | <p>Lists the names of the inline policies that are embedded in the specified IAM role.</p> <p>An IAM role can also have managed policies attached to it. To list the managed policies that are attached to a role, use <a>ListAttachedRolePolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified role, the action returns an empty list.</p>
listRolePolicies :: forall eff. ListRolePoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListRolePoliciesResponse
listRolePolicies = AWS.request serviceName "ListRolePolicies" 


-- | <p>Lists the IAM roles that have the specified path prefix. If there are none, the action returns an empty list. For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listRoles :: forall eff. ListRolesRequest -> Aff (err :: AWS.RequestError | eff) ListRolesResponse
listRoles = AWS.request serviceName "ListRoles" 


-- | <p>Lists the SAML provider resource objects defined in IAM in the account.</p> <note> <p> This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>
listSAMLProviders :: forall eff. ListSAMLProvidersRequest -> Aff (err :: AWS.RequestError | eff) ListSAMLProvidersResponse
listSAMLProviders = AWS.request serviceName "ListSAMLProviders" 


-- | <p>Returns information about the SSH public keys associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>The SSH public keys returned by this action are used only for authenticating the IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p> <p>Although each user is limited to a small number of keys, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listSSHPublicKeys :: forall eff. ListSSHPublicKeysRequest -> Aff (err :: AWS.RequestError | eff) ListSSHPublicKeysResponse
listSSHPublicKeys = AWS.request serviceName "ListSSHPublicKeys" 


-- | <p>Lists the server certificates stored in IAM that have the specified path prefix. If none exist, the action returns an empty list.</p> <p> You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p>
listServerCertificates :: forall eff. ListServerCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListServerCertificatesResponse
listServerCertificates = AWS.request serviceName "ListServerCertificates" 


-- | <p>Returns information about the service-specific credentials associated with the specified IAM user. If there are none, the action returns an empty list. The service-specific credentials returned by this action are used only for authenticating the IAM user to a specific service. For more information about using service-specific credentials to authenticate to an AWS service, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-gc.html">Set Up service-specific credentials</a> in the AWS CodeCommit User Guide.</p>
listServiceSpecificCredentials :: forall eff. ListServiceSpecificCredentialsRequest -> Aff (err :: AWS.RequestError | eff) ListServiceSpecificCredentialsResponse
listServiceSpecificCredentials = AWS.request serviceName "ListServiceSpecificCredentials" 


-- | <p>Returns information about the signing certificates associated with the specified IAM user. If there are none, the action returns an empty list.</p> <p>Although each user is limited to a small number of signing certificates, you can still paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p> <p>If the <code>UserName</code> field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request for this API. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>
listSigningCertificates :: forall eff. ListSigningCertificatesRequest -> Aff (err :: AWS.RequestError | eff) ListSigningCertificatesResponse
listSigningCertificates = AWS.request serviceName "ListSigningCertificates" 


-- | <p>Lists the names of the inline policies embedded in the specified IAM user.</p> <p>An IAM user can also have managed policies attached to it. To list the managed policies that are attached to a user, use <a>ListAttachedUserPolicies</a>. For more information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters. If there are no inline policies embedded with the specified user, the action returns an empty list.</p>
listUserPolicies :: forall eff. ListUserPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListUserPoliciesResponse
listUserPolicies = AWS.request serviceName "ListUserPolicies" 


-- | <p>Lists the IAM users that have the specified path prefix. If no path prefix is specified, the action returns all users in the AWS account. If there are none, the action returns an empty list.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listUsers :: forall eff. ListUsersRequest -> Aff (err :: AWS.RequestError | eff) ListUsersResponse
listUsers = AWS.request serviceName "ListUsers" 


-- | <p>Lists the virtual MFA devices defined in the AWS account by assignment status. If you do not specify an assignment status, the action returns a list of all virtual MFA devices. Assignment status can be <code>Assigned</code>, <code>Unassigned</code>, or <code>Any</code>.</p> <p>You can paginate the results using the <code>MaxItems</code> and <code>Marker</code> parameters.</p>
listVirtualMFADevices :: forall eff. ListVirtualMFADevicesRequest -> Aff (err :: AWS.RequestError | eff) ListVirtualMFADevicesResponse
listVirtualMFADevices = AWS.request serviceName "ListVirtualMFADevices" 


-- | <p>Adds or updates an inline policy document that is embedded in the specified IAM group.</p> <p>A user can also have managed policies attached to it. To attach a managed policy to a group, use <a>AttachGroupPolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed in a group, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutGroupPolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>
putGroupPolicy :: forall eff. PutGroupPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putGroupPolicy = AWS.request serviceName "PutGroupPolicy" 


-- | <p>Adds or updates an inline policy document that is embedded in the specified IAM role.</p> <p>When you embed an inline policy in a role, the inline policy is used as part of the role's access (permissions) policy. The role's trust policy is created at the same time as the role, using <a>CreateRole</a>. You can update a role's trust policy using <a>UpdateAssumeRolePolicy</a>. For more information about IAM roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p> <p>A role can also have a managed policy attached to it. To attach a managed policy to a role, use <a>AttachRolePolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed with a role, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutRolePolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>
putRolePolicy :: forall eff. PutRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putRolePolicy = AWS.request serviceName "PutRolePolicy" 


-- | <p>Adds or updates an inline policy document that is embedded in the specified IAM user.</p> <p>An IAM user can also have a managed policy attached to it. To attach a managed policy to a user, use <a>AttachUserPolicy</a>. To create a new managed policy, use <a>CreatePolicy</a>. For information about policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p> <p>For information about limits on the number of inline policies that you can embed in a user, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html">Limitations on IAM Entities</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because policy documents can be large, you should use POST rather than GET when calling <code>PutUserPolicy</code>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>
putUserPolicy :: forall eff. PutUserPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putUserPolicy = AWS.request serviceName "PutUserPolicy" 


-- | <p>Removes the specified client ID (also known as audience) from the list of client IDs registered for the specified IAM OpenID Connect (OIDC) provider resource object.</p> <p>This action is idempotent; it does not fail or return an error if you try to remove a client ID that does not exist.</p>
removeClientIDFromOpenIDConnectProvider :: forall eff. RemoveClientIDFromOpenIDConnectProviderRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeClientIDFromOpenIDConnectProvider = AWS.request serviceName "RemoveClientIDFromOpenIDConnectProvider" 


-- | <p>Removes the specified IAM role from the specified EC2 instance profile.</p> <important> <p>Make sure you do not have any Amazon EC2 instances running with the role you are about to remove from the instance profile. Removing a role from an instance profile that is associated with a running instance might break any applications running on the instance.</p> </important> <p> For more information about IAM roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html">Working with Roles</a>. For more information about instance profiles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html">About Instance Profiles</a>.</p>
removeRoleFromInstanceProfile :: forall eff. RemoveRoleFromInstanceProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeRoleFromInstanceProfile = AWS.request serviceName "RemoveRoleFromInstanceProfile" 


-- | <p>Removes the specified user from the specified group.</p>
removeUserFromGroup :: forall eff. RemoveUserFromGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeUserFromGroup = AWS.request serviceName "RemoveUserFromGroup" 


-- | <p>Resets the password for a service-specific credential. The new password is AWS generated and cryptographically strong. It cannot be configured by the user. Resetting the password immediately invalidates the previous password associated with this user.</p>
resetServiceSpecificCredential :: forall eff. ResetServiceSpecificCredentialRequest -> Aff (err :: AWS.RequestError | eff) ResetServiceSpecificCredentialResponse
resetServiceSpecificCredential = AWS.request serviceName "ResetServiceSpecificCredential" 


-- | <p>Synchronizes the specified MFA device with its IAM resource object on the AWS servers.</p> <p>For more information about creating and working with virtual MFA devices, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html">Using a Virtual MFA Device</a> in the <i>IAM User Guide</i>.</p>
resyncMFADevice :: forall eff. ResyncMFADeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
resyncMFADevice = AWS.request serviceName "ResyncMFADevice" 


-- | <p>Sets the specified version of the specified policy as the policy's default (operative) version.</p> <p>This action affects all users, groups, and roles that the policy is attached to. To list the users, groups, and roles that the policy is attached to, use the <a>ListEntitiesForPolicy</a> API.</p> <p>For information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>IAM User Guide</i>.</p>
setDefaultPolicyVersion :: forall eff. SetDefaultPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
setDefaultPolicyVersion = AWS.request serviceName "SetDefaultPolicyVersion" 


-- | <p>Simulate how a set of IAM policies and optionally a resource-based policy works with a list of API actions and AWS resources to determine the policies' effective permissions. The policies are provided as strings.</p> <p>The simulation does not perform the API actions; it only checks the authorization to determine if the simulated policies allow or deny the actions.</p> <p>If you want to simulate existing policies attached to an IAM user, group, or role, use <a>SimulatePrincipalPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the <code>Condition</code> element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use <a>GetContextKeysForCustomPolicy</a>.</p> <p>If the output is long, you can use <code>MaxItems</code> and <code>Marker</code> parameters to paginate the results.</p>
simulateCustomPolicy :: forall eff. SimulateCustomPolicyRequest -> Aff (err :: AWS.RequestError | eff) SimulatePolicyResponse
simulateCustomPolicy = AWS.request serviceName "SimulateCustomPolicy" 


-- | <p>Simulate how a set of IAM policies attached to an IAM entity works with a list of API actions and AWS resources to determine the policies' effective permissions. The entity can be an IAM user, group, or role. If you specify a user, then the simulation also includes all of the policies that are attached to groups that the user belongs to .</p> <p>You can optionally include a list of one or more additional policies specified as strings to include in the simulation. If you want to simulate only policies specified as strings, use <a>SimulateCustomPolicy</a> instead.</p> <p>You can also optionally include one resource-based policy to be evaluated with each of the resources included in the simulation.</p> <p>The simulation does not perform the API actions, it only checks the authorization to determine if the simulated policies allow or deny the actions.</p> <p> <b>Note:</b> This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use <a>SimulateCustomPolicy</a> instead.</p> <p>Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. You can use the <code>Condition</code> element of an IAM policy to evaluate context keys. To get the list of context keys that the policies require for correct simulation, use <a>GetContextKeysForPrincipalPolicy</a>.</p> <p>If the output is long, you can use the <code>MaxItems</code> and <code>Marker</code> parameters to paginate the results.</p>
simulatePrincipalPolicy :: forall eff. SimulatePrincipalPolicyRequest -> Aff (err :: AWS.RequestError | eff) SimulatePolicyResponse
simulatePrincipalPolicy = AWS.request serviceName "SimulatePrincipalPolicy" 


-- | <p>Changes the status of the specified access key from Active to Inactive, or vice versa. This action can be used to disable a user's key as part of a key rotation work flow.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <p>For information about rotating keys, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html">Managing Keys and Certificates</a> in the <i>IAM User Guide</i>.</p>
updateAccessKey :: forall eff. UpdateAccessKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateAccessKey = AWS.request serviceName "UpdateAccessKey" 


-- | <p>Updates the password policy settings for the AWS account.</p> <note> <p>This action does not support partial updates. No parameters are required, but if you do not specify a parameter, that parameter's value reverts to its default value. See the <b>Request Parameters</b> section for each parameter's default value.</p> </note> <p> For more information about using a password policy, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html">Managing an IAM Password Policy</a> in the <i>IAM User Guide</i>.</p>
updateAccountPasswordPolicy :: forall eff. UpdateAccountPasswordPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateAccountPasswordPolicy = AWS.request serviceName "UpdateAccountPasswordPolicy" 


-- | <p>Updates the policy that grants an IAM entity permission to assume a role. This is typically referred to as the "role trust policy". For more information about roles, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html">Using Roles to Delegate Permissions and Federate Identities</a>.</p>
updateAssumeRolePolicy :: forall eff. UpdateAssumeRolePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateAssumeRolePolicy = AWS.request serviceName "UpdateAssumeRolePolicy" 


-- | <p>Updates the name and/or the path of the specified IAM group.</p> <important> <p> You should understand the implications of changing a group's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html">Renaming Users and Groups</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p>To change an IAM group name the requester must have appropriate permissions on both the source object and the target object. For example, to change "Managers" to "MGRs", the entity making the request must have permission on both "Managers" and "MGRs", or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html">Permissions and Policies</a>. </p> </note>
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateGroup = AWS.request serviceName "UpdateGroup" 


-- | <p>Changes the password for the specified IAM user.</p> <p>IAM users can change their own passwords by calling <a>ChangePassword</a>. For more information about modifying passwords, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html">Managing Passwords</a> in the <i>IAM User Guide</i>.</p>
updateLoginProfile :: forall eff. UpdateLoginProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateLoginProfile = AWS.request serviceName "UpdateLoginProfile" 


-- | <p>Replaces the existing list of server certificate thumbprints associated with an OpenID Connect (OIDC) provider resource object with a new list of thumbprints.</p> <p>The list that you pass with this action completely replaces the existing list of thumbprints. (The lists are not merged.)</p> <p>Typically, you need to update a thumbprint only when the identity provider's certificate changes, which occurs rarely. However, if the provider's certificate <i>does</i> change, any attempt to assume an IAM role that specifies the OIDC provider as a principal fails until the certificate thumbprint is updated.</p> <note> <p>Because trust for the OIDC provider is ultimately derived from the provider's certificate and is validated by the thumbprint, it is a best practice to limit access to the <code>UpdateOpenIDConnectProviderThumbprint</code> action to highly-privileged users.</p> </note>
updateOpenIDConnectProviderThumbprint :: forall eff. UpdateOpenIDConnectProviderThumbprintRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateOpenIDConnectProviderThumbprint = AWS.request serviceName "UpdateOpenIDConnectProviderThumbprint" 


-- | <p>Modifies the description of a role.</p>
updateRoleDescription :: forall eff. UpdateRoleDescriptionRequest -> Aff (err :: AWS.RequestError | eff) UpdateRoleDescriptionResponse
updateRoleDescription = AWS.request serviceName "UpdateRoleDescription" 


-- | <p>Updates the metadata document for an existing SAML provider resource object.</p> <note> <p>This operation requires <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>.</p> </note>
updateSAMLProvider :: forall eff. UpdateSAMLProviderRequest -> Aff (err :: AWS.RequestError | eff) UpdateSAMLProviderResponse
updateSAMLProvider = AWS.request serviceName "UpdateSAMLProvider" 


-- | <p>Sets the status of an IAM user's SSH public key to active or inactive. SSH public keys that are inactive cannot be used for authentication. This action can be used to disable a user's SSH public key as part of a key rotation work flow.</p> <p>The SSH public key affected by this action is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>
updateSSHPublicKey :: forall eff. UpdateSSHPublicKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateSSHPublicKey = AWS.request serviceName "UpdateSSHPublicKey" 


-- | <p>Updates the name and/or the path of the specified server certificate stored in IAM.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <important> <p>You should understand the implications of changing a server certificate's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs_manage.html#RenamingServerCerts">Renaming a Server Certificate</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p>To change a server certificate name the requester must have appropriate permissions on both the source object and the target object. For example, to change the name from "ProductionCert" to "ProdCert", the entity making the request must have permission on "ProductionCert" and "ProdCert", or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html">Access Management</a> in the <i>IAM User Guide</i>.</p> </note>
updateServerCertificate :: forall eff. UpdateServerCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateServerCertificate = AWS.request serviceName "UpdateServerCertificate" 


-- | <p>Sets the status of a service-specific credential to <code>Active</code> or <code>Inactive</code>. Service-specific credentials that are inactive cannot be used for authentication to the service. This action can be used to disable a user’s service-specific credential as part of a credential rotation work flow.</p>
updateServiceSpecificCredential :: forall eff. UpdateServiceSpecificCredentialRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateServiceSpecificCredential = AWS.request serviceName "UpdateServiceSpecificCredential" 


-- | <p>Changes the status of the specified user signing certificate from active to disabled, or vice versa. This action can be used to disable an IAM user's signing certificate as part of a certificate rotation work flow.</p> <p>If the <code>UserName</code> field is not specified, the UserName is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p>
updateSigningCertificate :: forall eff. UpdateSigningCertificateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateSigningCertificate = AWS.request serviceName "UpdateSigningCertificate" 


-- | <p>Updates the name and/or the path of the specified IAM user.</p> <important> <p> You should understand the implications of changing an IAM user's path or name. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_renaming">Renaming an IAM User</a> and <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_groups_manage_rename.html">Renaming an IAM Group</a> in the <i>IAM User Guide</i>.</p> </important> <note> <p> To change a user name the requester must have appropriate permissions on both the source object and the target object. For example, to change Bob to Robert, the entity making the request must have permission on Bob and Robert, or must have permission on all (*). For more information about permissions, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html">Permissions and Policies</a>. </p> </note>
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateUser = AWS.request serviceName "UpdateUser" 


-- | <p>Uploads an SSH public key and associates it with the specified IAM user.</p> <p>The SSH public key uploaded by this action can be used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <a href="http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html">Set up AWS CodeCommit for SSH Connections</a> in the <i>AWS CodeCommit User Guide</i>.</p>
uploadSSHPublicKey :: forall eff. UploadSSHPublicKeyRequest -> Aff (err :: AWS.RequestError | eff) UploadSSHPublicKeyResponse
uploadSSHPublicKey = AWS.request serviceName "UploadSSHPublicKey" 


-- | <p>Uploads a server certificate entity for the AWS account. The server certificate entity includes a public key certificate, a private key, and an optional certificate chain, which should all be PEM-encoded.</p> <p>We recommend that you use <a href="https://aws.amazon.com/certificate-manager/">AWS Certificate Manager</a> to provision, manage, and deploy your server certificates. With ACM you can request a certificate, deploy it to AWS resources, and let ACM handle certificate renewals for you. Certificates provided by ACM are free. For more information about using ACM, see the <a href="http://docs.aws.amazon.com/acm/latest/userguide/">AWS Certificate Manager User Guide</a>.</p> <p>For more information about working with server certificates, including a list of AWS services that can use the server certificates that you manage with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html">Working with Server Certificates</a> in the <i>IAM User Guide</i>.</p> <p>For information about the number of server certificates you can upload, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html">Limitations on IAM Entities and Objects</a> in the <i>IAM User Guide</i>.</p> <note> <p>Because the body of the public key certificate, private key, and the certificate chain can be large, you should use POST rather than GET when calling <code>UploadServerCertificate</code>. For information about setting up signatures and authorization through the API, go to <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a> in the <i>AWS General Reference</i>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/programming.html">Calling the API by Making HTTP Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>
uploadServerCertificate :: forall eff. UploadServerCertificateRequest -> Aff (err :: AWS.RequestError | eff) UploadServerCertificateResponse
uploadServerCertificate = AWS.request serviceName "UploadServerCertificate" 


-- | <p>Uploads an X.509 signing certificate and associates it with the specified IAM user. Some AWS services use X.509 signing certificates to validate requests that are signed with a corresponding private key. When you upload the certificate, its default status is <code>Active</code>.</p> <p>If the <code>UserName</code> field is not specified, the IAM user name is determined implicitly based on the AWS access key ID used to sign the request. Because this action works for access keys under the AWS account, you can use this action to manage root credentials even if the AWS account has no associated users.</p> <note> <p>Because the body of a X.509 certificate can be large, you should use POST rather than GET when calling <code>UploadSigningCertificate</code>. For information about setting up signatures and authorization through the API, go to <a href="http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html">Signing AWS API Requests</a> in the <i>AWS General Reference</i>. For general information about using the Query API with IAM, go to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html">Making Query Requests</a> in the <i>IAM User Guide</i>.</p> </note>
uploadSigningCertificate :: forall eff. UploadSigningCertificateRequest -> Aff (err :: AWS.RequestError | eff) UploadSigningCertificateResponse
uploadSigningCertificate = AWS.request serviceName "UploadSigningCertificate" 


-- | <p>Contains information about an AWS access key.</p> <p> This data type is used as a response element in the <a>CreateAccessKey</a> and <a>ListAccessKeys</a> actions. </p> <note> <p>The <code>SecretAccessKey</code> value is returned only in response to <a>CreateAccessKey</a>. You can get a secret access key only when you first create an access key; you cannot recover the secret access key later. If you lose a secret access key, you must create a new access key.</p> </note>
newtype AccessKey = AccessKey 
  { "UserName" :: (UserNameType')
  , "AccessKeyId" :: (AccessKeyIdType')
  , "Status" :: (StatusType')
  , "SecretAccessKey" :: (AccessKeySecretType')
  , "CreateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeAccessKey :: Newtype AccessKey _


-- | <p>Contains information about the last time an AWS access key was used.</p> <p>This data type is used as a response element in the <a>GetAccessKeyLastUsed</a> action.</p>
newtype AccessKeyLastUsed = AccessKeyLastUsed 
  { "LastUsedDate" :: (DateType')
  , "ServiceName" :: (StringType')
  , "Region" :: (StringType')
  }
derive instance newtypeAccessKeyLastUsed :: Newtype AccessKeyLastUsed _


-- | <p>Contains information about an AWS access key, without its secret key.</p> <p>This data type is used as a response element in the <a>ListAccessKeys</a> action.</p>
newtype AccessKeyMetadata = AccessKeyMetadata 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "AccessKeyId" :: NullOrUndefined (AccessKeyIdType')
  , "Status" :: NullOrUndefined (StatusType')
  , "CreateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeAccessKeyMetadata :: Newtype AccessKeyMetadata _


newtype ActionNameListType = ActionNameListType (Array ActionNameType)
derive instance newtypeActionNameListType :: Newtype ActionNameListType _


newtype ActionNameType = ActionNameType String
derive instance newtypeActionNameType :: Newtype ActionNameType _


newtype AddClientIDToOpenIDConnectProviderRequest = AddClientIDToOpenIDConnectProviderRequest 
  { "OpenIDConnectProviderArn" :: (ArnType')
  , "ClientID" :: (ClientIDType')
  }
derive instance newtypeAddClientIDToOpenIDConnectProviderRequest :: Newtype AddClientIDToOpenIDConnectProviderRequest _


newtype AddRoleToInstanceProfileRequest = AddRoleToInstanceProfileRequest 
  { "InstanceProfileName" :: (InstanceProfileNameType')
  , "RoleName" :: (RoleNameType')
  }
derive instance newtypeAddRoleToInstanceProfileRequest :: Newtype AddRoleToInstanceProfileRequest _


newtype AddUserToGroupRequest = AddUserToGroupRequest 
  { "GroupName" :: (GroupNameType')
  , "UserName" :: (ExistingUserNameType')
  }
derive instance newtypeAddUserToGroupRequest :: Newtype AddUserToGroupRequest _


newtype ArnListType = ArnListType (Array ArnType')
derive instance newtypeArnListType :: Newtype ArnListType _


newtype AttachGroupPolicyRequest = AttachGroupPolicyRequest 
  { "GroupName" :: (GroupNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeAttachGroupPolicyRequest :: Newtype AttachGroupPolicyRequest _


newtype AttachRolePolicyRequest = AttachRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeAttachRolePolicyRequest :: Newtype AttachRolePolicyRequest _


newtype AttachUserPolicyRequest = AttachUserPolicyRequest 
  { "UserName" :: (UserNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeAttachUserPolicyRequest :: Newtype AttachUserPolicyRequest _


-- | <p>Contains information about an attached policy.</p> <p>An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the <a>ListAttachedGroupPolicies</a>, <a>ListAttachedRolePolicies</a>, <a>ListAttachedUserPolicies</a>, and <a>GetAccountAuthorizationDetails</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype AttachedPolicy = AttachedPolicy 
  { "PolicyName" :: NullOrUndefined (PolicyNameType')
  , "PolicyArn" :: NullOrUndefined (ArnType')
  }
derive instance newtypeAttachedPolicy :: Newtype AttachedPolicy _


newtype BootstrapDatum = BootstrapDatum String
derive instance newtypeBootstrapDatum :: Newtype BootstrapDatum _


newtype ChangePasswordRequest = ChangePasswordRequest 
  { "OldPassword" :: (PasswordType')
  , "NewPassword" :: (PasswordType')
  }
derive instance newtypeChangePasswordRequest :: Newtype ChangePasswordRequest _


newtype ColumnNumber = ColumnNumber Int
derive instance newtypeColumnNumber :: Newtype ColumnNumber _


-- | <p>Contains information about a condition context key. It includes the name of the key and specifies the value (or values, if the context key supports multiple values) to use in the simulation. This information is used when evaluating the <code>Condition</code> elements of the input policies.</p> <p>This data type is used as an input parameter to <code> <a>SimulateCustomPolicy</a> </code> and <code> <a>SimulateCustomPolicy</a> </code>.</p>
newtype ContextEntry = ContextEntry 
  { "ContextKeyName" :: NullOrUndefined (ContextKeyNameType)
  , "ContextKeyValues" :: NullOrUndefined (ContextKeyValueListType)
  , "ContextKeyType" :: NullOrUndefined (ContextKeyTypeEnum)
  }
derive instance newtypeContextEntry :: Newtype ContextEntry _


newtype ContextEntryListType = ContextEntryListType (Array ContextEntry)
derive instance newtypeContextEntryListType :: Newtype ContextEntryListType _


newtype ContextKeyNameType = ContextKeyNameType String
derive instance newtypeContextKeyNameType :: Newtype ContextKeyNameType _


newtype ContextKeyNamesResultListType = ContextKeyNamesResultListType (Array ContextKeyNameType)
derive instance newtypeContextKeyNamesResultListType :: Newtype ContextKeyNamesResultListType _


newtype ContextKeyTypeEnum = ContextKeyTypeEnum String
derive instance newtypeContextKeyTypeEnum :: Newtype ContextKeyTypeEnum _


newtype ContextKeyValueListType = ContextKeyValueListType (Array ContextKeyValueType)
derive instance newtypeContextKeyValueListType :: Newtype ContextKeyValueListType _


newtype ContextKeyValueType = ContextKeyValueType String
derive instance newtypeContextKeyValueType :: Newtype ContextKeyValueType _


newtype CreateAccessKeyRequest = CreateAccessKeyRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  }
derive instance newtypeCreateAccessKeyRequest :: Newtype CreateAccessKeyRequest _


-- | <p>Contains the response to a successful <a>CreateAccessKey</a> request. </p>
newtype CreateAccessKeyResponse = CreateAccessKeyResponse 
  { "AccessKey" :: (AccessKey)
  }
derive instance newtypeCreateAccessKeyResponse :: Newtype CreateAccessKeyResponse _


newtype CreateAccountAliasRequest = CreateAccountAliasRequest 
  { "AccountAlias" :: (AccountAliasType')
  }
derive instance newtypeCreateAccountAliasRequest :: Newtype CreateAccountAliasRequest _


newtype CreateGroupRequest = CreateGroupRequest 
  { "Path" :: NullOrUndefined (PathType')
  , "GroupName" :: (GroupNameType')
  }
derive instance newtypeCreateGroupRequest :: Newtype CreateGroupRequest _


-- | <p>Contains the response to a successful <a>CreateGroup</a> request. </p>
newtype CreateGroupResponse = CreateGroupResponse 
  { "Group" :: (Group)
  }
derive instance newtypeCreateGroupResponse :: Newtype CreateGroupResponse _


newtype CreateInstanceProfileRequest = CreateInstanceProfileRequest 
  { "InstanceProfileName" :: (InstanceProfileNameType')
  , "Path" :: NullOrUndefined (PathType')
  }
derive instance newtypeCreateInstanceProfileRequest :: Newtype CreateInstanceProfileRequest _


-- | <p>Contains the response to a successful <a>CreateInstanceProfile</a> request. </p>
newtype CreateInstanceProfileResponse = CreateInstanceProfileResponse 
  { "InstanceProfile" :: (InstanceProfile)
  }
derive instance newtypeCreateInstanceProfileResponse :: Newtype CreateInstanceProfileResponse _


newtype CreateLoginProfileRequest = CreateLoginProfileRequest 
  { "UserName" :: (UserNameType')
  , "Password" :: (PasswordType')
  , "PasswordResetRequired" :: NullOrUndefined (BooleanType')
  }
derive instance newtypeCreateLoginProfileRequest :: Newtype CreateLoginProfileRequest _


-- | <p>Contains the response to a successful <a>CreateLoginProfile</a> request. </p>
newtype CreateLoginProfileResponse = CreateLoginProfileResponse 
  { "LoginProfile" :: (LoginProfile)
  }
derive instance newtypeCreateLoginProfileResponse :: Newtype CreateLoginProfileResponse _


newtype CreateOpenIDConnectProviderRequest = CreateOpenIDConnectProviderRequest 
  { "Url" :: (OpenIDConnectProviderUrlType)
  , "ClientIDList" :: NullOrUndefined (ClientIDListType')
  , "ThumbprintList" :: (ThumbprintListType')
  }
derive instance newtypeCreateOpenIDConnectProviderRequest :: Newtype CreateOpenIDConnectProviderRequest _


-- | <p>Contains the response to a successful <a>CreateOpenIDConnectProvider</a> request. </p>
newtype CreateOpenIDConnectProviderResponse = CreateOpenIDConnectProviderResponse 
  { "OpenIDConnectProviderArn" :: NullOrUndefined (ArnType')
  }
derive instance newtypeCreateOpenIDConnectProviderResponse :: Newtype CreateOpenIDConnectProviderResponse _


newtype CreatePolicyRequest = CreatePolicyRequest 
  { "PolicyName" :: (PolicyNameType')
  , "Path" :: NullOrUndefined (PolicyPathType')
  , "PolicyDocument" :: (PolicyDocumentType')
  , "Description" :: NullOrUndefined (PolicyDescriptionType')
  }
derive instance newtypeCreatePolicyRequest :: Newtype CreatePolicyRequest _


-- | <p>Contains the response to a successful <a>CreatePolicy</a> request. </p>
newtype CreatePolicyResponse = CreatePolicyResponse 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeCreatePolicyResponse :: Newtype CreatePolicyResponse _


newtype CreatePolicyVersionRequest = CreatePolicyVersionRequest 
  { "PolicyArn" :: (ArnType')
  , "PolicyDocument" :: (PolicyDocumentType')
  , "SetAsDefault" :: NullOrUndefined (BooleanType')
  }
derive instance newtypeCreatePolicyVersionRequest :: Newtype CreatePolicyVersionRequest _


-- | <p>Contains the response to a successful <a>CreatePolicyVersion</a> request. </p>
newtype CreatePolicyVersionResponse = CreatePolicyVersionResponse 
  { "PolicyVersion" :: NullOrUndefined (PolicyVersion)
  }
derive instance newtypeCreatePolicyVersionResponse :: Newtype CreatePolicyVersionResponse _


newtype CreateRoleRequest = CreateRoleRequest 
  { "Path" :: NullOrUndefined (PathType')
  , "RoleName" :: (RoleNameType')
  , "AssumeRolePolicyDocument" :: (PolicyDocumentType')
  , "Description" :: NullOrUndefined (RoleDescriptionType')
  }
derive instance newtypeCreateRoleRequest :: Newtype CreateRoleRequest _


-- | <p>Contains the response to a successful <a>CreateRole</a> request. </p>
newtype CreateRoleResponse = CreateRoleResponse 
  { "Role" :: (Role)
  }
derive instance newtypeCreateRoleResponse :: Newtype CreateRoleResponse _


newtype CreateSAMLProviderRequest = CreateSAMLProviderRequest 
  { "SAMLMetadataDocument" :: (SAMLMetadataDocumentType)
  , "Name" :: (SAMLProviderNameType)
  }
derive instance newtypeCreateSAMLProviderRequest :: Newtype CreateSAMLProviderRequest _


-- | <p>Contains the response to a successful <a>CreateSAMLProvider</a> request. </p>
newtype CreateSAMLProviderResponse = CreateSAMLProviderResponse 
  { "SAMLProviderArn" :: NullOrUndefined (ArnType')
  }
derive instance newtypeCreateSAMLProviderResponse :: Newtype CreateSAMLProviderResponse _


newtype CreateServiceLinkedRoleRequest = CreateServiceLinkedRoleRequest 
  { "AWSServiceName" :: (GroupNameType')
  , "Description" :: NullOrUndefined (RoleDescriptionType')
  , "CustomSuffix" :: NullOrUndefined (CustomSuffixType')
  }
derive instance newtypeCreateServiceLinkedRoleRequest :: Newtype CreateServiceLinkedRoleRequest _


newtype CreateServiceLinkedRoleResponse = CreateServiceLinkedRoleResponse 
  { "Role" :: NullOrUndefined (Role)
  }
derive instance newtypeCreateServiceLinkedRoleResponse :: Newtype CreateServiceLinkedRoleResponse _


newtype CreateServiceSpecificCredentialRequest = CreateServiceSpecificCredentialRequest 
  { "UserName" :: (UserNameType')
  , "ServiceName" :: (ServiceName')
  }
derive instance newtypeCreateServiceSpecificCredentialRequest :: Newtype CreateServiceSpecificCredentialRequest _


newtype CreateServiceSpecificCredentialResponse = CreateServiceSpecificCredentialResponse 
  { "ServiceSpecificCredential" :: NullOrUndefined (ServiceSpecificCredential)
  }
derive instance newtypeCreateServiceSpecificCredentialResponse :: Newtype CreateServiceSpecificCredentialResponse _


newtype CreateUserRequest = CreateUserRequest 
  { "Path" :: NullOrUndefined (PathType')
  , "UserName" :: (UserNameType')
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _


-- | <p>Contains the response to a successful <a>CreateUser</a> request. </p>
newtype CreateUserResponse = CreateUserResponse 
  { "User" :: NullOrUndefined (User)
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _


newtype CreateVirtualMFADeviceRequest = CreateVirtualMFADeviceRequest 
  { "Path" :: NullOrUndefined (PathType')
  , "VirtualMFADeviceName" :: (VirtualMFADeviceName')
  }
derive instance newtypeCreateVirtualMFADeviceRequest :: Newtype CreateVirtualMFADeviceRequest _


-- | <p>Contains the response to a successful <a>CreateVirtualMFADevice</a> request. </p>
newtype CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse 
  { "VirtualMFADevice" :: (VirtualMFADevice)
  }
derive instance newtypeCreateVirtualMFADeviceResponse :: Newtype CreateVirtualMFADeviceResponse _


-- | <p>The request was rejected because the most recent credential report has expired. To generate a new credential report, use <a>GenerateCredentialReport</a>. For more information about credential report expiration, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html">Getting Credential Reports</a> in the <i>IAM User Guide</i>.</p>
newtype CredentialReportExpiredException = CredentialReportExpiredException 
  { "Message'" :: NullOrUndefined (CredentialReportExpiredExceptionMessage')
  }
derive instance newtypeCredentialReportExpiredException :: Newtype CredentialReportExpiredException _


-- | <p>The request was rejected because the credential report does not exist. To generate a credential report, use <a>GenerateCredentialReport</a>.</p>
newtype CredentialReportNotPresentException = CredentialReportNotPresentException 
  { "Message'" :: NullOrUndefined (CredentialReportNotPresentExceptionMessage')
  }
derive instance newtypeCredentialReportNotPresentException :: Newtype CredentialReportNotPresentException _


-- | <p>The request was rejected because the credential report is still being generated.</p>
newtype CredentialReportNotReadyException = CredentialReportNotReadyException 
  { "Message'" :: NullOrUndefined (CredentialReportNotReadyExceptionMessage')
  }
derive instance newtypeCredentialReportNotReadyException :: Newtype CredentialReportNotReadyException _


newtype DeactivateMFADeviceRequest = DeactivateMFADeviceRequest 
  { "UserName" :: (ExistingUserNameType')
  , "SerialNumber" :: (SerialNumberType')
  }
derive instance newtypeDeactivateMFADeviceRequest :: Newtype DeactivateMFADeviceRequest _


newtype DeleteAccessKeyRequest = DeleteAccessKeyRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "AccessKeyId" :: (AccessKeyIdType')
  }
derive instance newtypeDeleteAccessKeyRequest :: Newtype DeleteAccessKeyRequest _


newtype DeleteAccountAliasRequest = DeleteAccountAliasRequest 
  { "AccountAlias" :: (AccountAliasType')
  }
derive instance newtypeDeleteAccountAliasRequest :: Newtype DeleteAccountAliasRequest _


-- | <p>The request was rejected because it attempted to delete a resource that has attached subordinate entities. The error message describes these entities.</p>
newtype DeleteConflictException = DeleteConflictException 
  { "Message'" :: NullOrUndefined (DeleteConflictMessage')
  }
derive instance newtypeDeleteConflictException :: Newtype DeleteConflictException _


newtype DeleteGroupPolicyRequest = DeleteGroupPolicyRequest 
  { "GroupName" :: (GroupNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeDeleteGroupPolicyRequest :: Newtype DeleteGroupPolicyRequest _


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "GroupName" :: (GroupNameType')
  }
derive instance newtypeDeleteGroupRequest :: Newtype DeleteGroupRequest _


newtype DeleteInstanceProfileRequest = DeleteInstanceProfileRequest 
  { "InstanceProfileName" :: (InstanceProfileNameType')
  }
derive instance newtypeDeleteInstanceProfileRequest :: Newtype DeleteInstanceProfileRequest _


newtype DeleteLoginProfileRequest = DeleteLoginProfileRequest 
  { "UserName" :: (UserNameType')
  }
derive instance newtypeDeleteLoginProfileRequest :: Newtype DeleteLoginProfileRequest _


newtype DeleteOpenIDConnectProviderRequest = DeleteOpenIDConnectProviderRequest 
  { "OpenIDConnectProviderArn" :: (ArnType')
  }
derive instance newtypeDeleteOpenIDConnectProviderRequest :: Newtype DeleteOpenIDConnectProviderRequest _


newtype DeletePolicyRequest = DeletePolicyRequest 
  { "PolicyArn" :: (ArnType')
  }
derive instance newtypeDeletePolicyRequest :: Newtype DeletePolicyRequest _


newtype DeletePolicyVersionRequest = DeletePolicyVersionRequest 
  { "PolicyArn" :: (ArnType')
  , "VersionId" :: (PolicyVersionIdType')
  }
derive instance newtypeDeletePolicyVersionRequest :: Newtype DeletePolicyVersionRequest _


newtype DeleteRolePolicyRequest = DeleteRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeDeleteRolePolicyRequest :: Newtype DeleteRolePolicyRequest _


newtype DeleteRoleRequest = DeleteRoleRequest 
  { "RoleName" :: (RoleNameType')
  }
derive instance newtypeDeleteRoleRequest :: Newtype DeleteRoleRequest _


newtype DeleteSAMLProviderRequest = DeleteSAMLProviderRequest 
  { "SAMLProviderArn" :: (ArnType')
  }
derive instance newtypeDeleteSAMLProviderRequest :: Newtype DeleteSAMLProviderRequest _


newtype DeleteSSHPublicKeyRequest = DeleteSSHPublicKeyRequest 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyId" :: (PublicKeyIdType')
  }
derive instance newtypeDeleteSSHPublicKeyRequest :: Newtype DeleteSSHPublicKeyRequest _


newtype DeleteServerCertificateRequest = DeleteServerCertificateRequest 
  { "ServerCertificateName" :: (ServerCertificateNameType')
  }
derive instance newtypeDeleteServerCertificateRequest :: Newtype DeleteServerCertificateRequest _


newtype DeleteServiceLinkedRoleRequest = DeleteServiceLinkedRoleRequest 
  { "RoleName" :: (RoleNameType')
  }
derive instance newtypeDeleteServiceLinkedRoleRequest :: Newtype DeleteServiceLinkedRoleRequest _


newtype DeleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse 
  { "DeletionTaskId" :: (DeletionTaskIdType)
  }
derive instance newtypeDeleteServiceLinkedRoleResponse :: Newtype DeleteServiceLinkedRoleResponse _


newtype DeleteServiceSpecificCredentialRequest = DeleteServiceSpecificCredentialRequest 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "ServiceSpecificCredentialId" :: (ServiceSpecificCredentialId')
  }
derive instance newtypeDeleteServiceSpecificCredentialRequest :: Newtype DeleteServiceSpecificCredentialRequest _


newtype DeleteSigningCertificateRequest = DeleteSigningCertificateRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "CertificateId" :: (CertificateIdType')
  }
derive instance newtypeDeleteSigningCertificateRequest :: Newtype DeleteSigningCertificateRequest _


newtype DeleteUserPolicyRequest = DeleteUserPolicyRequest 
  { "UserName" :: (ExistingUserNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeDeleteUserPolicyRequest :: Newtype DeleteUserPolicyRequest _


newtype DeleteUserRequest = DeleteUserRequest 
  { "UserName" :: (ExistingUserNameType')
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _


newtype DeleteVirtualMFADeviceRequest = DeleteVirtualMFADeviceRequest 
  { "SerialNumber" :: (SerialNumberType')
  }
derive instance newtypeDeleteVirtualMFADeviceRequest :: Newtype DeleteVirtualMFADeviceRequest _


-- | <p>The reason that the service-linked role deletion failed.</p> <p>This data type is used as a response element in the <a>GetServiceLinkedRoleDeletionStatus</a> operation.</p>
newtype DeletionTaskFailureReasonType = DeletionTaskFailureReasonType 
  { "Reason" :: NullOrUndefined (ReasonType)
  , "RoleUsageList" :: NullOrUndefined (RoleUsageListType)
  }
derive instance newtypeDeletionTaskFailureReasonType :: Newtype DeletionTaskFailureReasonType _


newtype DeletionTaskIdType = DeletionTaskIdType String
derive instance newtypeDeletionTaskIdType :: Newtype DeletionTaskIdType _


newtype DeletionTaskStatusType = DeletionTaskStatusType String
derive instance newtypeDeletionTaskStatusType :: Newtype DeletionTaskStatusType _


newtype DetachGroupPolicyRequest = DetachGroupPolicyRequest 
  { "GroupName" :: (GroupNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeDetachGroupPolicyRequest :: Newtype DetachGroupPolicyRequest _


newtype DetachRolePolicyRequest = DetachRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeDetachRolePolicyRequest :: Newtype DetachRolePolicyRequest _


newtype DetachUserPolicyRequest = DetachUserPolicyRequest 
  { "UserName" :: (UserNameType')
  , "PolicyArn" :: (ArnType')
  }
derive instance newtypeDetachUserPolicyRequest :: Newtype DetachUserPolicyRequest _


-- | <p>The request was rejected because the same certificate is associated with an IAM user in the account.</p>
newtype DuplicateCertificateException = DuplicateCertificateException 
  { "Message'" :: NullOrUndefined (DuplicateCertificateMessage')
  }
derive instance newtypeDuplicateCertificateException :: Newtype DuplicateCertificateException _


-- | <p>The request was rejected because the SSH public key is already associated with the specified IAM user.</p>
newtype DuplicateSSHPublicKeyException = DuplicateSSHPublicKeyException 
  { "Message'" :: NullOrUndefined (DuplicateSSHPublicKeyMessage')
  }
derive instance newtypeDuplicateSSHPublicKeyException :: Newtype DuplicateSSHPublicKeyException _


newtype EnableMFADeviceRequest = EnableMFADeviceRequest 
  { "UserName" :: (ExistingUserNameType')
  , "SerialNumber" :: (SerialNumberType')
  , "AuthenticationCode1" :: (AuthenticationCodeType')
  , "AuthenticationCode2" :: (AuthenticationCodeType')
  }
derive instance newtypeEnableMFADeviceRequest :: Newtype EnableMFADeviceRequest _


-- | <p>The request was rejected because it attempted to create a resource that already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message'" :: NullOrUndefined (EntityAlreadyExistsMessage')
  }
derive instance newtypeEntityAlreadyExistsException :: Newtype EntityAlreadyExistsException _


-- | <p>The request was rejected because it referenced an entity that is temporarily unmodifiable, such as a user name that was deleted and then recreated. The error indicates that the request is likely to succeed if you try again after waiting several minutes. The error message describes the entity.</p>
newtype EntityTemporarilyUnmodifiableException = EntityTemporarilyUnmodifiableException 
  { "Message'" :: NullOrUndefined (EntityTemporarilyUnmodifiableMessage')
  }
derive instance newtypeEntityTemporarilyUnmodifiableException :: Newtype EntityTemporarilyUnmodifiableException _


newtype EntityType = EntityType String
derive instance newtypeEntityType :: Newtype EntityType _


newtype EvalDecisionDetailsType = EvalDecisionDetailsType (Map EvalDecisionSourceType PolicyEvaluationDecisionType)
derive instance newtypeEvalDecisionDetailsType :: Newtype EvalDecisionDetailsType _


newtype EvalDecisionSourceType = EvalDecisionSourceType String
derive instance newtypeEvalDecisionSourceType :: Newtype EvalDecisionSourceType _


-- | <p>Contains the results of a simulation.</p> <p>This data type is used by the return parameter of <code> <a>SimulateCustomPolicy</a> </code> and <code> <a>SimulatePrincipalPolicy</a> </code>.</p>
newtype EvaluationResult = EvaluationResult 
  { "EvalActionName" :: (ActionNameType)
  , "EvalResourceName" :: NullOrUndefined (ResourceNameType)
  , "EvalDecision" :: (PolicyEvaluationDecisionType)
  , "MatchedStatements" :: NullOrUndefined (StatementListType)
  , "MissingContextValues" :: NullOrUndefined (ContextKeyNamesResultListType)
  , "OrganizationsDecisionDetail" :: NullOrUndefined (OrganizationsDecisionDetail)
  , "EvalDecisionDetails" :: NullOrUndefined (EvalDecisionDetailsType)
  , "ResourceSpecificResults" :: NullOrUndefined (ResourceSpecificResultListType)
  }
derive instance newtypeEvaluationResult :: Newtype EvaluationResult _


newtype EvaluationResultsListType = EvaluationResultsListType (Array EvaluationResult)
derive instance newtypeEvaluationResultsListType :: Newtype EvaluationResultsListType _


-- | <p>Contains the response to a successful <a>GenerateCredentialReport</a> request. </p>
newtype GenerateCredentialReportResponse = GenerateCredentialReportResponse 
  { "State" :: NullOrUndefined (ReportStateType)
  , "Description" :: NullOrUndefined (ReportStateDescriptionType)
  }
derive instance newtypeGenerateCredentialReportResponse :: Newtype GenerateCredentialReportResponse _


newtype GetAccessKeyLastUsedRequest = GetAccessKeyLastUsedRequest 
  { "AccessKeyId" :: (AccessKeyIdType')
  }
derive instance newtypeGetAccessKeyLastUsedRequest :: Newtype GetAccessKeyLastUsedRequest _


-- | <p>Contains the response to a successful <a>GetAccessKeyLastUsed</a> request. It is also returned as a member of the <a>AccessKeyMetaData</a> structure returned by the <a>ListAccessKeys</a> action.</p>
newtype GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "AccessKeyLastUsed" :: NullOrUndefined (AccessKeyLastUsed)
  }
derive instance newtypeGetAccessKeyLastUsedResponse :: Newtype GetAccessKeyLastUsedResponse _


newtype GetAccountAuthorizationDetailsRequest = GetAccountAuthorizationDetailsRequest 
  { "Filter" :: NullOrUndefined (EntityListType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeGetAccountAuthorizationDetailsRequest :: Newtype GetAccountAuthorizationDetailsRequest _


-- | <p>Contains the response to a successful <a>GetAccountAuthorizationDetails</a> request. </p>
newtype GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse 
  { "UserDetailList" :: NullOrUndefined (UserDetailListType')
  , "GroupDetailList" :: NullOrUndefined (GroupDetailListType')
  , "RoleDetailList" :: NullOrUndefined (RoleDetailListType')
  , "Policies" :: NullOrUndefined (ManagedPolicyDetailListType)
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeGetAccountAuthorizationDetailsResponse :: Newtype GetAccountAuthorizationDetailsResponse _


-- | <p>Contains the response to a successful <a>GetAccountPasswordPolicy</a> request. </p>
newtype GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse 
  { "PasswordPolicy" :: (PasswordPolicy)
  }
derive instance newtypeGetAccountPasswordPolicyResponse :: Newtype GetAccountPasswordPolicyResponse _


-- | <p>Contains the response to a successful <a>GetAccountSummary</a> request. </p>
newtype GetAccountSummaryResponse = GetAccountSummaryResponse 
  { "SummaryMap" :: NullOrUndefined (SummaryMapType')
  }
derive instance newtypeGetAccountSummaryResponse :: Newtype GetAccountSummaryResponse _


newtype GetContextKeysForCustomPolicyRequest = GetContextKeysForCustomPolicyRequest 
  { "PolicyInputList" :: (SimulationPolicyListType)
  }
derive instance newtypeGetContextKeysForCustomPolicyRequest :: Newtype GetContextKeysForCustomPolicyRequest _


-- | <p>Contains the response to a successful <a>GetContextKeysForPrincipalPolicy</a> or <a>GetContextKeysForCustomPolicy</a> request. </p>
newtype GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse 
  { "ContextKeyNames" :: NullOrUndefined (ContextKeyNamesResultListType)
  }
derive instance newtypeGetContextKeysForPolicyResponse :: Newtype GetContextKeysForPolicyResponse _


newtype GetContextKeysForPrincipalPolicyRequest = GetContextKeysForPrincipalPolicyRequest 
  { "PolicySourceArn" :: (ArnType')
  , "PolicyInputList" :: NullOrUndefined (SimulationPolicyListType)
  }
derive instance newtypeGetContextKeysForPrincipalPolicyRequest :: Newtype GetContextKeysForPrincipalPolicyRequest _


-- | <p>Contains the response to a successful <a>GetCredentialReport</a> request. </p>
newtype GetCredentialReportResponse = GetCredentialReportResponse 
  { "Content" :: NullOrUndefined (ReportContentType)
  , "ReportFormat" :: NullOrUndefined (ReportFormatType)
  , "GeneratedTime" :: NullOrUndefined (DateType')
  }
derive instance newtypeGetCredentialReportResponse :: Newtype GetCredentialReportResponse _


newtype GetGroupPolicyRequest = GetGroupPolicyRequest 
  { "GroupName" :: (GroupNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeGetGroupPolicyRequest :: Newtype GetGroupPolicyRequest _


-- | <p>Contains the response to a successful <a>GetGroupPolicy</a> request. </p>
newtype GetGroupPolicyResponse = GetGroupPolicyResponse 
  { "GroupName" :: (GroupNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypeGetGroupPolicyResponse :: Newtype GetGroupPolicyResponse _


newtype GetGroupRequest = GetGroupRequest 
  { "GroupName" :: (GroupNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeGetGroupRequest :: Newtype GetGroupRequest _


-- | <p>Contains the response to a successful <a>GetGroup</a> request. </p>
newtype GetGroupResponse = GetGroupResponse 
  { "Group" :: (Group)
  , "Users" :: (UserListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeGetGroupResponse :: Newtype GetGroupResponse _


newtype GetInstanceProfileRequest = GetInstanceProfileRequest 
  { "InstanceProfileName" :: (InstanceProfileNameType')
  }
derive instance newtypeGetInstanceProfileRequest :: Newtype GetInstanceProfileRequest _


-- | <p>Contains the response to a successful <a>GetInstanceProfile</a> request. </p>
newtype GetInstanceProfileResponse = GetInstanceProfileResponse 
  { "InstanceProfile" :: (InstanceProfile)
  }
derive instance newtypeGetInstanceProfileResponse :: Newtype GetInstanceProfileResponse _


newtype GetLoginProfileRequest = GetLoginProfileRequest 
  { "UserName" :: (UserNameType')
  }
derive instance newtypeGetLoginProfileRequest :: Newtype GetLoginProfileRequest _


-- | <p>Contains the response to a successful <a>GetLoginProfile</a> request. </p>
newtype GetLoginProfileResponse = GetLoginProfileResponse 
  { "LoginProfile" :: (LoginProfile)
  }
derive instance newtypeGetLoginProfileResponse :: Newtype GetLoginProfileResponse _


newtype GetOpenIDConnectProviderRequest = GetOpenIDConnectProviderRequest 
  { "OpenIDConnectProviderArn" :: (ArnType')
  }
derive instance newtypeGetOpenIDConnectProviderRequest :: Newtype GetOpenIDConnectProviderRequest _


-- | <p>Contains the response to a successful <a>GetOpenIDConnectProvider</a> request. </p>
newtype GetOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse 
  { "Url" :: NullOrUndefined (OpenIDConnectProviderUrlType)
  , "ClientIDList" :: NullOrUndefined (ClientIDListType')
  , "ThumbprintList" :: NullOrUndefined (ThumbprintListType')
  , "CreateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeGetOpenIDConnectProviderResponse :: Newtype GetOpenIDConnectProviderResponse _


newtype GetPolicyRequest = GetPolicyRequest 
  { "PolicyArn" :: (ArnType')
  }
derive instance newtypeGetPolicyRequest :: Newtype GetPolicyRequest _


-- | <p>Contains the response to a successful <a>GetPolicy</a> request. </p>
newtype GetPolicyResponse = GetPolicyResponse 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeGetPolicyResponse :: Newtype GetPolicyResponse _


newtype GetPolicyVersionRequest = GetPolicyVersionRequest 
  { "PolicyArn" :: (ArnType')
  , "VersionId" :: (PolicyVersionIdType')
  }
derive instance newtypeGetPolicyVersionRequest :: Newtype GetPolicyVersionRequest _


-- | <p>Contains the response to a successful <a>GetPolicyVersion</a> request. </p>
newtype GetPolicyVersionResponse = GetPolicyVersionResponse 
  { "PolicyVersion" :: NullOrUndefined (PolicyVersion)
  }
derive instance newtypeGetPolicyVersionResponse :: Newtype GetPolicyVersionResponse _


newtype GetRolePolicyRequest = GetRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeGetRolePolicyRequest :: Newtype GetRolePolicyRequest _


-- | <p>Contains the response to a successful <a>GetRolePolicy</a> request. </p>
newtype GetRolePolicyResponse = GetRolePolicyResponse 
  { "RoleName" :: (RoleNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypeGetRolePolicyResponse :: Newtype GetRolePolicyResponse _


newtype GetRoleRequest = GetRoleRequest 
  { "RoleName" :: (RoleNameType')
  }
derive instance newtypeGetRoleRequest :: Newtype GetRoleRequest _


-- | <p>Contains the response to a successful <a>GetRole</a> request. </p>
newtype GetRoleResponse = GetRoleResponse 
  { "Role" :: (Role)
  }
derive instance newtypeGetRoleResponse :: Newtype GetRoleResponse _


newtype GetSAMLProviderRequest = GetSAMLProviderRequest 
  { "SAMLProviderArn" :: (ArnType')
  }
derive instance newtypeGetSAMLProviderRequest :: Newtype GetSAMLProviderRequest _


-- | <p>Contains the response to a successful <a>GetSAMLProvider</a> request. </p>
newtype GetSAMLProviderResponse = GetSAMLProviderResponse 
  { "SAMLMetadataDocument" :: NullOrUndefined (SAMLMetadataDocumentType)
  , "CreateDate" :: NullOrUndefined (DateType')
  , "ValidUntil" :: NullOrUndefined (DateType')
  }
derive instance newtypeGetSAMLProviderResponse :: Newtype GetSAMLProviderResponse _


newtype GetSSHPublicKeyRequest = GetSSHPublicKeyRequest 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyId" :: (PublicKeyIdType')
  , "Encoding" :: (EncodingType')
  }
derive instance newtypeGetSSHPublicKeyRequest :: Newtype GetSSHPublicKeyRequest _


-- | <p>Contains the response to a successful <a>GetSSHPublicKey</a> request.</p>
newtype GetSSHPublicKeyResponse = GetSSHPublicKeyResponse 
  { "SSHPublicKey" :: NullOrUndefined (SSHPublicKey)
  }
derive instance newtypeGetSSHPublicKeyResponse :: Newtype GetSSHPublicKeyResponse _


newtype GetServerCertificateRequest = GetServerCertificateRequest 
  { "ServerCertificateName" :: (ServerCertificateNameType')
  }
derive instance newtypeGetServerCertificateRequest :: Newtype GetServerCertificateRequest _


-- | <p>Contains the response to a successful <a>GetServerCertificate</a> request. </p>
newtype GetServerCertificateResponse = GetServerCertificateResponse 
  { "ServerCertificate" :: (ServerCertificate)
  }
derive instance newtypeGetServerCertificateResponse :: Newtype GetServerCertificateResponse _


newtype GetServiceLinkedRoleDeletionStatusRequest = GetServiceLinkedRoleDeletionStatusRequest 
  { "DeletionTaskId" :: (DeletionTaskIdType)
  }
derive instance newtypeGetServiceLinkedRoleDeletionStatusRequest :: Newtype GetServiceLinkedRoleDeletionStatusRequest _


newtype GetServiceLinkedRoleDeletionStatusResponse = GetServiceLinkedRoleDeletionStatusResponse 
  { "Status" :: (DeletionTaskStatusType)
  , "Reason" :: NullOrUndefined (DeletionTaskFailureReasonType)
  }
derive instance newtypeGetServiceLinkedRoleDeletionStatusResponse :: Newtype GetServiceLinkedRoleDeletionStatusResponse _


newtype GetUserPolicyRequest = GetUserPolicyRequest 
  { "UserName" :: (ExistingUserNameType')
  , "PolicyName" :: (PolicyNameType')
  }
derive instance newtypeGetUserPolicyRequest :: Newtype GetUserPolicyRequest _


-- | <p>Contains the response to a successful <a>GetUserPolicy</a> request. </p>
newtype GetUserPolicyResponse = GetUserPolicyResponse 
  { "UserName" :: (ExistingUserNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypeGetUserPolicyResponse :: Newtype GetUserPolicyResponse _


newtype GetUserRequest = GetUserRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  }
derive instance newtypeGetUserRequest :: Newtype GetUserRequest _


-- | <p>Contains the response to a successful <a>GetUser</a> request. </p>
newtype GetUserResponse = GetUserResponse 
  { "User" :: (User)
  }
derive instance newtypeGetUserResponse :: Newtype GetUserResponse _


-- | <p>Contains information about an IAM group entity.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateGroup</a> </p> </li> <li> <p> <a>GetGroup</a> </p> </li> <li> <p> <a>ListGroups</a> </p> </li> </ul>
newtype Group = Group 
  { "Path" :: (PathType')
  , "GroupName" :: (GroupNameType')
  , "GroupId" :: (IdType')
  , "Arn" :: (ArnType')
  , "CreateDate" :: (DateType')
  }
derive instance newtypeGroup :: Newtype Group _


-- | <p>Contains information about an IAM group, including all of the group's policies.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>
newtype GroupDetail = GroupDetail 
  { "Path" :: NullOrUndefined (PathType')
  , "GroupName" :: NullOrUndefined (GroupNameType')
  , "GroupId" :: NullOrUndefined (IdType')
  , "Arn" :: NullOrUndefined (ArnType')
  , "CreateDate" :: NullOrUndefined (DateType')
  , "GroupPolicyList" :: NullOrUndefined (PolicyDetailListType')
  , "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  }
derive instance newtypeGroupDetail :: Newtype GroupDetail _


-- | <p>Contains information about an instance profile.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateInstanceProfile</a> </p> </li> <li> <p> <a>GetInstanceProfile</a> </p> </li> <li> <p> <a>ListInstanceProfiles</a> </p> </li> <li> <p> <a>ListInstanceProfilesForRole</a> </p> </li> </ul>
newtype InstanceProfile = InstanceProfile 
  { "Path" :: (PathType')
  , "InstanceProfileName" :: (InstanceProfileNameType')
  , "InstanceProfileId" :: (IdType')
  , "Arn" :: (ArnType')
  , "CreateDate" :: (DateType')
  , "Roles" :: (RoleListType')
  }
derive instance newtypeInstanceProfile :: Newtype InstanceProfile _


-- | <p>The request was rejected because the authentication code was not recognized. The error message describes the specific error.</p>
newtype InvalidAuthenticationCodeException = InvalidAuthenticationCodeException 
  { "Message'" :: NullOrUndefined (InvalidAuthenticationCodeMessage')
  }
derive instance newtypeInvalidAuthenticationCodeException :: Newtype InvalidAuthenticationCodeException _


-- | <p>The request was rejected because the certificate is invalid.</p>
newtype InvalidCertificateException = InvalidCertificateException 
  { "Message'" :: NullOrUndefined (InvalidCertificateMessage')
  }
derive instance newtypeInvalidCertificateException :: Newtype InvalidCertificateException _


-- | <p>The request was rejected because an invalid or out-of-range value was supplied for an input parameter.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message'" :: NullOrUndefined (InvalidInputMessage')
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


-- | <p>The request was rejected because the public key is malformed or otherwise invalid.</p>
newtype InvalidPublicKeyException = InvalidPublicKeyException 
  { "Message'" :: NullOrUndefined (InvalidPublicKeyMessage')
  }
derive instance newtypeInvalidPublicKeyException :: Newtype InvalidPublicKeyException _


-- | <p>The request was rejected because the type of user for the transaction was incorrect.</p>
newtype InvalidUserTypeException = InvalidUserTypeException 
  { "Message'" :: NullOrUndefined (InvalidUserTypeMessage')
  }
derive instance newtypeInvalidUserTypeException :: Newtype InvalidUserTypeException _


-- | <p>The request was rejected because the public key certificate and the private key do not match.</p>
newtype KeyPairMismatchException = KeyPairMismatchException 
  { "Message'" :: NullOrUndefined (KeyPairMismatchMessage')
  }
derive instance newtypeKeyPairMismatchException :: Newtype KeyPairMismatchException _


-- | <p>The request was rejected because it attempted to create resources beyond the current AWS account limits. The error message describes the limit exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (LimitExceededMessage')
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LineNumber = LineNumber Int
derive instance newtypeLineNumber :: Newtype LineNumber _


newtype ListAccessKeysRequest = ListAccessKeysRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListAccessKeysRequest :: Newtype ListAccessKeysRequest _


-- | <p>Contains the response to a successful <a>ListAccessKeys</a> request. </p>
newtype ListAccessKeysResponse = ListAccessKeysResponse 
  { "AccessKeyMetadata" :: (AccessKeyMetadataListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListAccessKeysResponse :: Newtype ListAccessKeysResponse _


newtype ListAccountAliasesRequest = ListAccountAliasesRequest 
  { "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListAccountAliasesRequest :: Newtype ListAccountAliasesRequest _


-- | <p>Contains the response to a successful <a>ListAccountAliases</a> request. </p>
newtype ListAccountAliasesResponse = ListAccountAliasesResponse 
  { "AccountAliases" :: (AccountAliasListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListAccountAliasesResponse :: Newtype ListAccountAliasesResponse _


newtype ListAttachedGroupPoliciesRequest = ListAttachedGroupPoliciesRequest 
  { "GroupName" :: (GroupNameType')
  , "PathPrefix" :: NullOrUndefined (PolicyPathType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListAttachedGroupPoliciesRequest :: Newtype ListAttachedGroupPoliciesRequest _


-- | <p>Contains the response to a successful <a>ListAttachedGroupPolicies</a> request. </p>
newtype ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse 
  { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListAttachedGroupPoliciesResponse :: Newtype ListAttachedGroupPoliciesResponse _


newtype ListAttachedRolePoliciesRequest = ListAttachedRolePoliciesRequest 
  { "RoleName" :: (RoleNameType')
  , "PathPrefix" :: NullOrUndefined (PolicyPathType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListAttachedRolePoliciesRequest :: Newtype ListAttachedRolePoliciesRequest _


-- | <p>Contains the response to a successful <a>ListAttachedRolePolicies</a> request. </p>
newtype ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse 
  { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListAttachedRolePoliciesResponse :: Newtype ListAttachedRolePoliciesResponse _


newtype ListAttachedUserPoliciesRequest = ListAttachedUserPoliciesRequest 
  { "UserName" :: (UserNameType')
  , "PathPrefix" :: NullOrUndefined (PolicyPathType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListAttachedUserPoliciesRequest :: Newtype ListAttachedUserPoliciesRequest _


-- | <p>Contains the response to a successful <a>ListAttachedUserPolicies</a> request. </p>
newtype ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse 
  { "AttachedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListAttachedUserPoliciesResponse :: Newtype ListAttachedUserPoliciesResponse _


newtype ListEntitiesForPolicyRequest = ListEntitiesForPolicyRequest 
  { "PolicyArn" :: (ArnType')
  , "EntityFilter" :: NullOrUndefined (EntityType)
  , "PathPrefix" :: NullOrUndefined (PathType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListEntitiesForPolicyRequest :: Newtype ListEntitiesForPolicyRequest _


-- | <p>Contains the response to a successful <a>ListEntitiesForPolicy</a> request. </p>
newtype ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse 
  { "PolicyGroups" :: NullOrUndefined (PolicyGroupListType)
  , "PolicyUsers" :: NullOrUndefined (PolicyUserListType)
  , "PolicyRoles" :: NullOrUndefined (PolicyRoleListType)
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListEntitiesForPolicyResponse :: Newtype ListEntitiesForPolicyResponse _


newtype ListGroupPoliciesRequest = ListGroupPoliciesRequest 
  { "GroupName" :: (GroupNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListGroupPoliciesRequest :: Newtype ListGroupPoliciesRequest _


-- | <p>Contains the response to a successful <a>ListGroupPolicies</a> request. </p>
newtype ListGroupPoliciesResponse = ListGroupPoliciesResponse 
  { "PolicyNames" :: (PolicyNameListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListGroupPoliciesResponse :: Newtype ListGroupPoliciesResponse _


newtype ListGroupsForUserRequest = ListGroupsForUserRequest 
  { "UserName" :: (ExistingUserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListGroupsForUserRequest :: Newtype ListGroupsForUserRequest _


-- | <p>Contains the response to a successful <a>ListGroupsForUser</a> request. </p>
newtype ListGroupsForUserResponse = ListGroupsForUserResponse 
  { "Groups" :: (GroupListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListGroupsForUserResponse :: Newtype ListGroupsForUserResponse _


newtype ListGroupsRequest = ListGroupsRequest 
  { "PathPrefix" :: NullOrUndefined (PathPrefixType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListGroupsRequest :: Newtype ListGroupsRequest _


-- | <p>Contains the response to a successful <a>ListGroups</a> request. </p>
newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: (GroupListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListGroupsResponse :: Newtype ListGroupsResponse _


newtype ListInstanceProfilesForRoleRequest = ListInstanceProfilesForRoleRequest 
  { "RoleName" :: (RoleNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListInstanceProfilesForRoleRequest :: Newtype ListInstanceProfilesForRoleRequest _


-- | <p>Contains the response to a successful <a>ListInstanceProfilesForRole</a> request. </p>
newtype ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse 
  { "InstanceProfiles" :: (InstanceProfileListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListInstanceProfilesForRoleResponse :: Newtype ListInstanceProfilesForRoleResponse _


newtype ListInstanceProfilesRequest = ListInstanceProfilesRequest 
  { "PathPrefix" :: NullOrUndefined (PathPrefixType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListInstanceProfilesRequest :: Newtype ListInstanceProfilesRequest _


-- | <p>Contains the response to a successful <a>ListInstanceProfiles</a> request. </p>
newtype ListInstanceProfilesResponse = ListInstanceProfilesResponse 
  { "InstanceProfiles" :: (InstanceProfileListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListInstanceProfilesResponse :: Newtype ListInstanceProfilesResponse _


newtype ListMFADevicesRequest = ListMFADevicesRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListMFADevicesRequest :: Newtype ListMFADevicesRequest _


-- | <p>Contains the response to a successful <a>ListMFADevices</a> request. </p>
newtype ListMFADevicesResponse = ListMFADevicesResponse 
  { "MFADevices" :: (MfaDeviceListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListMFADevicesResponse :: Newtype ListMFADevicesResponse _


newtype ListOpenIDConnectProvidersRequest = ListOpenIDConnectProvidersRequest 
  { 
  }
derive instance newtypeListOpenIDConnectProvidersRequest :: Newtype ListOpenIDConnectProvidersRequest _


-- | <p>Contains the response to a successful <a>ListOpenIDConnectProviders</a> request. </p>
newtype ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse 
  { "OpenIDConnectProviderList" :: NullOrUndefined (OpenIDConnectProviderListType)
  }
derive instance newtypeListOpenIDConnectProvidersResponse :: Newtype ListOpenIDConnectProvidersResponse _


newtype ListPoliciesRequest = ListPoliciesRequest 
  { "Scope" :: NullOrUndefined (PolicyScopeType')
  , "OnlyAttached" :: NullOrUndefined (BooleanType')
  , "PathPrefix" :: NullOrUndefined (PolicyPathType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListPoliciesRequest :: Newtype ListPoliciesRequest _


-- | <p>Contains the response to a successful <a>ListPolicies</a> request. </p>
newtype ListPoliciesResponse = ListPoliciesResponse 
  { "Policies" :: NullOrUndefined (PolicyListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListPoliciesResponse :: Newtype ListPoliciesResponse _


newtype ListPolicyVersionsRequest = ListPolicyVersionsRequest 
  { "PolicyArn" :: (ArnType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListPolicyVersionsRequest :: Newtype ListPolicyVersionsRequest _


-- | <p>Contains the response to a successful <a>ListPolicyVersions</a> request. </p>
newtype ListPolicyVersionsResponse = ListPolicyVersionsResponse 
  { "Versions" :: NullOrUndefined (PolicyDocumentVersionListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListPolicyVersionsResponse :: Newtype ListPolicyVersionsResponse _


newtype ListRolePoliciesRequest = ListRolePoliciesRequest 
  { "RoleName" :: (RoleNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListRolePoliciesRequest :: Newtype ListRolePoliciesRequest _


-- | <p>Contains the response to a successful <a>ListRolePolicies</a> request. </p>
newtype ListRolePoliciesResponse = ListRolePoliciesResponse 
  { "PolicyNames" :: (PolicyNameListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListRolePoliciesResponse :: Newtype ListRolePoliciesResponse _


newtype ListRolesRequest = ListRolesRequest 
  { "PathPrefix" :: NullOrUndefined (PathPrefixType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListRolesRequest :: Newtype ListRolesRequest _


-- | <p>Contains the response to a successful <a>ListRoles</a> request. </p>
newtype ListRolesResponse = ListRolesResponse 
  { "Roles" :: (RoleListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListRolesResponse :: Newtype ListRolesResponse _


newtype ListSAMLProvidersRequest = ListSAMLProvidersRequest 
  { 
  }
derive instance newtypeListSAMLProvidersRequest :: Newtype ListSAMLProvidersRequest _


-- | <p>Contains the response to a successful <a>ListSAMLProviders</a> request. </p>
newtype ListSAMLProvidersResponse = ListSAMLProvidersResponse 
  { "SAMLProviderList" :: NullOrUndefined (SAMLProviderListType)
  }
derive instance newtypeListSAMLProvidersResponse :: Newtype ListSAMLProvidersResponse _


newtype ListSSHPublicKeysRequest = ListSSHPublicKeysRequest 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListSSHPublicKeysRequest :: Newtype ListSSHPublicKeysRequest _


-- | <p>Contains the response to a successful <a>ListSSHPublicKeys</a> request.</p>
newtype ListSSHPublicKeysResponse = ListSSHPublicKeysResponse 
  { "SSHPublicKeys" :: NullOrUndefined (SSHPublicKeyListType)
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListSSHPublicKeysResponse :: Newtype ListSSHPublicKeysResponse _


newtype ListServerCertificatesRequest = ListServerCertificatesRequest 
  { "PathPrefix" :: NullOrUndefined (PathPrefixType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListServerCertificatesRequest :: Newtype ListServerCertificatesRequest _


-- | <p>Contains the response to a successful <a>ListServerCertificates</a> request. </p>
newtype ListServerCertificatesResponse = ListServerCertificatesResponse 
  { "ServerCertificateMetadataList" :: (ServerCertificateMetadataListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListServerCertificatesResponse :: Newtype ListServerCertificatesResponse _


newtype ListServiceSpecificCredentialsRequest = ListServiceSpecificCredentialsRequest 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "ServiceName" :: NullOrUndefined (ServiceName')
  }
derive instance newtypeListServiceSpecificCredentialsRequest :: Newtype ListServiceSpecificCredentialsRequest _


newtype ListServiceSpecificCredentialsResponse = ListServiceSpecificCredentialsResponse 
  { "ServiceSpecificCredentials" :: NullOrUndefined (ServiceSpecificCredentialsListType)
  }
derive instance newtypeListServiceSpecificCredentialsResponse :: Newtype ListServiceSpecificCredentialsResponse _


newtype ListSigningCertificatesRequest = ListSigningCertificatesRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListSigningCertificatesRequest :: Newtype ListSigningCertificatesRequest _


-- | <p>Contains the response to a successful <a>ListSigningCertificates</a> request. </p>
newtype ListSigningCertificatesResponse = ListSigningCertificatesResponse 
  { "Certificates" :: (CertificateListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListSigningCertificatesResponse :: Newtype ListSigningCertificatesResponse _


newtype ListUserPoliciesRequest = ListUserPoliciesRequest 
  { "UserName" :: (ExistingUserNameType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListUserPoliciesRequest :: Newtype ListUserPoliciesRequest _


-- | <p>Contains the response to a successful <a>ListUserPolicies</a> request. </p>
newtype ListUserPoliciesResponse = ListUserPoliciesResponse 
  { "PolicyNames" :: (PolicyNameListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListUserPoliciesResponse :: Newtype ListUserPoliciesResponse _


newtype ListUsersRequest = ListUsersRequest 
  { "PathPrefix" :: NullOrUndefined (PathPrefixType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListUsersRequest :: Newtype ListUsersRequest _


-- | <p>Contains the response to a successful <a>ListUsers</a> request. </p>
newtype ListUsersResponse = ListUsersResponse 
  { "Users" :: (UserListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListUsersResponse :: Newtype ListUsersResponse _


newtype ListVirtualMFADevicesRequest = ListVirtualMFADevicesRequest 
  { "AssignmentStatus" :: NullOrUndefined (AssignmentStatusType')
  , "Marker" :: NullOrUndefined (MarkerType')
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  }
derive instance newtypeListVirtualMFADevicesRequest :: Newtype ListVirtualMFADevicesRequest _


-- | <p>Contains the response to a successful <a>ListVirtualMFADevices</a> request. </p>
newtype ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse 
  { "VirtualMFADevices" :: (VirtualMFADeviceListType')
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeListVirtualMFADevicesResponse :: Newtype ListVirtualMFADevicesResponse _


-- | <p>Contains the user name and password create date for a user.</p> <p> This data type is used as a response element in the <a>CreateLoginProfile</a> and <a>GetLoginProfile</a> actions. </p>
newtype LoginProfile = LoginProfile 
  { "UserName" :: (UserNameType')
  , "CreateDate" :: (DateType')
  , "PasswordResetRequired" :: NullOrUndefined (BooleanType')
  }
derive instance newtypeLoginProfile :: Newtype LoginProfile _


-- | <p>Contains information about an MFA device.</p> <p>This data type is used as a response element in the <a>ListMFADevices</a> action.</p>
newtype MFADevice = MFADevice 
  { "UserName" :: (UserNameType')
  , "SerialNumber" :: (SerialNumberType')
  , "EnableDate" :: (DateType')
  }
derive instance newtypeMFADevice :: Newtype MFADevice _


-- | <p>The request was rejected because the certificate was malformed or expired. The error message describes the specific error.</p>
newtype MalformedCertificateException = MalformedCertificateException 
  { "Message'" :: NullOrUndefined (MalformedCertificateMessage')
  }
derive instance newtypeMalformedCertificateException :: Newtype MalformedCertificateException _


-- | <p>The request was rejected because the policy document was malformed. The error message describes the specific error.</p>
newtype MalformedPolicyDocumentException = MalformedPolicyDocumentException 
  { "Message'" :: NullOrUndefined (MalformedPolicyDocumentMessage')
  }
derive instance newtypeMalformedPolicyDocumentException :: Newtype MalformedPolicyDocumentException _


-- | <p>Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p> <p>For more information about managed policies, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype ManagedPolicyDetail = ManagedPolicyDetail 
  { "PolicyName" :: NullOrUndefined (PolicyNameType')
  , "PolicyId" :: NullOrUndefined (IdType')
  , "Arn" :: NullOrUndefined (ArnType')
  , "Path" :: NullOrUndefined (PolicyPathType')
  , "DefaultVersionId" :: NullOrUndefined (PolicyVersionIdType')
  , "AttachmentCount" :: NullOrUndefined (AttachmentCountType')
  , "IsAttachable" :: NullOrUndefined (BooleanType')
  , "Description" :: NullOrUndefined (PolicyDescriptionType')
  , "CreateDate" :: NullOrUndefined (DateType')
  , "UpdateDate" :: NullOrUndefined (DateType')
  , "PolicyVersionList" :: NullOrUndefined (PolicyDocumentVersionListType')
  }
derive instance newtypeManagedPolicyDetail :: Newtype ManagedPolicyDetail _


newtype ManagedPolicyDetailListType = ManagedPolicyDetailListType (Array ManagedPolicyDetail)
derive instance newtypeManagedPolicyDetailListType :: Newtype ManagedPolicyDetailListType _


-- | <p>The request was rejected because it referenced an entity that does not exist. The error message describes the entity.</p>
newtype NoSuchEntityException = NoSuchEntityException 
  { "Message'" :: NullOrUndefined (NoSuchEntityMessage')
  }
derive instance newtypeNoSuchEntityException :: Newtype NoSuchEntityException _


-- | <p>Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.</p>
newtype OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry 
  { "Arn" :: NullOrUndefined (ArnType')
  }
derive instance newtypeOpenIDConnectProviderListEntry :: Newtype OpenIDConnectProviderListEntry _


-- | <p>Contains a list of IAM OpenID Connect providers.</p>
newtype OpenIDConnectProviderListType = OpenIDConnectProviderListType (Array OpenIDConnectProviderListEntry)
derive instance newtypeOpenIDConnectProviderListType :: Newtype OpenIDConnectProviderListType _


-- | <p>Contains a URL that specifies the endpoint for an OpenID Connect provider.</p>
newtype OpenIDConnectProviderUrlType = OpenIDConnectProviderUrlType String
derive instance newtypeOpenIDConnectProviderUrlType :: Newtype OpenIDConnectProviderUrlType _


-- | <p>Contains information about AWS Organizations's affect on a policy simulation.</p>
newtype OrganizationsDecisionDetail = OrganizationsDecisionDetail 
  { "AllowedByOrganizations" :: NullOrUndefined (BooleanType')
  }
derive instance newtypeOrganizationsDecisionDetail :: Newtype OrganizationsDecisionDetail _


-- | <p>Contains information about the account password policy.</p> <p> This data type is used as a response element in the <a>GetAccountPasswordPolicy</a> action. </p>
newtype PasswordPolicy = PasswordPolicy 
  { "MinimumPasswordLength" :: NullOrUndefined (MinimumPasswordLengthType')
  , "RequireSymbols" :: NullOrUndefined (BooleanType')
  , "RequireNumbers" :: NullOrUndefined (BooleanType')
  , "RequireUppercaseCharacters" :: NullOrUndefined (BooleanType')
  , "RequireLowercaseCharacters" :: NullOrUndefined (BooleanType')
  , "AllowUsersToChangePassword" :: NullOrUndefined (BooleanType')
  , "ExpirePasswords" :: NullOrUndefined (BooleanType')
  , "MaxPasswordAge" :: NullOrUndefined (MaxPasswordAgeType')
  , "PasswordReusePrevention" :: NullOrUndefined (PasswordReusePreventionType')
  , "HardExpiry" :: NullOrUndefined (BooleanObjectType')
  }
derive instance newtypePasswordPolicy :: Newtype PasswordPolicy _


-- | <p>The request was rejected because the provided password did not meet the requirements imposed by the account password policy.</p>
newtype PasswordPolicyViolationException = PasswordPolicyViolationException 
  { "Message'" :: NullOrUndefined (PasswordPolicyViolationMessage')
  }
derive instance newtypePasswordPolicyViolationException :: Newtype PasswordPolicyViolationException _


-- | <p>Contains information about a managed policy.</p> <p>This data type is used as a response element in the <a>CreatePolicy</a>, <a>GetPolicy</a>, and <a>ListPolicies</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype Policy = Policy 
  { "PolicyName" :: NullOrUndefined (PolicyNameType')
  , "PolicyId" :: NullOrUndefined (IdType')
  , "Arn" :: NullOrUndefined (ArnType')
  , "Path" :: NullOrUndefined (PolicyPathType')
  , "DefaultVersionId" :: NullOrUndefined (PolicyVersionIdType')
  , "AttachmentCount" :: NullOrUndefined (AttachmentCountType')
  , "IsAttachable" :: NullOrUndefined (BooleanType')
  , "Description" :: NullOrUndefined (PolicyDescriptionType')
  , "CreateDate" :: NullOrUndefined (DateType')
  , "UpdateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypePolicy :: Newtype Policy _


-- | <p>Contains information about an IAM policy, including the policy document.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>
newtype PolicyDetail = PolicyDetail 
  { "PolicyName" :: NullOrUndefined (PolicyNameType')
  , "PolicyDocument" :: NullOrUndefined (PolicyDocumentType')
  }
derive instance newtypePolicyDetail :: Newtype PolicyDetail _


newtype PolicyEvaluationDecisionType = PolicyEvaluationDecisionType String
derive instance newtypePolicyEvaluationDecisionType :: Newtype PolicyEvaluationDecisionType _


-- | <p>The request failed because a provided policy could not be successfully evaluated. An additional detailed message indicates the source of the failure.</p>
newtype PolicyEvaluationException = PolicyEvaluationException 
  { "Message'" :: NullOrUndefined (PolicyEvaluationErrorMessage')
  }
derive instance newtypePolicyEvaluationException :: Newtype PolicyEvaluationException _


-- | <p>Contains information about a group that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype PolicyGroup = PolicyGroup 
  { "GroupName" :: NullOrUndefined (GroupNameType')
  , "GroupId" :: NullOrUndefined (IdType')
  }
derive instance newtypePolicyGroup :: Newtype PolicyGroup _


newtype PolicyGroupListType = PolicyGroupListType (Array PolicyGroup)
derive instance newtypePolicyGroupListType :: Newtype PolicyGroupListType _


newtype PolicyIdentifierType = PolicyIdentifierType String
derive instance newtypePolicyIdentifierType :: Newtype PolicyIdentifierType _


-- | <p>The request failed because AWS service role policies can only be attached to the service-linked role for that service.</p>
newtype PolicyNotAttachableException = PolicyNotAttachableException 
  { "Message'" :: NullOrUndefined (PolicyNotAttachableMessage')
  }
derive instance newtypePolicyNotAttachableException :: Newtype PolicyNotAttachableException _


-- | <p>Contains information about a role that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype PolicyRole = PolicyRole 
  { "RoleName" :: NullOrUndefined (RoleNameType')
  , "RoleId" :: NullOrUndefined (IdType')
  }
derive instance newtypePolicyRole :: Newtype PolicyRole _


newtype PolicyRoleListType = PolicyRoleListType (Array PolicyRole)
derive instance newtypePolicyRoleListType :: Newtype PolicyRoleListType _


newtype PolicySourceType = PolicySourceType String
derive instance newtypePolicySourceType :: Newtype PolicySourceType _


-- | <p>Contains information about a user that a managed policy is attached to.</p> <p>This data type is used as a response element in the <a>ListEntitiesForPolicy</a> action. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype PolicyUser = PolicyUser 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "UserId" :: NullOrUndefined (IdType')
  }
derive instance newtypePolicyUser :: Newtype PolicyUser _


newtype PolicyUserListType = PolicyUserListType (Array PolicyUser)
derive instance newtypePolicyUserListType :: Newtype PolicyUserListType _


-- | <p>Contains information about a version of a managed policy.</p> <p>This data type is used as a response element in the <a>CreatePolicyVersion</a>, <a>GetPolicyVersion</a>, <a>ListPolicyVersions</a>, and <a>GetAccountAuthorizationDetails</a> actions. </p> <p>For more information about managed policies, refer to <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html">Managed Policies and Inline Policies</a> in the <i>Using IAM</i> guide. </p>
newtype PolicyVersion = PolicyVersion 
  { "Document" :: NullOrUndefined (PolicyDocumentType')
  , "VersionId" :: NullOrUndefined (PolicyVersionIdType')
  , "IsDefaultVersion" :: NullOrUndefined (BooleanType')
  , "CreateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypePolicyVersion :: Newtype PolicyVersion _


-- | <p>Contains the row and column of a location of a <code>Statement</code> element in a policy document.</p> <p>This data type is used as a member of the <code> <a>Statement</a> </code> type.</p>
newtype Position = Position 
  { "Line" :: NullOrUndefined (LineNumber)
  , "Column" :: NullOrUndefined (ColumnNumber)
  }
derive instance newtypePosition :: Newtype Position _


newtype PutGroupPolicyRequest = PutGroupPolicyRequest 
  { "GroupName" :: (GroupNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypePutGroupPolicyRequest :: Newtype PutGroupPolicyRequest _


newtype PutRolePolicyRequest = PutRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypePutRolePolicyRequest :: Newtype PutRolePolicyRequest _


newtype PutUserPolicyRequest = PutUserPolicyRequest 
  { "UserName" :: (ExistingUserNameType')
  , "PolicyName" :: (PolicyNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypePutUserPolicyRequest :: Newtype PutUserPolicyRequest _


newtype ReasonType = ReasonType String
derive instance newtypeReasonType :: Newtype ReasonType _


newtype RegionNameType = RegionNameType String
derive instance newtypeRegionNameType :: Newtype RegionNameType _


newtype RemoveClientIDFromOpenIDConnectProviderRequest = RemoveClientIDFromOpenIDConnectProviderRequest 
  { "OpenIDConnectProviderArn" :: (ArnType')
  , "ClientID" :: (ClientIDType')
  }
derive instance newtypeRemoveClientIDFromOpenIDConnectProviderRequest :: Newtype RemoveClientIDFromOpenIDConnectProviderRequest _


newtype RemoveRoleFromInstanceProfileRequest = RemoveRoleFromInstanceProfileRequest 
  { "InstanceProfileName" :: (InstanceProfileNameType')
  , "RoleName" :: (RoleNameType')
  }
derive instance newtypeRemoveRoleFromInstanceProfileRequest :: Newtype RemoveRoleFromInstanceProfileRequest _


newtype RemoveUserFromGroupRequest = RemoveUserFromGroupRequest 
  { "GroupName" :: (GroupNameType')
  , "UserName" :: (ExistingUserNameType')
  }
derive instance newtypeRemoveUserFromGroupRequest :: Newtype RemoveUserFromGroupRequest _


newtype ReportContentType = ReportContentType String
derive instance newtypeReportContentType :: Newtype ReportContentType _


newtype ReportFormatType = ReportFormatType String
derive instance newtypeReportFormatType :: Newtype ReportFormatType _


newtype ReportStateDescriptionType = ReportStateDescriptionType String
derive instance newtypeReportStateDescriptionType :: Newtype ReportStateDescriptionType _


newtype ReportStateType = ReportStateType String
derive instance newtypeReportStateType :: Newtype ReportStateType _


newtype ResetServiceSpecificCredentialRequest = ResetServiceSpecificCredentialRequest 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "ServiceSpecificCredentialId" :: (ServiceSpecificCredentialId')
  }
derive instance newtypeResetServiceSpecificCredentialRequest :: Newtype ResetServiceSpecificCredentialRequest _


newtype ResetServiceSpecificCredentialResponse = ResetServiceSpecificCredentialResponse 
  { "ServiceSpecificCredential" :: NullOrUndefined (ServiceSpecificCredential)
  }
derive instance newtypeResetServiceSpecificCredentialResponse :: Newtype ResetServiceSpecificCredentialResponse _


newtype ResourceHandlingOptionType = ResourceHandlingOptionType String
derive instance newtypeResourceHandlingOptionType :: Newtype ResourceHandlingOptionType _


newtype ResourceNameListType = ResourceNameListType (Array ResourceNameType)
derive instance newtypeResourceNameListType :: Newtype ResourceNameListType _


newtype ResourceNameType = ResourceNameType String
derive instance newtypeResourceNameType :: Newtype ResourceNameType _


-- | <p>Contains the result of the simulation of a single API action call on a single resource.</p> <p>This data type is used by a member of the <a>EvaluationResult</a> data type.</p>
newtype ResourceSpecificResult = ResourceSpecificResult 
  { "EvalResourceName" :: (ResourceNameType)
  , "EvalResourceDecision" :: (PolicyEvaluationDecisionType)
  , "MatchedStatements" :: NullOrUndefined (StatementListType)
  , "MissingContextValues" :: NullOrUndefined (ContextKeyNamesResultListType)
  , "EvalDecisionDetails" :: NullOrUndefined (EvalDecisionDetailsType)
  }
derive instance newtypeResourceSpecificResult :: Newtype ResourceSpecificResult _


newtype ResourceSpecificResultListType = ResourceSpecificResultListType (Array ResourceSpecificResult)
derive instance newtypeResourceSpecificResultListType :: Newtype ResourceSpecificResultListType _


newtype ResyncMFADeviceRequest = ResyncMFADeviceRequest 
  { "UserName" :: (ExistingUserNameType')
  , "SerialNumber" :: (SerialNumberType')
  , "AuthenticationCode1" :: (AuthenticationCodeType')
  , "AuthenticationCode2" :: (AuthenticationCodeType')
  }
derive instance newtypeResyncMFADeviceRequest :: Newtype ResyncMFADeviceRequest _


-- | <p>Contains information about an IAM role. This structure is returned as a response element in several APIs that interact with roles.</p>
newtype Role = Role 
  { "Path" :: (PathType')
  , "RoleName" :: (RoleNameType')
  , "RoleId" :: (IdType')
  , "Arn" :: (ArnType')
  , "CreateDate" :: (DateType')
  , "AssumeRolePolicyDocument" :: NullOrUndefined (PolicyDocumentType')
  , "Description" :: NullOrUndefined (RoleDescriptionType')
  }
derive instance newtypeRole :: Newtype Role _


-- | <p>Contains information about an IAM role, including all of the role's policies.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>
newtype RoleDetail = RoleDetail 
  { "Path" :: NullOrUndefined (PathType')
  , "RoleName" :: NullOrUndefined (RoleNameType')
  , "RoleId" :: NullOrUndefined (IdType')
  , "Arn" :: NullOrUndefined (ArnType')
  , "CreateDate" :: NullOrUndefined (DateType')
  , "AssumeRolePolicyDocument" :: NullOrUndefined (PolicyDocumentType')
  , "InstanceProfileList" :: NullOrUndefined (InstanceProfileListType')
  , "RolePolicyList" :: NullOrUndefined (PolicyDetailListType')
  , "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  }
derive instance newtypeRoleDetail :: Newtype RoleDetail _


newtype RoleUsageListType = RoleUsageListType (Array RoleUsageType)
derive instance newtypeRoleUsageListType :: Newtype RoleUsageListType _


-- | <p>An object that contains details about how a service-linked role is used.</p> <p>This data type is used as a response element in the <a>GetServiceLinkedRoleDeletionStatus</a> operation.</p>
newtype RoleUsageType = RoleUsageType 
  { "Region" :: NullOrUndefined (RegionNameType)
  , "Resources" :: NullOrUndefined (ArnListType)
  }
derive instance newtypeRoleUsageType :: Newtype RoleUsageType _


newtype SAMLMetadataDocumentType = SAMLMetadataDocumentType String
derive instance newtypeSAMLMetadataDocumentType :: Newtype SAMLMetadataDocumentType _


-- | <p>Contains the list of SAML providers for this account.</p>
newtype SAMLProviderListEntry = SAMLProviderListEntry 
  { "Arn" :: NullOrUndefined (ArnType')
  , "ValidUntil" :: NullOrUndefined (DateType')
  , "CreateDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeSAMLProviderListEntry :: Newtype SAMLProviderListEntry _


newtype SAMLProviderListType = SAMLProviderListType (Array SAMLProviderListEntry)
derive instance newtypeSAMLProviderListType :: Newtype SAMLProviderListType _


newtype SAMLProviderNameType = SAMLProviderNameType String
derive instance newtypeSAMLProviderNameType :: Newtype SAMLProviderNameType _


-- | <p>Contains information about an SSH public key.</p> <p>This data type is used as a response element in the <a>GetSSHPublicKey</a> and <a>UploadSSHPublicKey</a> actions. </p>
newtype SSHPublicKey = SSHPublicKey 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyId" :: (PublicKeyIdType')
  , "Fingerprint" :: (PublicKeyFingerprintType')
  , "SSHPublicKeyBody" :: (PublicKeyMaterialType')
  , "Status" :: (StatusType')
  , "UploadDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeSSHPublicKey :: Newtype SSHPublicKey _


newtype SSHPublicKeyListType = SSHPublicKeyListType (Array SSHPublicKeyMetadata)
derive instance newtypeSSHPublicKeyListType :: Newtype SSHPublicKeyListType _


-- | <p>Contains information about an SSH public key, without the key's body or fingerprint.</p> <p>This data type is used as a response element in the <a>ListSSHPublicKeys</a> action.</p>
newtype SSHPublicKeyMetadata = SSHPublicKeyMetadata 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyId" :: (PublicKeyIdType')
  , "Status" :: (StatusType')
  , "UploadDate" :: (DateType')
  }
derive instance newtypeSSHPublicKeyMetadata :: Newtype SSHPublicKeyMetadata _


-- | <p>Contains information about a server certificate.</p> <p> This data type is used as a response element in the <a>GetServerCertificate</a> action. </p>
newtype ServerCertificate = ServerCertificate 
  { "ServerCertificateMetadata" :: (ServerCertificateMetadata)
  , "CertificateBody" :: (CertificateBodyType')
  , "CertificateChain" :: NullOrUndefined (CertificateChainType')
  }
derive instance newtypeServerCertificate :: Newtype ServerCertificate _


-- | <p>Contains information about a server certificate without its certificate body, certificate chain, and private key.</p> <p> This data type is used as a response element in the <a>UploadServerCertificate</a> and <a>ListServerCertificates</a> actions. </p>
newtype ServerCertificateMetadata = ServerCertificateMetadata 
  { "Path" :: (PathType')
  , "ServerCertificateName" :: (ServerCertificateNameType')
  , "ServerCertificateId" :: (IdType')
  , "Arn" :: (ArnType')
  , "UploadDate" :: NullOrUndefined (DateType')
  , "Expiration" :: NullOrUndefined (DateType')
  }
derive instance newtypeServerCertificateMetadata :: Newtype ServerCertificateMetadata _


-- | <p>The request processing has failed because of an unknown error, exception or failure.</p>
newtype ServiceFailureException = ServiceFailureException 
  { "Message'" :: NullOrUndefined (ServiceFailureExceptionMessage')
  }
derive instance newtypeServiceFailureException :: Newtype ServiceFailureException _


-- | <p>The specified service does not support service-specific credentials.</p>
newtype ServiceNotSupportedException = ServiceNotSupportedException 
  { "Message'" :: NullOrUndefined (ServiceNotSupportedMessage')
  }
derive instance newtypeServiceNotSupportedException :: Newtype ServiceNotSupportedException _


-- | <p>Contains the details of a service specific credential.</p>
newtype ServiceSpecificCredential = ServiceSpecificCredential 
  { "CreateDate" :: (DateType')
  , "ServiceName" :: (ServiceName')
  , "ServiceUserName" :: (ServiceUserName')
  , "ServicePassword" :: (ServicePassword')
  , "ServiceSpecificCredentialId" :: (ServiceSpecificCredentialId')
  , "UserName" :: (UserNameType')
  , "Status" :: (StatusType')
  }
derive instance newtypeServiceSpecificCredential :: Newtype ServiceSpecificCredential _


-- | <p>Contains additional details about a service-specific credential.</p>
newtype ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata 
  { "UserName" :: (UserNameType')
  , "Status" :: (StatusType')
  , "ServiceUserName" :: (ServiceUserName')
  , "CreateDate" :: (DateType')
  , "ServiceSpecificCredentialId" :: (ServiceSpecificCredentialId')
  , "ServiceName" :: (ServiceName')
  }
derive instance newtypeServiceSpecificCredentialMetadata :: Newtype ServiceSpecificCredentialMetadata _


newtype ServiceSpecificCredentialsListType = ServiceSpecificCredentialsListType (Array ServiceSpecificCredentialMetadata)
derive instance newtypeServiceSpecificCredentialsListType :: Newtype ServiceSpecificCredentialsListType _


newtype SetDefaultPolicyVersionRequest = SetDefaultPolicyVersionRequest 
  { "PolicyArn" :: (ArnType')
  , "VersionId" :: (PolicyVersionIdType')
  }
derive instance newtypeSetDefaultPolicyVersionRequest :: Newtype SetDefaultPolicyVersionRequest _


-- | <p>Contains information about an X.509 signing certificate.</p> <p>This data type is used as a response element in the <a>UploadSigningCertificate</a> and <a>ListSigningCertificates</a> actions. </p>
newtype SigningCertificate = SigningCertificate 
  { "UserName" :: (UserNameType')
  , "CertificateId" :: (CertificateIdType')
  , "CertificateBody" :: (CertificateBodyType')
  , "Status" :: (StatusType')
  , "UploadDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeSigningCertificate :: Newtype SigningCertificate _


newtype SimulateCustomPolicyRequest = SimulateCustomPolicyRequest 
  { "PolicyInputList" :: (SimulationPolicyListType)
  , "ActionNames" :: (ActionNameListType)
  , "ResourceArns" :: NullOrUndefined (ResourceNameListType)
  , "ResourcePolicy" :: NullOrUndefined (PolicyDocumentType')
  , "ResourceOwner" :: NullOrUndefined (ResourceNameType)
  , "CallerArn" :: NullOrUndefined (ResourceNameType)
  , "ContextEntries" :: NullOrUndefined (ContextEntryListType)
  , "ResourceHandlingOption" :: NullOrUndefined (ResourceHandlingOptionType)
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeSimulateCustomPolicyRequest :: Newtype SimulateCustomPolicyRequest _


-- | <p>Contains the response to a successful <a>SimulatePrincipalPolicy</a> or <a>SimulateCustomPolicy</a> request.</p>
newtype SimulatePolicyResponse = SimulatePolicyResponse 
  { "EvaluationResults" :: NullOrUndefined (EvaluationResultsListType)
  , "IsTruncated" :: NullOrUndefined (BooleanType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeSimulatePolicyResponse :: Newtype SimulatePolicyResponse _


newtype SimulatePrincipalPolicyRequest = SimulatePrincipalPolicyRequest 
  { "PolicySourceArn" :: (ArnType')
  , "PolicyInputList" :: NullOrUndefined (SimulationPolicyListType)
  , "ActionNames" :: (ActionNameListType)
  , "ResourceArns" :: NullOrUndefined (ResourceNameListType)
  , "ResourcePolicy" :: NullOrUndefined (PolicyDocumentType')
  , "ResourceOwner" :: NullOrUndefined (ResourceNameType)
  , "CallerArn" :: NullOrUndefined (ResourceNameType)
  , "ContextEntries" :: NullOrUndefined (ContextEntryListType)
  , "ResourceHandlingOption" :: NullOrUndefined (ResourceHandlingOptionType)
  , "MaxItems" :: NullOrUndefined (MaxItemsType')
  , "Marker" :: NullOrUndefined (MarkerType')
  }
derive instance newtypeSimulatePrincipalPolicyRequest :: Newtype SimulatePrincipalPolicyRequest _


newtype SimulationPolicyListType = SimulationPolicyListType (Array PolicyDocumentType')
derive instance newtypeSimulationPolicyListType :: Newtype SimulationPolicyListType _


-- | <p>Contains a reference to a <code>Statement</code> element in a policy document that determines the result of the simulation.</p> <p>This data type is used by the <code>MatchedStatements</code> member of the <code> <a>EvaluationResult</a> </code> type.</p>
newtype Statement = Statement 
  { "SourcePolicyId" :: NullOrUndefined (PolicyIdentifierType)
  , "SourcePolicyType" :: NullOrUndefined (PolicySourceType)
  , "StartPosition" :: NullOrUndefined (Position)
  , "EndPosition" :: NullOrUndefined (Position)
  }
derive instance newtypeStatement :: Newtype Statement _


newtype StatementListType = StatementListType (Array Statement)
derive instance newtypeStatementListType :: Newtype StatementListType _


-- | <p>The request was rejected because only the service that depends on the service-linked role can modify or delete the role on your behalf. The error message includes the name of the service that depends on this service-linked role. You must request the change through that service.</p>
newtype UnmodifiableEntityException = UnmodifiableEntityException 
  { "Message'" :: NullOrUndefined (UnmodifiableEntityMessage')
  }
derive instance newtypeUnmodifiableEntityException :: Newtype UnmodifiableEntityException _


-- | <p>The request was rejected because the public key encoding format is unsupported or unrecognized.</p>
newtype UnrecognizedPublicKeyEncodingException = UnrecognizedPublicKeyEncodingException 
  { "Message'" :: NullOrUndefined (UnrecognizedPublicKeyEncodingMessage')
  }
derive instance newtypeUnrecognizedPublicKeyEncodingException :: Newtype UnrecognizedPublicKeyEncodingException _


newtype UpdateAccessKeyRequest = UpdateAccessKeyRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "AccessKeyId" :: (AccessKeyIdType')
  , "Status" :: (StatusType')
  }
derive instance newtypeUpdateAccessKeyRequest :: Newtype UpdateAccessKeyRequest _


newtype UpdateAccountPasswordPolicyRequest = UpdateAccountPasswordPolicyRequest 
  { "MinimumPasswordLength" :: NullOrUndefined (MinimumPasswordLengthType')
  , "RequireSymbols" :: NullOrUndefined (BooleanType')
  , "RequireNumbers" :: NullOrUndefined (BooleanType')
  , "RequireUppercaseCharacters" :: NullOrUndefined (BooleanType')
  , "RequireLowercaseCharacters" :: NullOrUndefined (BooleanType')
  , "AllowUsersToChangePassword" :: NullOrUndefined (BooleanType')
  , "MaxPasswordAge" :: NullOrUndefined (MaxPasswordAgeType')
  , "PasswordReusePrevention" :: NullOrUndefined (PasswordReusePreventionType')
  , "HardExpiry" :: NullOrUndefined (BooleanObjectType')
  }
derive instance newtypeUpdateAccountPasswordPolicyRequest :: Newtype UpdateAccountPasswordPolicyRequest _


newtype UpdateAssumeRolePolicyRequest = UpdateAssumeRolePolicyRequest 
  { "RoleName" :: (RoleNameType')
  , "PolicyDocument" :: (PolicyDocumentType')
  }
derive instance newtypeUpdateAssumeRolePolicyRequest :: Newtype UpdateAssumeRolePolicyRequest _


newtype UpdateGroupRequest = UpdateGroupRequest 
  { "GroupName" :: (GroupNameType')
  , "NewPath" :: NullOrUndefined (PathType')
  , "NewGroupName" :: NullOrUndefined (GroupNameType')
  }
derive instance newtypeUpdateGroupRequest :: Newtype UpdateGroupRequest _


newtype UpdateLoginProfileRequest = UpdateLoginProfileRequest 
  { "UserName" :: (UserNameType')
  , "Password" :: NullOrUndefined (PasswordType')
  , "PasswordResetRequired" :: NullOrUndefined (BooleanObjectType')
  }
derive instance newtypeUpdateLoginProfileRequest :: Newtype UpdateLoginProfileRequest _


newtype UpdateOpenIDConnectProviderThumbprintRequest = UpdateOpenIDConnectProviderThumbprintRequest 
  { "OpenIDConnectProviderArn" :: (ArnType')
  , "ThumbprintList" :: (ThumbprintListType')
  }
derive instance newtypeUpdateOpenIDConnectProviderThumbprintRequest :: Newtype UpdateOpenIDConnectProviderThumbprintRequest _


newtype UpdateRoleDescriptionRequest = UpdateRoleDescriptionRequest 
  { "RoleName" :: (RoleNameType')
  , "Description" :: (RoleDescriptionType')
  }
derive instance newtypeUpdateRoleDescriptionRequest :: Newtype UpdateRoleDescriptionRequest _


newtype UpdateRoleDescriptionResponse = UpdateRoleDescriptionResponse 
  { "Role" :: NullOrUndefined (Role)
  }
derive instance newtypeUpdateRoleDescriptionResponse :: Newtype UpdateRoleDescriptionResponse _


newtype UpdateSAMLProviderRequest = UpdateSAMLProviderRequest 
  { "SAMLMetadataDocument" :: (SAMLMetadataDocumentType)
  , "SAMLProviderArn" :: (ArnType')
  }
derive instance newtypeUpdateSAMLProviderRequest :: Newtype UpdateSAMLProviderRequest _


-- | <p>Contains the response to a successful <a>UpdateSAMLProvider</a> request. </p>
newtype UpdateSAMLProviderResponse = UpdateSAMLProviderResponse 
  { "SAMLProviderArn" :: NullOrUndefined (ArnType')
  }
derive instance newtypeUpdateSAMLProviderResponse :: Newtype UpdateSAMLProviderResponse _


newtype UpdateSSHPublicKeyRequest = UpdateSSHPublicKeyRequest 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyId" :: (PublicKeyIdType')
  , "Status" :: (StatusType')
  }
derive instance newtypeUpdateSSHPublicKeyRequest :: Newtype UpdateSSHPublicKeyRequest _


newtype UpdateServerCertificateRequest = UpdateServerCertificateRequest 
  { "ServerCertificateName" :: (ServerCertificateNameType')
  , "NewPath" :: NullOrUndefined (PathType')
  , "NewServerCertificateName" :: NullOrUndefined (ServerCertificateNameType')
  }
derive instance newtypeUpdateServerCertificateRequest :: Newtype UpdateServerCertificateRequest _


newtype UpdateServiceSpecificCredentialRequest = UpdateServiceSpecificCredentialRequest 
  { "UserName" :: NullOrUndefined (UserNameType')
  , "ServiceSpecificCredentialId" :: (ServiceSpecificCredentialId')
  , "Status" :: (StatusType')
  }
derive instance newtypeUpdateServiceSpecificCredentialRequest :: Newtype UpdateServiceSpecificCredentialRequest _


newtype UpdateSigningCertificateRequest = UpdateSigningCertificateRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "CertificateId" :: (CertificateIdType')
  , "Status" :: (StatusType')
  }
derive instance newtypeUpdateSigningCertificateRequest :: Newtype UpdateSigningCertificateRequest _


newtype UpdateUserRequest = UpdateUserRequest 
  { "UserName" :: (ExistingUserNameType')
  , "NewPath" :: NullOrUndefined (PathType')
  , "NewUserName" :: NullOrUndefined (UserNameType')
  }
derive instance newtypeUpdateUserRequest :: Newtype UpdateUserRequest _


newtype UploadSSHPublicKeyRequest = UploadSSHPublicKeyRequest 
  { "UserName" :: (UserNameType')
  , "SSHPublicKeyBody" :: (PublicKeyMaterialType')
  }
derive instance newtypeUploadSSHPublicKeyRequest :: Newtype UploadSSHPublicKeyRequest _


-- | <p>Contains the response to a successful <a>UploadSSHPublicKey</a> request.</p>
newtype UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse 
  { "SSHPublicKey" :: NullOrUndefined (SSHPublicKey)
  }
derive instance newtypeUploadSSHPublicKeyResponse :: Newtype UploadSSHPublicKeyResponse _


newtype UploadServerCertificateRequest = UploadServerCertificateRequest 
  { "Path" :: NullOrUndefined (PathType')
  , "ServerCertificateName" :: (ServerCertificateNameType')
  , "CertificateBody" :: (CertificateBodyType')
  , "PrivateKey" :: (PrivateKeyType')
  , "CertificateChain" :: NullOrUndefined (CertificateChainType')
  }
derive instance newtypeUploadServerCertificateRequest :: Newtype UploadServerCertificateRequest _


-- | <p>Contains the response to a successful <a>UploadServerCertificate</a> request. </p>
newtype UploadServerCertificateResponse = UploadServerCertificateResponse 
  { "ServerCertificateMetadata" :: NullOrUndefined (ServerCertificateMetadata)
  }
derive instance newtypeUploadServerCertificateResponse :: Newtype UploadServerCertificateResponse _


newtype UploadSigningCertificateRequest = UploadSigningCertificateRequest 
  { "UserName" :: NullOrUndefined (ExistingUserNameType')
  , "CertificateBody" :: (CertificateBodyType')
  }
derive instance newtypeUploadSigningCertificateRequest :: Newtype UploadSigningCertificateRequest _


-- | <p>Contains the response to a successful <a>UploadSigningCertificate</a> request. </p>
newtype UploadSigningCertificateResponse = UploadSigningCertificateResponse 
  { "Certificate" :: (SigningCertificate)
  }
derive instance newtypeUploadSigningCertificateResponse :: Newtype UploadSigningCertificateResponse _


-- | <p>Contains information about an IAM user entity.</p> <p>This data type is used as a response element in the following actions:</p> <ul> <li> <p> <a>CreateUser</a> </p> </li> <li> <p> <a>GetUser</a> </p> </li> <li> <p> <a>ListUsers</a> </p> </li> </ul>
newtype User = User 
  { "Path" :: (PathType')
  , "UserName" :: (UserNameType')
  , "UserId" :: (IdType')
  , "Arn" :: (ArnType')
  , "CreateDate" :: (DateType')
  , "PasswordLastUsed" :: NullOrUndefined (DateType')
  }
derive instance newtypeUser :: Newtype User _


-- | <p>Contains information about an IAM user, including all the user's policies and all the IAM groups the user is in.</p> <p>This data type is used as a response element in the <a>GetAccountAuthorizationDetails</a> action.</p>
newtype UserDetail = UserDetail 
  { "Path" :: NullOrUndefined (PathType')
  , "UserName" :: NullOrUndefined (UserNameType')
  , "UserId" :: NullOrUndefined (IdType')
  , "Arn" :: NullOrUndefined (ArnType')
  , "CreateDate" :: NullOrUndefined (DateType')
  , "UserPolicyList" :: NullOrUndefined (PolicyDetailListType')
  , "GroupList" :: NullOrUndefined (GroupNameListType')
  , "AttachedManagedPolicies" :: NullOrUndefined (AttachedPoliciesListType')
  }
derive instance newtypeUserDetail :: Newtype UserDetail _


-- | <p>Contains information about a virtual MFA device.</p>
newtype VirtualMFADevice = VirtualMFADevice 
  { "SerialNumber" :: (SerialNumberType')
  , "Base32StringSeed" :: NullOrUndefined (BootstrapDatum)
  , "QRCodePNG" :: NullOrUndefined (BootstrapDatum)
  , "User" :: NullOrUndefined (User)
  , "EnableDate" :: NullOrUndefined (DateType')
  }
derive instance newtypeVirtualMFADevice :: Newtype VirtualMFADevice _


newtype AccessKeyIdType' = AccessKeyIdType' String
derive instance newtypeAccessKeyIdType' :: Newtype AccessKeyIdType' _


-- | <p>Contains a list of access key metadata.</p> <p>This data type is used as a response element in the <a>ListAccessKeys</a> action.</p>
newtype AccessKeyMetadataListType' = AccessKeyMetadataListType' (Array AccessKeyMetadata)
derive instance newtypeAccessKeyMetadataListType' :: Newtype AccessKeyMetadataListType' _


newtype AccessKeySecretType' = AccessKeySecretType' String
derive instance newtypeAccessKeySecretType' :: Newtype AccessKeySecretType' _


newtype AccountAliasListType' = AccountAliasListType' (Array AccountAliasType')
derive instance newtypeAccountAliasListType' :: Newtype AccountAliasListType' _


newtype AccountAliasType' = AccountAliasType' String
derive instance newtypeAccountAliasType' :: Newtype AccountAliasType' _


-- | <p>The Amazon Resource Name (ARN). ARNs are unique identifiers for AWS resources.</p> <p>For more information about ARNs, go to <a href="http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html">Amazon Resource Names (ARNs) and AWS Service Namespaces</a> in the <i>AWS General Reference</i>. </p>
newtype ArnType' = ArnType' String
derive instance newtypeArnType' :: Newtype ArnType' _


newtype AssignmentStatusType' = AssignmentStatusType' String
derive instance newtypeAssignmentStatusType' :: Newtype AssignmentStatusType' _


newtype AttachedPoliciesListType' = AttachedPoliciesListType' (Array AttachedPolicy)
derive instance newtypeAttachedPoliciesListType' :: Newtype AttachedPoliciesListType' _


newtype AttachmentCountType' = AttachmentCountType' Int
derive instance newtypeAttachmentCountType' :: Newtype AttachmentCountType' _


newtype AuthenticationCodeType' = AuthenticationCodeType' String
derive instance newtypeAuthenticationCodeType' :: Newtype AuthenticationCodeType' _


newtype BooleanObjectType' = BooleanObjectType' Boolean
derive instance newtypeBooleanObjectType' :: Newtype BooleanObjectType' _


newtype BooleanType' = BooleanType' Boolean
derive instance newtypeBooleanType' :: Newtype BooleanType' _


newtype CertificateBodyType' = CertificateBodyType' String
derive instance newtypeCertificateBodyType' :: Newtype CertificateBodyType' _


newtype CertificateChainType' = CertificateChainType' String
derive instance newtypeCertificateChainType' :: Newtype CertificateChainType' _


newtype CertificateIdType' = CertificateIdType' String
derive instance newtypeCertificateIdType' :: Newtype CertificateIdType' _


-- | <p>Contains a list of signing certificates.</p> <p>This data type is used as a response element in the <a>ListSigningCertificates</a> action.</p>
newtype CertificateListType' = CertificateListType' (Array SigningCertificate)
derive instance newtypeCertificateListType' :: Newtype CertificateListType' _


newtype ClientIDListType' = ClientIDListType' (Array ClientIDType')
derive instance newtypeClientIDListType' :: Newtype ClientIDListType' _


newtype ClientIDType' = ClientIDType' String
derive instance newtypeClientIDType' :: Newtype ClientIDType' _


newtype CredentialReportExpiredExceptionMessage' = CredentialReportExpiredExceptionMessage' String
derive instance newtypeCredentialReportExpiredExceptionMessage' :: Newtype CredentialReportExpiredExceptionMessage' _


newtype CredentialReportNotPresentExceptionMessage' = CredentialReportNotPresentExceptionMessage' String
derive instance newtypeCredentialReportNotPresentExceptionMessage' :: Newtype CredentialReportNotPresentExceptionMessage' _


newtype CredentialReportNotReadyExceptionMessage' = CredentialReportNotReadyExceptionMessage' String
derive instance newtypeCredentialReportNotReadyExceptionMessage' :: Newtype CredentialReportNotReadyExceptionMessage' _


newtype CustomSuffixType' = CustomSuffixType' String
derive instance newtypeCustomSuffixType' :: Newtype CustomSuffixType' _


newtype DateType' = DateType' Number
derive instance newtypeDateType' :: Newtype DateType' _


newtype DeleteConflictMessage' = DeleteConflictMessage' String
derive instance newtypeDeleteConflictMessage' :: Newtype DeleteConflictMessage' _


newtype DuplicateCertificateMessage' = DuplicateCertificateMessage' String
derive instance newtypeDuplicateCertificateMessage' :: Newtype DuplicateCertificateMessage' _


newtype DuplicateSSHPublicKeyMessage' = DuplicateSSHPublicKeyMessage' String
derive instance newtypeDuplicateSSHPublicKeyMessage' :: Newtype DuplicateSSHPublicKeyMessage' _


newtype EncodingType' = EncodingType' String
derive instance newtypeEncodingType' :: Newtype EncodingType' _


newtype EntityAlreadyExistsMessage' = EntityAlreadyExistsMessage' String
derive instance newtypeEntityAlreadyExistsMessage' :: Newtype EntityAlreadyExistsMessage' _


newtype EntityListType' = EntityListType' (Array EntityType)
derive instance newtypeEntityListType' :: Newtype EntityListType' _


newtype EntityTemporarilyUnmodifiableMessage' = EntityTemporarilyUnmodifiableMessage' String
derive instance newtypeEntityTemporarilyUnmodifiableMessage' :: Newtype EntityTemporarilyUnmodifiableMessage' _


newtype ExistingUserNameType' = ExistingUserNameType' String
derive instance newtypeExistingUserNameType' :: Newtype ExistingUserNameType' _


newtype GroupDetailListType' = GroupDetailListType' (Array GroupDetail)
derive instance newtypeGroupDetailListType' :: Newtype GroupDetailListType' _


-- | <p>Contains a list of IAM groups.</p> <p>This data type is used as a response element in the <a>ListGroups</a> action.</p>
newtype GroupListType' = GroupListType' (Array Group)
derive instance newtypeGroupListType' :: Newtype GroupListType' _


newtype GroupNameListType' = GroupNameListType' (Array GroupNameType')
derive instance newtypeGroupNameListType' :: Newtype GroupNameListType' _


newtype GroupNameType' = GroupNameType' String
derive instance newtypeGroupNameType' :: Newtype GroupNameType' _


newtype IdType' = IdType' String
derive instance newtypeIdType' :: Newtype IdType' _


-- | <p>Contains a list of instance profiles.</p>
newtype InstanceProfileListType' = InstanceProfileListType' (Array InstanceProfile)
derive instance newtypeInstanceProfileListType' :: Newtype InstanceProfileListType' _


newtype InstanceProfileNameType' = InstanceProfileNameType' String
derive instance newtypeInstanceProfileNameType' :: Newtype InstanceProfileNameType' _


newtype InvalidAuthenticationCodeMessage' = InvalidAuthenticationCodeMessage' String
derive instance newtypeInvalidAuthenticationCodeMessage' :: Newtype InvalidAuthenticationCodeMessage' _


newtype InvalidCertificateMessage' = InvalidCertificateMessage' String
derive instance newtypeInvalidCertificateMessage' :: Newtype InvalidCertificateMessage' _


newtype InvalidInputMessage' = InvalidInputMessage' String
derive instance newtypeInvalidInputMessage' :: Newtype InvalidInputMessage' _


newtype InvalidPublicKeyMessage' = InvalidPublicKeyMessage' String
derive instance newtypeInvalidPublicKeyMessage' :: Newtype InvalidPublicKeyMessage' _


newtype InvalidUserTypeMessage' = InvalidUserTypeMessage' String
derive instance newtypeInvalidUserTypeMessage' :: Newtype InvalidUserTypeMessage' _


newtype KeyPairMismatchMessage' = KeyPairMismatchMessage' String
derive instance newtypeKeyPairMismatchMessage' :: Newtype KeyPairMismatchMessage' _


newtype LimitExceededMessage' = LimitExceededMessage' String
derive instance newtypeLimitExceededMessage' :: Newtype LimitExceededMessage' _


newtype MalformedCertificateMessage' = MalformedCertificateMessage' String
derive instance newtypeMalformedCertificateMessage' :: Newtype MalformedCertificateMessage' _


newtype MalformedPolicyDocumentMessage' = MalformedPolicyDocumentMessage' String
derive instance newtypeMalformedPolicyDocumentMessage' :: Newtype MalformedPolicyDocumentMessage' _


newtype MarkerType' = MarkerType' String
derive instance newtypeMarkerType' :: Newtype MarkerType' _


newtype MaxItemsType' = MaxItemsType' Int
derive instance newtypeMaxItemsType' :: Newtype MaxItemsType' _


newtype MaxPasswordAgeType' = MaxPasswordAgeType' Int
derive instance newtypeMaxPasswordAgeType' :: Newtype MaxPasswordAgeType' _


-- | <p>Contains a list of MFA devices.</p> <p>This data type is used as a response element in the <a>ListMFADevices</a> and <a>ListVirtualMFADevices</a> actions. </p>
newtype MfaDeviceListType' = MfaDeviceListType' (Array MFADevice)
derive instance newtypeMfaDeviceListType' :: Newtype MfaDeviceListType' _


newtype MinimumPasswordLengthType' = MinimumPasswordLengthType' Int
derive instance newtypeMinimumPasswordLengthType' :: Newtype MinimumPasswordLengthType' _


newtype NoSuchEntityMessage' = NoSuchEntityMessage' String
derive instance newtypeNoSuchEntityMessage' :: Newtype NoSuchEntityMessage' _


newtype PasswordPolicyViolationMessage' = PasswordPolicyViolationMessage' String
derive instance newtypePasswordPolicyViolationMessage' :: Newtype PasswordPolicyViolationMessage' _


newtype PasswordReusePreventionType' = PasswordReusePreventionType' Int
derive instance newtypePasswordReusePreventionType' :: Newtype PasswordReusePreventionType' _


newtype PasswordType' = PasswordType' String
derive instance newtypePasswordType' :: Newtype PasswordType' _


newtype PathPrefixType' = PathPrefixType' String
derive instance newtypePathPrefixType' :: Newtype PathPrefixType' _


newtype PathType' = PathType' String
derive instance newtypePathType' :: Newtype PathType' _


newtype PolicyDescriptionType' = PolicyDescriptionType' String
derive instance newtypePolicyDescriptionType' :: Newtype PolicyDescriptionType' _


newtype PolicyDetailListType' = PolicyDetailListType' (Array PolicyDetail)
derive instance newtypePolicyDetailListType' :: Newtype PolicyDetailListType' _


newtype PolicyDocumentType' = PolicyDocumentType' String
derive instance newtypePolicyDocumentType' :: Newtype PolicyDocumentType' _


newtype PolicyDocumentVersionListType' = PolicyDocumentVersionListType' (Array PolicyVersion)
derive instance newtypePolicyDocumentVersionListType' :: Newtype PolicyDocumentVersionListType' _


newtype PolicyEvaluationErrorMessage' = PolicyEvaluationErrorMessage' String
derive instance newtypePolicyEvaluationErrorMessage' :: Newtype PolicyEvaluationErrorMessage' _


newtype PolicyListType' = PolicyListType' (Array Policy)
derive instance newtypePolicyListType' :: Newtype PolicyListType' _


-- | <p>Contains a list of policy names.</p> <p>This data type is used as a response element in the <a>ListPolicies</a> action.</p>
newtype PolicyNameListType' = PolicyNameListType' (Array PolicyNameType')
derive instance newtypePolicyNameListType' :: Newtype PolicyNameListType' _


newtype PolicyNameType' = PolicyNameType' String
derive instance newtypePolicyNameType' :: Newtype PolicyNameType' _


newtype PolicyNotAttachableMessage' = PolicyNotAttachableMessage' String
derive instance newtypePolicyNotAttachableMessage' :: Newtype PolicyNotAttachableMessage' _


newtype PolicyPathType' = PolicyPathType' String
derive instance newtypePolicyPathType' :: Newtype PolicyPathType' _


newtype PolicyScopeType' = PolicyScopeType' String
derive instance newtypePolicyScopeType' :: Newtype PolicyScopeType' _


newtype PolicyVersionIdType' = PolicyVersionIdType' String
derive instance newtypePolicyVersionIdType' :: Newtype PolicyVersionIdType' _


newtype PrivateKeyType' = PrivateKeyType' String
derive instance newtypePrivateKeyType' :: Newtype PrivateKeyType' _


newtype PublicKeyFingerprintType' = PublicKeyFingerprintType' String
derive instance newtypePublicKeyFingerprintType' :: Newtype PublicKeyFingerprintType' _


newtype PublicKeyIdType' = PublicKeyIdType' String
derive instance newtypePublicKeyIdType' :: Newtype PublicKeyIdType' _


newtype PublicKeyMaterialType' = PublicKeyMaterialType' String
derive instance newtypePublicKeyMaterialType' :: Newtype PublicKeyMaterialType' _


newtype RoleDescriptionType' = RoleDescriptionType' String
derive instance newtypeRoleDescriptionType' :: Newtype RoleDescriptionType' _


newtype RoleDetailListType' = RoleDetailListType' (Array RoleDetail)
derive instance newtypeRoleDetailListType' :: Newtype RoleDetailListType' _


-- | <p>Contains a list of IAM roles.</p> <p>This data type is used as a response element in the <a>ListRoles</a> action.</p>
newtype RoleListType' = RoleListType' (Array Role)
derive instance newtypeRoleListType' :: Newtype RoleListType' _


newtype RoleNameType' = RoleNameType' String
derive instance newtypeRoleNameType' :: Newtype RoleNameType' _


newtype SerialNumberType' = SerialNumberType' String
derive instance newtypeSerialNumberType' :: Newtype SerialNumberType' _


newtype ServerCertificateMetadataListType' = ServerCertificateMetadataListType' (Array ServerCertificateMetadata)
derive instance newtypeServerCertificateMetadataListType' :: Newtype ServerCertificateMetadataListType' _


newtype ServerCertificateNameType' = ServerCertificateNameType' String
derive instance newtypeServerCertificateNameType' :: Newtype ServerCertificateNameType' _


newtype ServiceFailureExceptionMessage' = ServiceFailureExceptionMessage' String
derive instance newtypeServiceFailureExceptionMessage' :: Newtype ServiceFailureExceptionMessage' _


newtype ServiceName' = ServiceName' String
derive instance newtypeServiceName' :: Newtype ServiceName' _


newtype ServiceNotSupportedMessage' = ServiceNotSupportedMessage' String
derive instance newtypeServiceNotSupportedMessage' :: Newtype ServiceNotSupportedMessage' _


newtype ServicePassword' = ServicePassword' String
derive instance newtypeServicePassword' :: Newtype ServicePassword' _


newtype ServiceSpecificCredentialId' = ServiceSpecificCredentialId' String
derive instance newtypeServiceSpecificCredentialId' :: Newtype ServiceSpecificCredentialId' _


newtype ServiceUserName' = ServiceUserName' String
derive instance newtypeServiceUserName' :: Newtype ServiceUserName' _


newtype StatusType' = StatusType' String
derive instance newtypeStatusType' :: Newtype StatusType' _


newtype StringType' = StringType' String
derive instance newtypeStringType' :: Newtype StringType' _


newtype SummaryKeyType' = SummaryKeyType' String
derive instance newtypeSummaryKeyType' :: Newtype SummaryKeyType' _


newtype SummaryMapType' = SummaryMapType' (Map SummaryKeyType' SummaryValueType')
derive instance newtypeSummaryMapType' :: Newtype SummaryMapType' _


newtype SummaryValueType' = SummaryValueType' Int
derive instance newtypeSummaryValueType' :: Newtype SummaryValueType' _


-- | <p>Contains a list of thumbprints of identity provider server certificates.</p>
newtype ThumbprintListType' = ThumbprintListType' (Array ThumbprintType')
derive instance newtypeThumbprintListType' :: Newtype ThumbprintListType' _


-- | <p>Contains a thumbprint for an identity provider's server certificate.</p> <p>The identity provider's server certificate thumbprint is the hex-encoded SHA-1 hash value of the self-signed X.509 certificate used by the domain where the OpenID Connect provider makes its keys available. It is always a 40-character string.</p>
newtype ThumbprintType' = ThumbprintType' String
derive instance newtypeThumbprintType' :: Newtype ThumbprintType' _


newtype UnmodifiableEntityMessage' = UnmodifiableEntityMessage' String
derive instance newtypeUnmodifiableEntityMessage' :: Newtype UnmodifiableEntityMessage' _


newtype UnrecognizedPublicKeyEncodingMessage' = UnrecognizedPublicKeyEncodingMessage' String
derive instance newtypeUnrecognizedPublicKeyEncodingMessage' :: Newtype UnrecognizedPublicKeyEncodingMessage' _


newtype UserDetailListType' = UserDetailListType' (Array UserDetail)
derive instance newtypeUserDetailListType' :: Newtype UserDetailListType' _


-- | <p>Contains a list of users.</p> <p>This data type is used as a response element in the <a>GetGroup</a> and <a>ListUsers</a> actions. </p>
newtype UserListType' = UserListType' (Array User)
derive instance newtypeUserListType' :: Newtype UserListType' _


newtype UserNameType' = UserNameType' String
derive instance newtypeUserNameType' :: Newtype UserNameType' _


newtype VirtualMFADeviceListType' = VirtualMFADeviceListType' (Array VirtualMFADevice)
derive instance newtypeVirtualMFADeviceListType' :: Newtype VirtualMFADeviceListType' _


newtype VirtualMFADeviceName' = VirtualMFADeviceName' String
derive instance newtypeVirtualMFADeviceName' :: Newtype VirtualMFADeviceName' _
