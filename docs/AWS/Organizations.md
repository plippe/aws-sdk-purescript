## Module AWS.Organizations

<fullname>AWS Organizations API Reference</fullname> <p>AWS Organizations is a web service that enables you to consolidate your multiple AWS accounts into an <i>organization</i> and centrally manage your accounts and their resources.</p> <p>This guide provides descriptions of the Organizations API. For more information about using this service, see the <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html">AWS Organizations User Guide</a>.</p> <p> <b>API Version</b> </p> <p>This version of the Organizations API Reference documents the Organizations API version 2016-11-28.</p> <note> <p>As an alternative to using the API directly, you can use one of the AWS SDKs, which consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, and more). The SDKs provide a convenient way to create programmatic access to AWS Organizations. For example, the SDKs take care of cryptographically signing requests, managing errors, and retrying requests automatically. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note> <p>We recommend that you use the AWS SDKs to make programmatic API calls to Organizations. However, you also can use the Organizations Query API to make direct calls to the Organizations web service. To learn more about the Organizations Query API, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_query-requests.html">Making Query Requests</a> in the <i>AWS Organizations User Guide</i>. Organizations supports GET and POST requests for all actions. That is, the API does not require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request.</p> <p> <b>Signing Requests</b> </p> <p>When you send HTTP requests to AWS, you must sign the requests so that AWS can identify who sent them. You sign requests with your AWS access key, which consists of an access key ID and a secret access key. We strongly recommend that you do not create an access key for your root account. Anyone who has the access key for your root account has unrestricted access to all the resources in your account. Instead, create an access key for an IAM user account that has administrative privileges. As another option, use AWS Security Token Service to generate temporary security credentials, and use those credentials to sign requests. </p> <p>To sign requests, we recommend that you use <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>. If you have an existing application that uses Signature Version 2, you do not have to update it to use Signature Version 4. However, some operations now require Signature Version 4. The documentation for operations that require version 4 indicate this requirement. </p> <p>When you use the AWS Command Line Interface (AWS CLI) or one of the AWS SDKs to make requests to AWS, these tools automatically sign the requests for you with the access key that you specify when you configure the tools.</p> <p>In this release, each organization can have only one root. In a future release, a single organization will support multiple roots.</p> <p> <b>Support and Feedback for AWS Organizations</b> </p> <p>We welcome your feedback. Send your comments to <a href="mailto:feedback-awsorganizations@amazon.com">feedback-awsorganizations@amazon.com</a> or post your feedback and questions in the <a href="http://forums.aws.amazon.com/forum.jspa?forumID=219">AWS Organizations support forum</a>. For more information about the AWS support forums, see <a href="http://forums.aws.amazon.com/help.jspa">Forums Help</a>.</p> <p> <b>Endpoint to Call When Using the CLI or the AWS API</b> </p> <p>For the current release of Organizations, you must specify the <code>us-east-1</code> region for all AWS API and CLI calls. You can do this in the CLI by using these parameters and commands:</p> <ul> <li> <p>Use the following parameter with each command to specify both the endpoint and its region:</p> <p> <code>--endpoint-url https://organizations.us-east-1.amazonaws.com</code> </p> </li> <li> <p>Use the default endpoint, but configure your default region with this command:</p> <p> <code>aws configure set default.region us-east-1</code> </p> </li> <li> <p>Use the following parameter with each command to specify the endpoint:</p> <p> <code>--region us-east-1</code> </p> </li> </ul> <p>For the various SDKs used to call the APIs, see the documentation for the SDK of interest to learn how to direct the requests to a specific endpoint. For more information, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region">Regions and Endpoints</a> in the <i>AWS General Reference</i>. </p> <p> <b>How examples are presented</b> </p> <p>The JSON returned by the AWS Organizations service as response to your requests is returned as a single long string without line breaks or formatting whitespace. Both line breaks and whitespace are included in the examples in this guide to improve readability. When example input parameters also would result in long strings that would extend beyond the screen, we insert line breaks to enhance readability. You should always submit the input as a single JSON text string.</p> <p> <b>Recording API Requests</b> </p> <p>AWS Organizations supports AWS CloudTrail, a service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information collected by AWS CloudTrail, you can determine which requests were successfully made to Organizations, who made the request, when it was made, and so on. For more about AWS Organizations and its support for AWS CloudTrail, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_cloudtrail-integration.html">Logging AWS Organizations Events with AWS CloudTrail</a> in the <i>AWS Organizations User Guide</i>. To learn more about CloudTrail, including how to turn it on and find your log files, see the <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html">AWS CloudTrail User Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptHandshake`

``` purescript
acceptHandshake :: forall eff. AcceptHandshakeRequest -> Aff (err :: RequestError | eff) AcceptHandshakeResponse
```

<p>Sends a response to the originator of a handshake agreeing to the action proposed by the handshake request. </p> <p>This operation can be called only by the following principals when they also have the relevant IAM permissions:</p> <ul> <li> <p> <b>Invitation to join</b> or <b>Approve all features request</b> handshakes: only a principal from the member account. </p> <p>The user who calls the API for an invitation to join must have the <code>organizations:AcceptHandshake</code> permission. If you enabled all features in the organization, then the user must also have the <code>iam:CreateServiceLinkedRole</code> permission so that Organizations can create the required service-linked role named <i>OrgsServiceLinkedRoleName</i>. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles">AWS Organizations and Service-Linked Roles</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p> <b>Enable all features final confirmation</b> handshake: only a principal from the master account.</p> <p>For more information about invitations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html">Inviting an AWS Account to Join Your Organization</a> in the <i>AWS Organizations User Guide</i>. For more information about requests to enable all features in the organization, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> </li> </ul> <p>After you accept a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>

#### `attachPolicy`

``` purescript
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches a policy to a root, an organizational unit, or an individual account. How the policy affects accounts depends on the type of policy:</p> <ul> <li> <p> <b>Service control policy (SCP)</b> - An SCP specifies what permissions can be delegated to users in affected member accounts. The scope of influence for a policy depends on what you attach the policy to:</p> <ul> <li> <p>If you attach an SCP to a root, it affects all accounts in the organization.</p> </li> <li> <p>If you attach an SCP to an OU, it affects all accounts in that OU and in any child OUs.</p> </li> <li> <p>If you attach the policy directly to an account, then it affects only that account.</p> </li> </ul> <p>SCPs essentially are permission "filters". When you attach one SCP to a higher level root or OU, and you also attach a different SCP to a child OU or to an account, the child policy can further restrict only the permissions that pass through the parent filter and are available to the child. An SCP that is attached to a child cannot grant a permission that is not already granted by the parent. For example, imagine that the parent SCP allows permissions A, B, C, D, and E. The child SCP allows C, D, E, F, and G. The result is that the accounts affected by the child SCP are allowed to use only C, D, and E. They cannot use A or B because they were filtered out by the child OU. They also cannot use F and G because they were filtered out by the parent OU. They cannot be granted back by the child SCP; child SCPs can only filter the permissions they receive from the parent SCP.</p> <p>AWS Organizations attaches a default SCP named <code>"FullAWSAccess</code> to every root, OU, and account. This default SCP allows all services and actions, enabling any new child OU or account to inherit the permissions of the parent root or OU. If you detach the default policy, you must replace it with a policy that specifies the permissions that you want to allow in that OU or account.</p> <p>For more information about how Organizations policies permissions work, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html">Using Service Control Policies</a> in the <i>AWS Organizations User Guide</i>.</p> </li> </ul> <p>This operation can be called only from the organization's master account.</p>

#### `cancelHandshake`

``` purescript
cancelHandshake :: forall eff. CancelHandshakeRequest -> Aff (err :: RequestError | eff) CancelHandshakeResponse
```

<p>Cancels a handshake. Canceling a handshake sets the handshake state to <code>CANCELED</code>. </p> <p>This operation can be called only from the account that originated the handshake. The recipient of the handshake can't cancel it, but can use <a>DeclineHandshake</a> instead. After a handshake is canceled, the recipient can no longer respond to that handshake.</p> <p>After you cancel a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>

#### `createAccount`

``` purescript
createAccount :: forall eff. CreateAccountRequest -> Aff (err :: RequestError | eff) CreateAccountResponse
```

<p>Creates an AWS account that is automatically a member of the organization whose credentials made the request. This is an asynchronous request that AWS performs in the background. If you want to check the status of the request later, you need the <code>OperationId</code> response element from this operation to provide as a parameter to the <a>DescribeCreateAccountStatus</a> operation.</p> <p>The user who calls the API for an invitation to join must have the <code>organizations:CreateAccount</code> permission. If you enabled all features in the organization, then the user must also have the <code>iam:CreateServiceLinkedRole</code> permission so that Organizations can create the required service-linked role named <i>OrgsServiceLinkedRoleName</i>. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles">AWS Organizations and Service-Linked Roles</a> in the <i>AWS Organizations User Guide</i>.</p> <p>The user in the master account who calls this API must also have the <code>iam:CreateRole</code> permission because AWS Organizations preconfigures the new member account with a role (named <code>OrganizationAccountAccessRole</code> by default) that grants users in the master account administrator permissions in the new member account. Principals in the master account can assume the role. AWS Organizations clones the company name and address information for the new account from the organization's master account.</p> <p>This operation can be called only from the organization's master account.</p> <p>For more information about creating accounts, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html">Creating an AWS Account in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> <important> <p>When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required for the account to operate as a standalone account, such as a payment method and signing the End User Licence Agreement (EULA) is <i>not</i> automatically collected. If you must remove an account from your organization later, you can do so only after you provide the missing information. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </important> <note> <p>When you create a member account with this operation, you can choose whether to create the account with the <b>IAM User and Role Access to Billing Information</b> switch enabled. If you enable it, IAM users and roles that have appropriate permissions can view billing information for the account. If you disable this, then only the account root user can access billing information. For information about how to disable this for an account, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html">Granting Access to Your Billing Information and Tools</a>.</p> </note> <p>This operation can be called only from the organization's master account.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important>

#### `createOrganization`

``` purescript
createOrganization :: forall eff. CreateOrganizationRequest -> Aff (err :: RequestError | eff) CreateOrganizationResponse
```

<p>Creates an AWS organization. The account whose user is calling the CreateOrganization operation automatically becomes the <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/orgs_getting-started_concepts.html#account">master account</a> of the new organization.</p> <p>This operation must be called using credentials from the account that is to become the new organization's master account. The principal must also have the relevant IAM permissions.</p> <p>By default (or if you set the <code>FeatureSet</code> parameter to <code>ALL</code>), the new organization is created with all features enabled and service control policies automatically enabled in the root. If you instead choose to create the organization supporting only the consolidated billing features by setting the <code>FeatureSet</code> parameter to <code>CONSOLIDATED_BILLING"</code>, then no policy types are enabled by default and you cannot use organization policies.</p>

#### `createOrganizationalUnit`

``` purescript
createOrganizationalUnit :: forall eff. CreateOrganizationalUnitRequest -> Aff (err :: RequestError | eff) CreateOrganizationalUnitResponse
```

<p>Creates an organizational unit (OU) within a root or parent OU. An OU is a container for accounts that enables you to organize your accounts to apply policies according to your business requirements. The number of levels deep that you can nest OUs is dependent upon the policy types enabled for that root. For service control policies, the limit is five. </p> <p>For more information about OUs, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html">Managing Organizational Units</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>

#### `createPolicy`

``` purescript
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: RequestError | eff) CreatePolicyResponse
```

<p>Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.</p> <p>For more information about policies and their use, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html">Managing Organization Policies</a>.</p> <p>This operation can be called only from the organization's master account.</p>

#### `declineHandshake`

``` purescript
declineHandshake :: forall eff. DeclineHandshakeRequest -> Aff (err :: RequestError | eff) DeclineHandshakeResponse
```

<p>Declines a handshake request. This sets the handshake state to <code>DECLINED</code> and effectively deactivates the request.</p> <p>This operation can be called only from the account that received the handshake. The originator of the handshake can use <a>CancelHandshake</a> instead. The originator can't reactivate a declined request, but can re-initiate the process with a new handshake request.</p> <p>After you decline a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>

#### `deleteOrganization`

``` purescript
deleteOrganization :: forall eff. Aff (err :: RequestError | eff) Unit
```

<p>Deletes the organization. You can delete an organization only by using credentials from the master account. The organization must be empty of member accounts, OUs, and policies.</p>

#### `deleteOrganizationalUnit`

``` purescript
deleteOrganizationalUnit :: forall eff. DeleteOrganizationalUnitRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an organizational unit from a root or another OU. You must first remove all accounts and child OUs from the OU that you want to delete.</p> <p>This operation can be called only from the organization's master account.</p>

#### `deletePolicy`

``` purescript
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified policy from your organization. Before you perform this operation, you must first detach the policy from all OUs, roots, and accounts.</p> <p>This operation can be called only from the organization's master account.</p>

#### `describeAccount`

``` purescript
describeAccount :: forall eff. DescribeAccountRequest -> Aff (err :: RequestError | eff) DescribeAccountResponse
```

<p>Retrieves Organizations-related information about the specified account.</p> <p>This operation can be called only from the organization's master account.</p>

#### `describeCreateAccountStatus`

``` purescript
describeCreateAccountStatus :: forall eff. DescribeCreateAccountStatusRequest -> Aff (err :: RequestError | eff) DescribeCreateAccountStatusResponse
```

<p>Retrieves the current status of an asynchronous request to create an account.</p> <p>This operation can be called only from the organization's master account.</p>

#### `describeHandshake`

``` purescript
describeHandshake :: forall eff. DescribeHandshakeRequest -> Aff (err :: RequestError | eff) DescribeHandshakeResponse
```

<p>Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original <a>InviteAccountToOrganization</a> operation that generated the handshake.</p> <p>You can access handshakes that are ACCEPTED, DECLINED, or CANCELED for only 30 days after they change to that state. They are then deleted and no longer accessible.</p> <p>This operation can be called from any account in the organization.</p>

#### `describeOrganization`

``` purescript
describeOrganization :: forall eff. Aff (err :: RequestError | eff) DescribeOrganizationResponse
```

<p>Retrieves information about the organization that the user's account belongs to.</p> <p>This operation can be called from any account in the organization.</p>

#### `describeOrganizationalUnit`

``` purescript
describeOrganizationalUnit :: forall eff. DescribeOrganizationalUnitRequest -> Aff (err :: RequestError | eff) DescribeOrganizationalUnitResponse
```

<p>Retrieves information about an organizational unit (OU).</p> <p>This operation can be called only from the organization's master account.</p>

#### `describePolicy`

``` purescript
describePolicy :: forall eff. DescribePolicyRequest -> Aff (err :: RequestError | eff) DescribePolicyResponse
```

<p>Retrieves information about a policy.</p> <p>This operation can be called only from the organization's master account.</p>

#### `detachPolicy`

``` purescript
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a policy from a target root, organizational unit, or account. If the policy being detached is a service control policy (SCP), the changes to permissions for IAM users and roles in affected accounts are immediate.</p> <p> <b>Note:</b> Every root, OU, and account must have at least one SCP attached. If you want to replace the default <code>FullAWSAccess</code> policy with one that limits the permissions that can be delegated, then you must attach the replacement policy before you can remove the default one. This is the authorization strategy of <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_whitelist">whitelisting</a>. If you instead attach a second SCP and leave the <code>FullAWSAccess</code> SCP still attached, and specify <code>"Effect": "Deny"</code> in the second SCP to override the <code>"Effect": "Allow"</code> in the <code>FullAWSAccess</code> policy (or any other attached SCP), then you are using the authorization strategy of <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_blacklist">blacklisting</a>. </p> <p>This operation can be called only from the organization's master account.</p>

#### `disableAWSServiceAccess`

``` purescript
disableAWSServiceAccess :: forall eff. DisableAWSServiceAccessRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disables the integration of an AWS service (the service that is specified by <code>ServicePrincipal</code>) with AWS Organizations. When you disable integration, the specified service no longer can create a <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html">service-linked role</a> in <i>new</i> accounts in your organization. This means the service can't perform operations on your behalf on any new accounts in your organization. The service can still perform operations in older accounts until the service completes its clean-up from AWS Organizations.</p> <p/> <important> <p>We recommend that you disable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the other service is aware that it can clean up any resources that are required only for the integration. How the service cleans up its resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.</p> </important> <p>After you perform the <code>DisableAWSServiceAccess</code> operation, the specified service can no longer perform operations in your organization's accounts unless the operations are explicitly permitted by the IAM policies that are attached to your roles. </p> <p>For more information about integrating other services with AWS Organizations, including the list of services that work with Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>

#### `disablePolicyType`

``` purescript
disablePolicyType :: forall eff. DisablePolicyTypeRequest -> Aff (err :: RequestError | eff) DisablePolicyTypeResponse
```

<p>Disables an organizational control policy type in a root. A policy of a certain type can be attached to entities in a root only if that type is enabled in the root. After you perform this operation, you no longer can attach policies of the specified type to that root or to any OU or account in that root. You can undo this by using the <a>EnablePolicyType</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>

#### `enableAWSServiceAccess`

``` purescript
enableAWSServiceAccess :: forall eff. EnableAWSServiceAccessRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables the integration of an AWS service (the service that is specified by <code>ServicePrincipal</code>) with AWS Organizations. When you enable integration, you allow the specified service to create a <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html">service-linked role</a> in all the accounts in your organization. This allows the service to perform operations on your behalf in your organization and its accounts.</p> <important> <p>We recommend that you enable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the service is aware that it can create the resources that are required for the integration. How the service creates those resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.</p> </important> <p>For more information about enabling services to integrate with AWS Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account and only if the organization has <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">enabled all features</a>.</p>

#### `enableAllFeatures`

``` purescript
enableAllFeatures :: forall eff. EnableAllFeaturesRequest -> Aff (err :: RequestError | eff) EnableAllFeaturesResponse
```

<p>Enables all features in an organization. This enables the use of organization policies that can restrict the services and actions that can be called in each account. Until you enable all features, you have access only to consolidated billing, and you can't use any of the advanced account administration features that AWS Organizations supports. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> <important> <p>This operation is required only for organizations that were created explicitly with only the consolidated billing features enabled, or that were migrated from a Consolidated Billing account family to Organizations. Calling this operation sends a handshake to every invited account in the organization. The feature set change can be finalized and the additional features enabled only after all administrators in the invited accounts approve the change by accepting the handshake.</p> </important> <p>After all invited member accounts accept the handshake, you finalize the feature set change by accepting the handshake that contains <code>"Action": "ENABLE_ALL_FEATURES"</code>. This completes the change.</p> <p>After you enable all features in your organization, the master account in the organization can apply policies on all member accounts. These policies can restrict what users and even administrators in those accounts can do. The master account can apply policies that prevent accounts from leaving the organization. Ensure that your account administrators are aware of this.</p> <p>This operation can be called only from the organization's master account. </p>

#### `enablePolicyType`

``` purescript
enablePolicyType :: forall eff. EnablePolicyTypeRequest -> Aff (err :: RequestError | eff) EnablePolicyTypeResponse
```

<p>Enables a policy type in a root. After you enable a policy type in a root, you can attach policies of that type to the root, any OU, or account in that root. You can undo this by using the <a>DisablePolicyType</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>

#### `inviteAccountToOrganization`

``` purescript
inviteAccountToOrganization :: forall eff. InviteAccountToOrganizationRequest -> Aff (err :: RequestError | eff) InviteAccountToOrganizationResponse
```

<p>Sends an invitation to another account to join your organization as a member account. Organizations sends email on your behalf to the email address that is associated with the other account's owner. The invitation is implemented as a <a>Handshake</a> whose details are in the response.</p> <important> <p>You can invite AWS accounts only from the same seller as the master account. For example, if your organization's master account was created by Amazon Internet Services Pvt. Ltd (AISPL), an AWS seller in India, then you can only invite other AISPL accounts to your organization. You can't combine accounts from AISPL and AWS, or any other AWS seller. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/useconsolidatedbilliing-India.html">Consolidated Billing in India</a>.</p> </important> <p>This operation can be called only from the organization's master account.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important>

#### `leaveOrganization`

``` purescript
leaveOrganization :: forall eff. Aff (err :: RequestError | eff) Unit
```

<p>Removes a member account from its parent organization. This version of the operation is performed by the account that wants to leave. To remove a member account as a user in the master account, use <a>RemoveAccountFromOrganization</a> instead.</p> <p>This operation can be called only from a member account in the organization.</p> <important> <ul> <li> <p>The master account in an organization with all features enabled can set service control policies (SCPs) that can restrict what administrators of member accounts can do, including preventing them from successfully calling <code>LeaveOrganization</code> and leaving the organization. </p> </li> <li> <p>You can leave an organization as a member account only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is <i>not</i> automatically collected. For each account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>You can leave an organization only after you enable IAM user access to billing in your account. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate">Activating Access to the Billing and Cost Management Console</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> </li> </ul> </important>

#### `listAWSServiceAccessForOrganization`

``` purescript
listAWSServiceAccessForOrganization :: forall eff. ListAWSServiceAccessForOrganizationRequest -> Aff (err :: RequestError | eff) ListAWSServiceAccessForOrganizationResponse
```

<p>Returns a list of the AWS services that you enabled to integrate with your organization. After a service on this list creates the resources that it requires for the integration, it can perform operations on your organization and its accounts.</p> <p>For more information about integrating other services with AWS Organizations, including the list of services that currently work with Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listAccounts`

``` purescript
listAccounts :: forall eff. ListAccountsRequest -> Aff (err :: RequestError | eff) ListAccountsResponse
```

<p>Lists all the accounts in the organization. To request only the accounts in a root or OU, use the <a>ListAccountsForParent</a> operation instead.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listAccountsForParent`

``` purescript
listAccountsForParent :: forall eff. ListAccountsForParentRequest -> Aff (err :: RequestError | eff) ListAccountsForParentResponse
```

<p>Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU). If you specify the root, you get a list of all the accounts that are not in any OU. If you specify an OU, you get a list of all the accounts in only that OU, and not in any child OUs. To get a list of all accounts in the organization, use the <a>ListAccounts</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listChildren`

``` purescript
listChildren :: forall eff. ListChildrenRequest -> Aff (err :: RequestError | eff) ListChildrenResponse
```

<p>Lists all of the OUs or accounts that are contained in the specified parent OU or root. This operation, along with <a>ListParents</a> enables you to traverse the tree structure that makes up this root.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listCreateAccountStatus`

``` purescript
listCreateAccountStatus :: forall eff. ListCreateAccountStatusRequest -> Aff (err :: RequestError | eff) ListCreateAccountStatusResponse
```

<p>Lists the account creation requests that match the specified status that is currently being tracked for the organization.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listHandshakesForAccount`

``` purescript
listHandshakesForAccount :: forall eff. ListHandshakesForAccountRequest -> Aff (err :: RequestError | eff) ListHandshakesForAccountResponse
```

<p>Lists the current handshakes that are associated with the account of the requesting user.</p> <p>Handshakes that are ACCEPTED, DECLINED, or CANCELED appear in the results of this API for only 30 days after changing to that state. After that they are deleted and no longer accessible.</p> <p>This operation can be called from any account in the organization.</p>

#### `listHandshakesForOrganization`

``` purescript
listHandshakesForOrganization :: forall eff. ListHandshakesForOrganizationRequest -> Aff (err :: RequestError | eff) ListHandshakesForOrganizationResponse
```

<p>Lists the handshakes that are associated with the organization that the requesting user is part of. The <code>ListHandshakesForOrganization</code> operation returns a list of handshake structures. Each structure contains details and status about a handshake.</p> <p>Handshakes that are ACCEPTED, DECLINED, or CANCELED appear in the results of this API for only 30 days after changing to that state. After that they are deleted and no longer accessible.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listOrganizationalUnitsForParent`

``` purescript
listOrganizationalUnitsForParent :: forall eff. ListOrganizationalUnitsForParentRequest -> Aff (err :: RequestError | eff) ListOrganizationalUnitsForParentResponse
```

<p>Lists the organizational units (OUs) in a parent organizational unit or root.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listParents`

``` purescript
listParents :: forall eff. ListParentsRequest -> Aff (err :: RequestError | eff) ListParentsResponse
```

<p>Lists the root or organizational units (OUs) that serve as the immediate parent of the specified child OU or account. This operation, along with <a>ListChildren</a> enables you to traverse the tree structure that makes up this root.</p> <p>This operation can be called only from the organization's master account.</p> <note> <p>In the current release, a child can have only a single parent. </p> </note>

#### `listPolicies`

``` purescript
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: RequestError | eff) ListPoliciesResponse
```

<p>Retrieves the list of all policies in an organization of a specified type.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listPoliciesForTarget`

``` purescript
listPoliciesForTarget :: forall eff. ListPoliciesForTargetRequest -> Aff (err :: RequestError | eff) ListPoliciesForTargetResponse
```

<p>Lists the policies that are directly attached to the specified target root, organizational unit (OU), or account. You must specify the policy type that you want included in the returned list.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listRoots`

``` purescript
listRoots :: forall eff. ListRootsRequest -> Aff (err :: RequestError | eff) ListRootsResponse
```

<p>Lists the roots that are defined in the current organization.</p> <p>This operation can be called only from the organization's master account.</p>

#### `listTargetsForPolicy`

``` purescript
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (err :: RequestError | eff) ListTargetsForPolicyResponse
```

<p>Lists all the roots, OUs, and accounts to which the specified policy is attached.</p> <p>This operation can be called only from the organization's master account.</p>

#### `moveAccount`

``` purescript
moveAccount :: forall eff. MoveAccountRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Moves an account from its current source parent root or OU to the specified destination parent root or OU.</p> <p>This operation can be called only from the organization's master account.</p>

#### `removeAccountFromOrganization`

``` purescript
removeAccountFromOrganization :: forall eff. RemoveAccountFromOrganizationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified account from the organization.</p> <p>The removed account becomes a stand-alone account that is not a member of any organization. It is no longer subject to any policies and is responsible for its own bill payments. The organization's master account is no longer charged for any expenses accrued by the member account after it is removed from the organization.</p> <p>This operation can be called only from the organization's master account. Member accounts can remove themselves with <a>LeaveOrganization</a> instead.</p> <important> <ul> <li> <p>You can remove an account from your organization only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is <i>not</i> automatically collected. For an account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. To remove an account that does not yet have this information, you must sign in as the member account and follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>You can remove a member account only after you enable IAM user access to billing in the member account. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate">Activating Access to the Billing and Cost Management Console</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> </li> </ul> </important>

#### `updateOrganizationalUnit`

``` purescript
updateOrganizationalUnit :: forall eff. UpdateOrganizationalUnitRequest -> Aff (err :: RequestError | eff) UpdateOrganizationalUnitResponse
```

<p>Renames the specified organizational unit (OU). The ID and ARN do not change. The child OUs and accounts remain in place, and any attached policies of the OU remain attached. </p> <p>This operation can be called only from the organization's master account.</p>

#### `updatePolicy`

``` purescript
updatePolicy :: forall eff. UpdatePolicyRequest -> Aff (err :: RequestError | eff) UpdatePolicyResponse
```

<p>Updates an existing policy with a new name, description, or content. If any parameter is not supplied, that value remains unchanged. Note that you cannot change a policy's type.</p> <p>This operation can be called only from the organization's master account.</p>

#### `AWSOrganizationsNotInUseException`

``` purescript
newtype AWSOrganizationsNotInUseException
  = AWSOrganizationsNotInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>Your account is not a member of an organization. To make this request, you must use the credentials of an account that belongs to an organization.</p>

##### Instances
``` purescript
Newtype AWSOrganizationsNotInUseException _
```

#### `AcceptHandshakeRequest`

``` purescript
newtype AcceptHandshakeRequest
  = AcceptHandshakeRequest { "HandshakeId" :: HandshakeId }
```

##### Instances
``` purescript
Newtype AcceptHandshakeRequest _
```

#### `AcceptHandshakeResponse`

``` purescript
newtype AcceptHandshakeResponse
  = AcceptHandshakeResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype AcceptHandshakeResponse _
```

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>You don't have permissions to perform the requested operation. The user or role that is making the request must have at least one IAM permissions policy attached that grants the required permissions. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html">Access Management</a> in the <i>IAM User Guide</i>.</p>

##### Instances
``` purescript
Newtype AccessDeniedException _
```

#### `AccessDeniedForDependencyException`

``` purescript
newtype AccessDeniedForDependencyException
  = AccessDeniedForDependencyException { "Message" :: NullOrUndefined (ExceptionMessage), "Reason" :: NullOrUndefined (AccessDeniedForDependencyExceptionReason) }
```

<p>The operation you attempted requires you to have the <code>iam:CreateServiceLinkedRole</code> so that Organizations can create the required service-linked role. You do not have that permission.</p>

##### Instances
``` purescript
Newtype AccessDeniedForDependencyException _
```

#### `AccessDeniedForDependencyExceptionReason`

``` purescript
newtype AccessDeniedForDependencyExceptionReason
  = AccessDeniedForDependencyExceptionReason String
```

##### Instances
``` purescript
Newtype AccessDeniedForDependencyExceptionReason _
```

#### `Account`

``` purescript
newtype Account
  = Account { "Id" :: NullOrUndefined (AccountId), "Arn" :: NullOrUndefined (AccountArn), "Email" :: NullOrUndefined (Email), "Name" :: NullOrUndefined (AccountName), "Status" :: NullOrUndefined (AccountStatus), "JoinedMethod" :: NullOrUndefined (AccountJoinedMethod), "JoinedTimestamp" :: NullOrUndefined (Number) }
```

<p>Contains information about an AWS account that is a member of an organization.</p>

##### Instances
``` purescript
Newtype Account _
```

#### `AccountArn`

``` purescript
newtype AccountArn
  = AccountArn String
```

##### Instances
``` purescript
Newtype AccountArn _
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

##### Instances
``` purescript
Newtype AccountId _
```

#### `AccountJoinedMethod`

``` purescript
newtype AccountJoinedMethod
  = AccountJoinedMethod String
```

##### Instances
``` purescript
Newtype AccountJoinedMethod _
```

#### `AccountName`

``` purescript
newtype AccountName
  = AccountName String
```

##### Instances
``` purescript
Newtype AccountName _
```

#### `AccountNotFoundException`

``` purescript
newtype AccountNotFoundException
  = AccountNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p> We can't find an AWS account with the AccountId that you specified, or the account whose credentials you used to make this request is not a member of an organization.</p>

##### Instances
``` purescript
Newtype AccountNotFoundException _
```

#### `AccountStatus`

``` purescript
newtype AccountStatus
  = AccountStatus String
```

##### Instances
``` purescript
Newtype AccountStatus _
```

#### `Accounts`

``` purescript
newtype Accounts
  = Accounts (Array Account)
```

##### Instances
``` purescript
Newtype Accounts _
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

#### `AlreadyInOrganizationException`

``` purescript
newtype AlreadyInOrganizationException
  = AlreadyInOrganizationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>This account is already a member of an organization. An account can belong to only one organization at a time.</p>

##### Instances
``` purescript
Newtype AlreadyInOrganizationException _
```

#### `AttachPolicyRequest`

``` purescript
newtype AttachPolicyRequest
  = AttachPolicyRequest { "PolicyId" :: PolicyId, "TargetId" :: PolicyTargetId }
```

##### Instances
``` purescript
Newtype AttachPolicyRequest _
```

#### `AwsManagedPolicy`

``` purescript
newtype AwsManagedPolicy
  = AwsManagedPolicy Boolean
```

##### Instances
``` purescript
Newtype AwsManagedPolicy _
```

#### `CancelHandshakeRequest`

``` purescript
newtype CancelHandshakeRequest
  = CancelHandshakeRequest { "HandshakeId" :: HandshakeId }
```

##### Instances
``` purescript
Newtype CancelHandshakeRequest _
```

#### `CancelHandshakeResponse`

``` purescript
newtype CancelHandshakeResponse
  = CancelHandshakeResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype CancelHandshakeResponse _
```

#### `Child`

``` purescript
newtype Child
  = Child { "Id" :: NullOrUndefined (ChildId), "Type" :: NullOrUndefined (ChildType) }
```

<p>Contains a list of child entities, either OUs or accounts.</p>

##### Instances
``` purescript
Newtype Child _
```

#### `ChildId`

``` purescript
newtype ChildId
  = ChildId String
```

##### Instances
``` purescript
Newtype ChildId _
```

#### `ChildNotFoundException`

``` purescript
newtype ChildNotFoundException
  = ChildNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find an organizational unit (OU) or AWS account with the ChildId that you specified.</p>

##### Instances
``` purescript
Newtype ChildNotFoundException _
```

#### `ChildType`

``` purescript
newtype ChildType
  = ChildType String
```

##### Instances
``` purescript
Newtype ChildType _
```

#### `Children`

``` purescript
newtype Children
  = Children (Array Child)
```

##### Instances
``` purescript
Newtype Children _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The target of the operation is currently being modified by a different request. Try again later.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `ConstraintViolationException`

``` purescript
newtype ConstraintViolationException
  = ConstraintViolationException { "Message" :: NullOrUndefined (ExceptionMessage), "Reason" :: NullOrUndefined (ConstraintViolationExceptionReason) }
```

<p>Performing this operation violates a minimum or maximum value limit. For example, attempting to removing the last SCP from an OU or root, inviting or creating too many accounts to the organization, or attaching too many policies to an account, OU, or root. This exception includes a reason that contains additional information about the violated limit:</p> <p/> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. If you need more accounts, contact AWS Support to request an increase in your limit. </p> <p>Or, The number of invitations that you tried to send would cause you to exceed the limit of accounts in your organization. Send fewer invitations, or contact AWS Support to request an increase in the number of accounts.</p> <p> <b>Note</b>: deleted and closed accounts still count toward your limit.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important> </li> <li> <p>HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes you can send in one day.</p> </li> <li> <p>OU_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of organizational units you can have in an organization.</p> </li> <li> <p>OU_DEPTH_LIMIT_EXCEEDED: You attempted to create an organizational unit tree that is too many levels deep.</p> </li> <li> <p>POLICY_NUMBER_LIMIT_EXCEEDED. You attempted to exceed the number of policies that you can have in an organization.</p> </li> <li> <p>MAX_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to exceed the number of policies of a certain type that can be attached to an entity at one time.</p> </li> <li> <p>MIN_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to detach a policy from an entity that would cause the entity to have fewer than the minimum number of policies of a certain type required.</p> </li> <li> <p>ACCOUNT_CANNOT_LEAVE_WITHOUT_EULA: You attempted to remove an account from the organization that does not yet have enough information to exist as a stand-alone account. This account requires you to first agree to the AWS Customer Agreement. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>ACCOUNT_CANNOT_LEAVE_WITHOUT_PHONE_VERIFICATION: You attempted to remove an account from the organization that does not yet have enough information to exist as a stand-alone account. This account requires you to first complete phone verification. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>MASTER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To create an organization with this account, you first must associate a payment instrument, such as a credit card, with the account. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>MEMBER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To complete this operation with this member account, you first must associate a payment instrument, such as a credit card, with the account. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>ACCOUNT_CREATION_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of accounts that you can create in one day.</p> </li> <li> <p>MASTER_ACCOUNT_ADDRESS_DOES_NOT_MATCH_MARKETPLACE: To create an account in this organization, you first must migrate the organization's master account to the marketplace that corresponds to the master account's address. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be associated with the same marketplace.</p> </li> <li> <p>MASTER_ACCOUNT_MISSING_CONTACT_INFO: To complete this operation, you must first provide contact a valid address and phone number for the master account. Then try the operation again.</p> </li> </ul>

##### Instances
``` purescript
Newtype ConstraintViolationException _
```

#### `ConstraintViolationExceptionReason`

``` purescript
newtype ConstraintViolationExceptionReason
  = ConstraintViolationExceptionReason String
```

##### Instances
``` purescript
Newtype ConstraintViolationExceptionReason _
```

#### `CreateAccountFailureReason`

``` purescript
newtype CreateAccountFailureReason
  = CreateAccountFailureReason String
```

##### Instances
``` purescript
Newtype CreateAccountFailureReason _
```

#### `CreateAccountRequest`

``` purescript
newtype CreateAccountRequest
  = CreateAccountRequest { "Email" :: Email, "AccountName" :: AccountName, "RoleName" :: NullOrUndefined (RoleName), "IamUserAccessToBilling" :: NullOrUndefined (IAMUserAccessToBilling) }
```

##### Instances
``` purescript
Newtype CreateAccountRequest _
```

#### `CreateAccountRequestId`

``` purescript
newtype CreateAccountRequestId
  = CreateAccountRequestId String
```

##### Instances
``` purescript
Newtype CreateAccountRequestId _
```

#### `CreateAccountResponse`

``` purescript
newtype CreateAccountResponse
  = CreateAccountResponse { "CreateAccountStatus" :: NullOrUndefined (CreateAccountStatus) }
```

##### Instances
``` purescript
Newtype CreateAccountResponse _
```

#### `CreateAccountState`

``` purescript
newtype CreateAccountState
  = CreateAccountState String
```

##### Instances
``` purescript
Newtype CreateAccountState _
```

#### `CreateAccountStates`

``` purescript
newtype CreateAccountStates
  = CreateAccountStates (Array CreateAccountState)
```

##### Instances
``` purescript
Newtype CreateAccountStates _
```

#### `CreateAccountStatus`

``` purescript
newtype CreateAccountStatus
  = CreateAccountStatus { "Id" :: NullOrUndefined (CreateAccountRequestId), "AccountName" :: NullOrUndefined (AccountName), "State" :: NullOrUndefined (CreateAccountState), "RequestedTimestamp" :: NullOrUndefined (Number), "CompletedTimestamp" :: NullOrUndefined (Number), "AccountId" :: NullOrUndefined (AccountId), "FailureReason" :: NullOrUndefined (CreateAccountFailureReason) }
```

<p>Contains the status about a <a>CreateAccount</a> request to create an AWS account in an organization.</p>

##### Instances
``` purescript
Newtype CreateAccountStatus _
```

#### `CreateAccountStatusNotFoundException`

``` purescript
newtype CreateAccountStatusNotFoundException
  = CreateAccountStatusNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find an create account request with the CreateAccountRequestId that you specified.</p>

##### Instances
``` purescript
Newtype CreateAccountStatusNotFoundException _
```

#### `CreateAccountStatuses`

``` purescript
newtype CreateAccountStatuses
  = CreateAccountStatuses (Array CreateAccountStatus)
```

##### Instances
``` purescript
Newtype CreateAccountStatuses _
```

#### `CreateOrganizationRequest`

``` purescript
newtype CreateOrganizationRequest
  = CreateOrganizationRequest { "FeatureSet" :: NullOrUndefined (OrganizationFeatureSet) }
```

##### Instances
``` purescript
Newtype CreateOrganizationRequest _
```

#### `CreateOrganizationResponse`

``` purescript
newtype CreateOrganizationResponse
  = CreateOrganizationResponse { "Organization" :: NullOrUndefined (Organization) }
```

##### Instances
``` purescript
Newtype CreateOrganizationResponse _
```

#### `CreateOrganizationalUnitRequest`

``` purescript
newtype CreateOrganizationalUnitRequest
  = CreateOrganizationalUnitRequest { "ParentId" :: ParentId, "Name" :: OrganizationalUnitName }
```

##### Instances
``` purescript
Newtype CreateOrganizationalUnitRequest _
```

#### `CreateOrganizationalUnitResponse`

``` purescript
newtype CreateOrganizationalUnitResponse
  = CreateOrganizationalUnitResponse { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit) }
```

##### Instances
``` purescript
Newtype CreateOrganizationalUnitResponse _
```

#### `CreatePolicyRequest`

``` purescript
newtype CreatePolicyRequest
  = CreatePolicyRequest { "Content" :: PolicyContent, "Description" :: PolicyDescription, "Name" :: PolicyName, "Type" :: PolicyType }
```

##### Instances
``` purescript
Newtype CreatePolicyRequest _
```

#### `CreatePolicyResponse`

``` purescript
newtype CreatePolicyResponse
  = CreatePolicyResponse { "Policy" :: NullOrUndefined (Policy) }
```

##### Instances
``` purescript
Newtype CreatePolicyResponse _
```

#### `DeclineHandshakeRequest`

``` purescript
newtype DeclineHandshakeRequest
  = DeclineHandshakeRequest { "HandshakeId" :: HandshakeId }
```

##### Instances
``` purescript
Newtype DeclineHandshakeRequest _
```

#### `DeclineHandshakeResponse`

``` purescript
newtype DeclineHandshakeResponse
  = DeclineHandshakeResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype DeclineHandshakeResponse _
```

#### `DeleteOrganizationalUnitRequest`

``` purescript
newtype DeleteOrganizationalUnitRequest
  = DeleteOrganizationalUnitRequest { "OrganizationalUnitId" :: OrganizationalUnitId }
```

##### Instances
``` purescript
Newtype DeleteOrganizationalUnitRequest _
```

#### `DeletePolicyRequest`

``` purescript
newtype DeletePolicyRequest
  = DeletePolicyRequest { "PolicyId" :: PolicyId }
```

##### Instances
``` purescript
Newtype DeletePolicyRequest _
```

#### `DescribeAccountRequest`

``` purescript
newtype DescribeAccountRequest
  = DescribeAccountRequest { "AccountId" :: AccountId }
```

##### Instances
``` purescript
Newtype DescribeAccountRequest _
```

#### `DescribeAccountResponse`

``` purescript
newtype DescribeAccountResponse
  = DescribeAccountResponse { "Account" :: NullOrUndefined (Account) }
```

##### Instances
``` purescript
Newtype DescribeAccountResponse _
```

#### `DescribeCreateAccountStatusRequest`

``` purescript
newtype DescribeCreateAccountStatusRequest
  = DescribeCreateAccountStatusRequest { "CreateAccountRequestId" :: CreateAccountRequestId }
```

##### Instances
``` purescript
Newtype DescribeCreateAccountStatusRequest _
```

#### `DescribeCreateAccountStatusResponse`

``` purescript
newtype DescribeCreateAccountStatusResponse
  = DescribeCreateAccountStatusResponse { "CreateAccountStatus" :: NullOrUndefined (CreateAccountStatus) }
```

##### Instances
``` purescript
Newtype DescribeCreateAccountStatusResponse _
```

#### `DescribeHandshakeRequest`

``` purescript
newtype DescribeHandshakeRequest
  = DescribeHandshakeRequest { "HandshakeId" :: HandshakeId }
```

##### Instances
``` purescript
Newtype DescribeHandshakeRequest _
```

#### `DescribeHandshakeResponse`

``` purescript
newtype DescribeHandshakeResponse
  = DescribeHandshakeResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype DescribeHandshakeResponse _
```

#### `DescribeOrganizationResponse`

``` purescript
newtype DescribeOrganizationResponse
  = DescribeOrganizationResponse { "Organization" :: NullOrUndefined (Organization) }
```

##### Instances
``` purescript
Newtype DescribeOrganizationResponse _
```

#### `DescribeOrganizationalUnitRequest`

``` purescript
newtype DescribeOrganizationalUnitRequest
  = DescribeOrganizationalUnitRequest { "OrganizationalUnitId" :: OrganizationalUnitId }
```

##### Instances
``` purescript
Newtype DescribeOrganizationalUnitRequest _
```

#### `DescribeOrganizationalUnitResponse`

``` purescript
newtype DescribeOrganizationalUnitResponse
  = DescribeOrganizationalUnitResponse { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit) }
```

##### Instances
``` purescript
Newtype DescribeOrganizationalUnitResponse _
```

#### `DescribePolicyRequest`

``` purescript
newtype DescribePolicyRequest
  = DescribePolicyRequest { "PolicyId" :: PolicyId }
```

##### Instances
``` purescript
Newtype DescribePolicyRequest _
```

#### `DescribePolicyResponse`

``` purescript
newtype DescribePolicyResponse
  = DescribePolicyResponse { "Policy" :: NullOrUndefined (Policy) }
```

##### Instances
``` purescript
Newtype DescribePolicyResponse _
```

#### `DestinationParentNotFoundException`

``` purescript
newtype DestinationParentNotFoundException
  = DestinationParentNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find the destination container (a root or OU) with the ParentId that you specified.</p>

##### Instances
``` purescript
Newtype DestinationParentNotFoundException _
```

#### `DetachPolicyRequest`

``` purescript
newtype DetachPolicyRequest
  = DetachPolicyRequest { "PolicyId" :: PolicyId, "TargetId" :: PolicyTargetId }
```

##### Instances
``` purescript
Newtype DetachPolicyRequest _
```

#### `DisableAWSServiceAccessRequest`

``` purescript
newtype DisableAWSServiceAccessRequest
  = DisableAWSServiceAccessRequest { "ServicePrincipal" :: ServicePrincipal }
```

##### Instances
``` purescript
Newtype DisableAWSServiceAccessRequest _
```

#### `DisablePolicyTypeRequest`

``` purescript
newtype DisablePolicyTypeRequest
  = DisablePolicyTypeRequest { "RootId" :: RootId, "PolicyType" :: PolicyType }
```

##### Instances
``` purescript
Newtype DisablePolicyTypeRequest _
```

#### `DisablePolicyTypeResponse`

``` purescript
newtype DisablePolicyTypeResponse
  = DisablePolicyTypeResponse { "Root" :: NullOrUndefined (Root) }
```

##### Instances
``` purescript
Newtype DisablePolicyTypeResponse _
```

#### `DuplicateAccountException`

``` purescript
newtype DuplicateAccountException
  = DuplicateAccountException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>That account is already present in the specified destination.</p>

##### Instances
``` purescript
Newtype DuplicateAccountException _
```

#### `DuplicateHandshakeException`

``` purescript
newtype DuplicateHandshakeException
  = DuplicateHandshakeException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A handshake with the same action and target already exists. For example, if you invited an account to join your organization, the invited account might already have a pending invitation from this organization. If you intend to resend an invitation to an account, ensure that existing handshakes that might be considered duplicates are canceled or declined.</p>

##### Instances
``` purescript
Newtype DuplicateHandshakeException _
```

#### `DuplicateOrganizationalUnitException`

``` purescript
newtype DuplicateOrganizationalUnitException
  = DuplicateOrganizationalUnitException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>An organizational unit (OU) with the same name already exists.</p>

##### Instances
``` purescript
Newtype DuplicateOrganizationalUnitException _
```

#### `DuplicatePolicyAttachmentException`

``` purescript
newtype DuplicatePolicyAttachmentException
  = DuplicatePolicyAttachmentException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The selected policy is already attached to the specified target.</p>

##### Instances
``` purescript
Newtype DuplicatePolicyAttachmentException _
```

#### `DuplicatePolicyException`

``` purescript
newtype DuplicatePolicyException
  = DuplicatePolicyException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>A policy with the same name already exists.</p>

##### Instances
``` purescript
Newtype DuplicatePolicyException _
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

##### Instances
``` purescript
Newtype Email _
```

#### `EnableAWSServiceAccessRequest`

``` purescript
newtype EnableAWSServiceAccessRequest
  = EnableAWSServiceAccessRequest { "ServicePrincipal" :: ServicePrincipal }
```

##### Instances
``` purescript
Newtype EnableAWSServiceAccessRequest _
```

#### `EnableAllFeaturesRequest`

``` purescript
newtype EnableAllFeaturesRequest
  = EnableAllFeaturesRequest {  }
```

##### Instances
``` purescript
Newtype EnableAllFeaturesRequest _
```

#### `EnableAllFeaturesResponse`

``` purescript
newtype EnableAllFeaturesResponse
  = EnableAllFeaturesResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype EnableAllFeaturesResponse _
```

#### `EnablePolicyTypeRequest`

``` purescript
newtype EnablePolicyTypeRequest
  = EnablePolicyTypeRequest { "RootId" :: RootId, "PolicyType" :: PolicyType }
```

##### Instances
``` purescript
Newtype EnablePolicyTypeRequest _
```

#### `EnablePolicyTypeResponse`

``` purescript
newtype EnablePolicyTypeResponse
  = EnablePolicyTypeResponse { "Root" :: NullOrUndefined (Root) }
```

##### Instances
``` purescript
Newtype EnablePolicyTypeResponse _
```

#### `EnabledServicePrincipal`

``` purescript
newtype EnabledServicePrincipal
  = EnabledServicePrincipal { "ServicePrincipal" :: NullOrUndefined (ServicePrincipal), "DateEnabled" :: NullOrUndefined (Number) }
```

<p>A structure that contains details of a service principal that is enabled to integrate with AWS Organizations.</p>

##### Instances
``` purescript
Newtype EnabledServicePrincipal _
```

#### `EnabledServicePrincipals`

``` purescript
newtype EnabledServicePrincipals
  = EnabledServicePrincipals (Array EnabledServicePrincipal)
```

##### Instances
``` purescript
Newtype EnabledServicePrincipals _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `ExceptionType`

``` purescript
newtype ExceptionType
  = ExceptionType String
```

##### Instances
``` purescript
Newtype ExceptionType _
```

#### `FinalizingOrganizationException`

``` purescript
newtype FinalizingOrganizationException
  = FinalizingOrganizationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>AWS Organizations could not finalize the creation of your organization. Try again later. If this persists, contact AWS customer support.</p>

##### Instances
``` purescript
Newtype FinalizingOrganizationException _
```

#### `GenericArn`

``` purescript
newtype GenericArn
  = GenericArn String
```

##### Instances
``` purescript
Newtype GenericArn _
```

#### `Handshake`

``` purescript
newtype Handshake
  = Handshake { "Id" :: NullOrUndefined (HandshakeId), "Arn" :: NullOrUndefined (HandshakeArn), "Parties" :: NullOrUndefined (HandshakeParties), "State" :: NullOrUndefined (HandshakeState), "RequestedTimestamp" :: NullOrUndefined (Number), "ExpirationTimestamp" :: NullOrUndefined (Number), "Action" :: NullOrUndefined (ActionType), "Resources" :: NullOrUndefined (HandshakeResources) }
```

<p>Contains information that must be exchanged to securely establish a relationship between two accounts (an <i>originator</i> and a <i>recipient</i>). For example, when a master account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.</p> <p> <b>Note:</b> Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.</p>

##### Instances
``` purescript
Newtype Handshake _
```

#### `HandshakeAlreadyInStateException`

``` purescript
newtype HandshakeAlreadyInStateException
  = HandshakeAlreadyInStateException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified handshake is already in the requested state. For example, you can't accept a handshake that was already accepted.</p>

##### Instances
``` purescript
Newtype HandshakeAlreadyInStateException _
```

#### `HandshakeArn`

``` purescript
newtype HandshakeArn
  = HandshakeArn String
```

##### Instances
``` purescript
Newtype HandshakeArn _
```

#### `HandshakeConstraintViolationException`

``` purescript
newtype HandshakeConstraintViolationException
  = HandshakeConstraintViolationException { "Message" :: NullOrUndefined (ExceptionMessage), "Reason" :: NullOrUndefined (HandshakeConstraintViolationExceptionReason) }
```

<p>The requested operation would violate the constraint identified in the reason code.</p> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. <b>Note</b>: deleted and closed accounts still count toward your limit.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important> </li> <li> <p>HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes you can send in one day.</p> </li> <li> <p>ALREADY_IN_AN_ORGANIZATION: The handshake request is invalid because the invited account is already a member of an organization.</p> </li> <li> <p>ORGANIZATION_ALREADY_HAS_ALL_FEATURES: The handshake request is invalid because the organization has already enabled all features.</p> </li> <li> <p>INVITE_DISABLED_DURING_ENABLE_ALL_FEATURES: You cannot issue new invitations to join an organization while it is in the process of enabling all features. You can resume inviting accounts after you finalize the process when all accounts have agreed to the change.</p> </li> <li> <p>PAYMENT_INSTRUMENT_REQUIRED: You cannot complete the operation with an account that does not have a payment instrument, such as a credit card, associated with it.</p> </li> <li> <p>ORGANIZATION_FROM_DIFFERENT_SELLER_OF_RECORD: The request failed because the account is from a different marketplace than the accounts in the organization. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be from the same marketplace.</p> </li> <li> <p>ORGANIZATION_MEMBERSHIP_CHANGE_RATE_LIMIT_EXCEEDED: You attempted to change the membership of an account too quickly after its previous change.</p> </li> </ul>

##### Instances
``` purescript
Newtype HandshakeConstraintViolationException _
```

#### `HandshakeConstraintViolationExceptionReason`

``` purescript
newtype HandshakeConstraintViolationExceptionReason
  = HandshakeConstraintViolationExceptionReason String
```

##### Instances
``` purescript
Newtype HandshakeConstraintViolationExceptionReason _
```

#### `HandshakeFilter`

``` purescript
newtype HandshakeFilter
  = HandshakeFilter { "ActionType" :: NullOrUndefined (ActionType), "ParentHandshakeId" :: NullOrUndefined (HandshakeId) }
```

<p>Specifies the criteria that are used to select the handshakes for the operation.</p>

##### Instances
``` purescript
Newtype HandshakeFilter _
```

#### `HandshakeId`

``` purescript
newtype HandshakeId
  = HandshakeId String
```

##### Instances
``` purescript
Newtype HandshakeId _
```

#### `HandshakeNotFoundException`

``` purescript
newtype HandshakeNotFoundException
  = HandshakeNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a handshake with the HandshakeId that you specified.</p>

##### Instances
``` purescript
Newtype HandshakeNotFoundException _
```

#### `HandshakeNotes`

``` purescript
newtype HandshakeNotes
  = HandshakeNotes String
```

##### Instances
``` purescript
Newtype HandshakeNotes _
```

#### `HandshakeParties`

``` purescript
newtype HandshakeParties
  = HandshakeParties (Array HandshakeParty)
```

##### Instances
``` purescript
Newtype HandshakeParties _
```

#### `HandshakeParty`

``` purescript
newtype HandshakeParty
  = HandshakeParty { "Id" :: HandshakePartyId, "Type" :: HandshakePartyType }
```

<p>Identifies a participant in a handshake.</p>

##### Instances
``` purescript
Newtype HandshakeParty _
```

#### `HandshakePartyId`

``` purescript
newtype HandshakePartyId
  = HandshakePartyId String
```

##### Instances
``` purescript
Newtype HandshakePartyId _
```

#### `HandshakePartyType`

``` purescript
newtype HandshakePartyType
  = HandshakePartyType String
```

##### Instances
``` purescript
Newtype HandshakePartyType _
```

#### `HandshakeResource`

``` purescript
newtype HandshakeResource
  = HandshakeResource { "Value" :: NullOrUndefined (HandshakeResourceValue), "Type" :: NullOrUndefined (HandshakeResourceType), "Resources" :: NullOrUndefined (HandshakeResources) }
```

<p>Contains additional data that is needed to process a handshake.</p>

##### Instances
``` purescript
Newtype HandshakeResource _
```

#### `HandshakeResourceType`

``` purescript
newtype HandshakeResourceType
  = HandshakeResourceType String
```

##### Instances
``` purescript
Newtype HandshakeResourceType _
```

#### `HandshakeResourceValue`

``` purescript
newtype HandshakeResourceValue
  = HandshakeResourceValue String
```

##### Instances
``` purescript
Newtype HandshakeResourceValue _
```

#### `HandshakeResources`

``` purescript
newtype HandshakeResources
  = HandshakeResources (Array HandshakeResource)
```

##### Instances
``` purescript
Newtype HandshakeResources _
```

#### `HandshakeState`

``` purescript
newtype HandshakeState
  = HandshakeState String
```

##### Instances
``` purescript
Newtype HandshakeState _
```

#### `Handshakes`

``` purescript
newtype Handshakes
  = Handshakes (Array Handshake)
```

##### Instances
``` purescript
Newtype Handshakes _
```

#### `IAMUserAccessToBilling`

``` purescript
newtype IAMUserAccessToBilling
  = IAMUserAccessToBilling String
```

##### Instances
``` purescript
Newtype IAMUserAccessToBilling _
```

#### `InvalidHandshakeTransitionException`

``` purescript
newtype InvalidHandshakeTransitionException
  = InvalidHandshakeTransitionException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>You can't perform the operation on the handshake in its current state. For example, you can't cancel a handshake that was already accepted, or accept a handshake that was already declined.</p>

##### Instances
``` purescript
Newtype InvalidHandshakeTransitionException _
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message" :: NullOrUndefined (ExceptionMessage), "Reason" :: NullOrUndefined (InvalidInputExceptionReason) }
```

<p>The requested operation failed because you provided invalid values for one or more of the request parameters. This exception includes a reason that contains additional information about the violated limit:</p> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>INVALID_PARTY_TYPE_TARGET: You specified the wrong type of entity (account, organization, or email) as a party.</p> </li> <li> <p>INVALID_SYNTAX_ORGANIZATION_ARN: You specified an invalid ARN for the organization.</p> </li> <li> <p>INVALID_SYNTAX_POLICY_ID: You specified an invalid policy ID. </p> </li> <li> <p>INVALID_ENUM: You specified a value that is not valid for that parameter.</p> </li> <li> <p>INVALID_FULL_NAME_TARGET: You specified a full name that contains invalid characters.</p> </li> <li> <p>INVALID_LIST_MEMBER: You provided a list to a parameter that contains at least one invalid value.</p> </li> <li> <p>MAX_LENGTH_EXCEEDED: You provided a string parameter that is longer than allowed.</p> </li> <li> <p>MAX_VALUE_EXCEEDED: You provided a numeric parameter that has a larger value than allowed.</p> </li> <li> <p>MIN_LENGTH_EXCEEDED: You provided a string parameter that is shorter than allowed.</p> </li> <li> <p>MIN_VALUE_EXCEEDED: You provided a numeric parameter that has a smaller value than allowed.</p> </li> <li> <p>IMMUTABLE_POLICY: You specified a policy that is managed by AWS and cannot be modified.</p> </li> <li> <p>INVALID_PATTERN: You provided a value that doesn't match the required pattern.</p> </li> <li> <p>INVALID_PATTERN_TARGET_ID: You specified a policy target ID that doesn't match the required pattern.</p> </li> <li> <p>INPUT_REQUIRED: You must include a value for all required parameters.</p> </li> <li> <p>INVALID_PAGINATION_TOKEN: Get the value for the NextToken parameter from the response to a previous call of the operation.</p> </li> <li> <p>MAX_FILTER_LIMIT_EXCEEDED: You can specify only one filter parameter for the operation.</p> </li> <li> <p>MOVING_ACCOUNT_BETWEEN_DIFFERENT_ROOTS: You can move an account only between entities in the same root.</p> </li> </ul>

##### Instances
``` purescript
Newtype InvalidInputException _
```

#### `InvalidInputExceptionReason`

``` purescript
newtype InvalidInputExceptionReason
  = InvalidInputExceptionReason String
```

##### Instances
``` purescript
Newtype InvalidInputExceptionReason _
```

#### `InviteAccountToOrganizationRequest`

``` purescript
newtype InviteAccountToOrganizationRequest
  = InviteAccountToOrganizationRequest { "Target" :: HandshakeParty, "Notes" :: NullOrUndefined (HandshakeNotes) }
```

##### Instances
``` purescript
Newtype InviteAccountToOrganizationRequest _
```

#### `InviteAccountToOrganizationResponse`

``` purescript
newtype InviteAccountToOrganizationResponse
  = InviteAccountToOrganizationResponse { "Handshake" :: NullOrUndefined (Handshake) }
```

##### Instances
``` purescript
Newtype InviteAccountToOrganizationResponse _
```

#### `ListAWSServiceAccessForOrganizationRequest`

``` purescript
newtype ListAWSServiceAccessForOrganizationRequest
  = ListAWSServiceAccessForOrganizationRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListAWSServiceAccessForOrganizationRequest _
```

#### `ListAWSServiceAccessForOrganizationResponse`

``` purescript
newtype ListAWSServiceAccessForOrganizationResponse
  = ListAWSServiceAccessForOrganizationResponse { "EnabledServicePrincipals" :: NullOrUndefined (EnabledServicePrincipals), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAWSServiceAccessForOrganizationResponse _
```

#### `ListAccountsForParentRequest`

``` purescript
newtype ListAccountsForParentRequest
  = ListAccountsForParentRequest { "ParentId" :: ParentId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListAccountsForParentRequest _
```

#### `ListAccountsForParentResponse`

``` purescript
newtype ListAccountsForParentResponse
  = ListAccountsForParentResponse { "Accounts" :: NullOrUndefined (Accounts), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAccountsForParentResponse _
```

#### `ListAccountsRequest`

``` purescript
newtype ListAccountsRequest
  = ListAccountsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListAccountsRequest _
```

#### `ListAccountsResponse`

``` purescript
newtype ListAccountsResponse
  = ListAccountsResponse { "Accounts" :: NullOrUndefined (Accounts), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAccountsResponse _
```

#### `ListChildrenRequest`

``` purescript
newtype ListChildrenRequest
  = ListChildrenRequest { "ParentId" :: ParentId, "ChildType" :: ChildType, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListChildrenRequest _
```

#### `ListChildrenResponse`

``` purescript
newtype ListChildrenResponse
  = ListChildrenResponse { "Children" :: NullOrUndefined (Children), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListChildrenResponse _
```

#### `ListCreateAccountStatusRequest`

``` purescript
newtype ListCreateAccountStatusRequest
  = ListCreateAccountStatusRequest { "States" :: NullOrUndefined (CreateAccountStates), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListCreateAccountStatusRequest _
```

#### `ListCreateAccountStatusResponse`

``` purescript
newtype ListCreateAccountStatusResponse
  = ListCreateAccountStatusResponse { "CreateAccountStatuses" :: NullOrUndefined (CreateAccountStatuses), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListCreateAccountStatusResponse _
```

#### `ListHandshakesForAccountRequest`

``` purescript
newtype ListHandshakesForAccountRequest
  = ListHandshakesForAccountRequest { "Filter" :: NullOrUndefined (HandshakeFilter), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListHandshakesForAccountRequest _
```

#### `ListHandshakesForAccountResponse`

``` purescript
newtype ListHandshakesForAccountResponse
  = ListHandshakesForAccountResponse { "Handshakes" :: NullOrUndefined (Handshakes), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListHandshakesForAccountResponse _
```

#### `ListHandshakesForOrganizationRequest`

``` purescript
newtype ListHandshakesForOrganizationRequest
  = ListHandshakesForOrganizationRequest { "Filter" :: NullOrUndefined (HandshakeFilter), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListHandshakesForOrganizationRequest _
```

#### `ListHandshakesForOrganizationResponse`

``` purescript
newtype ListHandshakesForOrganizationResponse
  = ListHandshakesForOrganizationResponse { "Handshakes" :: NullOrUndefined (Handshakes), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListHandshakesForOrganizationResponse _
```

#### `ListOrganizationalUnitsForParentRequest`

``` purescript
newtype ListOrganizationalUnitsForParentRequest
  = ListOrganizationalUnitsForParentRequest { "ParentId" :: ParentId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListOrganizationalUnitsForParentRequest _
```

#### `ListOrganizationalUnitsForParentResponse`

``` purescript
newtype ListOrganizationalUnitsForParentResponse
  = ListOrganizationalUnitsForParentResponse { "OrganizationalUnits" :: NullOrUndefined (OrganizationalUnits), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListOrganizationalUnitsForParentResponse _
```

#### `ListParentsRequest`

``` purescript
newtype ListParentsRequest
  = ListParentsRequest { "ChildId" :: ChildId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListParentsRequest _
```

#### `ListParentsResponse`

``` purescript
newtype ListParentsResponse
  = ListParentsResponse { "Parents" :: NullOrUndefined (Parents), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListParentsResponse _
```

#### `ListPoliciesForTargetRequest`

``` purescript
newtype ListPoliciesForTargetRequest
  = ListPoliciesForTargetRequest { "TargetId" :: PolicyTargetId, "Filter" :: PolicyType, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListPoliciesForTargetRequest _
```

#### `ListPoliciesForTargetResponse`

``` purescript
newtype ListPoliciesForTargetResponse
  = ListPoliciesForTargetResponse { "Policies" :: NullOrUndefined (Policies), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListPoliciesForTargetResponse _
```

#### `ListPoliciesRequest`

``` purescript
newtype ListPoliciesRequest
  = ListPoliciesRequest { "Filter" :: PolicyType, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListPoliciesRequest _
```

#### `ListPoliciesResponse`

``` purescript
newtype ListPoliciesResponse
  = ListPoliciesResponse { "Policies" :: NullOrUndefined (Policies), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListPoliciesResponse _
```

#### `ListRootsRequest`

``` purescript
newtype ListRootsRequest
  = ListRootsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListRootsRequest _
```

#### `ListRootsResponse`

``` purescript
newtype ListRootsResponse
  = ListRootsResponse { "Roots" :: NullOrUndefined (Roots), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListRootsResponse _
```

#### `ListTargetsForPolicyRequest`

``` purescript
newtype ListTargetsForPolicyRequest
  = ListTargetsForPolicyRequest { "PolicyId" :: PolicyId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTargetsForPolicyRequest _
```

#### `ListTargetsForPolicyResponse`

``` purescript
newtype ListTargetsForPolicyResponse
  = ListTargetsForPolicyResponse { "Targets" :: NullOrUndefined (PolicyTargets), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTargetsForPolicyResponse _
```

#### `MalformedPolicyDocumentException`

``` purescript
newtype MalformedPolicyDocumentException
  = MalformedPolicyDocumentException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The provided policy document does not meet the requirements of the specified policy type. For example, the syntax might be incorrect. For details about service control policy syntax, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html">Service Control Policy Syntax</a> in the <i>AWS Organizations User Guide</i>.</p>

##### Instances
``` purescript
Newtype MalformedPolicyDocumentException _
```

#### `MasterCannotLeaveOrganizationException`

``` purescript
newtype MasterCannotLeaveOrganizationException
  = MasterCannotLeaveOrganizationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>You can't remove a master account from an organization. If you want the master account to become a member account in another organization, you must first delete the current organization of the master account.</p>

##### Instances
``` purescript
Newtype MasterCannotLeaveOrganizationException _
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

#### `MoveAccountRequest`

``` purescript
newtype MoveAccountRequest
  = MoveAccountRequest { "AccountId" :: AccountId, "SourceParentId" :: ParentId, "DestinationParentId" :: ParentId }
```

##### Instances
``` purescript
Newtype MoveAccountRequest _
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

#### `Organization`

``` purescript
newtype Organization
  = Organization { "Id" :: NullOrUndefined (OrganizationId), "Arn" :: NullOrUndefined (OrganizationArn), "FeatureSet" :: NullOrUndefined (OrganizationFeatureSet), "MasterAccountArn" :: NullOrUndefined (AccountArn), "MasterAccountId" :: NullOrUndefined (AccountId), "MasterAccountEmail" :: NullOrUndefined (Email), "AvailablePolicyTypes" :: NullOrUndefined (PolicyTypes) }
```

<p>Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .</p>

##### Instances
``` purescript
Newtype Organization _
```

#### `OrganizationArn`

``` purescript
newtype OrganizationArn
  = OrganizationArn String
```

##### Instances
``` purescript
Newtype OrganizationArn _
```

#### `OrganizationFeatureSet`

``` purescript
newtype OrganizationFeatureSet
  = OrganizationFeatureSet String
```

##### Instances
``` purescript
Newtype OrganizationFeatureSet _
```

#### `OrganizationId`

``` purescript
newtype OrganizationId
  = OrganizationId String
```

##### Instances
``` purescript
Newtype OrganizationId _
```

#### `OrganizationNotEmptyException`

``` purescript
newtype OrganizationNotEmptyException
  = OrganizationNotEmptyException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The organization isn't empty. To delete an organization, you must first remove all accounts except the master account, delete all organizational units (OUs), and delete all policies.</p>

##### Instances
``` purescript
Newtype OrganizationNotEmptyException _
```

#### `OrganizationalUnit`

``` purescript
newtype OrganizationalUnit
  = OrganizationalUnit { "Id" :: NullOrUndefined (OrganizationalUnitId), "Arn" :: NullOrUndefined (OrganizationalUnitArn), "Name" :: NullOrUndefined (OrganizationalUnitName) }
```

<p>Contains details about an organizational unit (OU). An OU is a container of AWS accounts within a root of an organization. Policies that are attached to an OU apply to all accounts contained in that OU and in any child OUs.</p>

##### Instances
``` purescript
Newtype OrganizationalUnit _
```

#### `OrganizationalUnitArn`

``` purescript
newtype OrganizationalUnitArn
  = OrganizationalUnitArn String
```

##### Instances
``` purescript
Newtype OrganizationalUnitArn _
```

#### `OrganizationalUnitId`

``` purescript
newtype OrganizationalUnitId
  = OrganizationalUnitId String
```

##### Instances
``` purescript
Newtype OrganizationalUnitId _
```

#### `OrganizationalUnitName`

``` purescript
newtype OrganizationalUnitName
  = OrganizationalUnitName String
```

##### Instances
``` purescript
Newtype OrganizationalUnitName _
```

#### `OrganizationalUnitNotEmptyException`

``` purescript
newtype OrganizationalUnitNotEmptyException
  = OrganizationalUnitNotEmptyException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified organizational unit (OU) is not empty. Move all accounts to another root or to other OUs, remove all child OUs, and then try the operation again.</p>

##### Instances
``` purescript
Newtype OrganizationalUnitNotEmptyException _
```

#### `OrganizationalUnitNotFoundException`

``` purescript
newtype OrganizationalUnitNotFoundException
  = OrganizationalUnitNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find an organizational unit (OU) with the OrganizationalUnitId that you specified.</p>

##### Instances
``` purescript
Newtype OrganizationalUnitNotFoundException _
```

#### `OrganizationalUnits`

``` purescript
newtype OrganizationalUnits
  = OrganizationalUnits (Array OrganizationalUnit)
```

##### Instances
``` purescript
Newtype OrganizationalUnits _
```

#### `Parent`

``` purescript
newtype Parent
  = Parent { "Id" :: NullOrUndefined (ParentId), "Type" :: NullOrUndefined (ParentType) }
```

<p>Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.</p>

##### Instances
``` purescript
Newtype Parent _
```

#### `ParentId`

``` purescript
newtype ParentId
  = ParentId String
```

##### Instances
``` purescript
Newtype ParentId _
```

#### `ParentNotFoundException`

``` purescript
newtype ParentNotFoundException
  = ParentNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a root or organizational unit (OU) with the ParentId that you specified.</p>

##### Instances
``` purescript
Newtype ParentNotFoundException _
```

#### `ParentType`

``` purescript
newtype ParentType
  = ParentType String
```

##### Instances
``` purescript
Newtype ParentType _
```

#### `Parents`

``` purescript
newtype Parents
  = Parents (Array Parent)
```

##### Instances
``` purescript
Newtype Parents _
```

#### `Policies`

``` purescript
newtype Policies
  = Policies (Array PolicySummary)
```

##### Instances
``` purescript
Newtype Policies _
```

#### `Policy`

``` purescript
newtype Policy
  = Policy { "PolicySummary" :: NullOrUndefined (PolicySummary), "Content" :: NullOrUndefined (PolicyContent) }
```

<p>Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.</p>

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

#### `PolicyContent`

``` purescript
newtype PolicyContent
  = PolicyContent String
```

##### Instances
``` purescript
Newtype PolicyContent _
```

#### `PolicyDescription`

``` purescript
newtype PolicyDescription
  = PolicyDescription String
```

##### Instances
``` purescript
Newtype PolicyDescription _
```

#### `PolicyId`

``` purescript
newtype PolicyId
  = PolicyId String
```

##### Instances
``` purescript
Newtype PolicyId _
```

#### `PolicyInUseException`

``` purescript
newtype PolicyInUseException
  = PolicyInUseException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The policy is attached to one or more entities. You must detach it from all roots, organizational units (OUs), and accounts before performing this operation.</p>

##### Instances
``` purescript
Newtype PolicyInUseException _
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

#### `PolicyNotAttachedException`

``` purescript
newtype PolicyNotAttachedException
  = PolicyNotAttachedException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The policy isn't attached to the specified target in the specified root.</p>

##### Instances
``` purescript
Newtype PolicyNotAttachedException _
```

#### `PolicyNotFoundException`

``` purescript
newtype PolicyNotFoundException
  = PolicyNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a policy with the PolicyId that you specified.</p>

##### Instances
``` purescript
Newtype PolicyNotFoundException _
```

#### `PolicySummary`

``` purescript
newtype PolicySummary
  = PolicySummary { "Id" :: NullOrUndefined (PolicyId), "Arn" :: NullOrUndefined (PolicyArn), "Name" :: NullOrUndefined (PolicyName), "Description" :: NullOrUndefined (PolicyDescription), "Type" :: NullOrUndefined (PolicyType), "AwsManaged" :: NullOrUndefined (AwsManagedPolicy) }
```

<p>Contains information about a policy, but does not include the content. To see the content of a policy, see <a>DescribePolicy</a>.</p>

##### Instances
``` purescript
Newtype PolicySummary _
```

#### `PolicyTargetId`

``` purescript
newtype PolicyTargetId
  = PolicyTargetId String
```

##### Instances
``` purescript
Newtype PolicyTargetId _
```

#### `PolicyTargetSummary`

``` purescript
newtype PolicyTargetSummary
  = PolicyTargetSummary { "TargetId" :: NullOrUndefined (PolicyTargetId), "Arn" :: NullOrUndefined (GenericArn), "Name" :: NullOrUndefined (TargetName), "Type" :: NullOrUndefined (TargetType) }
```

<p>Contains information about a root, OU, or account that a policy is attached to.</p>

##### Instances
``` purescript
Newtype PolicyTargetSummary _
```

#### `PolicyTargets`

``` purescript
newtype PolicyTargets
  = PolicyTargets (Array PolicyTargetSummary)
```

##### Instances
``` purescript
Newtype PolicyTargets _
```

#### `PolicyType`

``` purescript
newtype PolicyType
  = PolicyType String
```

##### Instances
``` purescript
Newtype PolicyType _
```

#### `PolicyTypeAlreadyEnabledException`

``` purescript
newtype PolicyTypeAlreadyEnabledException
  = PolicyTypeAlreadyEnabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified policy type is already enabled in the specified root.</p>

##### Instances
``` purescript
Newtype PolicyTypeAlreadyEnabledException _
```

#### `PolicyTypeNotAvailableForOrganizationException`

``` purescript
newtype PolicyTypeNotAvailableForOrganizationException
  = PolicyTypeNotAvailableForOrganizationException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>You can't use the specified policy type with the feature set currently enabled for this organization. For example, you can enable service control policies (SCPs) only after you enable all features in the organization. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root">Enabling and Disabling a Policy Type on a Root</a> in the <i>AWS Organizations User Guide</i>.</p>

##### Instances
``` purescript
Newtype PolicyTypeNotAvailableForOrganizationException _
```

#### `PolicyTypeNotEnabledException`

``` purescript
newtype PolicyTypeNotEnabledException
  = PolicyTypeNotEnabledException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>The specified policy type is not currently enabled in this root. You cannot attach policies of the specified type to entities in a root until you enable that type in the root. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p>

##### Instances
``` purescript
Newtype PolicyTypeNotEnabledException _
```

#### `PolicyTypeStatus`

``` purescript
newtype PolicyTypeStatus
  = PolicyTypeStatus String
```

##### Instances
``` purescript
Newtype PolicyTypeStatus _
```

#### `PolicyTypeSummary`

``` purescript
newtype PolicyTypeSummary
  = PolicyTypeSummary { "Type" :: NullOrUndefined (PolicyType), "Status" :: NullOrUndefined (PolicyTypeStatus) }
```

<p>Contains information about a policy type and its status in the associated root.</p>

##### Instances
``` purescript
Newtype PolicyTypeSummary _
```

#### `PolicyTypes`

``` purescript
newtype PolicyTypes
  = PolicyTypes (Array PolicyTypeSummary)
```

##### Instances
``` purescript
Newtype PolicyTypes _
```

#### `RemoveAccountFromOrganizationRequest`

``` purescript
newtype RemoveAccountFromOrganizationRequest
  = RemoveAccountFromOrganizationRequest { "AccountId" :: AccountId }
```

##### Instances
``` purescript
Newtype RemoveAccountFromOrganizationRequest _
```

#### `RoleName`

``` purescript
newtype RoleName
  = RoleName String
```

##### Instances
``` purescript
Newtype RoleName _
```

#### `Root`

``` purescript
newtype Root
  = Root { "Id" :: NullOrUndefined (RootId), "Arn" :: NullOrUndefined (RootArn), "Name" :: NullOrUndefined (RootName), "PolicyTypes" :: NullOrUndefined (PolicyTypes) }
```

<p>Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. Every root contains every AWS account in the organization. Each root enables the accounts to be organized in a different way and to have different policy types enabled for use in that root.</p>

##### Instances
``` purescript
Newtype Root _
```

#### `RootArn`

``` purescript
newtype RootArn
  = RootArn String
```

##### Instances
``` purescript
Newtype RootArn _
```

#### `RootId`

``` purescript
newtype RootId
  = RootId String
```

##### Instances
``` purescript
Newtype RootId _
```

#### `RootName`

``` purescript
newtype RootName
  = RootName String
```

##### Instances
``` purescript
Newtype RootName _
```

#### `RootNotFoundException`

``` purescript
newtype RootNotFoundException
  = RootNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a root with the RootId that you specified.</p>

##### Instances
``` purescript
Newtype RootNotFoundException _
```

#### `Roots`

``` purescript
newtype Roots
  = Roots (Array Root)
```

##### Instances
``` purescript
Newtype Roots _
```

#### `ServiceException`

``` purescript
newtype ServiceException
  = ServiceException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>AWS Organizations can't complete your request because of an internal service error. Try again later.</p>

##### Instances
``` purescript
Newtype ServiceException _
```

#### `ServicePrincipal`

``` purescript
newtype ServicePrincipal
  = ServicePrincipal String
```

##### Instances
``` purescript
Newtype ServicePrincipal _
```

#### `SourceParentNotFoundException`

``` purescript
newtype SourceParentNotFoundException
  = SourceParentNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a source root or OU with the ParentId that you specified.</p>

##### Instances
``` purescript
Newtype SourceParentNotFoundException _
```

#### `TargetName`

``` purescript
newtype TargetName
  = TargetName String
```

##### Instances
``` purescript
Newtype TargetName _
```

#### `TargetNotFoundException`

``` purescript
newtype TargetNotFoundException
  = TargetNotFoundException { "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>We can't find a root, OU, or account with the TargetId that you specified.</p>

##### Instances
``` purescript
Newtype TargetNotFoundException _
```

#### `TargetType`

``` purescript
newtype TargetType
  = TargetType String
```

##### Instances
``` purescript
Newtype TargetType _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Type" :: NullOrUndefined (ExceptionType), "Message" :: NullOrUndefined (ExceptionMessage) }
```

<p>You've sent too many requests in too short a period of time. The limit helps protect against denial-of-service attacks. Try again later.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UpdateOrganizationalUnitRequest`

``` purescript
newtype UpdateOrganizationalUnitRequest
  = UpdateOrganizationalUnitRequest { "OrganizationalUnitId" :: OrganizationalUnitId, "Name" :: NullOrUndefined (OrganizationalUnitName) }
```

##### Instances
``` purescript
Newtype UpdateOrganizationalUnitRequest _
```

#### `UpdateOrganizationalUnitResponse`

``` purescript
newtype UpdateOrganizationalUnitResponse
  = UpdateOrganizationalUnitResponse { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit) }
```

##### Instances
``` purescript
Newtype UpdateOrganizationalUnitResponse _
```

#### `UpdatePolicyRequest`

``` purescript
newtype UpdatePolicyRequest
  = UpdatePolicyRequest { "PolicyId" :: PolicyId, "Name" :: NullOrUndefined (PolicyName), "Description" :: NullOrUndefined (PolicyDescription), "Content" :: NullOrUndefined (PolicyContent) }
```

##### Instances
``` purescript
Newtype UpdatePolicyRequest _
```

#### `UpdatePolicyResponse`

``` purescript
newtype UpdatePolicyResponse
  = UpdatePolicyResponse { "Policy" :: NullOrUndefined (Policy) }
```

##### Instances
``` purescript
Newtype UpdatePolicyResponse _
```


