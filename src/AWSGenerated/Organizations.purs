

-- | <fullname>AWS Organizations API Reference</fullname> <p>AWS Organizations is a web service that enables you to consolidate your multiple AWS accounts into an <i>organization</i> and centrally manage your accounts and their resources.</p> <p>This guide provides descriptions of the Organizations API. For more information about using this service, see the <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_introduction.html">AWS Organizations User Guide</a>.</p> <p> <b>API Version</b> </p> <p>This version of the Organizations API Reference documents the Organizations API version 2016-11-28.</p> <note> <p>As an alternative to using the API directly, you can use one of the AWS SDKs, which consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, and more). The SDKs provide a convenient way to create programmatic access to AWS Organizations. For example, the SDKs take care of cryptographically signing requests, managing errors, and retrying requests automatically. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note> <p>We recommend that you use the AWS SDKs to make programmatic API calls to Organizations. However, you also can use the Organizations Query API to make direct calls to the Organizations web service. To learn more about the Organizations Query API, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_query-requests.html">Making Query Requests</a> in the <i>AWS Organizations User Guide</i>. Organizations supports GET and POST requests for all actions. That is, the API does not require you to use GET for some actions and POST for others. However, GET requests are subject to the limitation size of a URL. Therefore, for operations that require larger sizes, use a POST request.</p> <p> <b>Signing Requests</b> </p> <p>When you send HTTP requests to AWS, you must sign the requests so that AWS can identify who sent them. You sign requests with your AWS access key, which consists of an access key ID and a secret access key. We strongly recommend that you do not create an access key for your root account. Anyone who has the access key for your root account has unrestricted access to all the resources in your account. Instead, create an access key for an IAM user account that has administrative privileges. As another option, use AWS Security Token Service to generate temporary security credentials, and use those credentials to sign requests. </p> <p>To sign requests, we recommend that you use <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4</a>. If you have an existing application that uses Signature Version 2, you do not have to update it to use Signature Version 4. However, some operations now require Signature Version 4. The documentation for operations that require version 4 indicate this requirement. </p> <p>When you use the AWS Command Line Interface (AWS CLI) or one of the AWS SDKs to make requests to AWS, these tools automatically sign the requests for you with the access key that you specify when you configure the tools.</p> <p>In this release, each organization can have only one root. In a future release, a single organization will support multiple roots.</p> <p> <b>Support and Feedback for AWS Organizations</b> </p> <p>We welcome your feedback. Send your comments to <a href="mailto:feedback-awsorganizations@amazon.com">feedback-awsorganizations@amazon.com</a> or post your feedback and questions in the <a href="http://forums.aws.amazon.com/forum.jspa?forumID=219">AWS Organizations support forum</a>. For more information about the AWS support forums, see <a href="http://forums.aws.amazon.com/help.jspa">Forums Help</a>.</p> <p> <b>Endpoint to Call When Using the CLI or the AWS API</b> </p> <p>For the current release of Organizations, you must specify the <code>us-east-1</code> region for all AWS API and CLI calls. You can do this in the CLI by using these parameters and commands:</p> <ul> <li> <p>Use the following parameter with each command to specify both the endpoint and its region:</p> <p> <code>--endpoint-url https://organizations.us-east-1.amazonaws.com</code> </p> </li> <li> <p>Use the default endpoint, but configure your default region with this command:</p> <p> <code>aws configure set default.region us-east-1</code> </p> </li> <li> <p>Use the following parameter with each command to specify the endpoint:</p> <p> <code>--region us-east-1</code> </p> </li> </ul> <p>For the various SDKs used to call the APIs, see the documentation for the SDK of interest to learn how to direct the requests to a specific endpoint. For more information, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region">Regions and Endpoints</a> in the <i>AWS General Reference</i>. </p> <p> <b>How examples are presented</b> </p> <p>The JSON returned by the AWS Organizations service as response to your requests is returned as a single long string without line breaks or formatting whitespace. Both line breaks and whitespace are included in the examples in this guide to improve readability. When example input parameters also would result in long strings that would extend beyond the screen, we insert line breaks to enhance readability. You should always submit the input as a single JSON text string.</p> <p> <b>Recording API Requests</b> </p> <p>AWS Organizations supports AWS CloudTrail, a service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information collected by AWS CloudTrail, you can determine which requests were successfully made to Organizations, who made the request, when it was made, and so on. For more about AWS Organizations and its support for AWS CloudTrail, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_cloudtrail-integration.html">Logging AWS Organizations Events with AWS CloudTrail</a> in the <i>AWS Organizations User Guide</i>. To learn more about CloudTrail, including how to turn it on and find your log files, see the <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html">AWS CloudTrail User Guide</a>.</p>
module AWS.Organizations where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Organizations" :: String


-- | <p>Sends a response to the originator of a handshake agreeing to the action proposed by the handshake request. </p> <p>This operation can be called only by the following principals when they also have the relevant IAM permissions:</p> <ul> <li> <p> <b>Invitation to join</b> or <b>Approve all features request</b> handshakes: only a principal from the member account. </p> <p>The user who calls the API for an invitation to join must have the <code>organizations:AcceptHandshake</code> permission. If you enabled all features in the organization, then the user must also have the <code>iam:CreateServiceLinkedRole</code> permission so that Organizations can create the required service-linked role named <i>OrgsServiceLinkedRoleName</i>. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles">AWS Organizations and Service-Linked Roles</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p> <b>Enable all features final confirmation</b> handshake: only a principal from the master account.</p> <p>For more information about invitations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html">Inviting an AWS Account to Join Your Organization</a> in the <i>AWS Organizations User Guide</i>. For more information about requests to enable all features in the organization, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> </li> </ul> <p>After you accept a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>
acceptHandshake :: forall eff. AcceptHandshakeRequest -> Aff (err :: AWS.RequestError | eff) AcceptHandshakeResponse
acceptHandshake = AWS.request serviceName "AcceptHandshake" 


-- | <p>Attaches a policy to a root, an organizational unit, or an individual account. How the policy affects accounts depends on the type of policy:</p> <ul> <li> <p> <b>Service control policy (SCP)</b> - An SCP specifies what permissions can be delegated to users in affected member accounts. The scope of influence for a policy depends on what you attach the policy to:</p> <ul> <li> <p>If you attach an SCP to a root, it affects all accounts in the organization.</p> </li> <li> <p>If you attach an SCP to an OU, it affects all accounts in that OU and in any child OUs.</p> </li> <li> <p>If you attach the policy directly to an account, then it affects only that account.</p> </li> </ul> <p>SCPs essentially are permission "filters". When you attach one SCP to a higher level root or OU, and you also attach a different SCP to a child OU or to an account, the child policy can further restrict only the permissions that pass through the parent filter and are available to the child. An SCP that is attached to a child cannot grant a permission that is not already granted by the parent. For example, imagine that the parent SCP allows permissions A, B, C, D, and E. The child SCP allows C, D, E, F, and G. The result is that the accounts affected by the child SCP are allowed to use only C, D, and E. They cannot use A or B because they were filtered out by the child OU. They also cannot use F and G because they were filtered out by the parent OU. They cannot be granted back by the child SCP; child SCPs can only filter the permissions they receive from the parent SCP.</p> <p>AWS Organizations attaches a default SCP named <code>"FullAWSAccess</code> to every root, OU, and account. This default SCP allows all services and actions, enabling any new child OU or account to inherit the permissions of the parent root or OU. If you detach the default policy, you must replace it with a policy that specifies the permissions that you want to allow in that OU or account.</p> <p>For more information about how Organizations policies permissions work, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html">Using Service Control Policies</a> in the <i>AWS Organizations User Guide</i>.</p> </li> </ul> <p>This operation can be called only from the organization's master account.</p>
attachPolicy :: forall eff. AttachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachPolicy = AWS.request serviceName "AttachPolicy" 


-- | <p>Cancels a handshake. Canceling a handshake sets the handshake state to <code>CANCELED</code>. </p> <p>This operation can be called only from the account that originated the handshake. The recipient of the handshake can't cancel it, but can use <a>DeclineHandshake</a> instead. After a handshake is canceled, the recipient can no longer respond to that handshake.</p> <p>After you cancel a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>
cancelHandshake :: forall eff. CancelHandshakeRequest -> Aff (err :: AWS.RequestError | eff) CancelHandshakeResponse
cancelHandshake = AWS.request serviceName "CancelHandshake" 


-- | <p>Creates an AWS account that is automatically a member of the organization whose credentials made the request. This is an asynchronous request that AWS performs in the background. If you want to check the status of the request later, you need the <code>OperationId</code> response element from this operation to provide as a parameter to the <a>DescribeCreateAccountStatus</a> operation.</p> <p>The user who calls the API for an invitation to join must have the <code>organizations:CreateAccount</code> permission. If you enabled all features in the organization, then the user must also have the <code>iam:CreateServiceLinkedRole</code> permission so that Organizations can create the required service-linked role named <i>OrgsServiceLinkedRoleName</i>. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles">AWS Organizations and Service-Linked Roles</a> in the <i>AWS Organizations User Guide</i>.</p> <p>The user in the master account who calls this API must also have the <code>iam:CreateRole</code> permission because AWS Organizations preconfigures the new member account with a role (named <code>OrganizationAccountAccessRole</code> by default) that grants users in the master account administrator permissions in the new member account. Principals in the master account can assume the role. AWS Organizations clones the company name and address information for the new account from the organization's master account.</p> <p>This operation can be called only from the organization's master account.</p> <p>For more information about creating accounts, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html">Creating an AWS Account in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> <important> <p>When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required for the account to operate as a standalone account, such as a payment method and signing the End User Licence Agreement (EULA) is <i>not</i> automatically collected. If you must remove an account from your organization later, you can do so only after you provide the missing information. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </important> <note> <p>When you create a member account with this operation, you can choose whether to create the account with the <b>IAM User and Role Access to Billing Information</b> switch enabled. If you enable it, IAM users and roles that have appropriate permissions can view billing information for the account. If you disable this, then only the account root user can access billing information. For information about how to disable this for an account, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html">Granting Access to Your Billing Information and Tools</a>.</p> </note> <p>This operation can be called only from the organization's master account.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important>
createAccount :: forall eff. CreateAccountRequest -> Aff (err :: AWS.RequestError | eff) CreateAccountResponse
createAccount = AWS.request serviceName "CreateAccount" 


-- | <p>Creates an AWS organization. The account whose user is calling the CreateOrganization operation automatically becomes the <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/orgs_getting-started_concepts.html#account">master account</a> of the new organization.</p> <p>This operation must be called using credentials from the account that is to become the new organization's master account. The principal must also have the relevant IAM permissions.</p> <p>By default (or if you set the <code>FeatureSet</code> parameter to <code>ALL</code>), the new organization is created with all features enabled and service control policies automatically enabled in the root. If you instead choose to create the organization supporting only the consolidated billing features by setting the <code>FeatureSet</code> parameter to <code>CONSOLIDATED_BILLING"</code>, then no policy types are enabled by default and you cannot use organization policies.</p>
createOrganization :: forall eff. CreateOrganizationRequest -> Aff (err :: AWS.RequestError | eff) CreateOrganizationResponse
createOrganization = AWS.request serviceName "CreateOrganization" 


-- | <p>Creates an organizational unit (OU) within a root or parent OU. An OU is a container for accounts that enables you to organize your accounts to apply policies according to your business requirements. The number of levels deep that you can nest OUs is dependent upon the policy types enabled for that root. For service control policies, the limit is five. </p> <p>For more information about OUs, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html">Managing Organizational Units</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>
createOrganizationalUnit :: forall eff. CreateOrganizationalUnitRequest -> Aff (err :: AWS.RequestError | eff) CreateOrganizationalUnitResponse
createOrganizationalUnit = AWS.request serviceName "CreateOrganizationalUnit" 


-- | <p>Creates a policy of a specified type that you can attach to a root, an organizational unit (OU), or an individual AWS account.</p> <p>For more information about policies and their use, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html">Managing Organization Policies</a>.</p> <p>This operation can be called only from the organization's master account.</p>
createPolicy :: forall eff. CreatePolicyRequest -> Aff (err :: AWS.RequestError | eff) CreatePolicyResponse
createPolicy = AWS.request serviceName "CreatePolicy" 


-- | <p>Declines a handshake request. This sets the handshake state to <code>DECLINED</code> and effectively deactivates the request.</p> <p>This operation can be called only from the account that received the handshake. The originator of the handshake can use <a>CancelHandshake</a> instead. The originator can't reactivate a declined request, but can re-initiate the process with a new handshake request.</p> <p>After you decline a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.</p>
declineHandshake :: forall eff. DeclineHandshakeRequest -> Aff (err :: AWS.RequestError | eff) DeclineHandshakeResponse
declineHandshake = AWS.request serviceName "DeclineHandshake" 


-- | <p>Deletes the organization. You can delete an organization only by using credentials from the master account. The organization must be empty of member accounts, OUs, and policies.</p>
deleteOrganization :: forall eff.  Aff (err :: AWS.RequestError | eff) Unit
deleteOrganization = AWS.request serviceName "DeleteOrganization" unit


-- | <p>Deletes an organizational unit from a root or another OU. You must first remove all accounts and child OUs from the OU that you want to delete.</p> <p>This operation can be called only from the organization's master account.</p>
deleteOrganizationalUnit :: forall eff. DeleteOrganizationalUnitRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteOrganizationalUnit = AWS.request serviceName "DeleteOrganizationalUnit" 


-- | <p>Deletes the specified policy from your organization. Before you perform this operation, you must first detach the policy from all OUs, roots, and accounts.</p> <p>This operation can be called only from the organization's master account.</p>
deletePolicy :: forall eff. DeletePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deletePolicy = AWS.request serviceName "DeletePolicy" 


-- | <p>Retrieves Organizations-related information about the specified account.</p> <p>This operation can be called only from the organization's master account.</p>
describeAccount :: forall eff. DescribeAccountRequest -> Aff (err :: AWS.RequestError | eff) DescribeAccountResponse
describeAccount = AWS.request serviceName "DescribeAccount" 


-- | <p>Retrieves the current status of an asynchronous request to create an account.</p> <p>This operation can be called only from the organization's master account.</p>
describeCreateAccountStatus :: forall eff. DescribeCreateAccountStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeCreateAccountStatusResponse
describeCreateAccountStatus = AWS.request serviceName "DescribeCreateAccountStatus" 


-- | <p>Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original <a>InviteAccountToOrganization</a> operation that generated the handshake.</p> <p>You can access handshakes that are ACCEPTED, DECLINED, or CANCELED for only 30 days after they change to that state. They are then deleted and no longer accessible.</p> <p>This operation can be called from any account in the organization.</p>
describeHandshake :: forall eff. DescribeHandshakeRequest -> Aff (err :: AWS.RequestError | eff) DescribeHandshakeResponse
describeHandshake = AWS.request serviceName "DescribeHandshake" 


-- | <p>Retrieves information about the organization that the user's account belongs to.</p> <p>This operation can be called from any account in the organization.</p>
describeOrganization :: forall eff.  Aff (err :: AWS.RequestError | eff) DescribeOrganizationResponse
describeOrganization = AWS.request serviceName "DescribeOrganization" unit


-- | <p>Retrieves information about an organizational unit (OU).</p> <p>This operation can be called only from the organization's master account.</p>
describeOrganizationalUnit :: forall eff. DescribeOrganizationalUnitRequest -> Aff (err :: AWS.RequestError | eff) DescribeOrganizationalUnitResponse
describeOrganizationalUnit = AWS.request serviceName "DescribeOrganizationalUnit" 


-- | <p>Retrieves information about a policy.</p> <p>This operation can be called only from the organization's master account.</p>
describePolicy :: forall eff. DescribePolicyRequest -> Aff (err :: AWS.RequestError | eff) DescribePolicyResponse
describePolicy = AWS.request serviceName "DescribePolicy" 


-- | <p>Detaches a policy from a target root, organizational unit, or account. If the policy being detached is a service control policy (SCP), the changes to permissions for IAM users and roles in affected accounts are immediate.</p> <p> <b>Note:</b> Every root, OU, and account must have at least one SCP attached. If you want to replace the default <code>FullAWSAccess</code> policy with one that limits the permissions that can be delegated, then you must attach the replacement policy before you can remove the default one. This is the authorization strategy of <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_whitelist">whitelisting</a>. If you instead attach a second SCP and leave the <code>FullAWSAccess</code> SCP still attached, and specify <code>"Effect": "Deny"</code> in the second SCP to override the <code>"Effect": "Allow"</code> in the <code>FullAWSAccess</code> policy (or any other attached SCP), then you are using the authorization strategy of <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_about-scps.html#orgs_policies_blacklist">blacklisting</a>. </p> <p>This operation can be called only from the organization's master account.</p>
detachPolicy :: forall eff. DetachPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachPolicy = AWS.request serviceName "DetachPolicy" 


-- | <p>Disables the integration of an AWS service (the service that is specified by <code>ServicePrincipal</code>) with AWS Organizations. When you disable integration, the specified service no longer can create a <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html">service-linked role</a> in <i>new</i> accounts in your organization. This means the service can't perform operations on your behalf on any new accounts in your organization. The service can still perform operations in older accounts until the service completes its clean-up from AWS Organizations.</p> <p/> <important> <p>We recommend that you disable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the other service is aware that it can clean up any resources that are required only for the integration. How the service cleans up its resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.</p> </important> <p>After you perform the <code>DisableAWSServiceAccess</code> operation, the specified service can no longer perform operations in your organization's accounts unless the operations are explicitly permitted by the IAM policies that are attached to your roles. </p> <p>For more information about integrating other services with AWS Organizations, including the list of services that work with Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>
disableAWSServiceAccess :: forall eff. DisableAWSServiceAccessRequest -> Aff (err :: AWS.RequestError | eff) Unit
disableAWSServiceAccess = AWS.request serviceName "DisableAWSServiceAccess" 


-- | <p>Disables an organizational control policy type in a root. A policy of a certain type can be attached to entities in a root only if that type is enabled in the root. After you perform this operation, you no longer can attach policies of the specified type to that root or to any OU or account in that root. You can undo this by using the <a>EnablePolicyType</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>
disablePolicyType :: forall eff. DisablePolicyTypeRequest -> Aff (err :: AWS.RequestError | eff) DisablePolicyTypeResponse
disablePolicyType = AWS.request serviceName "DisablePolicyType" 


-- | <p>Enables the integration of an AWS service (the service that is specified by <code>ServicePrincipal</code>) with AWS Organizations. When you enable integration, you allow the specified service to create a <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html">service-linked role</a> in all the accounts in your organization. This allows the service to perform operations on your behalf in your organization and its accounts.</p> <important> <p>We recommend that you enable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the service is aware that it can create the resources that are required for the integration. How the service creates those resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.</p> </important> <p>For more information about enabling services to integrate with AWS Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account and only if the organization has <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">enabled all features</a>.</p>
enableAWSServiceAccess :: forall eff. EnableAWSServiceAccessRequest -> Aff (err :: AWS.RequestError | eff) Unit
enableAWSServiceAccess = AWS.request serviceName "EnableAWSServiceAccess" 


-- | <p>Enables all features in an organization. This enables the use of organization policies that can restrict the services and actions that can be called in each account. Until you enable all features, you have access only to consolidated billing, and you can't use any of the advanced account administration features that AWS Organizations supports. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p> <important> <p>This operation is required only for organizations that were created explicitly with only the consolidated billing features enabled, or that were migrated from a Consolidated Billing account family to Organizations. Calling this operation sends a handshake to every invited account in the organization. The feature set change can be finalized and the additional features enabled only after all administrators in the invited accounts approve the change by accepting the handshake.</p> </important> <p>After all invited member accounts accept the handshake, you finalize the feature set change by accepting the handshake that contains <code>"Action": "ENABLE_ALL_FEATURES"</code>. This completes the change.</p> <p>After you enable all features in your organization, the master account in the organization can apply policies on all member accounts. These policies can restrict what users and even administrators in those accounts can do. The master account can apply policies that prevent accounts from leaving the organization. Ensure that your account administrators are aware of this.</p> <p>This operation can be called only from the organization's master account. </p>
enableAllFeatures :: forall eff. EnableAllFeaturesRequest -> Aff (err :: AWS.RequestError | eff) EnableAllFeaturesResponse
enableAllFeatures = AWS.request serviceName "EnableAllFeatures" 


-- | <p>Enables a policy type in a root. After you enable a policy type in a root, you can attach policies of that type to the root, any OU, or account in that root. You can undo this by using the <a>DisablePolicyType</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>
enablePolicyType :: forall eff. EnablePolicyTypeRequest -> Aff (err :: AWS.RequestError | eff) EnablePolicyTypeResponse
enablePolicyType = AWS.request serviceName "EnablePolicyType" 


-- | <p>Sends an invitation to another account to join your organization as a member account. Organizations sends email on your behalf to the email address that is associated with the other account's owner. The invitation is implemented as a <a>Handshake</a> whose details are in the response.</p> <important> <p>You can invite AWS accounts only from the same seller as the master account. For example, if your organization's master account was created by Amazon Internet Services Pvt. Ltd (AISPL), an AWS seller in India, then you can only invite other AISPL accounts to your organization. You can't combine accounts from AISPL and AWS, or any other AWS seller. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/useconsolidatedbilliing-India.html">Consolidated Billing in India</a>.</p> </important> <p>This operation can be called only from the organization's master account.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important>
inviteAccountToOrganization :: forall eff. InviteAccountToOrganizationRequest -> Aff (err :: AWS.RequestError | eff) InviteAccountToOrganizationResponse
inviteAccountToOrganization = AWS.request serviceName "InviteAccountToOrganization" 


-- | <p>Removes a member account from its parent organization. This version of the operation is performed by the account that wants to leave. To remove a member account as a user in the master account, use <a>RemoveAccountFromOrganization</a> instead.</p> <p>This operation can be called only from a member account in the organization.</p> <important> <ul> <li> <p>The master account in an organization with all features enabled can set service control policies (SCPs) that can restrict what administrators of member accounts can do, including preventing them from successfully calling <code>LeaveOrganization</code> and leaving the organization. </p> </li> <li> <p>You can leave an organization as a member account only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is <i>not</i> automatically collected. For each account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>You can leave an organization only after you enable IAM user access to billing in your account. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate">Activating Access to the Billing and Cost Management Console</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> </li> </ul> </important>
leaveOrganization :: forall eff.  Aff (err :: AWS.RequestError | eff) Unit
leaveOrganization = AWS.request serviceName "LeaveOrganization" unit


-- | <p>Returns a list of the AWS services that you enabled to integrate with your organization. After a service on this list creates the resources that it requires for the integration, it can perform operations on your organization and its accounts.</p> <p>For more information about integrating other services with AWS Organizations, including the list of services that currently work with Organizations, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html">Integrating AWS Organizations with Other AWS Services</a> in the <i>AWS Organizations User Guide</i>.</p> <p>This operation can be called only from the organization's master account.</p>
listAWSServiceAccessForOrganization :: forall eff. ListAWSServiceAccessForOrganizationRequest -> Aff (err :: AWS.RequestError | eff) ListAWSServiceAccessForOrganizationResponse
listAWSServiceAccessForOrganization = AWS.request serviceName "ListAWSServiceAccessForOrganization" 


-- | <p>Lists all the accounts in the organization. To request only the accounts in a root or OU, use the <a>ListAccountsForParent</a> operation instead.</p> <p>This operation can be called only from the organization's master account.</p>
listAccounts :: forall eff. ListAccountsRequest -> Aff (err :: AWS.RequestError | eff) ListAccountsResponse
listAccounts = AWS.request serviceName "ListAccounts" 


-- | <p>Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU). If you specify the root, you get a list of all the accounts that are not in any OU. If you specify an OU, you get a list of all the accounts in only that OU, and not in any child OUs. To get a list of all accounts in the organization, use the <a>ListAccounts</a> operation.</p> <p>This operation can be called only from the organization's master account.</p>
listAccountsForParent :: forall eff. ListAccountsForParentRequest -> Aff (err :: AWS.RequestError | eff) ListAccountsForParentResponse
listAccountsForParent = AWS.request serviceName "ListAccountsForParent" 


-- | <p>Lists all of the OUs or accounts that are contained in the specified parent OU or root. This operation, along with <a>ListParents</a> enables you to traverse the tree structure that makes up this root.</p> <p>This operation can be called only from the organization's master account.</p>
listChildren :: forall eff. ListChildrenRequest -> Aff (err :: AWS.RequestError | eff) ListChildrenResponse
listChildren = AWS.request serviceName "ListChildren" 


-- | <p>Lists the account creation requests that match the specified status that is currently being tracked for the organization.</p> <p>This operation can be called only from the organization's master account.</p>
listCreateAccountStatus :: forall eff. ListCreateAccountStatusRequest -> Aff (err :: AWS.RequestError | eff) ListCreateAccountStatusResponse
listCreateAccountStatus = AWS.request serviceName "ListCreateAccountStatus" 


-- | <p>Lists the current handshakes that are associated with the account of the requesting user.</p> <p>Handshakes that are ACCEPTED, DECLINED, or CANCELED appear in the results of this API for only 30 days after changing to that state. After that they are deleted and no longer accessible.</p> <p>This operation can be called from any account in the organization.</p>
listHandshakesForAccount :: forall eff. ListHandshakesForAccountRequest -> Aff (err :: AWS.RequestError | eff) ListHandshakesForAccountResponse
listHandshakesForAccount = AWS.request serviceName "ListHandshakesForAccount" 


-- | <p>Lists the handshakes that are associated with the organization that the requesting user is part of. The <code>ListHandshakesForOrganization</code> operation returns a list of handshake structures. Each structure contains details and status about a handshake.</p> <p>Handshakes that are ACCEPTED, DECLINED, or CANCELED appear in the results of this API for only 30 days after changing to that state. After that they are deleted and no longer accessible.</p> <p>This operation can be called only from the organization's master account.</p>
listHandshakesForOrganization :: forall eff. ListHandshakesForOrganizationRequest -> Aff (err :: AWS.RequestError | eff) ListHandshakesForOrganizationResponse
listHandshakesForOrganization = AWS.request serviceName "ListHandshakesForOrganization" 


-- | <p>Lists the organizational units (OUs) in a parent organizational unit or root.</p> <p>This operation can be called only from the organization's master account.</p>
listOrganizationalUnitsForParent :: forall eff. ListOrganizationalUnitsForParentRequest -> Aff (err :: AWS.RequestError | eff) ListOrganizationalUnitsForParentResponse
listOrganizationalUnitsForParent = AWS.request serviceName "ListOrganizationalUnitsForParent" 


-- | <p>Lists the root or organizational units (OUs) that serve as the immediate parent of the specified child OU or account. This operation, along with <a>ListChildren</a> enables you to traverse the tree structure that makes up this root.</p> <p>This operation can be called only from the organization's master account.</p> <note> <p>In the current release, a child can have only a single parent. </p> </note>
listParents :: forall eff. ListParentsRequest -> Aff (err :: AWS.RequestError | eff) ListParentsResponse
listParents = AWS.request serviceName "ListParents" 


-- | <p>Retrieves the list of all policies in an organization of a specified type.</p> <p>This operation can be called only from the organization's master account.</p>
listPolicies :: forall eff. ListPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListPoliciesResponse
listPolicies = AWS.request serviceName "ListPolicies" 


-- | <p>Lists the policies that are directly attached to the specified target root, organizational unit (OU), or account. You must specify the policy type that you want included in the returned list.</p> <p>This operation can be called only from the organization's master account.</p>
listPoliciesForTarget :: forall eff. ListPoliciesForTargetRequest -> Aff (err :: AWS.RequestError | eff) ListPoliciesForTargetResponse
listPoliciesForTarget = AWS.request serviceName "ListPoliciesForTarget" 


-- | <p>Lists the roots that are defined in the current organization.</p> <p>This operation can be called only from the organization's master account.</p>
listRoots :: forall eff. ListRootsRequest -> Aff (err :: AWS.RequestError | eff) ListRootsResponse
listRoots = AWS.request serviceName "ListRoots" 


-- | <p>Lists all the roots, OUs, and accounts to which the specified policy is attached.</p> <p>This operation can be called only from the organization's master account.</p>
listTargetsForPolicy :: forall eff. ListTargetsForPolicyRequest -> Aff (err :: AWS.RequestError | eff) ListTargetsForPolicyResponse
listTargetsForPolicy = AWS.request serviceName "ListTargetsForPolicy" 


-- | <p>Moves an account from its current source parent root or OU to the specified destination parent root or OU.</p> <p>This operation can be called only from the organization's master account.</p>
moveAccount :: forall eff. MoveAccountRequest -> Aff (err :: AWS.RequestError | eff) Unit
moveAccount = AWS.request serviceName "MoveAccount" 


-- | <p>Removes the specified account from the organization.</p> <p>The removed account becomes a stand-alone account that is not a member of any organization. It is no longer subject to any policies and is responsible for its own bill payments. The organization's master account is no longer charged for any expenses accrued by the member account after it is removed from the organization.</p> <p>This operation can be called only from the organization's master account. Member accounts can remove themselves with <a>LeaveOrganization</a> instead.</p> <important> <ul> <li> <p>You can remove an account from your organization only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is <i>not</i> automatically collected. For an account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. To remove an account that does not yet have this information, you must sign in as the member account and follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info"> To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>You can remove a member account only after you enable IAM user access to billing in the member account. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate">Activating Access to the Billing and Cost Management Console</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p> </li> </ul> </important>
removeAccountFromOrganization :: forall eff. RemoveAccountFromOrganizationRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeAccountFromOrganization = AWS.request serviceName "RemoveAccountFromOrganization" 


-- | <p>Renames the specified organizational unit (OU). The ID and ARN do not change. The child OUs and accounts remain in place, and any attached policies of the OU remain attached. </p> <p>This operation can be called only from the organization's master account.</p>
updateOrganizationalUnit :: forall eff. UpdateOrganizationalUnitRequest -> Aff (err :: AWS.RequestError | eff) UpdateOrganizationalUnitResponse
updateOrganizationalUnit = AWS.request serviceName "UpdateOrganizationalUnit" 


-- | <p>Updates an existing policy with a new name, description, or content. If any parameter is not supplied, that value remains unchanged. Note that you cannot change a policy's type.</p> <p>This operation can be called only from the organization's master account.</p>
updatePolicy :: forall eff. UpdatePolicyRequest -> Aff (err :: AWS.RequestError | eff) UpdatePolicyResponse
updatePolicy = AWS.request serviceName "UpdatePolicy" 


-- | <p>Your account is not a member of an organization. To make this request, you must use the credentials of an account that belongs to an organization.</p>
newtype AWSOrganizationsNotInUseException = AWSOrganizationsNotInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAWSOrganizationsNotInUseException :: Newtype AWSOrganizationsNotInUseException _


newtype AcceptHandshakeRequest = AcceptHandshakeRequest 
  { "HandshakeId" :: (HandshakeId)
  }
derive instance newtypeAcceptHandshakeRequest :: Newtype AcceptHandshakeRequest _


newtype AcceptHandshakeResponse = AcceptHandshakeResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeAcceptHandshakeResponse :: Newtype AcceptHandshakeResponse _


-- | <p>You don't have permissions to perform the requested operation. The user or role that is making the request must have at least one IAM permissions policy attached that grants the required permissions. For more information, see <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/access.html">Access Management</a> in the <i>IAM User Guide</i>.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


-- | <p>The operation you attempted requires you to have the <code>iam:CreateServiceLinkedRole</code> so that Organizations can create the required service-linked role. You do not have that permission.</p>
newtype AccessDeniedForDependencyException = AccessDeniedForDependencyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "Reason" :: NullOrUndefined (AccessDeniedForDependencyExceptionReason)
  }
derive instance newtypeAccessDeniedForDependencyException :: Newtype AccessDeniedForDependencyException _


newtype AccessDeniedForDependencyExceptionReason = AccessDeniedForDependencyExceptionReason String
derive instance newtypeAccessDeniedForDependencyExceptionReason :: Newtype AccessDeniedForDependencyExceptionReason _


-- | <p>Contains information about an AWS account that is a member of an organization.</p>
newtype Account = Account 
  { "Id" :: NullOrUndefined (AccountId)
  , "Arn" :: NullOrUndefined (AccountArn)
  , "Email" :: NullOrUndefined (Email)
  , "Name" :: NullOrUndefined (AccountName)
  , "Status" :: NullOrUndefined (AccountStatus)
  , "JoinedMethod" :: NullOrUndefined (AccountJoinedMethod)
  , "JoinedTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeAccount :: Newtype Account _


newtype AccountArn = AccountArn String
derive instance newtypeAccountArn :: Newtype AccountArn _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


newtype AccountJoinedMethod = AccountJoinedMethod String
derive instance newtypeAccountJoinedMethod :: Newtype AccountJoinedMethod _


newtype AccountName = AccountName String
derive instance newtypeAccountName :: Newtype AccountName _


-- | <p> We can't find an AWS account with the AccountId that you specified, or the account whose credentials you used to make this request is not a member of an organization.</p>
newtype AccountNotFoundException = AccountNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccountNotFoundException :: Newtype AccountNotFoundException _


newtype AccountStatus = AccountStatus String
derive instance newtypeAccountStatus :: Newtype AccountStatus _


newtype Accounts = Accounts (Array Account)
derive instance newtypeAccounts :: Newtype Accounts _


newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _


-- | <p>This account is already a member of an organization. An account can belong to only one organization at a time.</p>
newtype AlreadyInOrganizationException = AlreadyInOrganizationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAlreadyInOrganizationException :: Newtype AlreadyInOrganizationException _


newtype AttachPolicyRequest = AttachPolicyRequest 
  { "PolicyId" :: (PolicyId)
  , "TargetId" :: (PolicyTargetId)
  }
derive instance newtypeAttachPolicyRequest :: Newtype AttachPolicyRequest _


newtype AwsManagedPolicy = AwsManagedPolicy Boolean
derive instance newtypeAwsManagedPolicy :: Newtype AwsManagedPolicy _


newtype CancelHandshakeRequest = CancelHandshakeRequest 
  { "HandshakeId" :: (HandshakeId)
  }
derive instance newtypeCancelHandshakeRequest :: Newtype CancelHandshakeRequest _


newtype CancelHandshakeResponse = CancelHandshakeResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeCancelHandshakeResponse :: Newtype CancelHandshakeResponse _


-- | <p>Contains a list of child entities, either OUs or accounts.</p>
newtype Child = Child 
  { "Id" :: NullOrUndefined (ChildId)
  , "Type" :: NullOrUndefined (ChildType)
  }
derive instance newtypeChild :: Newtype Child _


newtype ChildId = ChildId String
derive instance newtypeChildId :: Newtype ChildId _


-- | <p>We can't find an organizational unit (OU) or AWS account with the ChildId that you specified.</p>
newtype ChildNotFoundException = ChildNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeChildNotFoundException :: Newtype ChildNotFoundException _


newtype ChildType = ChildType String
derive instance newtypeChildType :: Newtype ChildType _


newtype Children = Children (Array Child)
derive instance newtypeChildren :: Newtype Children _


-- | <p>The target of the operation is currently being modified by a different request. Try again later.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>Performing this operation violates a minimum or maximum value limit. For example, attempting to removing the last SCP from an OU or root, inviting or creating too many accounts to the organization, or attaching too many policies to an account, OU, or root. This exception includes a reason that contains additional information about the violated limit:</p> <p/> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. If you need more accounts, contact AWS Support to request an increase in your limit. </p> <p>Or, The number of invitations that you tried to send would cause you to exceed the limit of accounts in your organization. Send fewer invitations, or contact AWS Support to request an increase in the number of accounts.</p> <p> <b>Note</b>: deleted and closed accounts still count toward your limit.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important> </li> <li> <p>HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes you can send in one day.</p> </li> <li> <p>OU_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of organizational units you can have in an organization.</p> </li> <li> <p>OU_DEPTH_LIMIT_EXCEEDED: You attempted to create an organizational unit tree that is too many levels deep.</p> </li> <li> <p>POLICY_NUMBER_LIMIT_EXCEEDED. You attempted to exceed the number of policies that you can have in an organization.</p> </li> <li> <p>MAX_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to exceed the number of policies of a certain type that can be attached to an entity at one time.</p> </li> <li> <p>MIN_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to detach a policy from an entity that would cause the entity to have fewer than the minimum number of policies of a certain type required.</p> </li> <li> <p>ACCOUNT_CANNOT_LEAVE_WITHOUT_EULA: You attempted to remove an account from the organization that does not yet have enough information to exist as a stand-alone account. This account requires you to first agree to the AWS Customer Agreement. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>ACCOUNT_CANNOT_LEAVE_WITHOUT_PHONE_VERIFICATION: You attempted to remove an account from the organization that does not yet have enough information to exist as a stand-alone account. This account requires you to first complete phone verification. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>MASTER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To create an organization with this account, you first must associate a payment instrument, such as a credit card, with the account. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>MEMBER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To complete this operation with this member account, you first must associate a payment instrument, such as a credit card, with the account. Follow the steps at <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info">To leave an organization when all required account information has not yet been provided</a> in the <i>AWS Organizations User Guide</i>.</p> </li> <li> <p>ACCOUNT_CREATION_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of accounts that you can create in one day.</p> </li> <li> <p>MASTER_ACCOUNT_ADDRESS_DOES_NOT_MATCH_MARKETPLACE: To create an account in this organization, you first must migrate the organization's master account to the marketplace that corresponds to the master account's address. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be associated with the same marketplace.</p> </li> <li> <p>MASTER_ACCOUNT_MISSING_CONTACT_INFO: To complete this operation, you must first provide contact a valid address and phone number for the master account. Then try the operation again.</p> </li> </ul>
newtype ConstraintViolationException = ConstraintViolationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "Reason" :: NullOrUndefined (ConstraintViolationExceptionReason)
  }
derive instance newtypeConstraintViolationException :: Newtype ConstraintViolationException _


newtype ConstraintViolationExceptionReason = ConstraintViolationExceptionReason String
derive instance newtypeConstraintViolationExceptionReason :: Newtype ConstraintViolationExceptionReason _


newtype CreateAccountFailureReason = CreateAccountFailureReason String
derive instance newtypeCreateAccountFailureReason :: Newtype CreateAccountFailureReason _


newtype CreateAccountRequest = CreateAccountRequest 
  { "Email" :: (Email)
  , "AccountName" :: (AccountName)
  , "RoleName" :: NullOrUndefined (RoleName)
  , "IamUserAccessToBilling" :: NullOrUndefined (IAMUserAccessToBilling)
  }
derive instance newtypeCreateAccountRequest :: Newtype CreateAccountRequest _


newtype CreateAccountRequestId = CreateAccountRequestId String
derive instance newtypeCreateAccountRequestId :: Newtype CreateAccountRequestId _


newtype CreateAccountResponse = CreateAccountResponse 
  { "CreateAccountStatus" :: NullOrUndefined (CreateAccountStatus)
  }
derive instance newtypeCreateAccountResponse :: Newtype CreateAccountResponse _


newtype CreateAccountState = CreateAccountState String
derive instance newtypeCreateAccountState :: Newtype CreateAccountState _


newtype CreateAccountStates = CreateAccountStates (Array CreateAccountState)
derive instance newtypeCreateAccountStates :: Newtype CreateAccountStates _


-- | <p>Contains the status about a <a>CreateAccount</a> request to create an AWS account in an organization.</p>
newtype CreateAccountStatus = CreateAccountStatus 
  { "Id" :: NullOrUndefined (CreateAccountRequestId)
  , "AccountName" :: NullOrUndefined (AccountName)
  , "State" :: NullOrUndefined (CreateAccountState)
  , "RequestedTimestamp" :: NullOrUndefined (Number)
  , "CompletedTimestamp" :: NullOrUndefined (Number)
  , "AccountId" :: NullOrUndefined (AccountId)
  , "FailureReason" :: NullOrUndefined (CreateAccountFailureReason)
  }
derive instance newtypeCreateAccountStatus :: Newtype CreateAccountStatus _


-- | <p>We can't find an create account request with the CreateAccountRequestId that you specified.</p>
newtype CreateAccountStatusNotFoundException = CreateAccountStatusNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeCreateAccountStatusNotFoundException :: Newtype CreateAccountStatusNotFoundException _


newtype CreateAccountStatuses = CreateAccountStatuses (Array CreateAccountStatus)
derive instance newtypeCreateAccountStatuses :: Newtype CreateAccountStatuses _


newtype CreateOrganizationRequest = CreateOrganizationRequest 
  { "FeatureSet" :: NullOrUndefined (OrganizationFeatureSet)
  }
derive instance newtypeCreateOrganizationRequest :: Newtype CreateOrganizationRequest _


newtype CreateOrganizationResponse = CreateOrganizationResponse 
  { "Organization" :: NullOrUndefined (Organization)
  }
derive instance newtypeCreateOrganizationResponse :: Newtype CreateOrganizationResponse _


newtype CreateOrganizationalUnitRequest = CreateOrganizationalUnitRequest 
  { "ParentId" :: (ParentId)
  , "Name" :: (OrganizationalUnitName)
  }
derive instance newtypeCreateOrganizationalUnitRequest :: Newtype CreateOrganizationalUnitRequest _


newtype CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse 
  { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit)
  }
derive instance newtypeCreateOrganizationalUnitResponse :: Newtype CreateOrganizationalUnitResponse _


newtype CreatePolicyRequest = CreatePolicyRequest 
  { "Content" :: (PolicyContent)
  , "Description" :: (PolicyDescription)
  , "Name" :: (PolicyName)
  , "Type" :: (PolicyType)
  }
derive instance newtypeCreatePolicyRequest :: Newtype CreatePolicyRequest _


newtype CreatePolicyResponse = CreatePolicyResponse 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeCreatePolicyResponse :: Newtype CreatePolicyResponse _


newtype DeclineHandshakeRequest = DeclineHandshakeRequest 
  { "HandshakeId" :: (HandshakeId)
  }
derive instance newtypeDeclineHandshakeRequest :: Newtype DeclineHandshakeRequest _


newtype DeclineHandshakeResponse = DeclineHandshakeResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeDeclineHandshakeResponse :: Newtype DeclineHandshakeResponse _


newtype DeleteOrganizationalUnitRequest = DeleteOrganizationalUnitRequest 
  { "OrganizationalUnitId" :: (OrganizationalUnitId)
  }
derive instance newtypeDeleteOrganizationalUnitRequest :: Newtype DeleteOrganizationalUnitRequest _


newtype DeletePolicyRequest = DeletePolicyRequest 
  { "PolicyId" :: (PolicyId)
  }
derive instance newtypeDeletePolicyRequest :: Newtype DeletePolicyRequest _


newtype DescribeAccountRequest = DescribeAccountRequest 
  { "AccountId" :: (AccountId)
  }
derive instance newtypeDescribeAccountRequest :: Newtype DescribeAccountRequest _


newtype DescribeAccountResponse = DescribeAccountResponse 
  { "Account" :: NullOrUndefined (Account)
  }
derive instance newtypeDescribeAccountResponse :: Newtype DescribeAccountResponse _


newtype DescribeCreateAccountStatusRequest = DescribeCreateAccountStatusRequest 
  { "CreateAccountRequestId" :: (CreateAccountRequestId)
  }
derive instance newtypeDescribeCreateAccountStatusRequest :: Newtype DescribeCreateAccountStatusRequest _


newtype DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse 
  { "CreateAccountStatus" :: NullOrUndefined (CreateAccountStatus)
  }
derive instance newtypeDescribeCreateAccountStatusResponse :: Newtype DescribeCreateAccountStatusResponse _


newtype DescribeHandshakeRequest = DescribeHandshakeRequest 
  { "HandshakeId" :: (HandshakeId)
  }
derive instance newtypeDescribeHandshakeRequest :: Newtype DescribeHandshakeRequest _


newtype DescribeHandshakeResponse = DescribeHandshakeResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeDescribeHandshakeResponse :: Newtype DescribeHandshakeResponse _


newtype DescribeOrganizationResponse = DescribeOrganizationResponse 
  { "Organization" :: NullOrUndefined (Organization)
  }
derive instance newtypeDescribeOrganizationResponse :: Newtype DescribeOrganizationResponse _


newtype DescribeOrganizationalUnitRequest = DescribeOrganizationalUnitRequest 
  { "OrganizationalUnitId" :: (OrganizationalUnitId)
  }
derive instance newtypeDescribeOrganizationalUnitRequest :: Newtype DescribeOrganizationalUnitRequest _


newtype DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse 
  { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit)
  }
derive instance newtypeDescribeOrganizationalUnitResponse :: Newtype DescribeOrganizationalUnitResponse _


newtype DescribePolicyRequest = DescribePolicyRequest 
  { "PolicyId" :: (PolicyId)
  }
derive instance newtypeDescribePolicyRequest :: Newtype DescribePolicyRequest _


newtype DescribePolicyResponse = DescribePolicyResponse 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeDescribePolicyResponse :: Newtype DescribePolicyResponse _


-- | <p>We can't find the destination container (a root or OU) with the ParentId that you specified.</p>
newtype DestinationParentNotFoundException = DestinationParentNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDestinationParentNotFoundException :: Newtype DestinationParentNotFoundException _


newtype DetachPolicyRequest = DetachPolicyRequest 
  { "PolicyId" :: (PolicyId)
  , "TargetId" :: (PolicyTargetId)
  }
derive instance newtypeDetachPolicyRequest :: Newtype DetachPolicyRequest _


newtype DisableAWSServiceAccessRequest = DisableAWSServiceAccessRequest 
  { "ServicePrincipal" :: (ServicePrincipal)
  }
derive instance newtypeDisableAWSServiceAccessRequest :: Newtype DisableAWSServiceAccessRequest _


newtype DisablePolicyTypeRequest = DisablePolicyTypeRequest 
  { "RootId" :: (RootId)
  , "PolicyType" :: (PolicyType)
  }
derive instance newtypeDisablePolicyTypeRequest :: Newtype DisablePolicyTypeRequest _


newtype DisablePolicyTypeResponse = DisablePolicyTypeResponse 
  { "Root" :: NullOrUndefined (Root)
  }
derive instance newtypeDisablePolicyTypeResponse :: Newtype DisablePolicyTypeResponse _


-- | <p>That account is already present in the specified destination.</p>
newtype DuplicateAccountException = DuplicateAccountException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDuplicateAccountException :: Newtype DuplicateAccountException _


-- | <p>A handshake with the same action and target already exists. For example, if you invited an account to join your organization, the invited account might already have a pending invitation from this organization. If you intend to resend an invitation to an account, ensure that existing handshakes that might be considered duplicates are canceled or declined.</p>
newtype DuplicateHandshakeException = DuplicateHandshakeException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDuplicateHandshakeException :: Newtype DuplicateHandshakeException _


-- | <p>An organizational unit (OU) with the same name already exists.</p>
newtype DuplicateOrganizationalUnitException = DuplicateOrganizationalUnitException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDuplicateOrganizationalUnitException :: Newtype DuplicateOrganizationalUnitException _


-- | <p>The selected policy is already attached to the specified target.</p>
newtype DuplicatePolicyAttachmentException = DuplicatePolicyAttachmentException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDuplicatePolicyAttachmentException :: Newtype DuplicatePolicyAttachmentException _


-- | <p>A policy with the same name already exists.</p>
newtype DuplicatePolicyException = DuplicatePolicyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeDuplicatePolicyException :: Newtype DuplicatePolicyException _


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


newtype EnableAWSServiceAccessRequest = EnableAWSServiceAccessRequest 
  { "ServicePrincipal" :: (ServicePrincipal)
  }
derive instance newtypeEnableAWSServiceAccessRequest :: Newtype EnableAWSServiceAccessRequest _


newtype EnableAllFeaturesRequest = EnableAllFeaturesRequest 
  { 
  }
derive instance newtypeEnableAllFeaturesRequest :: Newtype EnableAllFeaturesRequest _


newtype EnableAllFeaturesResponse = EnableAllFeaturesResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeEnableAllFeaturesResponse :: Newtype EnableAllFeaturesResponse _


newtype EnablePolicyTypeRequest = EnablePolicyTypeRequest 
  { "RootId" :: (RootId)
  , "PolicyType" :: (PolicyType)
  }
derive instance newtypeEnablePolicyTypeRequest :: Newtype EnablePolicyTypeRequest _


newtype EnablePolicyTypeResponse = EnablePolicyTypeResponse 
  { "Root" :: NullOrUndefined (Root)
  }
derive instance newtypeEnablePolicyTypeResponse :: Newtype EnablePolicyTypeResponse _


-- | <p>A structure that contains details of a service principal that is enabled to integrate with AWS Organizations.</p>
newtype EnabledServicePrincipal = EnabledServicePrincipal 
  { "ServicePrincipal" :: NullOrUndefined (ServicePrincipal)
  , "DateEnabled" :: NullOrUndefined (Number)
  }
derive instance newtypeEnabledServicePrincipal :: Newtype EnabledServicePrincipal _


newtype EnabledServicePrincipals = EnabledServicePrincipals (Array EnabledServicePrincipal)
derive instance newtypeEnabledServicePrincipals :: Newtype EnabledServicePrincipals _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


newtype ExceptionType = ExceptionType String
derive instance newtypeExceptionType :: Newtype ExceptionType _


-- | <p>AWS Organizations could not finalize the creation of your organization. Try again later. If this persists, contact AWS customer support.</p>
newtype FinalizingOrganizationException = FinalizingOrganizationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeFinalizingOrganizationException :: Newtype FinalizingOrganizationException _


newtype GenericArn = GenericArn String
derive instance newtypeGenericArn :: Newtype GenericArn _


-- | <p>Contains information that must be exchanged to securely establish a relationship between two accounts (an <i>originator</i> and a <i>recipient</i>). For example, when a master account (the originator) invites another account (the recipient) to join its organization, the two accounts exchange information as a series of handshake requests and responses.</p> <p> <b>Note:</b> Handshakes that are CANCELED, ACCEPTED, or DECLINED show up in lists for only 30 days after entering that state After that they are deleted.</p>
newtype Handshake = Handshake 
  { "Id" :: NullOrUndefined (HandshakeId)
  , "Arn" :: NullOrUndefined (HandshakeArn)
  , "Parties" :: NullOrUndefined (HandshakeParties)
  , "State" :: NullOrUndefined (HandshakeState)
  , "RequestedTimestamp" :: NullOrUndefined (Number)
  , "ExpirationTimestamp" :: NullOrUndefined (Number)
  , "Action" :: NullOrUndefined (ActionType)
  , "Resources" :: NullOrUndefined (HandshakeResources)
  }
derive instance newtypeHandshake :: Newtype Handshake _


-- | <p>The specified handshake is already in the requested state. For example, you can't accept a handshake that was already accepted.</p>
newtype HandshakeAlreadyInStateException = HandshakeAlreadyInStateException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeHandshakeAlreadyInStateException :: Newtype HandshakeAlreadyInStateException _


newtype HandshakeArn = HandshakeArn String
derive instance newtypeHandshakeArn :: Newtype HandshakeArn _


-- | <p>The requested operation would violate the constraint identified in the reason code.</p> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. <b>Note</b>: deleted and closed accounts still count toward your limit.</p> <important> <p>If you get an exception that indicates that you exceeded your account limits for the organization or that you can"t add an account because your organization is still initializing, please contact <a href="https://console.aws.amazon.com/support/home#/"> AWS Customer Support</a>.</p> </important> </li> <li> <p>HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes you can send in one day.</p> </li> <li> <p>ALREADY_IN_AN_ORGANIZATION: The handshake request is invalid because the invited account is already a member of an organization.</p> </li> <li> <p>ORGANIZATION_ALREADY_HAS_ALL_FEATURES: The handshake request is invalid because the organization has already enabled all features.</p> </li> <li> <p>INVITE_DISABLED_DURING_ENABLE_ALL_FEATURES: You cannot issue new invitations to join an organization while it is in the process of enabling all features. You can resume inviting accounts after you finalize the process when all accounts have agreed to the change.</p> </li> <li> <p>PAYMENT_INSTRUMENT_REQUIRED: You cannot complete the operation with an account that does not have a payment instrument, such as a credit card, associated with it.</p> </li> <li> <p>ORGANIZATION_FROM_DIFFERENT_SELLER_OF_RECORD: The request failed because the account is from a different marketplace than the accounts in the organization. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be from the same marketplace.</p> </li> <li> <p>ORGANIZATION_MEMBERSHIP_CHANGE_RATE_LIMIT_EXCEEDED: You attempted to change the membership of an account too quickly after its previous change.</p> </li> </ul>
newtype HandshakeConstraintViolationException = HandshakeConstraintViolationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "Reason" :: NullOrUndefined (HandshakeConstraintViolationExceptionReason)
  }
derive instance newtypeHandshakeConstraintViolationException :: Newtype HandshakeConstraintViolationException _


newtype HandshakeConstraintViolationExceptionReason = HandshakeConstraintViolationExceptionReason String
derive instance newtypeHandshakeConstraintViolationExceptionReason :: Newtype HandshakeConstraintViolationExceptionReason _


-- | <p>Specifies the criteria that are used to select the handshakes for the operation.</p>
newtype HandshakeFilter = HandshakeFilter 
  { "ActionType" :: NullOrUndefined (ActionType)
  , "ParentHandshakeId" :: NullOrUndefined (HandshakeId)
  }
derive instance newtypeHandshakeFilter :: Newtype HandshakeFilter _


newtype HandshakeId = HandshakeId String
derive instance newtypeHandshakeId :: Newtype HandshakeId _


-- | <p>We can't find a handshake with the HandshakeId that you specified.</p>
newtype HandshakeNotFoundException = HandshakeNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeHandshakeNotFoundException :: Newtype HandshakeNotFoundException _


newtype HandshakeNotes = HandshakeNotes String
derive instance newtypeHandshakeNotes :: Newtype HandshakeNotes _


newtype HandshakeParties = HandshakeParties (Array HandshakeParty)
derive instance newtypeHandshakeParties :: Newtype HandshakeParties _


-- | <p>Identifies a participant in a handshake.</p>
newtype HandshakeParty = HandshakeParty 
  { "Id" :: (HandshakePartyId)
  , "Type" :: (HandshakePartyType)
  }
derive instance newtypeHandshakeParty :: Newtype HandshakeParty _


newtype HandshakePartyId = HandshakePartyId String
derive instance newtypeHandshakePartyId :: Newtype HandshakePartyId _


newtype HandshakePartyType = HandshakePartyType String
derive instance newtypeHandshakePartyType :: Newtype HandshakePartyType _


-- | <p>Contains additional data that is needed to process a handshake.</p>
newtype HandshakeResource = HandshakeResource 
  { "Value" :: NullOrUndefined (HandshakeResourceValue)
  , "Type" :: NullOrUndefined (HandshakeResourceType)
  , "Resources" :: NullOrUndefined (HandshakeResources)
  }
derive instance newtypeHandshakeResource :: Newtype HandshakeResource _


newtype HandshakeResourceType = HandshakeResourceType String
derive instance newtypeHandshakeResourceType :: Newtype HandshakeResourceType _


newtype HandshakeResourceValue = HandshakeResourceValue String
derive instance newtypeHandshakeResourceValue :: Newtype HandshakeResourceValue _


newtype HandshakeResources = HandshakeResources (Array HandshakeResource)
derive instance newtypeHandshakeResources :: Newtype HandshakeResources _


newtype HandshakeState = HandshakeState String
derive instance newtypeHandshakeState :: Newtype HandshakeState _


newtype Handshakes = Handshakes (Array Handshake)
derive instance newtypeHandshakes :: Newtype Handshakes _


newtype IAMUserAccessToBilling = IAMUserAccessToBilling String
derive instance newtypeIAMUserAccessToBilling :: Newtype IAMUserAccessToBilling _


-- | <p>You can't perform the operation on the handshake in its current state. For example, you can't cancel a handshake that was already accepted, or accept a handshake that was already declined.</p>
newtype InvalidHandshakeTransitionException = InvalidHandshakeTransitionException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidHandshakeTransitionException :: Newtype InvalidHandshakeTransitionException _


-- | <p>The requested operation failed because you provided invalid values for one or more of the request parameters. This exception includes a reason that contains additional information about the violated limit:</p> <note> <p>Some of the reasons in the following list might not be applicable to this specific API or operation:</p> </note> <ul> <li> <p>INVALID_PARTY_TYPE_TARGET: You specified the wrong type of entity (account, organization, or email) as a party.</p> </li> <li> <p>INVALID_SYNTAX_ORGANIZATION_ARN: You specified an invalid ARN for the organization.</p> </li> <li> <p>INVALID_SYNTAX_POLICY_ID: You specified an invalid policy ID. </p> </li> <li> <p>INVALID_ENUM: You specified a value that is not valid for that parameter.</p> </li> <li> <p>INVALID_FULL_NAME_TARGET: You specified a full name that contains invalid characters.</p> </li> <li> <p>INVALID_LIST_MEMBER: You provided a list to a parameter that contains at least one invalid value.</p> </li> <li> <p>MAX_LENGTH_EXCEEDED: You provided a string parameter that is longer than allowed.</p> </li> <li> <p>MAX_VALUE_EXCEEDED: You provided a numeric parameter that has a larger value than allowed.</p> </li> <li> <p>MIN_LENGTH_EXCEEDED: You provided a string parameter that is shorter than allowed.</p> </li> <li> <p>MIN_VALUE_EXCEEDED: You provided a numeric parameter that has a smaller value than allowed.</p> </li> <li> <p>IMMUTABLE_POLICY: You specified a policy that is managed by AWS and cannot be modified.</p> </li> <li> <p>INVALID_PATTERN: You provided a value that doesn't match the required pattern.</p> </li> <li> <p>INVALID_PATTERN_TARGET_ID: You specified a policy target ID that doesn't match the required pattern.</p> </li> <li> <p>INPUT_REQUIRED: You must include a value for all required parameters.</p> </li> <li> <p>INVALID_PAGINATION_TOKEN: Get the value for the NextToken parameter from the response to a previous call of the operation.</p> </li> <li> <p>MAX_FILTER_LIMIT_EXCEEDED: You can specify only one filter parameter for the operation.</p> </li> <li> <p>MOVING_ACCOUNT_BETWEEN_DIFFERENT_ROOTS: You can move an account only between entities in the same root.</p> </li> </ul>
newtype InvalidInputException = InvalidInputException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "Reason" :: NullOrUndefined (InvalidInputExceptionReason)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


newtype InvalidInputExceptionReason = InvalidInputExceptionReason String
derive instance newtypeInvalidInputExceptionReason :: Newtype InvalidInputExceptionReason _


newtype InviteAccountToOrganizationRequest = InviteAccountToOrganizationRequest 
  { "Target" :: (HandshakeParty)
  , "Notes" :: NullOrUndefined (HandshakeNotes)
  }
derive instance newtypeInviteAccountToOrganizationRequest :: Newtype InviteAccountToOrganizationRequest _


newtype InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse 
  { "Handshake" :: NullOrUndefined (Handshake)
  }
derive instance newtypeInviteAccountToOrganizationResponse :: Newtype InviteAccountToOrganizationResponse _


newtype ListAWSServiceAccessForOrganizationRequest = ListAWSServiceAccessForOrganizationRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListAWSServiceAccessForOrganizationRequest :: Newtype ListAWSServiceAccessForOrganizationRequest _


newtype ListAWSServiceAccessForOrganizationResponse = ListAWSServiceAccessForOrganizationResponse 
  { "EnabledServicePrincipals" :: NullOrUndefined (EnabledServicePrincipals)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAWSServiceAccessForOrganizationResponse :: Newtype ListAWSServiceAccessForOrganizationResponse _


newtype ListAccountsForParentRequest = ListAccountsForParentRequest 
  { "ParentId" :: (ParentId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListAccountsForParentRequest :: Newtype ListAccountsForParentRequest _


newtype ListAccountsForParentResponse = ListAccountsForParentResponse 
  { "Accounts" :: NullOrUndefined (Accounts)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAccountsForParentResponse :: Newtype ListAccountsForParentResponse _


newtype ListAccountsRequest = ListAccountsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListAccountsRequest :: Newtype ListAccountsRequest _


newtype ListAccountsResponse = ListAccountsResponse 
  { "Accounts" :: NullOrUndefined (Accounts)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAccountsResponse :: Newtype ListAccountsResponse _


newtype ListChildrenRequest = ListChildrenRequest 
  { "ParentId" :: (ParentId)
  , "ChildType" :: (ChildType)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListChildrenRequest :: Newtype ListChildrenRequest _


newtype ListChildrenResponse = ListChildrenResponse 
  { "Children" :: NullOrUndefined (Children)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListChildrenResponse :: Newtype ListChildrenResponse _


newtype ListCreateAccountStatusRequest = ListCreateAccountStatusRequest 
  { "States" :: NullOrUndefined (CreateAccountStates)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListCreateAccountStatusRequest :: Newtype ListCreateAccountStatusRequest _


newtype ListCreateAccountStatusResponse = ListCreateAccountStatusResponse 
  { "CreateAccountStatuses" :: NullOrUndefined (CreateAccountStatuses)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListCreateAccountStatusResponse :: Newtype ListCreateAccountStatusResponse _


newtype ListHandshakesForAccountRequest = ListHandshakesForAccountRequest 
  { "Filter" :: NullOrUndefined (HandshakeFilter)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListHandshakesForAccountRequest :: Newtype ListHandshakesForAccountRequest _


newtype ListHandshakesForAccountResponse = ListHandshakesForAccountResponse 
  { "Handshakes" :: NullOrUndefined (Handshakes)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListHandshakesForAccountResponse :: Newtype ListHandshakesForAccountResponse _


newtype ListHandshakesForOrganizationRequest = ListHandshakesForOrganizationRequest 
  { "Filter" :: NullOrUndefined (HandshakeFilter)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListHandshakesForOrganizationRequest :: Newtype ListHandshakesForOrganizationRequest _


newtype ListHandshakesForOrganizationResponse = ListHandshakesForOrganizationResponse 
  { "Handshakes" :: NullOrUndefined (Handshakes)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListHandshakesForOrganizationResponse :: Newtype ListHandshakesForOrganizationResponse _


newtype ListOrganizationalUnitsForParentRequest = ListOrganizationalUnitsForParentRequest 
  { "ParentId" :: (ParentId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListOrganizationalUnitsForParentRequest :: Newtype ListOrganizationalUnitsForParentRequest _


newtype ListOrganizationalUnitsForParentResponse = ListOrganizationalUnitsForParentResponse 
  { "OrganizationalUnits" :: NullOrUndefined (OrganizationalUnits)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOrganizationalUnitsForParentResponse :: Newtype ListOrganizationalUnitsForParentResponse _


newtype ListParentsRequest = ListParentsRequest 
  { "ChildId" :: (ChildId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListParentsRequest :: Newtype ListParentsRequest _


newtype ListParentsResponse = ListParentsResponse 
  { "Parents" :: NullOrUndefined (Parents)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListParentsResponse :: Newtype ListParentsResponse _


newtype ListPoliciesForTargetRequest = ListPoliciesForTargetRequest 
  { "TargetId" :: (PolicyTargetId)
  , "Filter" :: (PolicyType)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListPoliciesForTargetRequest :: Newtype ListPoliciesForTargetRequest _


newtype ListPoliciesForTargetResponse = ListPoliciesForTargetResponse 
  { "Policies" :: NullOrUndefined (Policies)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPoliciesForTargetResponse :: Newtype ListPoliciesForTargetResponse _


newtype ListPoliciesRequest = ListPoliciesRequest 
  { "Filter" :: (PolicyType)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListPoliciesRequest :: Newtype ListPoliciesRequest _


newtype ListPoliciesResponse = ListPoliciesResponse 
  { "Policies" :: NullOrUndefined (Policies)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPoliciesResponse :: Newtype ListPoliciesResponse _


newtype ListRootsRequest = ListRootsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListRootsRequest :: Newtype ListRootsRequest _


newtype ListRootsResponse = ListRootsResponse 
  { "Roots" :: NullOrUndefined (Roots)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListRootsResponse :: Newtype ListRootsResponse _


newtype ListTargetsForPolicyRequest = ListTargetsForPolicyRequest 
  { "PolicyId" :: (PolicyId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTargetsForPolicyRequest :: Newtype ListTargetsForPolicyRequest _


newtype ListTargetsForPolicyResponse = ListTargetsForPolicyResponse 
  { "Targets" :: NullOrUndefined (PolicyTargets)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTargetsForPolicyResponse :: Newtype ListTargetsForPolicyResponse _


-- | <p>The provided policy document does not meet the requirements of the specified policy type. For example, the syntax might be incorrect. For details about service control policy syntax, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html">Service Control Policy Syntax</a> in the <i>AWS Organizations User Guide</i>.</p>
newtype MalformedPolicyDocumentException = MalformedPolicyDocumentException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeMalformedPolicyDocumentException :: Newtype MalformedPolicyDocumentException _


-- | <p>You can't remove a master account from an organization. If you want the master account to become a member account in another organization, you must first delete the current organization of the master account.</p>
newtype MasterCannotLeaveOrganizationException = MasterCannotLeaveOrganizationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeMasterCannotLeaveOrganizationException :: Newtype MasterCannotLeaveOrganizationException _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MoveAccountRequest = MoveAccountRequest 
  { "AccountId" :: (AccountId)
  , "SourceParentId" :: (ParentId)
  , "DestinationParentId" :: (ParentId)
  }
derive instance newtypeMoveAccountRequest :: Newtype MoveAccountRequest _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .</p>
newtype Organization = Organization 
  { "Id" :: NullOrUndefined (OrganizationId)
  , "Arn" :: NullOrUndefined (OrganizationArn)
  , "FeatureSet" :: NullOrUndefined (OrganizationFeatureSet)
  , "MasterAccountArn" :: NullOrUndefined (AccountArn)
  , "MasterAccountId" :: NullOrUndefined (AccountId)
  , "MasterAccountEmail" :: NullOrUndefined (Email)
  , "AvailablePolicyTypes" :: NullOrUndefined (PolicyTypes)
  }
derive instance newtypeOrganization :: Newtype Organization _


newtype OrganizationArn = OrganizationArn String
derive instance newtypeOrganizationArn :: Newtype OrganizationArn _


newtype OrganizationFeatureSet = OrganizationFeatureSet String
derive instance newtypeOrganizationFeatureSet :: Newtype OrganizationFeatureSet _


newtype OrganizationId = OrganizationId String
derive instance newtypeOrganizationId :: Newtype OrganizationId _


-- | <p>The organization isn't empty. To delete an organization, you must first remove all accounts except the master account, delete all organizational units (OUs), and delete all policies.</p>
newtype OrganizationNotEmptyException = OrganizationNotEmptyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeOrganizationNotEmptyException :: Newtype OrganizationNotEmptyException _


-- | <p>Contains details about an organizational unit (OU). An OU is a container of AWS accounts within a root of an organization. Policies that are attached to an OU apply to all accounts contained in that OU and in any child OUs.</p>
newtype OrganizationalUnit = OrganizationalUnit 
  { "Id" :: NullOrUndefined (OrganizationalUnitId)
  , "Arn" :: NullOrUndefined (OrganizationalUnitArn)
  , "Name" :: NullOrUndefined (OrganizationalUnitName)
  }
derive instance newtypeOrganizationalUnit :: Newtype OrganizationalUnit _


newtype OrganizationalUnitArn = OrganizationalUnitArn String
derive instance newtypeOrganizationalUnitArn :: Newtype OrganizationalUnitArn _


newtype OrganizationalUnitId = OrganizationalUnitId String
derive instance newtypeOrganizationalUnitId :: Newtype OrganizationalUnitId _


newtype OrganizationalUnitName = OrganizationalUnitName String
derive instance newtypeOrganizationalUnitName :: Newtype OrganizationalUnitName _


-- | <p>The specified organizational unit (OU) is not empty. Move all accounts to another root or to other OUs, remove all child OUs, and then try the operation again.</p>
newtype OrganizationalUnitNotEmptyException = OrganizationalUnitNotEmptyException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeOrganizationalUnitNotEmptyException :: Newtype OrganizationalUnitNotEmptyException _


-- | <p>We can't find an organizational unit (OU) with the OrganizationalUnitId that you specified.</p>
newtype OrganizationalUnitNotFoundException = OrganizationalUnitNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeOrganizationalUnitNotFoundException :: Newtype OrganizationalUnitNotFoundException _


newtype OrganizationalUnits = OrganizationalUnits (Array OrganizationalUnit)
derive instance newtypeOrganizationalUnits :: Newtype OrganizationalUnits _


-- | <p>Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.</p>
newtype Parent = Parent 
  { "Id" :: NullOrUndefined (ParentId)
  , "Type" :: NullOrUndefined (ParentType)
  }
derive instance newtypeParent :: Newtype Parent _


newtype ParentId = ParentId String
derive instance newtypeParentId :: Newtype ParentId _


-- | <p>We can't find a root or organizational unit (OU) with the ParentId that you specified.</p>
newtype ParentNotFoundException = ParentNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeParentNotFoundException :: Newtype ParentNotFoundException _


newtype ParentType = ParentType String
derive instance newtypeParentType :: Newtype ParentType _


newtype Parents = Parents (Array Parent)
derive instance newtypeParents :: Newtype Parents _


newtype Policies = Policies (Array PolicySummary)
derive instance newtypePolicies :: Newtype Policies _


-- | <p>Contains rules to be applied to the affected accounts. Policies can be attached directly to accounts, or to roots and OUs to affect all accounts in those hierarchies.</p>
newtype Policy = Policy 
  { "PolicySummary" :: NullOrUndefined (PolicySummary)
  , "Content" :: NullOrUndefined (PolicyContent)
  }
derive instance newtypePolicy :: Newtype Policy _


newtype PolicyArn = PolicyArn String
derive instance newtypePolicyArn :: Newtype PolicyArn _


newtype PolicyContent = PolicyContent String
derive instance newtypePolicyContent :: Newtype PolicyContent _


newtype PolicyDescription = PolicyDescription String
derive instance newtypePolicyDescription :: Newtype PolicyDescription _


newtype PolicyId = PolicyId String
derive instance newtypePolicyId :: Newtype PolicyId _


-- | <p>The policy is attached to one or more entities. You must detach it from all roots, organizational units (OUs), and accounts before performing this operation.</p>
newtype PolicyInUseException = PolicyInUseException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyInUseException :: Newtype PolicyInUseException _


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _


-- | <p>The policy isn't attached to the specified target in the specified root.</p>
newtype PolicyNotAttachedException = PolicyNotAttachedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyNotAttachedException :: Newtype PolicyNotAttachedException _


-- | <p>We can't find a policy with the PolicyId that you specified.</p>
newtype PolicyNotFoundException = PolicyNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyNotFoundException :: Newtype PolicyNotFoundException _


-- | <p>Contains information about a policy, but does not include the content. To see the content of a policy, see <a>DescribePolicy</a>.</p>
newtype PolicySummary = PolicySummary 
  { "Id" :: NullOrUndefined (PolicyId)
  , "Arn" :: NullOrUndefined (PolicyArn)
  , "Name" :: NullOrUndefined (PolicyName)
  , "Description" :: NullOrUndefined (PolicyDescription)
  , "Type" :: NullOrUndefined (PolicyType)
  , "AwsManaged" :: NullOrUndefined (AwsManagedPolicy)
  }
derive instance newtypePolicySummary :: Newtype PolicySummary _


newtype PolicyTargetId = PolicyTargetId String
derive instance newtypePolicyTargetId :: Newtype PolicyTargetId _


-- | <p>Contains information about a root, OU, or account that a policy is attached to.</p>
newtype PolicyTargetSummary = PolicyTargetSummary 
  { "TargetId" :: NullOrUndefined (PolicyTargetId)
  , "Arn" :: NullOrUndefined (GenericArn)
  , "Name" :: NullOrUndefined (TargetName)
  , "Type" :: NullOrUndefined (TargetType)
  }
derive instance newtypePolicyTargetSummary :: Newtype PolicyTargetSummary _


newtype PolicyTargets = PolicyTargets (Array PolicyTargetSummary)
derive instance newtypePolicyTargets :: Newtype PolicyTargets _


newtype PolicyType = PolicyType String
derive instance newtypePolicyType :: Newtype PolicyType _


-- | <p>The specified policy type is already enabled in the specified root.</p>
newtype PolicyTypeAlreadyEnabledException = PolicyTypeAlreadyEnabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyTypeAlreadyEnabledException :: Newtype PolicyTypeAlreadyEnabledException _


-- | <p>You can't use the specified policy type with the feature set currently enabled for this organization. For example, you can enable service control policies (SCPs) only after you enable all features in the organization. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root">Enabling and Disabling a Policy Type on a Root</a> in the <i>AWS Organizations User Guide</i>.</p>
newtype PolicyTypeNotAvailableForOrganizationException = PolicyTypeNotAvailableForOrganizationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyTypeNotAvailableForOrganizationException :: Newtype PolicyTypeNotAvailableForOrganizationException _


-- | <p>The specified policy type is not currently enabled in this root. You cannot attach policies of the specified type to entities in a root until you enable that type in the root. For more information, see <a href="http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html">Enabling All Features in Your Organization</a> in the <i>AWS Organizations User Guide</i>.</p>
newtype PolicyTypeNotEnabledException = PolicyTypeNotEnabledException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypePolicyTypeNotEnabledException :: Newtype PolicyTypeNotEnabledException _


newtype PolicyTypeStatus = PolicyTypeStatus String
derive instance newtypePolicyTypeStatus :: Newtype PolicyTypeStatus _


-- | <p>Contains information about a policy type and its status in the associated root.</p>
newtype PolicyTypeSummary = PolicyTypeSummary 
  { "Type" :: NullOrUndefined (PolicyType)
  , "Status" :: NullOrUndefined (PolicyTypeStatus)
  }
derive instance newtypePolicyTypeSummary :: Newtype PolicyTypeSummary _


newtype PolicyTypes = PolicyTypes (Array PolicyTypeSummary)
derive instance newtypePolicyTypes :: Newtype PolicyTypes _


newtype RemoveAccountFromOrganizationRequest = RemoveAccountFromOrganizationRequest 
  { "AccountId" :: (AccountId)
  }
derive instance newtypeRemoveAccountFromOrganizationRequest :: Newtype RemoveAccountFromOrganizationRequest _


newtype RoleName = RoleName String
derive instance newtypeRoleName :: Newtype RoleName _


-- | <p>Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. Every root contains every AWS account in the organization. Each root enables the accounts to be organized in a different way and to have different policy types enabled for use in that root.</p>
newtype Root = Root 
  { "Id" :: NullOrUndefined (RootId)
  , "Arn" :: NullOrUndefined (RootArn)
  , "Name" :: NullOrUndefined (RootName)
  , "PolicyTypes" :: NullOrUndefined (PolicyTypes)
  }
derive instance newtypeRoot :: Newtype Root _


newtype RootArn = RootArn String
derive instance newtypeRootArn :: Newtype RootArn _


newtype RootId = RootId String
derive instance newtypeRootId :: Newtype RootId _


newtype RootName = RootName String
derive instance newtypeRootName :: Newtype RootName _


-- | <p>We can't find a root with the RootId that you specified.</p>
newtype RootNotFoundException = RootNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeRootNotFoundException :: Newtype RootNotFoundException _


newtype Roots = Roots (Array Root)
derive instance newtypeRoots :: Newtype Roots _


-- | <p>AWS Organizations can't complete your request because of an internal service error. Try again later.</p>
newtype ServiceException = ServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeServiceException :: Newtype ServiceException _


newtype ServicePrincipal = ServicePrincipal String
derive instance newtypeServicePrincipal :: Newtype ServicePrincipal _


-- | <p>We can't find a source root or OU with the ParentId that you specified.</p>
newtype SourceParentNotFoundException = SourceParentNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeSourceParentNotFoundException :: Newtype SourceParentNotFoundException _


newtype TargetName = TargetName String
derive instance newtypeTargetName :: Newtype TargetName _


-- | <p>We can't find a root, OU, or account with the TargetId that you specified.</p>
newtype TargetNotFoundException = TargetNotFoundException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeTargetNotFoundException :: Newtype TargetNotFoundException _


newtype TargetType = TargetType String
derive instance newtypeTargetType :: Newtype TargetType _


-- | <p>You've sent too many requests in too short a period of time. The limit helps protect against denial-of-service attacks. Try again later.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Type" :: NullOrUndefined (ExceptionType)
  , "Message" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


newtype UpdateOrganizationalUnitRequest = UpdateOrganizationalUnitRequest 
  { "OrganizationalUnitId" :: (OrganizationalUnitId)
  , "Name" :: NullOrUndefined (OrganizationalUnitName)
  }
derive instance newtypeUpdateOrganizationalUnitRequest :: Newtype UpdateOrganizationalUnitRequest _


newtype UpdateOrganizationalUnitResponse = UpdateOrganizationalUnitResponse 
  { "OrganizationalUnit" :: NullOrUndefined (OrganizationalUnit)
  }
derive instance newtypeUpdateOrganizationalUnitResponse :: Newtype UpdateOrganizationalUnitResponse _


newtype UpdatePolicyRequest = UpdatePolicyRequest 
  { "PolicyId" :: (PolicyId)
  , "Name" :: NullOrUndefined (PolicyName)
  , "Description" :: NullOrUndefined (PolicyDescription)
  , "Content" :: NullOrUndefined (PolicyContent)
  }
derive instance newtypeUpdatePolicyRequest :: Newtype UpdatePolicyRequest _


newtype UpdatePolicyResponse = UpdatePolicyResponse 
  { "Policy" :: NullOrUndefined (Policy)
  }
derive instance newtypeUpdatePolicyResponse :: Newtype UpdatePolicyResponse _
