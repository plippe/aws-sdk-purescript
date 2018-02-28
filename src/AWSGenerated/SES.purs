

-- | <fullname>Amazon Simple Email Service</fullname> <p> This is the API Reference for <a href="https://aws.amazon.com/ses/">Amazon Simple Email Service</a> (Amazon SES). This documentation is intended to be used in conjunction with the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> <note> <p> For a list of Amazon SES endpoints to use in service requests, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html">Regions and Amazon SES</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> </note>
module AWS.SES where

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

serviceName = "SES" :: String


-- | <p>Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.</p> <p>For information about setting up rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
cloneReceiptRuleSet :: forall eff. CloneReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) CloneReceiptRuleSetResponse
cloneReceiptRuleSet = Request.request serviceName "cloneReceiptRuleSet" 


-- | <p>Creates a configuration set.</p> <p>Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createConfigurationSet :: forall eff. CreateConfigurationSetRequest -> Aff (exception :: EXCEPTION | eff) CreateConfigurationSetResponse
createConfigurationSet = Request.request serviceName "createConfigurationSet" 


-- | <p>Creates a configuration set event destination.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createConfigurationSetEventDestination :: forall eff. CreateConfigurationSetEventDestinationRequest -> Aff (exception :: EXCEPTION | eff) CreateConfigurationSetEventDestinationResponse
createConfigurationSetEventDestination = Request.request serviceName "createConfigurationSetEventDestination" 


-- | <p>Creates an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
createConfigurationSetTrackingOptions :: forall eff. CreateConfigurationSetTrackingOptionsRequest -> Aff (exception :: EXCEPTION | eff) CreateConfigurationSetTrackingOptionsResponse
createConfigurationSetTrackingOptions = Request.request serviceName "createConfigurationSetTrackingOptions" 


-- | <p>Creates a new custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
createCustomVerificationEmailTemplate :: forall eff. CreateCustomVerificationEmailTemplateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createCustomVerificationEmailTemplate = Request.request serviceName "createCustomVerificationEmailTemplate" 


-- | <p>Creates a new IP address filter.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptFilter :: forall eff. CreateReceiptFilterRequest -> Aff (exception :: EXCEPTION | eff) CreateReceiptFilterResponse
createReceiptFilter = Request.request serviceName "createReceiptFilter" 


-- | <p>Creates a receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptRule :: forall eff. CreateReceiptRuleRequest -> Aff (exception :: EXCEPTION | eff) CreateReceiptRuleResponse
createReceiptRule = Request.request serviceName "createReceiptRule" 


-- | <p>Creates an empty receipt rule set.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptRuleSet :: forall eff. CreateReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) CreateReceiptRuleSetResponse
createReceiptRuleSet = Request.request serviceName "createReceiptRuleSet" 


-- | <p>Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createTemplate :: forall eff. CreateTemplateRequest -> Aff (exception :: EXCEPTION | eff) CreateTemplateResponse
createTemplate = Request.request serviceName "createTemplate" 


-- | <p>Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteConfigurationSet :: forall eff. DeleteConfigurationSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteConfigurationSetResponse
deleteConfigurationSet = Request.request serviceName "deleteConfigurationSet" 


-- | <p>Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteConfigurationSetEventDestination :: forall eff. DeleteConfigurationSetEventDestinationRequest -> Aff (exception :: EXCEPTION | eff) DeleteConfigurationSetEventDestinationResponse
deleteConfigurationSetEventDestination = Request.request serviceName "deleteConfigurationSetEventDestination" 


-- | <p>Deletes an association between a configuration set and a custom domain for open and click event tracking.</p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p> <note> <p>Deleting this kind of association will result in emails sent using the specified configuration set to capture open and click events using the standard, Amazon SES-operated domains.</p> </note>
deleteConfigurationSetTrackingOptions :: forall eff. DeleteConfigurationSetTrackingOptionsRequest -> Aff (exception :: EXCEPTION | eff) DeleteConfigurationSetTrackingOptionsResponse
deleteConfigurationSetTrackingOptions = Request.request serviceName "deleteConfigurationSetTrackingOptions" 


-- | <p>Deletes an existing custom verification email template. </p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
deleteCustomVerificationEmailTemplate :: forall eff. DeleteCustomVerificationEmailTemplateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCustomVerificationEmailTemplate = Request.request serviceName "deleteCustomVerificationEmailTemplate" 


-- | <p>Deletes the specified identity (an email address or a domain) from the list of verified identities.</p> <p>You can execute this operation no more than once per second.</p>
deleteIdentity :: forall eff. DeleteIdentityRequest -> Aff (exception :: EXCEPTION | eff) DeleteIdentityResponse
deleteIdentity = Request.request serviceName "deleteIdentity" 


-- | <p>Deletes the specified sending authorization policy for the given identity (an email address or a domain). This API returns successfully even if a policy with the specified name does not exist.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteIdentityPolicy :: forall eff. DeleteIdentityPolicyRequest -> Aff (exception :: EXCEPTION | eff) DeleteIdentityPolicyResponse
deleteIdentityPolicy = Request.request serviceName "deleteIdentityPolicy" 


-- | <p>Deletes the specified IP address filter.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptFilter :: forall eff. DeleteReceiptFilterRequest -> Aff (exception :: EXCEPTION | eff) DeleteReceiptFilterResponse
deleteReceiptFilter = Request.request serviceName "deleteReceiptFilter" 


-- | <p>Deletes the specified receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptRule :: forall eff. DeleteReceiptRuleRequest -> Aff (exception :: EXCEPTION | eff) DeleteReceiptRuleResponse
deleteReceiptRule = Request.request serviceName "deleteReceiptRule" 


-- | <p>Deletes the specified receipt rule set and all of the receipt rules it contains.</p> <note> <p>The currently active rule set cannot be deleted.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptRuleSet :: forall eff. DeleteReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteReceiptRuleSetResponse
deleteReceiptRuleSet = Request.request serviceName "deleteReceiptRuleSet" 


-- | <p>Deletes an email template.</p> <p>You can execute this operation no more than once per second.</p>
deleteTemplate :: forall eff. DeleteTemplateRequest -> Aff (exception :: EXCEPTION | eff) DeleteTemplateResponse
deleteTemplate = Request.request serviceName "deleteTemplate" 


-- | <p>Deprecated. Use the <code>DeleteIdentity</code> operation to delete email addresses and domains.</p>
deleteVerifiedEmailAddress :: forall eff. DeleteVerifiedEmailAddressRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteVerifiedEmailAddress = Request.request serviceName "deleteVerifiedEmailAddress" 


-- | <p>Returns the metadata and receipt rules for the receipt rule set that is currently active.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeActiveReceiptRuleSet :: forall eff. DescribeActiveReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) DescribeActiveReceiptRuleSetResponse
describeActiveReceiptRuleSet = Request.request serviceName "describeActiveReceiptRuleSet" 


-- | <p>Returns the details of the specified configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeConfigurationSet :: forall eff. DescribeConfigurationSetRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigurationSetResponse
describeConfigurationSet = Request.request serviceName "describeConfigurationSet" 


-- | <p>Returns the details of the specified receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeReceiptRule :: forall eff. DescribeReceiptRuleRequest -> Aff (exception :: EXCEPTION | eff) DescribeReceiptRuleResponse
describeReceiptRule = Request.request serviceName "describeReceiptRule" 


-- | <p>Returns the details of the specified receipt rule set.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeReceiptRuleSet :: forall eff. DescribeReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) DescribeReceiptRuleSetResponse
describeReceiptRuleSet = Request.request serviceName "describeReceiptRuleSet" 


-- | <p>Returns the email sending status of the Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>
getAccountSendingEnabled :: forall eff.  Aff (exception :: EXCEPTION | eff) GetAccountSendingEnabledResponse
getAccountSendingEnabled = Request.request serviceName "getAccountSendingEnabled" (Types.NoInput unit)


-- | <p>Returns the custom email verification template for the template name you specify.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
getCustomVerificationEmailTemplate :: forall eff. GetCustomVerificationEmailTemplateRequest -> Aff (exception :: EXCEPTION | eff) GetCustomVerificationEmailTemplateResponse
getCustomVerificationEmailTemplate = Request.request serviceName "getCustomVerificationEmailTemplate" 


-- | <p>Returns the current status of Easy DKIM signing for an entity. For domain name identities, this operation also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES has successfully verified that these tokens have been published.</p> <p>This operation takes a list of identities as input and returns the following information for each:</p> <ul> <li> <p>Whether Easy DKIM signing is enabled or disabled.</p> </li> <li> <p>A set of DKIM tokens that represent the identity. If the identity is an email address, the tokens represent the domain of that address.</p> </li> <li> <p>Whether Amazon SES has successfully verified the DKIM tokens published in the domain's DNS. This information is only returned for domain name identities, not for email addresses.</p> </li> </ul> <p>This operation is throttled at one request per second and can only get DKIM attributes for up to 100 identities at a time.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>
getIdentityDkimAttributes :: forall eff. GetIdentityDkimAttributesRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityDkimAttributesResponse
getIdentityDkimAttributes = Request.request serviceName "getIdentityDkimAttributes" 


-- | <p>Returns the custom MAIL FROM attributes for a list of identities (email addresses : domains).</p> <p>This operation is throttled at one request per second and can only get custom MAIL FROM attributes for up to 100 identities at a time.</p>
getIdentityMailFromDomainAttributes :: forall eff. GetIdentityMailFromDomainAttributesRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityMailFromDomainAttributesResponse
getIdentityMailFromDomainAttributes = Request.request serviceName "getIdentityMailFromDomainAttributes" 


-- | <p>Given a list of verified identities (email addresses and/or domains), returns a structure describing identity notification attributes.</p> <p>This operation is throttled at one request per second and can only get notification attributes for up to 100 identities at a time.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
getIdentityNotificationAttributes :: forall eff. GetIdentityNotificationAttributesRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributes = Request.request serviceName "getIdentityNotificationAttributes" 


-- | <p>Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
getIdentityPolicies :: forall eff. GetIdentityPoliciesRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityPoliciesResponse
getIdentityPolicies = Request.request serviceName "getIdentityPolicies" 


-- | <p>Given a list of identities (email addresses and/or domains), returns the verification status and (for domain identities) the verification token for each identity.</p> <p>The verification status of an email address is "Pending" until the email address owner clicks the link within the verification email that Amazon SES sent to that address. If the email address owner clicks the link within 24 hours, the verification status of the email address changes to "Success". If the link is not clicked within 24 hours, the verification status changes to "Failed." In that case, if you still want to verify the email address, you must restart the verification process from the beginning.</p> <p>For domain identities, the domain's verification status is "Pending" as Amazon SES searches for the required TXT record in the DNS settings of the domain. When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.</p> <p>This operation is throttled at one request per second and can only get verification attributes for up to 100 identities at a time.</p>
getIdentityVerificationAttributes :: forall eff. GetIdentityVerificationAttributesRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributes = Request.request serviceName "getIdentityVerificationAttributes" 


-- | <p>Provides the sending limits for the Amazon SES account. </p> <p>You can execute this operation no more than once per second.</p>
getSendQuota :: forall eff.  Aff (exception :: EXCEPTION | eff) GetSendQuotaResponse
getSendQuota = Request.request serviceName "getSendQuota" (Types.NoInput unit)


-- | <p>Provides sending statistics for the Amazon SES account. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.</p> <p>You can execute this operation no more than once per second.</p>
getSendStatistics :: forall eff.  Aff (exception :: EXCEPTION | eff) GetSendStatisticsResponse
getSendStatistics = Request.request serviceName "getSendStatistics" (Types.NoInput unit)


-- | <p>Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.</p> <p>You can execute this operation no more than once per second.</p>
getTemplate :: forall eff. GetTemplateRequest -> Aff (exception :: EXCEPTION | eff) GetTemplateResponse
getTemplate = Request.request serviceName "getTemplate" 


-- | <p>Provides a list of the configuration sets associated with your Amazon SES account. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the <code>ListConfigurationSets</code> operation again, passing the <code>NextToken</code> parameter and the value of the NextToken element to retrieve additional results.</p>
listConfigurationSets :: forall eff. ListConfigurationSetsRequest -> Aff (exception :: EXCEPTION | eff) ListConfigurationSetsResponse
listConfigurationSets = Request.request serviceName "listConfigurationSets" 


-- | <p>Lists the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
listCustomVerificationEmailTemplates :: forall eff. ListCustomVerificationEmailTemplatesRequest -> Aff (exception :: EXCEPTION | eff) ListCustomVerificationEmailTemplatesResponse
listCustomVerificationEmailTemplates = Request.request serviceName "listCustomVerificationEmailTemplates" 


-- | <p>Returns a list containing all of the identities (email addresses and domains) for your AWS account, regardless of verification status.</p> <p>You can execute this operation no more than once per second.</p>
listIdentities :: forall eff. ListIdentitiesRequest -> Aff (exception :: EXCEPTION | eff) ListIdentitiesResponse
listIdentities = Request.request serviceName "listIdentities" 


-- | <p>Returns a list of sending authorization policies that are attached to the given identity (an email address or a domain). This API returns only a list. If you want the actual policy content, you can use <code>GetIdentityPolicies</code>.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listIdentityPolicies :: forall eff. ListIdentityPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListIdentityPoliciesResponse
listIdentityPolicies = Request.request serviceName "listIdentityPolicies" 


-- | <p>Lists the IP address filters associated with your AWS account.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listReceiptFilters :: forall eff. ListReceiptFiltersRequest -> Aff (exception :: EXCEPTION | eff) ListReceiptFiltersResponse
listReceiptFilters = Request.request serviceName "listReceiptFilters" 


-- | <p>Lists the receipt rule sets that exist under your AWS account. If there are additional receipt rule sets to be retrieved, you will receive a <code>NextToken</code> that you can provide to the next call to <code>ListReceiptRuleSets</code> to retrieve the additional entries.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listReceiptRuleSets :: forall eff. ListReceiptRuleSetsRequest -> Aff (exception :: EXCEPTION | eff) ListReceiptRuleSetsResponse
listReceiptRuleSets = Request.request serviceName "listReceiptRuleSets" 


-- | <p>Lists the email templates present in your Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>
listTemplates :: forall eff. ListTemplatesRequest -> Aff (exception :: EXCEPTION | eff) ListTemplatesResponse
listTemplates = Request.request serviceName "listTemplates" 


-- | <p>Deprecated. Use the <code>ListIdentities</code> operation to list the email addresses and domains associated with your account.</p>
listVerifiedEmailAddresses :: forall eff.  Aff (exception :: EXCEPTION | eff) ListVerifiedEmailAddressesResponse
listVerifiedEmailAddresses = Request.request serviceName "listVerifiedEmailAddresses" (Types.NoInput unit)


-- | <p>Adds or updates a sending authorization policy for the specified identity (an email address or a domain).</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
putIdentityPolicy :: forall eff. PutIdentityPolicyRequest -> Aff (exception :: EXCEPTION | eff) PutIdentityPolicyResponse
putIdentityPolicy = Request.request serviceName "putIdentityPolicy" 


-- | <p>Reorders the receipt rules within a receipt rule set.</p> <note> <p>All of the rules in the rule set must be represented in this request. That is, this API will return an error if the reorder request doesn't explicitly position all of the rules.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
reorderReceiptRuleSet :: forall eff. ReorderReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) ReorderReceiptRuleSetResponse
reorderReceiptRuleSet = Request.request serviceName "reorderReceiptRuleSet" 


-- | <p>Generates and sends a bounce message to the sender of an email you received through Amazon SES. You can only use this API on an email up to 24 hours after you receive it.</p> <note> <p>You cannot use this API to send generic bounces for mail that was not received by Amazon SES.</p> </note> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
sendBounce :: forall eff. SendBounceRequest -> Aff (exception :: EXCEPTION | eff) SendBounceResponse
sendBounce = Request.request serviceName "sendBounce" 


-- | <p>Composes an email message to multiple destinations. The message body is created using an email template.</p> <p>In order to send email using the <code>SendBulkTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Each <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>
sendBulkTemplatedEmail :: forall eff. SendBulkTemplatedEmailRequest -> Aff (exception :: EXCEPTION | eff) SendBulkTemplatedEmailResponse
sendBulkTemplatedEmail = Request.request serviceName "sendBulkTemplatedEmail" 


-- | <p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a customized verification email is sent to the specified address.</p> <p>To use this operation, you must first create a custom verification email template. For more information about creating and using custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
sendCustomVerificationEmail :: forall eff. SendCustomVerificationEmailRequest -> Aff (exception :: EXCEPTION | eff) SendCustomVerificationEmailResponse
sendCustomVerificationEmail = Request.request serviceName "sendCustomVerificationEmail" 


-- | <p>Composes an email message and immediately queues it for sending. In order to send email using the <code>SendEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important>
sendEmail :: forall eff. SendEmailRequest -> Aff (exception :: EXCEPTION | eff) SendEmailResponse
sendEmail = Request.request serviceName "sendEmail" 


-- | <p>Composes an email message and immediately queues it for sending. When calling this operation, you may specify the message headers as well as the content. The <code>SendRawEmail</code> operation is particularly useful for sending multipart MIME emails (such as those that contain both a plain-text and an HTML version). </p> <p>In order to send email using the <code>SendRawEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendRawEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important> <p>Additionally, keep the following considerations in mind when using the <code>SendRawEmail</code> operation:</p> <ul> <li> <p>Although you can customize the message headers when using the <code>SendRawEmail</code> operation, Amazon SES will automatically apply its own <code>Message-ID</code> and <code>Date</code> headers; if you passed these headers when creating the message, they will be overwritten by the values that Amazon SES provides.</p> </li> <li> <p>If you are using sending authorization to send on behalf of another user, <code>SendRawEmail</code> enables you to specify the cross-account identity for the email's Source, From, and Return-Path parameters in one of two ways: you can pass optional parameters <code>SourceArn</code>, <code>FromArn</code>, and/or <code>ReturnPathArn</code> to the API, or you can include the following X-headers in the header of your raw email:</p> <ul> <li> <p> <code>X-SES-SOURCE-ARN</code> </p> </li> <li> <p> <code>X-SES-FROM-ARN</code> </p> </li> <li> <p> <code>X-SES-RETURN-PATH-ARN</code> </p> </li> </ul> <important> <p>Do not include these X-headers in the DKIM signature; Amazon SES will remove them before sending the email.</p> </important> <p>For most common sending authorization scenarios, we recommend that you specify the <code>SourceIdentityArn</code> parameter and not the <code>FromIdentityArn</code> or <code>ReturnPathIdentityArn</code> parameters. If you only specify the <code>SourceIdentityArn</code> parameter, Amazon SES will set the From and Return Path addresses to the identity specified in <code>SourceIdentityArn</code>. For more information about sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Using Sending Authorization with Amazon SES</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> </ul>
sendRawEmail :: forall eff. SendRawEmailRequest -> Aff (exception :: EXCEPTION | eff) SendRawEmailResponse
sendRawEmail = Request.request serviceName "sendRawEmail" 


-- | <p>Composes an email message using an email template and immediately queues it for sending.</p> <p>In order to send email using the <code>SendTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Calls to the <code>SendTemplatedEmail</code> operation may only include one <code>Destination</code> parameter. A destination is a set of recipients who will receive the same version of the email. The <code>Destination</code> parameter can include up to 50 recipients, across the To:, CC: and BCC: fields.</p> </li> <li> <p>The <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>
sendTemplatedEmail :: forall eff. SendTemplatedEmailRequest -> Aff (exception :: EXCEPTION | eff) SendTemplatedEmailResponse
sendTemplatedEmail = Request.request serviceName "sendTemplatedEmail" 


-- | <p>Sets the specified receipt rule set as the active receipt rule set.</p> <note> <p>To disable your email-receiving through Amazon SES completely, you can call this API with RuleSetName set to null.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
setActiveReceiptRuleSet :: forall eff. SetActiveReceiptRuleSetRequest -> Aff (exception :: EXCEPTION | eff) SetActiveReceiptRuleSetResponse
setActiveReceiptRuleSet = Request.request serviceName "setActiveReceiptRuleSet" 


-- | <p>Enables or disables Easy DKIM signing of email sent from an identity:</p> <ul> <li> <p>If Easy DKIM signing is enabled for a domain name identity (such as <code>example.com</code>), then Amazon SES will DKIM-sign all email sent by addresses under that domain name (for example, <code>user@example.com</code>).</p> </li> <li> <p>If Easy DKIM signing is enabled for an email address, then Amazon SES will DKIM-sign all email sent by that email address.</p> </li> </ul> <p>For email addresses (for example, <code>user@example.com</code>), you can only enable Easy DKIM signing if the corresponding domain (in this case, <code>example.com</code>) has been set up for Easy DKIM using the AWS Console or the <code>VerifyDomainDkim</code> operation.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about Easy DKIM signing, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
setIdentityDkimEnabled :: forall eff. SetIdentityDkimEnabledRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityDkimEnabledResponse
setIdentityDkimEnabled = Request.request serviceName "setIdentityDkimEnabled" 


-- | <p>Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.</p> <note> <p>Feedback forwarding does not apply to delivery notifications. Delivery notifications are only available through Amazon SNS.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityFeedbackForwardingEnabled :: forall eff. SetIdentityFeedbackForwardingEnabledRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabled = Request.request serviceName "setIdentityFeedbackForwardingEnabled" 


-- | <p>Given an identity (an email address or a domain), sets whether Amazon SES includes the original email headers in the Amazon Simple Notification Service (Amazon SNS) notifications of a specified type.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityHeadersInNotificationsEnabled :: forall eff. SetIdentityHeadersInNotificationsEnabledRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityHeadersInNotificationsEnabledResponse
setIdentityHeadersInNotificationsEnabled = Request.request serviceName "setIdentityHeadersInNotificationsEnabled" 


-- | <p>Enables or disables the custom MAIL FROM domain setup for a verified identity (an email address or a domain).</p> <important> <p>To send emails using the specified MAIL FROM domain, you must add an MX record to your MAIL FROM domain's DNS settings. If you want your emails to pass Sender Policy Framework (SPF) checks, you must also add or update an SPF record. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html">Amazon SES Developer Guide</a>.</p> </important> <p>You can execute this operation no more than once per second.</p>
setIdentityMailFromDomain :: forall eff. SetIdentityMailFromDomainRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityMailFromDomainResponse
setIdentityMailFromDomain = Request.request serviceName "setIdentityMailFromDomain" 


-- | <p>Given an identity (an email address or a domain), sets the Amazon Simple Notification Service (Amazon SNS) topic to which Amazon SES will publish bounce, complaint, and/or delivery notifications for emails sent with that identity as the <code>Source</code>.</p> <note> <p>Unless feedback forwarding is enabled, you must specify Amazon SNS topics for bounce and complaint notifications. For more information, see <code>SetIdentityFeedbackForwardingEnabled</code>.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about feedback notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityNotificationTopic :: forall eff. SetIdentityNotificationTopicRequest -> Aff (exception :: EXCEPTION | eff) SetIdentityNotificationTopicResponse
setIdentityNotificationTopic = Request.request serviceName "setIdentityNotificationTopic" 


-- | <p>Sets the position of the specified receipt rule in the receipt rule set.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
setReceiptRulePosition :: forall eff. SetReceiptRulePositionRequest -> Aff (exception :: EXCEPTION | eff) SetReceiptRulePositionResponse
setReceiptRulePosition = Request.request serviceName "setReceiptRulePosition" 


-- | <p>Creates a preview of the MIME content of an email when provided with a template and a set of replacement data.</p> <p>You can execute this operation no more than once per second.</p>
testRenderTemplate :: forall eff. TestRenderTemplateRequest -> Aff (exception :: EXCEPTION | eff) TestRenderTemplateResponse
testRenderTemplate = Request.request serviceName "testRenderTemplate" 


-- | <p>Enables or disables email sending across your entire Amazon SES account. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending across your Amazon SES account when reputation metrics (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>
updateAccountSendingEnabled :: forall eff. UpdateAccountSendingEnabledRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateAccountSendingEnabled = Request.request serviceName "updateAccountSendingEnabled" 


-- | <p>Updates the event destination of a configuration set. Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetEventDestination :: forall eff. UpdateConfigurationSetEventDestinationRequest -> Aff (exception :: EXCEPTION | eff) UpdateConfigurationSetEventDestinationResponse
updateConfigurationSetEventDestination = Request.request serviceName "updateConfigurationSetEventDestination" 


-- | <p>Enables or disables the publishing of reputation metrics for emails sent using a specific configuration set. Reputation metrics include bounce and complaint rates. These metrics are published to Amazon CloudWatch. By using Amazon CloudWatch, you can create alarms when bounce or complaint rates exceed a certain threshold.</p> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetReputationMetricsEnabled :: forall eff. UpdateConfigurationSetReputationMetricsEnabledRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateConfigurationSetReputationMetricsEnabled = Request.request serviceName "updateConfigurationSetReputationMetricsEnabled" 


-- | <p>Enables or disables email sending for messages sent using a specific configuration set. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetSendingEnabled :: forall eff. UpdateConfigurationSetSendingEnabledRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateConfigurationSetSendingEnabled = Request.request serviceName "updateConfigurationSetSendingEnabled" 


-- | <p>Modifies an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
updateConfigurationSetTrackingOptions :: forall eff. UpdateConfigurationSetTrackingOptionsRequest -> Aff (exception :: EXCEPTION | eff) UpdateConfigurationSetTrackingOptionsResponse
updateConfigurationSetTrackingOptions = Request.request serviceName "updateConfigurationSetTrackingOptions" 


-- | <p>Updates an existing custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
updateCustomVerificationEmailTemplate :: forall eff. UpdateCustomVerificationEmailTemplateRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateCustomVerificationEmailTemplate = Request.request serviceName "updateCustomVerificationEmailTemplate" 


-- | <p>Updates a receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
updateReceiptRule :: forall eff. UpdateReceiptRuleRequest -> Aff (exception :: EXCEPTION | eff) UpdateReceiptRuleResponse
updateReceiptRule = Request.request serviceName "updateReceiptRule" 


-- | <p>Updates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
updateTemplate :: forall eff. UpdateTemplateRequest -> Aff (exception :: EXCEPTION | eff) UpdateTemplateResponse
updateTemplate = Request.request serviceName "updateTemplate" 


-- | <p>Returns a set of DKIM tokens for a domain. DKIM <i>tokens</i> are character strings that represent your domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain.</p> <p>You can execute this operation no more than once per second.</p> <p>To enable or disable Easy DKIM signing for a domain, use the <code>SetIdentityDkimEnabled</code> operation.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>
verifyDomainDkim :: forall eff. VerifyDomainDkimRequest -> Aff (exception :: EXCEPTION | eff) VerifyDomainDkimResponse
verifyDomainDkim = Request.request serviceName "verifyDomainDkim" 


-- | <p>Adds a domain to the list of identities for your Amazon SES account and attempts to verify it. For more information about verifying domains, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second.</p>
verifyDomainIdentity :: forall eff. VerifyDomainIdentityRequest -> Aff (exception :: EXCEPTION | eff) VerifyDomainIdentityResponse
verifyDomainIdentity = Request.request serviceName "verifyDomainIdentity" 


-- | <p>Deprecated. Use the <code>VerifyEmailIdentity</code> operation to verify a new email address.</p>
verifyEmailAddress :: forall eff. VerifyEmailAddressRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
verifyEmailAddress = Request.request serviceName "verifyEmailAddress" 


-- | <p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a verification email is sent to the specified address.</p> <p>You can execute this operation no more than once per second.</p>
verifyEmailIdentity :: forall eff. VerifyEmailIdentityRequest -> Aff (exception :: EXCEPTION | eff) VerifyEmailIdentityResponse
verifyEmailIdentity = Request.request serviceName "verifyEmailIdentity" 


-- | <p>Indicates that email sending is disabled for your entire Amazon SES account.</p> <p>You can enable or disable email sending for your Amazon SES account using <a>UpdateAccountSendingEnabled</a>.</p>
newtype AccountSendingPausedException = AccountSendingPausedException Types.NoArguments
derive instance newtypeAccountSendingPausedException :: Newtype AccountSendingPausedException _
derive instance repGenericAccountSendingPausedException :: Generic AccountSendingPausedException _
instance showAccountSendingPausedException :: Show AccountSendingPausedException where
  show = genericShow
instance decodeAccountSendingPausedException :: Decode AccountSendingPausedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountSendingPausedException :: Encode AccountSendingPausedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action adds a header to the received email.</p> <p>For information about adding a header using a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html">Amazon SES Developer Guide</a>.</p>
newtype AddHeaderAction = AddHeaderAction 
  { "HeaderName" :: (HeaderName)
  , "HeaderValue" :: (HeaderValue)
  }
derive instance newtypeAddHeaderAction :: Newtype AddHeaderAction _
derive instance repGenericAddHeaderAction :: Generic AddHeaderAction _
instance showAddHeaderAction :: Show AddHeaderAction where
  show = genericShow
instance decodeAddHeaderAction :: Decode AddHeaderAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddHeaderAction :: Encode AddHeaderAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Address = Address String
derive instance newtypeAddress :: Newtype Address _
derive instance repGenericAddress :: Generic Address _
instance showAddress :: Show Address where
  show = genericShow
instance decodeAddress :: Decode Address where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddress :: Encode Address where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddressList = AddressList (Array Address)
derive instance newtypeAddressList :: Newtype AddressList _
derive instance repGenericAddressList :: Generic AddressList _
instance showAddressList :: Show AddressList where
  show = genericShow
instance decodeAddressList :: Decode AddressList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddressList :: Encode AddressList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a resource could not be created because of a naming conflict.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleOrRuleSetName)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _
derive instance repGenericAlreadyExistsException :: Generic AlreadyExistsException _
instance showAlreadyExistsException :: Show AlreadyExistsException where
  show = genericShow
instance decodeAlreadyExistsException :: Decode AlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyExistsException :: Encode AlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AmazonResourceName = AmazonResourceName String
derive instance newtypeAmazonResourceName :: Newtype AmazonResourceName _
derive instance repGenericAmazonResourceName :: Generic AmazonResourceName _
instance showAmazonResourceName :: Show AmazonResourceName where
  show = genericShow
instance decodeAmazonResourceName :: Decode AmazonResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmazonResourceName :: Encode AmazonResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArrivalDate = ArrivalDate Number
derive instance newtypeArrivalDate :: Newtype ArrivalDate _
derive instance repGenericArrivalDate :: Generic ArrivalDate _
instance showArrivalDate :: Show ArrivalDate where
  show = genericShow
instance decodeArrivalDate :: Decode ArrivalDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrivalDate :: Encode ArrivalDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BehaviorOnMXFailure = BehaviorOnMXFailure String
derive instance newtypeBehaviorOnMXFailure :: Newtype BehaviorOnMXFailure _
derive instance repGenericBehaviorOnMXFailure :: Generic BehaviorOnMXFailure _
instance showBehaviorOnMXFailure :: Show BehaviorOnMXFailure where
  show = genericShow
instance decodeBehaviorOnMXFailure :: Decode BehaviorOnMXFailure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBehaviorOnMXFailure :: Encode BehaviorOnMXFailure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.</p>
newtype Body = Body 
  { "Text" :: NullOrUndefined.NullOrUndefined (Content)
  , "Html" :: NullOrUndefined.NullOrUndefined (Content)
  }
derive instance newtypeBody :: Newtype Body _
derive instance repGenericBody :: Generic Body _
instance showBody :: Show Body where
  show = genericShow
instance decodeBody :: Decode Body where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBody :: Encode Body where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about sending a bounce message in response to a received email, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html">Amazon SES Developer Guide</a>.</p>
newtype BounceAction = BounceAction 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "SmtpReplyCode" :: (BounceSmtpReplyCode)
  , "StatusCode" :: NullOrUndefined.NullOrUndefined (BounceStatusCode)
  , "Message" :: (BounceMessage)
  , "Sender" :: (Address)
  }
derive instance newtypeBounceAction :: Newtype BounceAction _
derive instance repGenericBounceAction :: Generic BounceAction _
instance showBounceAction :: Show BounceAction where
  show = genericShow
instance decodeBounceAction :: Decode BounceAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBounceAction :: Encode BounceAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BounceMessage = BounceMessage String
derive instance newtypeBounceMessage :: Newtype BounceMessage _
derive instance repGenericBounceMessage :: Generic BounceMessage _
instance showBounceMessage :: Show BounceMessage where
  show = genericShow
instance decodeBounceMessage :: Decode BounceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBounceMessage :: Encode BounceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BounceSmtpReplyCode = BounceSmtpReplyCode String
derive instance newtypeBounceSmtpReplyCode :: Newtype BounceSmtpReplyCode _
derive instance repGenericBounceSmtpReplyCode :: Generic BounceSmtpReplyCode _
instance showBounceSmtpReplyCode :: Show BounceSmtpReplyCode where
  show = genericShow
instance decodeBounceSmtpReplyCode :: Decode BounceSmtpReplyCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBounceSmtpReplyCode :: Encode BounceSmtpReplyCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BounceStatusCode = BounceStatusCode String
derive instance newtypeBounceStatusCode :: Newtype BounceStatusCode _
derive instance repGenericBounceStatusCode :: Generic BounceStatusCode _
instance showBounceStatusCode :: Show BounceStatusCode where
  show = genericShow
instance decodeBounceStatusCode :: Decode BounceStatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBounceStatusCode :: Encode BounceStatusCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BounceType = BounceType String
derive instance newtypeBounceType :: Newtype BounceType _
derive instance repGenericBounceType :: Generic BounceType _
instance showBounceType :: Show BounceType where
  show = genericShow
instance decodeBounceType :: Decode BounceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBounceType :: Encode BounceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype BouncedRecipientInfo = BouncedRecipientInfo 
  { "Recipient" :: (Address)
  , "RecipientArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "BounceType" :: NullOrUndefined.NullOrUndefined (BounceType)
  , "RecipientDsnFields" :: NullOrUndefined.NullOrUndefined (RecipientDsnFields)
  }
derive instance newtypeBouncedRecipientInfo :: Newtype BouncedRecipientInfo _
derive instance repGenericBouncedRecipientInfo :: Generic BouncedRecipientInfo _
instance showBouncedRecipientInfo :: Show BouncedRecipientInfo where
  show = genericShow
instance decodeBouncedRecipientInfo :: Decode BouncedRecipientInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBouncedRecipientInfo :: Encode BouncedRecipientInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BouncedRecipientInfoList = BouncedRecipientInfoList (Array BouncedRecipientInfo)
derive instance newtypeBouncedRecipientInfoList :: Newtype BouncedRecipientInfoList _
derive instance repGenericBouncedRecipientInfoList :: Generic BouncedRecipientInfoList _
instance showBouncedRecipientInfoList :: Show BouncedRecipientInfoList where
  show = genericShow
instance decodeBouncedRecipientInfoList :: Decode BouncedRecipientInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBouncedRecipientInfoList :: Encode BouncedRecipientInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.</p>
newtype BulkEmailDestination = BulkEmailDestination 
  { "Destination" :: (Destination)
  , "ReplacementTags" :: NullOrUndefined.NullOrUndefined (MessageTagList)
  , "ReplacementTemplateData" :: NullOrUndefined.NullOrUndefined (TemplateData)
  }
derive instance newtypeBulkEmailDestination :: Newtype BulkEmailDestination _
derive instance repGenericBulkEmailDestination :: Generic BulkEmailDestination _
instance showBulkEmailDestination :: Show BulkEmailDestination where
  show = genericShow
instance decodeBulkEmailDestination :: Decode BulkEmailDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkEmailDestination :: Encode BulkEmailDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BulkEmailDestinationList = BulkEmailDestinationList (Array BulkEmailDestination)
derive instance newtypeBulkEmailDestinationList :: Newtype BulkEmailDestinationList _
derive instance repGenericBulkEmailDestinationList :: Generic BulkEmailDestinationList _
instance showBulkEmailDestinationList :: Show BulkEmailDestinationList where
  show = genericShow
instance decodeBulkEmailDestinationList :: Decode BulkEmailDestinationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkEmailDestinationList :: Encode BulkEmailDestinationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that contains the response from the <code>SendBulkTemplatedEmail</code> operation.</p>
newtype BulkEmailDestinationStatus = BulkEmailDestinationStatus 
  { "Status" :: NullOrUndefined.NullOrUndefined (BulkEmailStatus)
  , "Error" :: NullOrUndefined.NullOrUndefined (Error)
  , "MessageId" :: NullOrUndefined.NullOrUndefined (MessageId)
  }
derive instance newtypeBulkEmailDestinationStatus :: Newtype BulkEmailDestinationStatus _
derive instance repGenericBulkEmailDestinationStatus :: Generic BulkEmailDestinationStatus _
instance showBulkEmailDestinationStatus :: Show BulkEmailDestinationStatus where
  show = genericShow
instance decodeBulkEmailDestinationStatus :: Decode BulkEmailDestinationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkEmailDestinationStatus :: Encode BulkEmailDestinationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BulkEmailDestinationStatusList = BulkEmailDestinationStatusList (Array BulkEmailDestinationStatus)
derive instance newtypeBulkEmailDestinationStatusList :: Newtype BulkEmailDestinationStatusList _
derive instance repGenericBulkEmailDestinationStatusList :: Generic BulkEmailDestinationStatusList _
instance showBulkEmailDestinationStatusList :: Show BulkEmailDestinationStatusList where
  show = genericShow
instance decodeBulkEmailDestinationStatusList :: Decode BulkEmailDestinationStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkEmailDestinationStatusList :: Encode BulkEmailDestinationStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BulkEmailStatus = BulkEmailStatus String
derive instance newtypeBulkEmailStatus :: Newtype BulkEmailStatus _
derive instance repGenericBulkEmailStatus :: Generic BulkEmailStatus _
instance showBulkEmailStatus :: Show BulkEmailStatus where
  show = genericShow
instance decodeBulkEmailStatus :: Decode BulkEmailStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBulkEmailStatus :: Encode BulkEmailStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the delete operation could not be completed.</p>
newtype CannotDeleteException = CannotDeleteException 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleOrRuleSetName)
  }
derive instance newtypeCannotDeleteException :: Newtype CannotDeleteException _
derive instance repGenericCannotDeleteException :: Generic CannotDeleteException _
instance showCannotDeleteException :: Show CannotDeleteException where
  show = genericShow
instance decodeCannotDeleteException :: Decode CannotDeleteException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCannotDeleteException :: Encode CannotDeleteException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Charset = Charset String
derive instance newtypeCharset :: Newtype Charset _
derive instance repGenericCharset :: Generic Charset _
instance showCharset :: Show Charset where
  show = genericShow
instance decodeCharset :: Decode Charset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCharset :: Encode Charset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Cidr = Cidr String
derive instance newtypeCidr :: Newtype Cidr _
derive instance repGenericCidr :: Generic Cidr _
instance showCidr :: Show Cidr where
  show = genericShow
instance decodeCidr :: Decode Cidr where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCidr :: Encode Cidr where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CloneReceiptRuleSetRequest = CloneReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "OriginalRuleSetName" :: (ReceiptRuleSetName)
  }
derive instance newtypeCloneReceiptRuleSetRequest :: Newtype CloneReceiptRuleSetRequest _
derive instance repGenericCloneReceiptRuleSetRequest :: Generic CloneReceiptRuleSetRequest _
instance showCloneReceiptRuleSetRequest :: Show CloneReceiptRuleSetRequest where
  show = genericShow
instance decodeCloneReceiptRuleSetRequest :: Decode CloneReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloneReceiptRuleSetRequest :: Encode CloneReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse Types.NoArguments
derive instance newtypeCloneReceiptRuleSetResponse :: Newtype CloneReceiptRuleSetResponse _
derive instance repGenericCloneReceiptRuleSetResponse :: Generic CloneReceiptRuleSetResponse _
instance showCloneReceiptRuleSetResponse :: Show CloneReceiptRuleSetResponse where
  show = genericShow
instance decodeCloneReceiptRuleSetResponse :: Decode CloneReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloneReceiptRuleSetResponse :: Encode CloneReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.</p> <p>Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CloudWatchDestination = CloudWatchDestination 
  { "DimensionConfigurations" :: (CloudWatchDimensionConfigurations)
  }
derive instance newtypeCloudWatchDestination :: Newtype CloudWatchDestination _
derive instance repGenericCloudWatchDestination :: Generic CloudWatchDestination _
instance showCloudWatchDestination :: Show CloudWatchDestination where
  show = genericShow
instance decodeCloudWatchDestination :: Decode CloudWatchDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchDestination :: Encode CloudWatchDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.</p> <p>For information about publishing email sending events to Amazon CloudWatch, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration 
  { "DimensionName" :: (DimensionName)
  , "DimensionValueSource" :: (DimensionValueSource)
  , "DefaultDimensionValue" :: (DefaultDimensionValue)
  }
derive instance newtypeCloudWatchDimensionConfiguration :: Newtype CloudWatchDimensionConfiguration _
derive instance repGenericCloudWatchDimensionConfiguration :: Generic CloudWatchDimensionConfiguration _
instance showCloudWatchDimensionConfiguration :: Show CloudWatchDimensionConfiguration where
  show = genericShow
instance decodeCloudWatchDimensionConfiguration :: Decode CloudWatchDimensionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchDimensionConfiguration :: Encode CloudWatchDimensionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchDimensionConfigurations = CloudWatchDimensionConfigurations (Array CloudWatchDimensionConfiguration)
derive instance newtypeCloudWatchDimensionConfigurations :: Newtype CloudWatchDimensionConfigurations _
derive instance repGenericCloudWatchDimensionConfigurations :: Generic CloudWatchDimensionConfigurations _
instance showCloudWatchDimensionConfigurations :: Show CloudWatchDimensionConfigurations where
  show = genericShow
instance decodeCloudWatchDimensionConfigurations :: Decode CloudWatchDimensionConfigurations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchDimensionConfigurations :: Encode CloudWatchDimensionConfigurations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the configuration set.</p> <p>Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html">Using Amazon SES Configuration Sets</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/">Amazon SES Developer Guide</a>.</p>
newtype ConfigurationSet = ConfigurationSet 
  { "Name" :: (ConfigurationSetName)
  }
derive instance newtypeConfigurationSet :: Newtype ConfigurationSet _
derive instance repGenericConfigurationSet :: Generic ConfigurationSet _
instance showConfigurationSet :: Show ConfigurationSet where
  show = genericShow
instance decodeConfigurationSet :: Decode ConfigurationSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSet :: Encode ConfigurationSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the configuration set could not be created because of a naming conflict.</p>
newtype ConfigurationSetAlreadyExistsException = ConfigurationSetAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeConfigurationSetAlreadyExistsException :: Newtype ConfigurationSetAlreadyExistsException _
derive instance repGenericConfigurationSetAlreadyExistsException :: Generic ConfigurationSetAlreadyExistsException _
instance showConfigurationSetAlreadyExistsException :: Show ConfigurationSetAlreadyExistsException where
  show = genericShow
instance decodeConfigurationSetAlreadyExistsException :: Decode ConfigurationSetAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetAlreadyExistsException :: Encode ConfigurationSetAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationSetAttribute = ConfigurationSetAttribute String
derive instance newtypeConfigurationSetAttribute :: Newtype ConfigurationSetAttribute _
derive instance repGenericConfigurationSetAttribute :: Generic ConfigurationSetAttribute _
instance showConfigurationSetAttribute :: Show ConfigurationSetAttribute where
  show = genericShow
instance decodeConfigurationSetAttribute :: Decode ConfigurationSetAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetAttribute :: Encode ConfigurationSetAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationSetAttributeList = ConfigurationSetAttributeList (Array ConfigurationSetAttribute)
derive instance newtypeConfigurationSetAttributeList :: Newtype ConfigurationSetAttributeList _
derive instance repGenericConfigurationSetAttributeList :: Generic ConfigurationSetAttributeList _
instance showConfigurationSetAttributeList :: Show ConfigurationSetAttributeList where
  show = genericShow
instance decodeConfigurationSetAttributeList :: Decode ConfigurationSetAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetAttributeList :: Encode ConfigurationSetAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the configuration set does not exist.</p>
newtype ConfigurationSetDoesNotExistException = ConfigurationSetDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeConfigurationSetDoesNotExistException :: Newtype ConfigurationSetDoesNotExistException _
derive instance repGenericConfigurationSetDoesNotExistException :: Generic ConfigurationSetDoesNotExistException _
instance showConfigurationSetDoesNotExistException :: Show ConfigurationSetDoesNotExistException where
  show = genericShow
instance decodeConfigurationSetDoesNotExistException :: Decode ConfigurationSetDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetDoesNotExistException :: Encode ConfigurationSetDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationSetName = ConfigurationSetName String
derive instance newtypeConfigurationSetName :: Newtype ConfigurationSetName _
derive instance repGenericConfigurationSetName :: Generic ConfigurationSetName _
instance showConfigurationSetName :: Show ConfigurationSetName where
  show = genericShow
instance decodeConfigurationSetName :: Decode ConfigurationSetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetName :: Encode ConfigurationSetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that email sending is disabled for the configuration set.</p> <p>You can enable or disable email sending for a configuration set using <a>UpdateConfigurationSetSendingEnabled</a>.</p>
newtype ConfigurationSetSendingPausedException = ConfigurationSetSendingPausedException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeConfigurationSetSendingPausedException :: Newtype ConfigurationSetSendingPausedException _
derive instance repGenericConfigurationSetSendingPausedException :: Generic ConfigurationSetSendingPausedException _
instance showConfigurationSetSendingPausedException :: Show ConfigurationSetSendingPausedException where
  show = genericShow
instance decodeConfigurationSetSendingPausedException :: Decode ConfigurationSetSendingPausedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSetSendingPausedException :: Encode ConfigurationSetSendingPausedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationSets = ConfigurationSets (Array ConfigurationSet)
derive instance newtypeConfigurationSets :: Newtype ConfigurationSets _
derive instance repGenericConfigurationSets :: Generic ConfigurationSets _
instance showConfigurationSets :: Show ConfigurationSets where
  show = genericShow
instance decodeConfigurationSets :: Decode ConfigurationSets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSets :: Encode ConfigurationSets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents textual data, plus an optional character set specification.</p> <p>By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.</p>
newtype Content = Content 
  { "Data" :: (MessageData)
  , "Charset" :: NullOrUndefined.NullOrUndefined (Charset)
  }
derive instance newtypeContent :: Newtype Content _
derive instance repGenericContent :: Generic Content _
instance showContent :: Show Content where
  show = genericShow
instance decodeContent :: Decode Content where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContent :: Encode Content where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Counter = Counter Number
derive instance newtypeCounter :: Newtype Counter _
derive instance repGenericCounter :: Generic Counter _
instance showCounter :: Show Counter where
  show = genericShow
instance decodeCounter :: Decode Counter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCounter :: Encode Counter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CreateConfigurationSetEventDestinationRequest = CreateConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestination" :: (EventDestination)
  }
derive instance newtypeCreateConfigurationSetEventDestinationRequest :: Newtype CreateConfigurationSetEventDestinationRequest _
derive instance repGenericCreateConfigurationSetEventDestinationRequest :: Generic CreateConfigurationSetEventDestinationRequest _
instance showCreateConfigurationSetEventDestinationRequest :: Show CreateConfigurationSetEventDestinationRequest where
  show = genericShow
instance decodeCreateConfigurationSetEventDestinationRequest :: Decode CreateConfigurationSetEventDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetEventDestinationRequest :: Encode CreateConfigurationSetEventDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse Types.NoArguments
derive instance newtypeCreateConfigurationSetEventDestinationResponse :: Newtype CreateConfigurationSetEventDestinationResponse _
derive instance repGenericCreateConfigurationSetEventDestinationResponse :: Generic CreateConfigurationSetEventDestinationResponse _
instance showCreateConfigurationSetEventDestinationResponse :: Show CreateConfigurationSetEventDestinationResponse where
  show = genericShow
instance decodeCreateConfigurationSetEventDestinationResponse :: Decode CreateConfigurationSetEventDestinationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetEventDestinationResponse :: Encode CreateConfigurationSetEventDestinationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CreateConfigurationSetRequest = CreateConfigurationSetRequest 
  { "ConfigurationSet" :: (ConfigurationSet)
  }
derive instance newtypeCreateConfigurationSetRequest :: Newtype CreateConfigurationSetRequest _
derive instance repGenericCreateConfigurationSetRequest :: Generic CreateConfigurationSetRequest _
instance showCreateConfigurationSetRequest :: Show CreateConfigurationSetRequest where
  show = genericShow
instance decodeCreateConfigurationSetRequest :: Decode CreateConfigurationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetRequest :: Encode CreateConfigurationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetResponse = CreateConfigurationSetResponse Types.NoArguments
derive instance newtypeCreateConfigurationSetResponse :: Newtype CreateConfigurationSetResponse _
derive instance repGenericCreateConfigurationSetResponse :: Generic CreateConfigurationSetResponse _
instance showCreateConfigurationSetResponse :: Show CreateConfigurationSetResponse where
  show = genericShow
instance decodeCreateConfigurationSetResponse :: Decode CreateConfigurationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetResponse :: Encode CreateConfigurationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create an open and click tracking option object in a configuration set. </p>
newtype CreateConfigurationSetTrackingOptionsRequest = CreateConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "TrackingOptions" :: (TrackingOptions)
  }
derive instance newtypeCreateConfigurationSetTrackingOptionsRequest :: Newtype CreateConfigurationSetTrackingOptionsRequest _
derive instance repGenericCreateConfigurationSetTrackingOptionsRequest :: Generic CreateConfigurationSetTrackingOptionsRequest _
instance showCreateConfigurationSetTrackingOptionsRequest :: Show CreateConfigurationSetTrackingOptionsRequest where
  show = genericShow
instance decodeCreateConfigurationSetTrackingOptionsRequest :: Decode CreateConfigurationSetTrackingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetTrackingOptionsRequest :: Encode CreateConfigurationSetTrackingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetTrackingOptionsResponse = CreateConfigurationSetTrackingOptionsResponse Types.NoArguments
derive instance newtypeCreateConfigurationSetTrackingOptionsResponse :: Newtype CreateConfigurationSetTrackingOptionsResponse _
derive instance repGenericCreateConfigurationSetTrackingOptionsResponse :: Generic CreateConfigurationSetTrackingOptionsResponse _
instance showCreateConfigurationSetTrackingOptionsResponse :: Show CreateConfigurationSetTrackingOptionsResponse where
  show = genericShow
instance decodeCreateConfigurationSetTrackingOptionsResponse :: Decode CreateConfigurationSetTrackingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationSetTrackingOptionsResponse :: Encode CreateConfigurationSetTrackingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a custom verification email template.</p>
newtype CreateCustomVerificationEmailTemplateRequest = CreateCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "FromEmailAddress" :: (FromAddress)
  , "TemplateSubject" :: (Subject)
  , "TemplateContent" :: (TemplateContent)
  , "SuccessRedirectionURL" :: (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: (FailureRedirectionURL)
  }
derive instance newtypeCreateCustomVerificationEmailTemplateRequest :: Newtype CreateCustomVerificationEmailTemplateRequest _
derive instance repGenericCreateCustomVerificationEmailTemplateRequest :: Generic CreateCustomVerificationEmailTemplateRequest _
instance showCreateCustomVerificationEmailTemplateRequest :: Show CreateCustomVerificationEmailTemplateRequest where
  show = genericShow
instance decodeCreateCustomVerificationEmailTemplateRequest :: Decode CreateCustomVerificationEmailTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCustomVerificationEmailTemplateRequest :: Encode CreateCustomVerificationEmailTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a new IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptFilterRequest = CreateReceiptFilterRequest 
  { "Filter" :: (ReceiptFilter)
  }
derive instance newtypeCreateReceiptFilterRequest :: Newtype CreateReceiptFilterRequest _
derive instance repGenericCreateReceiptFilterRequest :: Generic CreateReceiptFilterRequest _
instance showCreateReceiptFilterRequest :: Show CreateReceiptFilterRequest where
  show = genericShow
instance decodeCreateReceiptFilterRequest :: Decode CreateReceiptFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptFilterRequest :: Encode CreateReceiptFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptFilterResponse = CreateReceiptFilterResponse Types.NoArguments
derive instance newtypeCreateReceiptFilterResponse :: Newtype CreateReceiptFilterResponse _
derive instance repGenericCreateReceiptFilterResponse :: Generic CreateReceiptFilterResponse _
instance showCreateReceiptFilterResponse :: Show CreateReceiptFilterResponse where
  show = genericShow
instance decodeCreateReceiptFilterResponse :: Decode CreateReceiptFilterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptFilterResponse :: Encode CreateReceiptFilterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptRuleRequest = CreateReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "After" :: NullOrUndefined.NullOrUndefined (ReceiptRuleName)
  , "Rule" :: (ReceiptRule)
  }
derive instance newtypeCreateReceiptRuleRequest :: Newtype CreateReceiptRuleRequest _
derive instance repGenericCreateReceiptRuleRequest :: Generic CreateReceiptRuleRequest _
instance showCreateReceiptRuleRequest :: Show CreateReceiptRuleRequest where
  show = genericShow
instance decodeCreateReceiptRuleRequest :: Decode CreateReceiptRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptRuleRequest :: Encode CreateReceiptRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptRuleResponse = CreateReceiptRuleResponse Types.NoArguments
derive instance newtypeCreateReceiptRuleResponse :: Newtype CreateReceiptRuleResponse _
derive instance repGenericCreateReceiptRuleResponse :: Generic CreateReceiptRuleResponse _
instance showCreateReceiptRuleResponse :: Show CreateReceiptRuleResponse where
  show = genericShow
instance decodeCreateReceiptRuleResponse :: Decode CreateReceiptRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptRuleResponse :: Encode CreateReceiptRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create an empty receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptRuleSetRequest = CreateReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }
derive instance newtypeCreateReceiptRuleSetRequest :: Newtype CreateReceiptRuleSetRequest _
derive instance repGenericCreateReceiptRuleSetRequest :: Generic CreateReceiptRuleSetRequest _
instance showCreateReceiptRuleSetRequest :: Show CreateReceiptRuleSetRequest where
  show = genericShow
instance decodeCreateReceiptRuleSetRequest :: Decode CreateReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptRuleSetRequest :: Encode CreateReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse Types.NoArguments
derive instance newtypeCreateReceiptRuleSetResponse :: Newtype CreateReceiptRuleSetResponse _
derive instance repGenericCreateReceiptRuleSetResponse :: Generic CreateReceiptRuleSetResponse _
instance showCreateReceiptRuleSetResponse :: Show CreateReceiptRuleSetResponse where
  show = genericShow
instance decodeCreateReceiptRuleSetResponse :: Decode CreateReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReceiptRuleSetResponse :: Encode CreateReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to create an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype CreateTemplateRequest = CreateTemplateRequest 
  { "Template" :: (Template)
  }
derive instance newtypeCreateTemplateRequest :: Newtype CreateTemplateRequest _
derive instance repGenericCreateTemplateRequest :: Generic CreateTemplateRequest _
instance showCreateTemplateRequest :: Show CreateTemplateRequest where
  show = genericShow
instance decodeCreateTemplateRequest :: Decode CreateTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTemplateRequest :: Encode CreateTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTemplateResponse = CreateTemplateResponse Types.NoArguments
derive instance newtypeCreateTemplateResponse :: Newtype CreateTemplateResponse _
derive instance repGenericCreateTemplateResponse :: Generic CreateTemplateResponse _
instance showCreateTemplateResponse :: Show CreateTemplateResponse where
  show = genericShow
instance decodeCreateTemplateResponse :: Decode CreateTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTemplateResponse :: Encode CreateTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomMailFromStatus = CustomMailFromStatus String
derive instance newtypeCustomMailFromStatus :: Newtype CustomMailFromStatus _
derive instance repGenericCustomMailFromStatus :: Generic CustomMailFromStatus _
instance showCustomMailFromStatus :: Show CustomMailFromStatus where
  show = genericShow
instance decodeCustomMailFromStatus :: Decode CustomMailFromStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMailFromStatus :: Encode CustomMailFromStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomRedirectDomain = CustomRedirectDomain String
derive instance newtypeCustomRedirectDomain :: Newtype CustomRedirectDomain _
derive instance repGenericCustomRedirectDomain :: Generic CustomRedirectDomain _
instance showCustomRedirectDomain :: Show CustomRedirectDomain where
  show = genericShow
instance decodeCustomRedirectDomain :: Decode CustomRedirectDomain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomRedirectDomain :: Encode CustomRedirectDomain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that custom verification email template provided content is invalid.</p>
newtype CustomVerificationEmailInvalidContentException = CustomVerificationEmailInvalidContentException Types.NoArguments
derive instance newtypeCustomVerificationEmailInvalidContentException :: Newtype CustomVerificationEmailInvalidContentException _
derive instance repGenericCustomVerificationEmailInvalidContentException :: Generic CustomVerificationEmailInvalidContentException _
instance showCustomVerificationEmailInvalidContentException :: Show CustomVerificationEmailInvalidContentException where
  show = genericShow
instance decodeCustomVerificationEmailInvalidContentException :: Decode CustomVerificationEmailInvalidContentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomVerificationEmailInvalidContentException :: Encode CustomVerificationEmailInvalidContentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a custom verification email template.</p>
newtype CustomVerificationEmailTemplate = CustomVerificationEmailTemplate 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined.NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined.NullOrUndefined (Subject)
  , "SuccessRedirectionURL" :: NullOrUndefined.NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined.NullOrUndefined (FailureRedirectionURL)
  }
derive instance newtypeCustomVerificationEmailTemplate :: Newtype CustomVerificationEmailTemplate _
derive instance repGenericCustomVerificationEmailTemplate :: Generic CustomVerificationEmailTemplate _
instance showCustomVerificationEmailTemplate :: Show CustomVerificationEmailTemplate where
  show = genericShow
instance decodeCustomVerificationEmailTemplate :: Decode CustomVerificationEmailTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomVerificationEmailTemplate :: Encode CustomVerificationEmailTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a custom verification email template with the name you specified already exists.</p>
newtype CustomVerificationEmailTemplateAlreadyExistsException = CustomVerificationEmailTemplateAlreadyExistsException 
  { "CustomVerificationEmailTemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeCustomVerificationEmailTemplateAlreadyExistsException :: Newtype CustomVerificationEmailTemplateAlreadyExistsException _
derive instance repGenericCustomVerificationEmailTemplateAlreadyExistsException :: Generic CustomVerificationEmailTemplateAlreadyExistsException _
instance showCustomVerificationEmailTemplateAlreadyExistsException :: Show CustomVerificationEmailTemplateAlreadyExistsException where
  show = genericShow
instance decodeCustomVerificationEmailTemplateAlreadyExistsException :: Decode CustomVerificationEmailTemplateAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomVerificationEmailTemplateAlreadyExistsException :: Encode CustomVerificationEmailTemplateAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a custom verification email template with the name you specified does not exist.</p>
newtype CustomVerificationEmailTemplateDoesNotExistException = CustomVerificationEmailTemplateDoesNotExistException 
  { "CustomVerificationEmailTemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeCustomVerificationEmailTemplateDoesNotExistException :: Newtype CustomVerificationEmailTemplateDoesNotExistException _
derive instance repGenericCustomVerificationEmailTemplateDoesNotExistException :: Generic CustomVerificationEmailTemplateDoesNotExistException _
instance showCustomVerificationEmailTemplateDoesNotExistException :: Show CustomVerificationEmailTemplateDoesNotExistException where
  show = genericShow
instance decodeCustomVerificationEmailTemplateDoesNotExistException :: Decode CustomVerificationEmailTemplateDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomVerificationEmailTemplateDoesNotExistException :: Encode CustomVerificationEmailTemplateDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomVerificationEmailTemplates = CustomVerificationEmailTemplates (Array CustomVerificationEmailTemplate)
derive instance newtypeCustomVerificationEmailTemplates :: Newtype CustomVerificationEmailTemplates _
derive instance repGenericCustomVerificationEmailTemplates :: Generic CustomVerificationEmailTemplates _
instance showCustomVerificationEmailTemplates :: Show CustomVerificationEmailTemplates where
  show = genericShow
instance decodeCustomVerificationEmailTemplates :: Decode CustomVerificationEmailTemplates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomVerificationEmailTemplates :: Encode CustomVerificationEmailTemplates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultDimensionValue = DefaultDimensionValue String
derive instance newtypeDefaultDimensionValue :: Newtype DefaultDimensionValue _
derive instance repGenericDefaultDimensionValue :: Generic DefaultDimensionValue _
instance showDefaultDimensionValue :: Show DefaultDimensionValue where
  show = genericShow
instance decodeDefaultDimensionValue :: Decode DefaultDimensionValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultDimensionValue :: Encode DefaultDimensionValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteConfigurationSetEventDestinationRequest = DeleteConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestinationName" :: (EventDestinationName)
  }
derive instance newtypeDeleteConfigurationSetEventDestinationRequest :: Newtype DeleteConfigurationSetEventDestinationRequest _
derive instance repGenericDeleteConfigurationSetEventDestinationRequest :: Generic DeleteConfigurationSetEventDestinationRequest _
instance showDeleteConfigurationSetEventDestinationRequest :: Show DeleteConfigurationSetEventDestinationRequest where
  show = genericShow
instance decodeDeleteConfigurationSetEventDestinationRequest :: Decode DeleteConfigurationSetEventDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetEventDestinationRequest :: Encode DeleteConfigurationSetEventDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse Types.NoArguments
derive instance newtypeDeleteConfigurationSetEventDestinationResponse :: Newtype DeleteConfigurationSetEventDestinationResponse _
derive instance repGenericDeleteConfigurationSetEventDestinationResponse :: Generic DeleteConfigurationSetEventDestinationResponse _
instance showDeleteConfigurationSetEventDestinationResponse :: Show DeleteConfigurationSetEventDestinationResponse where
  show = genericShow
instance decodeDeleteConfigurationSetEventDestinationResponse :: Decode DeleteConfigurationSetEventDestinationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetEventDestinationResponse :: Encode DeleteConfigurationSetEventDestinationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteConfigurationSetRequest = DeleteConfigurationSetRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  }
derive instance newtypeDeleteConfigurationSetRequest :: Newtype DeleteConfigurationSetRequest _
derive instance repGenericDeleteConfigurationSetRequest :: Generic DeleteConfigurationSetRequest _
instance showDeleteConfigurationSetRequest :: Show DeleteConfigurationSetRequest where
  show = genericShow
instance decodeDeleteConfigurationSetRequest :: Decode DeleteConfigurationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetRequest :: Encode DeleteConfigurationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetResponse = DeleteConfigurationSetResponse Types.NoArguments
derive instance newtypeDeleteConfigurationSetResponse :: Newtype DeleteConfigurationSetResponse _
derive instance repGenericDeleteConfigurationSetResponse :: Generic DeleteConfigurationSetResponse _
instance showDeleteConfigurationSetResponse :: Show DeleteConfigurationSetResponse where
  show = genericShow
instance decodeDeleteConfigurationSetResponse :: Decode DeleteConfigurationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetResponse :: Encode DeleteConfigurationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete open and click tracking options in a configuration set. </p>
newtype DeleteConfigurationSetTrackingOptionsRequest = DeleteConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  }
derive instance newtypeDeleteConfigurationSetTrackingOptionsRequest :: Newtype DeleteConfigurationSetTrackingOptionsRequest _
derive instance repGenericDeleteConfigurationSetTrackingOptionsRequest :: Generic DeleteConfigurationSetTrackingOptionsRequest _
instance showDeleteConfigurationSetTrackingOptionsRequest :: Show DeleteConfigurationSetTrackingOptionsRequest where
  show = genericShow
instance decodeDeleteConfigurationSetTrackingOptionsRequest :: Decode DeleteConfigurationSetTrackingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetTrackingOptionsRequest :: Encode DeleteConfigurationSetTrackingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse Types.NoArguments
derive instance newtypeDeleteConfigurationSetTrackingOptionsResponse :: Newtype DeleteConfigurationSetTrackingOptionsResponse _
derive instance repGenericDeleteConfigurationSetTrackingOptionsResponse :: Generic DeleteConfigurationSetTrackingOptionsResponse _
instance showDeleteConfigurationSetTrackingOptionsResponse :: Show DeleteConfigurationSetTrackingOptionsResponse where
  show = genericShow
instance decodeDeleteConfigurationSetTrackingOptionsResponse :: Decode DeleteConfigurationSetTrackingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationSetTrackingOptionsResponse :: Encode DeleteConfigurationSetTrackingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete an existing custom verification email template.</p>
newtype DeleteCustomVerificationEmailTemplateRequest = DeleteCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }
derive instance newtypeDeleteCustomVerificationEmailTemplateRequest :: Newtype DeleteCustomVerificationEmailTemplateRequest _
derive instance repGenericDeleteCustomVerificationEmailTemplateRequest :: Generic DeleteCustomVerificationEmailTemplateRequest _
instance showDeleteCustomVerificationEmailTemplateRequest :: Show DeleteCustomVerificationEmailTemplateRequest where
  show = genericShow
instance decodeDeleteCustomVerificationEmailTemplateRequest :: Decode DeleteCustomVerificationEmailTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCustomVerificationEmailTemplateRequest :: Encode DeleteCustomVerificationEmailTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteIdentityPolicyRequest = DeleteIdentityPolicyRequest 
  { "Identity" :: (Identity)
  , "PolicyName" :: (PolicyName)
  }
derive instance newtypeDeleteIdentityPolicyRequest :: Newtype DeleteIdentityPolicyRequest _
derive instance repGenericDeleteIdentityPolicyRequest :: Generic DeleteIdentityPolicyRequest _
instance showDeleteIdentityPolicyRequest :: Show DeleteIdentityPolicyRequest where
  show = genericShow
instance decodeDeleteIdentityPolicyRequest :: Decode DeleteIdentityPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityPolicyRequest :: Encode DeleteIdentityPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse Types.NoArguments
derive instance newtypeDeleteIdentityPolicyResponse :: Newtype DeleteIdentityPolicyResponse _
derive instance repGenericDeleteIdentityPolicyResponse :: Generic DeleteIdentityPolicyResponse _
instance showDeleteIdentityPolicyResponse :: Show DeleteIdentityPolicyResponse where
  show = genericShow
instance decodeDeleteIdentityPolicyResponse :: Decode DeleteIdentityPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityPolicyResponse :: Encode DeleteIdentityPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete one of your Amazon SES identities (an email address or domain).</p>
newtype DeleteIdentityRequest = DeleteIdentityRequest 
  { "Identity" :: (Identity)
  }
derive instance newtypeDeleteIdentityRequest :: Newtype DeleteIdentityRequest _
derive instance repGenericDeleteIdentityRequest :: Generic DeleteIdentityRequest _
instance showDeleteIdentityRequest :: Show DeleteIdentityRequest where
  show = genericShow
instance decodeDeleteIdentityRequest :: Decode DeleteIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityRequest :: Encode DeleteIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteIdentityResponse = DeleteIdentityResponse Types.NoArguments
derive instance newtypeDeleteIdentityResponse :: Newtype DeleteIdentityResponse _
derive instance repGenericDeleteIdentityResponse :: Generic DeleteIdentityResponse _
instance showDeleteIdentityResponse :: Show DeleteIdentityResponse where
  show = genericShow
instance decodeDeleteIdentityResponse :: Decode DeleteIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityResponse :: Encode DeleteIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptFilterRequest = DeleteReceiptFilterRequest 
  { "FilterName" :: (ReceiptFilterName)
  }
derive instance newtypeDeleteReceiptFilterRequest :: Newtype DeleteReceiptFilterRequest _
derive instance repGenericDeleteReceiptFilterRequest :: Generic DeleteReceiptFilterRequest _
instance showDeleteReceiptFilterRequest :: Show DeleteReceiptFilterRequest where
  show = genericShow
instance decodeDeleteReceiptFilterRequest :: Decode DeleteReceiptFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptFilterRequest :: Encode DeleteReceiptFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse Types.NoArguments
derive instance newtypeDeleteReceiptFilterResponse :: Newtype DeleteReceiptFilterResponse _
derive instance repGenericDeleteReceiptFilterResponse :: Generic DeleteReceiptFilterResponse _
instance showDeleteReceiptFilterResponse :: Show DeleteReceiptFilterResponse where
  show = genericShow
instance decodeDeleteReceiptFilterResponse :: Decode DeleteReceiptFilterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptFilterResponse :: Encode DeleteReceiptFilterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptRuleRequest = DeleteReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  }
derive instance newtypeDeleteReceiptRuleRequest :: Newtype DeleteReceiptRuleRequest _
derive instance repGenericDeleteReceiptRuleRequest :: Generic DeleteReceiptRuleRequest _
instance showDeleteReceiptRuleRequest :: Show DeleteReceiptRuleRequest where
  show = genericShow
instance decodeDeleteReceiptRuleRequest :: Decode DeleteReceiptRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptRuleRequest :: Encode DeleteReceiptRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptRuleResponse = DeleteReceiptRuleResponse Types.NoArguments
derive instance newtypeDeleteReceiptRuleResponse :: Newtype DeleteReceiptRuleResponse _
derive instance repGenericDeleteReceiptRuleResponse :: Generic DeleteReceiptRuleResponse _
instance showDeleteReceiptRuleResponse :: Show DeleteReceiptRuleResponse where
  show = genericShow
instance decodeDeleteReceiptRuleResponse :: Decode DeleteReceiptRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptRuleResponse :: Encode DeleteReceiptRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete a receipt rule set and all of the receipt rules it contains. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptRuleSetRequest = DeleteReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }
derive instance newtypeDeleteReceiptRuleSetRequest :: Newtype DeleteReceiptRuleSetRequest _
derive instance repGenericDeleteReceiptRuleSetRequest :: Generic DeleteReceiptRuleSetRequest _
instance showDeleteReceiptRuleSetRequest :: Show DeleteReceiptRuleSetRequest where
  show = genericShow
instance decodeDeleteReceiptRuleSetRequest :: Decode DeleteReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptRuleSetRequest :: Encode DeleteReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse Types.NoArguments
derive instance newtypeDeleteReceiptRuleSetResponse :: Newtype DeleteReceiptRuleSetResponse _
derive instance repGenericDeleteReceiptRuleSetResponse :: Generic DeleteReceiptRuleSetResponse _
instance showDeleteReceiptRuleSetResponse :: Show DeleteReceiptRuleSetResponse where
  show = genericShow
instance decodeDeleteReceiptRuleSetResponse :: Decode DeleteReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReceiptRuleSetResponse :: Encode DeleteReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteTemplateRequest = DeleteTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }
derive instance newtypeDeleteTemplateRequest :: Newtype DeleteTemplateRequest _
derive instance repGenericDeleteTemplateRequest :: Generic DeleteTemplateRequest _
instance showDeleteTemplateRequest :: Show DeleteTemplateRequest where
  show = genericShow
instance decodeDeleteTemplateRequest :: Decode DeleteTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTemplateRequest :: Encode DeleteTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTemplateResponse = DeleteTemplateResponse Types.NoArguments
derive instance newtypeDeleteTemplateResponse :: Newtype DeleteTemplateResponse _
derive instance repGenericDeleteTemplateResponse :: Generic DeleteTemplateResponse _
instance showDeleteTemplateResponse :: Show DeleteTemplateResponse where
  show = genericShow
instance decodeDeleteTemplateResponse :: Decode DeleteTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTemplateResponse :: Encode DeleteTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to delete an email address from the list of email addresses you have attempted to verify under your AWS account.</p>
newtype DeleteVerifiedEmailAddressRequest = DeleteVerifiedEmailAddressRequest 
  { "EmailAddress" :: (Address)
  }
derive instance newtypeDeleteVerifiedEmailAddressRequest :: Newtype DeleteVerifiedEmailAddressRequest _
derive instance repGenericDeleteVerifiedEmailAddressRequest :: Generic DeleteVerifiedEmailAddressRequest _
instance showDeleteVerifiedEmailAddressRequest :: Show DeleteVerifiedEmailAddressRequest where
  show = genericShow
instance decodeDeleteVerifiedEmailAddressRequest :: Decode DeleteVerifiedEmailAddressRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVerifiedEmailAddressRequest :: Encode DeleteVerifiedEmailAddressRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the metadata and receipt rules for the receipt rule set that is currently active. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeActiveReceiptRuleSetRequest = DescribeActiveReceiptRuleSetRequest Types.NoArguments
derive instance newtypeDescribeActiveReceiptRuleSetRequest :: Newtype DescribeActiveReceiptRuleSetRequest _
derive instance repGenericDescribeActiveReceiptRuleSetRequest :: Generic DescribeActiveReceiptRuleSetRequest _
instance showDescribeActiveReceiptRuleSetRequest :: Show DescribeActiveReceiptRuleSetRequest where
  show = genericShow
instance decodeDescribeActiveReceiptRuleSetRequest :: Decode DescribeActiveReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActiveReceiptRuleSetRequest :: Encode DescribeActiveReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the metadata and receipt rules for the receipt rule set that is currently active.</p>
newtype DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (ReceiptRuleSetMetadata)
  , "Rules" :: NullOrUndefined.NullOrUndefined (ReceiptRulesList)
  }
derive instance newtypeDescribeActiveReceiptRuleSetResponse :: Newtype DescribeActiveReceiptRuleSetResponse _
derive instance repGenericDescribeActiveReceiptRuleSetResponse :: Generic DescribeActiveReceiptRuleSetResponse _
instance showDescribeActiveReceiptRuleSetResponse :: Show DescribeActiveReceiptRuleSetResponse where
  show = genericShow
instance decodeDescribeActiveReceiptRuleSetResponse :: Decode DescribeActiveReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActiveReceiptRuleSetResponse :: Encode DescribeActiveReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeConfigurationSetRequest = DescribeConfigurationSetRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "ConfigurationSetAttributeNames" :: NullOrUndefined.NullOrUndefined (ConfigurationSetAttributeList)
  }
derive instance newtypeDescribeConfigurationSetRequest :: Newtype DescribeConfigurationSetRequest _
derive instance repGenericDescribeConfigurationSetRequest :: Generic DescribeConfigurationSetRequest _
instance showDescribeConfigurationSetRequest :: Show DescribeConfigurationSetRequest where
  show = genericShow
instance decodeDescribeConfigurationSetRequest :: Decode DescribeConfigurationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationSetRequest :: Encode DescribeConfigurationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeConfigurationSetResponse = DescribeConfigurationSetResponse 
  { "ConfigurationSet" :: NullOrUndefined.NullOrUndefined (ConfigurationSet)
  , "EventDestinations" :: NullOrUndefined.NullOrUndefined (EventDestinations)
  , "TrackingOptions" :: NullOrUndefined.NullOrUndefined (TrackingOptions)
  , "ReputationOptions" :: NullOrUndefined.NullOrUndefined (ReputationOptions)
  }
derive instance newtypeDescribeConfigurationSetResponse :: Newtype DescribeConfigurationSetResponse _
derive instance repGenericDescribeConfigurationSetResponse :: Generic DescribeConfigurationSetResponse _
instance showDescribeConfigurationSetResponse :: Show DescribeConfigurationSetResponse where
  show = genericShow
instance decodeDescribeConfigurationSetResponse :: Decode DescribeConfigurationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationSetResponse :: Encode DescribeConfigurationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeReceiptRuleRequest = DescribeReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  }
derive instance newtypeDescribeReceiptRuleRequest :: Newtype DescribeReceiptRuleRequest _
derive instance repGenericDescribeReceiptRuleRequest :: Generic DescribeReceiptRuleRequest _
instance showDescribeReceiptRuleRequest :: Show DescribeReceiptRuleRequest where
  show = genericShow
instance decodeDescribeReceiptRuleRequest :: Decode DescribeReceiptRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReceiptRuleRequest :: Encode DescribeReceiptRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the details of a receipt rule.</p>
newtype DescribeReceiptRuleResponse = DescribeReceiptRuleResponse 
  { "Rule" :: NullOrUndefined.NullOrUndefined (ReceiptRule)
  }
derive instance newtypeDescribeReceiptRuleResponse :: Newtype DescribeReceiptRuleResponse _
derive instance repGenericDescribeReceiptRuleResponse :: Generic DescribeReceiptRuleResponse _
instance showDescribeReceiptRuleResponse :: Show DescribeReceiptRuleResponse where
  show = genericShow
instance decodeDescribeReceiptRuleResponse :: Decode DescribeReceiptRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReceiptRuleResponse :: Encode DescribeReceiptRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the details of a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeReceiptRuleSetRequest = DescribeReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }
derive instance newtypeDescribeReceiptRuleSetRequest :: Newtype DescribeReceiptRuleSetRequest _
derive instance repGenericDescribeReceiptRuleSetRequest :: Generic DescribeReceiptRuleSetRequest _
instance showDescribeReceiptRuleSetRequest :: Show DescribeReceiptRuleSetRequest where
  show = genericShow
instance decodeDescribeReceiptRuleSetRequest :: Decode DescribeReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReceiptRuleSetRequest :: Encode DescribeReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the details of the specified receipt rule set.</p>
newtype DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (ReceiptRuleSetMetadata)
  , "Rules" :: NullOrUndefined.NullOrUndefined (ReceiptRulesList)
  }
derive instance newtypeDescribeReceiptRuleSetResponse :: Newtype DescribeReceiptRuleSetResponse _
derive instance repGenericDescribeReceiptRuleSetResponse :: Generic DescribeReceiptRuleSetResponse _
instance showDescribeReceiptRuleSetResponse :: Show DescribeReceiptRuleSetResponse where
  show = genericShow
instance decodeDescribeReceiptRuleSetResponse :: Decode DescribeReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeReceiptRuleSetResponse :: Encode DescribeReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the destination of the message, consisting of To:, CC:, and BCC: fields.</p> <note> <p>Amazon SES does not support the SMTPUTF8 extension, as described in <a href="https://tools.ietf.org/html/rfc6531">RFC6531</a>. For this reason, the <i>local part</i> of a destination email address (the part of the email address that precedes the @ sign) may only contain <a href="https://en.wikipedia.org/wiki/Email_address#Local-part">7-bit ASCII characters</a>. If the <i>domain part</i> of an address (the part after the @ sign) contains non-ASCII characters, they must be encoded using Punycode, as described in <a href="https://tools.ietf.org/html/rfc3492.html">RFC3492</a>.</p> </note>
newtype Destination = Destination 
  { "ToAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "CcAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "BccAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  }
derive instance newtypeDestination :: Newtype Destination _
derive instance repGenericDestination :: Generic Destination _
instance showDestination :: Show Destination where
  show = genericShow
instance decodeDestination :: Decode Destination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestination :: Encode Destination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiagnosticCode = DiagnosticCode String
derive instance newtypeDiagnosticCode :: Newtype DiagnosticCode _
derive instance repGenericDiagnosticCode :: Generic DiagnosticCode _
instance showDiagnosticCode :: Show DiagnosticCode where
  show = genericShow
instance decodeDiagnosticCode :: Decode DiagnosticCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiagnosticCode :: Encode DiagnosticCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionName = DimensionName String
derive instance newtypeDimensionName :: Newtype DimensionName _
derive instance repGenericDimensionName :: Generic DimensionName _
instance showDimensionName :: Show DimensionName where
  show = genericShow
instance decodeDimensionName :: Decode DimensionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionName :: Encode DimensionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionValueSource = DimensionValueSource String
derive instance newtypeDimensionValueSource :: Newtype DimensionValueSource _
derive instance repGenericDimensionValueSource :: Generic DimensionValueSource _
instance showDimensionValueSource :: Show DimensionValueSource where
  show = genericShow
instance decodeDimensionValueSource :: Decode DimensionValueSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValueSource :: Encode DimensionValueSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DkimAttributes = DkimAttributes (StrMap.StrMap IdentityDkimAttributes)
derive instance newtypeDkimAttributes :: Newtype DkimAttributes _
derive instance repGenericDkimAttributes :: Generic DkimAttributes _
instance showDkimAttributes :: Show DkimAttributes where
  show = genericShow
instance decodeDkimAttributes :: Decode DkimAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDkimAttributes :: Encode DkimAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Domain = Domain String
derive instance newtypeDomain :: Newtype Domain _
derive instance repGenericDomain :: Generic Domain _
instance showDomain :: Show Domain where
  show = genericShow
instance decodeDomain :: Decode Domain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomain :: Encode Domain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DsnAction = DsnAction String
derive instance newtypeDsnAction :: Newtype DsnAction _
derive instance repGenericDsnAction :: Generic DsnAction _
instance showDsnAction :: Show DsnAction where
  show = genericShow
instance decodeDsnAction :: Decode DsnAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDsnAction :: Encode DsnAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DsnStatus = DsnStatus String
derive instance newtypeDsnStatus :: Newtype DsnStatus _
derive instance repGenericDsnStatus :: Generic DsnStatus _
instance showDsnStatus :: Show DsnStatus where
  show = genericShow
instance decodeDsnStatus :: Decode DsnStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDsnStatus :: Encode DsnStatus where
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


newtype Error = Error String
derive instance newtypeError :: Newtype Error _
derive instance repGenericError :: Generic Error _
instance showError :: Show Error where
  show = genericShow
instance decodeError :: Decode Error where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeError :: Encode Error where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the event destination that the specified email sending events will be published to.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype EventDestination = EventDestination 
  { "Name" :: (EventDestinationName)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "MatchingEventTypes" :: (EventTypes)
  , "KinesisFirehoseDestination" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseDestination)
  , "CloudWatchDestination" :: NullOrUndefined.NullOrUndefined (CloudWatchDestination)
  , "SNSDestination" :: NullOrUndefined.NullOrUndefined (SNSDestination)
  }
derive instance newtypeEventDestination :: Newtype EventDestination _
derive instance repGenericEventDestination :: Generic EventDestination _
instance showEventDestination :: Show EventDestination where
  show = genericShow
instance decodeEventDestination :: Decode EventDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDestination :: Encode EventDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the event destination could not be created because of a naming conflict.</p>
newtype EventDestinationAlreadyExistsException = EventDestinationAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined.NullOrUndefined (EventDestinationName)
  }
derive instance newtypeEventDestinationAlreadyExistsException :: Newtype EventDestinationAlreadyExistsException _
derive instance repGenericEventDestinationAlreadyExistsException :: Generic EventDestinationAlreadyExistsException _
instance showEventDestinationAlreadyExistsException :: Show EventDestinationAlreadyExistsException where
  show = genericShow
instance decodeEventDestinationAlreadyExistsException :: Decode EventDestinationAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDestinationAlreadyExistsException :: Encode EventDestinationAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the event destination does not exist.</p>
newtype EventDestinationDoesNotExistException = EventDestinationDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined.NullOrUndefined (EventDestinationName)
  }
derive instance newtypeEventDestinationDoesNotExistException :: Newtype EventDestinationDoesNotExistException _
derive instance repGenericEventDestinationDoesNotExistException :: Generic EventDestinationDoesNotExistException _
instance showEventDestinationDoesNotExistException :: Show EventDestinationDoesNotExistException where
  show = genericShow
instance decodeEventDestinationDoesNotExistException :: Decode EventDestinationDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDestinationDoesNotExistException :: Encode EventDestinationDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDestinationName = EventDestinationName String
derive instance newtypeEventDestinationName :: Newtype EventDestinationName _
derive instance repGenericEventDestinationName :: Generic EventDestinationName _
instance showEventDestinationName :: Show EventDestinationName where
  show = genericShow
instance decodeEventDestinationName :: Decode EventDestinationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDestinationName :: Encode EventDestinationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDestinations = EventDestinations (Array EventDestination)
derive instance newtypeEventDestinations :: Newtype EventDestinations _
derive instance repGenericEventDestinations :: Generic EventDestinations _
instance showEventDestinations :: Show EventDestinations where
  show = genericShow
instance decodeEventDestinations :: Decode EventDestinations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDestinations :: Encode EventDestinations where
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


newtype EventTypes = EventTypes (Array EventType)
derive instance newtypeEventTypes :: Newtype EventTypes _
derive instance repGenericEventTypes :: Generic EventTypes _
instance showEventTypes :: Show EventTypes where
  show = genericShow
instance decodeEventTypes :: Decode EventTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypes :: Encode EventTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Explanation = Explanation String
derive instance newtypeExplanation :: Newtype Explanation _
derive instance repGenericExplanation :: Generic Explanation _
instance showExplanation :: Show Explanation where
  show = genericShow
instance decodeExplanation :: Decode Explanation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExplanation :: Encode Explanation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype ExtensionField = ExtensionField 
  { "Name" :: (ExtensionFieldName)
  , "Value" :: (ExtensionFieldValue)
  }
derive instance newtypeExtensionField :: Newtype ExtensionField _
derive instance repGenericExtensionField :: Generic ExtensionField _
instance showExtensionField :: Show ExtensionField where
  show = genericShow
instance decodeExtensionField :: Decode ExtensionField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtensionField :: Encode ExtensionField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtensionFieldList = ExtensionFieldList (Array ExtensionField)
derive instance newtypeExtensionFieldList :: Newtype ExtensionFieldList _
derive instance repGenericExtensionFieldList :: Generic ExtensionFieldList _
instance showExtensionFieldList :: Show ExtensionFieldList where
  show = genericShow
instance decodeExtensionFieldList :: Decode ExtensionFieldList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtensionFieldList :: Encode ExtensionFieldList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtensionFieldName = ExtensionFieldName String
derive instance newtypeExtensionFieldName :: Newtype ExtensionFieldName _
derive instance repGenericExtensionFieldName :: Generic ExtensionFieldName _
instance showExtensionFieldName :: Show ExtensionFieldName where
  show = genericShow
instance decodeExtensionFieldName :: Decode ExtensionFieldName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtensionFieldName :: Encode ExtensionFieldName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtensionFieldValue = ExtensionFieldValue String
derive instance newtypeExtensionFieldValue :: Newtype ExtensionFieldValue _
derive instance repGenericExtensionFieldValue :: Generic ExtensionFieldValue _
instance showExtensionFieldValue :: Show ExtensionFieldValue where
  show = genericShow
instance decodeExtensionFieldValue :: Decode ExtensionFieldValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtensionFieldValue :: Encode ExtensionFieldValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureRedirectionURL = FailureRedirectionURL String
derive instance newtypeFailureRedirectionURL :: Newtype FailureRedirectionURL _
derive instance repGenericFailureRedirectionURL :: Generic FailureRedirectionURL _
instance showFailureRedirectionURL :: Show FailureRedirectionURL where
  show = genericShow
instance decodeFailureRedirectionURL :: Decode FailureRedirectionURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureRedirectionURL :: Encode FailureRedirectionURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FromAddress = FromAddress String
derive instance newtypeFromAddress :: Newtype FromAddress _
derive instance repGenericFromAddress :: Generic FromAddress _
instance showFromAddress :: Show FromAddress where
  show = genericShow
instance decodeFromAddress :: Decode FromAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFromAddress :: Encode FromAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the sender address specified for a custom verification email is not verified, and is therefore not eligible to send the custom verification email. </p>
newtype FromEmailAddressNotVerifiedException = FromEmailAddressNotVerifiedException 
  { "FromEmailAddress" :: NullOrUndefined.NullOrUndefined (FromAddress)
  }
derive instance newtypeFromEmailAddressNotVerifiedException :: Newtype FromEmailAddressNotVerifiedException _
derive instance repGenericFromEmailAddressNotVerifiedException :: Generic FromEmailAddressNotVerifiedException _
instance showFromEmailAddressNotVerifiedException :: Show FromEmailAddressNotVerifiedException where
  show = genericShow
instance decodeFromEmailAddressNotVerifiedException :: Decode FromEmailAddressNotVerifiedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFromEmailAddressNotVerifiedException :: Encode FromEmailAddressNotVerifiedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the email sending status for your Amazon SES account.</p>
newtype GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  }
derive instance newtypeGetAccountSendingEnabledResponse :: Newtype GetAccountSendingEnabledResponse _
derive instance repGenericGetAccountSendingEnabledResponse :: Generic GetAccountSendingEnabledResponse _
instance showGetAccountSendingEnabledResponse :: Show GetAccountSendingEnabledResponse where
  show = genericShow
instance decodeGetAccountSendingEnabledResponse :: Decode GetAccountSendingEnabledResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountSendingEnabledResponse :: Encode GetAccountSendingEnabledResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to retrieve an existing custom verification email template.</p>
newtype GetCustomVerificationEmailTemplateRequest = GetCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }
derive instance newtypeGetCustomVerificationEmailTemplateRequest :: Newtype GetCustomVerificationEmailTemplateRequest _
derive instance repGenericGetCustomVerificationEmailTemplateRequest :: Generic GetCustomVerificationEmailTemplateRequest _
instance showGetCustomVerificationEmailTemplateRequest :: Show GetCustomVerificationEmailTemplateRequest where
  show = genericShow
instance decodeGetCustomVerificationEmailTemplateRequest :: Decode GetCustomVerificationEmailTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCustomVerificationEmailTemplateRequest :: Encode GetCustomVerificationEmailTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The content of the custom verification email template.</p>
newtype GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined.NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined.NullOrUndefined (Subject)
  , "TemplateContent" :: NullOrUndefined.NullOrUndefined (TemplateContent)
  , "SuccessRedirectionURL" :: NullOrUndefined.NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined.NullOrUndefined (FailureRedirectionURL)
  }
derive instance newtypeGetCustomVerificationEmailTemplateResponse :: Newtype GetCustomVerificationEmailTemplateResponse _
derive instance repGenericGetCustomVerificationEmailTemplateResponse :: Generic GetCustomVerificationEmailTemplateResponse _
instance showGetCustomVerificationEmailTemplateResponse :: Show GetCustomVerificationEmailTemplateResponse where
  show = genericShow
instance decodeGetCustomVerificationEmailTemplateResponse :: Decode GetCustomVerificationEmailTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCustomVerificationEmailTemplateResponse :: Encode GetCustomVerificationEmailTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request for the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this request also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published. For more information about Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityDkimAttributesRequest = GetIdentityDkimAttributesRequest 
  { "Identities" :: (IdentityList)
  }
derive instance newtypeGetIdentityDkimAttributesRequest :: Newtype GetIdentityDkimAttributesRequest _
derive instance repGenericGetIdentityDkimAttributesRequest :: Generic GetIdentityDkimAttributesRequest _
instance showGetIdentityDkimAttributesRequest :: Show GetIdentityDkimAttributesRequest where
  show = genericShow
instance decodeGetIdentityDkimAttributesRequest :: Decode GetIdentityDkimAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityDkimAttributesRequest :: Encode GetIdentityDkimAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this response also contains the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published.</p>
newtype GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse 
  { "DkimAttributes" :: (DkimAttributes)
  }
derive instance newtypeGetIdentityDkimAttributesResponse :: Newtype GetIdentityDkimAttributesResponse _
derive instance repGenericGetIdentityDkimAttributesResponse :: Generic GetIdentityDkimAttributesResponse _
instance showGetIdentityDkimAttributesResponse :: Show GetIdentityDkimAttributesResponse where
  show = genericShow
instance decodeGetIdentityDkimAttributesResponse :: Decode GetIdentityDkimAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityDkimAttributesResponse :: Encode GetIdentityDkimAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the Amazon SES custom MAIL FROM attributes for a list of identities. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityMailFromDomainAttributesRequest = GetIdentityMailFromDomainAttributesRequest 
  { "Identities" :: (IdentityList)
  }
derive instance newtypeGetIdentityMailFromDomainAttributesRequest :: Newtype GetIdentityMailFromDomainAttributesRequest _
derive instance repGenericGetIdentityMailFromDomainAttributesRequest :: Generic GetIdentityMailFromDomainAttributesRequest _
instance showGetIdentityMailFromDomainAttributesRequest :: Show GetIdentityMailFromDomainAttributesRequest where
  show = genericShow
instance decodeGetIdentityMailFromDomainAttributesRequest :: Decode GetIdentityMailFromDomainAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityMailFromDomainAttributesRequest :: Encode GetIdentityMailFromDomainAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the custom MAIL FROM attributes for a list of identities.</p>
newtype GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse 
  { "MailFromDomainAttributes" :: (MailFromDomainAttributes)
  }
derive instance newtypeGetIdentityMailFromDomainAttributesResponse :: Newtype GetIdentityMailFromDomainAttributesResponse _
derive instance repGenericGetIdentityMailFromDomainAttributesResponse :: Generic GetIdentityMailFromDomainAttributesResponse _
instance showGetIdentityMailFromDomainAttributesResponse :: Show GetIdentityMailFromDomainAttributesResponse where
  show = genericShow
instance decodeGetIdentityMailFromDomainAttributesResponse :: Decode GetIdentityMailFromDomainAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityMailFromDomainAttributesResponse :: Encode GetIdentityMailFromDomainAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the notification attributes for a list of identities you verified with Amazon SES. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityNotificationAttributesRequest = GetIdentityNotificationAttributesRequest 
  { "Identities" :: (IdentityList)
  }
derive instance newtypeGetIdentityNotificationAttributesRequest :: Newtype GetIdentityNotificationAttributesRequest _
derive instance repGenericGetIdentityNotificationAttributesRequest :: Generic GetIdentityNotificationAttributesRequest _
instance showGetIdentityNotificationAttributesRequest :: Show GetIdentityNotificationAttributesRequest where
  show = genericShow
instance decodeGetIdentityNotificationAttributesRequest :: Decode GetIdentityNotificationAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityNotificationAttributesRequest :: Encode GetIdentityNotificationAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the notification attributes for a list of identities.</p>
newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse 
  { "NotificationAttributes" :: (NotificationAttributes)
  }
derive instance newtypeGetIdentityNotificationAttributesResponse :: Newtype GetIdentityNotificationAttributesResponse _
derive instance repGenericGetIdentityNotificationAttributesResponse :: Generic GetIdentityNotificationAttributesResponse _
instance showGetIdentityNotificationAttributesResponse :: Show GetIdentityNotificationAttributesResponse where
  show = genericShow
instance decodeGetIdentityNotificationAttributesResponse :: Decode GetIdentityNotificationAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityNotificationAttributesResponse :: Encode GetIdentityNotificationAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityPoliciesRequest = GetIdentityPoliciesRequest 
  { "Identity" :: (Identity)
  , "PolicyNames" :: (PolicyNameList)
  }
derive instance newtypeGetIdentityPoliciesRequest :: Newtype GetIdentityPoliciesRequest _
derive instance repGenericGetIdentityPoliciesRequest :: Generic GetIdentityPoliciesRequest _
instance showGetIdentityPoliciesRequest :: Show GetIdentityPoliciesRequest where
  show = genericShow
instance decodeGetIdentityPoliciesRequest :: Decode GetIdentityPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoliciesRequest :: Encode GetIdentityPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the requested sending authorization policies.</p>
newtype GetIdentityPoliciesResponse = GetIdentityPoliciesResponse 
  { "Policies" :: (PolicyMap)
  }
derive instance newtypeGetIdentityPoliciesResponse :: Newtype GetIdentityPoliciesResponse _
derive instance repGenericGetIdentityPoliciesResponse :: Generic GetIdentityPoliciesResponse _
instance showGetIdentityPoliciesResponse :: Show GetIdentityPoliciesResponse where
  show = genericShow
instance decodeGetIdentityPoliciesResponse :: Decode GetIdentityPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoliciesResponse :: Encode GetIdentityPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return the Amazon SES verification status of a list of identities. For domain identities, this request also returns the verification token. For information about verifying identities with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityVerificationAttributesRequest = GetIdentityVerificationAttributesRequest 
  { "Identities" :: (IdentityList)
  }
derive instance newtypeGetIdentityVerificationAttributesRequest :: Newtype GetIdentityVerificationAttributesRequest _
derive instance repGenericGetIdentityVerificationAttributesRequest :: Generic GetIdentityVerificationAttributesRequest _
instance showGetIdentityVerificationAttributesRequest :: Show GetIdentityVerificationAttributesRequest where
  show = genericShow
instance decodeGetIdentityVerificationAttributesRequest :: Decode GetIdentityVerificationAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityVerificationAttributesRequest :: Encode GetIdentityVerificationAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon SES verification status of a list of identities. For domain identities, this response also contains the verification token.</p>
newtype GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse 
  { "VerificationAttributes" :: (VerificationAttributes)
  }
derive instance newtypeGetIdentityVerificationAttributesResponse :: Newtype GetIdentityVerificationAttributesResponse _
derive instance repGenericGetIdentityVerificationAttributesResponse :: Generic GetIdentityVerificationAttributesResponse _
instance showGetIdentityVerificationAttributesResponse :: Show GetIdentityVerificationAttributesResponse where
  show = genericShow
instance decodeGetIdentityVerificationAttributesResponse :: Decode GetIdentityVerificationAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityVerificationAttributesResponse :: Encode GetIdentityVerificationAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents your Amazon SES daily sending quota, maximum send rate, and the number of emails you have sent in the last 24 hours.</p>
newtype GetSendQuotaResponse = GetSendQuotaResponse 
  { "Max24HourSend" :: NullOrUndefined.NullOrUndefined (Max24HourSend)
  , "MaxSendRate" :: NullOrUndefined.NullOrUndefined (MaxSendRate)
  , "SentLast24Hours" :: NullOrUndefined.NullOrUndefined (SentLast24Hours)
  }
derive instance newtypeGetSendQuotaResponse :: Newtype GetSendQuotaResponse _
derive instance repGenericGetSendQuotaResponse :: Generic GetSendQuotaResponse _
instance showGetSendQuotaResponse :: Show GetSendQuotaResponse where
  show = genericShow
instance decodeGetSendQuotaResponse :: Decode GetSendQuotaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSendQuotaResponse :: Encode GetSendQuotaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.</p>
newtype GetSendStatisticsResponse = GetSendStatisticsResponse 
  { "SendDataPoints" :: NullOrUndefined.NullOrUndefined (SendDataPointList)
  }
derive instance newtypeGetSendStatisticsResponse :: Newtype GetSendStatisticsResponse _
derive instance repGenericGetSendStatisticsResponse :: Generic GetSendStatisticsResponse _
instance showGetSendStatisticsResponse :: Show GetSendStatisticsResponse where
  show = genericShow
instance decodeGetSendStatisticsResponse :: Decode GetSendStatisticsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSendStatisticsResponse :: Encode GetSendStatisticsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTemplateRequest = GetTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }
derive instance newtypeGetTemplateRequest :: Newtype GetTemplateRequest _
derive instance repGenericGetTemplateRequest :: Generic GetTemplateRequest _
instance showGetTemplateRequest :: Show GetTemplateRequest where
  show = genericShow
instance decodeGetTemplateRequest :: Decode GetTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateRequest :: Encode GetTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTemplateResponse = GetTemplateResponse 
  { "Template" :: NullOrUndefined.NullOrUndefined (Template)
  }
derive instance newtypeGetTemplateResponse :: Newtype GetTemplateResponse _
derive instance repGenericGetTemplateResponse :: Generic GetTemplateResponse _
instance showGetTemplateResponse :: Show GetTemplateResponse where
  show = genericShow
instance decodeGetTemplateResponse :: Decode GetTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateResponse :: Encode GetTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeaderName = HeaderName String
derive instance newtypeHeaderName :: Newtype HeaderName _
derive instance repGenericHeaderName :: Generic HeaderName _
instance showHeaderName :: Show HeaderName where
  show = genericShow
instance decodeHeaderName :: Decode HeaderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaderName :: Encode HeaderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeaderValue = HeaderValue String
derive instance newtypeHeaderValue :: Newtype HeaderValue _
derive instance repGenericHeaderValue :: Generic HeaderValue _
instance showHeaderValue :: Show HeaderValue where
  show = genericShow
instance decodeHeaderValue :: Decode HeaderValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaderValue :: Encode HeaderValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HtmlPart = HtmlPart String
derive instance newtypeHtmlPart :: Newtype HtmlPart _
derive instance repGenericHtmlPart :: Generic HtmlPart _
instance showHtmlPart :: Show HtmlPart where
  show = genericShow
instance decodeHtmlPart :: Decode HtmlPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHtmlPart :: Encode HtmlPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Identity = Identity String
derive instance newtypeIdentity :: Newtype Identity _
derive instance repGenericIdentity :: Generic Identity _
instance showIdentity :: Show Identity where
  show = genericShow
instance decodeIdentity :: Decode Identity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentity :: Encode Identity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the DKIM attributes of a verified email address or a domain.</p>
newtype IdentityDkimAttributes = IdentityDkimAttributes 
  { "DkimEnabled" :: (Enabled)
  , "DkimVerificationStatus" :: (VerificationStatus)
  , "DkimTokens" :: NullOrUndefined.NullOrUndefined (VerificationTokenList)
  }
derive instance newtypeIdentityDkimAttributes :: Newtype IdentityDkimAttributes _
derive instance repGenericIdentityDkimAttributes :: Generic IdentityDkimAttributes _
instance showIdentityDkimAttributes :: Show IdentityDkimAttributes where
  show = genericShow
instance decodeIdentityDkimAttributes :: Decode IdentityDkimAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityDkimAttributes :: Encode IdentityDkimAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityList = IdentityList (Array Identity)
derive instance newtypeIdentityList :: Newtype IdentityList _
derive instance repGenericIdentityList :: Generic IdentityList _
instance showIdentityList :: Show IdentityList where
  show = genericShow
instance decodeIdentityList :: Decode IdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityList :: Encode IdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).</p>
newtype IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes 
  { "MailFromDomain" :: (MailFromDomainName)
  , "MailFromDomainStatus" :: (CustomMailFromStatus)
  , "BehaviorOnMXFailure" :: (BehaviorOnMXFailure)
  }
derive instance newtypeIdentityMailFromDomainAttributes :: Newtype IdentityMailFromDomainAttributes _
derive instance repGenericIdentityMailFromDomainAttributes :: Generic IdentityMailFromDomainAttributes _
instance showIdentityMailFromDomainAttributes :: Show IdentityMailFromDomainAttributes where
  show = genericShow
instance decodeIdentityMailFromDomainAttributes :: Decode IdentityMailFromDomainAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityMailFromDomainAttributes :: Encode IdentityMailFromDomainAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.</p>
newtype IdentityNotificationAttributes = IdentityNotificationAttributes 
  { "BounceTopic" :: (NotificationTopic)
  , "ComplaintTopic" :: (NotificationTopic)
  , "DeliveryTopic" :: (NotificationTopic)
  , "ForwardingEnabled" :: (Enabled)
  , "HeadersInBounceNotificationsEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "HeadersInComplaintNotificationsEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "HeadersInDeliveryNotificationsEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  }
derive instance newtypeIdentityNotificationAttributes :: Newtype IdentityNotificationAttributes _
derive instance repGenericIdentityNotificationAttributes :: Generic IdentityNotificationAttributes _
instance showIdentityNotificationAttributes :: Show IdentityNotificationAttributes where
  show = genericShow
instance decodeIdentityNotificationAttributes :: Decode IdentityNotificationAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityNotificationAttributes :: Encode IdentityNotificationAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityType = IdentityType String
derive instance newtypeIdentityType :: Newtype IdentityType _
derive instance repGenericIdentityType :: Generic IdentityType _
instance showIdentityType :: Show IdentityType where
  show = genericShow
instance decodeIdentityType :: Decode IdentityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityType :: Encode IdentityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the verification attributes of a single identity.</p>
newtype IdentityVerificationAttributes = IdentityVerificationAttributes 
  { "VerificationStatus" :: (VerificationStatus)
  , "VerificationToken" :: NullOrUndefined.NullOrUndefined (VerificationToken)
  }
derive instance newtypeIdentityVerificationAttributes :: Newtype IdentityVerificationAttributes _
derive instance repGenericIdentityVerificationAttributes :: Generic IdentityVerificationAttributes _
instance showIdentityVerificationAttributes :: Show IdentityVerificationAttributes where
  show = genericShow
instance decodeIdentityVerificationAttributes :: Decode IdentityVerificationAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityVerificationAttributes :: Encode IdentityVerificationAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the Amazon CloudWatch destination is invalid. See the error message for details.</p>
newtype InvalidCloudWatchDestinationException = InvalidCloudWatchDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined.NullOrUndefined (EventDestinationName)
  }
derive instance newtypeInvalidCloudWatchDestinationException :: Newtype InvalidCloudWatchDestinationException _
derive instance repGenericInvalidCloudWatchDestinationException :: Generic InvalidCloudWatchDestinationException _
instance showInvalidCloudWatchDestinationException :: Show InvalidCloudWatchDestinationException where
  show = genericShow
instance decodeInvalidCloudWatchDestinationException :: Decode InvalidCloudWatchDestinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCloudWatchDestinationException :: Encode InvalidCloudWatchDestinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the configuration set is invalid. See the error message for details.</p>
newtype InvalidConfigurationSetException = InvalidConfigurationSetException Types.NoArguments
derive instance newtypeInvalidConfigurationSetException :: Newtype InvalidConfigurationSetException _
derive instance repGenericInvalidConfigurationSetException :: Generic InvalidConfigurationSetException _
instance showInvalidConfigurationSetException :: Show InvalidConfigurationSetException where
  show = genericShow
instance decodeInvalidConfigurationSetException :: Decode InvalidConfigurationSetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationSetException :: Encode InvalidConfigurationSetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the Amazon Kinesis Firehose destination is invalid. See the error message for details.</p>
newtype InvalidFirehoseDestinationException = InvalidFirehoseDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined.NullOrUndefined (EventDestinationName)
  }
derive instance newtypeInvalidFirehoseDestinationException :: Newtype InvalidFirehoseDestinationException _
derive instance repGenericInvalidFirehoseDestinationException :: Generic InvalidFirehoseDestinationException _
instance showInvalidFirehoseDestinationException :: Show InvalidFirehoseDestinationException where
  show = genericShow
instance decodeInvalidFirehoseDestinationException :: Decode InvalidFirehoseDestinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFirehoseDestinationException :: Encode InvalidFirehoseDestinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided AWS Lambda function is invalid, or that Amazon SES could not execute the provided function, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidLambdaFunctionException = InvalidLambdaFunctionException 
  { "FunctionArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  }
derive instance newtypeInvalidLambdaFunctionException :: Newtype InvalidLambdaFunctionException _
derive instance repGenericInvalidLambdaFunctionException :: Generic InvalidLambdaFunctionException _
instance showInvalidLambdaFunctionException :: Show InvalidLambdaFunctionException where
  show = genericShow
instance decodeInvalidLambdaFunctionException :: Decode InvalidLambdaFunctionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLambdaFunctionException :: Encode InvalidLambdaFunctionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided policy is invalid. Check the error stack for more information about what caused the error.</p>
newtype InvalidPolicyException = InvalidPolicyException Types.NoArguments
derive instance newtypeInvalidPolicyException :: Newtype InvalidPolicyException _
derive instance repGenericInvalidPolicyException :: Generic InvalidPolicyException _
instance showInvalidPolicyException :: Show InvalidPolicyException where
  show = genericShow
instance decodeInvalidPolicyException :: Decode InvalidPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPolicyException :: Encode InvalidPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that one or more of the replacement values you provided is invalid. This error may occur when the TemplateData object contains invalid JSON.</p>
newtype InvalidRenderingParameterException = InvalidRenderingParameterException 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeInvalidRenderingParameterException :: Newtype InvalidRenderingParameterException _
derive instance repGenericInvalidRenderingParameterException :: Generic InvalidRenderingParameterException _
instance showInvalidRenderingParameterException :: Show InvalidRenderingParameterException where
  show = genericShow
instance decodeInvalidRenderingParameterException :: Decode InvalidRenderingParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRenderingParameterException :: Encode InvalidRenderingParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided Amazon S3 bucket or AWS KMS encryption key is invalid, or that Amazon SES could not publish to the bucket, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidS3ConfigurationException = InvalidS3ConfigurationException 
  { "Bucket" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  }
derive instance newtypeInvalidS3ConfigurationException :: Newtype InvalidS3ConfigurationException _
derive instance repGenericInvalidS3ConfigurationException :: Generic InvalidS3ConfigurationException _
instance showInvalidS3ConfigurationException :: Show InvalidS3ConfigurationException where
  show = genericShow
instance decodeInvalidS3ConfigurationException :: Decode InvalidS3ConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3ConfigurationException :: Encode InvalidS3ConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the Amazon Simple Notification Service (Amazon SNS) destination is invalid. See the error message for details.</p>
newtype InvalidSNSDestinationException = InvalidSNSDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined.NullOrUndefined (EventDestinationName)
  }
derive instance newtypeInvalidSNSDestinationException :: Newtype InvalidSNSDestinationException _
derive instance repGenericInvalidSNSDestinationException :: Generic InvalidSNSDestinationException _
instance showInvalidSNSDestinationException :: Show InvalidSNSDestinationException where
  show = genericShow
instance decodeInvalidSNSDestinationException :: Decode InvalidSNSDestinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSNSDestinationException :: Encode InvalidSNSDestinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided Amazon SNS topic is invalid, or that Amazon SES could not publish to the topic, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidSnsTopicException = InvalidSnsTopicException 
  { "Topic" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  }
derive instance newtypeInvalidSnsTopicException :: Newtype InvalidSnsTopicException _
derive instance repGenericInvalidSnsTopicException :: Generic InvalidSnsTopicException _
instance showInvalidSnsTopicException :: Show InvalidSnsTopicException where
  show = genericShow
instance decodeInvalidSnsTopicException :: Decode InvalidSnsTopicException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSnsTopicException :: Encode InvalidSnsTopicException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a template could not be created because it contained invalid JSON.</p>
newtype InvalidTemplateException = InvalidTemplateException 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeInvalidTemplateException :: Newtype InvalidTemplateException _
derive instance repGenericInvalidTemplateException :: Generic InvalidTemplateException _
instance showInvalidTemplateException :: Show InvalidTemplateException where
  show = genericShow
instance decodeInvalidTemplateException :: Decode InvalidTemplateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTemplateException :: Encode InvalidTemplateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the custom domain to be used for open and click tracking redirects is invalid. This error appears most often in the following situations:</p> <ul> <li> <p>When the tracking domain you specified is not verified in Amazon SES.</p> </li> <li> <p>When the tracking domain you specified is not a valid domain or subdomain.</p> </li> </ul>
newtype InvalidTrackingOptionsException = InvalidTrackingOptionsException Types.NoArguments
derive instance newtypeInvalidTrackingOptionsException :: Newtype InvalidTrackingOptionsException _
derive instance repGenericInvalidTrackingOptionsException :: Generic InvalidTrackingOptionsException _
instance showInvalidTrackingOptionsException :: Show InvalidTrackingOptionsException where
  show = genericShow
instance decodeInvalidTrackingOptionsException :: Decode InvalidTrackingOptionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTrackingOptionsException :: Encode InvalidTrackingOptionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvocationType = InvocationType String
derive instance newtypeInvocationType :: Newtype InvocationType _
derive instance repGenericInvocationType :: Generic InvocationType _
instance showInvocationType :: Show InvocationType where
  show = genericShow
instance decodeInvocationType :: Decode InvocationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvocationType :: Encode InvocationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.</p> <p>Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype KinesisFirehoseDestination = KinesisFirehoseDestination 
  { "IAMRoleARN" :: (AmazonResourceName)
  , "DeliveryStreamARN" :: (AmazonResourceName)
  }
derive instance newtypeKinesisFirehoseDestination :: Newtype KinesisFirehoseDestination _
derive instance repGenericKinesisFirehoseDestination :: Generic KinesisFirehoseDestination _
instance showKinesisFirehoseDestination :: Show KinesisFirehoseDestination where
  show = genericShow
instance decodeKinesisFirehoseDestination :: Decode KinesisFirehoseDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseDestination :: Encode KinesisFirehoseDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <p>For information about using AWS Lambda actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html">Amazon SES Developer Guide</a>.</p>
newtype LambdaAction = LambdaAction 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "FunctionArn" :: (AmazonResourceName)
  , "InvocationType" :: NullOrUndefined.NullOrUndefined (InvocationType)
  }
derive instance newtypeLambdaAction :: Newtype LambdaAction _
derive instance repGenericLambdaAction :: Generic LambdaAction _
instance showLambdaAction :: Show LambdaAction where
  show = genericShow
instance decodeLambdaAction :: Decode LambdaAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaAction :: Encode LambdaAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastAttemptDate = LastAttemptDate Number
derive instance newtypeLastAttemptDate :: Newtype LastAttemptDate _
derive instance repGenericLastAttemptDate :: Generic LastAttemptDate _
instance showLastAttemptDate :: Show LastAttemptDate where
  show = genericShow
instance decodeLastAttemptDate :: Decode LastAttemptDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastAttemptDate :: Encode LastAttemptDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastFreshStart = LastFreshStart Number
derive instance newtypeLastFreshStart :: Newtype LastFreshStart _
derive instance repGenericLastFreshStart :: Generic LastFreshStart _
instance showLastFreshStart :: Show LastFreshStart where
  show = genericShow
instance decodeLastFreshStart :: Decode LastFreshStart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastFreshStart :: Encode LastFreshStart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a resource could not be created because of service limits. For a list of Amazon SES limits, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html">Amazon SES Developer Guide</a>.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype ListConfigurationSetsRequest = ListConfigurationSetsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  }
derive instance newtypeListConfigurationSetsRequest :: Newtype ListConfigurationSetsRequest _
derive instance repGenericListConfigurationSetsRequest :: Generic ListConfigurationSetsRequest _
instance showListConfigurationSetsRequest :: Show ListConfigurationSetsRequest where
  show = genericShow
instance decodeListConfigurationSetsRequest :: Decode ListConfigurationSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListConfigurationSetsRequest :: Encode ListConfigurationSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype ListConfigurationSetsResponse = ListConfigurationSetsResponse 
  { "ConfigurationSets" :: NullOrUndefined.NullOrUndefined (ConfigurationSets)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListConfigurationSetsResponse :: Newtype ListConfigurationSetsResponse _
derive instance repGenericListConfigurationSetsResponse :: Generic ListConfigurationSetsResponse _
instance showListConfigurationSetsResponse :: Show ListConfigurationSetsResponse where
  show = genericShow
instance decodeListConfigurationSetsResponse :: Decode ListConfigurationSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListConfigurationSetsResponse :: Encode ListConfigurationSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to list the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p>
newtype ListCustomVerificationEmailTemplatesRequest = ListCustomVerificationEmailTemplatesRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListCustomVerificationEmailTemplatesRequest :: Newtype ListCustomVerificationEmailTemplatesRequest _
derive instance repGenericListCustomVerificationEmailTemplatesRequest :: Generic ListCustomVerificationEmailTemplatesRequest _
instance showListCustomVerificationEmailTemplatesRequest :: Show ListCustomVerificationEmailTemplatesRequest where
  show = genericShow
instance decodeListCustomVerificationEmailTemplatesRequest :: Decode ListCustomVerificationEmailTemplatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCustomVerificationEmailTemplatesRequest :: Encode ListCustomVerificationEmailTemplatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A paginated list of custom verification email templates.</p>
newtype ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse 
  { "CustomVerificationEmailTemplates" :: NullOrUndefined.NullOrUndefined (CustomVerificationEmailTemplates)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListCustomVerificationEmailTemplatesResponse :: Newtype ListCustomVerificationEmailTemplatesResponse _
derive instance repGenericListCustomVerificationEmailTemplatesResponse :: Generic ListCustomVerificationEmailTemplatesResponse _
instance showListCustomVerificationEmailTemplatesResponse :: Show ListCustomVerificationEmailTemplatesResponse where
  show = genericShow
instance decodeListCustomVerificationEmailTemplatesResponse :: Decode ListCustomVerificationEmailTemplatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCustomVerificationEmailTemplatesResponse :: Encode ListCustomVerificationEmailTemplatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.</p>
newtype ListIdentitiesRequest = ListIdentitiesRequest 
  { "IdentityType" :: NullOrUndefined.NullOrUndefined (IdentityType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  }
derive instance newtypeListIdentitiesRequest :: Newtype ListIdentitiesRequest _
derive instance repGenericListIdentitiesRequest :: Generic ListIdentitiesRequest _
instance showListIdentitiesRequest :: Show ListIdentitiesRequest where
  show = genericShow
instance decodeListIdentitiesRequest :: Decode ListIdentitiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentitiesRequest :: Encode ListIdentitiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.</p>
newtype ListIdentitiesResponse = ListIdentitiesResponse 
  { "Identities" :: (IdentityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIdentitiesResponse :: Newtype ListIdentitiesResponse _
derive instance repGenericListIdentitiesResponse :: Generic ListIdentitiesResponse _
instance showListIdentitiesResponse :: Show ListIdentitiesResponse where
  show = genericShow
instance decodeListIdentitiesResponse :: Decode ListIdentitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentitiesResponse :: Encode ListIdentitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to return a list of sending authorization policies that are attached to an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype ListIdentityPoliciesRequest = ListIdentityPoliciesRequest 
  { "Identity" :: (Identity)
  }
derive instance newtypeListIdentityPoliciesRequest :: Newtype ListIdentityPoliciesRequest _
derive instance repGenericListIdentityPoliciesRequest :: Generic ListIdentityPoliciesRequest _
instance showListIdentityPoliciesRequest :: Show ListIdentityPoliciesRequest where
  show = genericShow
instance decodeListIdentityPoliciesRequest :: Decode ListIdentityPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoliciesRequest :: Encode ListIdentityPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of names of sending authorization policies that apply to an identity.</p>
newtype ListIdentityPoliciesResponse = ListIdentityPoliciesResponse 
  { "PolicyNames" :: (PolicyNameList)
  }
derive instance newtypeListIdentityPoliciesResponse :: Newtype ListIdentityPoliciesResponse _
derive instance repGenericListIdentityPoliciesResponse :: Generic ListIdentityPoliciesResponse _
instance showListIdentityPoliciesResponse :: Show ListIdentityPoliciesResponse where
  show = genericShow
instance decodeListIdentityPoliciesResponse :: Decode ListIdentityPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoliciesResponse :: Encode ListIdentityPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ListReceiptFiltersRequest = ListReceiptFiltersRequest Types.NoArguments
derive instance newtypeListReceiptFiltersRequest :: Newtype ListReceiptFiltersRequest _
derive instance repGenericListReceiptFiltersRequest :: Generic ListReceiptFiltersRequest _
instance showListReceiptFiltersRequest :: Show ListReceiptFiltersRequest where
  show = genericShow
instance decodeListReceiptFiltersRequest :: Decode ListReceiptFiltersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReceiptFiltersRequest :: Encode ListReceiptFiltersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of IP address filters that exist under your AWS account.</p>
newtype ListReceiptFiltersResponse = ListReceiptFiltersResponse 
  { "Filters" :: NullOrUndefined.NullOrUndefined (ReceiptFilterList)
  }
derive instance newtypeListReceiptFiltersResponse :: Newtype ListReceiptFiltersResponse _
derive instance repGenericListReceiptFiltersResponse :: Generic ListReceiptFiltersResponse _
instance showListReceiptFiltersResponse :: Show ListReceiptFiltersResponse where
  show = genericShow
instance decodeListReceiptFiltersResponse :: Decode ListReceiptFiltersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReceiptFiltersResponse :: Encode ListReceiptFiltersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to list the receipt rule sets that exist under your AWS account. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ListReceiptRuleSetsRequest = ListReceiptRuleSetsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListReceiptRuleSetsRequest :: Newtype ListReceiptRuleSetsRequest _
derive instance repGenericListReceiptRuleSetsRequest :: Generic ListReceiptRuleSetsRequest _
instance showListReceiptRuleSetsRequest :: Show ListReceiptRuleSetsRequest where
  show = genericShow
instance decodeListReceiptRuleSetsRequest :: Decode ListReceiptRuleSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReceiptRuleSetsRequest :: Encode ListReceiptRuleSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of receipt rule sets that exist under your AWS account.</p>
newtype ListReceiptRuleSetsResponse = ListReceiptRuleSetsResponse 
  { "RuleSets" :: NullOrUndefined.NullOrUndefined (ReceiptRuleSetsLists)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListReceiptRuleSetsResponse :: Newtype ListReceiptRuleSetsResponse _
derive instance repGenericListReceiptRuleSetsResponse :: Generic ListReceiptRuleSetsResponse _
instance showListReceiptRuleSetsResponse :: Show ListReceiptRuleSetsResponse where
  show = genericShow
instance decodeListReceiptRuleSetsResponse :: Decode ListReceiptRuleSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReceiptRuleSetsResponse :: Encode ListReceiptRuleSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTemplatesRequest = ListTemplatesRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  }
derive instance newtypeListTemplatesRequest :: Newtype ListTemplatesRequest _
derive instance repGenericListTemplatesRequest :: Generic ListTemplatesRequest _
instance showListTemplatesRequest :: Show ListTemplatesRequest where
  show = genericShow
instance decodeListTemplatesRequest :: Decode ListTemplatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTemplatesRequest :: Encode ListTemplatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTemplatesResponse = ListTemplatesResponse 
  { "TemplatesMetadata" :: NullOrUndefined.NullOrUndefined (TemplateMetadataList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTemplatesResponse :: Newtype ListTemplatesResponse _
derive instance repGenericListTemplatesResponse :: Generic ListTemplatesResponse _
instance showListTemplatesResponse :: Show ListTemplatesResponse where
  show = genericShow
instance decodeListTemplatesResponse :: Decode ListTemplatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTemplatesResponse :: Encode ListTemplatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of email addresses that you have verified with Amazon SES under your AWS account.</p>
newtype ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse 
  { "VerifiedEmailAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  }
derive instance newtypeListVerifiedEmailAddressesResponse :: Newtype ListVerifiedEmailAddressesResponse _
derive instance repGenericListVerifiedEmailAddressesResponse :: Generic ListVerifiedEmailAddressesResponse _
instance showListVerifiedEmailAddressesResponse :: Show ListVerifiedEmailAddressesResponse where
  show = genericShow
instance decodeListVerifiedEmailAddressesResponse :: Decode ListVerifiedEmailAddressesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVerifiedEmailAddressesResponse :: Encode ListVerifiedEmailAddressesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MailFromDomainAttributes = MailFromDomainAttributes (StrMap.StrMap IdentityMailFromDomainAttributes)
derive instance newtypeMailFromDomainAttributes :: Newtype MailFromDomainAttributes _
derive instance repGenericMailFromDomainAttributes :: Generic MailFromDomainAttributes _
instance showMailFromDomainAttributes :: Show MailFromDomainAttributes where
  show = genericShow
instance decodeMailFromDomainAttributes :: Decode MailFromDomainAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMailFromDomainAttributes :: Encode MailFromDomainAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MailFromDomainName = MailFromDomainName String
derive instance newtypeMailFromDomainName :: Newtype MailFromDomainName _
derive instance repGenericMailFromDomainName :: Generic MailFromDomainName _
instance showMailFromDomainName :: Show MailFromDomainName where
  show = genericShow
instance decodeMailFromDomainName :: Decode MailFromDomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMailFromDomainName :: Encode MailFromDomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Indicates that the message could not be sent because Amazon SES could not read the MX record required to use the specified MAIL FROM domain. For information about editing the custom MAIL FROM domain settings for an identity, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html">Amazon SES Developer Guide</a>.</p>
newtype MailFromDomainNotVerifiedException = MailFromDomainNotVerifiedException Types.NoArguments
derive instance newtypeMailFromDomainNotVerifiedException :: Newtype MailFromDomainNotVerifiedException _
derive instance repGenericMailFromDomainNotVerifiedException :: Generic MailFromDomainNotVerifiedException _
instance showMailFromDomainNotVerifiedException :: Show MailFromDomainNotVerifiedException where
  show = genericShow
instance decodeMailFromDomainNotVerifiedException :: Decode MailFromDomainNotVerifiedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMailFromDomainNotVerifiedException :: Encode MailFromDomainNotVerifiedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Max24HourSend = Max24HourSend Number
derive instance newtypeMax24HourSend :: Newtype Max24HourSend _
derive instance repGenericMax24HourSend :: Generic Max24HourSend _
instance showMax24HourSend :: Show Max24HourSend where
  show = genericShow
instance decodeMax24HourSend :: Decode Max24HourSend where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMax24HourSend :: Encode Max24HourSend where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxItems = MaxItems Int
derive instance newtypeMaxItems :: Newtype MaxItems _
derive instance repGenericMaxItems :: Generic MaxItems _
instance showMaxItems :: Show MaxItems where
  show = genericShow
instance decodeMaxItems :: Decode MaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxItems :: Encode MaxItems where
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


newtype MaxSendRate = MaxSendRate Number
derive instance newtypeMaxSendRate :: Newtype MaxSendRate _
derive instance repGenericMaxSendRate :: Generic MaxSendRate _
instance showMaxSendRate :: Show MaxSendRate where
  show = genericShow
instance decodeMaxSendRate :: Decode MaxSendRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxSendRate :: Encode MaxSendRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the message to be sent, composed of a subject and a body.</p>
newtype Message = Message 
  { "Subject" :: (Content)
  , "Body" :: (Body)
  }
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageData = MessageData String
derive instance newtypeMessageData :: Newtype MessageData _
derive instance repGenericMessageData :: Generic MessageData _
instance showMessageData :: Show MessageData where
  show = genericShow
instance decodeMessageData :: Decode MessageData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageData :: Encode MessageData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype MessageDsn = MessageDsn 
  { "ReportingMta" :: (ReportingMta)
  , "ArrivalDate" :: NullOrUndefined.NullOrUndefined (ArrivalDate)
  , "ExtensionFields" :: NullOrUndefined.NullOrUndefined (ExtensionFieldList)
  }
derive instance newtypeMessageDsn :: Newtype MessageDsn _
derive instance repGenericMessageDsn :: Generic MessageDsn _
instance showMessageDsn :: Show MessageDsn where
  show = genericShow
instance decodeMessageDsn :: Decode MessageDsn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageDsn :: Encode MessageDsn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageId = MessageId String
derive instance newtypeMessageId :: Newtype MessageId _
derive instance repGenericMessageId :: Generic MessageId _
instance showMessageId :: Show MessageId where
  show = genericShow
instance decodeMessageId :: Decode MessageId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageId :: Encode MessageId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the action failed, and the message could not be sent. Check the error stack for more information about what caused the error.</p>
newtype MessageRejected = MessageRejected Types.NoArguments
derive instance newtypeMessageRejected :: Newtype MessageRejected _
derive instance repGenericMessageRejected :: Generic MessageRejected _
instance showMessageRejected :: Show MessageRejected where
  show = genericShow
instance decodeMessageRejected :: Decode MessageRejected where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageRejected :: Encode MessageRejected where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the name and value of a tag that you can provide to <code>SendEmail</code> or <code>SendRawEmail</code> to apply to an email.</p> <p>Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype MessageTag = MessageTag 
  { "Name" :: (MessageTagName)
  , "Value" :: (MessageTagValue)
  }
derive instance newtypeMessageTag :: Newtype MessageTag _
derive instance repGenericMessageTag :: Generic MessageTag _
instance showMessageTag :: Show MessageTag where
  show = genericShow
instance decodeMessageTag :: Decode MessageTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageTag :: Encode MessageTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageTagList = MessageTagList (Array MessageTag)
derive instance newtypeMessageTagList :: Newtype MessageTagList _
derive instance repGenericMessageTagList :: Generic MessageTagList _
instance showMessageTagList :: Show MessageTagList where
  show = genericShow
instance decodeMessageTagList :: Decode MessageTagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageTagList :: Encode MessageTagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageTagName = MessageTagName String
derive instance newtypeMessageTagName :: Newtype MessageTagName _
derive instance repGenericMessageTagName :: Generic MessageTagName _
instance showMessageTagName :: Show MessageTagName where
  show = genericShow
instance decodeMessageTagName :: Decode MessageTagName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageTagName :: Encode MessageTagName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageTagValue = MessageTagValue String
derive instance newtypeMessageTagValue :: Newtype MessageTagValue _
derive instance repGenericMessageTagValue :: Generic MessageTagValue _
instance showMessageTagValue :: Show MessageTagValue where
  show = genericShow
instance decodeMessageTagValue :: Decode MessageTagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageTagValue :: Encode MessageTagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that one or more of the replacement values for the specified template was not specified. Ensure that the TemplateData object contains references to all of the replacement tags in the specified template.</p>
newtype MissingRenderingAttributeException = MissingRenderingAttributeException 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeMissingRenderingAttributeException :: Newtype MissingRenderingAttributeException _
derive instance repGenericMissingRenderingAttributeException :: Generic MissingRenderingAttributeException _
instance showMissingRenderingAttributeException :: Show MissingRenderingAttributeException where
  show = genericShow
instance decodeMissingRenderingAttributeException :: Decode MissingRenderingAttributeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingRenderingAttributeException :: Encode MissingRenderingAttributeException where
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


newtype NotificationAttributes = NotificationAttributes (StrMap.StrMap IdentityNotificationAttributes)
derive instance newtypeNotificationAttributes :: Newtype NotificationAttributes _
derive instance repGenericNotificationAttributes :: Generic NotificationAttributes _
instance showNotificationAttributes :: Show NotificationAttributes where
  show = genericShow
instance decodeNotificationAttributes :: Decode NotificationAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationAttributes :: Encode NotificationAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationTopic = NotificationTopic String
derive instance newtypeNotificationTopic :: Newtype NotificationTopic _
derive instance repGenericNotificationTopic :: Generic NotificationTopic _
instance showNotificationTopic :: Show NotificationTopic where
  show = genericShow
instance decodeNotificationTopic :: Decode NotificationTopic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationTopic :: Encode NotificationTopic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationType = NotificationType String
derive instance newtypeNotificationType :: Newtype NotificationType _
derive instance repGenericNotificationType :: Generic NotificationType _
instance showNotificationType :: Show NotificationType where
  show = genericShow
instance decodeNotificationType :: Decode NotificationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationType :: Encode NotificationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Policy = Policy String
derive instance newtypePolicy :: Newtype Policy _
derive instance repGenericPolicy :: Generic Policy _
instance showPolicy :: Show Policy where
  show = genericShow
instance decodePolicy :: Decode Policy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicy :: Encode Policy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyMap = PolicyMap (StrMap.StrMap Policy)
derive instance newtypePolicyMap :: Newtype PolicyMap _
derive instance repGenericPolicyMap :: Generic PolicyMap _
instance showPolicyMap :: Show PolicyMap where
  show = genericShow
instance decodePolicyMap :: Decode PolicyMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyMap :: Encode PolicyMap where
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


newtype PolicyNameList = PolicyNameList (Array PolicyName)
derive instance newtypePolicyNameList :: Newtype PolicyNameList _
derive instance repGenericPolicyNameList :: Generic PolicyNameList _
instance showPolicyNameList :: Show PolicyNameList where
  show = genericShow
instance decodePolicyNameList :: Decode PolicyNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNameList :: Encode PolicyNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the account has not been granted production access.</p>
newtype ProductionAccessNotGrantedException = ProductionAccessNotGrantedException Types.NoArguments
derive instance newtypeProductionAccessNotGrantedException :: Newtype ProductionAccessNotGrantedException _
derive instance repGenericProductionAccessNotGrantedException :: Generic ProductionAccessNotGrantedException _
instance showProductionAccessNotGrantedException :: Show ProductionAccessNotGrantedException where
  show = genericShow
instance decodeProductionAccessNotGrantedException :: Decode ProductionAccessNotGrantedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProductionAccessNotGrantedException :: Encode ProductionAccessNotGrantedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype PutIdentityPolicyRequest = PutIdentityPolicyRequest 
  { "Identity" :: (Identity)
  , "PolicyName" :: (PolicyName)
  , "Policy" :: (Policy)
  }
derive instance newtypePutIdentityPolicyRequest :: Newtype PutIdentityPolicyRequest _
derive instance repGenericPutIdentityPolicyRequest :: Generic PutIdentityPolicyRequest _
instance showPutIdentityPolicyRequest :: Show PutIdentityPolicyRequest where
  show = genericShow
instance decodePutIdentityPolicyRequest :: Decode PutIdentityPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIdentityPolicyRequest :: Encode PutIdentityPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse Types.NoArguments
derive instance newtypePutIdentityPolicyResponse :: Newtype PutIdentityPolicyResponse _
derive instance repGenericPutIdentityPolicyResponse :: Generic PutIdentityPolicyResponse _
instance showPutIdentityPolicyResponse :: Show PutIdentityPolicyResponse where
  show = genericShow
instance decodePutIdentityPolicyResponse :: Decode PutIdentityPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutIdentityPolicyResponse :: Encode PutIdentityPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the raw data of the message.</p>
newtype RawMessage = RawMessage 
  { "Data" :: (RawMessageData)
  }
derive instance newtypeRawMessage :: Newtype RawMessage _
derive instance repGenericRawMessage :: Generic RawMessage _
instance showRawMessage :: Show RawMessage where
  show = genericShow
instance decodeRawMessage :: Decode RawMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRawMessage :: Encode RawMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RawMessageData = RawMessageData String
derive instance newtypeRawMessageData :: Newtype RawMessageData _
derive instance repGenericRawMessageData :: Generic RawMessageData _
instance showRawMessageData :: Show RawMessageData where
  show = genericShow
instance decodeRawMessageData :: Decode RawMessageData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRawMessageData :: Encode RawMessageData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptAction = ReceiptAction 
  { "S3Action" :: NullOrUndefined.NullOrUndefined (S3Action)
  , "BounceAction" :: NullOrUndefined.NullOrUndefined (BounceAction)
  , "WorkmailAction" :: NullOrUndefined.NullOrUndefined (WorkmailAction)
  , "LambdaAction" :: NullOrUndefined.NullOrUndefined (LambdaAction)
  , "StopAction" :: NullOrUndefined.NullOrUndefined (StopAction)
  , "AddHeaderAction" :: NullOrUndefined.NullOrUndefined (AddHeaderAction)
  , "SNSAction" :: NullOrUndefined.NullOrUndefined (SNSAction)
  }
derive instance newtypeReceiptAction :: Newtype ReceiptAction _
derive instance repGenericReceiptAction :: Generic ReceiptAction _
instance showReceiptAction :: Show ReceiptAction where
  show = genericShow
instance decodeReceiptAction :: Decode ReceiptAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptAction :: Encode ReceiptAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptActionsList = ReceiptActionsList (Array ReceiptAction)
derive instance newtypeReceiptActionsList :: Newtype ReceiptActionsList _
derive instance repGenericReceiptActionsList :: Generic ReceiptActionsList _
instance showReceiptActionsList :: Show ReceiptActionsList where
  show = genericShow
instance decodeReceiptActionsList :: Decode ReceiptActionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptActionsList :: Encode ReceiptActionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptFilter = ReceiptFilter 
  { "Name" :: (ReceiptFilterName)
  , "IpFilter" :: (ReceiptIpFilter)
  }
derive instance newtypeReceiptFilter :: Newtype ReceiptFilter _
derive instance repGenericReceiptFilter :: Generic ReceiptFilter _
instance showReceiptFilter :: Show ReceiptFilter where
  show = genericShow
instance decodeReceiptFilter :: Decode ReceiptFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptFilter :: Encode ReceiptFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptFilterList = ReceiptFilterList (Array ReceiptFilter)
derive instance newtypeReceiptFilterList :: Newtype ReceiptFilterList _
derive instance repGenericReceiptFilterList :: Generic ReceiptFilterList _
instance showReceiptFilterList :: Show ReceiptFilterList where
  show = genericShow
instance decodeReceiptFilterList :: Decode ReceiptFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptFilterList :: Encode ReceiptFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptFilterName = ReceiptFilterName String
derive instance newtypeReceiptFilterName :: Newtype ReceiptFilterName _
derive instance repGenericReceiptFilterName :: Generic ReceiptFilterName _
instance showReceiptFilterName :: Show ReceiptFilterName where
  show = genericShow
instance decodeReceiptFilterName :: Decode ReceiptFilterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptFilterName :: Encode ReceiptFilterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptFilterPolicy = ReceiptFilterPolicy String
derive instance newtypeReceiptFilterPolicy :: Newtype ReceiptFilterPolicy _
derive instance repGenericReceiptFilterPolicy :: Generic ReceiptFilterPolicy _
instance showReceiptFilterPolicy :: Show ReceiptFilterPolicy where
  show = genericShow
instance decodeReceiptFilterPolicy :: Decode ReceiptFilterPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptFilterPolicy :: Encode ReceiptFilterPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptIpFilter = ReceiptIpFilter 
  { "Policy" :: (ReceiptFilterPolicy)
  , "Cidr" :: (Cidr)
  }
derive instance newtypeReceiptIpFilter :: Newtype ReceiptIpFilter _
derive instance repGenericReceiptIpFilter :: Generic ReceiptIpFilter _
instance showReceiptIpFilter :: Show ReceiptIpFilter where
  show = genericShow
instance decodeReceiptIpFilter :: Decode ReceiptIpFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptIpFilter :: Encode ReceiptIpFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.</p> <p>Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptRule = ReceiptRule 
  { "Name" :: (ReceiptRuleName)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "TlsPolicy" :: NullOrUndefined.NullOrUndefined (TlsPolicy)
  , "Recipients" :: NullOrUndefined.NullOrUndefined (RecipientsList)
  , "Actions" :: NullOrUndefined.NullOrUndefined (ReceiptActionsList)
  , "ScanEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  }
derive instance newtypeReceiptRule :: Newtype ReceiptRule _
derive instance repGenericReceiptRule :: Generic ReceiptRule _
instance showReceiptRule :: Show ReceiptRule where
  show = genericShow
instance decodeReceiptRule :: Decode ReceiptRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRule :: Encode ReceiptRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptRuleName = ReceiptRuleName String
derive instance newtypeReceiptRuleName :: Newtype ReceiptRuleName _
derive instance repGenericReceiptRuleName :: Generic ReceiptRuleName _
instance showReceiptRuleName :: Show ReceiptRuleName where
  show = genericShow
instance decodeReceiptRuleName :: Decode ReceiptRuleName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRuleName :: Encode ReceiptRuleName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptRuleNamesList = ReceiptRuleNamesList (Array ReceiptRuleName)
derive instance newtypeReceiptRuleNamesList :: Newtype ReceiptRuleNamesList _
derive instance repGenericReceiptRuleNamesList :: Generic ReceiptRuleNamesList _
instance showReceiptRuleNamesList :: Show ReceiptRuleNamesList where
  show = genericShow
instance decodeReceiptRuleNamesList :: Decode ReceiptRuleNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRuleNamesList :: Encode ReceiptRuleNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a receipt rule set.</p> <p>A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptRuleSetMetadata = ReceiptRuleSetMetadata 
  { "Name" :: NullOrUndefined.NullOrUndefined (ReceiptRuleSetName)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeReceiptRuleSetMetadata :: Newtype ReceiptRuleSetMetadata _
derive instance repGenericReceiptRuleSetMetadata :: Generic ReceiptRuleSetMetadata _
instance showReceiptRuleSetMetadata :: Show ReceiptRuleSetMetadata where
  show = genericShow
instance decodeReceiptRuleSetMetadata :: Decode ReceiptRuleSetMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRuleSetMetadata :: Encode ReceiptRuleSetMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptRuleSetName = ReceiptRuleSetName String
derive instance newtypeReceiptRuleSetName :: Newtype ReceiptRuleSetName _
derive instance repGenericReceiptRuleSetName :: Generic ReceiptRuleSetName _
instance showReceiptRuleSetName :: Show ReceiptRuleSetName where
  show = genericShow
instance decodeReceiptRuleSetName :: Decode ReceiptRuleSetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRuleSetName :: Encode ReceiptRuleSetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptRuleSetsLists = ReceiptRuleSetsLists (Array ReceiptRuleSetMetadata)
derive instance newtypeReceiptRuleSetsLists :: Newtype ReceiptRuleSetsLists _
derive instance repGenericReceiptRuleSetsLists :: Generic ReceiptRuleSetsLists _
instance showReceiptRuleSetsLists :: Show ReceiptRuleSetsLists where
  show = genericShow
instance decodeReceiptRuleSetsLists :: Decode ReceiptRuleSetsLists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRuleSetsLists :: Encode ReceiptRuleSetsLists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReceiptRulesList = ReceiptRulesList (Array ReceiptRule)
derive instance newtypeReceiptRulesList :: Newtype ReceiptRulesList _
derive instance repGenericReceiptRulesList :: Generic ReceiptRulesList _
instance showReceiptRulesList :: Show ReceiptRulesList where
  show = genericShow
instance decodeReceiptRulesList :: Decode ReceiptRulesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptRulesList :: Encode ReceiptRulesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Recipient = Recipient String
derive instance newtypeRecipient :: Newtype Recipient _
derive instance repGenericRecipient :: Generic Recipient _
instance showRecipient :: Show Recipient where
  show = genericShow
instance decodeRecipient :: Decode Recipient where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecipient :: Encode Recipient where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype RecipientDsnFields = RecipientDsnFields 
  { "FinalRecipient" :: NullOrUndefined.NullOrUndefined (Address)
  , "Action" :: (DsnAction)
  , "RemoteMta" :: NullOrUndefined.NullOrUndefined (RemoteMta)
  , "Status" :: (DsnStatus)
  , "DiagnosticCode" :: NullOrUndefined.NullOrUndefined (DiagnosticCode)
  , "LastAttemptDate" :: NullOrUndefined.NullOrUndefined (LastAttemptDate)
  , "ExtensionFields" :: NullOrUndefined.NullOrUndefined (ExtensionFieldList)
  }
derive instance newtypeRecipientDsnFields :: Newtype RecipientDsnFields _
derive instance repGenericRecipientDsnFields :: Generic RecipientDsnFields _
instance showRecipientDsnFields :: Show RecipientDsnFields where
  show = genericShow
instance decodeRecipientDsnFields :: Decode RecipientDsnFields where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecipientDsnFields :: Encode RecipientDsnFields where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecipientsList = RecipientsList (Array Recipient)
derive instance newtypeRecipientsList :: Newtype RecipientsList _
derive instance repGenericRecipientsList :: Generic RecipientsList _
instance showRecipientsList :: Show RecipientsList where
  show = genericShow
instance decodeRecipientsList :: Decode RecipientsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecipientsList :: Encode RecipientsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoteMta = RemoteMta String
derive instance newtypeRemoteMta :: Newtype RemoteMta _
derive instance repGenericRemoteMta :: Generic RemoteMta _
instance showRemoteMta :: Show RemoteMta where
  show = genericShow
instance decodeRemoteMta :: Decode RemoteMta where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteMta :: Encode RemoteMta where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RenderedTemplate = RenderedTemplate String
derive instance newtypeRenderedTemplate :: Newtype RenderedTemplate _
derive instance repGenericRenderedTemplate :: Generic RenderedTemplate _
instance showRenderedTemplate :: Show RenderedTemplate where
  show = genericShow
instance decodeRenderedTemplate :: Decode RenderedTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRenderedTemplate :: Encode RenderedTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to reorder the receipt rules within a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ReorderReceiptRuleSetRequest = ReorderReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleNames" :: (ReceiptRuleNamesList)
  }
derive instance newtypeReorderReceiptRuleSetRequest :: Newtype ReorderReceiptRuleSetRequest _
derive instance repGenericReorderReceiptRuleSetRequest :: Generic ReorderReceiptRuleSetRequest _
instance showReorderReceiptRuleSetRequest :: Show ReorderReceiptRuleSetRequest where
  show = genericShow
instance decodeReorderReceiptRuleSetRequest :: Decode ReorderReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReorderReceiptRuleSetRequest :: Encode ReorderReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse Types.NoArguments
derive instance newtypeReorderReceiptRuleSetResponse :: Newtype ReorderReceiptRuleSetResponse _
derive instance repGenericReorderReceiptRuleSetResponse :: Generic ReorderReceiptRuleSetResponse _
instance showReorderReceiptRuleSetResponse :: Show ReorderReceiptRuleSetResponse where
  show = genericShow
instance decodeReorderReceiptRuleSetResponse :: Decode ReorderReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReorderReceiptRuleSetResponse :: Encode ReorderReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReportingMta = ReportingMta String
derive instance newtypeReportingMta :: Newtype ReportingMta _
derive instance repGenericReportingMta :: Generic ReportingMta _
instance showReportingMta :: Show ReportingMta where
  show = genericShow
instance decodeReportingMta :: Decode ReportingMta where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReportingMta :: Encode ReportingMta where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the reputation settings for a configuration set.</p>
newtype ReputationOptions = ReputationOptions 
  { "SendingEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "ReputationMetricsEnabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "LastFreshStart" :: NullOrUndefined.NullOrUndefined (LastFreshStart)
  }
derive instance newtypeReputationOptions :: Newtype ReputationOptions _
derive instance repGenericReputationOptions :: Generic ReputationOptions _
instance showReputationOptions :: Show ReputationOptions where
  show = genericShow
instance decodeReputationOptions :: Decode ReputationOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReputationOptions :: Encode ReputationOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided receipt rule does not exist.</p>
newtype RuleDoesNotExistException = RuleDoesNotExistException 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleOrRuleSetName)
  }
derive instance newtypeRuleDoesNotExistException :: Newtype RuleDoesNotExistException _
derive instance repGenericRuleDoesNotExistException :: Generic RuleDoesNotExistException _
instance showRuleDoesNotExistException :: Show RuleDoesNotExistException where
  show = genericShow
instance decodeRuleDoesNotExistException :: Decode RuleDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleDoesNotExistException :: Encode RuleDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleOrRuleSetName = RuleOrRuleSetName String
derive instance newtypeRuleOrRuleSetName :: Newtype RuleOrRuleSetName _
derive instance repGenericRuleOrRuleSetName :: Generic RuleOrRuleSetName _
instance showRuleOrRuleSetName :: Show RuleOrRuleSetName where
  show = genericShow
instance decodeRuleOrRuleSetName :: Decode RuleOrRuleSetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleOrRuleSetName :: Encode RuleOrRuleSetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the provided receipt rule set does not exist.</p>
newtype RuleSetDoesNotExistException = RuleSetDoesNotExistException 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleOrRuleSetName)
  }
derive instance newtypeRuleSetDoesNotExistException :: Newtype RuleSetDoesNotExistException _
derive instance repGenericRuleSetDoesNotExistException :: Generic RuleSetDoesNotExistException _
instance showRuleSetDoesNotExistException :: Show RuleSetDoesNotExistException where
  show = genericShow
instance decodeRuleSetDoesNotExistException :: Decode RuleSetDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleSetDoesNotExistException :: Encode RuleSetDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <note> <p>When you save your emails to an Amazon S3 bucket, the maximum email size (including headers) is 30 MB. Emails larger than that will bounce.</p> </note> <p>For information about specifying Amazon S3 actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html">Amazon SES Developer Guide</a>.</p>
newtype S3Action = S3Action 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "BucketName" :: (S3BucketName)
  , "ObjectKeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  , "KmsKeyArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  }
derive instance newtypeS3Action :: Newtype S3Action _
derive instance repGenericS3Action :: Generic S3Action _
instance showS3Action :: Show S3Action where
  show = genericShow
instance decodeS3Action :: Decode S3Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Action :: Encode S3Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _
derive instance repGenericS3BucketName :: Generic S3BucketName _
instance showS3BucketName :: Show S3BucketName where
  show = genericShow
instance decodeS3BucketName :: Decode S3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketName :: Encode S3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3KeyPrefix = S3KeyPrefix String
derive instance newtypeS3KeyPrefix :: Newtype S3KeyPrefix _
derive instance repGenericS3KeyPrefix :: Generic S3KeyPrefix _
instance showS3KeyPrefix :: Show S3KeyPrefix where
  show = genericShow
instance decodeS3KeyPrefix :: Decode S3KeyPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3KeyPrefix :: Encode S3KeyPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action publishes a notification to Amazon Simple Notification Service (Amazon SNS). This action includes a complete copy of the email content in the Amazon SNS notifications. Amazon SNS notifications for all other actions simply provide information about the email. They do not include the email content itself.</p> <p>If you own the Amazon SNS topic, you don't need to do anything to give Amazon SES permission to publish emails to it. However, if you don't own the Amazon SNS topic, you need to attach a policy to the topic to give Amazon SES permissions to access it. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <important> <p>You can only publish emails that are 150 KB or less (including the header) to Amazon SNS. Larger emails will bounce. If you anticipate emails larger than 150 KB, use the S3 action instead.</p> </important> <p>For information about using a receipt rule to publish an Amazon SNS notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SNSAction = SNSAction 
  { "TopicArn" :: (AmazonResourceName)
  , "Encoding" :: NullOrUndefined.NullOrUndefined (SNSActionEncoding)
  }
derive instance newtypeSNSAction :: Newtype SNSAction _
derive instance repGenericSNSAction :: Generic SNSAction _
instance showSNSAction :: Show SNSAction where
  show = genericShow
instance decodeSNSAction :: Decode SNSAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSAction :: Encode SNSAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SNSActionEncoding = SNSActionEncoding String
derive instance newtypeSNSActionEncoding :: Newtype SNSActionEncoding _
derive instance repGenericSNSActionEncoding :: Generic SNSActionEncoding _
instance showSNSActionEncoding :: Show SNSActionEncoding where
  show = genericShow
instance decodeSNSActionEncoding :: Decode SNSActionEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSActionEncoding :: Encode SNSActionEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.</p> <p>Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype SNSDestination = SNSDestination 
  { "TopicARN" :: (AmazonResourceName)
  }
derive instance newtypeSNSDestination :: Newtype SNSDestination _
derive instance repGenericSNSDestination :: Generic SNSDestination _
instance showSNSDestination :: Show SNSDestination where
  show = genericShow
instance decodeSNSDestination :: Decode SNSDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSDestination :: Encode SNSDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a bounce message to the sender of an email you received through Amazon SES.</p>
newtype SendBounceRequest = SendBounceRequest 
  { "OriginalMessageId" :: (MessageId)
  , "BounceSender" :: (Address)
  , "Explanation" :: NullOrUndefined.NullOrUndefined (Explanation)
  , "MessageDsn" :: NullOrUndefined.NullOrUndefined (MessageDsn)
  , "BouncedRecipientInfoList" :: (BouncedRecipientInfoList)
  , "BounceSenderArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  }
derive instance newtypeSendBounceRequest :: Newtype SendBounceRequest _
derive instance repGenericSendBounceRequest :: Generic SendBounceRequest _
instance showSendBounceRequest :: Show SendBounceRequest where
  show = genericShow
instance decodeSendBounceRequest :: Decode SendBounceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendBounceRequest :: Encode SendBounceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a unique message ID.</p>
newtype SendBounceResponse = SendBounceResponse 
  { "MessageId" :: NullOrUndefined.NullOrUndefined (MessageId)
  }
derive instance newtypeSendBounceResponse :: Newtype SendBounceResponse _
derive instance repGenericSendBounceResponse :: Generic SendBounceResponse _
instance showSendBounceResponse :: Show SendBounceResponse where
  show = genericShow
instance decodeSendBounceResponse :: Decode SendBounceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendBounceResponse :: Encode SendBounceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype SendBulkTemplatedEmailRequest = SendBulkTemplatedEmailRequest 
  { "Source" :: (Address)
  , "SourceArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "ReplyToAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined.NullOrUndefined (Address)
  , "ReturnPathArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "DefaultTags" :: NullOrUndefined.NullOrUndefined (MessageTagList)
  , "Template" :: (TemplateName)
  , "TemplateArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "DefaultTemplateData" :: NullOrUndefined.NullOrUndefined (TemplateData)
  , "Destinations" :: (BulkEmailDestinationList)
  }
derive instance newtypeSendBulkTemplatedEmailRequest :: Newtype SendBulkTemplatedEmailRequest _
derive instance repGenericSendBulkTemplatedEmailRequest :: Generic SendBulkTemplatedEmailRequest _
instance showSendBulkTemplatedEmailRequest :: Show SendBulkTemplatedEmailRequest where
  show = genericShow
instance decodeSendBulkTemplatedEmailRequest :: Decode SendBulkTemplatedEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendBulkTemplatedEmailRequest :: Encode SendBulkTemplatedEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse 
  { "Status" :: (BulkEmailDestinationStatusList)
  }
derive instance newtypeSendBulkTemplatedEmailResponse :: Newtype SendBulkTemplatedEmailResponse _
derive instance repGenericSendBulkTemplatedEmailResponse :: Generic SendBulkTemplatedEmailResponse _
instance showSendBulkTemplatedEmailResponse :: Show SendBulkTemplatedEmailResponse where
  show = genericShow
instance decodeSendBulkTemplatedEmailResponse :: Decode SendBulkTemplatedEmailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendBulkTemplatedEmailResponse :: Encode SendBulkTemplatedEmailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a custom verification email to a specified recipient.</p>
newtype SendCustomVerificationEmailRequest = SendCustomVerificationEmailRequest 
  { "EmailAddress" :: (Address)
  , "TemplateName" :: (TemplateName)
  , "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeSendCustomVerificationEmailRequest :: Newtype SendCustomVerificationEmailRequest _
derive instance repGenericSendCustomVerificationEmailRequest :: Generic SendCustomVerificationEmailRequest _
instance showSendCustomVerificationEmailRequest :: Show SendCustomVerificationEmailRequest where
  show = genericShow
instance decodeSendCustomVerificationEmailRequest :: Decode SendCustomVerificationEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendCustomVerificationEmailRequest :: Encode SendCustomVerificationEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response received when attempting to send the custom verification email.</p>
newtype SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse 
  { "MessageId" :: NullOrUndefined.NullOrUndefined (MessageId)
  }
derive instance newtypeSendCustomVerificationEmailResponse :: Newtype SendCustomVerificationEmailResponse _
derive instance repGenericSendCustomVerificationEmailResponse :: Generic SendCustomVerificationEmailResponse _
instance showSendCustomVerificationEmailResponse :: Show SendCustomVerificationEmailResponse where
  show = genericShow
instance decodeSendCustomVerificationEmailResponse :: Decode SendCustomVerificationEmailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendCustomVerificationEmailResponse :: Encode SendCustomVerificationEmailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents sending statistics data. Each <code>SendDataPoint</code> contains statistics for a 15-minute period of sending activity. </p>
newtype SendDataPoint = SendDataPoint 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "DeliveryAttempts" :: NullOrUndefined.NullOrUndefined (Counter)
  , "Bounces" :: NullOrUndefined.NullOrUndefined (Counter)
  , "Complaints" :: NullOrUndefined.NullOrUndefined (Counter)
  , "Rejects" :: NullOrUndefined.NullOrUndefined (Counter)
  }
derive instance newtypeSendDataPoint :: Newtype SendDataPoint _
derive instance repGenericSendDataPoint :: Generic SendDataPoint _
instance showSendDataPoint :: Show SendDataPoint where
  show = genericShow
instance decodeSendDataPoint :: Decode SendDataPoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendDataPoint :: Encode SendDataPoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendDataPointList = SendDataPointList (Array SendDataPoint)
derive instance newtypeSendDataPointList :: Newtype SendDataPointList _
derive instance repGenericSendDataPointList :: Generic SendDataPointList _
instance showSendDataPointList :: Show SendDataPointList where
  show = genericShow
instance decodeSendDataPointList :: Decode SendDataPointList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendDataPointList :: Encode SendDataPointList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a single formatted email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html">Amazon SES Developer Guide</a>.</p>
newtype SendEmailRequest = SendEmailRequest 
  { "Source" :: (Address)
  , "Destination" :: (Destination)
  , "Message" :: (Message)
  , "ReplyToAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined.NullOrUndefined (Address)
  , "SourceArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined.NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeSendEmailRequest :: Newtype SendEmailRequest _
derive instance repGenericSendEmailRequest :: Generic SendEmailRequest _
instance showSendEmailRequest :: Show SendEmailRequest where
  show = genericShow
instance decodeSendEmailRequest :: Decode SendEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendEmailRequest :: Encode SendEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a unique message ID.</p>
newtype SendEmailResponse = SendEmailResponse 
  { "MessageId" :: (MessageId)
  }
derive instance newtypeSendEmailResponse :: Newtype SendEmailResponse _
derive instance repGenericSendEmailResponse :: Generic SendEmailResponse _
instance showSendEmailResponse :: Show SendEmailResponse where
  show = genericShow
instance decodeSendEmailResponse :: Decode SendEmailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendEmailResponse :: Encode SendEmailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a single raw email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html">Amazon SES Developer Guide</a>.</p>
newtype SendRawEmailRequest = SendRawEmailRequest 
  { "Source" :: NullOrUndefined.NullOrUndefined (Address)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "RawMessage" :: (RawMessage)
  , "FromArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "SourceArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined.NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeSendRawEmailRequest :: Newtype SendRawEmailRequest _
derive instance repGenericSendRawEmailRequest :: Generic SendRawEmailRequest _
instance showSendRawEmailRequest :: Show SendRawEmailRequest where
  show = genericShow
instance decodeSendRawEmailRequest :: Decode SendRawEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendRawEmailRequest :: Encode SendRawEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a unique message ID.</p>
newtype SendRawEmailResponse = SendRawEmailResponse 
  { "MessageId" :: (MessageId)
  }
derive instance newtypeSendRawEmailResponse :: Newtype SendRawEmailResponse _
derive instance repGenericSendRawEmailResponse :: Generic SendRawEmailResponse _
instance showSendRawEmailResponse :: Show SendRawEmailResponse where
  show = genericShow
instance decodeSendRawEmailResponse :: Decode SendRawEmailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendRawEmailResponse :: Encode SendRawEmailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to send a templated email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype SendTemplatedEmailRequest = SendTemplatedEmailRequest 
  { "Source" :: (Address)
  , "Destination" :: (Destination)
  , "ReplyToAddresses" :: NullOrUndefined.NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined.NullOrUndefined (Address)
  , "SourceArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined.NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  , "Template" :: (TemplateName)
  , "TemplateArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "TemplateData" :: (TemplateData)
  }
derive instance newtypeSendTemplatedEmailRequest :: Newtype SendTemplatedEmailRequest _
derive instance repGenericSendTemplatedEmailRequest :: Generic SendTemplatedEmailRequest _
instance showSendTemplatedEmailRequest :: Show SendTemplatedEmailRequest where
  show = genericShow
instance decodeSendTemplatedEmailRequest :: Decode SendTemplatedEmailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTemplatedEmailRequest :: Encode SendTemplatedEmailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTemplatedEmailResponse = SendTemplatedEmailResponse 
  { "MessageId" :: (MessageId)
  }
derive instance newtypeSendTemplatedEmailResponse :: Newtype SendTemplatedEmailResponse _
derive instance repGenericSendTemplatedEmailResponse :: Generic SendTemplatedEmailResponse _
instance showSendTemplatedEmailResponse :: Show SendTemplatedEmailResponse where
  show = genericShow
instance decodeSendTemplatedEmailResponse :: Decode SendTemplatedEmailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTemplatedEmailResponse :: Encode SendTemplatedEmailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SentLast24Hours = SentLast24Hours Number
derive instance newtypeSentLast24Hours :: Newtype SentLast24Hours _
derive instance repGenericSentLast24Hours :: Generic SentLast24Hours _
instance showSentLast24Hours :: Show SentLast24Hours where
  show = genericShow
instance decodeSentLast24Hours :: Decode SentLast24Hours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSentLast24Hours :: Encode SentLast24Hours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to set a receipt rule set as the active receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype SetActiveReceiptRuleSetRequest = SetActiveReceiptRuleSetRequest 
  { "RuleSetName" :: NullOrUndefined.NullOrUndefined (ReceiptRuleSetName)
  }
derive instance newtypeSetActiveReceiptRuleSetRequest :: Newtype SetActiveReceiptRuleSetRequest _
derive instance repGenericSetActiveReceiptRuleSetRequest :: Generic SetActiveReceiptRuleSetRequest _
instance showSetActiveReceiptRuleSetRequest :: Show SetActiveReceiptRuleSetRequest where
  show = genericShow
instance decodeSetActiveReceiptRuleSetRequest :: Decode SetActiveReceiptRuleSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetActiveReceiptRuleSetRequest :: Encode SetActiveReceiptRuleSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse Types.NoArguments
derive instance newtypeSetActiveReceiptRuleSetResponse :: Newtype SetActiveReceiptRuleSetResponse _
derive instance repGenericSetActiveReceiptRuleSetResponse :: Generic SetActiveReceiptRuleSetResponse _
instance showSetActiveReceiptRuleSetResponse :: Show SetActiveReceiptRuleSetResponse where
  show = genericShow
instance decodeSetActiveReceiptRuleSetResponse :: Decode SetActiveReceiptRuleSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetActiveReceiptRuleSetResponse :: Encode SetActiveReceiptRuleSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to enable or disable Amazon SES Easy DKIM signing for an identity. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityDkimEnabledRequest = SetIdentityDkimEnabledRequest 
  { "Identity" :: (Identity)
  , "DkimEnabled" :: (Enabled)
  }
derive instance newtypeSetIdentityDkimEnabledRequest :: Newtype SetIdentityDkimEnabledRequest _
derive instance repGenericSetIdentityDkimEnabledRequest :: Generic SetIdentityDkimEnabledRequest _
instance showSetIdentityDkimEnabledRequest :: Show SetIdentityDkimEnabledRequest where
  show = genericShow
instance decodeSetIdentityDkimEnabledRequest :: Decode SetIdentityDkimEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityDkimEnabledRequest :: Encode SetIdentityDkimEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse Types.NoArguments
derive instance newtypeSetIdentityDkimEnabledResponse :: Newtype SetIdentityDkimEnabledResponse _
derive instance repGenericSetIdentityDkimEnabledResponse :: Generic SetIdentityDkimEnabledResponse _
instance showSetIdentityDkimEnabledResponse :: Show SetIdentityDkimEnabledResponse where
  show = genericShow
instance decodeSetIdentityDkimEnabledResponse :: Decode SetIdentityDkimEnabledResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityDkimEnabledResponse :: Encode SetIdentityDkimEnabledResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityFeedbackForwardingEnabledRequest = SetIdentityFeedbackForwardingEnabledRequest 
  { "Identity" :: (Identity)
  , "ForwardingEnabled" :: (Enabled)
  }
derive instance newtypeSetIdentityFeedbackForwardingEnabledRequest :: Newtype SetIdentityFeedbackForwardingEnabledRequest _
derive instance repGenericSetIdentityFeedbackForwardingEnabledRequest :: Generic SetIdentityFeedbackForwardingEnabledRequest _
instance showSetIdentityFeedbackForwardingEnabledRequest :: Show SetIdentityFeedbackForwardingEnabledRequest where
  show = genericShow
instance decodeSetIdentityFeedbackForwardingEnabledRequest :: Decode SetIdentityFeedbackForwardingEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityFeedbackForwardingEnabledRequest :: Encode SetIdentityFeedbackForwardingEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse Types.NoArguments
derive instance newtypeSetIdentityFeedbackForwardingEnabledResponse :: Newtype SetIdentityFeedbackForwardingEnabledResponse _
derive instance repGenericSetIdentityFeedbackForwardingEnabledResponse :: Generic SetIdentityFeedbackForwardingEnabledResponse _
instance showSetIdentityFeedbackForwardingEnabledResponse :: Show SetIdentityFeedbackForwardingEnabledResponse where
  show = genericShow
instance decodeSetIdentityFeedbackForwardingEnabledResponse :: Decode SetIdentityFeedbackForwardingEnabledResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityFeedbackForwardingEnabledResponse :: Encode SetIdentityFeedbackForwardingEnabledResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityHeadersInNotificationsEnabledRequest = SetIdentityHeadersInNotificationsEnabledRequest 
  { "Identity" :: (Identity)
  , "NotificationType" :: (NotificationType)
  , "Enabled" :: (Enabled)
  }
derive instance newtypeSetIdentityHeadersInNotificationsEnabledRequest :: Newtype SetIdentityHeadersInNotificationsEnabledRequest _
derive instance repGenericSetIdentityHeadersInNotificationsEnabledRequest :: Generic SetIdentityHeadersInNotificationsEnabledRequest _
instance showSetIdentityHeadersInNotificationsEnabledRequest :: Show SetIdentityHeadersInNotificationsEnabledRequest where
  show = genericShow
instance decodeSetIdentityHeadersInNotificationsEnabledRequest :: Decode SetIdentityHeadersInNotificationsEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityHeadersInNotificationsEnabledRequest :: Encode SetIdentityHeadersInNotificationsEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse Types.NoArguments
derive instance newtypeSetIdentityHeadersInNotificationsEnabledResponse :: Newtype SetIdentityHeadersInNotificationsEnabledResponse _
derive instance repGenericSetIdentityHeadersInNotificationsEnabledResponse :: Generic SetIdentityHeadersInNotificationsEnabledResponse _
instance showSetIdentityHeadersInNotificationsEnabledResponse :: Show SetIdentityHeadersInNotificationsEnabledResponse where
  show = genericShow
instance decodeSetIdentityHeadersInNotificationsEnabledResponse :: Decode SetIdentityHeadersInNotificationsEnabledResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityHeadersInNotificationsEnabledResponse :: Encode SetIdentityHeadersInNotificationsEnabledResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to enable or disable the Amazon SES custom MAIL FROM domain setup for a verified identity. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityMailFromDomainRequest = SetIdentityMailFromDomainRequest 
  { "Identity" :: (Identity)
  , "MailFromDomain" :: NullOrUndefined.NullOrUndefined (MailFromDomainName)
  , "BehaviorOnMXFailure" :: NullOrUndefined.NullOrUndefined (BehaviorOnMXFailure)
  }
derive instance newtypeSetIdentityMailFromDomainRequest :: Newtype SetIdentityMailFromDomainRequest _
derive instance repGenericSetIdentityMailFromDomainRequest :: Generic SetIdentityMailFromDomainRequest _
instance showSetIdentityMailFromDomainRequest :: Show SetIdentityMailFromDomainRequest where
  show = genericShow
instance decodeSetIdentityMailFromDomainRequest :: Decode SetIdentityMailFromDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityMailFromDomainRequest :: Encode SetIdentityMailFromDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityMailFromDomainResponse = SetIdentityMailFromDomainResponse Types.NoArguments
derive instance newtypeSetIdentityMailFromDomainResponse :: Newtype SetIdentityMailFromDomainResponse _
derive instance repGenericSetIdentityMailFromDomainResponse :: Generic SetIdentityMailFromDomainResponse _
instance showSetIdentityMailFromDomainResponse :: Show SetIdentityMailFromDomainResponse where
  show = genericShow
instance decodeSetIdentityMailFromDomainResponse :: Decode SetIdentityMailFromDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityMailFromDomainResponse :: Encode SetIdentityMailFromDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to specify the Amazon SNS topic to which Amazon SES will publish bounce, complaint, or delivery notifications for emails sent with that identity as the Source. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityNotificationTopicRequest = SetIdentityNotificationTopicRequest 
  { "Identity" :: (Identity)
  , "NotificationType" :: (NotificationType)
  , "SnsTopic" :: NullOrUndefined.NullOrUndefined (NotificationTopic)
  }
derive instance newtypeSetIdentityNotificationTopicRequest :: Newtype SetIdentityNotificationTopicRequest _
derive instance repGenericSetIdentityNotificationTopicRequest :: Generic SetIdentityNotificationTopicRequest _
instance showSetIdentityNotificationTopicRequest :: Show SetIdentityNotificationTopicRequest where
  show = genericShow
instance decodeSetIdentityNotificationTopicRequest :: Decode SetIdentityNotificationTopicRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityNotificationTopicRequest :: Encode SetIdentityNotificationTopicRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse Types.NoArguments
derive instance newtypeSetIdentityNotificationTopicResponse :: Newtype SetIdentityNotificationTopicResponse _
derive instance repGenericSetIdentityNotificationTopicResponse :: Generic SetIdentityNotificationTopicResponse _
instance showSetIdentityNotificationTopicResponse :: Show SetIdentityNotificationTopicResponse where
  show = genericShow
instance decodeSetIdentityNotificationTopicResponse :: Decode SetIdentityNotificationTopicResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityNotificationTopicResponse :: Encode SetIdentityNotificationTopicResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype SetReceiptRulePositionRequest = SetReceiptRulePositionRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  , "After" :: NullOrUndefined.NullOrUndefined (ReceiptRuleName)
  }
derive instance newtypeSetReceiptRulePositionRequest :: Newtype SetReceiptRulePositionRequest _
derive instance repGenericSetReceiptRulePositionRequest :: Generic SetReceiptRulePositionRequest _
instance showSetReceiptRulePositionRequest :: Show SetReceiptRulePositionRequest where
  show = genericShow
instance decodeSetReceiptRulePositionRequest :: Decode SetReceiptRulePositionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetReceiptRulePositionRequest :: Encode SetReceiptRulePositionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse Types.NoArguments
derive instance newtypeSetReceiptRulePositionResponse :: Newtype SetReceiptRulePositionResponse _
derive instance repGenericSetReceiptRulePositionResponse :: Generic SetReceiptRulePositionResponse _
instance showSetReceiptRulePositionResponse :: Show SetReceiptRulePositionResponse where
  show = genericShow
instance decodeSetReceiptRulePositionResponse :: Decode SetReceiptRulePositionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetReceiptRulePositionResponse :: Encode SetReceiptRulePositionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about setting a stop action in a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html">Amazon SES Developer Guide</a>.</p>
newtype StopAction = StopAction 
  { "Scope" :: (StopScope)
  , "TopicArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  }
derive instance newtypeStopAction :: Newtype StopAction _
derive instance repGenericStopAction :: Generic StopAction _
instance showStopAction :: Show StopAction where
  show = genericShow
instance decodeStopAction :: Decode StopAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopAction :: Encode StopAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopScope = StopScope String
derive instance newtypeStopScope :: Newtype StopScope _
derive instance repGenericStopScope :: Generic StopScope _
instance showStopScope :: Show StopScope where
  show = genericShow
instance decodeStopScope :: Decode StopScope where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopScope :: Encode StopScope where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Subject = Subject String
derive instance newtypeSubject :: Newtype Subject _
derive instance repGenericSubject :: Generic Subject _
instance showSubject :: Show Subject where
  show = genericShow
instance decodeSubject :: Decode Subject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubject :: Encode Subject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubjectPart = SubjectPart String
derive instance newtypeSubjectPart :: Newtype SubjectPart _
derive instance repGenericSubjectPart :: Generic SubjectPart _
instance showSubjectPart :: Show SubjectPart where
  show = genericShow
instance decodeSubjectPart :: Decode SubjectPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubjectPart :: Encode SubjectPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SuccessRedirectionURL = SuccessRedirectionURL String
derive instance newtypeSuccessRedirectionURL :: Newtype SuccessRedirectionURL _
derive instance repGenericSuccessRedirectionURL :: Generic SuccessRedirectionURL _
instance showSuccessRedirectionURL :: Show SuccessRedirectionURL where
  show = genericShow
instance decodeSuccessRedirectionURL :: Decode SuccessRedirectionURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuccessRedirectionURL :: Encode SuccessRedirectionURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The content of the email, composed of a subject line, an HTML part, and a text-only part.</p>
newtype Template = Template 
  { "TemplateName" :: (TemplateName)
  , "SubjectPart" :: NullOrUndefined.NullOrUndefined (SubjectPart)
  , "TextPart" :: NullOrUndefined.NullOrUndefined (TextPart)
  , "HtmlPart" :: NullOrUndefined.NullOrUndefined (HtmlPart)
  }
derive instance newtypeTemplate :: Newtype Template _
derive instance repGenericTemplate :: Generic Template _
instance showTemplate :: Show Template where
  show = genericShow
instance decodeTemplate :: Decode Template where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplate :: Encode Template where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateContent = TemplateContent String
derive instance newtypeTemplateContent :: Newtype TemplateContent _
derive instance repGenericTemplateContent :: Generic TemplateContent _
instance showTemplateContent :: Show TemplateContent where
  show = genericShow
instance decodeTemplateContent :: Decode TemplateContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateContent :: Encode TemplateContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateData = TemplateData String
derive instance newtypeTemplateData :: Newtype TemplateData _
derive instance repGenericTemplateData :: Generic TemplateData _
instance showTemplateData :: Show TemplateData where
  show = genericShow
instance decodeTemplateData :: Decode TemplateData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateData :: Encode TemplateData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the Template object you specified does not exist in your Amazon SES account.</p>
newtype TemplateDoesNotExistException = TemplateDoesNotExistException 
  { "TemplateName" :: NullOrUndefined.NullOrUndefined (TemplateName)
  }
derive instance newtypeTemplateDoesNotExistException :: Newtype TemplateDoesNotExistException _
derive instance repGenericTemplateDoesNotExistException :: Generic TemplateDoesNotExistException _
instance showTemplateDoesNotExistException :: Show TemplateDoesNotExistException where
  show = genericShow
instance decodeTemplateDoesNotExistException :: Decode TemplateDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateDoesNotExistException :: Encode TemplateDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an email template.</p>
newtype TemplateMetadata = TemplateMetadata 
  { "Name" :: NullOrUndefined.NullOrUndefined (TemplateName)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeTemplateMetadata :: Newtype TemplateMetadata _
derive instance repGenericTemplateMetadata :: Generic TemplateMetadata _
instance showTemplateMetadata :: Show TemplateMetadata where
  show = genericShow
instance decodeTemplateMetadata :: Decode TemplateMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateMetadata :: Encode TemplateMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateMetadataList = TemplateMetadataList (Array TemplateMetadata)
derive instance newtypeTemplateMetadataList :: Newtype TemplateMetadataList _
derive instance repGenericTemplateMetadataList :: Generic TemplateMetadataList _
instance showTemplateMetadataList :: Show TemplateMetadataList where
  show = genericShow
instance decodeTemplateMetadataList :: Decode TemplateMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateMetadataList :: Encode TemplateMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateName = TemplateName String
derive instance newtypeTemplateName :: Newtype TemplateName _
derive instance repGenericTemplateName :: Generic TemplateName _
instance showTemplateName :: Show TemplateName where
  show = genericShow
instance decodeTemplateName :: Decode TemplateName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateName :: Encode TemplateName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestRenderTemplateRequest = TestRenderTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "TemplateData" :: (TemplateData)
  }
derive instance newtypeTestRenderTemplateRequest :: Newtype TestRenderTemplateRequest _
derive instance repGenericTestRenderTemplateRequest :: Generic TestRenderTemplateRequest _
instance showTestRenderTemplateRequest :: Show TestRenderTemplateRequest where
  show = genericShow
instance decodeTestRenderTemplateRequest :: Decode TestRenderTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRenderTemplateRequest :: Encode TestRenderTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestRenderTemplateResponse = TestRenderTemplateResponse 
  { "RenderedTemplate" :: NullOrUndefined.NullOrUndefined (RenderedTemplate)
  }
derive instance newtypeTestRenderTemplateResponse :: Newtype TestRenderTemplateResponse _
derive instance repGenericTestRenderTemplateResponse :: Generic TestRenderTemplateResponse _
instance showTestRenderTemplateResponse :: Show TestRenderTemplateResponse where
  show = genericShow
instance decodeTestRenderTemplateResponse :: Decode TestRenderTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRenderTemplateResponse :: Encode TestRenderTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TextPart = TextPart String
derive instance newtypeTextPart :: Newtype TextPart _
derive instance repGenericTextPart :: Generic TextPart _
instance showTextPart :: Show TextPart where
  show = genericShow
instance decodeTextPart :: Decode TextPart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextPart :: Encode TextPart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TlsPolicy = TlsPolicy String
derive instance newtypeTlsPolicy :: Newtype TlsPolicy _
derive instance repGenericTlsPolicy :: Generic TlsPolicy _
instance showTlsPolicy :: Show TlsPolicy where
  show = genericShow
instance decodeTlsPolicy :: Decode TlsPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTlsPolicy :: Encode TlsPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A domain that is used to redirect email recipients to an Amazon SES-operated domain. This domain captures open and click events generated by Amazon SES emails.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
newtype TrackingOptions = TrackingOptions 
  { "CustomRedirectDomain" :: NullOrUndefined.NullOrUndefined (CustomRedirectDomain)
  }
derive instance newtypeTrackingOptions :: Newtype TrackingOptions _
derive instance repGenericTrackingOptions :: Generic TrackingOptions _
instance showTrackingOptions :: Show TrackingOptions where
  show = genericShow
instance decodeTrackingOptions :: Decode TrackingOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrackingOptions :: Encode TrackingOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the configuration set you specified already contains a TrackingOptions object.</p>
newtype TrackingOptionsAlreadyExistsException = TrackingOptionsAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeTrackingOptionsAlreadyExistsException :: Newtype TrackingOptionsAlreadyExistsException _
derive instance repGenericTrackingOptionsAlreadyExistsException :: Generic TrackingOptionsAlreadyExistsException _
instance showTrackingOptionsAlreadyExistsException :: Show TrackingOptionsAlreadyExistsException where
  show = genericShow
instance decodeTrackingOptionsAlreadyExistsException :: Decode TrackingOptionsAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrackingOptionsAlreadyExistsException :: Encode TrackingOptionsAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the TrackingOptions object you specified does not exist.</p>
newtype TrackingOptionsDoesNotExistException = TrackingOptionsDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined.NullOrUndefined (ConfigurationSetName)
  }
derive instance newtypeTrackingOptionsDoesNotExistException :: Newtype TrackingOptionsDoesNotExistException _
derive instance repGenericTrackingOptionsDoesNotExistException :: Generic TrackingOptionsDoesNotExistException _
instance showTrackingOptionsDoesNotExistException :: Show TrackingOptionsDoesNotExistException where
  show = genericShow
instance decodeTrackingOptionsDoesNotExistException :: Decode TrackingOptionsDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrackingOptionsDoesNotExistException :: Encode TrackingOptionsDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to enable or disable the email sending capabilities for your entire Amazon SES account.</p>
newtype UpdateAccountSendingEnabledRequest = UpdateAccountSendingEnabledRequest 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Enabled)
  }
derive instance newtypeUpdateAccountSendingEnabledRequest :: Newtype UpdateAccountSendingEnabledRequest _
derive instance repGenericUpdateAccountSendingEnabledRequest :: Generic UpdateAccountSendingEnabledRequest _
instance showUpdateAccountSendingEnabledRequest :: Show UpdateAccountSendingEnabledRequest where
  show = genericShow
instance decodeUpdateAccountSendingEnabledRequest :: Decode UpdateAccountSendingEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAccountSendingEnabledRequest :: Encode UpdateAccountSendingEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to update the event destination of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype UpdateConfigurationSetEventDestinationRequest = UpdateConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestination" :: (EventDestination)
  }
derive instance newtypeUpdateConfigurationSetEventDestinationRequest :: Newtype UpdateConfigurationSetEventDestinationRequest _
derive instance repGenericUpdateConfigurationSetEventDestinationRequest :: Generic UpdateConfigurationSetEventDestinationRequest _
instance showUpdateConfigurationSetEventDestinationRequest :: Show UpdateConfigurationSetEventDestinationRequest where
  show = genericShow
instance decodeUpdateConfigurationSetEventDestinationRequest :: Decode UpdateConfigurationSetEventDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetEventDestinationRequest :: Encode UpdateConfigurationSetEventDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateConfigurationSetEventDestinationResponse = UpdateConfigurationSetEventDestinationResponse Types.NoArguments
derive instance newtypeUpdateConfigurationSetEventDestinationResponse :: Newtype UpdateConfigurationSetEventDestinationResponse _
derive instance repGenericUpdateConfigurationSetEventDestinationResponse :: Generic UpdateConfigurationSetEventDestinationResponse _
instance showUpdateConfigurationSetEventDestinationResponse :: Show UpdateConfigurationSetEventDestinationResponse where
  show = genericShow
instance decodeUpdateConfigurationSetEventDestinationResponse :: Decode UpdateConfigurationSetEventDestinationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetEventDestinationResponse :: Encode UpdateConfigurationSetEventDestinationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to modify the reputation metric publishing settings for a configuration set.</p>
newtype UpdateConfigurationSetReputationMetricsEnabledRequest = UpdateConfigurationSetReputationMetricsEnabledRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "Enabled" :: (Enabled)
  }
derive instance newtypeUpdateConfigurationSetReputationMetricsEnabledRequest :: Newtype UpdateConfigurationSetReputationMetricsEnabledRequest _
derive instance repGenericUpdateConfigurationSetReputationMetricsEnabledRequest :: Generic UpdateConfigurationSetReputationMetricsEnabledRequest _
instance showUpdateConfigurationSetReputationMetricsEnabledRequest :: Show UpdateConfigurationSetReputationMetricsEnabledRequest where
  show = genericShow
instance decodeUpdateConfigurationSetReputationMetricsEnabledRequest :: Decode UpdateConfigurationSetReputationMetricsEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetReputationMetricsEnabledRequest :: Encode UpdateConfigurationSetReputationMetricsEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to enable or disable the email sending capabilities for a specific configuration set.</p>
newtype UpdateConfigurationSetSendingEnabledRequest = UpdateConfigurationSetSendingEnabledRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "Enabled" :: (Enabled)
  }
derive instance newtypeUpdateConfigurationSetSendingEnabledRequest :: Newtype UpdateConfigurationSetSendingEnabledRequest _
derive instance repGenericUpdateConfigurationSetSendingEnabledRequest :: Generic UpdateConfigurationSetSendingEnabledRequest _
instance showUpdateConfigurationSetSendingEnabledRequest :: Show UpdateConfigurationSetSendingEnabledRequest where
  show = genericShow
instance decodeUpdateConfigurationSetSendingEnabledRequest :: Decode UpdateConfigurationSetSendingEnabledRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetSendingEnabledRequest :: Encode UpdateConfigurationSetSendingEnabledRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to update the tracking options for a configuration set. </p>
newtype UpdateConfigurationSetTrackingOptionsRequest = UpdateConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "TrackingOptions" :: (TrackingOptions)
  }
derive instance newtypeUpdateConfigurationSetTrackingOptionsRequest :: Newtype UpdateConfigurationSetTrackingOptionsRequest _
derive instance repGenericUpdateConfigurationSetTrackingOptionsRequest :: Generic UpdateConfigurationSetTrackingOptionsRequest _
instance showUpdateConfigurationSetTrackingOptionsRequest :: Show UpdateConfigurationSetTrackingOptionsRequest where
  show = genericShow
instance decodeUpdateConfigurationSetTrackingOptionsRequest :: Decode UpdateConfigurationSetTrackingOptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetTrackingOptionsRequest :: Encode UpdateConfigurationSetTrackingOptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse Types.NoArguments
derive instance newtypeUpdateConfigurationSetTrackingOptionsResponse :: Newtype UpdateConfigurationSetTrackingOptionsResponse _
derive instance repGenericUpdateConfigurationSetTrackingOptionsResponse :: Generic UpdateConfigurationSetTrackingOptionsResponse _
instance showUpdateConfigurationSetTrackingOptionsResponse :: Show UpdateConfigurationSetTrackingOptionsResponse where
  show = genericShow
instance decodeUpdateConfigurationSetTrackingOptionsResponse :: Decode UpdateConfigurationSetTrackingOptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationSetTrackingOptionsResponse :: Encode UpdateConfigurationSetTrackingOptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to update an existing custom verification email template.</p>
newtype UpdateCustomVerificationEmailTemplateRequest = UpdateCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined.NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined.NullOrUndefined (Subject)
  , "TemplateContent" :: NullOrUndefined.NullOrUndefined (TemplateContent)
  , "SuccessRedirectionURL" :: NullOrUndefined.NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined.NullOrUndefined (FailureRedirectionURL)
  }
derive instance newtypeUpdateCustomVerificationEmailTemplateRequest :: Newtype UpdateCustomVerificationEmailTemplateRequest _
derive instance repGenericUpdateCustomVerificationEmailTemplateRequest :: Generic UpdateCustomVerificationEmailTemplateRequest _
instance showUpdateCustomVerificationEmailTemplateRequest :: Show UpdateCustomVerificationEmailTemplateRequest where
  show = genericShow
instance decodeUpdateCustomVerificationEmailTemplateRequest :: Decode UpdateCustomVerificationEmailTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCustomVerificationEmailTemplateRequest :: Encode UpdateCustomVerificationEmailTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to update a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype UpdateReceiptRuleRequest = UpdateReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "Rule" :: (ReceiptRule)
  }
derive instance newtypeUpdateReceiptRuleRequest :: Newtype UpdateReceiptRuleRequest _
derive instance repGenericUpdateReceiptRuleRequest :: Generic UpdateReceiptRuleRequest _
instance showUpdateReceiptRuleRequest :: Show UpdateReceiptRuleRequest where
  show = genericShow
instance decodeUpdateReceiptRuleRequest :: Decode UpdateReceiptRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateReceiptRuleRequest :: Encode UpdateReceiptRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateReceiptRuleResponse = UpdateReceiptRuleResponse Types.NoArguments
derive instance newtypeUpdateReceiptRuleResponse :: Newtype UpdateReceiptRuleResponse _
derive instance repGenericUpdateReceiptRuleResponse :: Generic UpdateReceiptRuleResponse _
instance showUpdateReceiptRuleResponse :: Show UpdateReceiptRuleResponse where
  show = genericShow
instance decodeUpdateReceiptRuleResponse :: Decode UpdateReceiptRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateReceiptRuleResponse :: Encode UpdateReceiptRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTemplateRequest = UpdateTemplateRequest 
  { "Template" :: (Template)
  }
derive instance newtypeUpdateTemplateRequest :: Newtype UpdateTemplateRequest _
derive instance repGenericUpdateTemplateRequest :: Generic UpdateTemplateRequest _
instance showUpdateTemplateRequest :: Show UpdateTemplateRequest where
  show = genericShow
instance decodeUpdateTemplateRequest :: Decode UpdateTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTemplateRequest :: Encode UpdateTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTemplateResponse = UpdateTemplateResponse Types.NoArguments
derive instance newtypeUpdateTemplateResponse :: Newtype UpdateTemplateResponse _
derive instance repGenericUpdateTemplateResponse :: Generic UpdateTemplateResponse _
instance showUpdateTemplateResponse :: Show UpdateTemplateResponse where
  show = genericShow
instance decodeUpdateTemplateResponse :: Decode UpdateTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTemplateResponse :: Encode UpdateTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerificationAttributes = VerificationAttributes (StrMap.StrMap IdentityVerificationAttributes)
derive instance newtypeVerificationAttributes :: Newtype VerificationAttributes _
derive instance repGenericVerificationAttributes :: Generic VerificationAttributes _
instance showVerificationAttributes :: Show VerificationAttributes where
  show = genericShow
instance decodeVerificationAttributes :: Decode VerificationAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerificationAttributes :: Encode VerificationAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerificationStatus = VerificationStatus String
derive instance newtypeVerificationStatus :: Newtype VerificationStatus _
derive instance repGenericVerificationStatus :: Generic VerificationStatus _
instance showVerificationStatus :: Show VerificationStatus where
  show = genericShow
instance decodeVerificationStatus :: Decode VerificationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerificationStatus :: Encode VerificationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerificationToken = VerificationToken String
derive instance newtypeVerificationToken :: Newtype VerificationToken _
derive instance repGenericVerificationToken :: Generic VerificationToken _
instance showVerificationToken :: Show VerificationToken where
  show = genericShow
instance decodeVerificationToken :: Decode VerificationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerificationToken :: Encode VerificationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerificationTokenList = VerificationTokenList (Array VerificationToken)
derive instance newtypeVerificationTokenList :: Newtype VerificationTokenList _
derive instance repGenericVerificationTokenList :: Generic VerificationTokenList _
instance showVerificationTokenList :: Show VerificationTokenList where
  show = genericShow
instance decodeVerificationTokenList :: Decode VerificationTokenList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerificationTokenList :: Encode VerificationTokenList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to generate the CNAME records needed to set up Easy DKIM with Amazon SES. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyDomainDkimRequest = VerifyDomainDkimRequest 
  { "Domain" :: (Domain)
  }
derive instance newtypeVerifyDomainDkimRequest :: Newtype VerifyDomainDkimRequest _
derive instance repGenericVerifyDomainDkimRequest :: Generic VerifyDomainDkimRequest _
instance showVerifyDomainDkimRequest :: Show VerifyDomainDkimRequest where
  show = genericShow
instance decodeVerifyDomainDkimRequest :: Decode VerifyDomainDkimRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyDomainDkimRequest :: Encode VerifyDomainDkimRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns CNAME records that you must publish to the DNS server of your domain to set up Easy DKIM with Amazon SES.</p>
newtype VerifyDomainDkimResponse = VerifyDomainDkimResponse 
  { "DkimTokens" :: (VerificationTokenList)
  }
derive instance newtypeVerifyDomainDkimResponse :: Newtype VerifyDomainDkimResponse _
derive instance repGenericVerifyDomainDkimResponse :: Generic VerifyDomainDkimResponse _
instance showVerifyDomainDkimResponse :: Show VerifyDomainDkimResponse where
  show = genericShow
instance decodeVerifyDomainDkimResponse :: Decode VerifyDomainDkimResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyDomainDkimResponse :: Encode VerifyDomainDkimResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyDomainIdentityRequest = VerifyDomainIdentityRequest 
  { "Domain" :: (Domain)
  }
derive instance newtypeVerifyDomainIdentityRequest :: Newtype VerifyDomainIdentityRequest _
derive instance repGenericVerifyDomainIdentityRequest :: Generic VerifyDomainIdentityRequest _
instance showVerifyDomainIdentityRequest :: Show VerifyDomainIdentityRequest where
  show = genericShow
instance decodeVerifyDomainIdentityRequest :: Decode VerifyDomainIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyDomainIdentityRequest :: Encode VerifyDomainIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.</p>
newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse 
  { "VerificationToken" :: (VerificationToken)
  }
derive instance newtypeVerifyDomainIdentityResponse :: Newtype VerifyDomainIdentityResponse _
derive instance repGenericVerifyDomainIdentityResponse :: Generic VerifyDomainIdentityResponse _
instance showVerifyDomainIdentityResponse :: Show VerifyDomainIdentityResponse where
  show = genericShow
instance decodeVerifyDomainIdentityResponse :: Decode VerifyDomainIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyDomainIdentityResponse :: Encode VerifyDomainIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyEmailAddressRequest = VerifyEmailAddressRequest 
  { "EmailAddress" :: (Address)
  }
derive instance newtypeVerifyEmailAddressRequest :: Newtype VerifyEmailAddressRequest _
derive instance repGenericVerifyEmailAddressRequest :: Generic VerifyEmailAddressRequest _
instance showVerifyEmailAddressRequest :: Show VerifyEmailAddressRequest where
  show = genericShow
instance decodeVerifyEmailAddressRequest :: Decode VerifyEmailAddressRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyEmailAddressRequest :: Encode VerifyEmailAddressRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyEmailIdentityRequest = VerifyEmailIdentityRequest 
  { "EmailAddress" :: (Address)
  }
derive instance newtypeVerifyEmailIdentityRequest :: Newtype VerifyEmailIdentityRequest _
derive instance repGenericVerifyEmailIdentityRequest :: Generic VerifyEmailIdentityRequest _
instance showVerifyEmailIdentityRequest :: Show VerifyEmailIdentityRequest where
  show = genericShow
instance decodeVerifyEmailIdentityRequest :: Decode VerifyEmailIdentityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyEmailIdentityRequest :: Encode VerifyEmailIdentityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element returned on a successful request.</p>
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse Types.NoArguments
derive instance newtypeVerifyEmailIdentityResponse :: Newtype VerifyEmailIdentityResponse _
derive instance repGenericVerifyEmailIdentityResponse :: Generic VerifyEmailIdentityResponse _
instance showVerifyEmailIdentityResponse :: Show VerifyEmailIdentityResponse where
  show = genericShow
instance decodeVerifyEmailIdentityResponse :: Decode VerifyEmailIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyEmailIdentityResponse :: Encode VerifyEmailIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.</p> <p>For information using a receipt rule to call Amazon WorkMail, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html">Amazon SES Developer Guide</a>.</p>
newtype WorkmailAction = WorkmailAction 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (AmazonResourceName)
  , "OrganizationArn" :: (AmazonResourceName)
  }
derive instance newtypeWorkmailAction :: Newtype WorkmailAction _
derive instance repGenericWorkmailAction :: Generic WorkmailAction _
instance showWorkmailAction :: Show WorkmailAction where
  show = genericShow
instance decodeWorkmailAction :: Decode WorkmailAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkmailAction :: Encode WorkmailAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
