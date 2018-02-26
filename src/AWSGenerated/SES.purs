

-- | <fullname>Amazon Simple Email Service</fullname> <p> This is the API Reference for <a href="https://aws.amazon.com/ses/">Amazon Simple Email Service</a> (Amazon SES). This documentation is intended to be used in conjunction with the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> <note> <p> For a list of Amazon SES endpoints to use in service requests, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html">Regions and Amazon SES</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> </note>
module AWS.SES where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SES" :: String


-- | <p>Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.</p> <p>For information about setting up rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
cloneReceiptRuleSet :: forall eff. CloneReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) CloneReceiptRuleSetResponse
cloneReceiptRuleSet = AWS.request serviceName "CloneReceiptRuleSet" 


-- | <p>Creates a configuration set.</p> <p>Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createConfigurationSet :: forall eff. CreateConfigurationSetRequest -> Aff (err :: AWS.RequestError | eff) CreateConfigurationSetResponse
createConfigurationSet = AWS.request serviceName "CreateConfigurationSet" 


-- | <p>Creates a configuration set event destination.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createConfigurationSetEventDestination :: forall eff. CreateConfigurationSetEventDestinationRequest -> Aff (err :: AWS.RequestError | eff) CreateConfigurationSetEventDestinationResponse
createConfigurationSetEventDestination = AWS.request serviceName "CreateConfigurationSetEventDestination" 


-- | <p>Creates an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
createConfigurationSetTrackingOptions :: forall eff. CreateConfigurationSetTrackingOptionsRequest -> Aff (err :: AWS.RequestError | eff) CreateConfigurationSetTrackingOptionsResponse
createConfigurationSetTrackingOptions = AWS.request serviceName "CreateConfigurationSetTrackingOptions" 


-- | <p>Creates a new custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
createCustomVerificationEmailTemplate :: forall eff. CreateCustomVerificationEmailTemplateRequest -> Aff (err :: AWS.RequestError | eff) Unit
createCustomVerificationEmailTemplate = AWS.request serviceName "CreateCustomVerificationEmailTemplate" 


-- | <p>Creates a new IP address filter.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptFilter :: forall eff. CreateReceiptFilterRequest -> Aff (err :: AWS.RequestError | eff) CreateReceiptFilterResponse
createReceiptFilter = AWS.request serviceName "CreateReceiptFilter" 


-- | <p>Creates a receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptRule :: forall eff. CreateReceiptRuleRequest -> Aff (err :: AWS.RequestError | eff) CreateReceiptRuleResponse
createReceiptRule = AWS.request serviceName "CreateReceiptRule" 


-- | <p>Creates an empty receipt rule set.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createReceiptRuleSet :: forall eff. CreateReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) CreateReceiptRuleSetResponse
createReceiptRuleSet = AWS.request serviceName "CreateReceiptRuleSet" 


-- | <p>Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
createTemplate :: forall eff. CreateTemplateRequest -> Aff (err :: AWS.RequestError | eff) CreateTemplateResponse
createTemplate = AWS.request serviceName "CreateTemplate" 


-- | <p>Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteConfigurationSet :: forall eff. DeleteConfigurationSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteConfigurationSetResponse
deleteConfigurationSet = AWS.request serviceName "DeleteConfigurationSet" 


-- | <p>Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteConfigurationSetEventDestination :: forall eff. DeleteConfigurationSetEventDestinationRequest -> Aff (err :: AWS.RequestError | eff) DeleteConfigurationSetEventDestinationResponse
deleteConfigurationSetEventDestination = AWS.request serviceName "DeleteConfigurationSetEventDestination" 


-- | <p>Deletes an association between a configuration set and a custom domain for open and click event tracking.</p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p> <note> <p>Deleting this kind of association will result in emails sent using the specified configuration set to capture open and click events using the standard, Amazon SES-operated domains.</p> </note>
deleteConfigurationSetTrackingOptions :: forall eff. DeleteConfigurationSetTrackingOptionsRequest -> Aff (err :: AWS.RequestError | eff) DeleteConfigurationSetTrackingOptionsResponse
deleteConfigurationSetTrackingOptions = AWS.request serviceName "DeleteConfigurationSetTrackingOptions" 


-- | <p>Deletes an existing custom verification email template. </p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
deleteCustomVerificationEmailTemplate :: forall eff. DeleteCustomVerificationEmailTemplateRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteCustomVerificationEmailTemplate = AWS.request serviceName "DeleteCustomVerificationEmailTemplate" 


-- | <p>Deletes the specified identity (an email address or a domain) from the list of verified identities.</p> <p>You can execute this operation no more than once per second.</p>
deleteIdentity :: forall eff. DeleteIdentityRequest -> Aff (err :: AWS.RequestError | eff) DeleteIdentityResponse
deleteIdentity = AWS.request serviceName "DeleteIdentity" 


-- | <p>Deletes the specified sending authorization policy for the given identity (an email address or a domain). This API returns successfully even if a policy with the specified name does not exist.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteIdentityPolicy :: forall eff. DeleteIdentityPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeleteIdentityPolicyResponse
deleteIdentityPolicy = AWS.request serviceName "DeleteIdentityPolicy" 


-- | <p>Deletes the specified IP address filter.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptFilter :: forall eff. DeleteReceiptFilterRequest -> Aff (err :: AWS.RequestError | eff) DeleteReceiptFilterResponse
deleteReceiptFilter = AWS.request serviceName "DeleteReceiptFilter" 


-- | <p>Deletes the specified receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptRule :: forall eff. DeleteReceiptRuleRequest -> Aff (err :: AWS.RequestError | eff) DeleteReceiptRuleResponse
deleteReceiptRule = AWS.request serviceName "DeleteReceiptRule" 


-- | <p>Deletes the specified receipt rule set and all of the receipt rules it contains.</p> <note> <p>The currently active rule set cannot be deleted.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
deleteReceiptRuleSet :: forall eff. DeleteReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteReceiptRuleSetResponse
deleteReceiptRuleSet = AWS.request serviceName "DeleteReceiptRuleSet" 


-- | <p>Deletes an email template.</p> <p>You can execute this operation no more than once per second.</p>
deleteTemplate :: forall eff. DeleteTemplateRequest -> Aff (err :: AWS.RequestError | eff) DeleteTemplateResponse
deleteTemplate = AWS.request serviceName "DeleteTemplate" 


-- | <p>Deprecated. Use the <code>DeleteIdentity</code> operation to delete email addresses and domains.</p>
deleteVerifiedEmailAddress :: forall eff. DeleteVerifiedEmailAddressRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteVerifiedEmailAddress = AWS.request serviceName "DeleteVerifiedEmailAddress" 


-- | <p>Returns the metadata and receipt rules for the receipt rule set that is currently active.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeActiveReceiptRuleSet :: forall eff. DescribeActiveReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) DescribeActiveReceiptRuleSetResponse
describeActiveReceiptRuleSet = AWS.request serviceName "DescribeActiveReceiptRuleSet" 


-- | <p>Returns the details of the specified configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeConfigurationSet :: forall eff. DescribeConfigurationSetRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationSetResponse
describeConfigurationSet = AWS.request serviceName "DescribeConfigurationSet" 


-- | <p>Returns the details of the specified receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeReceiptRule :: forall eff. DescribeReceiptRuleRequest -> Aff (err :: AWS.RequestError | eff) DescribeReceiptRuleResponse
describeReceiptRule = AWS.request serviceName "DescribeReceiptRule" 


-- | <p>Returns the details of the specified receipt rule set.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
describeReceiptRuleSet :: forall eff. DescribeReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) DescribeReceiptRuleSetResponse
describeReceiptRuleSet = AWS.request serviceName "DescribeReceiptRuleSet" 


-- | <p>Returns the email sending status of the Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>
getAccountSendingEnabled :: forall eff.  Aff (err :: AWS.RequestError | eff) GetAccountSendingEnabledResponse
getAccountSendingEnabled = AWS.request serviceName "GetAccountSendingEnabled" unit


-- | <p>Returns the custom email verification template for the template name you specify.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
getCustomVerificationEmailTemplate :: forall eff. GetCustomVerificationEmailTemplateRequest -> Aff (err :: AWS.RequestError | eff) GetCustomVerificationEmailTemplateResponse
getCustomVerificationEmailTemplate = AWS.request serviceName "GetCustomVerificationEmailTemplate" 


-- | <p>Returns the current status of Easy DKIM signing for an entity. For domain name identities, this operation also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES has successfully verified that these tokens have been published.</p> <p>This operation takes a list of identities as input and returns the following information for each:</p> <ul> <li> <p>Whether Easy DKIM signing is enabled or disabled.</p> </li> <li> <p>A set of DKIM tokens that represent the identity. If the identity is an email address, the tokens represent the domain of that address.</p> </li> <li> <p>Whether Amazon SES has successfully verified the DKIM tokens published in the domain's DNS. This information is only returned for domain name identities, not for email addresses.</p> </li> </ul> <p>This operation is throttled at one request per second and can only get DKIM attributes for up to 100 identities at a time.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>
getIdentityDkimAttributes :: forall eff. GetIdentityDkimAttributesRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityDkimAttributesResponse
getIdentityDkimAttributes = AWS.request serviceName "GetIdentityDkimAttributes" 


-- | <p>Returns the custom MAIL FROM attributes for a list of identities (email addresses : domains).</p> <p>This operation is throttled at one request per second and can only get custom MAIL FROM attributes for up to 100 identities at a time.</p>
getIdentityMailFromDomainAttributes :: forall eff. GetIdentityMailFromDomainAttributesRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityMailFromDomainAttributesResponse
getIdentityMailFromDomainAttributes = AWS.request serviceName "GetIdentityMailFromDomainAttributes" 


-- | <p>Given a list of verified identities (email addresses and/or domains), returns a structure describing identity notification attributes.</p> <p>This operation is throttled at one request per second and can only get notification attributes for up to 100 identities at a time.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
getIdentityNotificationAttributes :: forall eff. GetIdentityNotificationAttributesRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributes = AWS.request serviceName "GetIdentityNotificationAttributes" 


-- | <p>Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
getIdentityPolicies :: forall eff. GetIdentityPoliciesRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityPoliciesResponse
getIdentityPolicies = AWS.request serviceName "GetIdentityPolicies" 


-- | <p>Given a list of identities (email addresses and/or domains), returns the verification status and (for domain identities) the verification token for each identity.</p> <p>The verification status of an email address is "Pending" until the email address owner clicks the link within the verification email that Amazon SES sent to that address. If the email address owner clicks the link within 24 hours, the verification status of the email address changes to "Success". If the link is not clicked within 24 hours, the verification status changes to "Failed." In that case, if you still want to verify the email address, you must restart the verification process from the beginning.</p> <p>For domain identities, the domain's verification status is "Pending" as Amazon SES searches for the required TXT record in the DNS settings of the domain. When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.</p> <p>This operation is throttled at one request per second and can only get verification attributes for up to 100 identities at a time.</p>
getIdentityVerificationAttributes :: forall eff. GetIdentityVerificationAttributesRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributes = AWS.request serviceName "GetIdentityVerificationAttributes" 


-- | <p>Provides the sending limits for the Amazon SES account. </p> <p>You can execute this operation no more than once per second.</p>
getSendQuota :: forall eff.  Aff (err :: AWS.RequestError | eff) GetSendQuotaResponse
getSendQuota = AWS.request serviceName "GetSendQuota" unit


-- | <p>Provides sending statistics for the Amazon SES account. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.</p> <p>You can execute this operation no more than once per second.</p>
getSendStatistics :: forall eff.  Aff (err :: AWS.RequestError | eff) GetSendStatisticsResponse
getSendStatistics = AWS.request serviceName "GetSendStatistics" unit


-- | <p>Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.</p> <p>You can execute this operation no more than once per second.</p>
getTemplate :: forall eff. GetTemplateRequest -> Aff (err :: AWS.RequestError | eff) GetTemplateResponse
getTemplate = AWS.request serviceName "GetTemplate" 


-- | <p>Provides a list of the configuration sets associated with your Amazon SES account. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the <code>ListConfigurationSets</code> operation again, passing the <code>NextToken</code> parameter and the value of the NextToken element to retrieve additional results.</p>
listConfigurationSets :: forall eff. ListConfigurationSetsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationSetsResponse
listConfigurationSets = AWS.request serviceName "ListConfigurationSets" 


-- | <p>Lists the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
listCustomVerificationEmailTemplates :: forall eff. ListCustomVerificationEmailTemplatesRequest -> Aff (err :: AWS.RequestError | eff) ListCustomVerificationEmailTemplatesResponse
listCustomVerificationEmailTemplates = AWS.request serviceName "ListCustomVerificationEmailTemplates" 


-- | <p>Returns a list containing all of the identities (email addresses and domains) for your AWS account, regardless of verification status.</p> <p>You can execute this operation no more than once per second.</p>
listIdentities :: forall eff. ListIdentitiesRequest -> Aff (err :: AWS.RequestError | eff) ListIdentitiesResponse
listIdentities = AWS.request serviceName "ListIdentities" 


-- | <p>Returns a list of sending authorization policies that are attached to the given identity (an email address or a domain). This API returns only a list. If you want the actual policy content, you can use <code>GetIdentityPolicies</code>.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listIdentityPolicies :: forall eff. ListIdentityPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListIdentityPoliciesResponse
listIdentityPolicies = AWS.request serviceName "ListIdentityPolicies" 


-- | <p>Lists the IP address filters associated with your AWS account.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listReceiptFilters :: forall eff. ListReceiptFiltersRequest -> Aff (err :: AWS.RequestError | eff) ListReceiptFiltersResponse
listReceiptFilters = AWS.request serviceName "ListReceiptFilters" 


-- | <p>Lists the receipt rule sets that exist under your AWS account. If there are additional receipt rule sets to be retrieved, you will receive a <code>NextToken</code> that you can provide to the next call to <code>ListReceiptRuleSets</code> to retrieve the additional entries.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
listReceiptRuleSets :: forall eff. ListReceiptRuleSetsRequest -> Aff (err :: AWS.RequestError | eff) ListReceiptRuleSetsResponse
listReceiptRuleSets = AWS.request serviceName "ListReceiptRuleSets" 


-- | <p>Lists the email templates present in your Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>
listTemplates :: forall eff. ListTemplatesRequest -> Aff (err :: AWS.RequestError | eff) ListTemplatesResponse
listTemplates = AWS.request serviceName "ListTemplates" 


-- | <p>Deprecated. Use the <code>ListIdentities</code> operation to list the email addresses and domains associated with your account.</p>
listVerifiedEmailAddresses :: forall eff.  Aff (err :: AWS.RequestError | eff) ListVerifiedEmailAddressesResponse
listVerifiedEmailAddresses = AWS.request serviceName "ListVerifiedEmailAddresses" unit


-- | <p>Adds or updates a sending authorization policy for the specified identity (an email address or a domain).</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
putIdentityPolicy :: forall eff. PutIdentityPolicyRequest -> Aff (err :: AWS.RequestError | eff) PutIdentityPolicyResponse
putIdentityPolicy = AWS.request serviceName "PutIdentityPolicy" 


-- | <p>Reorders the receipt rules within a receipt rule set.</p> <note> <p>All of the rules in the rule set must be represented in this request. That is, this API will return an error if the reorder request doesn't explicitly position all of the rules.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
reorderReceiptRuleSet :: forall eff. ReorderReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) ReorderReceiptRuleSetResponse
reorderReceiptRuleSet = AWS.request serviceName "ReorderReceiptRuleSet" 


-- | <p>Generates and sends a bounce message to the sender of an email you received through Amazon SES. You can only use this API on an email up to 24 hours after you receive it.</p> <note> <p>You cannot use this API to send generic bounces for mail that was not received by Amazon SES.</p> </note> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
sendBounce :: forall eff. SendBounceRequest -> Aff (err :: AWS.RequestError | eff) SendBounceResponse
sendBounce = AWS.request serviceName "SendBounce" 


-- | <p>Composes an email message to multiple destinations. The message body is created using an email template.</p> <p>In order to send email using the <code>SendBulkTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Each <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>
sendBulkTemplatedEmail :: forall eff. SendBulkTemplatedEmailRequest -> Aff (err :: AWS.RequestError | eff) SendBulkTemplatedEmailResponse
sendBulkTemplatedEmail = AWS.request serviceName "SendBulkTemplatedEmail" 


-- | <p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a customized verification email is sent to the specified address.</p> <p>To use this operation, you must first create a custom verification email template. For more information about creating and using custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
sendCustomVerificationEmail :: forall eff. SendCustomVerificationEmailRequest -> Aff (err :: AWS.RequestError | eff) SendCustomVerificationEmailResponse
sendCustomVerificationEmail = AWS.request serviceName "SendCustomVerificationEmail" 


-- | <p>Composes an email message and immediately queues it for sending. In order to send email using the <code>SendEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important>
sendEmail :: forall eff. SendEmailRequest -> Aff (err :: AWS.RequestError | eff) SendEmailResponse
sendEmail = AWS.request serviceName "SendEmail" 


-- | <p>Composes an email message and immediately queues it for sending. When calling this operation, you may specify the message headers as well as the content. The <code>SendRawEmail</code> operation is particularly useful for sending multipart MIME emails (such as those that contain both a plain-text and an HTML version). </p> <p>In order to send email using the <code>SendRawEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendRawEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important> <p>Additionally, keep the following considerations in mind when using the <code>SendRawEmail</code> operation:</p> <ul> <li> <p>Although you can customize the message headers when using the <code>SendRawEmail</code> operation, Amazon SES will automatically apply its own <code>Message-ID</code> and <code>Date</code> headers; if you passed these headers when creating the message, they will be overwritten by the values that Amazon SES provides.</p> </li> <li> <p>If you are using sending authorization to send on behalf of another user, <code>SendRawEmail</code> enables you to specify the cross-account identity for the email's Source, From, and Return-Path parameters in one of two ways: you can pass optional parameters <code>SourceArn</code>, <code>FromArn</code>, and/or <code>ReturnPathArn</code> to the API, or you can include the following X-headers in the header of your raw email:</p> <ul> <li> <p> <code>X-SES-SOURCE-ARN</code> </p> </li> <li> <p> <code>X-SES-FROM-ARN</code> </p> </li> <li> <p> <code>X-SES-RETURN-PATH-ARN</code> </p> </li> </ul> <important> <p>Do not include these X-headers in the DKIM signature; Amazon SES will remove them before sending the email.</p> </important> <p>For most common sending authorization scenarios, we recommend that you specify the <code>SourceIdentityArn</code> parameter and not the <code>FromIdentityArn</code> or <code>ReturnPathIdentityArn</code> parameters. If you only specify the <code>SourceIdentityArn</code> parameter, Amazon SES will set the From and Return Path addresses to the identity specified in <code>SourceIdentityArn</code>. For more information about sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Using Sending Authorization with Amazon SES</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> </ul>
sendRawEmail :: forall eff. SendRawEmailRequest -> Aff (err :: AWS.RequestError | eff) SendRawEmailResponse
sendRawEmail = AWS.request serviceName "SendRawEmail" 


-- | <p>Composes an email message using an email template and immediately queues it for sending.</p> <p>In order to send email using the <code>SendTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Calls to the <code>SendTemplatedEmail</code> operation may only include one <code>Destination</code> parameter. A destination is a set of recipients who will receive the same version of the email. The <code>Destination</code> parameter can include up to 50 recipients, across the To:, CC: and BCC: fields.</p> </li> <li> <p>The <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>
sendTemplatedEmail :: forall eff. SendTemplatedEmailRequest -> Aff (err :: AWS.RequestError | eff) SendTemplatedEmailResponse
sendTemplatedEmail = AWS.request serviceName "SendTemplatedEmail" 


-- | <p>Sets the specified receipt rule set as the active receipt rule set.</p> <note> <p>To disable your email-receiving through Amazon SES completely, you can call this API with RuleSetName set to null.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
setActiveReceiptRuleSet :: forall eff. SetActiveReceiptRuleSetRequest -> Aff (err :: AWS.RequestError | eff) SetActiveReceiptRuleSetResponse
setActiveReceiptRuleSet = AWS.request serviceName "SetActiveReceiptRuleSet" 


-- | <p>Enables or disables Easy DKIM signing of email sent from an identity:</p> <ul> <li> <p>If Easy DKIM signing is enabled for a domain name identity (such as <code>example.com</code>), then Amazon SES will DKIM-sign all email sent by addresses under that domain name (for example, <code>user@example.com</code>).</p> </li> <li> <p>If Easy DKIM signing is enabled for an email address, then Amazon SES will DKIM-sign all email sent by that email address.</p> </li> </ul> <p>For email addresses (for example, <code>user@example.com</code>), you can only enable Easy DKIM signing if the corresponding domain (in this case, <code>example.com</code>) has been set up for Easy DKIM using the AWS Console or the <code>VerifyDomainDkim</code> operation.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about Easy DKIM signing, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
setIdentityDkimEnabled :: forall eff. SetIdentityDkimEnabledRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityDkimEnabledResponse
setIdentityDkimEnabled = AWS.request serviceName "SetIdentityDkimEnabled" 


-- | <p>Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.</p> <note> <p>Feedback forwarding does not apply to delivery notifications. Delivery notifications are only available through Amazon SNS.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityFeedbackForwardingEnabled :: forall eff. SetIdentityFeedbackForwardingEnabledRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabled = AWS.request serviceName "SetIdentityFeedbackForwardingEnabled" 


-- | <p>Given an identity (an email address or a domain), sets whether Amazon SES includes the original email headers in the Amazon Simple Notification Service (Amazon SNS) notifications of a specified type.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityHeadersInNotificationsEnabled :: forall eff. SetIdentityHeadersInNotificationsEnabledRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityHeadersInNotificationsEnabledResponse
setIdentityHeadersInNotificationsEnabled = AWS.request serviceName "SetIdentityHeadersInNotificationsEnabled" 


-- | <p>Enables or disables the custom MAIL FROM domain setup for a verified identity (an email address or a domain).</p> <important> <p>To send emails using the specified MAIL FROM domain, you must add an MX record to your MAIL FROM domain's DNS settings. If you want your emails to pass Sender Policy Framework (SPF) checks, you must also add or update an SPF record. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html">Amazon SES Developer Guide</a>.</p> </important> <p>You can execute this operation no more than once per second.</p>
setIdentityMailFromDomain :: forall eff. SetIdentityMailFromDomainRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityMailFromDomainResponse
setIdentityMailFromDomain = AWS.request serviceName "SetIdentityMailFromDomain" 


-- | <p>Given an identity (an email address or a domain), sets the Amazon Simple Notification Service (Amazon SNS) topic to which Amazon SES will publish bounce, complaint, and/or delivery notifications for emails sent with that identity as the <code>Source</code>.</p> <note> <p>Unless feedback forwarding is enabled, you must specify Amazon SNS topics for bounce and complaint notifications. For more information, see <code>SetIdentityFeedbackForwardingEnabled</code>.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about feedback notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
setIdentityNotificationTopic :: forall eff. SetIdentityNotificationTopicRequest -> Aff (err :: AWS.RequestError | eff) SetIdentityNotificationTopicResponse
setIdentityNotificationTopic = AWS.request serviceName "SetIdentityNotificationTopic" 


-- | <p>Sets the position of the specified receipt rule in the receipt rule set.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
setReceiptRulePosition :: forall eff. SetReceiptRulePositionRequest -> Aff (err :: AWS.RequestError | eff) SetReceiptRulePositionResponse
setReceiptRulePosition = AWS.request serviceName "SetReceiptRulePosition" 


-- | <p>Creates a preview of the MIME content of an email when provided with a template and a set of replacement data.</p> <p>You can execute this operation no more than once per second.</p>
testRenderTemplate :: forall eff. TestRenderTemplateRequest -> Aff (err :: AWS.RequestError | eff) TestRenderTemplateResponse
testRenderTemplate = AWS.request serviceName "TestRenderTemplate" 


-- | <p>Enables or disables email sending across your entire Amazon SES account. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending across your Amazon SES account when reputation metrics (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>
updateAccountSendingEnabled :: forall eff. UpdateAccountSendingEnabledRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateAccountSendingEnabled = AWS.request serviceName "UpdateAccountSendingEnabled" 


-- | <p>Updates the event destination of a configuration set. Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetEventDestination :: forall eff. UpdateConfigurationSetEventDestinationRequest -> Aff (err :: AWS.RequestError | eff) UpdateConfigurationSetEventDestinationResponse
updateConfigurationSetEventDestination = AWS.request serviceName "UpdateConfigurationSetEventDestination" 


-- | <p>Enables or disables the publishing of reputation metrics for emails sent using a specific configuration set. Reputation metrics include bounce and complaint rates. These metrics are published to Amazon CloudWatch. By using Amazon CloudWatch, you can create alarms when bounce or complaint rates exceed a certain threshold.</p> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetReputationMetricsEnabled :: forall eff. UpdateConfigurationSetReputationMetricsEnabledRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateConfigurationSetReputationMetricsEnabled = AWS.request serviceName "UpdateConfigurationSetReputationMetricsEnabled" 


-- | <p>Enables or disables email sending for messages sent using a specific configuration set. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>
updateConfigurationSetSendingEnabled :: forall eff. UpdateConfigurationSetSendingEnabledRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateConfigurationSetSendingEnabled = AWS.request serviceName "UpdateConfigurationSetSendingEnabled" 


-- | <p>Modifies an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
updateConfigurationSetTrackingOptions :: forall eff. UpdateConfigurationSetTrackingOptionsRequest -> Aff (err :: AWS.RequestError | eff) UpdateConfigurationSetTrackingOptionsResponse
updateConfigurationSetTrackingOptions = AWS.request serviceName "UpdateConfigurationSetTrackingOptions" 


-- | <p>Updates an existing custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>
updateCustomVerificationEmailTemplate :: forall eff. UpdateCustomVerificationEmailTemplateRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateCustomVerificationEmailTemplate = AWS.request serviceName "UpdateCustomVerificationEmailTemplate" 


-- | <p>Updates a receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
updateReceiptRule :: forall eff. UpdateReceiptRuleRequest -> Aff (err :: AWS.RequestError | eff) UpdateReceiptRuleResponse
updateReceiptRule = AWS.request serviceName "UpdateReceiptRule" 


-- | <p>Updates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>
updateTemplate :: forall eff. UpdateTemplateRequest -> Aff (err :: AWS.RequestError | eff) UpdateTemplateResponse
updateTemplate = AWS.request serviceName "UpdateTemplate" 


-- | <p>Returns a set of DKIM tokens for a domain. DKIM <i>tokens</i> are character strings that represent your domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain.</p> <p>You can execute this operation no more than once per second.</p> <p>To enable or disable Easy DKIM signing for a domain, use the <code>SetIdentityDkimEnabled</code> operation.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>
verifyDomainDkim :: forall eff. VerifyDomainDkimRequest -> Aff (err :: AWS.RequestError | eff) VerifyDomainDkimResponse
verifyDomainDkim = AWS.request serviceName "VerifyDomainDkim" 


-- | <p>Adds a domain to the list of identities for your Amazon SES account and attempts to verify it. For more information about verifying domains, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second.</p>
verifyDomainIdentity :: forall eff. VerifyDomainIdentityRequest -> Aff (err :: AWS.RequestError | eff) VerifyDomainIdentityResponse
verifyDomainIdentity = AWS.request serviceName "VerifyDomainIdentity" 


-- | <p>Deprecated. Use the <code>VerifyEmailIdentity</code> operation to verify a new email address.</p>
verifyEmailAddress :: forall eff. VerifyEmailAddressRequest -> Aff (err :: AWS.RequestError | eff) Unit
verifyEmailAddress = AWS.request serviceName "VerifyEmailAddress" 


-- | <p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a verification email is sent to the specified address.</p> <p>You can execute this operation no more than once per second.</p>
verifyEmailIdentity :: forall eff. VerifyEmailIdentityRequest -> Aff (err :: AWS.RequestError | eff) VerifyEmailIdentityResponse
verifyEmailIdentity = AWS.request serviceName "VerifyEmailIdentity" 


-- | <p>Indicates that email sending is disabled for your entire Amazon SES account.</p> <p>You can enable or disable email sending for your Amazon SES account using <a>UpdateAccountSendingEnabled</a>.</p>
newtype AccountSendingPausedException = AccountSendingPausedException 
  { 
  }


-- | <p>When included in a receipt rule, this action adds a header to the received email.</p> <p>For information about adding a header using a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html">Amazon SES Developer Guide</a>.</p>
newtype AddHeaderAction = AddHeaderAction 
  { "HeaderName" :: (HeaderName)
  , "HeaderValue" :: (HeaderValue)
  }


newtype Address = Address String


newtype AddressList = AddressList (Array Address)


-- | <p>Indicates that a resource could not be created because of a naming conflict.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Name" :: NullOrUndefined (RuleOrRuleSetName)
  }


newtype AmazonResourceName = AmazonResourceName String


newtype ArrivalDate = ArrivalDate Number


newtype BehaviorOnMXFailure = BehaviorOnMXFailure String


-- | <p>Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.</p>
newtype Body = Body 
  { "Text" :: NullOrUndefined (Content)
  , "Html" :: NullOrUndefined (Content)
  }


-- | <p>When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about sending a bounce message in response to a received email, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html">Amazon SES Developer Guide</a>.</p>
newtype BounceAction = BounceAction 
  { "TopicArn" :: NullOrUndefined (AmazonResourceName)
  , "SmtpReplyCode" :: (BounceSmtpReplyCode)
  , "StatusCode" :: NullOrUndefined (BounceStatusCode)
  , "Message" :: (BounceMessage)
  , "Sender" :: (Address)
  }


newtype BounceMessage = BounceMessage String


newtype BounceSmtpReplyCode = BounceSmtpReplyCode String


newtype BounceStatusCode = BounceStatusCode String


newtype BounceType = BounceType String


-- | <p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype BouncedRecipientInfo = BouncedRecipientInfo 
  { "Recipient" :: (Address)
  , "RecipientArn" :: NullOrUndefined (AmazonResourceName)
  , "BounceType" :: NullOrUndefined (BounceType)
  , "RecipientDsnFields" :: NullOrUndefined (RecipientDsnFields)
  }


newtype BouncedRecipientInfoList = BouncedRecipientInfoList (Array BouncedRecipientInfo)


-- | <p>An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.</p>
newtype BulkEmailDestination = BulkEmailDestination 
  { "Destination" :: (Destination)
  , "ReplacementTags" :: NullOrUndefined (MessageTagList)
  , "ReplacementTemplateData" :: NullOrUndefined (TemplateData)
  }


newtype BulkEmailDestinationList = BulkEmailDestinationList (Array BulkEmailDestination)


-- | <p>An object that contains the response from the <code>SendBulkTemplatedEmail</code> operation.</p>
newtype BulkEmailDestinationStatus = BulkEmailDestinationStatus 
  { "Status" :: NullOrUndefined (BulkEmailStatus)
  , "Error" :: NullOrUndefined (Error)
  , "MessageId" :: NullOrUndefined (MessageId)
  }


newtype BulkEmailDestinationStatusList = BulkEmailDestinationStatusList (Array BulkEmailDestinationStatus)


newtype BulkEmailStatus = BulkEmailStatus String


-- | <p>Indicates that the delete operation could not be completed.</p>
newtype CannotDeleteException = CannotDeleteException 
  { "Name" :: NullOrUndefined (RuleOrRuleSetName)
  }


newtype Charset = Charset String


newtype Cidr = Cidr String


-- | <p>Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CloneReceiptRuleSetRequest = CloneReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "OriginalRuleSetName" :: (ReceiptRuleSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CloneReceiptRuleSetResponse = CloneReceiptRuleSetResponse 
  { 
  }


-- | <p>Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.</p> <p>Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CloudWatchDestination = CloudWatchDestination 
  { "DimensionConfigurations" :: (CloudWatchDimensionConfigurations)
  }


-- | <p>Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.</p> <p>For information about publishing email sending events to Amazon CloudWatch, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CloudWatchDimensionConfiguration = CloudWatchDimensionConfiguration 
  { "DimensionName" :: (DimensionName)
  , "DimensionValueSource" :: (DimensionValueSource)
  , "DefaultDimensionValue" :: (DefaultDimensionValue)
  }


newtype CloudWatchDimensionConfigurations = CloudWatchDimensionConfigurations (Array CloudWatchDimensionConfiguration)


-- | <p>The name of the configuration set.</p> <p>Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html">Using Amazon SES Configuration Sets</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/">Amazon SES Developer Guide</a>.</p>
newtype ConfigurationSet = ConfigurationSet 
  { "Name" :: (ConfigurationSetName)
  }


-- | <p>Indicates that the configuration set could not be created because of a naming conflict.</p>
newtype ConfigurationSetAlreadyExistsException = ConfigurationSetAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


newtype ConfigurationSetAttribute = ConfigurationSetAttribute String


newtype ConfigurationSetAttributeList = ConfigurationSetAttributeList (Array ConfigurationSetAttribute)


-- | <p>Indicates that the configuration set does not exist.</p>
newtype ConfigurationSetDoesNotExistException = ConfigurationSetDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


newtype ConfigurationSetName = ConfigurationSetName String


-- | <p>Indicates that email sending is disabled for the configuration set.</p> <p>You can enable or disable email sending for a configuration set using <a>UpdateConfigurationSetSendingEnabled</a>.</p>
newtype ConfigurationSetSendingPausedException = ConfigurationSetSendingPausedException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


newtype ConfigurationSets = ConfigurationSets (Array ConfigurationSet)


-- | <p>Represents textual data, plus an optional character set specification.</p> <p>By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.</p>
newtype Content = Content 
  { "Data" :: (MessageData)
  , "Charset" :: NullOrUndefined (Charset)
  }


newtype Counter = Counter Number


-- | <p>Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CreateConfigurationSetEventDestinationRequest = CreateConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestination" :: (EventDestination)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse 
  { 
  }


-- | <p>Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype CreateConfigurationSetRequest = CreateConfigurationSetRequest 
  { "ConfigurationSet" :: (ConfigurationSet)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetResponse = CreateConfigurationSetResponse 
  { 
  }


-- | <p>Represents a request to create an open and click tracking option object in a configuration set. </p>
newtype CreateConfigurationSetTrackingOptionsRequest = CreateConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "TrackingOptions" :: (TrackingOptions)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateConfigurationSetTrackingOptionsResponse = CreateConfigurationSetTrackingOptionsResponse 
  { 
  }


-- | <p>Represents a request to create a custom verification email template.</p>
newtype CreateCustomVerificationEmailTemplateRequest = CreateCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "FromEmailAddress" :: (FromAddress)
  , "TemplateSubject" :: (Subject)
  , "TemplateContent" :: (TemplateContent)
  , "SuccessRedirectionURL" :: (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: (FailureRedirectionURL)
  }


-- | <p>Represents a request to create a new IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptFilterRequest = CreateReceiptFilterRequest 
  { "Filter" :: (ReceiptFilter)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptFilterResponse = CreateReceiptFilterResponse 
  { 
  }


-- | <p>Represents a request to create a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptRuleRequest = CreateReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "After" :: NullOrUndefined (ReceiptRuleName)
  , "Rule" :: (ReceiptRule)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptRuleResponse = CreateReceiptRuleResponse 
  { 
  }


-- | <p>Represents a request to create an empty receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype CreateReceiptRuleSetRequest = CreateReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype CreateReceiptRuleSetResponse = CreateReceiptRuleSetResponse 
  { 
  }


-- | <p>Represents a request to create an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype CreateTemplateRequest = CreateTemplateRequest 
  { "Template" :: (Template)
  }


newtype CreateTemplateResponse = CreateTemplateResponse 
  { 
  }


newtype CustomMailFromStatus = CustomMailFromStatus String


newtype CustomRedirectDomain = CustomRedirectDomain String


-- | <p>Indicates that custom verification email template provided content is invalid.</p>
newtype CustomVerificationEmailInvalidContentException = CustomVerificationEmailInvalidContentException 
  { 
  }


-- | <p>Contains information about a custom verification email template.</p>
newtype CustomVerificationEmailTemplate = CustomVerificationEmailTemplate 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined (Subject)
  , "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL)
  }


-- | <p>Indicates that a custom verification email template with the name you specified already exists.</p>
newtype CustomVerificationEmailTemplateAlreadyExistsException = CustomVerificationEmailTemplateAlreadyExistsException 
  { "CustomVerificationEmailTemplateName" :: NullOrUndefined (TemplateName)
  }


-- | <p>Indicates that a custom verification email template with the name you specified does not exist.</p>
newtype CustomVerificationEmailTemplateDoesNotExistException = CustomVerificationEmailTemplateDoesNotExistException 
  { "CustomVerificationEmailTemplateName" :: NullOrUndefined (TemplateName)
  }


newtype CustomVerificationEmailTemplates = CustomVerificationEmailTemplates (Array CustomVerificationEmailTemplate)


newtype DefaultDimensionValue = DefaultDimensionValue String


-- | <p>Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteConfigurationSetEventDestinationRequest = DeleteConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestinationName" :: (EventDestinationName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse 
  { 
  }


-- | <p>Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteConfigurationSetRequest = DeleteConfigurationSetRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetResponse = DeleteConfigurationSetResponse 
  { 
  }


-- | <p>Represents a request to delete open and click tracking options in a configuration set. </p>
newtype DeleteConfigurationSetTrackingOptionsRequest = DeleteConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse 
  { 
  }


-- | <p>Represents a request to delete an existing custom verification email template.</p>
newtype DeleteCustomVerificationEmailTemplateRequest = DeleteCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }


-- | <p>Represents a request to delete a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteIdentityPolicyRequest = DeleteIdentityPolicyRequest 
  { "Identity" :: (Identity)
  , "PolicyName" :: (PolicyName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteIdentityPolicyResponse = DeleteIdentityPolicyResponse 
  { 
  }


-- | <p>Represents a request to delete one of your Amazon SES identities (an email address or domain).</p>
newtype DeleteIdentityRequest = DeleteIdentityRequest 
  { "Identity" :: (Identity)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteIdentityResponse = DeleteIdentityResponse 
  { 
  }


-- | <p>Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptFilterRequest = DeleteReceiptFilterRequest 
  { "FilterName" :: (ReceiptFilterName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptFilterResponse = DeleteReceiptFilterResponse 
  { 
  }


-- | <p>Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptRuleRequest = DeleteReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptRuleResponse = DeleteReceiptRuleResponse 
  { 
  }


-- | <p>Represents a request to delete a receipt rule set and all of the receipt rules it contains. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteReceiptRuleSetRequest = DeleteReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype DeleteReceiptRuleSetResponse = DeleteReceiptRuleSetResponse 
  { 
  }


-- | <p>Represents a request to delete an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype DeleteTemplateRequest = DeleteTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }


newtype DeleteTemplateResponse = DeleteTemplateResponse 
  { 
  }


-- | <p>Represents a request to delete an email address from the list of email addresses you have attempted to verify under your AWS account.</p>
newtype DeleteVerifiedEmailAddressRequest = DeleteVerifiedEmailAddressRequest 
  { "EmailAddress" :: (Address)
  }


-- | <p>Represents a request to return the metadata and receipt rules for the receipt rule set that is currently active. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeActiveReceiptRuleSetRequest = DescribeActiveReceiptRuleSetRequest 
  { 
  }


-- | <p>Represents the metadata and receipt rules for the receipt rule set that is currently active.</p>
newtype DescribeActiveReceiptRuleSetResponse = DescribeActiveReceiptRuleSetResponse 
  { "Metadata" :: NullOrUndefined (ReceiptRuleSetMetadata)
  , "Rules" :: NullOrUndefined (ReceiptRulesList)
  }


-- | <p>Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeConfigurationSetRequest = DescribeConfigurationSetRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "ConfigurationSetAttributeNames" :: NullOrUndefined (ConfigurationSetAttributeList)
  }


-- | <p>Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeConfigurationSetResponse = DescribeConfigurationSetResponse 
  { "ConfigurationSet" :: NullOrUndefined (ConfigurationSet)
  , "EventDestinations" :: NullOrUndefined (EventDestinations)
  , "TrackingOptions" :: NullOrUndefined (TrackingOptions)
  , "ReputationOptions" :: NullOrUndefined (ReputationOptions)
  }


-- | <p>Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeReceiptRuleRequest = DescribeReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  }


-- | <p>Represents the details of a receipt rule.</p>
newtype DescribeReceiptRuleResponse = DescribeReceiptRuleResponse 
  { "Rule" :: NullOrUndefined (ReceiptRule)
  }


-- | <p>Represents a request to return the details of a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype DescribeReceiptRuleSetRequest = DescribeReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  }


-- | <p>Represents the details of the specified receipt rule set.</p>
newtype DescribeReceiptRuleSetResponse = DescribeReceiptRuleSetResponse 
  { "Metadata" :: NullOrUndefined (ReceiptRuleSetMetadata)
  , "Rules" :: NullOrUndefined (ReceiptRulesList)
  }


-- | <p>Represents the destination of the message, consisting of To:, CC:, and BCC: fields.</p> <note> <p>Amazon SES does not support the SMTPUTF8 extension, as described in <a href="https://tools.ietf.org/html/rfc6531">RFC6531</a>. For this reason, the <i>local part</i> of a destination email address (the part of the email address that precedes the @ sign) may only contain <a href="https://en.wikipedia.org/wiki/Email_address#Local-part">7-bit ASCII characters</a>. If the <i>domain part</i> of an address (the part after the @ sign) contains non-ASCII characters, they must be encoded using Punycode, as described in <a href="https://tools.ietf.org/html/rfc3492.html">RFC3492</a>.</p> </note>
newtype Destination = Destination 
  { "ToAddresses" :: NullOrUndefined (AddressList)
  , "CcAddresses" :: NullOrUndefined (AddressList)
  , "BccAddresses" :: NullOrUndefined (AddressList)
  }


newtype DiagnosticCode = DiagnosticCode String


newtype DimensionName = DimensionName String


newtype DimensionValueSource = DimensionValueSource String


newtype DkimAttributes = DkimAttributes (Map Identity IdentityDkimAttributes)


newtype Domain = Domain String


newtype DsnAction = DsnAction String


newtype DsnStatus = DsnStatus String


newtype Enabled = Enabled Boolean


newtype Error = Error String


-- | <p>Contains information about the event destination that the specified email sending events will be published to.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype EventDestination = EventDestination 
  { "Name" :: (EventDestinationName)
  , "Enabled" :: NullOrUndefined (Enabled)
  , "MatchingEventTypes" :: (EventTypes)
  , "KinesisFirehoseDestination" :: NullOrUndefined (KinesisFirehoseDestination)
  , "CloudWatchDestination" :: NullOrUndefined (CloudWatchDestination)
  , "SNSDestination" :: NullOrUndefined (SNSDestination)
  }


-- | <p>Indicates that the event destination could not be created because of a naming conflict.</p>
newtype EventDestinationAlreadyExistsException = EventDestinationAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined (EventDestinationName)
  }


-- | <p>Indicates that the event destination does not exist.</p>
newtype EventDestinationDoesNotExistException = EventDestinationDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined (EventDestinationName)
  }


newtype EventDestinationName = EventDestinationName String


newtype EventDestinations = EventDestinations (Array EventDestination)


newtype EventType = EventType String


newtype EventTypes = EventTypes (Array EventType)


newtype Explanation = Explanation String


-- | <p>Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype ExtensionField = ExtensionField 
  { "Name" :: (ExtensionFieldName)
  , "Value" :: (ExtensionFieldValue)
  }


newtype ExtensionFieldList = ExtensionFieldList (Array ExtensionField)


newtype ExtensionFieldName = ExtensionFieldName String


newtype ExtensionFieldValue = ExtensionFieldValue String


newtype FailureRedirectionURL = FailureRedirectionURL String


newtype FromAddress = FromAddress String


-- | <p>Indicates that the sender address specified for a custom verification email is not verified, and is therefore not eligible to send the custom verification email. </p>
newtype FromEmailAddressNotVerifiedException = FromEmailAddressNotVerifiedException 
  { "FromEmailAddress" :: NullOrUndefined (FromAddress)
  }


-- | <p>Represents a request to return the email sending status for your Amazon SES account.</p>
newtype GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse 
  { "Enabled" :: NullOrUndefined (Enabled)
  }


-- | <p>Represents a request to retrieve an existing custom verification email template.</p>
newtype GetCustomVerificationEmailTemplateRequest = GetCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }


-- | <p>The content of the custom verification email template.</p>
newtype GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined (Subject)
  , "TemplateContent" :: NullOrUndefined (TemplateContent)
  , "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL)
  }


-- | <p>Represents a request for the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this request also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published. For more information about Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityDkimAttributesRequest = GetIdentityDkimAttributesRequest 
  { "Identities" :: (IdentityList)
  }


-- | <p>Represents the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this response also contains the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published.</p>
newtype GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse 
  { "DkimAttributes" :: (DkimAttributes)
  }


-- | <p>Represents a request to return the Amazon SES custom MAIL FROM attributes for a list of identities. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityMailFromDomainAttributesRequest = GetIdentityMailFromDomainAttributesRequest 
  { "Identities" :: (IdentityList)
  }


-- | <p>Represents the custom MAIL FROM attributes for a list of identities.</p>
newtype GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse 
  { "MailFromDomainAttributes" :: (MailFromDomainAttributes)
  }


-- | <p>Represents a request to return the notification attributes for a list of identities you verified with Amazon SES. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityNotificationAttributesRequest = GetIdentityNotificationAttributesRequest 
  { "Identities" :: (IdentityList)
  }


-- | <p>Represents the notification attributes for a list of identities.</p>
newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse 
  { "NotificationAttributes" :: (NotificationAttributes)
  }


-- | <p>Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityPoliciesRequest = GetIdentityPoliciesRequest 
  { "Identity" :: (Identity)
  , "PolicyNames" :: (PolicyNameList)
  }


-- | <p>Represents the requested sending authorization policies.</p>
newtype GetIdentityPoliciesResponse = GetIdentityPoliciesResponse 
  { "Policies" :: (PolicyMap)
  }


-- | <p>Represents a request to return the Amazon SES verification status of a list of identities. For domain identities, this request also returns the verification token. For information about verifying identities with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Amazon SES Developer Guide</a>.</p>
newtype GetIdentityVerificationAttributesRequest = GetIdentityVerificationAttributesRequest 
  { "Identities" :: (IdentityList)
  }


-- | <p>The Amazon SES verification status of a list of identities. For domain identities, this response also contains the verification token.</p>
newtype GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse 
  { "VerificationAttributes" :: (VerificationAttributes)
  }


-- | <p>Represents your Amazon SES daily sending quota, maximum send rate, and the number of emails you have sent in the last 24 hours.</p>
newtype GetSendQuotaResponse = GetSendQuotaResponse 
  { "Max24HourSend" :: NullOrUndefined (Max24HourSend)
  , "MaxSendRate" :: NullOrUndefined (MaxSendRate)
  , "SentLast24Hours" :: NullOrUndefined (SentLast24Hours)
  }


-- | <p>Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.</p>
newtype GetSendStatisticsResponse = GetSendStatisticsResponse 
  { "SendDataPoints" :: NullOrUndefined (SendDataPointList)
  }


newtype GetTemplateRequest = GetTemplateRequest 
  { "TemplateName" :: (TemplateName)
  }


newtype GetTemplateResponse = GetTemplateResponse 
  { "Template" :: NullOrUndefined (Template)
  }


newtype HeaderName = HeaderName String


newtype HeaderValue = HeaderValue String


newtype HtmlPart = HtmlPart String


newtype Identity = Identity String


-- | <p>Represents the DKIM attributes of a verified email address or a domain.</p>
newtype IdentityDkimAttributes = IdentityDkimAttributes 
  { "DkimEnabled" :: (Enabled)
  , "DkimVerificationStatus" :: (VerificationStatus)
  , "DkimTokens" :: NullOrUndefined (VerificationTokenList)
  }


newtype IdentityList = IdentityList (Array Identity)


-- | <p>Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).</p>
newtype IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes 
  { "MailFromDomain" :: (MailFromDomainName)
  , "MailFromDomainStatus" :: (CustomMailFromStatus)
  , "BehaviorOnMXFailure" :: (BehaviorOnMXFailure)
  }


-- | <p>Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.</p>
newtype IdentityNotificationAttributes = IdentityNotificationAttributes 
  { "BounceTopic" :: (NotificationTopic)
  , "ComplaintTopic" :: (NotificationTopic)
  , "DeliveryTopic" :: (NotificationTopic)
  , "ForwardingEnabled" :: (Enabled)
  , "HeadersInBounceNotificationsEnabled" :: NullOrUndefined (Enabled)
  , "HeadersInComplaintNotificationsEnabled" :: NullOrUndefined (Enabled)
  , "HeadersInDeliveryNotificationsEnabled" :: NullOrUndefined (Enabled)
  }


newtype IdentityType = IdentityType String


-- | <p>Represents the verification attributes of a single identity.</p>
newtype IdentityVerificationAttributes = IdentityVerificationAttributes 
  { "VerificationStatus" :: (VerificationStatus)
  , "VerificationToken" :: NullOrUndefined (VerificationToken)
  }


-- | <p>Indicates that the Amazon CloudWatch destination is invalid. See the error message for details.</p>
newtype InvalidCloudWatchDestinationException = InvalidCloudWatchDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined (EventDestinationName)
  }


-- | <p>Indicates that the configuration set is invalid. See the error message for details.</p>
newtype InvalidConfigurationSetException = InvalidConfigurationSetException 
  { 
  }


-- | <p>Indicates that the Amazon Kinesis Firehose destination is invalid. See the error message for details.</p>
newtype InvalidFirehoseDestinationException = InvalidFirehoseDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined (EventDestinationName)
  }


-- | <p>Indicates that the provided AWS Lambda function is invalid, or that Amazon SES could not execute the provided function, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidLambdaFunctionException = InvalidLambdaFunctionException 
  { "FunctionArn" :: NullOrUndefined (AmazonResourceName)
  }


-- | <p>Indicates that the provided policy is invalid. Check the error stack for more information about what caused the error.</p>
newtype InvalidPolicyException = InvalidPolicyException 
  { 
  }


-- | <p>Indicates that one or more of the replacement values you provided is invalid. This error may occur when the TemplateData object contains invalid JSON.</p>
newtype InvalidRenderingParameterException = InvalidRenderingParameterException 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  }


-- | <p>Indicates that the provided Amazon S3 bucket or AWS KMS encryption key is invalid, or that Amazon SES could not publish to the bucket, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidS3ConfigurationException = InvalidS3ConfigurationException 
  { "Bucket" :: NullOrUndefined (S3BucketName)
  }


-- | <p>Indicates that the Amazon Simple Notification Service (Amazon SNS) destination is invalid. See the error message for details.</p>
newtype InvalidSNSDestinationException = InvalidSNSDestinationException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "EventDestinationName" :: NullOrUndefined (EventDestinationName)
  }


-- | <p>Indicates that the provided Amazon SNS topic is invalid, or that Amazon SES could not publish to the topic, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>
newtype InvalidSnsTopicException = InvalidSnsTopicException 
  { "Topic" :: NullOrUndefined (AmazonResourceName)
  }


-- | <p>Indicates that a template could not be created because it contained invalid JSON.</p>
newtype InvalidTemplateException = InvalidTemplateException 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  }


-- | <p>Indicates that the custom domain to be used for open and click tracking redirects is invalid. This error appears most often in the following situations:</p> <ul> <li> <p>When the tracking domain you specified is not verified in Amazon SES.</p> </li> <li> <p>When the tracking domain you specified is not a valid domain or subdomain.</p> </li> </ul>
newtype InvalidTrackingOptionsException = InvalidTrackingOptionsException 
  { 
  }


newtype InvocationType = InvocationType String


-- | <p>Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.</p> <p>Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype KinesisFirehoseDestination = KinesisFirehoseDestination 
  { "IAMRoleARN" :: (AmazonResourceName)
  , "DeliveryStreamARN" :: (AmazonResourceName)
  }


-- | <p>When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <p>For information about using AWS Lambda actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html">Amazon SES Developer Guide</a>.</p>
newtype LambdaAction = LambdaAction 
  { "TopicArn" :: NullOrUndefined (AmazonResourceName)
  , "FunctionArn" :: (AmazonResourceName)
  , "InvocationType" :: NullOrUndefined (InvocationType)
  }


newtype LastAttemptDate = LastAttemptDate Number


newtype LastFreshStart = LastFreshStart Number


-- | <p>Indicates that a resource could not be created because of service limits. For a list of Amazon SES limits, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html">Amazon SES Developer Guide</a>.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


-- | <p>Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype ListConfigurationSetsRequest = ListConfigurationSetsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined (MaxItems)
  }


-- | <p>A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype ListConfigurationSetsResponse = ListConfigurationSetsResponse 
  { "ConfigurationSets" :: NullOrUndefined (ConfigurationSets)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents a request to list the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p>
newtype ListCustomVerificationEmailTemplatesRequest = ListCustomVerificationEmailTemplatesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


-- | <p>A paginated list of custom verification email templates.</p>
newtype ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse 
  { "CustomVerificationEmailTemplates" :: NullOrUndefined (CustomVerificationEmailTemplates)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.</p>
newtype ListIdentitiesRequest = ListIdentitiesRequest 
  { "IdentityType" :: NullOrUndefined (IdentityType)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined (MaxItems)
  }


-- | <p>A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.</p>
newtype ListIdentitiesResponse = ListIdentitiesResponse 
  { "Identities" :: (IdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents a request to return a list of sending authorization policies that are attached to an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype ListIdentityPoliciesRequest = ListIdentityPoliciesRequest 
  { "Identity" :: (Identity)
  }


-- | <p>A list of names of sending authorization policies that apply to an identity.</p>
newtype ListIdentityPoliciesResponse = ListIdentityPoliciesResponse 
  { "PolicyNames" :: (PolicyNameList)
  }


-- | <p>Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ListReceiptFiltersRequest = ListReceiptFiltersRequest 
  { 
  }


-- | <p>A list of IP address filters that exist under your AWS account.</p>
newtype ListReceiptFiltersResponse = ListReceiptFiltersResponse 
  { "Filters" :: NullOrUndefined (ReceiptFilterList)
  }


-- | <p>Represents a request to list the receipt rule sets that exist under your AWS account. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ListReceiptRuleSetsRequest = ListReceiptRuleSetsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>A list of receipt rule sets that exist under your AWS account.</p>
newtype ListReceiptRuleSetsResponse = ListReceiptRuleSetsResponse 
  { "RuleSets" :: NullOrUndefined (ReceiptRuleSetsLists)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTemplatesRequest = ListTemplatesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxItems" :: NullOrUndefined (MaxItems)
  }


newtype ListTemplatesResponse = ListTemplatesResponse 
  { "TemplatesMetadata" :: NullOrUndefined (TemplateMetadataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>A list of email addresses that you have verified with Amazon SES under your AWS account.</p>
newtype ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse 
  { "VerifiedEmailAddresses" :: NullOrUndefined (AddressList)
  }


newtype MailFromDomainAttributes = MailFromDomainAttributes (Map Identity IdentityMailFromDomainAttributes)


newtype MailFromDomainName = MailFromDomainName String


-- | <p> Indicates that the message could not be sent because Amazon SES could not read the MX record required to use the specified MAIL FROM domain. For information about editing the custom MAIL FROM domain settings for an identity, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html">Amazon SES Developer Guide</a>.</p>
newtype MailFromDomainNotVerifiedException = MailFromDomainNotVerifiedException 
  { 
  }


newtype Max24HourSend = Max24HourSend Number


newtype MaxItems = MaxItems Int


newtype MaxResults = MaxResults Int


newtype MaxSendRate = MaxSendRate Number


-- | <p>Represents the message to be sent, composed of a subject and a body.</p>
newtype Message = Message 
  { "Subject" :: (Content)
  , "Body" :: (Body)
  }


newtype MessageData = MessageData String


-- | <p>Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype MessageDsn = MessageDsn 
  { "ReportingMta" :: (ReportingMta)
  , "ArrivalDate" :: NullOrUndefined (ArrivalDate)
  , "ExtensionFields" :: NullOrUndefined (ExtensionFieldList)
  }


newtype MessageId = MessageId String


-- | <p>Indicates that the action failed, and the message could not be sent. Check the error stack for more information about what caused the error.</p>
newtype MessageRejected = MessageRejected 
  { 
  }


-- | <p>Contains the name and value of a tag that you can provide to <code>SendEmail</code> or <code>SendRawEmail</code> to apply to an email.</p> <p>Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype MessageTag = MessageTag 
  { "Name" :: (MessageTagName)
  , "Value" :: (MessageTagValue)
  }


newtype MessageTagList = MessageTagList (Array MessageTag)


newtype MessageTagName = MessageTagName String


newtype MessageTagValue = MessageTagValue String


-- | <p>Indicates that one or more of the replacement values for the specified template was not specified. Ensure that the TemplateData object contains references to all of the replacement tags in the specified template.</p>
newtype MissingRenderingAttributeException = MissingRenderingAttributeException 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  }


newtype NextToken = NextToken String


newtype NotificationAttributes = NotificationAttributes (Map Identity IdentityNotificationAttributes)


newtype NotificationTopic = NotificationTopic String


newtype NotificationType = NotificationType String


newtype Policy = Policy String


newtype PolicyMap = PolicyMap (Map PolicyName Policy)


newtype PolicyName = PolicyName String


newtype PolicyNameList = PolicyNameList (Array PolicyName)


-- | <p>Indicates that the account has not been granted production access.</p>
newtype ProductionAccessNotGrantedException = ProductionAccessNotGrantedException 
  { 
  }


-- | <p>Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>
newtype PutIdentityPolicyRequest = PutIdentityPolicyRequest 
  { "Identity" :: (Identity)
  , "PolicyName" :: (PolicyName)
  , "Policy" :: (Policy)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse 
  { 
  }


-- | <p>Represents the raw data of the message.</p>
newtype RawMessage = RawMessage 
  { "Data" :: (RawMessageData)
  }


newtype RawMessageData = RawMessageData String


-- | <p>An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptAction = ReceiptAction 
  { "S3Action" :: NullOrUndefined (S3Action)
  , "BounceAction" :: NullOrUndefined (BounceAction)
  , "WorkmailAction" :: NullOrUndefined (WorkmailAction)
  , "LambdaAction" :: NullOrUndefined (LambdaAction)
  , "StopAction" :: NullOrUndefined (StopAction)
  , "AddHeaderAction" :: NullOrUndefined (AddHeaderAction)
  , "SNSAction" :: NullOrUndefined (SNSAction)
  }


newtype ReceiptActionsList = ReceiptActionsList (Array ReceiptAction)


-- | <p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptFilter = ReceiptFilter 
  { "Name" :: (ReceiptFilterName)
  , "IpFilter" :: (ReceiptIpFilter)
  }


newtype ReceiptFilterList = ReceiptFilterList (Array ReceiptFilter)


newtype ReceiptFilterName = ReceiptFilterName String


newtype ReceiptFilterPolicy = ReceiptFilterPolicy String


-- | <p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptIpFilter = ReceiptIpFilter 
  { "Policy" :: (ReceiptFilterPolicy)
  , "Cidr" :: (Cidr)
  }


-- | <p>Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.</p> <p>Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptRule = ReceiptRule 
  { "Name" :: (ReceiptRuleName)
  , "Enabled" :: NullOrUndefined (Enabled)
  , "TlsPolicy" :: NullOrUndefined (TlsPolicy)
  , "Recipients" :: NullOrUndefined (RecipientsList)
  , "Actions" :: NullOrUndefined (ReceiptActionsList)
  , "ScanEnabled" :: NullOrUndefined (Enabled)
  }


newtype ReceiptRuleName = ReceiptRuleName String


newtype ReceiptRuleNamesList = ReceiptRuleNamesList (Array ReceiptRuleName)


-- | <p>Information about a receipt rule set.</p> <p>A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p>
newtype ReceiptRuleSetMetadata = ReceiptRuleSetMetadata 
  { "Name" :: NullOrUndefined (ReceiptRuleSetName)
  , "CreatedTimestamp" :: NullOrUndefined (Number)
  }


newtype ReceiptRuleSetName = ReceiptRuleSetName String


newtype ReceiptRuleSetsLists = ReceiptRuleSetsLists (Array ReceiptRuleSetMetadata)


newtype ReceiptRulesList = ReceiptRulesList (Array ReceiptRule)


newtype Recipient = Recipient String


-- | <p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>
newtype RecipientDsnFields = RecipientDsnFields 
  { "FinalRecipient" :: NullOrUndefined (Address)
  , "Action" :: (DsnAction)
  , "RemoteMta" :: NullOrUndefined (RemoteMta)
  , "Status" :: (DsnStatus)
  , "DiagnosticCode" :: NullOrUndefined (DiagnosticCode)
  , "LastAttemptDate" :: NullOrUndefined (LastAttemptDate)
  , "ExtensionFields" :: NullOrUndefined (ExtensionFieldList)
  }


newtype RecipientsList = RecipientsList (Array Recipient)


newtype RemoteMta = RemoteMta String


newtype RenderedTemplate = RenderedTemplate String


-- | <p>Represents a request to reorder the receipt rules within a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype ReorderReceiptRuleSetRequest = ReorderReceiptRuleSetRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleNames" :: (ReceiptRuleNamesList)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype ReorderReceiptRuleSetResponse = ReorderReceiptRuleSetResponse 
  { 
  }


newtype ReportingMta = ReportingMta String


-- | <p>Contains information about the reputation settings for a configuration set.</p>
newtype ReputationOptions = ReputationOptions 
  { "SendingEnabled" :: NullOrUndefined (Enabled)
  , "ReputationMetricsEnabled" :: NullOrUndefined (Enabled)
  , "LastFreshStart" :: NullOrUndefined (LastFreshStart)
  }


-- | <p>Indicates that the provided receipt rule does not exist.</p>
newtype RuleDoesNotExistException = RuleDoesNotExistException 
  { "Name" :: NullOrUndefined (RuleOrRuleSetName)
  }


newtype RuleOrRuleSetName = RuleOrRuleSetName String


-- | <p>Indicates that the provided receipt rule set does not exist.</p>
newtype RuleSetDoesNotExistException = RuleSetDoesNotExistException 
  { "Name" :: NullOrUndefined (RuleOrRuleSetName)
  }


-- | <p>When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <note> <p>When you save your emails to an Amazon S3 bucket, the maximum email size (including headers) is 30 MB. Emails larger than that will bounce.</p> </note> <p>For information about specifying Amazon S3 actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html">Amazon SES Developer Guide</a>.</p>
newtype S3Action = S3Action 
  { "TopicArn" :: NullOrUndefined (AmazonResourceName)
  , "BucketName" :: (S3BucketName)
  , "ObjectKeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "KmsKeyArn" :: NullOrUndefined (AmazonResourceName)
  }


newtype S3BucketName = S3BucketName String


newtype S3KeyPrefix = S3KeyPrefix String


-- | <p>When included in a receipt rule, this action publishes a notification to Amazon Simple Notification Service (Amazon SNS). This action includes a complete copy of the email content in the Amazon SNS notifications. Amazon SNS notifications for all other actions simply provide information about the email. They do not include the email content itself.</p> <p>If you own the Amazon SNS topic, you don't need to do anything to give Amazon SES permission to publish emails to it. However, if you don't own the Amazon SNS topic, you need to attach a policy to the topic to give Amazon SES permissions to access it. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <important> <p>You can only publish emails that are 150 KB or less (including the header) to Amazon SNS. Larger emails will bounce. If you anticipate emails larger than 150 KB, use the S3 action instead.</p> </important> <p>For information about using a receipt rule to publish an Amazon SNS notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SNSAction = SNSAction 
  { "TopicArn" :: (AmazonResourceName)
  , "Encoding" :: NullOrUndefined (SNSActionEncoding)
  }


newtype SNSActionEncoding = SNSActionEncoding String


-- | <p>Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.</p> <p>Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype SNSDestination = SNSDestination 
  { "TopicARN" :: (AmazonResourceName)
  }


-- | <p>Represents a request to send a bounce message to the sender of an email you received through Amazon SES.</p>
newtype SendBounceRequest = SendBounceRequest 
  { "OriginalMessageId" :: (MessageId)
  , "BounceSender" :: (Address)
  , "Explanation" :: NullOrUndefined (Explanation)
  , "MessageDsn" :: NullOrUndefined (MessageDsn)
  , "BouncedRecipientInfoList" :: (BouncedRecipientInfoList)
  , "BounceSenderArn" :: NullOrUndefined (AmazonResourceName)
  }


-- | <p>Represents a unique message ID.</p>
newtype SendBounceResponse = SendBounceResponse 
  { "MessageId" :: NullOrUndefined (MessageId)
  }


-- | <p>Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype SendBulkTemplatedEmailRequest = SendBulkTemplatedEmailRequest 
  { "Source" :: (Address)
  , "SourceArn" :: NullOrUndefined (AmazonResourceName)
  , "ReplyToAddresses" :: NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined (Address)
  , "ReturnPathArn" :: NullOrUndefined (AmazonResourceName)
  , "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "DefaultTags" :: NullOrUndefined (MessageTagList)
  , "Template" :: (TemplateName)
  , "TemplateArn" :: NullOrUndefined (AmazonResourceName)
  , "DefaultTemplateData" :: NullOrUndefined (TemplateData)
  , "Destinations" :: (BulkEmailDestinationList)
  }


newtype SendBulkTemplatedEmailResponse = SendBulkTemplatedEmailResponse 
  { "Status" :: (BulkEmailDestinationStatusList)
  }


-- | <p>Represents a request to send a custom verification email to a specified recipient.</p>
newtype SendCustomVerificationEmailRequest = SendCustomVerificationEmailRequest 
  { "EmailAddress" :: (Address)
  , "TemplateName" :: (TemplateName)
  , "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


-- | <p>The response received when attempting to send the custom verification email.</p>
newtype SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse 
  { "MessageId" :: NullOrUndefined (MessageId)
  }


-- | <p>Represents sending statistics data. Each <code>SendDataPoint</code> contains statistics for a 15-minute period of sending activity. </p>
newtype SendDataPoint = SendDataPoint 
  { "Number" :: NullOrUndefined (Number)
  , "DeliveryAttempts" :: NullOrUndefined (Counter)
  , "Bounces" :: NullOrUndefined (Counter)
  , "Complaints" :: NullOrUndefined (Counter)
  , "Rejects" :: NullOrUndefined (Counter)
  }


newtype SendDataPointList = SendDataPointList (Array SendDataPoint)


-- | <p>Represents a request to send a single formatted email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html">Amazon SES Developer Guide</a>.</p>
newtype SendEmailRequest = SendEmailRequest 
  { "Source" :: (Address)
  , "Destination" :: (Destination)
  , "Message" :: (Message)
  , "ReplyToAddresses" :: NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined (Address)
  , "SourceArn" :: NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


-- | <p>Represents a unique message ID.</p>
newtype SendEmailResponse = SendEmailResponse 
  { "MessageId" :: (MessageId)
  }


-- | <p>Represents a request to send a single raw email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html">Amazon SES Developer Guide</a>.</p>
newtype SendRawEmailRequest = SendRawEmailRequest 
  { "Source" :: NullOrUndefined (Address)
  , "Destinations" :: NullOrUndefined (AddressList)
  , "RawMessage" :: (RawMessage)
  , "FromArn" :: NullOrUndefined (AmazonResourceName)
  , "SourceArn" :: NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


-- | <p>Represents a unique message ID.</p>
newtype SendRawEmailResponse = SendRawEmailResponse 
  { "MessageId" :: (MessageId)
  }


-- | <p>Represents a request to send a templated email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>
newtype SendTemplatedEmailRequest = SendTemplatedEmailRequest 
  { "Source" :: (Address)
  , "Destination" :: (Destination)
  , "ReplyToAddresses" :: NullOrUndefined (AddressList)
  , "ReturnPath" :: NullOrUndefined (Address)
  , "SourceArn" :: NullOrUndefined (AmazonResourceName)
  , "ReturnPathArn" :: NullOrUndefined (AmazonResourceName)
  , "Tags" :: NullOrUndefined (MessageTagList)
  , "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  , "Template" :: (TemplateName)
  , "TemplateArn" :: NullOrUndefined (AmazonResourceName)
  , "TemplateData" :: (TemplateData)
  }


newtype SendTemplatedEmailResponse = SendTemplatedEmailResponse 
  { "MessageId" :: (MessageId)
  }


newtype SentLast24Hours = SentLast24Hours Number


-- | <p>Represents a request to set a receipt rule set as the active receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype SetActiveReceiptRuleSetRequest = SetActiveReceiptRuleSetRequest 
  { "RuleSetName" :: NullOrUndefined (ReceiptRuleSetName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetActiveReceiptRuleSetResponse = SetActiveReceiptRuleSetResponse 
  { 
  }


-- | <p>Represents a request to enable or disable Amazon SES Easy DKIM signing for an identity. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityDkimEnabledRequest = SetIdentityDkimEnabledRequest 
  { "Identity" :: (Identity)
  , "DkimEnabled" :: (Enabled)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse 
  { 
  }


-- | <p>Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityFeedbackForwardingEnabledRequest = SetIdentityFeedbackForwardingEnabledRequest 
  { "Identity" :: (Identity)
  , "ForwardingEnabled" :: (Enabled)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse 
  { 
  }


-- | <p>Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityHeadersInNotificationsEnabledRequest = SetIdentityHeadersInNotificationsEnabledRequest 
  { "Identity" :: (Identity)
  , "NotificationType" :: (NotificationType)
  , "Enabled" :: (Enabled)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse 
  { 
  }


-- | <p>Represents a request to enable or disable the Amazon SES custom MAIL FROM domain setup for a verified identity. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityMailFromDomainRequest = SetIdentityMailFromDomainRequest 
  { "Identity" :: (Identity)
  , "MailFromDomain" :: NullOrUndefined (MailFromDomainName)
  , "BehaviorOnMXFailure" :: NullOrUndefined (BehaviorOnMXFailure)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityMailFromDomainResponse = SetIdentityMailFromDomainResponse 
  { 
  }


-- | <p>Represents a request to specify the Amazon SNS topic to which Amazon SES will publish bounce, complaint, or delivery notifications for emails sent with that identity as the Source. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>
newtype SetIdentityNotificationTopicRequest = SetIdentityNotificationTopicRequest 
  { "Identity" :: (Identity)
  , "NotificationType" :: (NotificationType)
  , "SnsTopic" :: NullOrUndefined (NotificationTopic)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse 
  { 
  }


-- | <p>Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype SetReceiptRulePositionRequest = SetReceiptRulePositionRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "RuleName" :: (ReceiptRuleName)
  , "After" :: NullOrUndefined (ReceiptRuleName)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype SetReceiptRulePositionResponse = SetReceiptRulePositionResponse 
  { 
  }


-- | <p>When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about setting a stop action in a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html">Amazon SES Developer Guide</a>.</p>
newtype StopAction = StopAction 
  { "Scope" :: (StopScope)
  , "TopicArn" :: NullOrUndefined (AmazonResourceName)
  }


newtype StopScope = StopScope String


newtype Subject = Subject String


newtype SubjectPart = SubjectPart String


newtype SuccessRedirectionURL = SuccessRedirectionURL String


-- | <p>The content of the email, composed of a subject line, an HTML part, and a text-only part.</p>
newtype Template = Template 
  { "TemplateName" :: (TemplateName)
  , "SubjectPart" :: NullOrUndefined (SubjectPart)
  , "TextPart" :: NullOrUndefined (TextPart)
  , "HtmlPart" :: NullOrUndefined (HtmlPart)
  }


newtype TemplateContent = TemplateContent String


newtype TemplateData = TemplateData String


-- | <p>Indicates that the Template object you specified does not exist in your Amazon SES account.</p>
newtype TemplateDoesNotExistException = TemplateDoesNotExistException 
  { "TemplateName" :: NullOrUndefined (TemplateName)
  }


-- | <p>Contains information about an email template.</p>
newtype TemplateMetadata = TemplateMetadata 
  { "Name" :: NullOrUndefined (TemplateName)
  , "CreatedTimestamp" :: NullOrUndefined (Number)
  }


newtype TemplateMetadataList = TemplateMetadataList (Array TemplateMetadata)


newtype TemplateName = TemplateName String


newtype TestRenderTemplateRequest = TestRenderTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "TemplateData" :: (TemplateData)
  }


newtype TestRenderTemplateResponse = TestRenderTemplateResponse 
  { "RenderedTemplate" :: NullOrUndefined (RenderedTemplate)
  }


newtype TextPart = TextPart String


newtype TlsPolicy = TlsPolicy String


-- | <p>A domain that is used to redirect email recipients to an Amazon SES-operated domain. This domain captures open and click events generated by Amazon SES emails.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>
newtype TrackingOptions = TrackingOptions 
  { "CustomRedirectDomain" :: NullOrUndefined (CustomRedirectDomain)
  }


-- | <p>Indicates that the configuration set you specified already contains a TrackingOptions object.</p>
newtype TrackingOptionsAlreadyExistsException = TrackingOptionsAlreadyExistsException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


-- | <p>Indicates that the TrackingOptions object you specified does not exist.</p>
newtype TrackingOptionsDoesNotExistException = TrackingOptionsDoesNotExistException 
  { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName)
  }


-- | <p>Represents a request to enable or disable the email sending capabilities for your entire Amazon SES account.</p>
newtype UpdateAccountSendingEnabledRequest = UpdateAccountSendingEnabledRequest 
  { "Enabled" :: NullOrUndefined (Enabled)
  }


-- | <p>Represents a request to update the event destination of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>
newtype UpdateConfigurationSetEventDestinationRequest = UpdateConfigurationSetEventDestinationRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "EventDestination" :: (EventDestination)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateConfigurationSetEventDestinationResponse = UpdateConfigurationSetEventDestinationResponse 
  { 
  }


-- | <p>Represents a request to modify the reputation metric publishing settings for a configuration set.</p>
newtype UpdateConfigurationSetReputationMetricsEnabledRequest = UpdateConfigurationSetReputationMetricsEnabledRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "Enabled" :: (Enabled)
  }


-- | <p>Represents a request to enable or disable the email sending capabilities for a specific configuration set.</p>
newtype UpdateConfigurationSetSendingEnabledRequest = UpdateConfigurationSetSendingEnabledRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "Enabled" :: (Enabled)
  }


-- | <p>Represents a request to update the tracking options for a configuration set. </p>
newtype UpdateConfigurationSetTrackingOptionsRequest = UpdateConfigurationSetTrackingOptionsRequest 
  { "ConfigurationSetName" :: (ConfigurationSetName)
  , "TrackingOptions" :: (TrackingOptions)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse 
  { 
  }


-- | <p>Represents a request to update an existing custom verification email template.</p>
newtype UpdateCustomVerificationEmailTemplateRequest = UpdateCustomVerificationEmailTemplateRequest 
  { "TemplateName" :: (TemplateName)
  , "FromEmailAddress" :: NullOrUndefined (FromAddress)
  , "TemplateSubject" :: NullOrUndefined (Subject)
  , "TemplateContent" :: NullOrUndefined (TemplateContent)
  , "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL)
  , "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL)
  }


-- | <p>Represents a request to update a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>
newtype UpdateReceiptRuleRequest = UpdateReceiptRuleRequest 
  { "RuleSetName" :: (ReceiptRuleSetName)
  , "Rule" :: (ReceiptRule)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype UpdateReceiptRuleResponse = UpdateReceiptRuleResponse 
  { 
  }


newtype UpdateTemplateRequest = UpdateTemplateRequest 
  { "Template" :: (Template)
  }


newtype UpdateTemplateResponse = UpdateTemplateResponse 
  { 
  }


newtype VerificationAttributes = VerificationAttributes (Map Identity IdentityVerificationAttributes)


newtype VerificationStatus = VerificationStatus String


newtype VerificationToken = VerificationToken String


newtype VerificationTokenList = VerificationTokenList (Array VerificationToken)


-- | <p>Represents a request to generate the CNAME records needed to set up Easy DKIM with Amazon SES. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyDomainDkimRequest = VerifyDomainDkimRequest 
  { "Domain" :: (Domain)
  }


-- | <p>Returns CNAME records that you must publish to the DNS server of your domain to set up Easy DKIM with Amazon SES.</p>
newtype VerifyDomainDkimResponse = VerifyDomainDkimResponse 
  { "DkimTokens" :: (VerificationTokenList)
  }


-- | <p>Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyDomainIdentityRequest = VerifyDomainIdentityRequest 
  { "Domain" :: (Domain)
  }


-- | <p>Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.</p>
newtype VerifyDomainIdentityResponse = VerifyDomainIdentityResponse 
  { "VerificationToken" :: (VerificationToken)
  }


-- | <p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyEmailAddressRequest = VerifyEmailAddressRequest 
  { "EmailAddress" :: (Address)
  }


-- | <p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>
newtype VerifyEmailIdentityRequest = VerifyEmailIdentityRequest 
  { "EmailAddress" :: (Address)
  }


-- | <p>An empty element returned on a successful request.</p>
newtype VerifyEmailIdentityResponse = VerifyEmailIdentityResponse 
  { 
  }


-- | <p>When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.</p> <p>For information using a receipt rule to call Amazon WorkMail, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html">Amazon SES Developer Guide</a>.</p>
newtype WorkmailAction = WorkmailAction 
  { "TopicArn" :: NullOrUndefined (AmazonResourceName)
  , "OrganizationArn" :: (AmazonResourceName)
  }