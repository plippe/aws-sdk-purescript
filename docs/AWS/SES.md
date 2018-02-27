## Module AWS.SES

<fullname>Amazon Simple Email Service</fullname> <p> This is the API Reference for <a href="https://aws.amazon.com/ses/">Amazon Simple Email Service</a> (Amazon SES). This documentation is intended to be used in conjunction with the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> <note> <p> For a list of Amazon SES endpoints to use in service requests, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html">Regions and Amazon SES</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>. </p> </note>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `cloneReceiptRuleSet`

``` purescript
cloneReceiptRuleSet :: forall eff. CloneReceiptRuleSetRequest -> Aff (err :: RequestError | eff) CloneReceiptRuleSetResponse
```

<p>Creates a receipt rule set by cloning an existing one. All receipt rules and configurations are copied to the new receipt rule set and are completely independent of the source rule set.</p> <p>For information about setting up rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createConfigurationSet`

``` purescript
createConfigurationSet :: forall eff. CreateConfigurationSetRequest -> Aff (err :: RequestError | eff) CreateConfigurationSetResponse
```

<p>Creates a configuration set.</p> <p>Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createConfigurationSetEventDestination`

``` purescript
createConfigurationSetEventDestination :: forall eff. CreateConfigurationSetEventDestinationRequest -> Aff (err :: RequestError | eff) CreateConfigurationSetEventDestinationResponse
```

<p>Creates a configuration set event destination.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createConfigurationSetTrackingOptions`

``` purescript
createConfigurationSetTrackingOptions :: forall eff. CreateConfigurationSetTrackingOptionsRequest -> Aff (err :: RequestError | eff) CreateConfigurationSetTrackingOptionsResponse
```

<p>Creates an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>

#### `createCustomVerificationEmailTemplate`

``` purescript
createCustomVerificationEmailTemplate :: forall eff. CreateCustomVerificationEmailTemplateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a new custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createReceiptFilter`

``` purescript
createReceiptFilter :: forall eff. CreateReceiptFilterRequest -> Aff (err :: RequestError | eff) CreateReceiptFilterResponse
```

<p>Creates a new IP address filter.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createReceiptRule`

``` purescript
createReceiptRule :: forall eff. CreateReceiptRuleRequest -> Aff (err :: RequestError | eff) CreateReceiptRuleResponse
```

<p>Creates a receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createReceiptRuleSet`

``` purescript
createReceiptRuleSet :: forall eff. CreateReceiptRuleSetRequest -> Aff (err :: RequestError | eff) CreateReceiptRuleSetResponse
```

<p>Creates an empty receipt rule set.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `createTemplate`

``` purescript
createTemplate :: forall eff. CreateTemplateRequest -> Aff (err :: RequestError | eff) CreateTemplateResponse
```

<p>Creates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteConfigurationSet`

``` purescript
deleteConfigurationSet :: forall eff. DeleteConfigurationSetRequest -> Aff (err :: RequestError | eff) DeleteConfigurationSetResponse
```

<p>Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteConfigurationSetEventDestination`

``` purescript
deleteConfigurationSetEventDestination :: forall eff. DeleteConfigurationSetEventDestinationRequest -> Aff (err :: RequestError | eff) DeleteConfigurationSetEventDestinationResponse
```

<p>Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteConfigurationSetTrackingOptions`

``` purescript
deleteConfigurationSetTrackingOptions :: forall eff. DeleteConfigurationSetTrackingOptionsRequest -> Aff (err :: RequestError | eff) DeleteConfigurationSetTrackingOptionsResponse
```

<p>Deletes an association between a configuration set and a custom domain for open and click event tracking.</p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p> <note> <p>Deleting this kind of association will result in emails sent using the specified configuration set to capture open and click events using the standard, Amazon SES-operated domains.</p> </note>

#### `deleteCustomVerificationEmailTemplate`

``` purescript
deleteCustomVerificationEmailTemplate :: forall eff. DeleteCustomVerificationEmailTemplateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an existing custom verification email template. </p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteIdentity`

``` purescript
deleteIdentity :: forall eff. DeleteIdentityRequest -> Aff (err :: RequestError | eff) DeleteIdentityResponse
```

<p>Deletes the specified identity (an email address or a domain) from the list of verified identities.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteIdentityPolicy`

``` purescript
deleteIdentityPolicy :: forall eff. DeleteIdentityPolicyRequest -> Aff (err :: RequestError | eff) DeleteIdentityPolicyResponse
```

<p>Deletes the specified sending authorization policy for the given identity (an email address or a domain). This API returns successfully even if a policy with the specified name does not exist.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteReceiptFilter`

``` purescript
deleteReceiptFilter :: forall eff. DeleteReceiptFilterRequest -> Aff (err :: RequestError | eff) DeleteReceiptFilterResponse
```

<p>Deletes the specified IP address filter.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteReceiptRule`

``` purescript
deleteReceiptRule :: forall eff. DeleteReceiptRuleRequest -> Aff (err :: RequestError | eff) DeleteReceiptRuleResponse
```

<p>Deletes the specified receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteReceiptRuleSet`

``` purescript
deleteReceiptRuleSet :: forall eff. DeleteReceiptRuleSetRequest -> Aff (err :: RequestError | eff) DeleteReceiptRuleSetResponse
```

<p>Deletes the specified receipt rule set and all of the receipt rules it contains.</p> <note> <p>The currently active rule set cannot be deleted.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteTemplate`

``` purescript
deleteTemplate :: forall eff. DeleteTemplateRequest -> Aff (err :: RequestError | eff) DeleteTemplateResponse
```

<p>Deletes an email template.</p> <p>You can execute this operation no more than once per second.</p>

#### `deleteVerifiedEmailAddress`

``` purescript
deleteVerifiedEmailAddress :: forall eff. DeleteVerifiedEmailAddressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deprecated. Use the <code>DeleteIdentity</code> operation to delete email addresses and domains.</p>

#### `describeActiveReceiptRuleSet`

``` purescript
describeActiveReceiptRuleSet :: forall eff. DescribeActiveReceiptRuleSetRequest -> Aff (err :: RequestError | eff) DescribeActiveReceiptRuleSetResponse
```

<p>Returns the metadata and receipt rules for the receipt rule set that is currently active.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `describeConfigurationSet`

``` purescript
describeConfigurationSet :: forall eff. DescribeConfigurationSetRequest -> Aff (err :: RequestError | eff) DescribeConfigurationSetResponse
```

<p>Returns the details of the specified configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `describeReceiptRule`

``` purescript
describeReceiptRule :: forall eff. DescribeReceiptRuleRequest -> Aff (err :: RequestError | eff) DescribeReceiptRuleResponse
```

<p>Returns the details of the specified receipt rule.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `describeReceiptRuleSet`

``` purescript
describeReceiptRuleSet :: forall eff. DescribeReceiptRuleSetRequest -> Aff (err :: RequestError | eff) DescribeReceiptRuleSetResponse
```

<p>Returns the details of the specified receipt rule set.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `getAccountSendingEnabled`

``` purescript
getAccountSendingEnabled :: forall eff. Aff (err :: RequestError | eff) GetAccountSendingEnabledResponse
```

<p>Returns the email sending status of the Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>

#### `getCustomVerificationEmailTemplate`

``` purescript
getCustomVerificationEmailTemplate :: forall eff. GetCustomVerificationEmailTemplateRequest -> Aff (err :: RequestError | eff) GetCustomVerificationEmailTemplateResponse
```

<p>Returns the custom email verification template for the template name you specify.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `getIdentityDkimAttributes`

``` purescript
getIdentityDkimAttributes :: forall eff. GetIdentityDkimAttributesRequest -> Aff (err :: RequestError | eff) GetIdentityDkimAttributesResponse
```

<p>Returns the current status of Easy DKIM signing for an entity. For domain name identities, this operation also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES has successfully verified that these tokens have been published.</p> <p>This operation takes a list of identities as input and returns the following information for each:</p> <ul> <li> <p>Whether Easy DKIM signing is enabled or disabled.</p> </li> <li> <p>A set of DKIM tokens that represent the identity. If the identity is an email address, the tokens represent the domain of that address.</p> </li> <li> <p>Whether Amazon SES has successfully verified the DKIM tokens published in the domain's DNS. This information is only returned for domain name identities, not for email addresses.</p> </li> </ul> <p>This operation is throttled at one request per second and can only get DKIM attributes for up to 100 identities at a time.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>

#### `getIdentityMailFromDomainAttributes`

``` purescript
getIdentityMailFromDomainAttributes :: forall eff. GetIdentityMailFromDomainAttributesRequest -> Aff (err :: RequestError | eff) GetIdentityMailFromDomainAttributesResponse
```

<p>Returns the custom MAIL FROM attributes for a list of identities (email addresses : domains).</p> <p>This operation is throttled at one request per second and can only get custom MAIL FROM attributes for up to 100 identities at a time.</p>

#### `getIdentityNotificationAttributes`

``` purescript
getIdentityNotificationAttributes :: forall eff. GetIdentityNotificationAttributesRequest -> Aff (err :: RequestError | eff) GetIdentityNotificationAttributesResponse
```

<p>Given a list of verified identities (email addresses and/or domains), returns a structure describing identity notification attributes.</p> <p>This operation is throttled at one request per second and can only get notification attributes for up to 100 identities at a time.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>

#### `getIdentityPolicies`

``` purescript
getIdentityPolicies :: forall eff. GetIdentityPoliciesRequest -> Aff (err :: RequestError | eff) GetIdentityPoliciesResponse
```

<p>Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `getIdentityVerificationAttributes`

``` purescript
getIdentityVerificationAttributes :: forall eff. GetIdentityVerificationAttributesRequest -> Aff (err :: RequestError | eff) GetIdentityVerificationAttributesResponse
```

<p>Given a list of identities (email addresses and/or domains), returns the verification status and (for domain identities) the verification token for each identity.</p> <p>The verification status of an email address is "Pending" until the email address owner clicks the link within the verification email that Amazon SES sent to that address. If the email address owner clicks the link within 24 hours, the verification status of the email address changes to "Success". If the link is not clicked within 24 hours, the verification status changes to "Failed." In that case, if you still want to verify the email address, you must restart the verification process from the beginning.</p> <p>For domain identities, the domain's verification status is "Pending" as Amazon SES searches for the required TXT record in the DNS settings of the domain. When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.</p> <p>This operation is throttled at one request per second and can only get verification attributes for up to 100 identities at a time.</p>

#### `getSendQuota`

``` purescript
getSendQuota :: forall eff. Aff (err :: RequestError | eff) GetSendQuotaResponse
```

<p>Provides the sending limits for the Amazon SES account. </p> <p>You can execute this operation no more than once per second.</p>

#### `getSendStatistics`

``` purescript
getSendStatistics :: forall eff. Aff (err :: RequestError | eff) GetSendStatisticsResponse
```

<p>Provides sending statistics for the Amazon SES account. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.</p> <p>You can execute this operation no more than once per second.</p>

#### `getTemplate`

``` purescript
getTemplate :: forall eff. GetTemplateRequest -> Aff (err :: RequestError | eff) GetTemplateResponse
```

<p>Displays the template object (which includes the Subject line, HTML part and text part) for the template you specify.</p> <p>You can execute this operation no more than once per second.</p>

#### `listConfigurationSets`

``` purescript
listConfigurationSets :: forall eff. ListConfigurationSetsRequest -> Aff (err :: RequestError | eff) ListConfigurationSetsResponse
```

<p>Provides a list of the configuration sets associated with your Amazon SES account. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the <code>ListConfigurationSets</code> operation again, passing the <code>NextToken</code> parameter and the value of the NextToken element to retrieve additional results.</p>

#### `listCustomVerificationEmailTemplates`

``` purescript
listCustomVerificationEmailTemplates :: forall eff. ListCustomVerificationEmailTemplatesRequest -> Aff (err :: RequestError | eff) ListCustomVerificationEmailTemplatesResponse
```

<p>Lists the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `listIdentities`

``` purescript
listIdentities :: forall eff. ListIdentitiesRequest -> Aff (err :: RequestError | eff) ListIdentitiesResponse
```

<p>Returns a list containing all of the identities (email addresses and domains) for your AWS account, regardless of verification status.</p> <p>You can execute this operation no more than once per second.</p>

#### `listIdentityPolicies`

``` purescript
listIdentityPolicies :: forall eff. ListIdentityPoliciesRequest -> Aff (err :: RequestError | eff) ListIdentityPoliciesResponse
```

<p>Returns a list of sending authorization policies that are attached to the given identity (an email address or a domain). This API returns only a list. If you want the actual policy content, you can use <code>GetIdentityPolicies</code>.</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `listReceiptFilters`

``` purescript
listReceiptFilters :: forall eff. ListReceiptFiltersRequest -> Aff (err :: RequestError | eff) ListReceiptFiltersResponse
```

<p>Lists the IP address filters associated with your AWS account.</p> <p>For information about managing IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `listReceiptRuleSets`

``` purescript
listReceiptRuleSets :: forall eff. ListReceiptRuleSetsRequest -> Aff (err :: RequestError | eff) ListReceiptRuleSetsResponse
```

<p>Lists the receipt rule sets that exist under your AWS account. If there are additional receipt rule sets to be retrieved, you will receive a <code>NextToken</code> that you can provide to the next call to <code>ListReceiptRuleSets</code> to retrieve the additional entries.</p> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `listTemplates`

``` purescript
listTemplates :: forall eff. ListTemplatesRequest -> Aff (err :: RequestError | eff) ListTemplatesResponse
```

<p>Lists the email templates present in your Amazon SES account.</p> <p>You can execute this operation no more than once per second.</p>

#### `listVerifiedEmailAddresses`

``` purescript
listVerifiedEmailAddresses :: forall eff. Aff (err :: RequestError | eff) ListVerifiedEmailAddressesResponse
```

<p>Deprecated. Use the <code>ListIdentities</code> operation to list the email addresses and domains associated with your account.</p>

#### `putIdentityPolicy`

``` purescript
putIdentityPolicy :: forall eff. PutIdentityPolicyRequest -> Aff (err :: RequestError | eff) PutIdentityPolicyResponse
```

<p>Adds or updates a sending authorization policy for the specified identity (an email address or a domain).</p> <note> <p>This API is for the identity owner only. If you have not verified the identity, this API will return an error.</p> </note> <p>Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `reorderReceiptRuleSet`

``` purescript
reorderReceiptRuleSet :: forall eff. ReorderReceiptRuleSetRequest -> Aff (err :: RequestError | eff) ReorderReceiptRuleSetResponse
```

<p>Reorders the receipt rules within a receipt rule set.</p> <note> <p>All of the rules in the rule set must be represented in this request. That is, this API will return an error if the reorder request doesn't explicitly position all of the rules.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `sendBounce`

``` purescript
sendBounce :: forall eff. SendBounceRequest -> Aff (err :: RequestError | eff) SendBounceResponse
```

<p>Generates and sends a bounce message to the sender of an email you received through Amazon SES. You can only use this API on an email up to 24 hours after you receive it.</p> <note> <p>You cannot use this API to send generic bounces for mail that was not received by Amazon SES.</p> </note> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `sendBulkTemplatedEmail`

``` purescript
sendBulkTemplatedEmail :: forall eff. SendBulkTemplatedEmailRequest -> Aff (err :: RequestError | eff) SendBulkTemplatedEmailResponse
```

<p>Composes an email message to multiple destinations. The message body is created using an email template.</p> <p>In order to send email using the <code>SendBulkTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Each <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>

#### `sendCustomVerificationEmail`

``` purescript
sendCustomVerificationEmail :: forall eff. SendCustomVerificationEmailRequest -> Aff (err :: RequestError | eff) SendCustomVerificationEmailResponse
```

<p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a customized verification email is sent to the specified address.</p> <p>To use this operation, you must first create a custom verification email template. For more information about creating and using custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `sendEmail`

``` purescript
sendEmail :: forall eff. SendEmailRequest -> Aff (err :: RequestError | eff) SendEmailResponse
```

<p>Composes an email message and immediately queues it for sending. In order to send email using the <code>SendEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important>

#### `sendRawEmail`

``` purescript
sendRawEmail :: forall eff. SendRawEmailRequest -> Aff (err :: RequestError | eff) SendRawEmailResponse
```

<p>Composes an email message and immediately queues it for sending. When calling this operation, you may specify the message headers as well as the content. The <code>SendRawEmail</code> operation is particularly useful for sending multipart MIME emails (such as those that contain both a plain-text and an HTML version). </p> <p>In order to send email using the <code>SendRawEmail</code> operation, your message must meet the following requirements:</p> <ul> <li> <p>The message must be sent from a verified email address or domain. If you attempt to send email using a non-verified address or domain, the operation will result in an "Email address not verified" error. </p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be smaller than 10 MB.</p> </li> <li> <p>The message must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> <li> <p>The message may not include more than 50 recipients, across the To:, CC: and BCC: fields. If you need to send an email message to a larger audience, you can divide your recipient list into groups of 50 or fewer, and then call the <code>SendRawEmail</code> operation several times to send the message to each group.</p> </li> </ul> <important> <p>For every message that you send, the total number of recipients (including each recipient in the To:, CC: and BCC: fields) is counted against the maximum number of emails you can send in a 24-hour period (your <i>sending quota</i>). For more information about sending quotas in Amazon SES, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/manage-sending-limits.html">Managing Your Amazon SES Sending Limits</a> in the <i>Amazon SES Developer Guide.</i> </p> </important> <p>Additionally, keep the following considerations in mind when using the <code>SendRawEmail</code> operation:</p> <ul> <li> <p>Although you can customize the message headers when using the <code>SendRawEmail</code> operation, Amazon SES will automatically apply its own <code>Message-ID</code> and <code>Date</code> headers; if you passed these headers when creating the message, they will be overwritten by the values that Amazon SES provides.</p> </li> <li> <p>If you are using sending authorization to send on behalf of another user, <code>SendRawEmail</code> enables you to specify the cross-account identity for the email's Source, From, and Return-Path parameters in one of two ways: you can pass optional parameters <code>SourceArn</code>, <code>FromArn</code>, and/or <code>ReturnPathArn</code> to the API, or you can include the following X-headers in the header of your raw email:</p> <ul> <li> <p> <code>X-SES-SOURCE-ARN</code> </p> </li> <li> <p> <code>X-SES-FROM-ARN</code> </p> </li> <li> <p> <code>X-SES-RETURN-PATH-ARN</code> </p> </li> </ul> <important> <p>Do not include these X-headers in the DKIM signature; Amazon SES will remove them before sending the email.</p> </important> <p>For most common sending authorization scenarios, we recommend that you specify the <code>SourceIdentityArn</code> parameter and not the <code>FromIdentityArn</code> or <code>ReturnPathIdentityArn</code> parameters. If you only specify the <code>SourceIdentityArn</code> parameter, Amazon SES will set the From and Return Path addresses to the identity specified in <code>SourceIdentityArn</code>. For more information about sending authorization, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Using Sending Authorization with Amazon SES</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> </ul>

#### `sendTemplatedEmail`

``` purescript
sendTemplatedEmail :: forall eff. SendTemplatedEmailRequest -> Aff (err :: RequestError | eff) SendTemplatedEmailResponse
```

<p>Composes an email message using an email template and immediately queues it for sending.</p> <p>In order to send email using the <code>SendTemplatedEmail</code> operation, your call to the API must meet the following requirements:</p> <ul> <li> <p>The call must refer to an existing email template. You can create email templates using the <a>CreateTemplate</a> operation.</p> </li> <li> <p>The message must be sent from a verified email address or domain.</p> </li> <li> <p>If your account is still in the Amazon SES sandbox, you may only send to verified addresses or domains, or to email addresses associated with the Amazon SES Mailbox Simulator. For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> </li> <li> <p>The total size of the message, including attachments, must be less than 10 MB.</p> </li> <li> <p>Calls to the <code>SendTemplatedEmail</code> operation may only include one <code>Destination</code> parameter. A destination is a set of recipients who will receive the same version of the email. The <code>Destination</code> parameter can include up to 50 recipients, across the To:, CC: and BCC: fields.</p> </li> <li> <p>The <code>Destination</code> parameter must include at least one recipient email address. The recipient address can be a To: address, a CC: address, or a BCC: address. If a recipient email address is invalid (that is, it is not in the format <i>UserName@[SubDomain.]Domain.TopLevelDomain</i>), the entire message will be rejected, even if the message contains other recipients that are valid.</p> </li> </ul>

#### `setActiveReceiptRuleSet`

``` purescript
setActiveReceiptRuleSet :: forall eff. SetActiveReceiptRuleSetRequest -> Aff (err :: RequestError | eff) SetActiveReceiptRuleSetResponse
```

<p>Sets the specified receipt rule set as the active receipt rule set.</p> <note> <p>To disable your email-receiving through Amazon SES completely, you can call this API with RuleSetName set to null.</p> </note> <p>For information about managing receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rule-sets.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `setIdentityDkimEnabled`

``` purescript
setIdentityDkimEnabled :: forall eff. SetIdentityDkimEnabledRequest -> Aff (err :: RequestError | eff) SetIdentityDkimEnabledResponse
```

<p>Enables or disables Easy DKIM signing of email sent from an identity:</p> <ul> <li> <p>If Easy DKIM signing is enabled for a domain name identity (such as <code>example.com</code>), then Amazon SES will DKIM-sign all email sent by addresses under that domain name (for example, <code>user@example.com</code>).</p> </li> <li> <p>If Easy DKIM signing is enabled for an email address, then Amazon SES will DKIM-sign all email sent by that email address.</p> </li> </ul> <p>For email addresses (for example, <code>user@example.com</code>), you can only enable Easy DKIM signing if the corresponding domain (in this case, <code>example.com</code>) has been set up for Easy DKIM using the AWS Console or the <code>VerifyDomainDkim</code> operation.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about Easy DKIM signing, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>

#### `setIdentityFeedbackForwardingEnabled`

``` purescript
setIdentityFeedbackForwardingEnabled :: forall eff. SetIdentityFeedbackForwardingEnabledRequest -> Aff (err :: RequestError | eff) SetIdentityFeedbackForwardingEnabledResponse
```

<p>Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.</p> <note> <p>Feedback forwarding does not apply to delivery notifications. Delivery notifications are only available through Amazon SNS.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>

#### `setIdentityHeadersInNotificationsEnabled`

``` purescript
setIdentityHeadersInNotificationsEnabled :: forall eff. SetIdentityHeadersInNotificationsEnabledRequest -> Aff (err :: RequestError | eff) SetIdentityHeadersInNotificationsEnabledResponse
```

<p>Given an identity (an email address or a domain), sets whether Amazon SES includes the original email headers in the Amazon Simple Notification Service (Amazon SNS) notifications of a specified type.</p> <p>You can execute this operation no more than once per second.</p> <p>For more information about using notifications with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>

#### `setIdentityMailFromDomain`

``` purescript
setIdentityMailFromDomain :: forall eff. SetIdentityMailFromDomainRequest -> Aff (err :: RequestError | eff) SetIdentityMailFromDomainResponse
```

<p>Enables or disables the custom MAIL FROM domain setup for a verified identity (an email address or a domain).</p> <important> <p>To send emails using the specified MAIL FROM domain, you must add an MX record to your MAIL FROM domain's DNS settings. If you want your emails to pass Sender Policy Framework (SPF) checks, you must also add or update an SPF record. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html">Amazon SES Developer Guide</a>.</p> </important> <p>You can execute this operation no more than once per second.</p>

#### `setIdentityNotificationTopic`

``` purescript
setIdentityNotificationTopic :: forall eff. SetIdentityNotificationTopicRequest -> Aff (err :: RequestError | eff) SetIdentityNotificationTopicResponse
```

<p>Given an identity (an email address or a domain), sets the Amazon Simple Notification Service (Amazon SNS) topic to which Amazon SES will publish bounce, complaint, and/or delivery notifications for emails sent with that identity as the <code>Source</code>.</p> <note> <p>Unless feedback forwarding is enabled, you must specify Amazon SNS topics for bounce and complaint notifications. For more information, see <code>SetIdentityFeedbackForwardingEnabled</code>.</p> </note> <p>You can execute this operation no more than once per second.</p> <p>For more information about feedback notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>

#### `setReceiptRulePosition`

``` purescript
setReceiptRulePosition :: forall eff. SetReceiptRulePositionRequest -> Aff (err :: RequestError | eff) SetReceiptRulePositionResponse
```

<p>Sets the position of the specified receipt rule in the receipt rule set.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `testRenderTemplate`

``` purescript
testRenderTemplate :: forall eff. TestRenderTemplateRequest -> Aff (err :: RequestError | eff) TestRenderTemplateResponse
```

<p>Creates a preview of the MIME content of an email when provided with a template and a set of replacement data.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateAccountSendingEnabled`

``` purescript
updateAccountSendingEnabled :: forall eff. UpdateAccountSendingEnabledRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables or disables email sending across your entire Amazon SES account. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending across your Amazon SES account when reputation metrics (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateConfigurationSetEventDestination`

``` purescript
updateConfigurationSetEventDestination :: forall eff. UpdateConfigurationSetEventDestinationRequest -> Aff (err :: RequestError | eff) UpdateConfigurationSetEventDestinationResponse
```

<p>Updates the event destination of a configuration set. Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Monitoring Your Amazon SES Sending Activity</a> in the <i>Amazon SES Developer Guide.</i> </p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>You can execute this operation no more than once per second.</p>

#### `updateConfigurationSetReputationMetricsEnabled`

``` purescript
updateConfigurationSetReputationMetricsEnabled :: forall eff. UpdateConfigurationSetReputationMetricsEnabledRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables or disables the publishing of reputation metrics for emails sent using a specific configuration set. Reputation metrics include bounce and complaint rates. These metrics are published to Amazon CloudWatch. By using Amazon CloudWatch, you can create alarms when bounce or complaint rates exceed a certain threshold.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateConfigurationSetSendingEnabled`

``` purescript
updateConfigurationSetSendingEnabled :: forall eff. UpdateConfigurationSetSendingEnabledRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables or disables email sending for messages sent using a specific configuration set. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) reach certain thresholds.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateConfigurationSetTrackingOptions`

``` purescript
updateConfigurationSetTrackingOptions :: forall eff. UpdateConfigurationSetTrackingOptionsRequest -> Aff (err :: RequestError | eff) UpdateConfigurationSetTrackingOptionsResponse
```

<p>Modifies an association between a configuration set and a custom domain for open and click event tracking. </p> <p>By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>

#### `updateCustomVerificationEmailTemplate`

``` purescript
updateCustomVerificationEmailTemplate :: forall eff. UpdateCustomVerificationEmailTemplateRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates an existing custom verification email template.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateReceiptRule`

``` purescript
updateReceiptRule :: forall eff. UpdateReceiptRuleRequest -> Aff (err :: RequestError | eff) UpdateReceiptRuleResponse
```

<p>Updates a receipt rule.</p> <p>For information about managing receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-receipt-rules.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `updateTemplate`

``` purescript
updateTemplate :: forall eff. UpdateTemplateRequest -> Aff (err :: RequestError | eff) UpdateTemplateResponse
```

<p>Updates an email template. Email templates enable you to send personalized email to one or more destinations in a single API operation. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p> <p>You can execute this operation no more than once per second.</p>

#### `verifyDomainDkim`

``` purescript
verifyDomainDkim :: forall eff. VerifyDomainDkimRequest -> Aff (err :: RequestError | eff) VerifyDomainDkimResponse
```

<p>Returns a set of DKIM tokens for a domain. DKIM <i>tokens</i> are character strings that represent your domain's identity. Using these tokens, you will need to create DNS CNAME records that point to DKIM public keys hosted by Amazon SES. Amazon Web Services will eventually detect that you have updated your DNS records; this detection process may take up to 72 hours. Upon successful detection, Amazon SES will be able to DKIM-sign email originating from that domain.</p> <p>You can execute this operation no more than once per second.</p> <p>To enable or disable Easy DKIM signing for a domain, use the <code>SetIdentityDkimEnabled</code> operation.</p> <p>For more information about creating DNS records using DKIM tokens, go to the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html">Amazon SES Developer Guide</a>.</p>

#### `verifyDomainIdentity`

``` purescript
verifyDomainIdentity :: forall eff. VerifyDomainIdentityRequest -> Aff (err :: RequestError | eff) VerifyDomainIdentityResponse
```

<p>Adds a domain to the list of identities for your Amazon SES account and attempts to verify it. For more information about verifying domains, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Verifying Email Addresses and Domains</a> in the <i>Amazon SES Developer Guide.</i> </p> <p>You can execute this operation no more than once per second.</p>

#### `verifyEmailAddress`

``` purescript
verifyEmailAddress :: forall eff. VerifyEmailAddressRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deprecated. Use the <code>VerifyEmailIdentity</code> operation to verify a new email address.</p>

#### `verifyEmailIdentity`

``` purescript
verifyEmailIdentity :: forall eff. VerifyEmailIdentityRequest -> Aff (err :: RequestError | eff) VerifyEmailIdentityResponse
```

<p>Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a verification email is sent to the specified address.</p> <p>You can execute this operation no more than once per second.</p>

#### `AccountSendingPausedException`

``` purescript
newtype AccountSendingPausedException
  = AccountSendingPausedException {  }
```

<p>Indicates that email sending is disabled for your entire Amazon SES account.</p> <p>You can enable or disable email sending for your Amazon SES account using <a>UpdateAccountSendingEnabled</a>.</p>

##### Instances
``` purescript
Newtype AccountSendingPausedException _
```

#### `AddHeaderAction`

``` purescript
newtype AddHeaderAction
  = AddHeaderAction { "HeaderName" :: HeaderName, "HeaderValue" :: HeaderValue }
```

<p>When included in a receipt rule, this action adds a header to the received email.</p> <p>For information about adding a header using a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype AddHeaderAction _
```

#### `Address`

``` purescript
newtype Address
  = Address String
```

##### Instances
``` purescript
Newtype Address _
```

#### `AddressList`

``` purescript
newtype AddressList
  = AddressList (Array Address)
```

##### Instances
``` purescript
Newtype AddressList _
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Name" :: NullOrUndefined (RuleOrRuleSetName) }
```

<p>Indicates that a resource could not be created because of a naming conflict.</p>

##### Instances
``` purescript
Newtype AlreadyExistsException _
```

#### `AmazonResourceName`

``` purescript
newtype AmazonResourceName
  = AmazonResourceName String
```

##### Instances
``` purescript
Newtype AmazonResourceName _
```

#### `ArrivalDate`

``` purescript
newtype ArrivalDate
  = ArrivalDate Number
```

##### Instances
``` purescript
Newtype ArrivalDate _
```

#### `BehaviorOnMXFailure`

``` purescript
newtype BehaviorOnMXFailure
  = BehaviorOnMXFailure String
```

##### Instances
``` purescript
Newtype BehaviorOnMXFailure _
```

#### `Body`

``` purescript
newtype Body
  = Body { "Text" :: NullOrUndefined (Content), "Html" :: NullOrUndefined (Content) }
```

<p>Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.</p>

##### Instances
``` purescript
Newtype Body _
```

#### `BounceAction`

``` purescript
newtype BounceAction
  = BounceAction { "TopicArn" :: NullOrUndefined (AmazonResourceName), "SmtpReplyCode" :: BounceSmtpReplyCode, "StatusCode" :: NullOrUndefined (BounceStatusCode), "Message" :: BounceMessage, "Sender" :: Address }
```

<p>When included in a receipt rule, this action rejects the received email by returning a bounce response to the sender and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about sending a bounce message in response to a received email, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-bounce.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype BounceAction _
```

#### `BounceMessage`

``` purescript
newtype BounceMessage
  = BounceMessage String
```

##### Instances
``` purescript
Newtype BounceMessage _
```

#### `BounceSmtpReplyCode`

``` purescript
newtype BounceSmtpReplyCode
  = BounceSmtpReplyCode String
```

##### Instances
``` purescript
Newtype BounceSmtpReplyCode _
```

#### `BounceStatusCode`

``` purescript
newtype BounceStatusCode
  = BounceStatusCode String
```

##### Instances
``` purescript
Newtype BounceStatusCode _
```

#### `BounceType`

``` purescript
newtype BounceType
  = BounceType String
```

##### Instances
``` purescript
Newtype BounceType _
```

#### `BouncedRecipientInfo`

``` purescript
newtype BouncedRecipientInfo
  = BouncedRecipientInfo { "Recipient" :: Address, "RecipientArn" :: NullOrUndefined (AmazonResourceName), "BounceType" :: NullOrUndefined (BounceType), "RecipientDsnFields" :: NullOrUndefined (RecipientDsnFields) }
```

<p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype BouncedRecipientInfo _
```

#### `BouncedRecipientInfoList`

``` purescript
newtype BouncedRecipientInfoList
  = BouncedRecipientInfoList (Array BouncedRecipientInfo)
```

##### Instances
``` purescript
Newtype BouncedRecipientInfoList _
```

#### `BulkEmailDestination`

``` purescript
newtype BulkEmailDestination
  = BulkEmailDestination { "Destination" :: Destination, "ReplacementTags" :: NullOrUndefined (MessageTagList), "ReplacementTemplateData" :: NullOrUndefined (TemplateData) }
```

<p>An array that contains one or more Destinations, as well as the tags and replacement data associated with each of those Destinations.</p>

##### Instances
``` purescript
Newtype BulkEmailDestination _
```

#### `BulkEmailDestinationList`

``` purescript
newtype BulkEmailDestinationList
  = BulkEmailDestinationList (Array BulkEmailDestination)
```

##### Instances
``` purescript
Newtype BulkEmailDestinationList _
```

#### `BulkEmailDestinationStatus`

``` purescript
newtype BulkEmailDestinationStatus
  = BulkEmailDestinationStatus { "Status" :: NullOrUndefined (BulkEmailStatus), "Error" :: NullOrUndefined (Error), "MessageId" :: NullOrUndefined (MessageId) }
```

<p>An object that contains the response from the <code>SendBulkTemplatedEmail</code> operation.</p>

##### Instances
``` purescript
Newtype BulkEmailDestinationStatus _
```

#### `BulkEmailDestinationStatusList`

``` purescript
newtype BulkEmailDestinationStatusList
  = BulkEmailDestinationStatusList (Array BulkEmailDestinationStatus)
```

##### Instances
``` purescript
Newtype BulkEmailDestinationStatusList _
```

#### `BulkEmailStatus`

``` purescript
newtype BulkEmailStatus
  = BulkEmailStatus String
```

##### Instances
``` purescript
Newtype BulkEmailStatus _
```

#### `CannotDeleteException`

``` purescript
newtype CannotDeleteException
  = CannotDeleteException { "Name" :: NullOrUndefined (RuleOrRuleSetName) }
```

<p>Indicates that the delete operation could not be completed.</p>

##### Instances
``` purescript
Newtype CannotDeleteException _
```

#### `Charset`

``` purescript
newtype Charset
  = Charset String
```

##### Instances
``` purescript
Newtype Charset _
```

#### `Cidr`

``` purescript
newtype Cidr
  = Cidr String
```

##### Instances
``` purescript
Newtype Cidr _
```

#### `CloneReceiptRuleSetRequest`

``` purescript
newtype CloneReceiptRuleSetRequest
  = CloneReceiptRuleSetRequest { "RuleSetName" :: ReceiptRuleSetName, "OriginalRuleSetName" :: ReceiptRuleSetName }
```

<p>Represents a request to create a receipt rule set by cloning an existing one. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CloneReceiptRuleSetRequest _
```

#### `CloneReceiptRuleSetResponse`

``` purescript
newtype CloneReceiptRuleSetResponse
  = CloneReceiptRuleSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CloneReceiptRuleSetResponse _
```

#### `CloudWatchDestination`

``` purescript
newtype CloudWatchDestination
  = CloudWatchDestination { "DimensionConfigurations" :: CloudWatchDimensionConfigurations }
```

<p>Contains information associated with an Amazon CloudWatch event destination to which email sending events are published.</p> <p>Event destinations, such as Amazon CloudWatch, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CloudWatchDestination _
```

#### `CloudWatchDimensionConfiguration`

``` purescript
newtype CloudWatchDimensionConfiguration
  = CloudWatchDimensionConfiguration { "DimensionName" :: DimensionName, "DimensionValueSource" :: DimensionValueSource, "DefaultDimensionValue" :: DefaultDimensionValue }
```

<p>Contains the dimension configuration to use when you publish email sending events to Amazon CloudWatch.</p> <p>For information about publishing email sending events to Amazon CloudWatch, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CloudWatchDimensionConfiguration _
```

#### `CloudWatchDimensionConfigurations`

``` purescript
newtype CloudWatchDimensionConfigurations
  = CloudWatchDimensionConfigurations (Array CloudWatchDimensionConfiguration)
```

##### Instances
``` purescript
Newtype CloudWatchDimensionConfigurations _
```

#### `ConfigurationSet`

``` purescript
newtype ConfigurationSet
  = ConfigurationSet { "Name" :: ConfigurationSetName }
```

<p>The name of the configuration set.</p> <p>Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html">Using Amazon SES Configuration Sets</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ConfigurationSet _
```

#### `ConfigurationSetAlreadyExistsException`

``` purescript
newtype ConfigurationSetAlreadyExistsException
  = ConfigurationSetAlreadyExistsException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Indicates that the configuration set could not be created because of a naming conflict.</p>

##### Instances
``` purescript
Newtype ConfigurationSetAlreadyExistsException _
```

#### `ConfigurationSetAttribute`

``` purescript
newtype ConfigurationSetAttribute
  = ConfigurationSetAttribute String
```

##### Instances
``` purescript
Newtype ConfigurationSetAttribute _
```

#### `ConfigurationSetAttributeList`

``` purescript
newtype ConfigurationSetAttributeList
  = ConfigurationSetAttributeList (Array ConfigurationSetAttribute)
```

##### Instances
``` purescript
Newtype ConfigurationSetAttributeList _
```

#### `ConfigurationSetDoesNotExistException`

``` purescript
newtype ConfigurationSetDoesNotExistException
  = ConfigurationSetDoesNotExistException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Indicates that the configuration set does not exist.</p>

##### Instances
``` purescript
Newtype ConfigurationSetDoesNotExistException _
```

#### `ConfigurationSetName`

``` purescript
newtype ConfigurationSetName
  = ConfigurationSetName String
```

##### Instances
``` purescript
Newtype ConfigurationSetName _
```

#### `ConfigurationSetSendingPausedException`

``` purescript
newtype ConfigurationSetSendingPausedException
  = ConfigurationSetSendingPausedException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Indicates that email sending is disabled for the configuration set.</p> <p>You can enable or disable email sending for a configuration set using <a>UpdateConfigurationSetSendingEnabled</a>.</p>

##### Instances
``` purescript
Newtype ConfigurationSetSendingPausedException _
```

#### `ConfigurationSets`

``` purescript
newtype ConfigurationSets
  = ConfigurationSets (Array ConfigurationSet)
```

##### Instances
``` purescript
Newtype ConfigurationSets _
```

#### `Content`

``` purescript
newtype Content
  = Content { "Data" :: MessageData, "Charset" :: NullOrUndefined (Charset) }
```

<p>Represents textual data, plus an optional character set specification.</p> <p>By default, the text must be 7-bit ASCII, due to the constraints of the SMTP protocol. If the text must contain any other characters, then you must also specify a character set. Examples include UTF-8, ISO-8859-1, and Shift_JIS.</p>

##### Instances
``` purescript
Newtype Content _
```

#### `Counter`

``` purescript
newtype Counter
  = Counter Number
```

##### Instances
``` purescript
Newtype Counter _
```

#### `CreateConfigurationSetEventDestinationRequest`

``` purescript
newtype CreateConfigurationSetEventDestinationRequest
  = CreateConfigurationSetEventDestinationRequest { "ConfigurationSetName" :: ConfigurationSetName, "EventDestination" :: EventDestination }
```

<p>Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateConfigurationSetEventDestinationRequest _
```

#### `CreateConfigurationSetEventDestinationResponse`

``` purescript
newtype CreateConfigurationSetEventDestinationResponse
  = CreateConfigurationSetEventDestinationResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateConfigurationSetEventDestinationResponse _
```

#### `CreateConfigurationSetRequest`

``` purescript
newtype CreateConfigurationSetRequest
  = CreateConfigurationSetRequest { "ConfigurationSet" :: ConfigurationSet }
```

<p>Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateConfigurationSetRequest _
```

#### `CreateConfigurationSetResponse`

``` purescript
newtype CreateConfigurationSetResponse
  = CreateConfigurationSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateConfigurationSetResponse _
```

#### `CreateConfigurationSetTrackingOptionsRequest`

``` purescript
newtype CreateConfigurationSetTrackingOptionsRequest
  = CreateConfigurationSetTrackingOptionsRequest { "ConfigurationSetName" :: ConfigurationSetName, "TrackingOptions" :: TrackingOptions }
```

<p>Represents a request to create an open and click tracking option object in a configuration set. </p>

##### Instances
``` purescript
Newtype CreateConfigurationSetTrackingOptionsRequest _
```

#### `CreateConfigurationSetTrackingOptionsResponse`

``` purescript
newtype CreateConfigurationSetTrackingOptionsResponse
  = CreateConfigurationSetTrackingOptionsResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateConfigurationSetTrackingOptionsResponse _
```

#### `CreateCustomVerificationEmailTemplateRequest`

``` purescript
newtype CreateCustomVerificationEmailTemplateRequest
  = CreateCustomVerificationEmailTemplateRequest { "TemplateName" :: TemplateName, "FromEmailAddress" :: FromAddress, "TemplateSubject" :: Subject, "TemplateContent" :: TemplateContent, "SuccessRedirectionURL" :: SuccessRedirectionURL, "FailureRedirectionURL" :: FailureRedirectionURL }
```

<p>Represents a request to create a custom verification email template.</p>

##### Instances
``` purescript
Newtype CreateCustomVerificationEmailTemplateRequest _
```

#### `CreateReceiptFilterRequest`

``` purescript
newtype CreateReceiptFilterRequest
  = CreateReceiptFilterRequest { "Filter" :: ReceiptFilter }
```

<p>Represents a request to create a new IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateReceiptFilterRequest _
```

#### `CreateReceiptFilterResponse`

``` purescript
newtype CreateReceiptFilterResponse
  = CreateReceiptFilterResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateReceiptFilterResponse _
```

#### `CreateReceiptRuleRequest`

``` purescript
newtype CreateReceiptRuleRequest
  = CreateReceiptRuleRequest { "RuleSetName" :: ReceiptRuleSetName, "After" :: NullOrUndefined (ReceiptRuleName), "Rule" :: ReceiptRule }
```

<p>Represents a request to create a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateReceiptRuleRequest _
```

#### `CreateReceiptRuleResponse`

``` purescript
newtype CreateReceiptRuleResponse
  = CreateReceiptRuleResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateReceiptRuleResponse _
```

#### `CreateReceiptRuleSetRequest`

``` purescript
newtype CreateReceiptRuleSetRequest
  = CreateReceiptRuleSetRequest { "RuleSetName" :: ReceiptRuleSetName }
```

<p>Represents a request to create an empty receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateReceiptRuleSetRequest _
```

#### `CreateReceiptRuleSetResponse`

``` purescript
newtype CreateReceiptRuleSetResponse
  = CreateReceiptRuleSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype CreateReceiptRuleSetResponse _
```

#### `CreateTemplateRequest`

``` purescript
newtype CreateTemplateRequest
  = CreateTemplateRequest { "Template" :: Template }
```

<p>Represents a request to create an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype CreateTemplateRequest _
```

#### `CreateTemplateResponse`

``` purescript
newtype CreateTemplateResponse
  = CreateTemplateResponse {  }
```

##### Instances
``` purescript
Newtype CreateTemplateResponse _
```

#### `CustomMailFromStatus`

``` purescript
newtype CustomMailFromStatus
  = CustomMailFromStatus String
```

##### Instances
``` purescript
Newtype CustomMailFromStatus _
```

#### `CustomRedirectDomain`

``` purescript
newtype CustomRedirectDomain
  = CustomRedirectDomain String
```

##### Instances
``` purescript
Newtype CustomRedirectDomain _
```

#### `CustomVerificationEmailInvalidContentException`

``` purescript
newtype CustomVerificationEmailInvalidContentException
  = CustomVerificationEmailInvalidContentException {  }
```

<p>Indicates that custom verification email template provided content is invalid.</p>

##### Instances
``` purescript
Newtype CustomVerificationEmailInvalidContentException _
```

#### `CustomVerificationEmailTemplate`

``` purescript
newtype CustomVerificationEmailTemplate
  = CustomVerificationEmailTemplate { "TemplateName" :: NullOrUndefined (TemplateName), "FromEmailAddress" :: NullOrUndefined (FromAddress), "TemplateSubject" :: NullOrUndefined (Subject), "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL), "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL) }
```

<p>Contains information about a custom verification email template.</p>

##### Instances
``` purescript
Newtype CustomVerificationEmailTemplate _
```

#### `CustomVerificationEmailTemplateAlreadyExistsException`

``` purescript
newtype CustomVerificationEmailTemplateAlreadyExistsException
  = CustomVerificationEmailTemplateAlreadyExistsException { "CustomVerificationEmailTemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that a custom verification email template with the name you specified already exists.</p>

##### Instances
``` purescript
Newtype CustomVerificationEmailTemplateAlreadyExistsException _
```

#### `CustomVerificationEmailTemplateDoesNotExistException`

``` purescript
newtype CustomVerificationEmailTemplateDoesNotExistException
  = CustomVerificationEmailTemplateDoesNotExistException { "CustomVerificationEmailTemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that a custom verification email template with the name you specified does not exist.</p>

##### Instances
``` purescript
Newtype CustomVerificationEmailTemplateDoesNotExistException _
```

#### `CustomVerificationEmailTemplates`

``` purescript
newtype CustomVerificationEmailTemplates
  = CustomVerificationEmailTemplates (Array CustomVerificationEmailTemplate)
```

##### Instances
``` purescript
Newtype CustomVerificationEmailTemplates _
```

#### `DefaultDimensionValue`

``` purescript
newtype DefaultDimensionValue
  = DefaultDimensionValue String
```

##### Instances
``` purescript
Newtype DefaultDimensionValue _
```

#### `DeleteConfigurationSetEventDestinationRequest`

``` purescript
newtype DeleteConfigurationSetEventDestinationRequest
  = DeleteConfigurationSetEventDestinationRequest { "ConfigurationSetName" :: ConfigurationSetName, "EventDestinationName" :: EventDestinationName }
```

<p>Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetEventDestinationRequest _
```

#### `DeleteConfigurationSetEventDestinationResponse`

``` purescript
newtype DeleteConfigurationSetEventDestinationResponse
  = DeleteConfigurationSetEventDestinationResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetEventDestinationResponse _
```

#### `DeleteConfigurationSetRequest`

``` purescript
newtype DeleteConfigurationSetRequest
  = DeleteConfigurationSetRequest { "ConfigurationSetName" :: ConfigurationSetName }
```

<p>Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetRequest _
```

#### `DeleteConfigurationSetResponse`

``` purescript
newtype DeleteConfigurationSetResponse
  = DeleteConfigurationSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetResponse _
```

#### `DeleteConfigurationSetTrackingOptionsRequest`

``` purescript
newtype DeleteConfigurationSetTrackingOptionsRequest
  = DeleteConfigurationSetTrackingOptionsRequest { "ConfigurationSetName" :: ConfigurationSetName }
```

<p>Represents a request to delete open and click tracking options in a configuration set. </p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetTrackingOptionsRequest _
```

#### `DeleteConfigurationSetTrackingOptionsResponse`

``` purescript
newtype DeleteConfigurationSetTrackingOptionsResponse
  = DeleteConfigurationSetTrackingOptionsResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteConfigurationSetTrackingOptionsResponse _
```

#### `DeleteCustomVerificationEmailTemplateRequest`

``` purescript
newtype DeleteCustomVerificationEmailTemplateRequest
  = DeleteCustomVerificationEmailTemplateRequest { "TemplateName" :: TemplateName }
```

<p>Represents a request to delete an existing custom verification email template.</p>

##### Instances
``` purescript
Newtype DeleteCustomVerificationEmailTemplateRequest _
```

#### `DeleteIdentityPolicyRequest`

``` purescript
newtype DeleteIdentityPolicyRequest
  = DeleteIdentityPolicyRequest { "Identity" :: Identity, "PolicyName" :: PolicyName }
```

<p>Represents a request to delete a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteIdentityPolicyRequest _
```

#### `DeleteIdentityPolicyResponse`

``` purescript
newtype DeleteIdentityPolicyResponse
  = DeleteIdentityPolicyResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteIdentityPolicyResponse _
```

#### `DeleteIdentityRequest`

``` purescript
newtype DeleteIdentityRequest
  = DeleteIdentityRequest { "Identity" :: Identity }
```

<p>Represents a request to delete one of your Amazon SES identities (an email address or domain).</p>

##### Instances
``` purescript
Newtype DeleteIdentityRequest _
```

#### `DeleteIdentityResponse`

``` purescript
newtype DeleteIdentityResponse
  = DeleteIdentityResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteIdentityResponse _
```

#### `DeleteReceiptFilterRequest`

``` purescript
newtype DeleteReceiptFilterRequest
  = DeleteReceiptFilterRequest { "FilterName" :: ReceiptFilterName }
```

<p>Represents a request to delete an IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteReceiptFilterRequest _
```

#### `DeleteReceiptFilterResponse`

``` purescript
newtype DeleteReceiptFilterResponse
  = DeleteReceiptFilterResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteReceiptFilterResponse _
```

#### `DeleteReceiptRuleRequest`

``` purescript
newtype DeleteReceiptRuleRequest
  = DeleteReceiptRuleRequest { "RuleSetName" :: ReceiptRuleSetName, "RuleName" :: ReceiptRuleName }
```

<p>Represents a request to delete a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteReceiptRuleRequest _
```

#### `DeleteReceiptRuleResponse`

``` purescript
newtype DeleteReceiptRuleResponse
  = DeleteReceiptRuleResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteReceiptRuleResponse _
```

#### `DeleteReceiptRuleSetRequest`

``` purescript
newtype DeleteReceiptRuleSetRequest
  = DeleteReceiptRuleSetRequest { "RuleSetName" :: ReceiptRuleSetName }
```

<p>Represents a request to delete a receipt rule set and all of the receipt rules it contains. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteReceiptRuleSetRequest _
```

#### `DeleteReceiptRuleSetResponse`

``` purescript
newtype DeleteReceiptRuleSetResponse
  = DeleteReceiptRuleSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype DeleteReceiptRuleSetResponse _
```

#### `DeleteTemplateRequest`

``` purescript
newtype DeleteTemplateRequest
  = DeleteTemplateRequest { "TemplateName" :: TemplateName }
```

<p>Represents a request to delete an email template. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DeleteTemplateRequest _
```

#### `DeleteTemplateResponse`

``` purescript
newtype DeleteTemplateResponse
  = DeleteTemplateResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTemplateResponse _
```

#### `DeleteVerifiedEmailAddressRequest`

``` purescript
newtype DeleteVerifiedEmailAddressRequest
  = DeleteVerifiedEmailAddressRequest { "EmailAddress" :: Address }
```

<p>Represents a request to delete an email address from the list of email addresses you have attempted to verify under your AWS account.</p>

##### Instances
``` purescript
Newtype DeleteVerifiedEmailAddressRequest _
```

#### `DescribeActiveReceiptRuleSetRequest`

``` purescript
newtype DescribeActiveReceiptRuleSetRequest
  = DescribeActiveReceiptRuleSetRequest {  }
```

<p>Represents a request to return the metadata and receipt rules for the receipt rule set that is currently active. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DescribeActiveReceiptRuleSetRequest _
```

#### `DescribeActiveReceiptRuleSetResponse`

``` purescript
newtype DescribeActiveReceiptRuleSetResponse
  = DescribeActiveReceiptRuleSetResponse { "Metadata" :: NullOrUndefined (ReceiptRuleSetMetadata), "Rules" :: NullOrUndefined (ReceiptRulesList) }
```

<p>Represents the metadata and receipt rules for the receipt rule set that is currently active.</p>

##### Instances
``` purescript
Newtype DescribeActiveReceiptRuleSetResponse _
```

#### `DescribeConfigurationSetRequest`

``` purescript
newtype DescribeConfigurationSetRequest
  = DescribeConfigurationSetRequest { "ConfigurationSetName" :: ConfigurationSetName, "ConfigurationSetAttributeNames" :: NullOrUndefined (ConfigurationSetAttributeList) }
```

<p>Represents a request to return the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DescribeConfigurationSetRequest _
```

#### `DescribeConfigurationSetResponse`

``` purescript
newtype DescribeConfigurationSetResponse
  = DescribeConfigurationSetResponse { "ConfigurationSet" :: NullOrUndefined (ConfigurationSet), "EventDestinations" :: NullOrUndefined (EventDestinations), "TrackingOptions" :: NullOrUndefined (TrackingOptions), "ReputationOptions" :: NullOrUndefined (ReputationOptions) }
```

<p>Represents the details of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DescribeConfigurationSetResponse _
```

#### `DescribeReceiptRuleRequest`

``` purescript
newtype DescribeReceiptRuleRequest
  = DescribeReceiptRuleRequest { "RuleSetName" :: ReceiptRuleSetName, "RuleName" :: ReceiptRuleName }
```

<p>Represents a request to return the details of a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DescribeReceiptRuleRequest _
```

#### `DescribeReceiptRuleResponse`

``` purescript
newtype DescribeReceiptRuleResponse
  = DescribeReceiptRuleResponse { "Rule" :: NullOrUndefined (ReceiptRule) }
```

<p>Represents the details of a receipt rule.</p>

##### Instances
``` purescript
Newtype DescribeReceiptRuleResponse _
```

#### `DescribeReceiptRuleSetRequest`

``` purescript
newtype DescribeReceiptRuleSetRequest
  = DescribeReceiptRuleSetRequest { "RuleSetName" :: ReceiptRuleSetName }
```

<p>Represents a request to return the details of a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype DescribeReceiptRuleSetRequest _
```

#### `DescribeReceiptRuleSetResponse`

``` purescript
newtype DescribeReceiptRuleSetResponse
  = DescribeReceiptRuleSetResponse { "Metadata" :: NullOrUndefined (ReceiptRuleSetMetadata), "Rules" :: NullOrUndefined (ReceiptRulesList) }
```

<p>Represents the details of the specified receipt rule set.</p>

##### Instances
``` purescript
Newtype DescribeReceiptRuleSetResponse _
```

#### `Destination`

``` purescript
newtype Destination
  = Destination { "ToAddresses" :: NullOrUndefined (AddressList), "CcAddresses" :: NullOrUndefined (AddressList), "BccAddresses" :: NullOrUndefined (AddressList) }
```

<p>Represents the destination of the message, consisting of To:, CC:, and BCC: fields.</p> <note> <p>Amazon SES does not support the SMTPUTF8 extension, as described in <a href="https://tools.ietf.org/html/rfc6531">RFC6531</a>. For this reason, the <i>local part</i> of a destination email address (the part of the email address that precedes the @ sign) may only contain <a href="https://en.wikipedia.org/wiki/Email_address#Local-part">7-bit ASCII characters</a>. If the <i>domain part</i> of an address (the part after the @ sign) contains non-ASCII characters, they must be encoded using Punycode, as described in <a href="https://tools.ietf.org/html/rfc3492.html">RFC3492</a>.</p> </note>

##### Instances
``` purescript
Newtype Destination _
```

#### `DiagnosticCode`

``` purescript
newtype DiagnosticCode
  = DiagnosticCode String
```

##### Instances
``` purescript
Newtype DiagnosticCode _
```

#### `DimensionName`

``` purescript
newtype DimensionName
  = DimensionName String
```

##### Instances
``` purescript
Newtype DimensionName _
```

#### `DimensionValueSource`

``` purescript
newtype DimensionValueSource
  = DimensionValueSource String
```

##### Instances
``` purescript
Newtype DimensionValueSource _
```

#### `DkimAttributes`

``` purescript
newtype DkimAttributes
  = DkimAttributes (Map Identity IdentityDkimAttributes)
```

##### Instances
``` purescript
Newtype DkimAttributes _
```

#### `Domain`

``` purescript
newtype Domain
  = Domain String
```

##### Instances
``` purescript
Newtype Domain _
```

#### `DsnAction`

``` purescript
newtype DsnAction
  = DsnAction String
```

##### Instances
``` purescript
Newtype DsnAction _
```

#### `DsnStatus`

``` purescript
newtype DsnStatus
  = DsnStatus String
```

##### Instances
``` purescript
Newtype DsnStatus _
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

#### `Error`

``` purescript
newtype Error
  = Error String
```

##### Instances
``` purescript
Newtype Error _
```

#### `EventDestination`

``` purescript
newtype EventDestination
  = EventDestination { "Name" :: EventDestinationName, "Enabled" :: NullOrUndefined (Enabled), "MatchingEventTypes" :: EventTypes, "KinesisFirehoseDestination" :: NullOrUndefined (KinesisFirehoseDestination), "CloudWatchDestination" :: NullOrUndefined (CloudWatchDestination), "SNSDestination" :: NullOrUndefined (SNSDestination) }
```

<p>Contains information about the event destination that the specified email sending events will be published to.</p> <note> <p>When you create or update an event destination, you must provide one, and only one, destination. The destination can be Amazon CloudWatch, Amazon Kinesis Firehose or Amazon Simple Notification Service (Amazon SNS).</p> </note> <p>Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype EventDestination _
```

#### `EventDestinationAlreadyExistsException`

``` purescript
newtype EventDestinationAlreadyExistsException
  = EventDestinationAlreadyExistsException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "EventDestinationName" :: NullOrUndefined (EventDestinationName) }
```

<p>Indicates that the event destination could not be created because of a naming conflict.</p>

##### Instances
``` purescript
Newtype EventDestinationAlreadyExistsException _
```

#### `EventDestinationDoesNotExistException`

``` purescript
newtype EventDestinationDoesNotExistException
  = EventDestinationDoesNotExistException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "EventDestinationName" :: NullOrUndefined (EventDestinationName) }
```

<p>Indicates that the event destination does not exist.</p>

##### Instances
``` purescript
Newtype EventDestinationDoesNotExistException _
```

#### `EventDestinationName`

``` purescript
newtype EventDestinationName
  = EventDestinationName String
```

##### Instances
``` purescript
Newtype EventDestinationName _
```

#### `EventDestinations`

``` purescript
newtype EventDestinations
  = EventDestinations (Array EventDestination)
```

##### Instances
``` purescript
Newtype EventDestinations _
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

#### `EventTypes`

``` purescript
newtype EventTypes
  = EventTypes (Array EventType)
```

##### Instances
``` purescript
Newtype EventTypes _
```

#### `Explanation`

``` purescript
newtype Explanation
  = Explanation String
```

##### Instances
``` purescript
Newtype Explanation _
```

#### `ExtensionField`

``` purescript
newtype ExtensionField
  = ExtensionField { "Name" :: ExtensionFieldName, "Value" :: ExtensionFieldValue }
```

<p>Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ExtensionField _
```

#### `ExtensionFieldList`

``` purescript
newtype ExtensionFieldList
  = ExtensionFieldList (Array ExtensionField)
```

##### Instances
``` purescript
Newtype ExtensionFieldList _
```

#### `ExtensionFieldName`

``` purescript
newtype ExtensionFieldName
  = ExtensionFieldName String
```

##### Instances
``` purescript
Newtype ExtensionFieldName _
```

#### `ExtensionFieldValue`

``` purescript
newtype ExtensionFieldValue
  = ExtensionFieldValue String
```

##### Instances
``` purescript
Newtype ExtensionFieldValue _
```

#### `FailureRedirectionURL`

``` purescript
newtype FailureRedirectionURL
  = FailureRedirectionURL String
```

##### Instances
``` purescript
Newtype FailureRedirectionURL _
```

#### `FromAddress`

``` purescript
newtype FromAddress
  = FromAddress String
```

##### Instances
``` purescript
Newtype FromAddress _
```

#### `FromEmailAddressNotVerifiedException`

``` purescript
newtype FromEmailAddressNotVerifiedException
  = FromEmailAddressNotVerifiedException { "FromEmailAddress" :: NullOrUndefined (FromAddress) }
```

<p>Indicates that the sender address specified for a custom verification email is not verified, and is therefore not eligible to send the custom verification email. </p>

##### Instances
``` purescript
Newtype FromEmailAddressNotVerifiedException _
```

#### `GetAccountSendingEnabledResponse`

``` purescript
newtype GetAccountSendingEnabledResponse
  = GetAccountSendingEnabledResponse { "Enabled" :: NullOrUndefined (Enabled) }
```

<p>Represents a request to return the email sending status for your Amazon SES account.</p>

##### Instances
``` purescript
Newtype GetAccountSendingEnabledResponse _
```

#### `GetCustomVerificationEmailTemplateRequest`

``` purescript
newtype GetCustomVerificationEmailTemplateRequest
  = GetCustomVerificationEmailTemplateRequest { "TemplateName" :: TemplateName }
```

<p>Represents a request to retrieve an existing custom verification email template.</p>

##### Instances
``` purescript
Newtype GetCustomVerificationEmailTemplateRequest _
```

#### `GetCustomVerificationEmailTemplateResponse`

``` purescript
newtype GetCustomVerificationEmailTemplateResponse
  = GetCustomVerificationEmailTemplateResponse { "TemplateName" :: NullOrUndefined (TemplateName), "FromEmailAddress" :: NullOrUndefined (FromAddress), "TemplateSubject" :: NullOrUndefined (Subject), "TemplateContent" :: NullOrUndefined (TemplateContent), "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL), "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL) }
```

<p>The content of the custom verification email template.</p>

##### Instances
``` purescript
Newtype GetCustomVerificationEmailTemplateResponse _
```

#### `GetIdentityDkimAttributesRequest`

``` purescript
newtype GetIdentityDkimAttributesRequest
  = GetIdentityDkimAttributesRequest { "Identities" :: IdentityList }
```

<p>Represents a request for the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this request also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published. For more information about Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype GetIdentityDkimAttributesRequest _
```

#### `GetIdentityDkimAttributesResponse`

``` purescript
newtype GetIdentityDkimAttributesResponse
  = GetIdentityDkimAttributesResponse { "DkimAttributes" :: DkimAttributes }
```

<p>Represents the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this response also contains the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published.</p>

##### Instances
``` purescript
Newtype GetIdentityDkimAttributesResponse _
```

#### `GetIdentityMailFromDomainAttributesRequest`

``` purescript
newtype GetIdentityMailFromDomainAttributesRequest
  = GetIdentityMailFromDomainAttributesRequest { "Identities" :: IdentityList }
```

<p>Represents a request to return the Amazon SES custom MAIL FROM attributes for a list of identities. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype GetIdentityMailFromDomainAttributesRequest _
```

#### `GetIdentityMailFromDomainAttributesResponse`

``` purescript
newtype GetIdentityMailFromDomainAttributesResponse
  = GetIdentityMailFromDomainAttributesResponse { "MailFromDomainAttributes" :: MailFromDomainAttributes }
```

<p>Represents the custom MAIL FROM attributes for a list of identities.</p>

##### Instances
``` purescript
Newtype GetIdentityMailFromDomainAttributesResponse _
```

#### `GetIdentityNotificationAttributesRequest`

``` purescript
newtype GetIdentityNotificationAttributesRequest
  = GetIdentityNotificationAttributesRequest { "Identities" :: IdentityList }
```

<p>Represents a request to return the notification attributes for a list of identities you verified with Amazon SES. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype GetIdentityNotificationAttributesRequest _
```

#### `GetIdentityNotificationAttributesResponse`

``` purescript
newtype GetIdentityNotificationAttributesResponse
  = GetIdentityNotificationAttributesResponse { "NotificationAttributes" :: NotificationAttributes }
```

<p>Represents the notification attributes for a list of identities.</p>

##### Instances
``` purescript
Newtype GetIdentityNotificationAttributesResponse _
```

#### `GetIdentityPoliciesRequest`

``` purescript
newtype GetIdentityPoliciesRequest
  = GetIdentityPoliciesRequest { "Identity" :: Identity, "PolicyNames" :: PolicyNameList }
```

<p>Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype GetIdentityPoliciesRequest _
```

#### `GetIdentityPoliciesResponse`

``` purescript
newtype GetIdentityPoliciesResponse
  = GetIdentityPoliciesResponse { "Policies" :: PolicyMap }
```

<p>Represents the requested sending authorization policies.</p>

##### Instances
``` purescript
Newtype GetIdentityPoliciesResponse _
```

#### `GetIdentityVerificationAttributesRequest`

``` purescript
newtype GetIdentityVerificationAttributesRequest
  = GetIdentityVerificationAttributesRequest { "Identities" :: IdentityList }
```

<p>Represents a request to return the Amazon SES verification status of a list of identities. For domain identities, this request also returns the verification token. For information about verifying identities with Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype GetIdentityVerificationAttributesRequest _
```

#### `GetIdentityVerificationAttributesResponse`

``` purescript
newtype GetIdentityVerificationAttributesResponse
  = GetIdentityVerificationAttributesResponse { "VerificationAttributes" :: VerificationAttributes }
```

<p>The Amazon SES verification status of a list of identities. For domain identities, this response also contains the verification token.</p>

##### Instances
``` purescript
Newtype GetIdentityVerificationAttributesResponse _
```

#### `GetSendQuotaResponse`

``` purescript
newtype GetSendQuotaResponse
  = GetSendQuotaResponse { "Max24HourSend" :: NullOrUndefined (Max24HourSend), "MaxSendRate" :: NullOrUndefined (MaxSendRate), "SentLast24Hours" :: NullOrUndefined (SentLast24Hours) }
```

<p>Represents your Amazon SES daily sending quota, maximum send rate, and the number of emails you have sent in the last 24 hours.</p>

##### Instances
``` purescript
Newtype GetSendQuotaResponse _
```

#### `GetSendStatisticsResponse`

``` purescript
newtype GetSendStatisticsResponse
  = GetSendStatisticsResponse { "SendDataPoints" :: NullOrUndefined (SendDataPointList) }
```

<p>Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.</p>

##### Instances
``` purescript
Newtype GetSendStatisticsResponse _
```

#### `GetTemplateRequest`

``` purescript
newtype GetTemplateRequest
  = GetTemplateRequest { "TemplateName" :: TemplateName }
```

##### Instances
``` purescript
Newtype GetTemplateRequest _
```

#### `GetTemplateResponse`

``` purescript
newtype GetTemplateResponse
  = GetTemplateResponse { "Template" :: NullOrUndefined (Template) }
```

##### Instances
``` purescript
Newtype GetTemplateResponse _
```

#### `HeaderName`

``` purescript
newtype HeaderName
  = HeaderName String
```

##### Instances
``` purescript
Newtype HeaderName _
```

#### `HeaderValue`

``` purescript
newtype HeaderValue
  = HeaderValue String
```

##### Instances
``` purescript
Newtype HeaderValue _
```

#### `HtmlPart`

``` purescript
newtype HtmlPart
  = HtmlPart String
```

##### Instances
``` purescript
Newtype HtmlPart _
```

#### `Identity`

``` purescript
newtype Identity
  = Identity String
```

##### Instances
``` purescript
Newtype Identity _
```

#### `IdentityDkimAttributes`

``` purescript
newtype IdentityDkimAttributes
  = IdentityDkimAttributes { "DkimEnabled" :: Enabled, "DkimVerificationStatus" :: VerificationStatus, "DkimTokens" :: NullOrUndefined (VerificationTokenList) }
```

<p>Represents the DKIM attributes of a verified email address or a domain.</p>

##### Instances
``` purescript
Newtype IdentityDkimAttributes _
```

#### `IdentityList`

``` purescript
newtype IdentityList
  = IdentityList (Array Identity)
```

##### Instances
``` purescript
Newtype IdentityList _
```

#### `IdentityMailFromDomainAttributes`

``` purescript
newtype IdentityMailFromDomainAttributes
  = IdentityMailFromDomainAttributes { "MailFromDomain" :: MailFromDomainName, "MailFromDomainStatus" :: CustomMailFromStatus, "BehaviorOnMXFailure" :: BehaviorOnMXFailure }
```

<p>Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).</p>

##### Instances
``` purescript
Newtype IdentityMailFromDomainAttributes _
```

#### `IdentityNotificationAttributes`

``` purescript
newtype IdentityNotificationAttributes
  = IdentityNotificationAttributes { "BounceTopic" :: NotificationTopic, "ComplaintTopic" :: NotificationTopic, "DeliveryTopic" :: NotificationTopic, "ForwardingEnabled" :: Enabled, "HeadersInBounceNotificationsEnabled" :: NullOrUndefined (Enabled), "HeadersInComplaintNotificationsEnabled" :: NullOrUndefined (Enabled), "HeadersInDeliveryNotificationsEnabled" :: NullOrUndefined (Enabled) }
```

<p>Represents the notification attributes of an identity, including whether an identity has Amazon Simple Notification Service (Amazon SNS) topics set for bounce, complaint, and/or delivery notifications, and whether feedback forwarding is enabled for bounce and complaint notifications.</p>

##### Instances
``` purescript
Newtype IdentityNotificationAttributes _
```

#### `IdentityType`

``` purescript
newtype IdentityType
  = IdentityType String
```

##### Instances
``` purescript
Newtype IdentityType _
```

#### `IdentityVerificationAttributes`

``` purescript
newtype IdentityVerificationAttributes
  = IdentityVerificationAttributes { "VerificationStatus" :: VerificationStatus, "VerificationToken" :: NullOrUndefined (VerificationToken) }
```

<p>Represents the verification attributes of a single identity.</p>

##### Instances
``` purescript
Newtype IdentityVerificationAttributes _
```

#### `InvalidCloudWatchDestinationException`

``` purescript
newtype InvalidCloudWatchDestinationException
  = InvalidCloudWatchDestinationException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "EventDestinationName" :: NullOrUndefined (EventDestinationName) }
```

<p>Indicates that the Amazon CloudWatch destination is invalid. See the error message for details.</p>

##### Instances
``` purescript
Newtype InvalidCloudWatchDestinationException _
```

#### `InvalidConfigurationSetException`

``` purescript
newtype InvalidConfigurationSetException
  = InvalidConfigurationSetException {  }
```

<p>Indicates that the configuration set is invalid. See the error message for details.</p>

##### Instances
``` purescript
Newtype InvalidConfigurationSetException _
```

#### `InvalidFirehoseDestinationException`

``` purescript
newtype InvalidFirehoseDestinationException
  = InvalidFirehoseDestinationException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "EventDestinationName" :: NullOrUndefined (EventDestinationName) }
```

<p>Indicates that the Amazon Kinesis Firehose destination is invalid. See the error message for details.</p>

##### Instances
``` purescript
Newtype InvalidFirehoseDestinationException _
```

#### `InvalidLambdaFunctionException`

``` purescript
newtype InvalidLambdaFunctionException
  = InvalidLambdaFunctionException { "FunctionArn" :: NullOrUndefined (AmazonResourceName) }
```

<p>Indicates that the provided AWS Lambda function is invalid, or that Amazon SES could not execute the provided function, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype InvalidLambdaFunctionException _
```

#### `InvalidPolicyException`

``` purescript
newtype InvalidPolicyException
  = InvalidPolicyException {  }
```

<p>Indicates that the provided policy is invalid. Check the error stack for more information about what caused the error.</p>

##### Instances
``` purescript
Newtype InvalidPolicyException _
```

#### `InvalidRenderingParameterException`

``` purescript
newtype InvalidRenderingParameterException
  = InvalidRenderingParameterException { "TemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that one or more of the replacement values you provided is invalid. This error may occur when the TemplateData object contains invalid JSON.</p>

##### Instances
``` purescript
Newtype InvalidRenderingParameterException _
```

#### `InvalidS3ConfigurationException`

``` purescript
newtype InvalidS3ConfigurationException
  = InvalidS3ConfigurationException { "Bucket" :: NullOrUndefined (S3BucketName) }
```

<p>Indicates that the provided Amazon S3 bucket or AWS KMS encryption key is invalid, or that Amazon SES could not publish to the bucket, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype InvalidS3ConfigurationException _
```

#### `InvalidSNSDestinationException`

``` purescript
newtype InvalidSNSDestinationException
  = InvalidSNSDestinationException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "EventDestinationName" :: NullOrUndefined (EventDestinationName) }
```

<p>Indicates that the Amazon Simple Notification Service (Amazon SNS) destination is invalid. See the error message for details.</p>

##### Instances
``` purescript
Newtype InvalidSNSDestinationException _
```

#### `InvalidSnsTopicException`

``` purescript
newtype InvalidSnsTopicException
  = InvalidSnsTopicException { "Topic" :: NullOrUndefined (AmazonResourceName) }
```

<p>Indicates that the provided Amazon SNS topic is invalid, or that Amazon SES could not publish to the topic, possibly due to permissions issues. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype InvalidSnsTopicException _
```

#### `InvalidTemplateException`

``` purescript
newtype InvalidTemplateException
  = InvalidTemplateException { "TemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that a template could not be created because it contained invalid JSON.</p>

##### Instances
``` purescript
Newtype InvalidTemplateException _
```

#### `InvalidTrackingOptionsException`

``` purescript
newtype InvalidTrackingOptionsException
  = InvalidTrackingOptionsException {  }
```

<p>Indicates that the custom domain to be used for open and click tracking redirects is invalid. This error appears most often in the following situations:</p> <ul> <li> <p>When the tracking domain you specified is not verified in Amazon SES.</p> </li> <li> <p>When the tracking domain you specified is not a valid domain or subdomain.</p> </li> </ul>

##### Instances
``` purescript
Newtype InvalidTrackingOptionsException _
```

#### `InvocationType`

``` purescript
newtype InvocationType
  = InvocationType String
```

##### Instances
``` purescript
Newtype InvocationType _
```

#### `KinesisFirehoseDestination`

``` purescript
newtype KinesisFirehoseDestination
  = KinesisFirehoseDestination { "IAMRoleARN" :: AmazonResourceName, "DeliveryStreamARN" :: AmazonResourceName }
```

<p>Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.</p> <p>Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype KinesisFirehoseDestination _
```

#### `LambdaAction`

``` purescript
newtype LambdaAction
  = LambdaAction { "TopicArn" :: NullOrUndefined (AmazonResourceName), "FunctionArn" :: AmazonResourceName, "InvocationType" :: NullOrUndefined (InvocationType) }
```

<p>When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <p>For information about using AWS Lambda actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype LambdaAction _
```

#### `LastAttemptDate`

``` purescript
newtype LastAttemptDate
  = LastAttemptDate Number
```

##### Instances
``` purescript
Newtype LastAttemptDate _
```

#### `LastFreshStart`

``` purescript
newtype LastFreshStart
  = LastFreshStart Number
```

##### Instances
``` purescript
Newtype LastFreshStart _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>Indicates that a resource could not be created because of service limits. For a list of Amazon SES limits, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/limits.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListConfigurationSetsRequest`

``` purescript
newtype ListConfigurationSetsRequest
  = ListConfigurationSetsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxItems" :: NullOrUndefined (MaxItems) }
```

<p>Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ListConfigurationSetsRequest _
```

#### `ListConfigurationSetsResponse`

``` purescript
newtype ListConfigurationSetsResponse
  = ListConfigurationSetsResponse { "ConfigurationSets" :: NullOrUndefined (ConfigurationSets), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ListConfigurationSetsResponse _
```

#### `ListCustomVerificationEmailTemplatesRequest`

``` purescript
newtype ListCustomVerificationEmailTemplatesRequest
  = ListCustomVerificationEmailTemplatesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

<p>Represents a request to list the existing custom verification email templates for your account.</p> <p>For more information about custom verification email templates, see <a href="https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html">Using Custom Verification Email Templates</a> in the <i>Amazon SES Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype ListCustomVerificationEmailTemplatesRequest _
```

#### `ListCustomVerificationEmailTemplatesResponse`

``` purescript
newtype ListCustomVerificationEmailTemplatesResponse
  = ListCustomVerificationEmailTemplatesResponse { "CustomVerificationEmailTemplates" :: NullOrUndefined (CustomVerificationEmailTemplates), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>A paginated list of custom verification email templates.</p>

##### Instances
``` purescript
Newtype ListCustomVerificationEmailTemplatesResponse _
```

#### `ListIdentitiesRequest`

``` purescript
newtype ListIdentitiesRequest
  = ListIdentitiesRequest { "IdentityType" :: NullOrUndefined (IdentityType), "NextToken" :: NullOrUndefined (NextToken), "MaxItems" :: NullOrUndefined (MaxItems) }
```

<p>Represents a request to return a list of all identities (email addresses and domains) that you have attempted to verify under your AWS account, regardless of verification status.</p>

##### Instances
``` purescript
Newtype ListIdentitiesRequest _
```

#### `ListIdentitiesResponse`

``` purescript
newtype ListIdentitiesResponse
  = ListIdentitiesResponse { "Identities" :: IdentityList, "NextToken" :: NullOrUndefined (NextToken) }
```

<p>A list of all identities that you have attempted to verify under your AWS account, regardless of verification status.</p>

##### Instances
``` purescript
Newtype ListIdentitiesResponse _
```

#### `ListIdentityPoliciesRequest`

``` purescript
newtype ListIdentityPoliciesRequest
  = ListIdentityPoliciesRequest { "Identity" :: Identity }
```

<p>Represents a request to return a list of sending authorization policies that are attached to an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ListIdentityPoliciesRequest _
```

#### `ListIdentityPoliciesResponse`

``` purescript
newtype ListIdentityPoliciesResponse
  = ListIdentityPoliciesResponse { "PolicyNames" :: PolicyNameList }
```

<p>A list of names of sending authorization policies that apply to an identity.</p>

##### Instances
``` purescript
Newtype ListIdentityPoliciesResponse _
```

#### `ListReceiptFiltersRequest`

``` purescript
newtype ListReceiptFiltersRequest
  = ListReceiptFiltersRequest {  }
```

<p>Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ListReceiptFiltersRequest _
```

#### `ListReceiptFiltersResponse`

``` purescript
newtype ListReceiptFiltersResponse
  = ListReceiptFiltersResponse { "Filters" :: NullOrUndefined (ReceiptFilterList) }
```

<p>A list of IP address filters that exist under your AWS account.</p>

##### Instances
``` purescript
Newtype ListReceiptFiltersResponse _
```

#### `ListReceiptRuleSetsRequest`

``` purescript
newtype ListReceiptRuleSetsRequest
  = ListReceiptRuleSetsRequest { "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Represents a request to list the receipt rule sets that exist under your AWS account. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ListReceiptRuleSetsRequest _
```

#### `ListReceiptRuleSetsResponse`

``` purescript
newtype ListReceiptRuleSetsResponse
  = ListReceiptRuleSetsResponse { "RuleSets" :: NullOrUndefined (ReceiptRuleSetsLists), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>A list of receipt rule sets that exist under your AWS account.</p>

##### Instances
``` purescript
Newtype ListReceiptRuleSetsResponse _
```

#### `ListTemplatesRequest`

``` purescript
newtype ListTemplatesRequest
  = ListTemplatesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxItems" :: NullOrUndefined (MaxItems) }
```

##### Instances
``` purescript
Newtype ListTemplatesRequest _
```

#### `ListTemplatesResponse`

``` purescript
newtype ListTemplatesResponse
  = ListTemplatesResponse { "TemplatesMetadata" :: NullOrUndefined (TemplateMetadataList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTemplatesResponse _
```

#### `ListVerifiedEmailAddressesResponse`

``` purescript
newtype ListVerifiedEmailAddressesResponse
  = ListVerifiedEmailAddressesResponse { "VerifiedEmailAddresses" :: NullOrUndefined (AddressList) }
```

<p>A list of email addresses that you have verified with Amazon SES under your AWS account.</p>

##### Instances
``` purescript
Newtype ListVerifiedEmailAddressesResponse _
```

#### `MailFromDomainAttributes`

``` purescript
newtype MailFromDomainAttributes
  = MailFromDomainAttributes (Map Identity IdentityMailFromDomainAttributes)
```

##### Instances
``` purescript
Newtype MailFromDomainAttributes _
```

#### `MailFromDomainName`

``` purescript
newtype MailFromDomainName
  = MailFromDomainName String
```

##### Instances
``` purescript
Newtype MailFromDomainName _
```

#### `MailFromDomainNotVerifiedException`

``` purescript
newtype MailFromDomainNotVerifiedException
  = MailFromDomainNotVerifiedException {  }
```

<p> Indicates that the message could not be sent because Amazon SES could not read the MX record required to use the specified MAIL FROM domain. For information about editing the custom MAIL FROM domain settings for an identity, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-edit.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype MailFromDomainNotVerifiedException _
```

#### `Max24HourSend`

``` purescript
newtype Max24HourSend
  = Max24HourSend Number
```

##### Instances
``` purescript
Newtype Max24HourSend _
```

#### `MaxItems`

``` purescript
newtype MaxItems
  = MaxItems Int
```

##### Instances
``` purescript
Newtype MaxItems _
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

#### `MaxSendRate`

``` purescript
newtype MaxSendRate
  = MaxSendRate Number
```

##### Instances
``` purescript
Newtype MaxSendRate _
```

#### `Message`

``` purescript
newtype Message
  = Message { "Subject" :: Content, "Body" :: Body }
```

<p>Represents the message to be sent, composed of a subject and a body.</p>

##### Instances
``` purescript
Newtype Message _
```

#### `MessageData`

``` purescript
newtype MessageData
  = MessageData String
```

##### Instances
``` purescript
Newtype MessageData _
```

#### `MessageDsn`

``` purescript
newtype MessageDsn
  = MessageDsn { "ReportingMta" :: ReportingMta, "ArrivalDate" :: NullOrUndefined (ArrivalDate), "ExtensionFields" :: NullOrUndefined (ExtensionFieldList) }
```

<p>Message-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype MessageDsn _
```

#### `MessageId`

``` purescript
newtype MessageId
  = MessageId String
```

##### Instances
``` purescript
Newtype MessageId _
```

#### `MessageRejected`

``` purescript
newtype MessageRejected
  = MessageRejected {  }
```

<p>Indicates that the action failed, and the message could not be sent. Check the error stack for more information about what caused the error.</p>

##### Instances
``` purescript
Newtype MessageRejected _
```

#### `MessageTag`

``` purescript
newtype MessageTag
  = MessageTag { "Name" :: MessageTagName, "Value" :: MessageTagValue }
```

<p>Contains the name and value of a tag that you can provide to <code>SendEmail</code> or <code>SendRawEmail</code> to apply to an email.</p> <p>Message tags, which you use with configuration sets, enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype MessageTag _
```

#### `MessageTagList`

``` purescript
newtype MessageTagList
  = MessageTagList (Array MessageTag)
```

##### Instances
``` purescript
Newtype MessageTagList _
```

#### `MessageTagName`

``` purescript
newtype MessageTagName
  = MessageTagName String
```

##### Instances
``` purescript
Newtype MessageTagName _
```

#### `MessageTagValue`

``` purescript
newtype MessageTagValue
  = MessageTagValue String
```

##### Instances
``` purescript
Newtype MessageTagValue _
```

#### `MissingRenderingAttributeException`

``` purescript
newtype MissingRenderingAttributeException
  = MissingRenderingAttributeException { "TemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that one or more of the replacement values for the specified template was not specified. Ensure that the TemplateData object contains references to all of the replacement tags in the specified template.</p>

##### Instances
``` purescript
Newtype MissingRenderingAttributeException _
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

#### `NotificationAttributes`

``` purescript
newtype NotificationAttributes
  = NotificationAttributes (Map Identity IdentityNotificationAttributes)
```

##### Instances
``` purescript
Newtype NotificationAttributes _
```

#### `NotificationTopic`

``` purescript
newtype NotificationTopic
  = NotificationTopic String
```

##### Instances
``` purescript
Newtype NotificationTopic _
```

#### `NotificationType`

``` purescript
newtype NotificationType
  = NotificationType String
```

##### Instances
``` purescript
Newtype NotificationType _
```

#### `Policy`

``` purescript
newtype Policy
  = Policy String
```

##### Instances
``` purescript
Newtype Policy _
```

#### `PolicyMap`

``` purescript
newtype PolicyMap
  = PolicyMap (Map PolicyName Policy)
```

##### Instances
``` purescript
Newtype PolicyMap _
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

#### `PolicyNameList`

``` purescript
newtype PolicyNameList
  = PolicyNameList (Array PolicyName)
```

##### Instances
``` purescript
Newtype PolicyNameList _
```

#### `ProductionAccessNotGrantedException`

``` purescript
newtype ProductionAccessNotGrantedException
  = ProductionAccessNotGrantedException {  }
```

<p>Indicates that the account has not been granted production access.</p>

##### Instances
``` purescript
Newtype ProductionAccessNotGrantedException _
```

#### `PutIdentityPolicyRequest`

``` purescript
newtype PutIdentityPolicyRequest
  = PutIdentityPolicyRequest { "Identity" :: Identity, "PolicyName" :: PolicyName, "Policy" :: Policy }
```

<p>Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype PutIdentityPolicyRequest _
```

#### `PutIdentityPolicyResponse`

``` purescript
newtype PutIdentityPolicyResponse
  = PutIdentityPolicyResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype PutIdentityPolicyResponse _
```

#### `RawMessage`

``` purescript
newtype RawMessage
  = RawMessage { "Data" :: RawMessageData }
```

<p>Represents the raw data of the message.</p>

##### Instances
``` purescript
Newtype RawMessage _
```

#### `RawMessageData`

``` purescript
newtype RawMessageData
  = RawMessageData String
```

##### Instances
``` purescript
Newtype RawMessageData _
```

#### `ReceiptAction`

``` purescript
newtype ReceiptAction
  = ReceiptAction { "S3Action" :: NullOrUndefined (S3Action), "BounceAction" :: NullOrUndefined (BounceAction), "WorkmailAction" :: NullOrUndefined (WorkmailAction), "LambdaAction" :: NullOrUndefined (LambdaAction), "StopAction" :: NullOrUndefined (StopAction), "AddHeaderAction" :: NullOrUndefined (AddHeaderAction), "SNSAction" :: NullOrUndefined (SNSAction) }
```

<p>An action that Amazon SES can take when it receives an email on behalf of one or more email addresses or domains that you own. An instance of this data type can represent only one action.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReceiptAction _
```

#### `ReceiptActionsList`

``` purescript
newtype ReceiptActionsList
  = ReceiptActionsList (Array ReceiptAction)
```

##### Instances
``` purescript
Newtype ReceiptActionsList _
```

#### `ReceiptFilter`

``` purescript
newtype ReceiptFilter
  = ReceiptFilter { "Name" :: ReceiptFilterName, "IpFilter" :: ReceiptIpFilter }
```

<p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReceiptFilter _
```

#### `ReceiptFilterList`

``` purescript
newtype ReceiptFilterList
  = ReceiptFilterList (Array ReceiptFilter)
```

##### Instances
``` purescript
Newtype ReceiptFilterList _
```

#### `ReceiptFilterName`

``` purescript
newtype ReceiptFilterName
  = ReceiptFilterName String
```

##### Instances
``` purescript
Newtype ReceiptFilterName _
```

#### `ReceiptFilterPolicy`

``` purescript
newtype ReceiptFilterPolicy
  = ReceiptFilterPolicy String
```

##### Instances
``` purescript
Newtype ReceiptFilterPolicy _
```

#### `ReceiptIpFilter`

``` purescript
newtype ReceiptIpFilter
  = ReceiptIpFilter { "Policy" :: ReceiptFilterPolicy, "Cidr" :: Cidr }
```

<p>A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.</p> <p>For information about setting up IP address filters, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReceiptIpFilter _
```

#### `ReceiptRule`

``` purescript
newtype ReceiptRule
  = ReceiptRule { "Name" :: ReceiptRuleName, "Enabled" :: NullOrUndefined (Enabled), "TlsPolicy" :: NullOrUndefined (TlsPolicy), "Recipients" :: NullOrUndefined (RecipientsList), "Actions" :: NullOrUndefined (ReceiptActionsList), "ScanEnabled" :: NullOrUndefined (Enabled) }
```

<p>Receipt rules enable you to specify which actions Amazon SES should take when it receives mail on behalf of one or more email addresses or domains that you own.</p> <p>Each receipt rule defines a set of email addresses or domains that it applies to. If the email addresses or domains match at least one recipient address of the message, Amazon SES executes all of the receipt rule's actions on the message.</p> <p>For information about setting up receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rules.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReceiptRule _
```

#### `ReceiptRuleName`

``` purescript
newtype ReceiptRuleName
  = ReceiptRuleName String
```

##### Instances
``` purescript
Newtype ReceiptRuleName _
```

#### `ReceiptRuleNamesList`

``` purescript
newtype ReceiptRuleNamesList
  = ReceiptRuleNamesList (Array ReceiptRuleName)
```

##### Instances
``` purescript
Newtype ReceiptRuleNamesList _
```

#### `ReceiptRuleSetMetadata`

``` purescript
newtype ReceiptRuleSetMetadata
  = ReceiptRuleSetMetadata { "Name" :: NullOrUndefined (ReceiptRuleSetName), "CreatedTimestamp" :: NullOrUndefined (Number) }
```

<p>Information about a receipt rule set.</p> <p>A receipt rule set is a collection of rules that specify what Amazon SES should do with mail it receives on behalf of your account's verified domains.</p> <p>For information about setting up receipt rule sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReceiptRuleSetMetadata _
```

#### `ReceiptRuleSetName`

``` purescript
newtype ReceiptRuleSetName
  = ReceiptRuleSetName String
```

##### Instances
``` purescript
Newtype ReceiptRuleSetName _
```

#### `ReceiptRuleSetsLists`

``` purescript
newtype ReceiptRuleSetsLists
  = ReceiptRuleSetsLists (Array ReceiptRuleSetMetadata)
```

##### Instances
``` purescript
Newtype ReceiptRuleSetsLists _
```

#### `ReceiptRulesList`

``` purescript
newtype ReceiptRulesList
  = ReceiptRulesList (Array ReceiptRule)
```

##### Instances
``` purescript
Newtype ReceiptRulesList _
```

#### `Recipient`

``` purescript
newtype Recipient
  = Recipient String
```

##### Instances
``` purescript
Newtype Recipient _
```

#### `RecipientDsnFields`

``` purescript
newtype RecipientDsnFields
  = RecipientDsnFields { "FinalRecipient" :: NullOrUndefined (Address), "Action" :: DsnAction, "RemoteMta" :: NullOrUndefined (RemoteMta), "Status" :: DsnStatus, "DiagnosticCode" :: NullOrUndefined (DiagnosticCode), "LastAttemptDate" :: NullOrUndefined (LastAttemptDate), "ExtensionFields" :: NullOrUndefined (ExtensionFieldList) }
```

<p>Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.</p> <p>For information about receiving email through Amazon SES, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype RecipientDsnFields _
```

#### `RecipientsList`

``` purescript
newtype RecipientsList
  = RecipientsList (Array Recipient)
```

##### Instances
``` purescript
Newtype RecipientsList _
```

#### `RemoteMta`

``` purescript
newtype RemoteMta
  = RemoteMta String
```

##### Instances
``` purescript
Newtype RemoteMta _
```

#### `RenderedTemplate`

``` purescript
newtype RenderedTemplate
  = RenderedTemplate String
```

##### Instances
``` purescript
Newtype RenderedTemplate _
```

#### `ReorderReceiptRuleSetRequest`

``` purescript
newtype ReorderReceiptRuleSetRequest
  = ReorderReceiptRuleSetRequest { "RuleSetName" :: ReceiptRuleSetName, "RuleNames" :: ReceiptRuleNamesList }
```

<p>Represents a request to reorder the receipt rules within a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype ReorderReceiptRuleSetRequest _
```

#### `ReorderReceiptRuleSetResponse`

``` purescript
newtype ReorderReceiptRuleSetResponse
  = ReorderReceiptRuleSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype ReorderReceiptRuleSetResponse _
```

#### `ReportingMta`

``` purescript
newtype ReportingMta
  = ReportingMta String
```

##### Instances
``` purescript
Newtype ReportingMta _
```

#### `ReputationOptions`

``` purescript
newtype ReputationOptions
  = ReputationOptions { "SendingEnabled" :: NullOrUndefined (Enabled), "ReputationMetricsEnabled" :: NullOrUndefined (Enabled), "LastFreshStart" :: NullOrUndefined (LastFreshStart) }
```

<p>Contains information about the reputation settings for a configuration set.</p>

##### Instances
``` purescript
Newtype ReputationOptions _
```

#### `RuleDoesNotExistException`

``` purescript
newtype RuleDoesNotExistException
  = RuleDoesNotExistException { "Name" :: NullOrUndefined (RuleOrRuleSetName) }
```

<p>Indicates that the provided receipt rule does not exist.</p>

##### Instances
``` purescript
Newtype RuleDoesNotExistException _
```

#### `RuleOrRuleSetName`

``` purescript
newtype RuleOrRuleSetName
  = RuleOrRuleSetName String
```

##### Instances
``` purescript
Newtype RuleOrRuleSetName _
```

#### `RuleSetDoesNotExistException`

``` purescript
newtype RuleSetDoesNotExistException
  = RuleSetDoesNotExistException { "Name" :: NullOrUndefined (RuleOrRuleSetName) }
```

<p>Indicates that the provided receipt rule set does not exist.</p>

##### Instances
``` purescript
Newtype RuleSetDoesNotExistException _
```

#### `S3Action`

``` purescript
newtype S3Action
  = S3Action { "TopicArn" :: NullOrUndefined (AmazonResourceName), "BucketName" :: S3BucketName, "ObjectKeyPrefix" :: NullOrUndefined (S3KeyPrefix), "KmsKeyArn" :: NullOrUndefined (AmazonResourceName) }
```

<p>When included in a receipt rule, this action saves the received message to an Amazon Simple Storage Service (Amazon S3) bucket and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>To enable Amazon SES to write emails to your Amazon S3 bucket, use an AWS KMS key to encrypt your emails, or publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <note> <p>When you save your emails to an Amazon S3 bucket, the maximum email size (including headers) is 30 MB. Emails larger than that will bounce.</p> </note> <p>For information about specifying Amazon S3 actions in receipt rules, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-s3.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype S3Action _
```

#### `S3BucketName`

``` purescript
newtype S3BucketName
  = S3BucketName String
```

##### Instances
``` purescript
Newtype S3BucketName _
```

#### `S3KeyPrefix`

``` purescript
newtype S3KeyPrefix
  = S3KeyPrefix String
```

##### Instances
``` purescript
Newtype S3KeyPrefix _
```

#### `SNSAction`

``` purescript
newtype SNSAction
  = SNSAction { "TopicArn" :: AmazonResourceName, "Encoding" :: NullOrUndefined (SNSActionEncoding) }
```

<p>When included in a receipt rule, this action publishes a notification to Amazon Simple Notification Service (Amazon SNS). This action includes a complete copy of the email content in the Amazon SNS notifications. Amazon SNS notifications for all other actions simply provide information about the email. They do not include the email content itself.</p> <p>If you own the Amazon SNS topic, you don't need to do anything to give Amazon SES permission to publish emails to it. However, if you don't own the Amazon SNS topic, you need to attach a policy to the topic to give Amazon SES permissions to access it. For information about giving permissions, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html">Amazon SES Developer Guide</a>.</p> <important> <p>You can only publish emails that are 150 KB or less (including the header) to Amazon SNS. Larger emails will bounce. If you anticipate emails larger than 150 KB, use the S3 action instead.</p> </important> <p>For information about using a receipt rule to publish an Amazon SNS notification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-sns.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SNSAction _
```

#### `SNSActionEncoding`

``` purescript
newtype SNSActionEncoding
  = SNSActionEncoding String
```

##### Instances
``` purescript
Newtype SNSActionEncoding _
```

#### `SNSDestination`

``` purescript
newtype SNSDestination
  = SNSDestination { "TopicARN" :: AmazonResourceName }
```

<p>Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.</p> <p>Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SNSDestination _
```

#### `SendBounceRequest`

``` purescript
newtype SendBounceRequest
  = SendBounceRequest { "OriginalMessageId" :: MessageId, "BounceSender" :: Address, "Explanation" :: NullOrUndefined (Explanation), "MessageDsn" :: NullOrUndefined (MessageDsn), "BouncedRecipientInfoList" :: BouncedRecipientInfoList, "BounceSenderArn" :: NullOrUndefined (AmazonResourceName) }
```

<p>Represents a request to send a bounce message to the sender of an email you received through Amazon SES.</p>

##### Instances
``` purescript
Newtype SendBounceRequest _
```

#### `SendBounceResponse`

``` purescript
newtype SendBounceResponse
  = SendBounceResponse { "MessageId" :: NullOrUndefined (MessageId) }
```

<p>Represents a unique message ID.</p>

##### Instances
``` purescript
Newtype SendBounceResponse _
```

#### `SendBulkTemplatedEmailRequest`

``` purescript
newtype SendBulkTemplatedEmailRequest
  = SendBulkTemplatedEmailRequest { "Source" :: Address, "SourceArn" :: NullOrUndefined (AmazonResourceName), "ReplyToAddresses" :: NullOrUndefined (AddressList), "ReturnPath" :: NullOrUndefined (Address), "ReturnPathArn" :: NullOrUndefined (AmazonResourceName), "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "DefaultTags" :: NullOrUndefined (MessageTagList), "Template" :: TemplateName, "TemplateArn" :: NullOrUndefined (AmazonResourceName), "DefaultTemplateData" :: NullOrUndefined (TemplateData), "Destinations" :: BulkEmailDestinationList }
```

<p>Represents a request to send a templated email to multiple destinations using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SendBulkTemplatedEmailRequest _
```

#### `SendBulkTemplatedEmailResponse`

``` purescript
newtype SendBulkTemplatedEmailResponse
  = SendBulkTemplatedEmailResponse { "Status" :: BulkEmailDestinationStatusList }
```

##### Instances
``` purescript
Newtype SendBulkTemplatedEmailResponse _
```

#### `SendCustomVerificationEmailRequest`

``` purescript
newtype SendCustomVerificationEmailRequest
  = SendCustomVerificationEmailRequest { "EmailAddress" :: Address, "TemplateName" :: TemplateName, "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Represents a request to send a custom verification email to a specified recipient.</p>

##### Instances
``` purescript
Newtype SendCustomVerificationEmailRequest _
```

#### `SendCustomVerificationEmailResponse`

``` purescript
newtype SendCustomVerificationEmailResponse
  = SendCustomVerificationEmailResponse { "MessageId" :: NullOrUndefined (MessageId) }
```

<p>The response received when attempting to send the custom verification email.</p>

##### Instances
``` purescript
Newtype SendCustomVerificationEmailResponse _
```

#### `SendDataPoint`

``` purescript
newtype SendDataPoint
  = SendDataPoint { "Number" :: NullOrUndefined (Number), "DeliveryAttempts" :: NullOrUndefined (Counter), "Bounces" :: NullOrUndefined (Counter), "Complaints" :: NullOrUndefined (Counter), "Rejects" :: NullOrUndefined (Counter) }
```

<p>Represents sending statistics data. Each <code>SendDataPoint</code> contains statistics for a 15-minute period of sending activity. </p>

##### Instances
``` purescript
Newtype SendDataPoint _
```

#### `SendDataPointList`

``` purescript
newtype SendDataPointList
  = SendDataPointList (Array SendDataPoint)
```

##### Instances
``` purescript
Newtype SendDataPointList _
```

#### `SendEmailRequest`

``` purescript
newtype SendEmailRequest
  = SendEmailRequest { "Source" :: Address, "Destination" :: Destination, "Message" :: Message, "ReplyToAddresses" :: NullOrUndefined (AddressList), "ReturnPath" :: NullOrUndefined (Address), "SourceArn" :: NullOrUndefined (AmazonResourceName), "ReturnPathArn" :: NullOrUndefined (AmazonResourceName), "Tags" :: NullOrUndefined (MessageTagList), "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Represents a request to send a single formatted email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-formatted.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SendEmailRequest _
```

#### `SendEmailResponse`

``` purescript
newtype SendEmailResponse
  = SendEmailResponse { "MessageId" :: MessageId }
```

<p>Represents a unique message ID.</p>

##### Instances
``` purescript
Newtype SendEmailResponse _
```

#### `SendRawEmailRequest`

``` purescript
newtype SendRawEmailRequest
  = SendRawEmailRequest { "Source" :: NullOrUndefined (Address), "Destinations" :: NullOrUndefined (AddressList), "RawMessage" :: RawMessage, "FromArn" :: NullOrUndefined (AmazonResourceName), "SourceArn" :: NullOrUndefined (AmazonResourceName), "ReturnPathArn" :: NullOrUndefined (AmazonResourceName), "Tags" :: NullOrUndefined (MessageTagList), "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Represents a request to send a single raw email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-raw.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SendRawEmailRequest _
```

#### `SendRawEmailResponse`

``` purescript
newtype SendRawEmailResponse
  = SendRawEmailResponse { "MessageId" :: MessageId }
```

<p>Represents a unique message ID.</p>

##### Instances
``` purescript
Newtype SendRawEmailResponse _
```

#### `SendTemplatedEmailRequest`

``` purescript
newtype SendTemplatedEmailRequest
  = SendTemplatedEmailRequest { "Source" :: Address, "Destination" :: Destination, "ReplyToAddresses" :: NullOrUndefined (AddressList), "ReturnPath" :: NullOrUndefined (Address), "SourceArn" :: NullOrUndefined (AmazonResourceName), "ReturnPathArn" :: NullOrUndefined (AmazonResourceName), "Tags" :: NullOrUndefined (MessageTagList), "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName), "Template" :: TemplateName, "TemplateArn" :: NullOrUndefined (AmazonResourceName), "TemplateData" :: TemplateData }
```

<p>Represents a request to send a templated email using Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SendTemplatedEmailRequest _
```

#### `SendTemplatedEmailResponse`

``` purescript
newtype SendTemplatedEmailResponse
  = SendTemplatedEmailResponse { "MessageId" :: MessageId }
```

##### Instances
``` purescript
Newtype SendTemplatedEmailResponse _
```

#### `SentLast24Hours`

``` purescript
newtype SentLast24Hours
  = SentLast24Hours Number
```

##### Instances
``` purescript
Newtype SentLast24Hours _
```

#### `SetActiveReceiptRuleSetRequest`

``` purescript
newtype SetActiveReceiptRuleSetRequest
  = SetActiveReceiptRuleSetRequest { "RuleSetName" :: NullOrUndefined (ReceiptRuleSetName) }
```

<p>Represents a request to set a receipt rule set as the active receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetActiveReceiptRuleSetRequest _
```

#### `SetActiveReceiptRuleSetResponse`

``` purescript
newtype SetActiveReceiptRuleSetResponse
  = SetActiveReceiptRuleSetResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetActiveReceiptRuleSetResponse _
```

#### `SetIdentityDkimEnabledRequest`

``` purescript
newtype SetIdentityDkimEnabledRequest
  = SetIdentityDkimEnabledRequest { "Identity" :: Identity, "DkimEnabled" :: Enabled }
```

<p>Represents a request to enable or disable Amazon SES Easy DKIM signing for an identity. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetIdentityDkimEnabledRequest _
```

#### `SetIdentityDkimEnabledResponse`

``` purescript
newtype SetIdentityDkimEnabledResponse
  = SetIdentityDkimEnabledResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetIdentityDkimEnabledResponse _
```

#### `SetIdentityFeedbackForwardingEnabledRequest`

``` purescript
newtype SetIdentityFeedbackForwardingEnabledRequest
  = SetIdentityFeedbackForwardingEnabledRequest { "Identity" :: Identity, "ForwardingEnabled" :: Enabled }
```

<p>Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetIdentityFeedbackForwardingEnabledRequest _
```

#### `SetIdentityFeedbackForwardingEnabledResponse`

``` purescript
newtype SetIdentityFeedbackForwardingEnabledResponse
  = SetIdentityFeedbackForwardingEnabledResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetIdentityFeedbackForwardingEnabledResponse _
```

#### `SetIdentityHeadersInNotificationsEnabledRequest`

``` purescript
newtype SetIdentityHeadersInNotificationsEnabledRequest
  = SetIdentityHeadersInNotificationsEnabledRequest { "Identity" :: Identity, "NotificationType" :: NotificationType, "Enabled" :: Enabled }
```

<p>Represents a request to set whether Amazon SES includes the original email headers in the Amazon SNS notifications of a specified type. For information about notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetIdentityHeadersInNotificationsEnabledRequest _
```

#### `SetIdentityHeadersInNotificationsEnabledResponse`

``` purescript
newtype SetIdentityHeadersInNotificationsEnabledResponse
  = SetIdentityHeadersInNotificationsEnabledResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetIdentityHeadersInNotificationsEnabledResponse _
```

#### `SetIdentityMailFromDomainRequest`

``` purescript
newtype SetIdentityMailFromDomainRequest
  = SetIdentityMailFromDomainRequest { "Identity" :: Identity, "MailFromDomain" :: NullOrUndefined (MailFromDomainName), "BehaviorOnMXFailure" :: NullOrUndefined (BehaviorOnMXFailure) }
```

<p>Represents a request to enable or disable the Amazon SES custom MAIL FROM domain setup for a verified identity. For information about using a custom MAIL FROM domain, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetIdentityMailFromDomainRequest _
```

#### `SetIdentityMailFromDomainResponse`

``` purescript
newtype SetIdentityMailFromDomainResponse
  = SetIdentityMailFromDomainResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetIdentityMailFromDomainResponse _
```

#### `SetIdentityNotificationTopicRequest`

``` purescript
newtype SetIdentityNotificationTopicRequest
  = SetIdentityNotificationTopicRequest { "Identity" :: Identity, "NotificationType" :: NotificationType, "SnsTopic" :: NullOrUndefined (NotificationTopic) }
```

<p>Represents a request to specify the Amazon SNS topic to which Amazon SES will publish bounce, complaint, or delivery notifications for emails sent with that identity as the Source. For information about Amazon SES notifications, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetIdentityNotificationTopicRequest _
```

#### `SetIdentityNotificationTopicResponse`

``` purescript
newtype SetIdentityNotificationTopicResponse
  = SetIdentityNotificationTopicResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetIdentityNotificationTopicResponse _
```

#### `SetReceiptRulePositionRequest`

``` purescript
newtype SetReceiptRulePositionRequest
  = SetReceiptRulePositionRequest { "RuleSetName" :: ReceiptRuleSetName, "RuleName" :: ReceiptRuleName, "After" :: NullOrUndefined (ReceiptRuleName) }
```

<p>Represents a request to set the position of a receipt rule in a receipt rule set. You use receipt rule sets to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype SetReceiptRulePositionRequest _
```

#### `SetReceiptRulePositionResponse`

``` purescript
newtype SetReceiptRulePositionResponse
  = SetReceiptRulePositionResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype SetReceiptRulePositionResponse _
```

#### `StopAction`

``` purescript
newtype StopAction
  = StopAction { "Scope" :: StopScope, "TopicArn" :: NullOrUndefined (AmazonResourceName) }
```

<p>When included in a receipt rule, this action terminates the evaluation of the receipt rule set and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).</p> <p>For information about setting a stop action in a receipt rule, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-stop.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype StopAction _
```

#### `StopScope`

``` purescript
newtype StopScope
  = StopScope String
```

##### Instances
``` purescript
Newtype StopScope _
```

#### `Subject`

``` purescript
newtype Subject
  = Subject String
```

##### Instances
``` purescript
Newtype Subject _
```

#### `SubjectPart`

``` purescript
newtype SubjectPart
  = SubjectPart String
```

##### Instances
``` purescript
Newtype SubjectPart _
```

#### `SuccessRedirectionURL`

``` purescript
newtype SuccessRedirectionURL
  = SuccessRedirectionURL String
```

##### Instances
``` purescript
Newtype SuccessRedirectionURL _
```

#### `Template`

``` purescript
newtype Template
  = Template { "TemplateName" :: TemplateName, "SubjectPart" :: NullOrUndefined (SubjectPart), "TextPart" :: NullOrUndefined (TextPart), "HtmlPart" :: NullOrUndefined (HtmlPart) }
```

<p>The content of the email, composed of a subject line, an HTML part, and a text-only part.</p>

##### Instances
``` purescript
Newtype Template _
```

#### `TemplateContent`

``` purescript
newtype TemplateContent
  = TemplateContent String
```

##### Instances
``` purescript
Newtype TemplateContent _
```

#### `TemplateData`

``` purescript
newtype TemplateData
  = TemplateData String
```

##### Instances
``` purescript
Newtype TemplateData _
```

#### `TemplateDoesNotExistException`

``` purescript
newtype TemplateDoesNotExistException
  = TemplateDoesNotExistException { "TemplateName" :: NullOrUndefined (TemplateName) }
```

<p>Indicates that the Template object you specified does not exist in your Amazon SES account.</p>

##### Instances
``` purescript
Newtype TemplateDoesNotExistException _
```

#### `TemplateMetadata`

``` purescript
newtype TemplateMetadata
  = TemplateMetadata { "Name" :: NullOrUndefined (TemplateName), "CreatedTimestamp" :: NullOrUndefined (Number) }
```

<p>Contains information about an email template.</p>

##### Instances
``` purescript
Newtype TemplateMetadata _
```

#### `TemplateMetadataList`

``` purescript
newtype TemplateMetadataList
  = TemplateMetadataList (Array TemplateMetadata)
```

##### Instances
``` purescript
Newtype TemplateMetadataList _
```

#### `TemplateName`

``` purescript
newtype TemplateName
  = TemplateName String
```

##### Instances
``` purescript
Newtype TemplateName _
```

#### `TestRenderTemplateRequest`

``` purescript
newtype TestRenderTemplateRequest
  = TestRenderTemplateRequest { "TemplateName" :: TemplateName, "TemplateData" :: TemplateData }
```

##### Instances
``` purescript
Newtype TestRenderTemplateRequest _
```

#### `TestRenderTemplateResponse`

``` purescript
newtype TestRenderTemplateResponse
  = TestRenderTemplateResponse { "RenderedTemplate" :: NullOrUndefined (RenderedTemplate) }
```

##### Instances
``` purescript
Newtype TestRenderTemplateResponse _
```

#### `TextPart`

``` purescript
newtype TextPart
  = TextPart String
```

##### Instances
``` purescript
Newtype TextPart _
```

#### `TlsPolicy`

``` purescript
newtype TlsPolicy
  = TlsPolicy String
```

##### Instances
``` purescript
Newtype TlsPolicy _
```

#### `TrackingOptions`

``` purescript
newtype TrackingOptions
  = TrackingOptions { "CustomRedirectDomain" :: NullOrUndefined (CustomRedirectDomain) }
```

<p>A domain that is used to redirect email recipients to an Amazon SES-operated domain. This domain captures open and click events generated by Amazon SES emails.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html">Configuring Custom Domains to Handle Open and Click Tracking</a> in the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype TrackingOptions _
```

#### `TrackingOptionsAlreadyExistsException`

``` purescript
newtype TrackingOptionsAlreadyExistsException
  = TrackingOptionsAlreadyExistsException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Indicates that the configuration set you specified already contains a TrackingOptions object.</p>

##### Instances
``` purescript
Newtype TrackingOptionsAlreadyExistsException _
```

#### `TrackingOptionsDoesNotExistException`

``` purescript
newtype TrackingOptionsDoesNotExistException
  = TrackingOptionsDoesNotExistException { "ConfigurationSetName" :: NullOrUndefined (ConfigurationSetName) }
```

<p>Indicates that the TrackingOptions object you specified does not exist.</p>

##### Instances
``` purescript
Newtype TrackingOptionsDoesNotExistException _
```

#### `UpdateAccountSendingEnabledRequest`

``` purescript
newtype UpdateAccountSendingEnabledRequest
  = UpdateAccountSendingEnabledRequest { "Enabled" :: NullOrUndefined (Enabled) }
```

<p>Represents a request to enable or disable the email sending capabilities for your entire Amazon SES account.</p>

##### Instances
``` purescript
Newtype UpdateAccountSendingEnabledRequest _
```

#### `UpdateConfigurationSetEventDestinationRequest`

``` purescript
newtype UpdateConfigurationSetEventDestinationRequest
  = UpdateConfigurationSetEventDestinationRequest { "ConfigurationSetName" :: ConfigurationSetName, "EventDestination" :: EventDestination }
```

<p>Represents a request to update the event destination of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetEventDestinationRequest _
```

#### `UpdateConfigurationSetEventDestinationResponse`

``` purescript
newtype UpdateConfigurationSetEventDestinationResponse
  = UpdateConfigurationSetEventDestinationResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetEventDestinationResponse _
```

#### `UpdateConfigurationSetReputationMetricsEnabledRequest`

``` purescript
newtype UpdateConfigurationSetReputationMetricsEnabledRequest
  = UpdateConfigurationSetReputationMetricsEnabledRequest { "ConfigurationSetName" :: ConfigurationSetName, "Enabled" :: Enabled }
```

<p>Represents a request to modify the reputation metric publishing settings for a configuration set.</p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetReputationMetricsEnabledRequest _
```

#### `UpdateConfigurationSetSendingEnabledRequest`

``` purescript
newtype UpdateConfigurationSetSendingEnabledRequest
  = UpdateConfigurationSetSendingEnabledRequest { "ConfigurationSetName" :: ConfigurationSetName, "Enabled" :: Enabled }
```

<p>Represents a request to enable or disable the email sending capabilities for a specific configuration set.</p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetSendingEnabledRequest _
```

#### `UpdateConfigurationSetTrackingOptionsRequest`

``` purescript
newtype UpdateConfigurationSetTrackingOptionsRequest
  = UpdateConfigurationSetTrackingOptionsRequest { "ConfigurationSetName" :: ConfigurationSetName, "TrackingOptions" :: TrackingOptions }
```

<p>Represents a request to update the tracking options for a configuration set. </p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetTrackingOptionsRequest _
```

#### `UpdateConfigurationSetTrackingOptionsResponse`

``` purescript
newtype UpdateConfigurationSetTrackingOptionsResponse
  = UpdateConfigurationSetTrackingOptionsResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype UpdateConfigurationSetTrackingOptionsResponse _
```

#### `UpdateCustomVerificationEmailTemplateRequest`

``` purescript
newtype UpdateCustomVerificationEmailTemplateRequest
  = UpdateCustomVerificationEmailTemplateRequest { "TemplateName" :: TemplateName, "FromEmailAddress" :: NullOrUndefined (FromAddress), "TemplateSubject" :: NullOrUndefined (Subject), "TemplateContent" :: NullOrUndefined (TemplateContent), "SuccessRedirectionURL" :: NullOrUndefined (SuccessRedirectionURL), "FailureRedirectionURL" :: NullOrUndefined (FailureRedirectionURL) }
```

<p>Represents a request to update an existing custom verification email template.</p>

##### Instances
``` purescript
Newtype UpdateCustomVerificationEmailTemplateRequest _
```

#### `UpdateReceiptRuleRequest`

``` purescript
newtype UpdateReceiptRuleRequest
  = UpdateReceiptRuleRequest { "RuleSetName" :: ReceiptRuleSetName, "Rule" :: ReceiptRule }
```

<p>Represents a request to update a receipt rule. You use receipt rules to receive email with Amazon SES. For more information, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype UpdateReceiptRuleRequest _
```

#### `UpdateReceiptRuleResponse`

``` purescript
newtype UpdateReceiptRuleResponse
  = UpdateReceiptRuleResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype UpdateReceiptRuleResponse _
```

#### `UpdateTemplateRequest`

``` purescript
newtype UpdateTemplateRequest
  = UpdateTemplateRequest { "Template" :: Template }
```

##### Instances
``` purescript
Newtype UpdateTemplateRequest _
```

#### `UpdateTemplateResponse`

``` purescript
newtype UpdateTemplateResponse
  = UpdateTemplateResponse {  }
```

##### Instances
``` purescript
Newtype UpdateTemplateResponse _
```

#### `VerificationAttributes`

``` purescript
newtype VerificationAttributes
  = VerificationAttributes (Map Identity IdentityVerificationAttributes)
```

##### Instances
``` purescript
Newtype VerificationAttributes _
```

#### `VerificationStatus`

``` purescript
newtype VerificationStatus
  = VerificationStatus String
```

##### Instances
``` purescript
Newtype VerificationStatus _
```

#### `VerificationToken`

``` purescript
newtype VerificationToken
  = VerificationToken String
```

##### Instances
``` purescript
Newtype VerificationToken _
```

#### `VerificationTokenList`

``` purescript
newtype VerificationTokenList
  = VerificationTokenList (Array VerificationToken)
```

##### Instances
``` purescript
Newtype VerificationTokenList _
```

#### `VerifyDomainDkimRequest`

``` purescript
newtype VerifyDomainDkimRequest
  = VerifyDomainDkimRequest { "Domain" :: Domain }
```

<p>Represents a request to generate the CNAME records needed to set up Easy DKIM with Amazon SES. For more information about setting up Easy DKIM, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype VerifyDomainDkimRequest _
```

#### `VerifyDomainDkimResponse`

``` purescript
newtype VerifyDomainDkimResponse
  = VerifyDomainDkimResponse { "DkimTokens" :: VerificationTokenList }
```

<p>Returns CNAME records that you must publish to the DNS server of your domain to set up Easy DKIM with Amazon SES.</p>

##### Instances
``` purescript
Newtype VerifyDomainDkimResponse _
```

#### `VerifyDomainIdentityRequest`

``` purescript
newtype VerifyDomainIdentityRequest
  = VerifyDomainIdentityRequest { "Domain" :: Domain }
```

<p>Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype VerifyDomainIdentityRequest _
```

#### `VerifyDomainIdentityResponse`

``` purescript
newtype VerifyDomainIdentityResponse
  = VerifyDomainIdentityResponse { "VerificationToken" :: VerificationToken }
```

<p>Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.</p>

##### Instances
``` purescript
Newtype VerifyDomainIdentityResponse _
```

#### `VerifyEmailAddressRequest`

``` purescript
newtype VerifyEmailAddressRequest
  = VerifyEmailAddressRequest { "EmailAddress" :: Address }
```

<p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype VerifyEmailAddressRequest _
```

#### `VerifyEmailIdentityRequest`

``` purescript
newtype VerifyEmailIdentityRequest
  = VerifyEmailIdentityRequest { "EmailAddress" :: Address }
```

<p>Represents a request to begin email address verification with Amazon SES. For information about email address verification, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype VerifyEmailIdentityRequest _
```

#### `VerifyEmailIdentityResponse`

``` purescript
newtype VerifyEmailIdentityResponse
  = VerifyEmailIdentityResponse {  }
```

<p>An empty element returned on a successful request.</p>

##### Instances
``` purescript
Newtype VerifyEmailIdentityResponse _
```

#### `WorkmailAction`

``` purescript
newtype WorkmailAction
  = WorkmailAction { "TopicArn" :: NullOrUndefined (AmazonResourceName), "OrganizationArn" :: AmazonResourceName }
```

<p>When included in a receipt rule, this action calls Amazon WorkMail and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS). You will typically not use this action directly because Amazon WorkMail adds the rule automatically during its setup procedure.</p> <p>For information using a receipt rule to call Amazon WorkMail, see the <a href="http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-workmail.html">Amazon SES Developer Guide</a>.</p>

##### Instances
``` purescript
Newtype WorkmailAction _
```


