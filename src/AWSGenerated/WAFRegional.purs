

-- | <p>This is the <i>AWS WAF Regional API Reference</i> for using AWS WAF with Elastic Load Balancing (ELB) Application Load Balancers. The AWS WAF actions and data types listed in the reference are available for protecting Application Load Balancers. You can use these actions and data types by means of the endpoints listed in <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region">AWS Regions and Endpoints</a>. This guide is for developers who need detailed information about the AWS WAF API actions, data types, and errors. For detailed information about AWS WAF features and an overview of how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
module AWS.WAFRegional where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "WAFRegional" :: String


-- | <p>Associates a web ACL with a resource.</p>
associateWebACL :: forall eff. AssociateWebACLRequest -> Aff (err :: AWS.RequestError | eff) AssociateWebACLResponse
associateWebACL = AWS.request serviceName "AssociateWebACL" 


-- | <p>Creates a <code>ByteMatchSet</code>. You then use <a>UpdateByteMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>ByteMatchSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>BadBot</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateByteMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createByteMatchSet :: forall eff. CreateByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateByteMatchSetResponse
createByteMatchSet = AWS.request serviceName "CreateByteMatchSet" 


-- | <p>Creates an <a>GeoMatchSet</a>, which you use to specify which web requests you want to allow or block based on the country that the requests originate from. For example, if you're receiving a lot of requests from one or more countries and you want to block the requests, you can create an <code>GeoMatchSet</code> that contains those countries and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSetSet</code> request to specify the countries that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createGeoMatchSet :: forall eff. CreateGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateGeoMatchSetResponse
createGeoMatchSet = AWS.request serviceName "CreateGeoMatchSet" 


-- | <p>Creates an <a>IPSet</a>, which you use to specify which web requests you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that contains those IP addresses and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateIPSet</code> request.</p> </li> <li> <p>Submit a <code>CreateIPSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createIPSet :: forall eff. CreateIPSetRequest -> Aff (err :: AWS.RequestError | eff) CreateIPSetResponse
createIPSet = AWS.request serviceName "CreateIPSet" 


-- | <p>Creates a <a>RateBasedRule</a>. The <code>RateBasedRule</code> contains a <code>RateLimit</code>, which specifies the maximum number of requests that AWS WAF allows from a specified IP address in a five-minute period. The <code>RateBasedRule</code> also contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to count or block if these requests exceed the <code>RateLimit</code>.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request not only must exceed the <code>RateLimit</code>, but it also must match all the specifications to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that meet the conditions in the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions must be received at a rate of more than 15,000 requests every five minutes. If both conditions are met and the rate is exceeded, AWS WAF blocks the requests. If the rate drops below 15,000 for a five-minute period, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p> <p>To create and configure a <code>RateBasedRule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the rule. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRateBasedRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRateBasedRule</code> request to specify the predicates that you want to include in the rule.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RateBasedRule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRateBasedRule :: forall eff. CreateRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) CreateRateBasedRuleResponse
createRateBasedRule = AWS.request serviceName "CreateRateBasedRule" 


-- | <p>Creates a <a>RegexMatchSet</a>. You then use <a>UpdateRegexMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>RegexMatchSet</code> that contains a <code>RegexMatchTuple</code> that looks for any requests with <code>User-Agent</code> headers that match a <code>RegexPatternSet</code> with pattern <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value, using a <code>RegexPatternSet</code>, that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexMatchSet :: forall eff. CreateRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateRegexMatchSetResponse
createRegexMatchSet = AWS.request serviceName "CreateRegexMatchSet" 


-- | <p>Creates a <code>RegexPatternSet</code>. You then use <a>UpdateRegexPatternSet</a> to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexPatternSet</a> request to specify the string that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexPatternSet :: forall eff. CreateRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) CreateRegexPatternSetResponse
createRegexPatternSet = AWS.request serviceName "CreateRegexPatternSet" 


-- | <p>Creates a <code>Rule</code>, which contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to block. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed or blocked. For example, suppose you add the following to a <code>Rule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to blocks requests that satisfy the <code>Rule</code>. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to specify the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRule :: forall eff. CreateRuleRequest -> Aff (err :: AWS.RequestError | eff) CreateRuleResponse
createRule = AWS.request serviceName "CreateRule" 


-- | <p>Creates a <code>RuleGroup</code>. A rule group is a collection of predefined rules that you add to a web ACL. You use <a>UpdateRuleGroup</a> to add rules to the rule group.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRuleGroup :: forall eff. CreateRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateRuleGroupResponse
createRuleGroup = AWS.request serviceName "CreateRuleGroup" 


-- | <p>Creates a <code>SizeConstraintSet</code>. You then use <a>UpdateSizeConstraintSet</a> to identify the part of a web request that you want AWS WAF to check for length, such as the length of the <code>User-Agent</code> header or the length of the query string. For example, you can create a <code>SizeConstraintSet</code> that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateSizeConstraintSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSizeConstraintSet :: forall eff. CreateSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) CreateSizeConstraintSetResponse
createSizeConstraintSet = AWS.request serviceName "CreateSizeConstraintSet" 


-- | <p>Creates a <a>SqlInjectionMatchSet</a>, which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateSqlInjectionMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSqlInjectionMatchSet :: forall eff. CreateSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateSqlInjectionMatchSetResponse
createSqlInjectionMatchSet = AWS.request serviceName "CreateSqlInjectionMatchSet" 


-- | <p>Creates a <code>WebACL</code>, which contains the <code>Rules</code> that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates <code>Rules</code> in order based on the value of <code>Priority</code> for each <code>Rule</code>.</p> <p>You also specify a default action, either <code>ALLOW</code> or <code>BLOCK</code>. If a web request doesn't match any of the <code>Rules</code> in a <code>WebACL</code>, AWS WAF responds to the request with the default action. </p> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>ByteMatchSet</code> objects and other predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateWebACL</code> request.</p> </li> <li> <p>Submit a <code>CreateWebACL</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <a>UpdateWebACL</a> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution.</p> </li> </ol> <p>For more information about how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createWebACL :: forall eff. CreateWebACLRequest -> Aff (err :: AWS.RequestError | eff) CreateWebACLResponse
createWebACL = AWS.request serviceName "CreateWebACL" 


-- | <p>Creates an <a>XssMatchSet</a>, which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateXssMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateXssMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createXssMatchSet :: forall eff. CreateXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateXssMatchSetResponse
createXssMatchSet = AWS.request serviceName "CreateXssMatchSet" 


-- | <p>Permanently deletes a <a>ByteMatchSet</a>. You can't delete a <code>ByteMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>ByteMatchTuple</a> objects (any filters).</p> <p>If you just want to remove a <code>ByteMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>ByteMatchSet</code> to remove filters, if any. For more information, see <a>UpdateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteByteMatchSet</code> request.</p> </li> </ol>
deleteByteMatchSet :: forall eff. DeleteByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteByteMatchSetResponse
deleteByteMatchSet = AWS.request serviceName "DeleteByteMatchSet" 


-- | <p>Permanently deletes a <a>GeoMatchSet</a>. You can't delete a <code>GeoMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any countries.</p> <p>If you just want to remove a <code>GeoMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>GeoMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>GeoMatchSet</code> to remove any countries. For more information, see <a>UpdateGeoMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteGeoMatchSet</code> request.</p> </li> </ol>
deleteGeoMatchSet :: forall eff. DeleteGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteGeoMatchSetResponse
deleteGeoMatchSet = AWS.request serviceName "DeleteGeoMatchSet" 


-- | <p>Permanently deletes an <a>IPSet</a>. You can't delete an <code>IPSet</code> if it's still used in any <code>Rules</code> or if it still includes any IP addresses.</p> <p>If you just want to remove an <code>IPSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>IPSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>IPSet</code> to remove IP address ranges, if any. For more information, see <a>UpdateIPSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteIPSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteIPSet</code> request.</p> </li> </ol>
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteIPSetResponse
deleteIPSet = AWS.request serviceName "DeleteIPSet" 


-- | <p>Permanently deletes an IAM policy from the specified RuleGroup.</p> <p>The user making the request must be the owner of the RuleGroup.</p>
deletePermissionPolicy :: forall eff. DeletePermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeletePermissionPolicyResponse
deletePermissionPolicy = AWS.request serviceName "DeletePermissionPolicy" 


-- | <p>Permanently deletes a <a>RateBasedRule</a>. You can't delete a rule if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a rule from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RateBasedRule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RateBasedRule</code> to remove predicates, if any. For more information, see <a>UpdateRateBasedRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRateBasedRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRateBasedRule</code> request.</p> </li> </ol>
deleteRateBasedRule :: forall eff. DeleteRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) DeleteRateBasedRuleResponse
deleteRateBasedRule = AWS.request serviceName "DeleteRateBasedRule" 


-- | <p>Permanently deletes a <a>RegexMatchSet</a>. You can't delete a <code>RegexMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <code>RegexMatchTuples</code> objects (any filters).</p> <p>If you just want to remove a <code>RegexMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>RegexMatchSet</code> to remove filters, if any. For more information, see <a>UpdateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteRegexMatchSet</code> request.</p> </li> </ol>
deleteRegexMatchSet :: forall eff. DeleteRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegexMatchSetResponse
deleteRegexMatchSet = AWS.request serviceName "DeleteRegexMatchSet" 


-- | <p>Permanently deletes a <a>RegexPatternSet</a>. You can't delete a <code>RegexPatternSet</code> if it's still used in any <code>RegexMatchSet</code> or if the <code>RegexPatternSet</code> is not empty. </p>
deleteRegexPatternSet :: forall eff. DeleteRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegexPatternSetResponse
deleteRegexPatternSet = AWS.request serviceName "DeleteRegexPatternSet" 


-- | <p>Permanently deletes a <a>Rule</a>. You can't delete a <code>Rule</code> if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a <code>Rule</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>Rule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>Rule</code> to remove predicates, if any. For more information, see <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRule</code> request.</p> </li> </ol>
deleteRule :: forall eff. DeleteRuleRequest -> Aff (err :: AWS.RequestError | eff) DeleteRuleResponse
deleteRule = AWS.request serviceName "DeleteRule" 


-- | <p>Permanently deletes a <a>RuleGroup</a>. You can't delete a <code>RuleGroup</code> if it's still used in any <code>WebACL</code> objects or if it still includes any rules.</p> <p>If you just want to remove a <code>RuleGroup</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RuleGroup</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RuleGroup</code> to remove rules, if any. For more information, see <a>UpdateRuleGroup</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRuleGroup</code> request.</p> </li> <li> <p>Submit a <code>DeleteRuleGroup</code> request.</p> </li> </ol>
deleteRuleGroup :: forall eff. DeleteRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteRuleGroupResponse
deleteRuleGroup = AWS.request serviceName "DeleteRuleGroup" 


-- | <p>Permanently deletes a <a>SizeConstraintSet</a>. You can't delete a <code>SizeConstraintSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>SizeConstraint</a> objects (any filters).</p> <p>If you just want to remove a <code>SizeConstraintSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>SizeConstraintSet</code> to remove filters, if any. For more information, see <a>UpdateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSizeConstraintSet</code> request.</p> </li> </ol>
deleteSizeConstraintSet :: forall eff. DeleteSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteSizeConstraintSetResponse
deleteSizeConstraintSet = AWS.request serviceName "DeleteSizeConstraintSet" 


-- | <p>Permanently deletes a <a>SqlInjectionMatchSet</a>. You can't delete a <code>SqlInjectionMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>SqlInjectionMatchTuple</a> objects.</p> <p>If you just want to remove a <code>SqlInjectionMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SqlInjectionMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>SqlInjectionMatchSet</code> to remove filters, if any. For more information, see <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> </ol>
deleteSqlInjectionMatchSet :: forall eff. DeleteSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteSqlInjectionMatchSetResponse
deleteSqlInjectionMatchSet = AWS.request serviceName "DeleteSqlInjectionMatchSet" 


-- | <p>Permanently deletes a <a>WebACL</a>. You can't delete a <code>WebACL</code> if it still contains any <code>Rules</code>.</p> <p>To delete a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>WebACL</code> to remove <code>Rules</code>, if any. For more information, see <a>UpdateWebACL</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteWebACL</code> request.</p> </li> <li> <p>Submit a <code>DeleteWebACL</code> request.</p> </li> </ol>
deleteWebACL :: forall eff. DeleteWebACLRequest -> Aff (err :: AWS.RequestError | eff) DeleteWebACLResponse
deleteWebACL = AWS.request serviceName "DeleteWebACL" 


-- | <p>Permanently deletes an <a>XssMatchSet</a>. You can't delete an <code>XssMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>XssMatchTuple</a> objects.</p> <p>If you just want to remove an <code>XssMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>XssMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>XssMatchSet</code> to remove filters, if any. For more information, see <a>UpdateXssMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteXssMatchSet</code> request.</p> </li> </ol>
deleteXssMatchSet :: forall eff. DeleteXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteXssMatchSetResponse
deleteXssMatchSet = AWS.request serviceName "DeleteXssMatchSet" 


-- | <p>Removes a web ACL from the specified resource.</p>
disassociateWebACL :: forall eff. DisassociateWebACLRequest -> Aff (err :: AWS.RequestError | eff) DisassociateWebACLResponse
disassociateWebACL = AWS.request serviceName "DisassociateWebACL" 


-- | <p>Returns the <a>ByteMatchSet</a> specified by <code>ByteMatchSetId</code>.</p>
getByteMatchSet :: forall eff. GetByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetByteMatchSetResponse
getByteMatchSet = AWS.request serviceName "GetByteMatchSet" 


-- | <p>When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request. Change tokens ensure that your application doesn't submit conflicting requests to AWS WAF.</p> <p>Each create, update, or delete request must use a unique change token. If your application submits a <code>GetChangeToken</code> request and then submits a second <code>GetChangeToken</code> request before submitting a create, update, or delete request, the second <code>GetChangeToken</code> request returns the same value as the first <code>GetChangeToken</code> request.</p> <p>When you use a change token in a create, update, or delete request, the status of the change token changes to <code>PENDING</code>, which indicates that AWS WAF is propagating the change to all AWS WAF servers. Use <code>GetChangeTokenStatus</code> to determine the status of your change token.</p>
getChangeToken :: forall eff. GetChangeTokenRequest -> Aff (err :: AWS.RequestError | eff) GetChangeTokenResponse
getChangeToken = AWS.request serviceName "GetChangeToken" 


-- | <p>Returns the status of a <code>ChangeToken</code> that you got by calling <a>GetChangeToken</a>. <code>ChangeTokenStatus</code> is one of the following values:</p> <ul> <li> <p> <code>PROVISIONED</code>: You requested the change token by calling <code>GetChangeToken</code>, but you haven't used it yet in a call to create, update, or delete an AWS WAF object.</p> </li> <li> <p> <code>PENDING</code>: AWS WAF is propagating the create, update, or delete request to all AWS WAF servers.</p> </li> <li> <p> <code>IN_SYNC</code>: Propagation is complete.</p> </li> </ul>
getChangeTokenStatus :: forall eff. GetChangeTokenStatusRequest -> Aff (err :: AWS.RequestError | eff) GetChangeTokenStatusResponse
getChangeTokenStatus = AWS.request serviceName "GetChangeTokenStatus" 


-- | <p>Returns the <a>GeoMatchSet</a> that is specified by <code>GeoMatchSetId</code>.</p>
getGeoMatchSet :: forall eff. GetGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetGeoMatchSetResponse
getGeoMatchSet = AWS.request serviceName "GetGeoMatchSet" 


-- | <p>Returns the <a>IPSet</a> that is specified by <code>IPSetId</code>.</p>
getIPSet :: forall eff. GetIPSetRequest -> Aff (err :: AWS.RequestError | eff) GetIPSetResponse
getIPSet = AWS.request serviceName "GetIPSet" 


-- | <p>Returns the IAM policy attached to the RuleGroup.</p>
getPermissionPolicy :: forall eff. GetPermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetPermissionPolicyResponse
getPermissionPolicy = AWS.request serviceName "GetPermissionPolicy" 


-- | <p>Returns the <a>RateBasedRule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRateBasedRule</code> request.</p>
getRateBasedRule :: forall eff. GetRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) GetRateBasedRuleResponse
getRateBasedRule = AWS.request serviceName "GetRateBasedRule" 


-- | <p>Returns an array of IP addresses currently being blocked by the <a>RateBasedRule</a> that is specified by the <code>RuleId</code>. The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.</p>
getRateBasedRuleManagedKeys :: forall eff. GetRateBasedRuleManagedKeysRequest -> Aff (err :: AWS.RequestError | eff) GetRateBasedRuleManagedKeysResponse
getRateBasedRuleManagedKeys = AWS.request serviceName "GetRateBasedRuleManagedKeys" 


-- | <p>Returns the <a>RegexMatchSet</a> specified by <code>RegexMatchSetId</code>.</p>
getRegexMatchSet :: forall eff. GetRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetRegexMatchSetResponse
getRegexMatchSet = AWS.request serviceName "GetRegexMatchSet" 


-- | <p>Returns the <a>RegexPatternSet</a> specified by <code>RegexPatternSetId</code>.</p>
getRegexPatternSet :: forall eff. GetRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) GetRegexPatternSetResponse
getRegexPatternSet = AWS.request serviceName "GetRegexPatternSet" 


-- | <p>Returns the <a>Rule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRule</code> request.</p>
getRule :: forall eff. GetRuleRequest -> Aff (err :: AWS.RequestError | eff) GetRuleResponse
getRule = AWS.request serviceName "GetRule" 


-- | <p>Returns the <a>RuleGroup</a> that is specified by the <code>RuleGroupId</code> that you included in the <code>GetRuleGroup</code> request.</p> <p>To view the rules in a rule group, use <a>ListActivatedRulesInRuleGroup</a>.</p>
getRuleGroup :: forall eff. GetRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) GetRuleGroupResponse
getRuleGroup = AWS.request serviceName "GetRuleGroup" 


-- | <p>Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose. You can specify a sample size of up to 500 requests, and you can specify any time range in the previous three hours.</p> <p> <code>GetSampledRequests</code> returns a time range, which is usually the time range that you specified. However, if your resource (such as a CloudFront distribution) received 5,000 requests before the specified time range elapsed, <code>GetSampledRequests</code> returns an updated time range. This new time range indicates the actual period during which AWS WAF selected the requests in the sample.</p>
getSampledRequests :: forall eff. GetSampledRequestsRequest -> Aff (err :: AWS.RequestError | eff) GetSampledRequestsResponse
getSampledRequests = AWS.request serviceName "GetSampledRequests" 


-- | <p>Returns the <a>SizeConstraintSet</a> specified by <code>SizeConstraintSetId</code>.</p>
getSizeConstraintSet :: forall eff. GetSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) GetSizeConstraintSetResponse
getSizeConstraintSet = AWS.request serviceName "GetSizeConstraintSet" 


-- | <p>Returns the <a>SqlInjectionMatchSet</a> that is specified by <code>SqlInjectionMatchSetId</code>.</p>
getSqlInjectionMatchSet :: forall eff. GetSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetSqlInjectionMatchSetResponse
getSqlInjectionMatchSet = AWS.request serviceName "GetSqlInjectionMatchSet" 


-- | <p>Returns the <a>WebACL</a> that is specified by <code>WebACLId</code>.</p>
getWebACL :: forall eff. GetWebACLRequest -> Aff (err :: AWS.RequestError | eff) GetWebACLResponse
getWebACL = AWS.request serviceName "GetWebACL" 


-- | <p>Returns the web ACL for the specified resource.</p>
getWebACLForResource :: forall eff. GetWebACLForResourceRequest -> Aff (err :: AWS.RequestError | eff) GetWebACLForResourceResponse
getWebACLForResource = AWS.request serviceName "GetWebACLForResource" 


-- | <p>Returns the <a>XssMatchSet</a> that is specified by <code>XssMatchSetId</code>.</p>
getXssMatchSet :: forall eff. GetXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetXssMatchSetResponse
getXssMatchSet = AWS.request serviceName "GetXssMatchSet" 


-- | <p>Returns an array of <a>ActivatedRule</a> objects.</p>
listActivatedRulesInRuleGroup :: forall eff. ListActivatedRulesInRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) ListActivatedRulesInRuleGroupResponse
listActivatedRulesInRuleGroup = AWS.request serviceName "ListActivatedRulesInRuleGroup" 


-- | <p>Returns an array of <a>ByteMatchSetSummary</a> objects.</p>
listByteMatchSets :: forall eff. ListByteMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListByteMatchSetsResponse
listByteMatchSets = AWS.request serviceName "ListByteMatchSets" 


-- | <p>Returns an array of <a>GeoMatchSetSummary</a> objects in the response.</p>
listGeoMatchSets :: forall eff. ListGeoMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListGeoMatchSetsResponse
listGeoMatchSets = AWS.request serviceName "ListGeoMatchSets" 


-- | <p>Returns an array of <a>IPSetSummary</a> objects in the response.</p>
listIPSets :: forall eff. ListIPSetsRequest -> Aff (err :: AWS.RequestError | eff) ListIPSetsResponse
listIPSets = AWS.request serviceName "ListIPSets" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRateBasedRules :: forall eff. ListRateBasedRulesRequest -> Aff (err :: AWS.RequestError | eff) ListRateBasedRulesResponse
listRateBasedRules = AWS.request serviceName "ListRateBasedRules" 


-- | <p>Returns an array of <a>RegexMatchSetSummary</a> objects.</p>
listRegexMatchSets :: forall eff. ListRegexMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListRegexMatchSetsResponse
listRegexMatchSets = AWS.request serviceName "ListRegexMatchSets" 


-- | <p>Returns an array of <a>RegexPatternSetSummary</a> objects.</p>
listRegexPatternSets :: forall eff. ListRegexPatternSetsRequest -> Aff (err :: AWS.RequestError | eff) ListRegexPatternSetsResponse
listRegexPatternSets = AWS.request serviceName "ListRegexPatternSets" 


-- | <p>Returns an array of resources associated with the specified web ACL.</p>
listResourcesForWebACL :: forall eff. ListResourcesForWebACLRequest -> Aff (err :: AWS.RequestError | eff) ListResourcesForWebACLResponse
listResourcesForWebACL = AWS.request serviceName "ListResourcesForWebACL" 


-- | <p>Returns an array of <a>RuleGroup</a> objects.</p>
listRuleGroups :: forall eff. ListRuleGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListRuleGroupsResponse
listRuleGroups = AWS.request serviceName "ListRuleGroups" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRules :: forall eff. ListRulesRequest -> Aff (err :: AWS.RequestError | eff) ListRulesResponse
listRules = AWS.request serviceName "ListRules" 


-- | <p>Returns an array of <a>SizeConstraintSetSummary</a> objects.</p>
listSizeConstraintSets :: forall eff. ListSizeConstraintSetsRequest -> Aff (err :: AWS.RequestError | eff) ListSizeConstraintSetsResponse
listSizeConstraintSets = AWS.request serviceName "ListSizeConstraintSets" 


-- | <p>Returns an array of <a>SqlInjectionMatchSet</a> objects.</p>
listSqlInjectionMatchSets :: forall eff. ListSqlInjectionMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListSqlInjectionMatchSetsResponse
listSqlInjectionMatchSets = AWS.request serviceName "ListSqlInjectionMatchSets" 


-- | <p>Returns an array of <a>RuleGroup</a> objects that you are subscribed to.</p>
listSubscribedRuleGroups :: forall eff. ListSubscribedRuleGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListSubscribedRuleGroupsResponse
listSubscribedRuleGroups = AWS.request serviceName "ListSubscribedRuleGroups" 


-- | <p>Returns an array of <a>WebACLSummary</a> objects in the response.</p>
listWebACLs :: forall eff. ListWebACLsRequest -> Aff (err :: AWS.RequestError | eff) ListWebACLsResponse
listWebACLs = AWS.request serviceName "ListWebACLs" 


-- | <p>Returns an array of <a>XssMatchSet</a> objects.</p>
listXssMatchSets :: forall eff. ListXssMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListXssMatchSetsResponse
listXssMatchSets = AWS.request serviceName "ListXssMatchSets" 


-- | <p>Attaches a IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.</p> <p>The <code>PutPermissionPolicy</code> is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> and <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html">IAM Policies</a>. </p> <p>An example of a valid policy parameter is shown in the Examples section below.</p>
putPermissionPolicy :: forall eff. PutPermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) PutPermissionPolicyResponse
putPermissionPolicy = AWS.request serviceName "PutPermissionPolicy" 


-- | <p>Inserts or deletes <a>ByteMatchTuple</a> objects (filters) in a <a>ByteMatchSet</a>. For each <code>ByteMatchTuple</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>ByteMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see <code>TargetString</code> in the <a>ByteMatchTuple</a> data type. </p> </li> <li> <p>Where to look, such as at the beginning or the end of a query string.</p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p>For example, you can add a <code>ByteMatchSetUpdate</code> object that matches web requests in which <code>User-Agent</code> headers contain the string <code>BadBot</code>. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>ByteMatchSet.</code> For more information, see <a>CreateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateByteMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateByteMatchSet :: forall eff. UpdateByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateByteMatchSetResponse
updateByteMatchSet = AWS.request serviceName "UpdateByteMatchSet" 


-- | <p>Inserts or deletes <a>GeoMatchConstraint</a> objects in an <code>GeoMatchSet</code>. For each <code>GeoMatchConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>GeoMatchConstraint</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The <code>Type</code>. The only valid value for <code>Type</code> is <code>Country</code>.</p> </li> <li> <p>The <code>Value</code>, which is a two character code for the country to add to the <code>GeoMatchConstraint</code> object. Valid codes are listed in <a>GeoMatchConstraint$Value</a>.</p> </li> </ul> <p>To create and configure an <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateGeoMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSet</code> request to specify the country that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>GeoMatchSet</code>, you specify the country that you want to add and/or the country that you want to delete. If you want to change a country, you delete the existing country and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateGeoMatchSet :: forall eff. UpdateGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateGeoMatchSetResponse
updateGeoMatchSet = AWS.request serviceName "UpdateGeoMatchSet" 


-- | <p>Inserts or deletes <a>IPSetDescriptor</a> objects in an <code>IPSet</code>. For each <code>IPSetDescriptor</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>IPSetDescriptor</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The IP address version, <code>IPv4</code> or <code>IPv6</code>. </p> </li> <li> <p>The IP address in CIDR notation, for example, <code>192.0.2.0/24</code> (for the range of IP addresses from <code>192.0.2.0</code> to <code>192.0.2.255</code>) or <code>192.0.2.44/32</code> (for the individual IP address <code>192.0.2.44</code>). </p> </li> </ul> <p>AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>.</p> <p>IPv6 addresses can be represented using any of the following formats:</p> <ul> <li> <p>1111:0000:0000:0000:0000:0000:0000:0111/128</p> </li> <li> <p>1111:0:0:0:0:0:0:0111/128</p> </li> <li> <p>1111::0111/128</p> </li> <li> <p>1111::111/128</p> </li> </ul> <p>You use an <code>IPSet</code> to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you're receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that specifies those IP addresses, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateIPSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>IPSet</code>, you specify the IP addresses that you want to add and/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateIPSetResponse
updateIPSet = AWS.request serviceName "UpdateIPSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a rule and updates the <code>RateLimit</code> in the rule. </p> <p>Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to block or count. The <code>RateLimit</code> specifies the number of requests every five minutes that triggers the rule.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request must match all the predicates and exceed the <code>RateLimit</code> to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions much be received at a rate of more than 15,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p>
updateRateBasedRule :: forall eff. UpdateRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) UpdateRateBasedRuleResponse
updateRateBasedRule = AWS.request serviceName "UpdateRateBasedRule" 


-- | <p>Inserts or deletes <a>RegexMatchTuple</a> objects (filters) in a <a>RegexMatchSet</a>. For each <code>RegexMatchSetUpdate</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>RegexMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p> For example, you can create a <code>RegexPatternSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexMatchSet.</code> For more information, see <a>CreateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the <code>RegexPatternSet</code> that contain the regular expression patters you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexMatchSet :: forall eff. UpdateRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateRegexMatchSetResponse
updateRegexMatchSet = AWS.request serviceName "UpdateRegexMatchSet" 


-- | <p>Inserts or deletes <code>RegexPatternString</code> objects in a <a>RegexPatternSet</a>. For each <code>RegexPatternString</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the <code>RegexPatternString</code>.</p> </li> <li> <p>The regular expression pattern that you want to insert or delete. For more information, see <a>RegexPatternSet</a>. </p> </li> </ul> <p> For example, you can create a <code>RegexPatternString</code> such as <code>B[a@]dB[o0]t</code>. AWS WAF will match this <code>RegexPatternString</code> to:</p> <ul> <li> <p>BadBot</p> </li> <li> <p>BadB0t</p> </li> <li> <p>B@dBot</p> </li> <li> <p>B@dB0t</p> </li> </ul> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexPatternSet.</code> For more information, see <a>CreateRegexPatternSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexPatternSet</code> request to specify the regular expression pattern that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexPatternSet :: forall eff. UpdateRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateRegexPatternSetResponse
updateRegexPatternSet = AWS.request serviceName "UpdateRegexPatternSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a <code>Rule</code>. Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose you add the following to a <code>Rule</code>: </p> <ul> <li> <p>A <code>ByteMatchSet</code> that matches the value <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44</code> </p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the <code>Rule</code>. For a request to be blocked, the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code> <i>and</i> the request must originate from the IP address 192.0.2.44.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create the <code>Rule</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to add predicates to the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>ByteMatchSet</code> or <code>IPSet</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRule :: forall eff. UpdateRuleRequest -> Aff (err :: AWS.RequestError | eff) UpdateRuleResponse
updateRule = AWS.request serviceName "UpdateRule" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>RuleGroup</code>.</p> <p>You can only insert <code>REGULAR</code> rules into a rule group.</p> <p>You can have a maximum of ten rules per rule group.</p> <p>To create and configure a <code>RuleGroup</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>RuleGroup</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRuleGroup</a> request.</p> </li> <li> <p>Submit an <code>UpdateRuleGroup</code> request to add <code>Rules</code> to the <code>RuleGroup</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RuleGroup</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>Rule</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRuleGroup :: forall eff. UpdateRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateRuleGroupResponse
updateRuleGroup = AWS.request serviceName "UpdateRuleGroup" 


-- | <p>Inserts or deletes <a>SizeConstraint</a> objects (filters) in a <a>SizeConstraintSet</a>. For each <code>SizeConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>SizeConstraintSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the <code>User-Agent</code> header.</p> </li> <li> <p>Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first <code>8192</code> bytes of your request to AWS WAF.</p> </li> <li> <p>A <code>ComparisonOperator</code> used for evaluating the selected part of the request against the specified <code>Size</code>, such as equals, greater than, less than, and so on.</p> </li> <li> <p>The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.</p> </li> </ul> <p>For example, you can add a <code>SizeConstraintSetUpdate</code> object that matches web requests in which the length of the <code>User-Agent</code> header is greater than 100 bytes. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>SizeConstraintSet.</code> For more information, see <a>CreateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateSizeConstraintSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSizeConstraintSet :: forall eff. UpdateSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateSizeConstraintSetResponse
updateSizeConstraintSet = AWS.request serviceName "UpdateSizeConstraintSet" 


-- | <p>Inserts or deletes <a>SqlInjectionMatchTuple</a> objects (filters) in a <a>SqlInjectionMatchSet</a>. For each <code>SqlInjectionMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>SqlInjectionMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.</p> </li> </ul> <p>You use <code>SqlInjectionMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a <code>SqlInjectionMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateSqlInjectionMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSqlInjectionMatchSet :: forall eff. UpdateSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateSqlInjectionMatchSetResponse
updateSqlInjectionMatchSet = AWS.request serviceName "UpdateSqlInjectionMatchSet" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>WebACL</code>. Each <code>Rule</code> identifies web requests that you want to allow, block, or count. When you update a <code>WebACL</code>, you specify the following values:</p> <ul> <li> <p>A default action for the <code>WebACL</code>, either <code>ALLOW</code> or <code>BLOCK</code>. AWS WAF performs the default action if a request doesn't match the criteria in any of the <code>Rules</code> in a <code>WebACL</code>.</p> </li> <li> <p>The <code>Rules</code> that you want to add and/or delete. If you want to replace one <code>Rule</code> with another, you delete the existing <code>Rule</code> and add the new one.</p> </li> <li> <p>For each <code>Rule</code>, whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the <code>Rule</code>.</p> </li> <li> <p>The order in which you want AWS WAF to evaluate the <code>Rules</code> in a <code>WebACL</code>. If you add more than one <code>Rule</code> to a <code>WebACL</code>, AWS WAF evaluates each request against the <code>Rules</code> in order based on the value of <code>Priority</code>. (The <code>Rule</code> that has the lowest value for <code>Priority</code> is evaluated first.) When a web request matches all of the predicates (such as <code>ByteMatchSets</code> and <code>IPSets</code>) in a <code>Rule</code>, AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining <code>Rules</code> in the <code>WebACL</code>, if any. </p> </li> </ul> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Create a <code>WebACL</code>. See <a>CreateWebACL</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <code>UpdateWebACL</code> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution. </p> </li> </ol> <p>Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the <a>UpdateWebACL</a> request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist. </p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateWebACL :: forall eff. UpdateWebACLRequest -> Aff (err :: AWS.RequestError | eff) UpdateWebACLResponse
updateWebACL = AWS.request serviceName "UpdateWebACL" 


-- | <p>Inserts or deletes <a>XssMatchTuple</a> objects (filters) in an <a>XssMatchSet</a>. For each <code>XssMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>XssMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.</p> </li> </ul> <p>You use <code>XssMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an <code>XssMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateXssMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateXssMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateXssMatchSet :: forall eff. UpdateXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateXssMatchSetResponse
updateXssMatchSet = AWS.request serviceName "UpdateXssMatchSet" 


newtype Action = Action String


-- | <p>The <code>ActivatedRule</code> object in an <a>UpdateWebACL</a> request specifies a <code>Rule</code> that you want to insert or delete, the priority of the <code>Rule</code> in the <code>WebACL</code>, and the action that you want AWS WAF to take when a web request matches the <code>Rule</code> (<code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>).</p> <p>To specify whether to insert or delete a <code>Rule</code>, use the <code>Action</code> parameter in the <a>WebACLUpdate</a> data type.</p>
newtype ActivatedRule = ActivatedRule 
  { "Priority" :: (RulePriority)
  , "RuleId" :: (ResourceId)
  , "Action" :: NullOrUndefined (WafAction)
  , "OverrideAction" :: NullOrUndefined (WafOverrideAction)
  , "Type" :: NullOrUndefined (WafRuleType)
  }


newtype ActivatedRules = ActivatedRules (Array ActivatedRule)


newtype AssociateWebACLRequest = AssociateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ResourceArn" :: (ResourceArn)
  }


newtype AssociateWebACLResponse = AssociateWebACLResponse 
  { 
  }


-- | <p>In a <a>GetByteMatchSet</a> request, <code>ByteMatchSet</code> is a complex type that contains the <code>ByteMatchSetId</code> and <code>Name</code> of a <code>ByteMatchSet</code>, and the values that you specified when you updated the <code>ByteMatchSet</code>. </p> <p>A complex type that contains <code>ByteMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>ByteMatchSet</code> contains more than one <code>ByteMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype ByteMatchSet = ByteMatchSet 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "ByteMatchTuples" :: (ByteMatchTuples)
  }


newtype ByteMatchSetSummaries = ByteMatchSetSummaries (Array ByteMatchSetSummary)


-- | <p>Returned by <a>ListByteMatchSets</a>. Each <code>ByteMatchSetSummary</code> object includes the <code>Name</code> and <code>ByteMatchSetId</code> for one <a>ByteMatchSet</a>.</p>
newtype ByteMatchSetSummary = ByteMatchSetSummary 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>In an <a>UpdateByteMatchSet</a> request, <code>ByteMatchSetUpdate</code> specifies whether to insert or delete a <a>ByteMatchTuple</a> and includes the settings for the <code>ByteMatchTuple</code>.</p>
newtype ByteMatchSetUpdate = ByteMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "ByteMatchTuple" :: (ByteMatchTuple)
  }


newtype ByteMatchSetUpdates = ByteMatchSetUpdates (Array ByteMatchSetUpdate)


newtype ByteMatchTargetString = ByteMatchTargetString String


-- | <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.</p>
newtype ByteMatchTuple = ByteMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TargetString" :: (ByteMatchTargetString)
  , "TextTransformation" :: (TextTransformation)
  , "PositionalConstraint" :: (PositionalConstraint)
  }


newtype ByteMatchTuples = ByteMatchTuples (Array ByteMatchTuple)


newtype ChangeAction = ChangeAction String


newtype ChangeToken = ChangeToken String


newtype ChangeTokenStatus = ChangeTokenStatus String


newtype ComparisonOperator = ComparisonOperator String


newtype Country = Country String


newtype CreateByteMatchSetRequest = CreateByteMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateByteMatchSetResponse = CreateByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined (ByteMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateGeoMatchSetRequest = CreateGeoMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateGeoMatchSetResponse = CreateGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined (GeoMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IPSet" :: NullOrUndefined (IPSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateRateBasedRuleRequest = CreateRateBasedRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateRateBasedRuleResponse = CreateRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined (RateBasedRule)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateRegexMatchSetRequest = CreateRegexMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateRegexMatchSetResponse = CreateRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined (RegexMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateRegexPatternSetRequest = CreateRegexPatternSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateRegexPatternSetResponse = CreateRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined (RegexPatternSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateRuleGroupRequest = CreateRuleGroupRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateRuleGroupResponse = CreateRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined (RuleGroup)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateRuleRequest = CreateRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateRuleResponse = CreateRuleResponse 
  { "Rule" :: NullOrUndefined (Rule)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateSizeConstraintSetRequest = CreateSizeConstraintSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined (SizeConstraintSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to create a <a>SqlInjectionMatchSet</a>.</p>
newtype CreateSqlInjectionMatchSetRequest = CreateSqlInjectionMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


-- | <p>The response to a <code>CreateSqlInjectionMatchSet</code> request.</p>
newtype CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined (SqlInjectionMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype CreateWebACLRequest = CreateWebACLRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "DefaultAction" :: (WafAction)
  , "ChangeToken" :: (ChangeToken)
  }


newtype CreateWebACLResponse = CreateWebACLResponse 
  { "WebACL" :: NullOrUndefined (WebACL)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to create an <a>XssMatchSet</a>.</p>
newtype CreateXssMatchSetRequest = CreateXssMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }


-- | <p>The response to a <code>CreateXssMatchSet</code> request.</p>
newtype CreateXssMatchSetResponse = CreateXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined (XssMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteByteMatchSetRequest = DeleteByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteByteMatchSetResponse = DeleteByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteGeoMatchSetRequest = DeleteGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteIPSetResponse = DeleteIPSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeletePermissionPolicyRequest = DeletePermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }


newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse 
  { 
  }


newtype DeleteRateBasedRuleRequest = DeleteRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteRegexMatchSetRequest = DeleteRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteRegexPatternSetRequest = DeleteRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteRuleGroupRequest = DeleteRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteRuleGroupResponse = DeleteRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteRuleResponse = DeleteRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteSizeConstraintSetRequest = DeleteSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetRequest = DeleteSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


-- | <p>The response to a request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DeleteWebACLRequest = DeleteWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


newtype DeleteWebACLResponse = DeleteWebACLResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetRequest = DeleteXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }


-- | <p>The response to a request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetResponse = DeleteXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype DisassociateWebACLRequest = DisassociateWebACLRequest 
  { "ResourceArn" :: (ResourceArn)
  }


newtype DisassociateWebACLResponse = DisassociateWebACLResponse 
  { 
  }


-- | <p>Specifies where in a web request to look for <code>TargetString</code>.</p>
newtype FieldToMatch = FieldToMatch 
  { "Type" :: (MatchFieldType)
  , "Data" :: NullOrUndefined (MatchFieldData)
  }


-- | <p>The country from which web requests originate that you want AWS WAF to search for.</p>
newtype GeoMatchConstraint = GeoMatchConstraint 
  { "Type" :: (GeoMatchConstraintType)
  , "Value" :: (GeoMatchConstraintValue)
  }


newtype GeoMatchConstraintType = GeoMatchConstraintType String


newtype GeoMatchConstraintValue = GeoMatchConstraintValue String


newtype GeoMatchConstraints = GeoMatchConstraints (Array GeoMatchConstraint)


-- | <p>Contains one or more countries that AWS WAF will search for.</p>
newtype GeoMatchSet = GeoMatchSet 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "GeoMatchConstraints" :: (GeoMatchConstraints)
  }


newtype GeoMatchSetSummaries = GeoMatchSetSummaries (Array GeoMatchSetSummary)


-- | <p>Contains the identifier and the name of the <code>GeoMatchSet</code>.</p>
newtype GeoMatchSetSummary = GeoMatchSetSummary 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies the type of update to perform to an <a>GeoMatchSet</a> with <a>UpdateGeoMatchSet</a>.</p>
newtype GeoMatchSetUpdate = GeoMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "GeoMatchConstraint" :: (GeoMatchConstraint)
  }


newtype GeoMatchSetUpdates = GeoMatchSetUpdates (Array GeoMatchSetUpdate)


newtype GetByteMatchSetRequest = GetByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  }


newtype GetByteMatchSetResponse = GetByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined (ByteMatchSet)
  }


newtype GetChangeTokenRequest = GetChangeTokenRequest 
  { 
  }


newtype GetChangeTokenResponse = GetChangeTokenResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype GetChangeTokenStatusRequest = GetChangeTokenStatusRequest 
  { "ChangeToken" :: (ChangeToken)
  }


newtype GetChangeTokenStatusResponse = GetChangeTokenStatusResponse 
  { "ChangeTokenStatus" :: NullOrUndefined (ChangeTokenStatus)
  }


newtype GetGeoMatchSetRequest = GetGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  }


newtype GetGeoMatchSetResponse = GetGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined (GeoMatchSet)
  }


newtype GetIPSetRequest = GetIPSetRequest 
  { "IPSetId" :: (ResourceId)
  }


newtype GetIPSetResponse = GetIPSetResponse 
  { "IPSet" :: NullOrUndefined (IPSet)
  }


newtype GetPermissionPolicyRequest = GetPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }


newtype GetPermissionPolicyResponse = GetPermissionPolicyResponse 
  { "Policy" :: NullOrUndefined (PolicyString)
  }


newtype GetRateBasedRuleManagedKeysRequest = GetRateBasedRuleManagedKeysRequest 
  { "RuleId" :: (ResourceId)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  }


newtype GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse 
  { "ManagedKeys" :: NullOrUndefined (ManagedKeys)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  }


newtype GetRateBasedRuleRequest = GetRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  }


newtype GetRateBasedRuleResponse = GetRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined (RateBasedRule)
  }


newtype GetRegexMatchSetRequest = GetRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  }


newtype GetRegexMatchSetResponse = GetRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined (RegexMatchSet)
  }


newtype GetRegexPatternSetRequest = GetRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  }


newtype GetRegexPatternSetResponse = GetRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined (RegexPatternSet)
  }


newtype GetRuleGroupRequest = GetRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  }


newtype GetRuleGroupResponse = GetRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined (RuleGroup)
  }


newtype GetRuleRequest = GetRuleRequest 
  { "RuleId" :: (ResourceId)
  }


newtype GetRuleResponse = GetRuleResponse 
  { "Rule" :: NullOrUndefined (Rule)
  }


newtype GetSampledRequestsMaxItems = GetSampledRequestsMaxItems Number


newtype GetSampledRequestsRequest = GetSampledRequestsRequest 
  { "WebAclId" :: (ResourceId)
  , "RuleId" :: (ResourceId)
  , "TimeWindow" :: (TimeWindow)
  , "MaxItems" :: (GetSampledRequestsMaxItems)
  }


newtype GetSampledRequestsResponse = GetSampledRequestsResponse 
  { "SampledRequests" :: NullOrUndefined (SampledHTTPRequests)
  , "PopulationSize" :: NullOrUndefined (PopulationSize)
  , "TimeWindow" :: NullOrUndefined (TimeWindow)
  }


newtype GetSizeConstraintSetRequest = GetSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  }


newtype GetSizeConstraintSetResponse = GetSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined (SizeConstraintSet)
  }


-- | <p>A request to get a <a>SqlInjectionMatchSet</a>.</p>
newtype GetSqlInjectionMatchSetRequest = GetSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  }


-- | <p>The response to a <a>GetSqlInjectionMatchSet</a> request.</p>
newtype GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined (SqlInjectionMatchSet)
  }


newtype GetWebACLForResourceRequest = GetWebACLForResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  }


newtype GetWebACLForResourceResponse = GetWebACLForResourceResponse 
  { "WebACLSummary" :: NullOrUndefined (WebACLSummary)
  }


newtype GetWebACLRequest = GetWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }


newtype GetWebACLResponse = GetWebACLResponse 
  { "WebACL" :: NullOrUndefined (WebACL)
  }


-- | <p>A request to get an <a>XssMatchSet</a>.</p>
newtype GetXssMatchSetRequest = GetXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  }


-- | <p>The response to a <a>GetXssMatchSet</a> request.</p>
newtype GetXssMatchSetResponse = GetXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined (XssMatchSet)
  }


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPHeader</code> complex type that appears as <code>Headers</code> in the response syntax. <code>HTTPHeader</code> contains the names and values of all of the headers that appear in one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPHeader = HTTPHeader 
  { "Name" :: NullOrUndefined (HeaderName)
  , "Value" :: NullOrUndefined (HeaderValue)
  }


newtype HTTPHeaders = HTTPHeaders (Array HTTPHeader)


newtype HTTPMethod = HTTPMethod String


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPRequest</code> complex type that appears as <code>Request</code> in the response syntax. <code>HTTPRequest</code> contains information about one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPRequest = HTTPRequest 
  { "ClientIP" :: NullOrUndefined (IPString)
  , "Country" :: NullOrUndefined (Country)
  , "URI" :: NullOrUndefined (URIString)
  , "Method" :: NullOrUndefined (HTTPMethod)
  , "HTTPVersion" :: NullOrUndefined (HTTPVersion)
  , "Headers" :: NullOrUndefined (HTTPHeaders)
  }


newtype HTTPVersion = HTTPVersion String


newtype HeaderName = HeaderName String


newtype HeaderValue = HeaderValue String


-- | <p>Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6.</p> <p>To specify an individual IP address, you specify the four-part IP address followed by a <code>/32</code>, for example, 192.0.2.0/31. To block a range of IP addresses, you can specify a <code>/128</code>, <code>/64</code>, <code>/56</code>, <code>/48</code>, <code>/32</code>, <code>/24</code>, <code>/16</code>, or <code>/8</code> CIDR. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p>
newtype IPSet = IPSet 
  { "IPSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "IPSetDescriptors" :: (IPSetDescriptors)
  }


-- | <p>Specifies the IP address type (<code>IPV4</code> or <code>IPV6</code>) and the IP address range (in CIDR format) that web requests originate from.</p>
newtype IPSetDescriptor = IPSetDescriptor 
  { "Type" :: (IPSetDescriptorType)
  , "Value" :: (IPSetDescriptorValue)
  }


newtype IPSetDescriptorType = IPSetDescriptorType String


newtype IPSetDescriptorValue = IPSetDescriptorValue String


newtype IPSetDescriptors = IPSetDescriptors (Array IPSetDescriptor)


newtype IPSetSummaries = IPSetSummaries (Array IPSetSummary)


-- | <p>Contains the identifier and the name of the <code>IPSet</code>.</p>
newtype IPSetSummary = IPSetSummary 
  { "IPSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies the type of update to perform to an <a>IPSet</a> with <a>UpdateIPSet</a>.</p>
newtype IPSetUpdate = IPSetUpdate 
  { "Action" :: (ChangeAction)
  , "IPSetDescriptor" :: (IPSetDescriptor)
  }


newtype IPSetUpdates = IPSetUpdates (Array IPSetUpdate)


newtype IPString = IPString String


newtype ListActivatedRulesInRuleGroupRequest = ListActivatedRulesInRuleGroupRequest 
  { "RuleGroupId" :: NullOrUndefined (ResourceId)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "ActivatedRules" :: NullOrUndefined (ActivatedRules)
  }


newtype ListByteMatchSetsRequest = ListByteMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListByteMatchSetsResponse = ListByteMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "ByteMatchSets" :: NullOrUndefined (ByteMatchSetSummaries)
  }


newtype ListGeoMatchSetsRequest = ListGeoMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListGeoMatchSetsResponse = ListGeoMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "GeoMatchSets" :: NullOrUndefined (GeoMatchSetSummaries)
  }


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "IPSets" :: NullOrUndefined (IPSetSummaries)
  }


newtype ListRateBasedRulesRequest = ListRateBasedRulesRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListRateBasedRulesResponse = ListRateBasedRulesResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined (RuleSummaries)
  }


newtype ListRegexMatchSetsRequest = ListRegexMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListRegexMatchSetsResponse = ListRegexMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RegexMatchSets" :: NullOrUndefined (RegexMatchSetSummaries)
  }


newtype ListRegexPatternSetsRequest = ListRegexPatternSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListRegexPatternSetsResponse = ListRegexPatternSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RegexPatternSets" :: NullOrUndefined (RegexPatternSetSummaries)
  }


newtype ListResourcesForWebACLRequest = ListResourcesForWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }


newtype ListResourcesForWebACLResponse = ListResourcesForWebACLResponse 
  { "ResourceArns" :: NullOrUndefined (ResourceArns)
  }


newtype ListRuleGroupsRequest = ListRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListRuleGroupsResponse = ListRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined (RuleGroupSummaries)
  }


newtype ListRulesRequest = ListRulesRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListRulesResponse = ListRulesResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined (RuleSummaries)
  }


newtype ListSizeConstraintSetsRequest = ListSizeConstraintSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "SizeConstraintSets" :: NullOrUndefined (SizeConstraintSetSummaries)
  }


-- | <p>A request to list the <a>SqlInjectionMatchSet</a> objects created by the current AWS account.</p>
newtype ListSqlInjectionMatchSetsRequest = ListSqlInjectionMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


-- | <p>The response to a <a>ListSqlInjectionMatchSets</a> request.</p>
newtype ListSqlInjectionMatchSetsResponse = ListSqlInjectionMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "SqlInjectionMatchSets" :: NullOrUndefined (SqlInjectionMatchSetSummaries)
  }


newtype ListSubscribedRuleGroupsRequest = ListSubscribedRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined (SubscribedRuleGroupSummaries)
  }


newtype ListWebACLsRequest = ListWebACLsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


newtype ListWebACLsResponse = ListWebACLsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "WebACLs" :: NullOrUndefined (WebACLSummaries)
  }


-- | <p>A request to list the <a>XssMatchSet</a> objects created by the current AWS account.</p>
newtype ListXssMatchSetsRequest = ListXssMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }


-- | <p>The response to a <a>ListXssMatchSets</a> request.</p>
newtype ListXssMatchSetsResponse = ListXssMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "XssMatchSets" :: NullOrUndefined (XssMatchSetSummaries)
  }


newtype ManagedKey = ManagedKey String


newtype ManagedKeys = ManagedKeys (Array ManagedKey)


newtype MatchFieldData = MatchFieldData String


newtype MatchFieldType = MatchFieldType String


newtype MetricName = MetricName String


newtype Negated = Negated Boolean


newtype NextMarker = NextMarker String


newtype PaginationLimit = PaginationLimit Int


newtype ParameterExceptionField = ParameterExceptionField String


newtype ParameterExceptionParameter = ParameterExceptionParameter String


newtype ParameterExceptionReason = ParameterExceptionReason String


newtype PolicyString = PolicyString String


newtype PopulationSize = PopulationSize Number


newtype PositionalConstraint = PositionalConstraint String


-- | <p>Specifies the <a>ByteMatchSet</a>, <a>IPSet</a>, <a>SqlInjectionMatchSet</a>, <a>XssMatchSet</a>, <a>RegexMatchSet</a>, <a>GeoMatchSet</a>, and <a>SizeConstraintSet</a> objects that you want to add to a <code>Rule</code> and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44. </p>
newtype Predicate = Predicate 
  { "Negated" :: (Negated)
  , "Type" :: (PredicateType)
  , "DataId" :: (ResourceId)
  }


newtype PredicateType = PredicateType String


newtype Predicates = Predicates (Array Predicate)


newtype PutPermissionPolicyRequest = PutPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  , "Policy" :: (PolicyString)
  }


newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse 
  { 
  }


-- | <p>A <code>RateBasedRule</code> is identical to a regular <a>Rule</a>, with one addition: a <code>RateBasedRule</code> counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a <code>RateBasedRule</code> that includes the following conditions: </p> <ul> <li> <p>The requests come from 192.0.2.44.</p> </li> <li> <p>They contain the value <code>BadBot</code> in the <code>User-Agent</code> header.</p> </li> </ul> <p>In the rule, you also define the rate limit as 15,000.</p> <p>Requests that meet both of these conditions and exceed 15,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.</p>
newtype RateBasedRule = RateBasedRule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "MatchPredicates" :: (Predicates)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  }


newtype RateKey = RateKey String


newtype RateLimit = RateLimit Number


-- | <p>In a <a>GetRegexMatchSet</a> request, <code>RegexMatchSet</code> is a complex type that contains the <code>RegexMatchSetId</code> and <code>Name</code> of a <code>RegexMatchSet</code>, and the values that you specified when you updated the <code>RegexMatchSet</code>.</p> <p> The values are contained in a <code>RegexMatchTuple</code> object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>RegexMatchSet</code> contains more than one <code>RegexMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype RegexMatchSet = RegexMatchSet 
  { "RegexMatchSetId" :: NullOrUndefined (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "RegexMatchTuples" :: NullOrUndefined (RegexMatchTuples)
  }


newtype RegexMatchSetSummaries = RegexMatchSetSummaries (Array RegexMatchSetSummary)


-- | <p>Returned by <a>ListRegexMatchSets</a>. Each <code>RegexMatchSetSummary</code> object includes the <code>Name</code> and <code>RegexMatchSetId</code> for one <a>RegexMatchSet</a>.</p>
newtype RegexMatchSetSummary = RegexMatchSetSummary 
  { "RegexMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>In an <a>UpdateRegexMatchSet</a> request, <code>RegexMatchSetUpdate</code> specifies whether to insert or delete a <a>RegexMatchTuple</a> and includes the settings for the <code>RegexMatchTuple</code>.</p>
newtype RegexMatchSetUpdate = RegexMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexMatchTuple" :: (RegexMatchTuple)
  }


newtype RegexMatchSetUpdates = RegexMatchSetUpdates (Array RegexMatchSetUpdate)


-- | <p>The regular expression pattern that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings. Each <code>RegexMatchTuple</code> object contains: </p> <ul> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul>
newtype RegexMatchTuple = RegexMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "RegexPatternSetId" :: (ResourceId)
  }


newtype RegexMatchTuples = RegexMatchTuples (Array RegexMatchTuple)


-- | <p>The <code>RegexPatternSet</code> specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p>
newtype RegexPatternSet = RegexPatternSet 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "RegexPatternStrings" :: (RegexPatternStrings)
  }


newtype RegexPatternSetSummaries = RegexPatternSetSummaries (Array RegexPatternSetSummary)


-- | <p>Returned by <a>ListRegexPatternSets</a>. Each <code>RegexPatternSetSummary</code> object includes the <code>Name</code> and <code>RegexPatternSetId</code> for one <a>RegexPatternSet</a>.</p>
newtype RegexPatternSetSummary = RegexPatternSetSummary 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>In an <a>UpdateRegexPatternSet</a> request, <code>RegexPatternSetUpdate</code> specifies whether to insert or delete a <code>RegexPatternString</code> and includes the settings for the <code>RegexPatternString</code>.</p>
newtype RegexPatternSetUpdate = RegexPatternSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexPatternString" :: (RegexPatternString)
  }


newtype RegexPatternSetUpdates = RegexPatternSetUpdates (Array RegexPatternSetUpdate)


newtype RegexPatternString = RegexPatternString String


newtype RegexPatternStrings = RegexPatternStrings (Array RegexPatternString)


newtype ResourceArn = ResourceArn String


newtype ResourceArns = ResourceArns (Array ResourceArn)


newtype ResourceId = ResourceId String


newtype ResourceName = ResourceName String


-- | <p>A combination of <a>ByteMatchSet</a>, <a>IPSet</a>, and/or <a>SqlInjectionMatchSet</a> objects that identify the web requests that you want to allow, block, or count. For example, you might create a <code>Rule</code> that includes the following predicates:</p> <ul> <li> <p>An <code>IPSet</code> that causes AWS WAF to search for web requests that originate from the IP address <code>192.0.2.44</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that causes AWS WAF to search for web requests for which the value of the <code>User-Agent</code> header is <code>BadBot</code>.</p> </li> </ul> <p>To match the settings in this <code>Rule</code>, a request must originate from <code>192.0.2.44</code> AND include a <code>User-Agent</code> header for which the value is <code>BadBot</code>.</p>
newtype Rule = Rule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "Predicates" :: (Predicates)
  }


-- | <p>A collection of predefined rules that you can add to a web ACL.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul>
newtype RuleGroup = RuleGroup 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  }


newtype RuleGroupSummaries = RuleGroupSummaries (Array RuleGroupSummary)


-- | <p>Contains the identifier and the friendly name or description of the <code>RuleGroup</code>.</p>
newtype RuleGroupSummary = RuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies an <code>ActivatedRule</code> and indicates whether you want to add it to a <code>RuleGroup</code> or delete it from a <code>RuleGroup</code>.</p>
newtype RuleGroupUpdate = RuleGroupUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }


newtype RuleGroupUpdates = RuleGroupUpdates (Array RuleGroupUpdate)


newtype RulePriority = RulePriority Int


newtype RuleSummaries = RuleSummaries (Array RuleSummary)


-- | <p>Contains the identifier and the friendly name or description of the <code>Rule</code>.</p>
newtype RuleSummary = RuleSummary 
  { "RuleId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies a <code>Predicate</code> (such as an <code>IPSet</code>) and indicates whether you want to add it to a <code>Rule</code> or delete it from a <code>Rule</code>.</p>
newtype RuleUpdate = RuleUpdate 
  { "Action" :: (ChangeAction)
  , "Predicate" :: (Predicate)
  }


newtype RuleUpdates = RuleUpdates (Array RuleUpdate)


newtype SampleWeight = SampleWeight Number


-- | <p>The response from a <a>GetSampledRequests</a> request includes a <code>SampledHTTPRequests</code> complex type that appears as <code>SampledRequests</code> in the response syntax. <code>SampledHTTPRequests</code> contains one <code>SampledHTTPRequest</code> object for each web request that is returned by <code>GetSampledRequests</code>.</p>
newtype SampledHTTPRequest = SampledHTTPRequest 
  { "Request" :: (HTTPRequest)
  , "Weight" :: (SampleWeight)
  , "Number" :: NullOrUndefined (Number)
  , "Action" :: NullOrUndefined (Action)
  , "RuleWithinRuleGroup" :: NullOrUndefined (ResourceId)
  }


newtype SampledHTTPRequests = SampledHTTPRequests (Array SampledHTTPRequest)


newtype Size = Size Number


-- | <p>Specifies a constraint on the size of a part of the web request. AWS WAF uses the <code>Size</code>, <code>ComparisonOperator</code>, and <code>FieldToMatch</code> to build an expression in the form of "<code>Size</code> <code>ComparisonOperator</code> size in bytes of <code>FieldToMatch</code>". If that expression is true, the <code>SizeConstraint</code> is considered to match.</p>
newtype SizeConstraint = SizeConstraint 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Size" :: (Size)
  }


-- | <p>A complex type that contains <code>SizeConstraint</code> objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a <code>SizeConstraintSet</code> contains more than one <code>SizeConstraint</code> object, a request only needs to match one constraint to be considered a match.</p>
newtype SizeConstraintSet = SizeConstraintSet 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "SizeConstraints" :: (SizeConstraints)
  }


newtype SizeConstraintSetSummaries = SizeConstraintSetSummaries (Array SizeConstraintSetSummary)


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetSummary = SizeConstraintSetSummary 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a <a>SizeConstraintSet</a> or delete it from a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetUpdate = SizeConstraintSetUpdate 
  { "Action" :: (ChangeAction)
  , "SizeConstraint" :: (SizeConstraint)
  }


newtype SizeConstraintSetUpdates = SizeConstraintSetUpdates (Array SizeConstraintSetUpdate)


newtype SizeConstraints = SizeConstraints (Array SizeConstraint)


-- | <p>A complex type that contains <code>SqlInjectionMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a <code>SqlInjectionMatchSet</code> contains more than one <code>SqlInjectionMatchTuple</code> object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.</p>
newtype SqlInjectionMatchSet = SqlInjectionMatchSet 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "SqlInjectionMatchTuples" :: (SqlInjectionMatchTuples)
  }


newtype SqlInjectionMatchSetSummaries = SqlInjectionMatchSetSummaries (Array SqlInjectionMatchSetSummary)


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a <a>SqlInjectionMatchSet</a> or delete it from a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "SqlInjectionMatchTuple" :: (SqlInjectionMatchTuple)
  }


newtype SqlInjectionMatchSetUpdates = SqlInjectionMatchSetUpdates (Array SqlInjectionMatchSetUpdate)


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype SqlInjectionMatchTuple = SqlInjectionMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }


newtype SqlInjectionMatchTuples = SqlInjectionMatchTuples (Array SqlInjectionMatchTuple)


newtype SubscribedRuleGroupSummaries = SubscribedRuleGroupSummaries (Array SubscribedRuleGroupSummary)


-- | <p>A summary of the rule groups you are subscribed to.</p>
newtype SubscribedRuleGroupSummary = SubscribedRuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  }


newtype TextTransformation = TextTransformation String


-- | <p>In a <a>GetSampledRequests</a> request, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which you want AWS WAF to return a sample of web requests.</p> <p>In a <a>GetSampledRequests</a> response, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, <code>EndTime</code> is the time that AWS WAF received the 5,000th request. </p>
newtype TimeWindow = TimeWindow 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  }


newtype URIString = URIString String


newtype UpdateByteMatchSetRequest = UpdateByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (ByteMatchSetUpdates)
  }


newtype UpdateByteMatchSetResponse = UpdateByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateGeoMatchSetRequest = UpdateGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (GeoMatchSetUpdates)
  }


newtype UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (IPSetUpdates)
  }


newtype UpdateIPSetResponse = UpdateIPSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateRateBasedRuleRequest = UpdateRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  , "RateLimit" :: (RateLimit)
  }


newtype UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateRegexMatchSetRequest = UpdateRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "Updates" :: (RegexMatchSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }


newtype UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateRegexPatternSetRequest = UpdateRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "Updates" :: (RegexPatternSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }


newtype UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateRuleGroupRequest = UpdateRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "Updates" :: (RuleGroupUpdates)
  , "ChangeToken" :: (ChangeToken)
  }


newtype UpdateRuleGroupResponse = UpdateRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateRuleRequest = UpdateRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  }


newtype UpdateRuleResponse = UpdateRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateSizeConstraintSetRequest = UpdateSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SizeConstraintSetUpdates)
  }


newtype UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to update a <a>SqlInjectionMatchSet</a>.</p>
newtype UpdateSqlInjectionMatchSetRequest = UpdateSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SqlInjectionMatchSetUpdates)
  }


-- | <p>The response to an <a>UpdateSqlInjectionMatchSets</a> request.</p>
newtype UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


newtype UpdateWebACLRequest = UpdateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: NullOrUndefined (WebACLUpdates)
  , "DefaultAction" :: NullOrUndefined (WafAction)
  }


newtype UpdateWebACLResponse = UpdateWebACLResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>A request to update an <a>XssMatchSet</a>.</p>
newtype UpdateXssMatchSetRequest = UpdateXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (XssMatchSetUpdates)
  }


-- | <p>The response to an <a>UpdateXssMatchSets</a> request.</p>
newtype UpdateXssMatchSetResponse = UpdateXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }


-- | <p>The name specified is invalid.</p>
newtype WAFDisallowedNameException = WAFDisallowedNameException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because of a system problem, even though the request was valid. Retry your request.</p>
newtype WAFInternalErrorException = WAFInternalErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.</p>
newtype WAFInvalidAccountException = WAFInvalidAccountException 
  { 
  }


-- | <p>The operation failed because there was nothing to do. For example:</p> <ul> <li> <p>You tried to remove a <code>Rule</code> from a <code>WebACL</code>, but the <code>Rule</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to remove an IP address from an <code>IPSet</code>, but the IP address isn't in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to remove a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add a <code>Rule</code> to a <code>WebACL</code>, but the <code>Rule</code> already exists in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add an IP address to an <code>IPSet</code>, but the IP address already exists in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> already exists in the specified <code>WebACL</code>.</p> </li> </ul>
newtype WAFInvalidOperationException = WAFInvalidOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because AWS WAF didn't recognize a parameter in the request. For example:</p> <ul> <li> <p>You specified an invalid parameter name.</p> </li> <li> <p>You specified an invalid value.</p> </li> <li> <p>You tried to update an object (<code>ByteMatchSet</code>, <code>IPSet</code>, <code>Rule</code>, or <code>WebACL</code>) using an action other than <code>INSERT</code> or <code>DELETE</code>.</p> </li> <li> <p>You tried to create a <code>WebACL</code> with a <code>DefaultAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to create a <code>RateBasedRule</code> with a <code>RateKey</code> value other than <code>IP</code>.</p> </li> <li> <p>You tried to update a <code>WebACL</code> with a <code>WafAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>FieldToMatch</code> <code>Type</code> other than HEADER, METHOD, QUERY_STRING, URI, or BODY.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>Field</code> of <code>HEADER</code> but no value for <code>Data</code>.</p> </li> <li> <p>Your request references an ARN that is malformed, or corresponds to a resource with which a web ACL cannot be associated.</p> </li> </ul>
newtype WAFInvalidParameterException = WAFInvalidParameterException 
  { "Field'" :: NullOrUndefined (ParameterExceptionField)
  , "Parameter'" :: NullOrUndefined (ParameterExceptionParameter)
  , "Reason'" :: NullOrUndefined (ParameterExceptionReason)
  }


-- | <p>The operation failed because the specified policy is not in the proper format. </p> <p>The policy is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> or <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul>
newtype WAFInvalidPermissionPolicyException = WAFInvalidPermissionPolicyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The regular expression (regex) you specified in <code>RegexPatternString</code> is invalid.</p>
newtype WAFInvalidRegexPatternException = WAFInvalidRegexPatternException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation exceeds a resource limit, for example, the maximum number of <code>WebACL</code> objects that you can create for an AWS account. For more information, see <a href="http://docs.aws.amazon.com/waf/latest/developerguide/limits.html">Limits</a> in the <i>AWS WAF Developer Guide</i>.</p>
newtype WAFLimitsExceededException = WAFLimitsExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because you tried to delete an object that isn't empty. For example:</p> <ul> <li> <p>You tried to delete a <code>WebACL</code> that still contains one or more <code>Rule</code> objects.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that still contains one or more <code>ByteMatchSet</code> objects or other predicates.</p> </li> <li> <p>You tried to delete a <code>ByteMatchSet</code> that contains one or more <code>ByteMatchTuple</code> objects.</p> </li> <li> <p>You tried to delete an <code>IPSet</code> that references one or more IP addresses.</p> </li> </ul>
newtype WAFNonEmptyEntityException = WAFNonEmptyEntityException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:</p> <ul> <li> <p>You tried to add a <code>Rule</code> to or delete a <code>Rule</code> from a <code>WebACL</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchSet</code> to or delete a <code>ByteMatchSet</code> from a <code>Rule</code> that doesn't exist.</p> </li> <li> <p>You tried to add an IP address to or delete an IP address from an <code>IPSet</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to or delete a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code> that doesn't exist.</p> </li> </ul>
newtype WAFNonexistentContainerException = WAFNonexistentContainerException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because the referenced object doesn't exist.</p>
newtype WAFNonexistentItemException = WAFNonexistentItemException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because you tried to delete an object that is still in use. For example:</p> <ul> <li> <p>You tried to delete a <code>ByteMatchSet</code> that is still referenced by a <code>Rule</code>.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that is still referenced by a <code>WebACL</code>.</p> </li> </ul>
newtype WAFReferencedItemException = WAFReferencedItemException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.</p>
newtype WAFStaleDataException = WAFStaleDataException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The specified subscription does not exist.</p>
newtype WAFSubscriptionNotFoundException = WAFSubscriptionNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The operation failed because the entity referenced is temporarily unavailable. Retry your request.</p>
newtype WAFUnavailableEntityException = WAFUnavailableEntityException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>For the action that is associated with a rule in a <code>WebACL</code>, specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a <code>WebACL</code>, specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a <code>WebACL</code>. </p>
newtype WafAction = WafAction 
  { "Type" :: (WafActionType)
  }


newtype WafActionType = WafActionType String


-- | <p>The action to take if any rule within the <code>RuleGroup</code> matches a request. </p>
newtype WafOverrideAction = WafOverrideAction 
  { "Type" :: (WafOverrideActionType)
  }


newtype WafOverrideActionType = WafOverrideActionType String


newtype WafRuleType = WafRuleType String


-- | <p>Contains the <code>Rules</code> that identify the requests that you want to allow, block, or count. In a <code>WebACL</code>, you also specify a default action (<code>ALLOW</code> or <code>BLOCK</code>), and the action for each <code>Rule</code> that you add to a <code>WebACL</code>, for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the <code>WebACL</code> with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one <code>Rule</code> to a <code>WebACL</code>, a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see <a>UpdateWebACL</a>.</p>
newtype WebACL = WebACL 
  { "WebACLId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "DefaultAction" :: (WafAction)
  , "Rules" :: (ActivatedRules)
  }


newtype WebACLSummaries = WebACLSummaries (Array WebACLSummary)


-- | <p>Contains the identifier and the name or description of the <a>WebACL</a>.</p>
newtype WebACLSummary = WebACLSummary 
  { "WebACLId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies whether to insert a <code>Rule</code> into or delete a <code>Rule</code> from a <code>WebACL</code>.</p>
newtype WebACLUpdate = WebACLUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }


newtype WebACLUpdates = WebACLUpdates (Array WebACLUpdate)


-- | <p>A complex type that contains <code>XssMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a <code>XssMatchSet</code> contains more than one <code>XssMatchTuple</code> object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.</p>
newtype XssMatchSet = XssMatchSet 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "XssMatchTuples" :: (XssMatchTuples)
  }


newtype XssMatchSetSummaries = XssMatchSetSummaries (Array XssMatchSetSummary)


-- | <p>The <code>Id</code> and <code>Name</code> of an <code>XssMatchSet</code>.</p>
newtype XssMatchSetSummary = XssMatchSetSummary 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }


-- | <p>Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an <a>XssMatchSet</a> or delete it from an <code>XssMatchSet</code>.</p>
newtype XssMatchSetUpdate = XssMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "XssMatchTuple" :: (XssMatchTuple)
  }


newtype XssMatchSetUpdates = XssMatchSetUpdates (Array XssMatchSetUpdate)


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype XssMatchTuple = XssMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }


newtype XssMatchTuples = XssMatchTuples (Array XssMatchTuple)


newtype ErrorMessage' = ErrorMessage' String
