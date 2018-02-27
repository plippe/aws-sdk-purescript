

-- | <p>This is the <i>AWS WAF Regional API Reference</i> for using AWS WAF with Elastic Load Balancing (ELB) Application Load Balancers. The AWS WAF actions and data types listed in the reference are available for protecting Application Load Balancers. You can use these actions and data types by means of the endpoints listed in <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region">AWS Regions and Endpoints</a>. This guide is for developers who need detailed information about the AWS WAF API actions, data types, and errors. For detailed information about AWS WAF features and an overview of how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
module AWS.WAFRegional where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "WAFRegional" :: String


-- | <p>Associates a web ACL with a resource.</p>
associateWebACL :: forall eff. AssociateWebACLRequest -> Aff (err :: AWS.RequestError | eff) AssociateWebACLResponse
associateWebACL = AWS.request serviceName "associateWebACL" 


-- | <p>Creates a <code>ByteMatchSet</code>. You then use <a>UpdateByteMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>ByteMatchSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>BadBot</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateByteMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createByteMatchSet :: forall eff. CreateByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateByteMatchSetResponse
createByteMatchSet = AWS.request serviceName "createByteMatchSet" 


-- | <p>Creates an <a>GeoMatchSet</a>, which you use to specify which web requests you want to allow or block based on the country that the requests originate from. For example, if you're receiving a lot of requests from one or more countries and you want to block the requests, you can create an <code>GeoMatchSet</code> that contains those countries and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSetSet</code> request to specify the countries that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createGeoMatchSet :: forall eff. CreateGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateGeoMatchSetResponse
createGeoMatchSet = AWS.request serviceName "createGeoMatchSet" 


-- | <p>Creates an <a>IPSet</a>, which you use to specify which web requests you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that contains those IP addresses and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateIPSet</code> request.</p> </li> <li> <p>Submit a <code>CreateIPSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createIPSet :: forall eff. CreateIPSetRequest -> Aff (err :: AWS.RequestError | eff) CreateIPSetResponse
createIPSet = AWS.request serviceName "createIPSet" 


-- | <p>Creates a <a>RateBasedRule</a>. The <code>RateBasedRule</code> contains a <code>RateLimit</code>, which specifies the maximum number of requests that AWS WAF allows from a specified IP address in a five-minute period. The <code>RateBasedRule</code> also contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to count or block if these requests exceed the <code>RateLimit</code>.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request not only must exceed the <code>RateLimit</code>, but it also must match all the specifications to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that meet the conditions in the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions must be received at a rate of more than 15,000 requests every five minutes. If both conditions are met and the rate is exceeded, AWS WAF blocks the requests. If the rate drops below 15,000 for a five-minute period, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p> <p>To create and configure a <code>RateBasedRule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the rule. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRateBasedRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRateBasedRule</code> request to specify the predicates that you want to include in the rule.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RateBasedRule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRateBasedRule :: forall eff. CreateRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) CreateRateBasedRuleResponse
createRateBasedRule = AWS.request serviceName "createRateBasedRule" 


-- | <p>Creates a <a>RegexMatchSet</a>. You then use <a>UpdateRegexMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>RegexMatchSet</code> that contains a <code>RegexMatchTuple</code> that looks for any requests with <code>User-Agent</code> headers that match a <code>RegexPatternSet</code> with pattern <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value, using a <code>RegexPatternSet</code>, that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexMatchSet :: forall eff. CreateRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateRegexMatchSetResponse
createRegexMatchSet = AWS.request serviceName "createRegexMatchSet" 


-- | <p>Creates a <code>RegexPatternSet</code>. You then use <a>UpdateRegexPatternSet</a> to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexPatternSet</a> request to specify the string that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexPatternSet :: forall eff. CreateRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) CreateRegexPatternSetResponse
createRegexPatternSet = AWS.request serviceName "createRegexPatternSet" 


-- | <p>Creates a <code>Rule</code>, which contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to block. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed or blocked. For example, suppose you add the following to a <code>Rule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to blocks requests that satisfy the <code>Rule</code>. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to specify the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRule :: forall eff. CreateRuleRequest -> Aff (err :: AWS.RequestError | eff) CreateRuleResponse
createRule = AWS.request serviceName "createRule" 


-- | <p>Creates a <code>RuleGroup</code>. A rule group is a collection of predefined rules that you add to a web ACL. You use <a>UpdateRuleGroup</a> to add rules to the rule group.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRuleGroup :: forall eff. CreateRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateRuleGroupResponse
createRuleGroup = AWS.request serviceName "createRuleGroup" 


-- | <p>Creates a <code>SizeConstraintSet</code>. You then use <a>UpdateSizeConstraintSet</a> to identify the part of a web request that you want AWS WAF to check for length, such as the length of the <code>User-Agent</code> header or the length of the query string. For example, you can create a <code>SizeConstraintSet</code> that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateSizeConstraintSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSizeConstraintSet :: forall eff. CreateSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) CreateSizeConstraintSetResponse
createSizeConstraintSet = AWS.request serviceName "createSizeConstraintSet" 


-- | <p>Creates a <a>SqlInjectionMatchSet</a>, which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateSqlInjectionMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSqlInjectionMatchSet :: forall eff. CreateSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateSqlInjectionMatchSetResponse
createSqlInjectionMatchSet = AWS.request serviceName "createSqlInjectionMatchSet" 


-- | <p>Creates a <code>WebACL</code>, which contains the <code>Rules</code> that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates <code>Rules</code> in order based on the value of <code>Priority</code> for each <code>Rule</code>.</p> <p>You also specify a default action, either <code>ALLOW</code> or <code>BLOCK</code>. If a web request doesn't match any of the <code>Rules</code> in a <code>WebACL</code>, AWS WAF responds to the request with the default action. </p> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>ByteMatchSet</code> objects and other predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateWebACL</code> request.</p> </li> <li> <p>Submit a <code>CreateWebACL</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <a>UpdateWebACL</a> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution.</p> </li> </ol> <p>For more information about how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createWebACL :: forall eff. CreateWebACLRequest -> Aff (err :: AWS.RequestError | eff) CreateWebACLResponse
createWebACL = AWS.request serviceName "createWebACL" 


-- | <p>Creates an <a>XssMatchSet</a>, which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateXssMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateXssMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createXssMatchSet :: forall eff. CreateXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) CreateXssMatchSetResponse
createXssMatchSet = AWS.request serviceName "createXssMatchSet" 


-- | <p>Permanently deletes a <a>ByteMatchSet</a>. You can't delete a <code>ByteMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>ByteMatchTuple</a> objects (any filters).</p> <p>If you just want to remove a <code>ByteMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>ByteMatchSet</code> to remove filters, if any. For more information, see <a>UpdateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteByteMatchSet</code> request.</p> </li> </ol>
deleteByteMatchSet :: forall eff. DeleteByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteByteMatchSetResponse
deleteByteMatchSet = AWS.request serviceName "deleteByteMatchSet" 


-- | <p>Permanently deletes a <a>GeoMatchSet</a>. You can't delete a <code>GeoMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any countries.</p> <p>If you just want to remove a <code>GeoMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>GeoMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>GeoMatchSet</code> to remove any countries. For more information, see <a>UpdateGeoMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteGeoMatchSet</code> request.</p> </li> </ol>
deleteGeoMatchSet :: forall eff. DeleteGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteGeoMatchSetResponse
deleteGeoMatchSet = AWS.request serviceName "deleteGeoMatchSet" 


-- | <p>Permanently deletes an <a>IPSet</a>. You can't delete an <code>IPSet</code> if it's still used in any <code>Rules</code> or if it still includes any IP addresses.</p> <p>If you just want to remove an <code>IPSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>IPSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>IPSet</code> to remove IP address ranges, if any. For more information, see <a>UpdateIPSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteIPSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteIPSet</code> request.</p> </li> </ol>
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteIPSetResponse
deleteIPSet = AWS.request serviceName "deleteIPSet" 


-- | <p>Permanently deletes an IAM policy from the specified RuleGroup.</p> <p>The user making the request must be the owner of the RuleGroup.</p>
deletePermissionPolicy :: forall eff. DeletePermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeletePermissionPolicyResponse
deletePermissionPolicy = AWS.request serviceName "deletePermissionPolicy" 


-- | <p>Permanently deletes a <a>RateBasedRule</a>. You can't delete a rule if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a rule from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RateBasedRule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RateBasedRule</code> to remove predicates, if any. For more information, see <a>UpdateRateBasedRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRateBasedRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRateBasedRule</code> request.</p> </li> </ol>
deleteRateBasedRule :: forall eff. DeleteRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) DeleteRateBasedRuleResponse
deleteRateBasedRule = AWS.request serviceName "deleteRateBasedRule" 


-- | <p>Permanently deletes a <a>RegexMatchSet</a>. You can't delete a <code>RegexMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <code>RegexMatchTuples</code> objects (any filters).</p> <p>If you just want to remove a <code>RegexMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>RegexMatchSet</code> to remove filters, if any. For more information, see <a>UpdateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteRegexMatchSet</code> request.</p> </li> </ol>
deleteRegexMatchSet :: forall eff. DeleteRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegexMatchSetResponse
deleteRegexMatchSet = AWS.request serviceName "deleteRegexMatchSet" 


-- | <p>Permanently deletes a <a>RegexPatternSet</a>. You can't delete a <code>RegexPatternSet</code> if it's still used in any <code>RegexMatchSet</code> or if the <code>RegexPatternSet</code> is not empty. </p>
deleteRegexPatternSet :: forall eff. DeleteRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteRegexPatternSetResponse
deleteRegexPatternSet = AWS.request serviceName "deleteRegexPatternSet" 


-- | <p>Permanently deletes a <a>Rule</a>. You can't delete a <code>Rule</code> if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a <code>Rule</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>Rule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>Rule</code> to remove predicates, if any. For more information, see <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRule</code> request.</p> </li> </ol>
deleteRule :: forall eff. DeleteRuleRequest -> Aff (err :: AWS.RequestError | eff) DeleteRuleResponse
deleteRule = AWS.request serviceName "deleteRule" 


-- | <p>Permanently deletes a <a>RuleGroup</a>. You can't delete a <code>RuleGroup</code> if it's still used in any <code>WebACL</code> objects or if it still includes any rules.</p> <p>If you just want to remove a <code>RuleGroup</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RuleGroup</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RuleGroup</code> to remove rules, if any. For more information, see <a>UpdateRuleGroup</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRuleGroup</code> request.</p> </li> <li> <p>Submit a <code>DeleteRuleGroup</code> request.</p> </li> </ol>
deleteRuleGroup :: forall eff. DeleteRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteRuleGroupResponse
deleteRuleGroup = AWS.request serviceName "deleteRuleGroup" 


-- | <p>Permanently deletes a <a>SizeConstraintSet</a>. You can't delete a <code>SizeConstraintSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>SizeConstraint</a> objects (any filters).</p> <p>If you just want to remove a <code>SizeConstraintSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>SizeConstraintSet</code> to remove filters, if any. For more information, see <a>UpdateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSizeConstraintSet</code> request.</p> </li> </ol>
deleteSizeConstraintSet :: forall eff. DeleteSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteSizeConstraintSetResponse
deleteSizeConstraintSet = AWS.request serviceName "deleteSizeConstraintSet" 


-- | <p>Permanently deletes a <a>SqlInjectionMatchSet</a>. You can't delete a <code>SqlInjectionMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>SqlInjectionMatchTuple</a> objects.</p> <p>If you just want to remove a <code>SqlInjectionMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SqlInjectionMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>SqlInjectionMatchSet</code> to remove filters, if any. For more information, see <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> </ol>
deleteSqlInjectionMatchSet :: forall eff. DeleteSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteSqlInjectionMatchSetResponse
deleteSqlInjectionMatchSet = AWS.request serviceName "deleteSqlInjectionMatchSet" 


-- | <p>Permanently deletes a <a>WebACL</a>. You can't delete a <code>WebACL</code> if it still contains any <code>Rules</code>.</p> <p>To delete a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>WebACL</code> to remove <code>Rules</code>, if any. For more information, see <a>UpdateWebACL</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteWebACL</code> request.</p> </li> <li> <p>Submit a <code>DeleteWebACL</code> request.</p> </li> </ol>
deleteWebACL :: forall eff. DeleteWebACLRequest -> Aff (err :: AWS.RequestError | eff) DeleteWebACLResponse
deleteWebACL = AWS.request serviceName "deleteWebACL" 


-- | <p>Permanently deletes an <a>XssMatchSet</a>. You can't delete an <code>XssMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>XssMatchTuple</a> objects.</p> <p>If you just want to remove an <code>XssMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>XssMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>XssMatchSet</code> to remove filters, if any. For more information, see <a>UpdateXssMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteXssMatchSet</code> request.</p> </li> </ol>
deleteXssMatchSet :: forall eff. DeleteXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteXssMatchSetResponse
deleteXssMatchSet = AWS.request serviceName "deleteXssMatchSet" 


-- | <p>Removes a web ACL from the specified resource.</p>
disassociateWebACL :: forall eff. DisassociateWebACLRequest -> Aff (err :: AWS.RequestError | eff) DisassociateWebACLResponse
disassociateWebACL = AWS.request serviceName "disassociateWebACL" 


-- | <p>Returns the <a>ByteMatchSet</a> specified by <code>ByteMatchSetId</code>.</p>
getByteMatchSet :: forall eff. GetByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetByteMatchSetResponse
getByteMatchSet = AWS.request serviceName "getByteMatchSet" 


-- | <p>When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request. Change tokens ensure that your application doesn't submit conflicting requests to AWS WAF.</p> <p>Each create, update, or delete request must use a unique change token. If your application submits a <code>GetChangeToken</code> request and then submits a second <code>GetChangeToken</code> request before submitting a create, update, or delete request, the second <code>GetChangeToken</code> request returns the same value as the first <code>GetChangeToken</code> request.</p> <p>When you use a change token in a create, update, or delete request, the status of the change token changes to <code>PENDING</code>, which indicates that AWS WAF is propagating the change to all AWS WAF servers. Use <code>GetChangeTokenStatus</code> to determine the status of your change token.</p>
getChangeToken :: forall eff. GetChangeTokenRequest -> Aff (err :: AWS.RequestError | eff) GetChangeTokenResponse
getChangeToken = AWS.request serviceName "getChangeToken" 


-- | <p>Returns the status of a <code>ChangeToken</code> that you got by calling <a>GetChangeToken</a>. <code>ChangeTokenStatus</code> is one of the following values:</p> <ul> <li> <p> <code>PROVISIONED</code>: You requested the change token by calling <code>GetChangeToken</code>, but you haven't used it yet in a call to create, update, or delete an AWS WAF object.</p> </li> <li> <p> <code>PENDING</code>: AWS WAF is propagating the create, update, or delete request to all AWS WAF servers.</p> </li> <li> <p> <code>IN_SYNC</code>: Propagation is complete.</p> </li> </ul>
getChangeTokenStatus :: forall eff. GetChangeTokenStatusRequest -> Aff (err :: AWS.RequestError | eff) GetChangeTokenStatusResponse
getChangeTokenStatus = AWS.request serviceName "getChangeTokenStatus" 


-- | <p>Returns the <a>GeoMatchSet</a> that is specified by <code>GeoMatchSetId</code>.</p>
getGeoMatchSet :: forall eff. GetGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetGeoMatchSetResponse
getGeoMatchSet = AWS.request serviceName "getGeoMatchSet" 


-- | <p>Returns the <a>IPSet</a> that is specified by <code>IPSetId</code>.</p>
getIPSet :: forall eff. GetIPSetRequest -> Aff (err :: AWS.RequestError | eff) GetIPSetResponse
getIPSet = AWS.request serviceName "getIPSet" 


-- | <p>Returns the IAM policy attached to the RuleGroup.</p>
getPermissionPolicy :: forall eff. GetPermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetPermissionPolicyResponse
getPermissionPolicy = AWS.request serviceName "getPermissionPolicy" 


-- | <p>Returns the <a>RateBasedRule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRateBasedRule</code> request.</p>
getRateBasedRule :: forall eff. GetRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) GetRateBasedRuleResponse
getRateBasedRule = AWS.request serviceName "getRateBasedRule" 


-- | <p>Returns an array of IP addresses currently being blocked by the <a>RateBasedRule</a> that is specified by the <code>RuleId</code>. The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.</p>
getRateBasedRuleManagedKeys :: forall eff. GetRateBasedRuleManagedKeysRequest -> Aff (err :: AWS.RequestError | eff) GetRateBasedRuleManagedKeysResponse
getRateBasedRuleManagedKeys = AWS.request serviceName "getRateBasedRuleManagedKeys" 


-- | <p>Returns the <a>RegexMatchSet</a> specified by <code>RegexMatchSetId</code>.</p>
getRegexMatchSet :: forall eff. GetRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetRegexMatchSetResponse
getRegexMatchSet = AWS.request serviceName "getRegexMatchSet" 


-- | <p>Returns the <a>RegexPatternSet</a> specified by <code>RegexPatternSetId</code>.</p>
getRegexPatternSet :: forall eff. GetRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) GetRegexPatternSetResponse
getRegexPatternSet = AWS.request serviceName "getRegexPatternSet" 


-- | <p>Returns the <a>Rule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRule</code> request.</p>
getRule :: forall eff. GetRuleRequest -> Aff (err :: AWS.RequestError | eff) GetRuleResponse
getRule = AWS.request serviceName "getRule" 


-- | <p>Returns the <a>RuleGroup</a> that is specified by the <code>RuleGroupId</code> that you included in the <code>GetRuleGroup</code> request.</p> <p>To view the rules in a rule group, use <a>ListActivatedRulesInRuleGroup</a>.</p>
getRuleGroup :: forall eff. GetRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) GetRuleGroupResponse
getRuleGroup = AWS.request serviceName "getRuleGroup" 


-- | <p>Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose. You can specify a sample size of up to 500 requests, and you can specify any time range in the previous three hours.</p> <p> <code>GetSampledRequests</code> returns a time range, which is usually the time range that you specified. However, if your resource (such as a CloudFront distribution) received 5,000 requests before the specified time range elapsed, <code>GetSampledRequests</code> returns an updated time range. This new time range indicates the actual period during which AWS WAF selected the requests in the sample.</p>
getSampledRequests :: forall eff. GetSampledRequestsRequest -> Aff (err :: AWS.RequestError | eff) GetSampledRequestsResponse
getSampledRequests = AWS.request serviceName "getSampledRequests" 


-- | <p>Returns the <a>SizeConstraintSet</a> specified by <code>SizeConstraintSetId</code>.</p>
getSizeConstraintSet :: forall eff. GetSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) GetSizeConstraintSetResponse
getSizeConstraintSet = AWS.request serviceName "getSizeConstraintSet" 


-- | <p>Returns the <a>SqlInjectionMatchSet</a> that is specified by <code>SqlInjectionMatchSetId</code>.</p>
getSqlInjectionMatchSet :: forall eff. GetSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetSqlInjectionMatchSetResponse
getSqlInjectionMatchSet = AWS.request serviceName "getSqlInjectionMatchSet" 


-- | <p>Returns the <a>WebACL</a> that is specified by <code>WebACLId</code>.</p>
getWebACL :: forall eff. GetWebACLRequest -> Aff (err :: AWS.RequestError | eff) GetWebACLResponse
getWebACL = AWS.request serviceName "getWebACL" 


-- | <p>Returns the web ACL for the specified resource.</p>
getWebACLForResource :: forall eff. GetWebACLForResourceRequest -> Aff (err :: AWS.RequestError | eff) GetWebACLForResourceResponse
getWebACLForResource = AWS.request serviceName "getWebACLForResource" 


-- | <p>Returns the <a>XssMatchSet</a> that is specified by <code>XssMatchSetId</code>.</p>
getXssMatchSet :: forall eff. GetXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) GetXssMatchSetResponse
getXssMatchSet = AWS.request serviceName "getXssMatchSet" 


-- | <p>Returns an array of <a>ActivatedRule</a> objects.</p>
listActivatedRulesInRuleGroup :: forall eff. ListActivatedRulesInRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) ListActivatedRulesInRuleGroupResponse
listActivatedRulesInRuleGroup = AWS.request serviceName "listActivatedRulesInRuleGroup" 


-- | <p>Returns an array of <a>ByteMatchSetSummary</a> objects.</p>
listByteMatchSets :: forall eff. ListByteMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListByteMatchSetsResponse
listByteMatchSets = AWS.request serviceName "listByteMatchSets" 


-- | <p>Returns an array of <a>GeoMatchSetSummary</a> objects in the response.</p>
listGeoMatchSets :: forall eff. ListGeoMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListGeoMatchSetsResponse
listGeoMatchSets = AWS.request serviceName "listGeoMatchSets" 


-- | <p>Returns an array of <a>IPSetSummary</a> objects in the response.</p>
listIPSets :: forall eff. ListIPSetsRequest -> Aff (err :: AWS.RequestError | eff) ListIPSetsResponse
listIPSets = AWS.request serviceName "listIPSets" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRateBasedRules :: forall eff. ListRateBasedRulesRequest -> Aff (err :: AWS.RequestError | eff) ListRateBasedRulesResponse
listRateBasedRules = AWS.request serviceName "listRateBasedRules" 


-- | <p>Returns an array of <a>RegexMatchSetSummary</a> objects.</p>
listRegexMatchSets :: forall eff. ListRegexMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListRegexMatchSetsResponse
listRegexMatchSets = AWS.request serviceName "listRegexMatchSets" 


-- | <p>Returns an array of <a>RegexPatternSetSummary</a> objects.</p>
listRegexPatternSets :: forall eff. ListRegexPatternSetsRequest -> Aff (err :: AWS.RequestError | eff) ListRegexPatternSetsResponse
listRegexPatternSets = AWS.request serviceName "listRegexPatternSets" 


-- | <p>Returns an array of resources associated with the specified web ACL.</p>
listResourcesForWebACL :: forall eff. ListResourcesForWebACLRequest -> Aff (err :: AWS.RequestError | eff) ListResourcesForWebACLResponse
listResourcesForWebACL = AWS.request serviceName "listResourcesForWebACL" 


-- | <p>Returns an array of <a>RuleGroup</a> objects.</p>
listRuleGroups :: forall eff. ListRuleGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListRuleGroupsResponse
listRuleGroups = AWS.request serviceName "listRuleGroups" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRules :: forall eff. ListRulesRequest -> Aff (err :: AWS.RequestError | eff) ListRulesResponse
listRules = AWS.request serviceName "listRules" 


-- | <p>Returns an array of <a>SizeConstraintSetSummary</a> objects.</p>
listSizeConstraintSets :: forall eff. ListSizeConstraintSetsRequest -> Aff (err :: AWS.RequestError | eff) ListSizeConstraintSetsResponse
listSizeConstraintSets = AWS.request serviceName "listSizeConstraintSets" 


-- | <p>Returns an array of <a>SqlInjectionMatchSet</a> objects.</p>
listSqlInjectionMatchSets :: forall eff. ListSqlInjectionMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListSqlInjectionMatchSetsResponse
listSqlInjectionMatchSets = AWS.request serviceName "listSqlInjectionMatchSets" 


-- | <p>Returns an array of <a>RuleGroup</a> objects that you are subscribed to.</p>
listSubscribedRuleGroups :: forall eff. ListSubscribedRuleGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListSubscribedRuleGroupsResponse
listSubscribedRuleGroups = AWS.request serviceName "listSubscribedRuleGroups" 


-- | <p>Returns an array of <a>WebACLSummary</a> objects in the response.</p>
listWebACLs :: forall eff. ListWebACLsRequest -> Aff (err :: AWS.RequestError | eff) ListWebACLsResponse
listWebACLs = AWS.request serviceName "listWebACLs" 


-- | <p>Returns an array of <a>XssMatchSet</a> objects.</p>
listXssMatchSets :: forall eff. ListXssMatchSetsRequest -> Aff (err :: AWS.RequestError | eff) ListXssMatchSetsResponse
listXssMatchSets = AWS.request serviceName "listXssMatchSets" 


-- | <p>Attaches a IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.</p> <p>The <code>PutPermissionPolicy</code> is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> and <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html">IAM Policies</a>. </p> <p>An example of a valid policy parameter is shown in the Examples section below.</p>
putPermissionPolicy :: forall eff. PutPermissionPolicyRequest -> Aff (err :: AWS.RequestError | eff) PutPermissionPolicyResponse
putPermissionPolicy = AWS.request serviceName "putPermissionPolicy" 


-- | <p>Inserts or deletes <a>ByteMatchTuple</a> objects (filters) in a <a>ByteMatchSet</a>. For each <code>ByteMatchTuple</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>ByteMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see <code>TargetString</code> in the <a>ByteMatchTuple</a> data type. </p> </li> <li> <p>Where to look, such as at the beginning or the end of a query string.</p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p>For example, you can add a <code>ByteMatchSetUpdate</code> object that matches web requests in which <code>User-Agent</code> headers contain the string <code>BadBot</code>. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>ByteMatchSet.</code> For more information, see <a>CreateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateByteMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateByteMatchSet :: forall eff. UpdateByteMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateByteMatchSetResponse
updateByteMatchSet = AWS.request serviceName "updateByteMatchSet" 


-- | <p>Inserts or deletes <a>GeoMatchConstraint</a> objects in an <code>GeoMatchSet</code>. For each <code>GeoMatchConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>GeoMatchConstraint</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The <code>Type</code>. The only valid value for <code>Type</code> is <code>Country</code>.</p> </li> <li> <p>The <code>Value</code>, which is a two character code for the country to add to the <code>GeoMatchConstraint</code> object. Valid codes are listed in <a>GeoMatchConstraint$Value</a>.</p> </li> </ul> <p>To create and configure an <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateGeoMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSet</code> request to specify the country that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>GeoMatchSet</code>, you specify the country that you want to add and/or the country that you want to delete. If you want to change a country, you delete the existing country and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateGeoMatchSet :: forall eff. UpdateGeoMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateGeoMatchSetResponse
updateGeoMatchSet = AWS.request serviceName "updateGeoMatchSet" 


-- | <p>Inserts or deletes <a>IPSetDescriptor</a> objects in an <code>IPSet</code>. For each <code>IPSetDescriptor</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>IPSetDescriptor</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The IP address version, <code>IPv4</code> or <code>IPv6</code>. </p> </li> <li> <p>The IP address in CIDR notation, for example, <code>192.0.2.0/24</code> (for the range of IP addresses from <code>192.0.2.0</code> to <code>192.0.2.255</code>) or <code>192.0.2.44/32</code> (for the individual IP address <code>192.0.2.44</code>). </p> </li> </ul> <p>AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>.</p> <p>IPv6 addresses can be represented using any of the following formats:</p> <ul> <li> <p>1111:0000:0000:0000:0000:0000:0000:0111/128</p> </li> <li> <p>1111:0:0:0:0:0:0:0111/128</p> </li> <li> <p>1111::0111/128</p> </li> <li> <p>1111::111/128</p> </li> </ul> <p>You use an <code>IPSet</code> to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you're receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that specifies those IP addresses, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateIPSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>IPSet</code>, you specify the IP addresses that you want to add and/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateIPSetResponse
updateIPSet = AWS.request serviceName "updateIPSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a rule and updates the <code>RateLimit</code> in the rule. </p> <p>Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to block or count. The <code>RateLimit</code> specifies the number of requests every five minutes that triggers the rule.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request must match all the predicates and exceed the <code>RateLimit</code> to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions much be received at a rate of more than 15,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p>
updateRateBasedRule :: forall eff. UpdateRateBasedRuleRequest -> Aff (err :: AWS.RequestError | eff) UpdateRateBasedRuleResponse
updateRateBasedRule = AWS.request serviceName "updateRateBasedRule" 


-- | <p>Inserts or deletes <a>RegexMatchTuple</a> objects (filters) in a <a>RegexMatchSet</a>. For each <code>RegexMatchSetUpdate</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>RegexMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p> For example, you can create a <code>RegexPatternSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexMatchSet.</code> For more information, see <a>CreateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the <code>RegexPatternSet</code> that contain the regular expression patters you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexMatchSet :: forall eff. UpdateRegexMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateRegexMatchSetResponse
updateRegexMatchSet = AWS.request serviceName "updateRegexMatchSet" 


-- | <p>Inserts or deletes <code>RegexPatternString</code> objects in a <a>RegexPatternSet</a>. For each <code>RegexPatternString</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the <code>RegexPatternString</code>.</p> </li> <li> <p>The regular expression pattern that you want to insert or delete. For more information, see <a>RegexPatternSet</a>. </p> </li> </ul> <p> For example, you can create a <code>RegexPatternString</code> such as <code>B[a@]dB[o0]t</code>. AWS WAF will match this <code>RegexPatternString</code> to:</p> <ul> <li> <p>BadBot</p> </li> <li> <p>BadB0t</p> </li> <li> <p>B@dBot</p> </li> <li> <p>B@dB0t</p> </li> </ul> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexPatternSet.</code> For more information, see <a>CreateRegexPatternSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexPatternSet</code> request to specify the regular expression pattern that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexPatternSet :: forall eff. UpdateRegexPatternSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateRegexPatternSetResponse
updateRegexPatternSet = AWS.request serviceName "updateRegexPatternSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a <code>Rule</code>. Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose you add the following to a <code>Rule</code>: </p> <ul> <li> <p>A <code>ByteMatchSet</code> that matches the value <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44</code> </p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the <code>Rule</code>. For a request to be blocked, the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code> <i>and</i> the request must originate from the IP address 192.0.2.44.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create the <code>Rule</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to add predicates to the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>ByteMatchSet</code> or <code>IPSet</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRule :: forall eff. UpdateRuleRequest -> Aff (err :: AWS.RequestError | eff) UpdateRuleResponse
updateRule = AWS.request serviceName "updateRule" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>RuleGroup</code>.</p> <p>You can only insert <code>REGULAR</code> rules into a rule group.</p> <p>You can have a maximum of ten rules per rule group.</p> <p>To create and configure a <code>RuleGroup</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>RuleGroup</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRuleGroup</a> request.</p> </li> <li> <p>Submit an <code>UpdateRuleGroup</code> request to add <code>Rules</code> to the <code>RuleGroup</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RuleGroup</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>Rule</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRuleGroup :: forall eff. UpdateRuleGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateRuleGroupResponse
updateRuleGroup = AWS.request serviceName "updateRuleGroup" 


-- | <p>Inserts or deletes <a>SizeConstraint</a> objects (filters) in a <a>SizeConstraintSet</a>. For each <code>SizeConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>SizeConstraintSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the <code>User-Agent</code> header.</p> </li> <li> <p>Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first <code>8192</code> bytes of your request to AWS WAF.</p> </li> <li> <p>A <code>ComparisonOperator</code> used for evaluating the selected part of the request against the specified <code>Size</code>, such as equals, greater than, less than, and so on.</p> </li> <li> <p>The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.</p> </li> </ul> <p>For example, you can add a <code>SizeConstraintSetUpdate</code> object that matches web requests in which the length of the <code>User-Agent</code> header is greater than 100 bytes. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>SizeConstraintSet.</code> For more information, see <a>CreateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateSizeConstraintSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSizeConstraintSet :: forall eff. UpdateSizeConstraintSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateSizeConstraintSetResponse
updateSizeConstraintSet = AWS.request serviceName "updateSizeConstraintSet" 


-- | <p>Inserts or deletes <a>SqlInjectionMatchTuple</a> objects (filters) in a <a>SqlInjectionMatchSet</a>. For each <code>SqlInjectionMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>SqlInjectionMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.</p> </li> </ul> <p>You use <code>SqlInjectionMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a <code>SqlInjectionMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateSqlInjectionMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSqlInjectionMatchSet :: forall eff. UpdateSqlInjectionMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateSqlInjectionMatchSetResponse
updateSqlInjectionMatchSet = AWS.request serviceName "updateSqlInjectionMatchSet" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>WebACL</code>. Each <code>Rule</code> identifies web requests that you want to allow, block, or count. When you update a <code>WebACL</code>, you specify the following values:</p> <ul> <li> <p>A default action for the <code>WebACL</code>, either <code>ALLOW</code> or <code>BLOCK</code>. AWS WAF performs the default action if a request doesn't match the criteria in any of the <code>Rules</code> in a <code>WebACL</code>.</p> </li> <li> <p>The <code>Rules</code> that you want to add and/or delete. If you want to replace one <code>Rule</code> with another, you delete the existing <code>Rule</code> and add the new one.</p> </li> <li> <p>For each <code>Rule</code>, whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the <code>Rule</code>.</p> </li> <li> <p>The order in which you want AWS WAF to evaluate the <code>Rules</code> in a <code>WebACL</code>. If you add more than one <code>Rule</code> to a <code>WebACL</code>, AWS WAF evaluates each request against the <code>Rules</code> in order based on the value of <code>Priority</code>. (The <code>Rule</code> that has the lowest value for <code>Priority</code> is evaluated first.) When a web request matches all of the predicates (such as <code>ByteMatchSets</code> and <code>IPSets</code>) in a <code>Rule</code>, AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining <code>Rules</code> in the <code>WebACL</code>, if any. </p> </li> </ul> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Create a <code>WebACL</code>. See <a>CreateWebACL</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <code>UpdateWebACL</code> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution. </p> </li> </ol> <p>Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the <a>UpdateWebACL</a> request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist. </p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateWebACL :: forall eff. UpdateWebACLRequest -> Aff (err :: AWS.RequestError | eff) UpdateWebACLResponse
updateWebACL = AWS.request serviceName "updateWebACL" 


-- | <p>Inserts or deletes <a>XssMatchTuple</a> objects (filters) in an <a>XssMatchSet</a>. For each <code>XssMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>XssMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.</p> </li> </ul> <p>You use <code>XssMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an <code>XssMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateXssMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateXssMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateXssMatchSet :: forall eff. UpdateXssMatchSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateXssMatchSetResponse
updateXssMatchSet = AWS.request serviceName "updateXssMatchSet" 


newtype Action = Action String
derive instance newtypeAction :: Newtype Action _


-- | <p>The <code>ActivatedRule</code> object in an <a>UpdateWebACL</a> request specifies a <code>Rule</code> that you want to insert or delete, the priority of the <code>Rule</code> in the <code>WebACL</code>, and the action that you want AWS WAF to take when a web request matches the <code>Rule</code> (<code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>).</p> <p>To specify whether to insert or delete a <code>Rule</code>, use the <code>Action</code> parameter in the <a>WebACLUpdate</a> data type.</p>
newtype ActivatedRule = ActivatedRule 
  { "Priority" :: (RulePriority)
  , "RuleId" :: (ResourceId)
  , "Action" :: NullOrUndefined (WafAction)
  , "OverrideAction" :: NullOrUndefined (WafOverrideAction)
  , "Type" :: NullOrUndefined (WafRuleType)
  }
derive instance newtypeActivatedRule :: Newtype ActivatedRule _


newtype ActivatedRules = ActivatedRules (Array ActivatedRule)
derive instance newtypeActivatedRules :: Newtype ActivatedRules _


newtype AssociateWebACLRequest = AssociateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeAssociateWebACLRequest :: Newtype AssociateWebACLRequest _


newtype AssociateWebACLResponse = AssociateWebACLResponse 
  { 
  }
derive instance newtypeAssociateWebACLResponse :: Newtype AssociateWebACLResponse _


-- | <p>In a <a>GetByteMatchSet</a> request, <code>ByteMatchSet</code> is a complex type that contains the <code>ByteMatchSetId</code> and <code>Name</code> of a <code>ByteMatchSet</code>, and the values that you specified when you updated the <code>ByteMatchSet</code>. </p> <p>A complex type that contains <code>ByteMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>ByteMatchSet</code> contains more than one <code>ByteMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype ByteMatchSet = ByteMatchSet 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "ByteMatchTuples" :: (ByteMatchTuples)
  }
derive instance newtypeByteMatchSet :: Newtype ByteMatchSet _


newtype ByteMatchSetSummaries = ByteMatchSetSummaries (Array ByteMatchSetSummary)
derive instance newtypeByteMatchSetSummaries :: Newtype ByteMatchSetSummaries _


-- | <p>Returned by <a>ListByteMatchSets</a>. Each <code>ByteMatchSetSummary</code> object includes the <code>Name</code> and <code>ByteMatchSetId</code> for one <a>ByteMatchSet</a>.</p>
newtype ByteMatchSetSummary = ByteMatchSetSummary 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeByteMatchSetSummary :: Newtype ByteMatchSetSummary _


-- | <p>In an <a>UpdateByteMatchSet</a> request, <code>ByteMatchSetUpdate</code> specifies whether to insert or delete a <a>ByteMatchTuple</a> and includes the settings for the <code>ByteMatchTuple</code>.</p>
newtype ByteMatchSetUpdate = ByteMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "ByteMatchTuple" :: (ByteMatchTuple)
  }
derive instance newtypeByteMatchSetUpdate :: Newtype ByteMatchSetUpdate _


newtype ByteMatchSetUpdates = ByteMatchSetUpdates (Array ByteMatchSetUpdate)
derive instance newtypeByteMatchSetUpdates :: Newtype ByteMatchSetUpdates _


newtype ByteMatchTargetString = ByteMatchTargetString String
derive instance newtypeByteMatchTargetString :: Newtype ByteMatchTargetString _


-- | <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.</p>
newtype ByteMatchTuple = ByteMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TargetString" :: (ByteMatchTargetString)
  , "TextTransformation" :: (TextTransformation)
  , "PositionalConstraint" :: (PositionalConstraint)
  }
derive instance newtypeByteMatchTuple :: Newtype ByteMatchTuple _


newtype ByteMatchTuples = ByteMatchTuples (Array ByteMatchTuple)
derive instance newtypeByteMatchTuples :: Newtype ByteMatchTuples _


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _


newtype ChangeToken = ChangeToken String
derive instance newtypeChangeToken :: Newtype ChangeToken _


newtype ChangeTokenStatus = ChangeTokenStatus String
derive instance newtypeChangeTokenStatus :: Newtype ChangeTokenStatus _


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _


newtype Country = Country String
derive instance newtypeCountry :: Newtype Country _


newtype CreateByteMatchSetRequest = CreateByteMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateByteMatchSetRequest :: Newtype CreateByteMatchSetRequest _


newtype CreateByteMatchSetResponse = CreateByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined (ByteMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateByteMatchSetResponse :: Newtype CreateByteMatchSetResponse _


newtype CreateGeoMatchSetRequest = CreateGeoMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateGeoMatchSetRequest :: Newtype CreateGeoMatchSetRequest _


newtype CreateGeoMatchSetResponse = CreateGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined (GeoMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateGeoMatchSetResponse :: Newtype CreateGeoMatchSetResponse _


newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateIPSetRequest :: Newtype CreateIPSetRequest _


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IPSet" :: NullOrUndefined (IPSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateIPSetResponse :: Newtype CreateIPSetResponse _


newtype CreateRateBasedRuleRequest = CreateRateBasedRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRateBasedRuleRequest :: Newtype CreateRateBasedRuleRequest _


newtype CreateRateBasedRuleResponse = CreateRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined (RateBasedRule)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRateBasedRuleResponse :: Newtype CreateRateBasedRuleResponse _


newtype CreateRegexMatchSetRequest = CreateRegexMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRegexMatchSetRequest :: Newtype CreateRegexMatchSetRequest _


newtype CreateRegexMatchSetResponse = CreateRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined (RegexMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRegexMatchSetResponse :: Newtype CreateRegexMatchSetResponse _


newtype CreateRegexPatternSetRequest = CreateRegexPatternSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRegexPatternSetRequest :: Newtype CreateRegexPatternSetRequest _


newtype CreateRegexPatternSetResponse = CreateRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined (RegexPatternSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRegexPatternSetResponse :: Newtype CreateRegexPatternSetResponse _


newtype CreateRuleGroupRequest = CreateRuleGroupRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRuleGroupRequest :: Newtype CreateRuleGroupRequest _


newtype CreateRuleGroupResponse = CreateRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined (RuleGroup)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRuleGroupResponse :: Newtype CreateRuleGroupResponse _


newtype CreateRuleRequest = CreateRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRuleRequest :: Newtype CreateRuleRequest _


newtype CreateRuleResponse = CreateRuleResponse 
  { "Rule" :: NullOrUndefined (Rule)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRuleResponse :: Newtype CreateRuleResponse _


newtype CreateSizeConstraintSetRequest = CreateSizeConstraintSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateSizeConstraintSetRequest :: Newtype CreateSizeConstraintSetRequest _


newtype CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined (SizeConstraintSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateSizeConstraintSetResponse :: Newtype CreateSizeConstraintSetResponse _


-- | <p>A request to create a <a>SqlInjectionMatchSet</a>.</p>
newtype CreateSqlInjectionMatchSetRequest = CreateSqlInjectionMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateSqlInjectionMatchSetRequest :: Newtype CreateSqlInjectionMatchSetRequest _


-- | <p>The response to a <code>CreateSqlInjectionMatchSet</code> request.</p>
newtype CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined (SqlInjectionMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateSqlInjectionMatchSetResponse :: Newtype CreateSqlInjectionMatchSetResponse _


newtype CreateWebACLRequest = CreateWebACLRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "DefaultAction" :: (WafAction)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateWebACLRequest :: Newtype CreateWebACLRequest _


newtype CreateWebACLResponse = CreateWebACLResponse 
  { "WebACL" :: NullOrUndefined (WebACL)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateWebACLResponse :: Newtype CreateWebACLResponse _


-- | <p>A request to create an <a>XssMatchSet</a>.</p>
newtype CreateXssMatchSetRequest = CreateXssMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateXssMatchSetRequest :: Newtype CreateXssMatchSetRequest _


-- | <p>The response to a <code>CreateXssMatchSet</code> request.</p>
newtype CreateXssMatchSetResponse = CreateXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined (XssMatchSet)
  , "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateXssMatchSetResponse :: Newtype CreateXssMatchSetResponse _


newtype DeleteByteMatchSetRequest = DeleteByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteByteMatchSetRequest :: Newtype DeleteByteMatchSetRequest _


newtype DeleteByteMatchSetResponse = DeleteByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteByteMatchSetResponse :: Newtype DeleteByteMatchSetResponse _


newtype DeleteGeoMatchSetRequest = DeleteGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteGeoMatchSetRequest :: Newtype DeleteGeoMatchSetRequest _


newtype DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteGeoMatchSetResponse :: Newtype DeleteGeoMatchSetResponse _


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteIPSetRequest :: Newtype DeleteIPSetRequest _


newtype DeleteIPSetResponse = DeleteIPSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteIPSetResponse :: Newtype DeleteIPSetResponse _


newtype DeletePermissionPolicyRequest = DeletePermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeDeletePermissionPolicyRequest :: Newtype DeletePermissionPolicyRequest _


newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse 
  { 
  }
derive instance newtypeDeletePermissionPolicyResponse :: Newtype DeletePermissionPolicyResponse _


newtype DeleteRateBasedRuleRequest = DeleteRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRateBasedRuleRequest :: Newtype DeleteRateBasedRuleRequest _


newtype DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRateBasedRuleResponse :: Newtype DeleteRateBasedRuleResponse _


newtype DeleteRegexMatchSetRequest = DeleteRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRegexMatchSetRequest :: Newtype DeleteRegexMatchSetRequest _


newtype DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRegexMatchSetResponse :: Newtype DeleteRegexMatchSetResponse _


newtype DeleteRegexPatternSetRequest = DeleteRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRegexPatternSetRequest :: Newtype DeleteRegexPatternSetRequest _


newtype DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRegexPatternSetResponse :: Newtype DeleteRegexPatternSetResponse _


newtype DeleteRuleGroupRequest = DeleteRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRuleGroupRequest :: Newtype DeleteRuleGroupRequest _


newtype DeleteRuleGroupResponse = DeleteRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRuleGroupResponse :: Newtype DeleteRuleGroupResponse _


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRuleRequest :: Newtype DeleteRuleRequest _


newtype DeleteRuleResponse = DeleteRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRuleResponse :: Newtype DeleteRuleResponse _


newtype DeleteSizeConstraintSetRequest = DeleteSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteSizeConstraintSetRequest :: Newtype DeleteSizeConstraintSetRequest _


newtype DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteSizeConstraintSetResponse :: Newtype DeleteSizeConstraintSetResponse _


-- | <p>A request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetRequest = DeleteSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteSqlInjectionMatchSetRequest :: Newtype DeleteSqlInjectionMatchSetRequest _


-- | <p>The response to a request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteSqlInjectionMatchSetResponse :: Newtype DeleteSqlInjectionMatchSetResponse _


newtype DeleteWebACLRequest = DeleteWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteWebACLRequest :: Newtype DeleteWebACLRequest _


newtype DeleteWebACLResponse = DeleteWebACLResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteWebACLResponse :: Newtype DeleteWebACLResponse _


-- | <p>A request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetRequest = DeleteXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteXssMatchSetRequest :: Newtype DeleteXssMatchSetRequest _


-- | <p>The response to a request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetResponse = DeleteXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteXssMatchSetResponse :: Newtype DeleteXssMatchSetResponse _


newtype DisassociateWebACLRequest = DisassociateWebACLRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeDisassociateWebACLRequest :: Newtype DisassociateWebACLRequest _


newtype DisassociateWebACLResponse = DisassociateWebACLResponse 
  { 
  }
derive instance newtypeDisassociateWebACLResponse :: Newtype DisassociateWebACLResponse _


-- | <p>Specifies where in a web request to look for <code>TargetString</code>.</p>
newtype FieldToMatch = FieldToMatch 
  { "Type" :: (MatchFieldType)
  , "Data" :: NullOrUndefined (MatchFieldData)
  }
derive instance newtypeFieldToMatch :: Newtype FieldToMatch _


-- | <p>The country from which web requests originate that you want AWS WAF to search for.</p>
newtype GeoMatchConstraint = GeoMatchConstraint 
  { "Type" :: (GeoMatchConstraintType)
  , "Value" :: (GeoMatchConstraintValue)
  }
derive instance newtypeGeoMatchConstraint :: Newtype GeoMatchConstraint _


newtype GeoMatchConstraintType = GeoMatchConstraintType String
derive instance newtypeGeoMatchConstraintType :: Newtype GeoMatchConstraintType _


newtype GeoMatchConstraintValue = GeoMatchConstraintValue String
derive instance newtypeGeoMatchConstraintValue :: Newtype GeoMatchConstraintValue _


newtype GeoMatchConstraints = GeoMatchConstraints (Array GeoMatchConstraint)
derive instance newtypeGeoMatchConstraints :: Newtype GeoMatchConstraints _


-- | <p>Contains one or more countries that AWS WAF will search for.</p>
newtype GeoMatchSet = GeoMatchSet 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "GeoMatchConstraints" :: (GeoMatchConstraints)
  }
derive instance newtypeGeoMatchSet :: Newtype GeoMatchSet _


newtype GeoMatchSetSummaries = GeoMatchSetSummaries (Array GeoMatchSetSummary)
derive instance newtypeGeoMatchSetSummaries :: Newtype GeoMatchSetSummaries _


-- | <p>Contains the identifier and the name of the <code>GeoMatchSet</code>.</p>
newtype GeoMatchSetSummary = GeoMatchSetSummary 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeGeoMatchSetSummary :: Newtype GeoMatchSetSummary _


-- | <p>Specifies the type of update to perform to an <a>GeoMatchSet</a> with <a>UpdateGeoMatchSet</a>.</p>
newtype GeoMatchSetUpdate = GeoMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "GeoMatchConstraint" :: (GeoMatchConstraint)
  }
derive instance newtypeGeoMatchSetUpdate :: Newtype GeoMatchSetUpdate _


newtype GeoMatchSetUpdates = GeoMatchSetUpdates (Array GeoMatchSetUpdate)
derive instance newtypeGeoMatchSetUpdates :: Newtype GeoMatchSetUpdates _


newtype GetByteMatchSetRequest = GetByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetByteMatchSetRequest :: Newtype GetByteMatchSetRequest _


newtype GetByteMatchSetResponse = GetByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined (ByteMatchSet)
  }
derive instance newtypeGetByteMatchSetResponse :: Newtype GetByteMatchSetResponse _


newtype GetChangeTokenRequest = GetChangeTokenRequest 
  { 
  }
derive instance newtypeGetChangeTokenRequest :: Newtype GetChangeTokenRequest _


newtype GetChangeTokenResponse = GetChangeTokenResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeGetChangeTokenResponse :: Newtype GetChangeTokenResponse _


newtype GetChangeTokenStatusRequest = GetChangeTokenStatusRequest 
  { "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeGetChangeTokenStatusRequest :: Newtype GetChangeTokenStatusRequest _


newtype GetChangeTokenStatusResponse = GetChangeTokenStatusResponse 
  { "ChangeTokenStatus" :: NullOrUndefined (ChangeTokenStatus)
  }
derive instance newtypeGetChangeTokenStatusResponse :: Newtype GetChangeTokenStatusResponse _


newtype GetGeoMatchSetRequest = GetGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetGeoMatchSetRequest :: Newtype GetGeoMatchSetRequest _


newtype GetGeoMatchSetResponse = GetGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined (GeoMatchSet)
  }
derive instance newtypeGetGeoMatchSetResponse :: Newtype GetGeoMatchSetResponse _


newtype GetIPSetRequest = GetIPSetRequest 
  { "IPSetId" :: (ResourceId)
  }
derive instance newtypeGetIPSetRequest :: Newtype GetIPSetRequest _


newtype GetIPSetResponse = GetIPSetResponse 
  { "IPSet" :: NullOrUndefined (IPSet)
  }
derive instance newtypeGetIPSetResponse :: Newtype GetIPSetResponse _


newtype GetPermissionPolicyRequest = GetPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeGetPermissionPolicyRequest :: Newtype GetPermissionPolicyRequest _


newtype GetPermissionPolicyResponse = GetPermissionPolicyResponse 
  { "Policy" :: NullOrUndefined (PolicyString)
  }
derive instance newtypeGetPermissionPolicyResponse :: Newtype GetPermissionPolicyResponse _


newtype GetRateBasedRuleManagedKeysRequest = GetRateBasedRuleManagedKeysRequest 
  { "RuleId" :: (ResourceId)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  }
derive instance newtypeGetRateBasedRuleManagedKeysRequest :: Newtype GetRateBasedRuleManagedKeysRequest _


newtype GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse 
  { "ManagedKeys" :: NullOrUndefined (ManagedKeys)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  }
derive instance newtypeGetRateBasedRuleManagedKeysResponse :: Newtype GetRateBasedRuleManagedKeysResponse _


newtype GetRateBasedRuleRequest = GetRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  }
derive instance newtypeGetRateBasedRuleRequest :: Newtype GetRateBasedRuleRequest _


newtype GetRateBasedRuleResponse = GetRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined (RateBasedRule)
  }
derive instance newtypeGetRateBasedRuleResponse :: Newtype GetRateBasedRuleResponse _


newtype GetRegexMatchSetRequest = GetRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetRegexMatchSetRequest :: Newtype GetRegexMatchSetRequest _


newtype GetRegexMatchSetResponse = GetRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined (RegexMatchSet)
  }
derive instance newtypeGetRegexMatchSetResponse :: Newtype GetRegexMatchSetResponse _


newtype GetRegexPatternSetRequest = GetRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  }
derive instance newtypeGetRegexPatternSetRequest :: Newtype GetRegexPatternSetRequest _


newtype GetRegexPatternSetResponse = GetRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined (RegexPatternSet)
  }
derive instance newtypeGetRegexPatternSetResponse :: Newtype GetRegexPatternSetResponse _


newtype GetRuleGroupRequest = GetRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  }
derive instance newtypeGetRuleGroupRequest :: Newtype GetRuleGroupRequest _


newtype GetRuleGroupResponse = GetRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined (RuleGroup)
  }
derive instance newtypeGetRuleGroupResponse :: Newtype GetRuleGroupResponse _


newtype GetRuleRequest = GetRuleRequest 
  { "RuleId" :: (ResourceId)
  }
derive instance newtypeGetRuleRequest :: Newtype GetRuleRequest _


newtype GetRuleResponse = GetRuleResponse 
  { "Rule" :: NullOrUndefined (Rule)
  }
derive instance newtypeGetRuleResponse :: Newtype GetRuleResponse _


newtype GetSampledRequestsMaxItems = GetSampledRequestsMaxItems Number
derive instance newtypeGetSampledRequestsMaxItems :: Newtype GetSampledRequestsMaxItems _


newtype GetSampledRequestsRequest = GetSampledRequestsRequest 
  { "WebAclId" :: (ResourceId)
  , "RuleId" :: (ResourceId)
  , "TimeWindow" :: (TimeWindow)
  , "MaxItems" :: (GetSampledRequestsMaxItems)
  }
derive instance newtypeGetSampledRequestsRequest :: Newtype GetSampledRequestsRequest _


newtype GetSampledRequestsResponse = GetSampledRequestsResponse 
  { "SampledRequests" :: NullOrUndefined (SampledHTTPRequests)
  , "PopulationSize" :: NullOrUndefined (PopulationSize)
  , "TimeWindow" :: NullOrUndefined (TimeWindow)
  }
derive instance newtypeGetSampledRequestsResponse :: Newtype GetSampledRequestsResponse _


newtype GetSizeConstraintSetRequest = GetSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  }
derive instance newtypeGetSizeConstraintSetRequest :: Newtype GetSizeConstraintSetRequest _


newtype GetSizeConstraintSetResponse = GetSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined (SizeConstraintSet)
  }
derive instance newtypeGetSizeConstraintSetResponse :: Newtype GetSizeConstraintSetResponse _


-- | <p>A request to get a <a>SqlInjectionMatchSet</a>.</p>
newtype GetSqlInjectionMatchSetRequest = GetSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetSqlInjectionMatchSetRequest :: Newtype GetSqlInjectionMatchSetRequest _


-- | <p>The response to a <a>GetSqlInjectionMatchSet</a> request.</p>
newtype GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined (SqlInjectionMatchSet)
  }
derive instance newtypeGetSqlInjectionMatchSetResponse :: Newtype GetSqlInjectionMatchSetResponse _


newtype GetWebACLForResourceRequest = GetWebACLForResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeGetWebACLForResourceRequest :: Newtype GetWebACLForResourceRequest _


newtype GetWebACLForResourceResponse = GetWebACLForResourceResponse 
  { "WebACLSummary" :: NullOrUndefined (WebACLSummary)
  }
derive instance newtypeGetWebACLForResourceResponse :: Newtype GetWebACLForResourceResponse _


newtype GetWebACLRequest = GetWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }
derive instance newtypeGetWebACLRequest :: Newtype GetWebACLRequest _


newtype GetWebACLResponse = GetWebACLResponse 
  { "WebACL" :: NullOrUndefined (WebACL)
  }
derive instance newtypeGetWebACLResponse :: Newtype GetWebACLResponse _


-- | <p>A request to get an <a>XssMatchSet</a>.</p>
newtype GetXssMatchSetRequest = GetXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetXssMatchSetRequest :: Newtype GetXssMatchSetRequest _


-- | <p>The response to a <a>GetXssMatchSet</a> request.</p>
newtype GetXssMatchSetResponse = GetXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined (XssMatchSet)
  }
derive instance newtypeGetXssMatchSetResponse :: Newtype GetXssMatchSetResponse _


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPHeader</code> complex type that appears as <code>Headers</code> in the response syntax. <code>HTTPHeader</code> contains the names and values of all of the headers that appear in one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPHeader = HTTPHeader 
  { "Name" :: NullOrUndefined (HeaderName)
  , "Value" :: NullOrUndefined (HeaderValue)
  }
derive instance newtypeHTTPHeader :: Newtype HTTPHeader _


newtype HTTPHeaders = HTTPHeaders (Array HTTPHeader)
derive instance newtypeHTTPHeaders :: Newtype HTTPHeaders _


newtype HTTPMethod = HTTPMethod String
derive instance newtypeHTTPMethod :: Newtype HTTPMethod _


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPRequest</code> complex type that appears as <code>Request</code> in the response syntax. <code>HTTPRequest</code> contains information about one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPRequest = HTTPRequest 
  { "ClientIP" :: NullOrUndefined (IPString)
  , "Country" :: NullOrUndefined (Country)
  , "URI" :: NullOrUndefined (URIString)
  , "Method" :: NullOrUndefined (HTTPMethod)
  , "HTTPVersion" :: NullOrUndefined (HTTPVersion)
  , "Headers" :: NullOrUndefined (HTTPHeaders)
  }
derive instance newtypeHTTPRequest :: Newtype HTTPRequest _


newtype HTTPVersion = HTTPVersion String
derive instance newtypeHTTPVersion :: Newtype HTTPVersion _


newtype HeaderName = HeaderName String
derive instance newtypeHeaderName :: Newtype HeaderName _


newtype HeaderValue = HeaderValue String
derive instance newtypeHeaderValue :: Newtype HeaderValue _


-- | <p>Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6.</p> <p>To specify an individual IP address, you specify the four-part IP address followed by a <code>/32</code>, for example, 192.0.2.0/31. To block a range of IP addresses, you can specify a <code>/128</code>, <code>/64</code>, <code>/56</code>, <code>/48</code>, <code>/32</code>, <code>/24</code>, <code>/16</code>, or <code>/8</code> CIDR. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p>
newtype IPSet = IPSet 
  { "IPSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "IPSetDescriptors" :: (IPSetDescriptors)
  }
derive instance newtypeIPSet :: Newtype IPSet _


-- | <p>Specifies the IP address type (<code>IPV4</code> or <code>IPV6</code>) and the IP address range (in CIDR format) that web requests originate from.</p>
newtype IPSetDescriptor = IPSetDescriptor 
  { "Type" :: (IPSetDescriptorType)
  , "Value" :: (IPSetDescriptorValue)
  }
derive instance newtypeIPSetDescriptor :: Newtype IPSetDescriptor _


newtype IPSetDescriptorType = IPSetDescriptorType String
derive instance newtypeIPSetDescriptorType :: Newtype IPSetDescriptorType _


newtype IPSetDescriptorValue = IPSetDescriptorValue String
derive instance newtypeIPSetDescriptorValue :: Newtype IPSetDescriptorValue _


newtype IPSetDescriptors = IPSetDescriptors (Array IPSetDescriptor)
derive instance newtypeIPSetDescriptors :: Newtype IPSetDescriptors _


newtype IPSetSummaries = IPSetSummaries (Array IPSetSummary)
derive instance newtypeIPSetSummaries :: Newtype IPSetSummaries _


-- | <p>Contains the identifier and the name of the <code>IPSet</code>.</p>
newtype IPSetSummary = IPSetSummary 
  { "IPSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeIPSetSummary :: Newtype IPSetSummary _


-- | <p>Specifies the type of update to perform to an <a>IPSet</a> with <a>UpdateIPSet</a>.</p>
newtype IPSetUpdate = IPSetUpdate 
  { "Action" :: (ChangeAction)
  , "IPSetDescriptor" :: (IPSetDescriptor)
  }
derive instance newtypeIPSetUpdate :: Newtype IPSetUpdate _


newtype IPSetUpdates = IPSetUpdates (Array IPSetUpdate)
derive instance newtypeIPSetUpdates :: Newtype IPSetUpdates _


newtype IPString = IPString String
derive instance newtypeIPString :: Newtype IPString _


newtype ListActivatedRulesInRuleGroupRequest = ListActivatedRulesInRuleGroupRequest 
  { "RuleGroupId" :: NullOrUndefined (ResourceId)
  , "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListActivatedRulesInRuleGroupRequest :: Newtype ListActivatedRulesInRuleGroupRequest _


newtype ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "ActivatedRules" :: NullOrUndefined (ActivatedRules)
  }
derive instance newtypeListActivatedRulesInRuleGroupResponse :: Newtype ListActivatedRulesInRuleGroupResponse _


newtype ListByteMatchSetsRequest = ListByteMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListByteMatchSetsRequest :: Newtype ListByteMatchSetsRequest _


newtype ListByteMatchSetsResponse = ListByteMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "ByteMatchSets" :: NullOrUndefined (ByteMatchSetSummaries)
  }
derive instance newtypeListByteMatchSetsResponse :: Newtype ListByteMatchSetsResponse _


newtype ListGeoMatchSetsRequest = ListGeoMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListGeoMatchSetsRequest :: Newtype ListGeoMatchSetsRequest _


newtype ListGeoMatchSetsResponse = ListGeoMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "GeoMatchSets" :: NullOrUndefined (GeoMatchSetSummaries)
  }
derive instance newtypeListGeoMatchSetsResponse :: Newtype ListGeoMatchSetsResponse _


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListIPSetsRequest :: Newtype ListIPSetsRequest _


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "IPSets" :: NullOrUndefined (IPSetSummaries)
  }
derive instance newtypeListIPSetsResponse :: Newtype ListIPSetsResponse _


newtype ListRateBasedRulesRequest = ListRateBasedRulesRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRateBasedRulesRequest :: Newtype ListRateBasedRulesRequest _


newtype ListRateBasedRulesResponse = ListRateBasedRulesResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined (RuleSummaries)
  }
derive instance newtypeListRateBasedRulesResponse :: Newtype ListRateBasedRulesResponse _


newtype ListRegexMatchSetsRequest = ListRegexMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRegexMatchSetsRequest :: Newtype ListRegexMatchSetsRequest _


newtype ListRegexMatchSetsResponse = ListRegexMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RegexMatchSets" :: NullOrUndefined (RegexMatchSetSummaries)
  }
derive instance newtypeListRegexMatchSetsResponse :: Newtype ListRegexMatchSetsResponse _


newtype ListRegexPatternSetsRequest = ListRegexPatternSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRegexPatternSetsRequest :: Newtype ListRegexPatternSetsRequest _


newtype ListRegexPatternSetsResponse = ListRegexPatternSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RegexPatternSets" :: NullOrUndefined (RegexPatternSetSummaries)
  }
derive instance newtypeListRegexPatternSetsResponse :: Newtype ListRegexPatternSetsResponse _


newtype ListResourcesForWebACLRequest = ListResourcesForWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }
derive instance newtypeListResourcesForWebACLRequest :: Newtype ListResourcesForWebACLRequest _


newtype ListResourcesForWebACLResponse = ListResourcesForWebACLResponse 
  { "ResourceArns" :: NullOrUndefined (ResourceArns)
  }
derive instance newtypeListResourcesForWebACLResponse :: Newtype ListResourcesForWebACLResponse _


newtype ListRuleGroupsRequest = ListRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRuleGroupsRequest :: Newtype ListRuleGroupsRequest _


newtype ListRuleGroupsResponse = ListRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined (RuleGroupSummaries)
  }
derive instance newtypeListRuleGroupsResponse :: Newtype ListRuleGroupsResponse _


newtype ListRulesRequest = ListRulesRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRulesRequest :: Newtype ListRulesRequest _


newtype ListRulesResponse = ListRulesResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined (RuleSummaries)
  }
derive instance newtypeListRulesResponse :: Newtype ListRulesResponse _


newtype ListSizeConstraintSetsRequest = ListSizeConstraintSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSizeConstraintSetsRequest :: Newtype ListSizeConstraintSetsRequest _


newtype ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "SizeConstraintSets" :: NullOrUndefined (SizeConstraintSetSummaries)
  }
derive instance newtypeListSizeConstraintSetsResponse :: Newtype ListSizeConstraintSetsResponse _


-- | <p>A request to list the <a>SqlInjectionMatchSet</a> objects created by the current AWS account.</p>
newtype ListSqlInjectionMatchSetsRequest = ListSqlInjectionMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSqlInjectionMatchSetsRequest :: Newtype ListSqlInjectionMatchSetsRequest _


-- | <p>The response to a <a>ListSqlInjectionMatchSets</a> request.</p>
newtype ListSqlInjectionMatchSetsResponse = ListSqlInjectionMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "SqlInjectionMatchSets" :: NullOrUndefined (SqlInjectionMatchSetSummaries)
  }
derive instance newtypeListSqlInjectionMatchSetsResponse :: Newtype ListSqlInjectionMatchSetsResponse _


newtype ListSubscribedRuleGroupsRequest = ListSubscribedRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSubscribedRuleGroupsRequest :: Newtype ListSubscribedRuleGroupsRequest _


newtype ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined (SubscribedRuleGroupSummaries)
  }
derive instance newtypeListSubscribedRuleGroupsResponse :: Newtype ListSubscribedRuleGroupsResponse _


newtype ListWebACLsRequest = ListWebACLsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListWebACLsRequest :: Newtype ListWebACLsRequest _


newtype ListWebACLsResponse = ListWebACLsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "WebACLs" :: NullOrUndefined (WebACLSummaries)
  }
derive instance newtypeListWebACLsResponse :: Newtype ListWebACLsResponse _


-- | <p>A request to list the <a>XssMatchSet</a> objects created by the current AWS account.</p>
newtype ListXssMatchSetsRequest = ListXssMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListXssMatchSetsRequest :: Newtype ListXssMatchSetsRequest _


-- | <p>The response to a <a>ListXssMatchSets</a> request.</p>
newtype ListXssMatchSetsResponse = ListXssMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined (NextMarker)
  , "XssMatchSets" :: NullOrUndefined (XssMatchSetSummaries)
  }
derive instance newtypeListXssMatchSetsResponse :: Newtype ListXssMatchSetsResponse _


newtype ManagedKey = ManagedKey String
derive instance newtypeManagedKey :: Newtype ManagedKey _


newtype ManagedKeys = ManagedKeys (Array ManagedKey)
derive instance newtypeManagedKeys :: Newtype ManagedKeys _


newtype MatchFieldData = MatchFieldData String
derive instance newtypeMatchFieldData :: Newtype MatchFieldData _


newtype MatchFieldType = MatchFieldType String
derive instance newtypeMatchFieldType :: Newtype MatchFieldType _


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _


newtype Negated = Negated Boolean
derive instance newtypeNegated :: Newtype Negated _


newtype NextMarker = NextMarker String
derive instance newtypeNextMarker :: Newtype NextMarker _


newtype PaginationLimit = PaginationLimit Int
derive instance newtypePaginationLimit :: Newtype PaginationLimit _


newtype ParameterExceptionField = ParameterExceptionField String
derive instance newtypeParameterExceptionField :: Newtype ParameterExceptionField _


newtype ParameterExceptionParameter = ParameterExceptionParameter String
derive instance newtypeParameterExceptionParameter :: Newtype ParameterExceptionParameter _


newtype ParameterExceptionReason = ParameterExceptionReason String
derive instance newtypeParameterExceptionReason :: Newtype ParameterExceptionReason _


newtype PolicyString = PolicyString String
derive instance newtypePolicyString :: Newtype PolicyString _


newtype PopulationSize = PopulationSize Number
derive instance newtypePopulationSize :: Newtype PopulationSize _


newtype PositionalConstraint = PositionalConstraint String
derive instance newtypePositionalConstraint :: Newtype PositionalConstraint _


-- | <p>Specifies the <a>ByteMatchSet</a>, <a>IPSet</a>, <a>SqlInjectionMatchSet</a>, <a>XssMatchSet</a>, <a>RegexMatchSet</a>, <a>GeoMatchSet</a>, and <a>SizeConstraintSet</a> objects that you want to add to a <code>Rule</code> and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44. </p>
newtype Predicate = Predicate 
  { "Negated" :: (Negated)
  , "Type" :: (PredicateType)
  , "DataId" :: (ResourceId)
  }
derive instance newtypePredicate :: Newtype Predicate _


newtype PredicateType = PredicateType String
derive instance newtypePredicateType :: Newtype PredicateType _


newtype Predicates = Predicates (Array Predicate)
derive instance newtypePredicates :: Newtype Predicates _


newtype PutPermissionPolicyRequest = PutPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  , "Policy" :: (PolicyString)
  }
derive instance newtypePutPermissionPolicyRequest :: Newtype PutPermissionPolicyRequest _


newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse 
  { 
  }
derive instance newtypePutPermissionPolicyResponse :: Newtype PutPermissionPolicyResponse _


-- | <p>A <code>RateBasedRule</code> is identical to a regular <a>Rule</a>, with one addition: a <code>RateBasedRule</code> counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a <code>RateBasedRule</code> that includes the following conditions: </p> <ul> <li> <p>The requests come from 192.0.2.44.</p> </li> <li> <p>They contain the value <code>BadBot</code> in the <code>User-Agent</code> header.</p> </li> </ul> <p>In the rule, you also define the rate limit as 15,000.</p> <p>Requests that meet both of these conditions and exceed 15,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.</p>
newtype RateBasedRule = RateBasedRule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "MatchPredicates" :: (Predicates)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  }
derive instance newtypeRateBasedRule :: Newtype RateBasedRule _


newtype RateKey = RateKey String
derive instance newtypeRateKey :: Newtype RateKey _


newtype RateLimit = RateLimit Number
derive instance newtypeRateLimit :: Newtype RateLimit _


-- | <p>In a <a>GetRegexMatchSet</a> request, <code>RegexMatchSet</code> is a complex type that contains the <code>RegexMatchSetId</code> and <code>Name</code> of a <code>RegexMatchSet</code>, and the values that you specified when you updated the <code>RegexMatchSet</code>.</p> <p> The values are contained in a <code>RegexMatchTuple</code> object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>RegexMatchSet</code> contains more than one <code>RegexMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype RegexMatchSet = RegexMatchSet 
  { "RegexMatchSetId" :: NullOrUndefined (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "RegexMatchTuples" :: NullOrUndefined (RegexMatchTuples)
  }
derive instance newtypeRegexMatchSet :: Newtype RegexMatchSet _


newtype RegexMatchSetSummaries = RegexMatchSetSummaries (Array RegexMatchSetSummary)
derive instance newtypeRegexMatchSetSummaries :: Newtype RegexMatchSetSummaries _


-- | <p>Returned by <a>ListRegexMatchSets</a>. Each <code>RegexMatchSetSummary</code> object includes the <code>Name</code> and <code>RegexMatchSetId</code> for one <a>RegexMatchSet</a>.</p>
newtype RegexMatchSetSummary = RegexMatchSetSummary 
  { "RegexMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRegexMatchSetSummary :: Newtype RegexMatchSetSummary _


-- | <p>In an <a>UpdateRegexMatchSet</a> request, <code>RegexMatchSetUpdate</code> specifies whether to insert or delete a <a>RegexMatchTuple</a> and includes the settings for the <code>RegexMatchTuple</code>.</p>
newtype RegexMatchSetUpdate = RegexMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexMatchTuple" :: (RegexMatchTuple)
  }
derive instance newtypeRegexMatchSetUpdate :: Newtype RegexMatchSetUpdate _


newtype RegexMatchSetUpdates = RegexMatchSetUpdates (Array RegexMatchSetUpdate)
derive instance newtypeRegexMatchSetUpdates :: Newtype RegexMatchSetUpdates _


-- | <p>The regular expression pattern that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings. Each <code>RegexMatchTuple</code> object contains: </p> <ul> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul>
newtype RegexMatchTuple = RegexMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "RegexPatternSetId" :: (ResourceId)
  }
derive instance newtypeRegexMatchTuple :: Newtype RegexMatchTuple _


newtype RegexMatchTuples = RegexMatchTuples (Array RegexMatchTuple)
derive instance newtypeRegexMatchTuples :: Newtype RegexMatchTuples _


-- | <p>The <code>RegexPatternSet</code> specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p>
newtype RegexPatternSet = RegexPatternSet 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "RegexPatternStrings" :: (RegexPatternStrings)
  }
derive instance newtypeRegexPatternSet :: Newtype RegexPatternSet _


newtype RegexPatternSetSummaries = RegexPatternSetSummaries (Array RegexPatternSetSummary)
derive instance newtypeRegexPatternSetSummaries :: Newtype RegexPatternSetSummaries _


-- | <p>Returned by <a>ListRegexPatternSets</a>. Each <code>RegexPatternSetSummary</code> object includes the <code>Name</code> and <code>RegexPatternSetId</code> for one <a>RegexPatternSet</a>.</p>
newtype RegexPatternSetSummary = RegexPatternSetSummary 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRegexPatternSetSummary :: Newtype RegexPatternSetSummary _


-- | <p>In an <a>UpdateRegexPatternSet</a> request, <code>RegexPatternSetUpdate</code> specifies whether to insert or delete a <code>RegexPatternString</code> and includes the settings for the <code>RegexPatternString</code>.</p>
newtype RegexPatternSetUpdate = RegexPatternSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexPatternString" :: (RegexPatternString)
  }
derive instance newtypeRegexPatternSetUpdate :: Newtype RegexPatternSetUpdate _


newtype RegexPatternSetUpdates = RegexPatternSetUpdates (Array RegexPatternSetUpdate)
derive instance newtypeRegexPatternSetUpdates :: Newtype RegexPatternSetUpdates _


newtype RegexPatternString = RegexPatternString String
derive instance newtypeRegexPatternString :: Newtype RegexPatternString _


newtype RegexPatternStrings = RegexPatternStrings (Array RegexPatternString)
derive instance newtypeRegexPatternStrings :: Newtype RegexPatternStrings _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


newtype ResourceArns = ResourceArns (Array ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


-- | <p>A combination of <a>ByteMatchSet</a>, <a>IPSet</a>, and/or <a>SqlInjectionMatchSet</a> objects that identify the web requests that you want to allow, block, or count. For example, you might create a <code>Rule</code> that includes the following predicates:</p> <ul> <li> <p>An <code>IPSet</code> that causes AWS WAF to search for web requests that originate from the IP address <code>192.0.2.44</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that causes AWS WAF to search for web requests for which the value of the <code>User-Agent</code> header is <code>BadBot</code>.</p> </li> </ul> <p>To match the settings in this <code>Rule</code>, a request must originate from <code>192.0.2.44</code> AND include a <code>User-Agent</code> header for which the value is <code>BadBot</code>.</p>
newtype Rule = Rule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "Predicates" :: (Predicates)
  }
derive instance newtypeRule :: Newtype Rule _


-- | <p>A collection of predefined rules that you can add to a web ACL.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul>
newtype RuleGroup = RuleGroup 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  }
derive instance newtypeRuleGroup :: Newtype RuleGroup _


newtype RuleGroupSummaries = RuleGroupSummaries (Array RuleGroupSummary)
derive instance newtypeRuleGroupSummaries :: Newtype RuleGroupSummaries _


-- | <p>Contains the identifier and the friendly name or description of the <code>RuleGroup</code>.</p>
newtype RuleGroupSummary = RuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRuleGroupSummary :: Newtype RuleGroupSummary _


-- | <p>Specifies an <code>ActivatedRule</code> and indicates whether you want to add it to a <code>RuleGroup</code> or delete it from a <code>RuleGroup</code>.</p>
newtype RuleGroupUpdate = RuleGroupUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }
derive instance newtypeRuleGroupUpdate :: Newtype RuleGroupUpdate _


newtype RuleGroupUpdates = RuleGroupUpdates (Array RuleGroupUpdate)
derive instance newtypeRuleGroupUpdates :: Newtype RuleGroupUpdates _


newtype RulePriority = RulePriority Int
derive instance newtypeRulePriority :: Newtype RulePriority _


newtype RuleSummaries = RuleSummaries (Array RuleSummary)
derive instance newtypeRuleSummaries :: Newtype RuleSummaries _


-- | <p>Contains the identifier and the friendly name or description of the <code>Rule</code>.</p>
newtype RuleSummary = RuleSummary 
  { "RuleId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRuleSummary :: Newtype RuleSummary _


-- | <p>Specifies a <code>Predicate</code> (such as an <code>IPSet</code>) and indicates whether you want to add it to a <code>Rule</code> or delete it from a <code>Rule</code>.</p>
newtype RuleUpdate = RuleUpdate 
  { "Action" :: (ChangeAction)
  , "Predicate" :: (Predicate)
  }
derive instance newtypeRuleUpdate :: Newtype RuleUpdate _


newtype RuleUpdates = RuleUpdates (Array RuleUpdate)
derive instance newtypeRuleUpdates :: Newtype RuleUpdates _


newtype SampleWeight = SampleWeight Number
derive instance newtypeSampleWeight :: Newtype SampleWeight _


-- | <p>The response from a <a>GetSampledRequests</a> request includes a <code>SampledHTTPRequests</code> complex type that appears as <code>SampledRequests</code> in the response syntax. <code>SampledHTTPRequests</code> contains one <code>SampledHTTPRequest</code> object for each web request that is returned by <code>GetSampledRequests</code>.</p>
newtype SampledHTTPRequest = SampledHTTPRequest 
  { "Request" :: (HTTPRequest)
  , "Weight" :: (SampleWeight)
  , "Number" :: NullOrUndefined (Number)
  , "Action" :: NullOrUndefined (Action)
  , "RuleWithinRuleGroup" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeSampledHTTPRequest :: Newtype SampledHTTPRequest _


newtype SampledHTTPRequests = SampledHTTPRequests (Array SampledHTTPRequest)
derive instance newtypeSampledHTTPRequests :: Newtype SampledHTTPRequests _


newtype Size = Size Number
derive instance newtypeSize :: Newtype Size _


-- | <p>Specifies a constraint on the size of a part of the web request. AWS WAF uses the <code>Size</code>, <code>ComparisonOperator</code>, and <code>FieldToMatch</code> to build an expression in the form of "<code>Size</code> <code>ComparisonOperator</code> size in bytes of <code>FieldToMatch</code>". If that expression is true, the <code>SizeConstraint</code> is considered to match.</p>
newtype SizeConstraint = SizeConstraint 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Size" :: (Size)
  }
derive instance newtypeSizeConstraint :: Newtype SizeConstraint _


-- | <p>A complex type that contains <code>SizeConstraint</code> objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a <code>SizeConstraintSet</code> contains more than one <code>SizeConstraint</code> object, a request only needs to match one constraint to be considered a match.</p>
newtype SizeConstraintSet = SizeConstraintSet 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "SizeConstraints" :: (SizeConstraints)
  }
derive instance newtypeSizeConstraintSet :: Newtype SizeConstraintSet _


newtype SizeConstraintSetSummaries = SizeConstraintSetSummaries (Array SizeConstraintSetSummary)
derive instance newtypeSizeConstraintSetSummaries :: Newtype SizeConstraintSetSummaries _


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetSummary = SizeConstraintSetSummary 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeSizeConstraintSetSummary :: Newtype SizeConstraintSetSummary _


-- | <p>Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a <a>SizeConstraintSet</a> or delete it from a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetUpdate = SizeConstraintSetUpdate 
  { "Action" :: (ChangeAction)
  , "SizeConstraint" :: (SizeConstraint)
  }
derive instance newtypeSizeConstraintSetUpdate :: Newtype SizeConstraintSetUpdate _


newtype SizeConstraintSetUpdates = SizeConstraintSetUpdates (Array SizeConstraintSetUpdate)
derive instance newtypeSizeConstraintSetUpdates :: Newtype SizeConstraintSetUpdates _


newtype SizeConstraints = SizeConstraints (Array SizeConstraint)
derive instance newtypeSizeConstraints :: Newtype SizeConstraints _


-- | <p>A complex type that contains <code>SqlInjectionMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a <code>SqlInjectionMatchSet</code> contains more than one <code>SqlInjectionMatchTuple</code> object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.</p>
newtype SqlInjectionMatchSet = SqlInjectionMatchSet 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "SqlInjectionMatchTuples" :: (SqlInjectionMatchTuples)
  }
derive instance newtypeSqlInjectionMatchSet :: Newtype SqlInjectionMatchSet _


newtype SqlInjectionMatchSetSummaries = SqlInjectionMatchSetSummaries (Array SqlInjectionMatchSetSummary)
derive instance newtypeSqlInjectionMatchSetSummaries :: Newtype SqlInjectionMatchSetSummaries _


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeSqlInjectionMatchSetSummary :: Newtype SqlInjectionMatchSetSummary _


-- | <p>Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a <a>SqlInjectionMatchSet</a> or delete it from a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "SqlInjectionMatchTuple" :: (SqlInjectionMatchTuple)
  }
derive instance newtypeSqlInjectionMatchSetUpdate :: Newtype SqlInjectionMatchSetUpdate _


newtype SqlInjectionMatchSetUpdates = SqlInjectionMatchSetUpdates (Array SqlInjectionMatchSetUpdate)
derive instance newtypeSqlInjectionMatchSetUpdates :: Newtype SqlInjectionMatchSetUpdates _


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype SqlInjectionMatchTuple = SqlInjectionMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }
derive instance newtypeSqlInjectionMatchTuple :: Newtype SqlInjectionMatchTuple _


newtype SqlInjectionMatchTuples = SqlInjectionMatchTuples (Array SqlInjectionMatchTuple)
derive instance newtypeSqlInjectionMatchTuples :: Newtype SqlInjectionMatchTuples _


newtype SubscribedRuleGroupSummaries = SubscribedRuleGroupSummaries (Array SubscribedRuleGroupSummary)
derive instance newtypeSubscribedRuleGroupSummaries :: Newtype SubscribedRuleGroupSummaries _


-- | <p>A summary of the rule groups you are subscribed to.</p>
newtype SubscribedRuleGroupSummary = SubscribedRuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  }
derive instance newtypeSubscribedRuleGroupSummary :: Newtype SubscribedRuleGroupSummary _


newtype TextTransformation = TextTransformation String
derive instance newtypeTextTransformation :: Newtype TextTransformation _


-- | <p>In a <a>GetSampledRequests</a> request, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which you want AWS WAF to return a sample of web requests.</p> <p>In a <a>GetSampledRequests</a> response, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, <code>EndTime</code> is the time that AWS WAF received the 5,000th request. </p>
newtype TimeWindow = TimeWindow 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  }
derive instance newtypeTimeWindow :: Newtype TimeWindow _


newtype URIString = URIString String
derive instance newtypeURIString :: Newtype URIString _


newtype UpdateByteMatchSetRequest = UpdateByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (ByteMatchSetUpdates)
  }
derive instance newtypeUpdateByteMatchSetRequest :: Newtype UpdateByteMatchSetRequest _


newtype UpdateByteMatchSetResponse = UpdateByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateByteMatchSetResponse :: Newtype UpdateByteMatchSetResponse _


newtype UpdateGeoMatchSetRequest = UpdateGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (GeoMatchSetUpdates)
  }
derive instance newtypeUpdateGeoMatchSetRequest :: Newtype UpdateGeoMatchSetRequest _


newtype UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateGeoMatchSetResponse :: Newtype UpdateGeoMatchSetResponse _


newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (IPSetUpdates)
  }
derive instance newtypeUpdateIPSetRequest :: Newtype UpdateIPSetRequest _


newtype UpdateIPSetResponse = UpdateIPSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateIPSetResponse :: Newtype UpdateIPSetResponse _


newtype UpdateRateBasedRuleRequest = UpdateRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  , "RateLimit" :: (RateLimit)
  }
derive instance newtypeUpdateRateBasedRuleRequest :: Newtype UpdateRateBasedRuleRequest _


newtype UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRateBasedRuleResponse :: Newtype UpdateRateBasedRuleResponse _


newtype UpdateRegexMatchSetRequest = UpdateRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "Updates" :: (RegexMatchSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRegexMatchSetRequest :: Newtype UpdateRegexMatchSetRequest _


newtype UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRegexMatchSetResponse :: Newtype UpdateRegexMatchSetResponse _


newtype UpdateRegexPatternSetRequest = UpdateRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "Updates" :: (RegexPatternSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRegexPatternSetRequest :: Newtype UpdateRegexPatternSetRequest _


newtype UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRegexPatternSetResponse :: Newtype UpdateRegexPatternSetResponse _


newtype UpdateRuleGroupRequest = UpdateRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "Updates" :: (RuleGroupUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRuleGroupRequest :: Newtype UpdateRuleGroupRequest _


newtype UpdateRuleGroupResponse = UpdateRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRuleGroupResponse :: Newtype UpdateRuleGroupResponse _


newtype UpdateRuleRequest = UpdateRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  }
derive instance newtypeUpdateRuleRequest :: Newtype UpdateRuleRequest _


newtype UpdateRuleResponse = UpdateRuleResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRuleResponse :: Newtype UpdateRuleResponse _


newtype UpdateSizeConstraintSetRequest = UpdateSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SizeConstraintSetUpdates)
  }
derive instance newtypeUpdateSizeConstraintSetRequest :: Newtype UpdateSizeConstraintSetRequest _


newtype UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateSizeConstraintSetResponse :: Newtype UpdateSizeConstraintSetResponse _


-- | <p>A request to update a <a>SqlInjectionMatchSet</a>.</p>
newtype UpdateSqlInjectionMatchSetRequest = UpdateSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SqlInjectionMatchSetUpdates)
  }
derive instance newtypeUpdateSqlInjectionMatchSetRequest :: Newtype UpdateSqlInjectionMatchSetRequest _


-- | <p>The response to an <a>UpdateSqlInjectionMatchSets</a> request.</p>
newtype UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateSqlInjectionMatchSetResponse :: Newtype UpdateSqlInjectionMatchSetResponse _


newtype UpdateWebACLRequest = UpdateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: NullOrUndefined (WebACLUpdates)
  , "DefaultAction" :: NullOrUndefined (WafAction)
  }
derive instance newtypeUpdateWebACLRequest :: Newtype UpdateWebACLRequest _


newtype UpdateWebACLResponse = UpdateWebACLResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateWebACLResponse :: Newtype UpdateWebACLResponse _


-- | <p>A request to update an <a>XssMatchSet</a>.</p>
newtype UpdateXssMatchSetRequest = UpdateXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (XssMatchSetUpdates)
  }
derive instance newtypeUpdateXssMatchSetRequest :: Newtype UpdateXssMatchSetRequest _


-- | <p>The response to an <a>UpdateXssMatchSets</a> request.</p>
newtype UpdateXssMatchSetResponse = UpdateXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateXssMatchSetResponse :: Newtype UpdateXssMatchSetResponse _


-- | <p>The name specified is invalid.</p>
newtype WAFDisallowedNameException = WAFDisallowedNameException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFDisallowedNameException :: Newtype WAFDisallowedNameException _


-- | <p>The operation failed because of a system problem, even though the request was valid. Retry your request.</p>
newtype WAFInternalErrorException = WAFInternalErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInternalErrorException :: Newtype WAFInternalErrorException _


-- | <p>The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.</p>
newtype WAFInvalidAccountException = WAFInvalidAccountException 
  { 
  }
derive instance newtypeWAFInvalidAccountException :: Newtype WAFInvalidAccountException _


-- | <p>The operation failed because there was nothing to do. For example:</p> <ul> <li> <p>You tried to remove a <code>Rule</code> from a <code>WebACL</code>, but the <code>Rule</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to remove an IP address from an <code>IPSet</code>, but the IP address isn't in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to remove a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add a <code>Rule</code> to a <code>WebACL</code>, but the <code>Rule</code> already exists in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add an IP address to an <code>IPSet</code>, but the IP address already exists in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> already exists in the specified <code>WebACL</code>.</p> </li> </ul>
newtype WAFInvalidOperationException = WAFInvalidOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidOperationException :: Newtype WAFInvalidOperationException _


-- | <p>The operation failed because AWS WAF didn't recognize a parameter in the request. For example:</p> <ul> <li> <p>You specified an invalid parameter name.</p> </li> <li> <p>You specified an invalid value.</p> </li> <li> <p>You tried to update an object (<code>ByteMatchSet</code>, <code>IPSet</code>, <code>Rule</code>, or <code>WebACL</code>) using an action other than <code>INSERT</code> or <code>DELETE</code>.</p> </li> <li> <p>You tried to create a <code>WebACL</code> with a <code>DefaultAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to create a <code>RateBasedRule</code> with a <code>RateKey</code> value other than <code>IP</code>.</p> </li> <li> <p>You tried to update a <code>WebACL</code> with a <code>WafAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>FieldToMatch</code> <code>Type</code> other than HEADER, METHOD, QUERY_STRING, URI, or BODY.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>Field</code> of <code>HEADER</code> but no value for <code>Data</code>.</p> </li> <li> <p>Your request references an ARN that is malformed, or corresponds to a resource with which a web ACL cannot be associated.</p> </li> </ul>
newtype WAFInvalidParameterException = WAFInvalidParameterException 
  { "Field'" :: NullOrUndefined (ParameterExceptionField)
  , "Parameter'" :: NullOrUndefined (ParameterExceptionParameter)
  , "Reason'" :: NullOrUndefined (ParameterExceptionReason)
  }
derive instance newtypeWAFInvalidParameterException :: Newtype WAFInvalidParameterException _


-- | <p>The operation failed because the specified policy is not in the proper format. </p> <p>The policy is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> or <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul>
newtype WAFInvalidPermissionPolicyException = WAFInvalidPermissionPolicyException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidPermissionPolicyException :: Newtype WAFInvalidPermissionPolicyException _


-- | <p>The regular expression (regex) you specified in <code>RegexPatternString</code> is invalid.</p>
newtype WAFInvalidRegexPatternException = WAFInvalidRegexPatternException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidRegexPatternException :: Newtype WAFInvalidRegexPatternException _


-- | <p>The operation exceeds a resource limit, for example, the maximum number of <code>WebACL</code> objects that you can create for an AWS account. For more information, see <a href="http://docs.aws.amazon.com/waf/latest/developerguide/limits.html">Limits</a> in the <i>AWS WAF Developer Guide</i>.</p>
newtype WAFLimitsExceededException = WAFLimitsExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFLimitsExceededException :: Newtype WAFLimitsExceededException _


-- | <p>The operation failed because you tried to delete an object that isn't empty. For example:</p> <ul> <li> <p>You tried to delete a <code>WebACL</code> that still contains one or more <code>Rule</code> objects.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that still contains one or more <code>ByteMatchSet</code> objects or other predicates.</p> </li> <li> <p>You tried to delete a <code>ByteMatchSet</code> that contains one or more <code>ByteMatchTuple</code> objects.</p> </li> <li> <p>You tried to delete an <code>IPSet</code> that references one or more IP addresses.</p> </li> </ul>
newtype WAFNonEmptyEntityException = WAFNonEmptyEntityException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonEmptyEntityException :: Newtype WAFNonEmptyEntityException _


-- | <p>The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:</p> <ul> <li> <p>You tried to add a <code>Rule</code> to or delete a <code>Rule</code> from a <code>WebACL</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchSet</code> to or delete a <code>ByteMatchSet</code> from a <code>Rule</code> that doesn't exist.</p> </li> <li> <p>You tried to add an IP address to or delete an IP address from an <code>IPSet</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to or delete a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code> that doesn't exist.</p> </li> </ul>
newtype WAFNonexistentContainerException = WAFNonexistentContainerException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonexistentContainerException :: Newtype WAFNonexistentContainerException _


-- | <p>The operation failed because the referenced object doesn't exist.</p>
newtype WAFNonexistentItemException = WAFNonexistentItemException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonexistentItemException :: Newtype WAFNonexistentItemException _


-- | <p>The operation failed because you tried to delete an object that is still in use. For example:</p> <ul> <li> <p>You tried to delete a <code>ByteMatchSet</code> that is still referenced by a <code>Rule</code>.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that is still referenced by a <code>WebACL</code>.</p> </li> </ul>
newtype WAFReferencedItemException = WAFReferencedItemException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFReferencedItemException :: Newtype WAFReferencedItemException _


-- | <p>The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.</p>
newtype WAFStaleDataException = WAFStaleDataException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFStaleDataException :: Newtype WAFStaleDataException _


-- | <p>The specified subscription does not exist.</p>
newtype WAFSubscriptionNotFoundException = WAFSubscriptionNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFSubscriptionNotFoundException :: Newtype WAFSubscriptionNotFoundException _


-- | <p>The operation failed because the entity referenced is temporarily unavailable. Retry your request.</p>
newtype WAFUnavailableEntityException = WAFUnavailableEntityException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFUnavailableEntityException :: Newtype WAFUnavailableEntityException _


-- | <p>For the action that is associated with a rule in a <code>WebACL</code>, specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a <code>WebACL</code>, specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a <code>WebACL</code>. </p>
newtype WafAction = WafAction 
  { "Type" :: (WafActionType)
  }
derive instance newtypeWafAction :: Newtype WafAction _


newtype WafActionType = WafActionType String
derive instance newtypeWafActionType :: Newtype WafActionType _


-- | <p>The action to take if any rule within the <code>RuleGroup</code> matches a request. </p>
newtype WafOverrideAction = WafOverrideAction 
  { "Type" :: (WafOverrideActionType)
  }
derive instance newtypeWafOverrideAction :: Newtype WafOverrideAction _


newtype WafOverrideActionType = WafOverrideActionType String
derive instance newtypeWafOverrideActionType :: Newtype WafOverrideActionType _


newtype WafRuleType = WafRuleType String
derive instance newtypeWafRuleType :: Newtype WafRuleType _


-- | <p>Contains the <code>Rules</code> that identify the requests that you want to allow, block, or count. In a <code>WebACL</code>, you also specify a default action (<code>ALLOW</code> or <code>BLOCK</code>), and the action for each <code>Rule</code> that you add to a <code>WebACL</code>, for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the <code>WebACL</code> with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one <code>Rule</code> to a <code>WebACL</code>, a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see <a>UpdateWebACL</a>.</p>
newtype WebACL = WebACL 
  { "WebACLId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "DefaultAction" :: (WafAction)
  , "Rules" :: (ActivatedRules)
  }
derive instance newtypeWebACL :: Newtype WebACL _


newtype WebACLSummaries = WebACLSummaries (Array WebACLSummary)
derive instance newtypeWebACLSummaries :: Newtype WebACLSummaries _


-- | <p>Contains the identifier and the name or description of the <a>WebACL</a>.</p>
newtype WebACLSummary = WebACLSummary 
  { "WebACLId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeWebACLSummary :: Newtype WebACLSummary _


-- | <p>Specifies whether to insert a <code>Rule</code> into or delete a <code>Rule</code> from a <code>WebACL</code>.</p>
newtype WebACLUpdate = WebACLUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }
derive instance newtypeWebACLUpdate :: Newtype WebACLUpdate _


newtype WebACLUpdates = WebACLUpdates (Array WebACLUpdate)
derive instance newtypeWebACLUpdates :: Newtype WebACLUpdates _


-- | <p>A complex type that contains <code>XssMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a <code>XssMatchSet</code> contains more than one <code>XssMatchTuple</code> object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.</p>
newtype XssMatchSet = XssMatchSet 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined (ResourceName)
  , "XssMatchTuples" :: (XssMatchTuples)
  }
derive instance newtypeXssMatchSet :: Newtype XssMatchSet _


newtype XssMatchSetSummaries = XssMatchSetSummaries (Array XssMatchSetSummary)
derive instance newtypeXssMatchSetSummaries :: Newtype XssMatchSetSummaries _


-- | <p>The <code>Id</code> and <code>Name</code> of an <code>XssMatchSet</code>.</p>
newtype XssMatchSetSummary = XssMatchSetSummary 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeXssMatchSetSummary :: Newtype XssMatchSetSummary _


-- | <p>Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an <a>XssMatchSet</a> or delete it from an <code>XssMatchSet</code>.</p>
newtype XssMatchSetUpdate = XssMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "XssMatchTuple" :: (XssMatchTuple)
  }
derive instance newtypeXssMatchSetUpdate :: Newtype XssMatchSetUpdate _


newtype XssMatchSetUpdates = XssMatchSetUpdates (Array XssMatchSetUpdate)
derive instance newtypeXssMatchSetUpdates :: Newtype XssMatchSetUpdates _


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype XssMatchTuple = XssMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }
derive instance newtypeXssMatchTuple :: Newtype XssMatchTuple _


newtype XssMatchTuples = XssMatchTuples (Array XssMatchTuple)
derive instance newtypeXssMatchTuples :: Newtype XssMatchTuples _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
