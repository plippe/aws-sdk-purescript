

-- | <p>This is the <i>AWS WAF Regional API Reference</i> for using AWS WAF with Elastic Load Balancing (ELB) Application Load Balancers. The AWS WAF actions and data types listed in the reference are available for protecting Application Load Balancers. You can use these actions and data types by means of the endpoints listed in <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#waf_region">AWS Regions and Endpoints</a>. This guide is for developers who need detailed information about the AWS WAF API actions, data types, and errors. For detailed information about AWS WAF features and an overview of how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
module AWS.WAFRegional where

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

serviceName = "WAFRegional" :: String


-- | <p>Associates a web ACL with a resource.</p>
associateWebACL :: forall eff. AssociateWebACLRequest -> Aff (exception :: EXCEPTION | eff) AssociateWebACLResponse
associateWebACL = Request.request serviceName "associateWebACL" 


-- | <p>Creates a <code>ByteMatchSet</code>. You then use <a>UpdateByteMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>ByteMatchSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>BadBot</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateByteMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateByteMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createByteMatchSet :: forall eff. CreateByteMatchSetRequest -> Aff (exception :: EXCEPTION | eff) CreateByteMatchSetResponse
createByteMatchSet = Request.request serviceName "createByteMatchSet" 


-- | <p>Creates an <a>GeoMatchSet</a>, which you use to specify which web requests you want to allow or block based on the country that the requests originate from. For example, if you're receiving a lot of requests from one or more countries and you want to block the requests, you can create an <code>GeoMatchSet</code> that contains those countries and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateGeoMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSetSet</code> request to specify the countries that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createGeoMatchSet :: forall eff. CreateGeoMatchSetRequest -> Aff (exception :: EXCEPTION | eff) CreateGeoMatchSetResponse
createGeoMatchSet = Request.request serviceName "createGeoMatchSet" 


-- | <p>Creates an <a>IPSet</a>, which you use to specify which web requests you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that contains those IP addresses and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateIPSet</code> request.</p> </li> <li> <p>Submit a <code>CreateIPSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createIPSet :: forall eff. CreateIPSetRequest -> Aff (exception :: EXCEPTION | eff) CreateIPSetResponse
createIPSet = Request.request serviceName "createIPSet" 


-- | <p>Creates a <a>RateBasedRule</a>. The <code>RateBasedRule</code> contains a <code>RateLimit</code>, which specifies the maximum number of requests that AWS WAF allows from a specified IP address in a five-minute period. The <code>RateBasedRule</code> also contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to count or block if these requests exceed the <code>RateLimit</code>.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request not only must exceed the <code>RateLimit</code>, but it also must match all the specifications to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that meet the conditions in the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions must be received at a rate of more than 15,000 requests every five minutes. If both conditions are met and the rate is exceeded, AWS WAF blocks the requests. If the rate drops below 15,000 for a five-minute period, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p> <p>To create and configure a <code>RateBasedRule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the rule. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRateBasedRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRateBasedRule</code> request to specify the predicates that you want to include in the rule.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RateBasedRule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRateBasedRule :: forall eff. CreateRateBasedRuleRequest -> Aff (exception :: EXCEPTION | eff) CreateRateBasedRuleResponse
createRateBasedRule = Request.request serviceName "createRateBasedRule" 


-- | <p>Creates a <a>RegexMatchSet</a>. You then use <a>UpdateRegexMatchSet</a> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the <code>User-Agent</code> header or the query string. For example, you can create a <code>RegexMatchSet</code> that contains a <code>RegexMatchTuple</code> that looks for any requests with <code>User-Agent</code> headers that match a <code>RegexPatternSet</code> with pattern <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexMatchSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value, using a <code>RegexPatternSet</code>, that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexMatchSet :: forall eff. CreateRegexMatchSetRequest -> Aff (exception :: EXCEPTION | eff) CreateRegexMatchSetResponse
createRegexMatchSet = Request.request serviceName "createRegexMatchSet" 


-- | <p>Creates a <code>RegexPatternSet</code>. You then use <a>UpdateRegexPatternSet</a> to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Submit a <code>CreateRegexPatternSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateRegexPatternSet</a> request to specify the string that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRegexPatternSet :: forall eff. CreateRegexPatternSetRequest -> Aff (exception :: EXCEPTION | eff) CreateRegexPatternSetResponse
createRegexPatternSet = Request.request serviceName "createRegexPatternSet" 


-- | <p>Creates a <code>Rule</code>, which contains the <code>IPSet</code> objects, <code>ByteMatchSet</code> objects, and other predicates that identify the requests that you want to block. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed or blocked. For example, suppose you add the following to a <code>Rule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to blocks requests that satisfy the <code>Rule</code>. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>. For more information, see <a>CreateByteMatchSet</a>, <a>CreateIPSet</a>, and <a>CreateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateRule</code> request.</p> </li> <li> <p>Submit a <code>CreateRule</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to specify the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. For more information, see <a>CreateWebACL</a>.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRule :: forall eff. CreateRuleRequest -> Aff (exception :: EXCEPTION | eff) CreateRuleResponse
createRule = Request.request serviceName "createRule" 


-- | <p>Creates a <code>RuleGroup</code>. A rule group is a collection of predefined rules that you add to a web ACL. You use <a>UpdateRuleGroup</a> to add rules to the rule group.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createRuleGroup :: forall eff. CreateRuleGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateRuleGroupResponse
createRuleGroup = Request.request serviceName "createRuleGroup" 


-- | <p>Creates a <code>SizeConstraintSet</code>. You then use <a>UpdateSizeConstraintSet</a> to identify the part of a web request that you want AWS WAF to check for length, such as the length of the <code>User-Agent</code> header or the length of the query string. For example, you can create a <code>SizeConstraintSet</code> that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSizeConstraintSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <a>UpdateSizeConstraintSet</a> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSizeConstraintSet :: forall eff. CreateSizeConstraintSetRequest -> Aff (exception :: EXCEPTION | eff) CreateSizeConstraintSetResponse
createSizeConstraintSet = Request.request serviceName "createSizeConstraintSet" 


-- | <p>Creates a <a>SqlInjectionMatchSet</a>, which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateSqlInjectionMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createSqlInjectionMatchSet :: forall eff. CreateSqlInjectionMatchSetRequest -> Aff (exception :: EXCEPTION | eff) CreateSqlInjectionMatchSetResponse
createSqlInjectionMatchSet = Request.request serviceName "createSqlInjectionMatchSet" 


-- | <p>Creates a <code>WebACL</code>, which contains the <code>Rules</code> that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates <code>Rules</code> in order based on the value of <code>Priority</code> for each <code>Rule</code>.</p> <p>You also specify a default action, either <code>ALLOW</code> or <code>BLOCK</code>. If a web request doesn't match any of the <code>Rules</code> in a <code>WebACL</code>, AWS WAF responds to the request with the default action. </p> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>ByteMatchSet</code> objects and other predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateWebACL</code> request.</p> </li> <li> <p>Submit a <code>CreateWebACL</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <a>UpdateWebACL</a> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution.</p> </li> </ol> <p>For more information about how to use the AWS WAF API, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createWebACL :: forall eff. CreateWebACLRequest -> Aff (exception :: EXCEPTION | eff) CreateWebACLResponse
createWebACL = Request.request serviceName "createWebACL" 


-- | <p>Creates an <a>XssMatchSet</a>, which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.</p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>CreateXssMatchSet</code> request.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateXssMatchSet</a> request.</p> </li> <li> <p>Submit an <a>UpdateXssMatchSet</a> request to specify the parts of web requests in which you want to allow, block, or count cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
createXssMatchSet :: forall eff. CreateXssMatchSetRequest -> Aff (exception :: EXCEPTION | eff) CreateXssMatchSetResponse
createXssMatchSet = Request.request serviceName "createXssMatchSet" 


-- | <p>Permanently deletes a <a>ByteMatchSet</a>. You can't delete a <code>ByteMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>ByteMatchTuple</a> objects (any filters).</p> <p>If you just want to remove a <code>ByteMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>ByteMatchSet</code> to remove filters, if any. For more information, see <a>UpdateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteByteMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteByteMatchSet</code> request.</p> </li> </ol>
deleteByteMatchSet :: forall eff. DeleteByteMatchSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteByteMatchSetResponse
deleteByteMatchSet = Request.request serviceName "deleteByteMatchSet" 


-- | <p>Permanently deletes a <a>GeoMatchSet</a>. You can't delete a <code>GeoMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any countries.</p> <p>If you just want to remove a <code>GeoMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>GeoMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>GeoMatchSet</code> to remove any countries. For more information, see <a>UpdateGeoMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteGeoMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteGeoMatchSet</code> request.</p> </li> </ol>
deleteGeoMatchSet :: forall eff. DeleteGeoMatchSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteGeoMatchSetResponse
deleteGeoMatchSet = Request.request serviceName "deleteGeoMatchSet" 


-- | <p>Permanently deletes an <a>IPSet</a>. You can't delete an <code>IPSet</code> if it's still used in any <code>Rules</code> or if it still includes any IP addresses.</p> <p>If you just want to remove an <code>IPSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>IPSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>IPSet</code> to remove IP address ranges, if any. For more information, see <a>UpdateIPSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteIPSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteIPSet</code> request.</p> </li> </ol>
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteIPSetResponse
deleteIPSet = Request.request serviceName "deleteIPSet" 


-- | <p>Permanently deletes an IAM policy from the specified RuleGroup.</p> <p>The user making the request must be the owner of the RuleGroup.</p>
deletePermissionPolicy :: forall eff. DeletePermissionPolicyRequest -> Aff (exception :: EXCEPTION | eff) DeletePermissionPolicyResponse
deletePermissionPolicy = Request.request serviceName "deletePermissionPolicy" 


-- | <p>Permanently deletes a <a>RateBasedRule</a>. You can't delete a rule if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a rule from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RateBasedRule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RateBasedRule</code> to remove predicates, if any. For more information, see <a>UpdateRateBasedRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRateBasedRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRateBasedRule</code> request.</p> </li> </ol>
deleteRateBasedRule :: forall eff. DeleteRateBasedRuleRequest -> Aff (exception :: EXCEPTION | eff) DeleteRateBasedRuleResponse
deleteRateBasedRule = Request.request serviceName "deleteRateBasedRule" 


-- | <p>Permanently deletes a <a>RegexMatchSet</a>. You can't delete a <code>RegexMatchSet</code> if it's still used in any <code>Rules</code> or if it still includes any <code>RegexMatchTuples</code> objects (any filters).</p> <p>If you just want to remove a <code>RegexMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>RegexMatchSet</code> to remove filters, if any. For more information, see <a>UpdateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRegexMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteRegexMatchSet</code> request.</p> </li> </ol>
deleteRegexMatchSet :: forall eff. DeleteRegexMatchSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteRegexMatchSetResponse
deleteRegexMatchSet = Request.request serviceName "deleteRegexMatchSet" 


-- | <p>Permanently deletes a <a>RegexPatternSet</a>. You can't delete a <code>RegexPatternSet</code> if it's still used in any <code>RegexMatchSet</code> or if the <code>RegexPatternSet</code> is not empty. </p>
deleteRegexPatternSet :: forall eff. DeleteRegexPatternSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteRegexPatternSetResponse
deleteRegexPatternSet = Request.request serviceName "deleteRegexPatternSet" 


-- | <p>Permanently deletes a <a>Rule</a>. You can't delete a <code>Rule</code> if it's still used in any <code>WebACL</code> objects or if it still includes any predicates, such as <code>ByteMatchSet</code> objects.</p> <p>If you just want to remove a <code>Rule</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>Rule</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>Rule</code> to remove predicates, if any. For more information, see <a>UpdateRule</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRule</code> request.</p> </li> <li> <p>Submit a <code>DeleteRule</code> request.</p> </li> </ol>
deleteRule :: forall eff. DeleteRuleRequest -> Aff (exception :: EXCEPTION | eff) DeleteRuleResponse
deleteRule = Request.request serviceName "deleteRule" 


-- | <p>Permanently deletes a <a>RuleGroup</a>. You can't delete a <code>RuleGroup</code> if it's still used in any <code>WebACL</code> objects or if it still includes any rules.</p> <p>If you just want to remove a <code>RuleGroup</code> from a <code>WebACL</code>, use <a>UpdateWebACL</a>.</p> <p>To permanently delete a <code>RuleGroup</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>RuleGroup</code> to remove rules, if any. For more information, see <a>UpdateRuleGroup</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteRuleGroup</code> request.</p> </li> <li> <p>Submit a <code>DeleteRuleGroup</code> request.</p> </li> </ol>
deleteRuleGroup :: forall eff. DeleteRuleGroupRequest -> Aff (exception :: EXCEPTION | eff) DeleteRuleGroupResponse
deleteRuleGroup = Request.request serviceName "deleteRuleGroup" 


-- | <p>Permanently deletes a <a>SizeConstraintSet</a>. You can't delete a <code>SizeConstraintSet</code> if it's still used in any <code>Rules</code> or if it still includes any <a>SizeConstraint</a> objects (any filters).</p> <p>If you just want to remove a <code>SizeConstraintSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>SizeConstraintSet</code> to remove filters, if any. For more information, see <a>UpdateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSizeConstraintSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSizeConstraintSet</code> request.</p> </li> </ol>
deleteSizeConstraintSet :: forall eff. DeleteSizeConstraintSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteSizeConstraintSetResponse
deleteSizeConstraintSet = Request.request serviceName "deleteSizeConstraintSet" 


-- | <p>Permanently deletes a <a>SqlInjectionMatchSet</a>. You can't delete a <code>SqlInjectionMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>SqlInjectionMatchTuple</a> objects.</p> <p>If you just want to remove a <code>SqlInjectionMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete a <code>SqlInjectionMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>SqlInjectionMatchSet</code> to remove filters, if any. For more information, see <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteSqlInjectionMatchSet</code> request.</p> </li> </ol>
deleteSqlInjectionMatchSet :: forall eff. DeleteSqlInjectionMatchSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteSqlInjectionMatchSetResponse
deleteSqlInjectionMatchSet = Request.request serviceName "deleteSqlInjectionMatchSet" 


-- | <p>Permanently deletes a <a>WebACL</a>. You can't delete a <code>WebACL</code> if it still contains any <code>Rules</code>.</p> <p>To delete a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Update the <code>WebACL</code> to remove <code>Rules</code>, if any. For more information, see <a>UpdateWebACL</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteWebACL</code> request.</p> </li> <li> <p>Submit a <code>DeleteWebACL</code> request.</p> </li> </ol>
deleteWebACL :: forall eff. DeleteWebACLRequest -> Aff (exception :: EXCEPTION | eff) DeleteWebACLResponse
deleteWebACL = Request.request serviceName "deleteWebACL" 


-- | <p>Permanently deletes an <a>XssMatchSet</a>. You can't delete an <code>XssMatchSet</code> if it's still used in any <code>Rules</code> or if it still contains any <a>XssMatchTuple</a> objects.</p> <p>If you just want to remove an <code>XssMatchSet</code> from a <code>Rule</code>, use <a>UpdateRule</a>.</p> <p>To permanently delete an <code>XssMatchSet</code> from AWS WAF, perform the following steps:</p> <ol> <li> <p>Update the <code>XssMatchSet</code> to remove filters, if any. For more information, see <a>UpdateXssMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of a <code>DeleteXssMatchSet</code> request.</p> </li> <li> <p>Submit a <code>DeleteXssMatchSet</code> request.</p> </li> </ol>
deleteXssMatchSet :: forall eff. DeleteXssMatchSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteXssMatchSetResponse
deleteXssMatchSet = Request.request serviceName "deleteXssMatchSet" 


-- | <p>Removes a web ACL from the specified resource.</p>
disassociateWebACL :: forall eff. DisassociateWebACLRequest -> Aff (exception :: EXCEPTION | eff) DisassociateWebACLResponse
disassociateWebACL = Request.request serviceName "disassociateWebACL" 


-- | <p>Returns the <a>ByteMatchSet</a> specified by <code>ByteMatchSetId</code>.</p>
getByteMatchSet :: forall eff. GetByteMatchSetRequest -> Aff (exception :: EXCEPTION | eff) GetByteMatchSetResponse
getByteMatchSet = Request.request serviceName "getByteMatchSet" 


-- | <p>When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request. Change tokens ensure that your application doesn't submit conflicting requests to AWS WAF.</p> <p>Each create, update, or delete request must use a unique change token. If your application submits a <code>GetChangeToken</code> request and then submits a second <code>GetChangeToken</code> request before submitting a create, update, or delete request, the second <code>GetChangeToken</code> request returns the same value as the first <code>GetChangeToken</code> request.</p> <p>When you use a change token in a create, update, or delete request, the status of the change token changes to <code>PENDING</code>, which indicates that AWS WAF is propagating the change to all AWS WAF servers. Use <code>GetChangeTokenStatus</code> to determine the status of your change token.</p>
getChangeToken :: forall eff. GetChangeTokenRequest -> Aff (exception :: EXCEPTION | eff) GetChangeTokenResponse
getChangeToken = Request.request serviceName "getChangeToken" 


-- | <p>Returns the status of a <code>ChangeToken</code> that you got by calling <a>GetChangeToken</a>. <code>ChangeTokenStatus</code> is one of the following values:</p> <ul> <li> <p> <code>PROVISIONED</code>: You requested the change token by calling <code>GetChangeToken</code>, but you haven't used it yet in a call to create, update, or delete an AWS WAF object.</p> </li> <li> <p> <code>PENDING</code>: AWS WAF is propagating the create, update, or delete request to all AWS WAF servers.</p> </li> <li> <p> <code>IN_SYNC</code>: Propagation is complete.</p> </li> </ul>
getChangeTokenStatus :: forall eff. GetChangeTokenStatusRequest -> Aff (exception :: EXCEPTION | eff) GetChangeTokenStatusResponse
getChangeTokenStatus = Request.request serviceName "getChangeTokenStatus" 


-- | <p>Returns the <a>GeoMatchSet</a> that is specified by <code>GeoMatchSetId</code>.</p>
getGeoMatchSet :: forall eff. GetGeoMatchSetRequest -> Aff (exception :: EXCEPTION | eff) GetGeoMatchSetResponse
getGeoMatchSet = Request.request serviceName "getGeoMatchSet" 


-- | <p>Returns the <a>IPSet</a> that is specified by <code>IPSetId</code>.</p>
getIPSet :: forall eff. GetIPSetRequest -> Aff (exception :: EXCEPTION | eff) GetIPSetResponse
getIPSet = Request.request serviceName "getIPSet" 


-- | <p>Returns the IAM policy attached to the RuleGroup.</p>
getPermissionPolicy :: forall eff. GetPermissionPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetPermissionPolicyResponse
getPermissionPolicy = Request.request serviceName "getPermissionPolicy" 


-- | <p>Returns the <a>RateBasedRule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRateBasedRule</code> request.</p>
getRateBasedRule :: forall eff. GetRateBasedRuleRequest -> Aff (exception :: EXCEPTION | eff) GetRateBasedRuleResponse
getRateBasedRule = Request.request serviceName "getRateBasedRule" 


-- | <p>Returns an array of IP addresses currently being blocked by the <a>RateBasedRule</a> that is specified by the <code>RuleId</code>. The maximum number of managed keys that will be blocked is 10,000. If more than 10,000 addresses exceed the rate limit, the 10,000 addresses with the highest rates will be blocked.</p>
getRateBasedRuleManagedKeys :: forall eff. GetRateBasedRuleManagedKeysRequest -> Aff (exception :: EXCEPTION | eff) GetRateBasedRuleManagedKeysResponse
getRateBasedRuleManagedKeys = Request.request serviceName "getRateBasedRuleManagedKeys" 


-- | <p>Returns the <a>RegexMatchSet</a> specified by <code>RegexMatchSetId</code>.</p>
getRegexMatchSet :: forall eff. GetRegexMatchSetRequest -> Aff (exception :: EXCEPTION | eff) GetRegexMatchSetResponse
getRegexMatchSet = Request.request serviceName "getRegexMatchSet" 


-- | <p>Returns the <a>RegexPatternSet</a> specified by <code>RegexPatternSetId</code>.</p>
getRegexPatternSet :: forall eff. GetRegexPatternSetRequest -> Aff (exception :: EXCEPTION | eff) GetRegexPatternSetResponse
getRegexPatternSet = Request.request serviceName "getRegexPatternSet" 


-- | <p>Returns the <a>Rule</a> that is specified by the <code>RuleId</code> that you included in the <code>GetRule</code> request.</p>
getRule :: forall eff. GetRuleRequest -> Aff (exception :: EXCEPTION | eff) GetRuleResponse
getRule = Request.request serviceName "getRule" 


-- | <p>Returns the <a>RuleGroup</a> that is specified by the <code>RuleGroupId</code> that you included in the <code>GetRuleGroup</code> request.</p> <p>To view the rules in a rule group, use <a>ListActivatedRulesInRuleGroup</a>.</p>
getRuleGroup :: forall eff. GetRuleGroupRequest -> Aff (exception :: EXCEPTION | eff) GetRuleGroupResponse
getRuleGroup = Request.request serviceName "getRuleGroup" 


-- | <p>Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose. You can specify a sample size of up to 500 requests, and you can specify any time range in the previous three hours.</p> <p> <code>GetSampledRequests</code> returns a time range, which is usually the time range that you specified. However, if your resource (such as a CloudFront distribution) received 5,000 requests before the specified time range elapsed, <code>GetSampledRequests</code> returns an updated time range. This new time range indicates the actual period during which AWS WAF selected the requests in the sample.</p>
getSampledRequests :: forall eff. GetSampledRequestsRequest -> Aff (exception :: EXCEPTION | eff) GetSampledRequestsResponse
getSampledRequests = Request.request serviceName "getSampledRequests" 


-- | <p>Returns the <a>SizeConstraintSet</a> specified by <code>SizeConstraintSetId</code>.</p>
getSizeConstraintSet :: forall eff. GetSizeConstraintSetRequest -> Aff (exception :: EXCEPTION | eff) GetSizeConstraintSetResponse
getSizeConstraintSet = Request.request serviceName "getSizeConstraintSet" 


-- | <p>Returns the <a>SqlInjectionMatchSet</a> that is specified by <code>SqlInjectionMatchSetId</code>.</p>
getSqlInjectionMatchSet :: forall eff. GetSqlInjectionMatchSetRequest -> Aff (exception :: EXCEPTION | eff) GetSqlInjectionMatchSetResponse
getSqlInjectionMatchSet = Request.request serviceName "getSqlInjectionMatchSet" 


-- | <p>Returns the <a>WebACL</a> that is specified by <code>WebACLId</code>.</p>
getWebACL :: forall eff. GetWebACLRequest -> Aff (exception :: EXCEPTION | eff) GetWebACLResponse
getWebACL = Request.request serviceName "getWebACL" 


-- | <p>Returns the web ACL for the specified resource.</p>
getWebACLForResource :: forall eff. GetWebACLForResourceRequest -> Aff (exception :: EXCEPTION | eff) GetWebACLForResourceResponse
getWebACLForResource = Request.request serviceName "getWebACLForResource" 


-- | <p>Returns the <a>XssMatchSet</a> that is specified by <code>XssMatchSetId</code>.</p>
getXssMatchSet :: forall eff. GetXssMatchSetRequest -> Aff (exception :: EXCEPTION | eff) GetXssMatchSetResponse
getXssMatchSet = Request.request serviceName "getXssMatchSet" 


-- | <p>Returns an array of <a>ActivatedRule</a> objects.</p>
listActivatedRulesInRuleGroup :: forall eff. ListActivatedRulesInRuleGroupRequest -> Aff (exception :: EXCEPTION | eff) ListActivatedRulesInRuleGroupResponse
listActivatedRulesInRuleGroup = Request.request serviceName "listActivatedRulesInRuleGroup" 


-- | <p>Returns an array of <a>ByteMatchSetSummary</a> objects.</p>
listByteMatchSets :: forall eff. ListByteMatchSetsRequest -> Aff (exception :: EXCEPTION | eff) ListByteMatchSetsResponse
listByteMatchSets = Request.request serviceName "listByteMatchSets" 


-- | <p>Returns an array of <a>GeoMatchSetSummary</a> objects in the response.</p>
listGeoMatchSets :: forall eff. ListGeoMatchSetsRequest -> Aff (exception :: EXCEPTION | eff) ListGeoMatchSetsResponse
listGeoMatchSets = Request.request serviceName "listGeoMatchSets" 


-- | <p>Returns an array of <a>IPSetSummary</a> objects in the response.</p>
listIPSets :: forall eff. ListIPSetsRequest -> Aff (exception :: EXCEPTION | eff) ListIPSetsResponse
listIPSets = Request.request serviceName "listIPSets" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRateBasedRules :: forall eff. ListRateBasedRulesRequest -> Aff (exception :: EXCEPTION | eff) ListRateBasedRulesResponse
listRateBasedRules = Request.request serviceName "listRateBasedRules" 


-- | <p>Returns an array of <a>RegexMatchSetSummary</a> objects.</p>
listRegexMatchSets :: forall eff. ListRegexMatchSetsRequest -> Aff (exception :: EXCEPTION | eff) ListRegexMatchSetsResponse
listRegexMatchSets = Request.request serviceName "listRegexMatchSets" 


-- | <p>Returns an array of <a>RegexPatternSetSummary</a> objects.</p>
listRegexPatternSets :: forall eff. ListRegexPatternSetsRequest -> Aff (exception :: EXCEPTION | eff) ListRegexPatternSetsResponse
listRegexPatternSets = Request.request serviceName "listRegexPatternSets" 


-- | <p>Returns an array of resources associated with the specified web ACL.</p>
listResourcesForWebACL :: forall eff. ListResourcesForWebACLRequest -> Aff (exception :: EXCEPTION | eff) ListResourcesForWebACLResponse
listResourcesForWebACL = Request.request serviceName "listResourcesForWebACL" 


-- | <p>Returns an array of <a>RuleGroup</a> objects.</p>
listRuleGroups :: forall eff. ListRuleGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListRuleGroupsResponse
listRuleGroups = Request.request serviceName "listRuleGroups" 


-- | <p>Returns an array of <a>RuleSummary</a> objects.</p>
listRules :: forall eff. ListRulesRequest -> Aff (exception :: EXCEPTION | eff) ListRulesResponse
listRules = Request.request serviceName "listRules" 


-- | <p>Returns an array of <a>SizeConstraintSetSummary</a> objects.</p>
listSizeConstraintSets :: forall eff. ListSizeConstraintSetsRequest -> Aff (exception :: EXCEPTION | eff) ListSizeConstraintSetsResponse
listSizeConstraintSets = Request.request serviceName "listSizeConstraintSets" 


-- | <p>Returns an array of <a>SqlInjectionMatchSet</a> objects.</p>
listSqlInjectionMatchSets :: forall eff. ListSqlInjectionMatchSetsRequest -> Aff (exception :: EXCEPTION | eff) ListSqlInjectionMatchSetsResponse
listSqlInjectionMatchSets = Request.request serviceName "listSqlInjectionMatchSets" 


-- | <p>Returns an array of <a>RuleGroup</a> objects that you are subscribed to.</p>
listSubscribedRuleGroups :: forall eff. ListSubscribedRuleGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListSubscribedRuleGroupsResponse
listSubscribedRuleGroups = Request.request serviceName "listSubscribedRuleGroups" 


-- | <p>Returns an array of <a>WebACLSummary</a> objects in the response.</p>
listWebACLs :: forall eff. ListWebACLsRequest -> Aff (exception :: EXCEPTION | eff) ListWebACLsResponse
listWebACLs = Request.request serviceName "listWebACLs" 


-- | <p>Returns an array of <a>XssMatchSet</a> objects.</p>
listXssMatchSets :: forall eff. ListXssMatchSetsRequest -> Aff (exception :: EXCEPTION | eff) ListXssMatchSetsResponse
listXssMatchSets = Request.request serviceName "listXssMatchSets" 


-- | <p>Attaches a IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.</p> <p>The <code>PutPermissionPolicy</code> is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> and <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html">IAM Policies</a>. </p> <p>An example of a valid policy parameter is shown in the Examples section below.</p>
putPermissionPolicy :: forall eff. PutPermissionPolicyRequest -> Aff (exception :: EXCEPTION | eff) PutPermissionPolicyResponse
putPermissionPolicy = Request.request serviceName "putPermissionPolicy" 


-- | <p>Inserts or deletes <a>ByteMatchTuple</a> objects (filters) in a <a>ByteMatchSet</a>. For each <code>ByteMatchTuple</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>ByteMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see <code>TargetString</code> in the <a>ByteMatchTuple</a> data type. </p> </li> <li> <p>Where to look, such as at the beginning or the end of a query string.</p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p>For example, you can add a <code>ByteMatchSetUpdate</code> object that matches web requests in which <code>User-Agent</code> headers contain the string <code>BadBot</code>. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>ByteMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>ByteMatchSet.</code> For more information, see <a>CreateByteMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateByteMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateByteMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateByteMatchSet :: forall eff. UpdateByteMatchSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateByteMatchSetResponse
updateByteMatchSet = Request.request serviceName "updateByteMatchSet" 


-- | <p>Inserts or deletes <a>GeoMatchConstraint</a> objects in an <code>GeoMatchSet</code>. For each <code>GeoMatchConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>GeoMatchConstraint</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The <code>Type</code>. The only valid value for <code>Type</code> is <code>Country</code>.</p> </li> <li> <p>The <code>Value</code>, which is a two character code for the country to add to the <code>GeoMatchConstraint</code> object. Valid codes are listed in <a>GeoMatchConstraint$Value</a>.</p> </li> </ul> <p>To create and configure an <code>GeoMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateGeoMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateGeoMatchSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateGeoMatchSet</code> request to specify the country that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>GeoMatchSet</code>, you specify the country that you want to add and/or the country that you want to delete. If you want to change a country, you delete the existing country and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateGeoMatchSet :: forall eff. UpdateGeoMatchSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateGeoMatchSetResponse
updateGeoMatchSet = Request.request serviceName "updateGeoMatchSet" 


-- | <p>Inserts or deletes <a>IPSetDescriptor</a> objects in an <code>IPSet</code>. For each <code>IPSetDescriptor</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change an <code>IPSetDescriptor</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The IP address version, <code>IPv4</code> or <code>IPv6</code>. </p> </li> <li> <p>The IP address in CIDR notation, for example, <code>192.0.2.0/24</code> (for the range of IP addresses from <code>192.0.2.0</code> to <code>192.0.2.255</code>) or <code>192.0.2.44/32</code> (for the individual IP address <code>192.0.2.44</code>). </p> </li> </ul> <p>AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>.</p> <p>IPv6 addresses can be represented using any of the following formats:</p> <ul> <li> <p>1111:0000:0000:0000:0000:0000:0000:0111/128</p> </li> <li> <p>1111:0:0:0:0:0:0:0111/128</p> </li> <li> <p>1111::0111/128</p> </li> <li> <p>1111::111/128</p> </li> </ul> <p>You use an <code>IPSet</code> to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you're receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an <code>IPSet</code> that specifies those IP addresses, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>IPSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateIPSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateIPSet</code> request to specify the IP addresses that you want AWS WAF to watch for.</p> </li> </ol> <p>When you update an <code>IPSet</code>, you specify the IP addresses that you want to add and/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateIPSetResponse
updateIPSet = Request.request serviceName "updateIPSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a rule and updates the <code>RateLimit</code> in the rule. </p> <p>Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to block or count. The <code>RateLimit</code> specifies the number of requests every five minutes that triggers the rule.</p> <p>If you add more than one predicate to a <code>RateBasedRule</code>, a request must match all the predicates and exceed the <code>RateLimit</code> to be counted or blocked. For example, suppose you add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44/32</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that matches <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>You then add the <code>RateBasedRule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 <i>and</i> the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code>. Further, requests that match these two conditions much be received at a rate of more than 15,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.</p> <p>As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a <code>RateBasedRule</code>:</p> <ul> <li> <p>A <code>ByteMatchSet</code> with <code>FieldToMatch</code> of <code>URI</code> </p> </li> <li> <p>A <code>PositionalConstraint</code> of <code>STARTS_WITH</code> </p> </li> <li> <p>A <code>TargetString</code> of <code>login</code> </p> </li> </ul> <p>Further, you specify a <code>RateLimit</code> of 15,000.</p> <p>By adding this <code>RateBasedRule</code> to a <code>WebACL</code>, you could limit requests to your login page without affecting the rest of your site.</p>
updateRateBasedRule :: forall eff. UpdateRateBasedRuleRequest -> Aff (exception :: EXCEPTION | eff) UpdateRateBasedRuleResponse
updateRateBasedRule = Request.request serviceName "updateRateBasedRule" 


-- | <p>Inserts or deletes <a>RegexMatchTuple</a> objects (filters) in a <a>RegexMatchSet</a>. For each <code>RegexMatchSetUpdate</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>RegexMatchSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul> <p> For example, you can create a <code>RegexPatternSet</code> that matches any requests with <code>User-Agent</code> headers that contain the string <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p> <p>To create and configure a <code>RegexMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexMatchSet.</code> For more information, see <a>CreateRegexMatchSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexMatchSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexMatchSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the <code>RegexPatternSet</code> that contain the regular expression patters you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexMatchSet :: forall eff. UpdateRegexMatchSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateRegexMatchSetResponse
updateRegexMatchSet = Request.request serviceName "updateRegexMatchSet" 


-- | <p>Inserts or deletes <code>RegexPatternString</code> objects in a <a>RegexPatternSet</a>. For each <code>RegexPatternString</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the <code>RegexPatternString</code>.</p> </li> <li> <p>The regular expression pattern that you want to insert or delete. For more information, see <a>RegexPatternSet</a>. </p> </li> </ul> <p> For example, you can create a <code>RegexPatternString</code> such as <code>B[a@]dB[o0]t</code>. AWS WAF will match this <code>RegexPatternString</code> to:</p> <ul> <li> <p>BadBot</p> </li> <li> <p>BadB0t</p> </li> <li> <p>B@dBot</p> </li> <li> <p>B@dB0t</p> </li> </ul> <p>To create and configure a <code>RegexPatternSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>RegexPatternSet.</code> For more information, see <a>CreateRegexPatternSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateRegexPatternSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateRegexPatternSet</code> request to specify the regular expression pattern that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRegexPatternSet :: forall eff. UpdateRegexPatternSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateRegexPatternSetResponse
updateRegexPatternSet = Request.request serviceName "updateRegexPatternSet" 


-- | <p>Inserts or deletes <a>Predicate</a> objects in a <code>Rule</code>. Each <code>Predicate</code> object identifies a predicate, such as a <a>ByteMatchSet</a> or an <a>IPSet</a>, that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a <code>Rule</code>, a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose you add the following to a <code>Rule</code>: </p> <ul> <li> <p>A <code>ByteMatchSet</code> that matches the value <code>BadBot</code> in the <code>User-Agent</code> header</p> </li> <li> <p>An <code>IPSet</code> that matches the IP address <code>192.0.2.44</code> </p> </li> </ul> <p>You then add the <code>Rule</code> to a <code>WebACL</code> and specify that you want to block requests that satisfy the <code>Rule</code>. For a request to be blocked, the <code>User-Agent</code> header in the request must contain the value <code>BadBot</code> <i>and</i> the request must originate from the IP address 192.0.2.44.</p> <p>To create and configure a <code>Rule</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in the <code>Rule</code>.</p> </li> <li> <p>Create the <code>Rule</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRule</a> request.</p> </li> <li> <p>Submit an <code>UpdateRule</code> request to add predicates to the <code>Rule</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>Rule</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>ByteMatchSet</code> or <code>IPSet</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRule :: forall eff. UpdateRuleRequest -> Aff (exception :: EXCEPTION | eff) UpdateRuleResponse
updateRule = Request.request serviceName "updateRule" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>RuleGroup</code>.</p> <p>You can only insert <code>REGULAR</code> rules into a rule group.</p> <p>You can have a maximum of ten rules per rule group.</p> <p>To create and configure a <code>RuleGroup</code>, perform the following steps:</p> <ol> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>RuleGroup</code>. See <a>CreateRule</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateRuleGroup</a> request.</p> </li> <li> <p>Submit an <code>UpdateRuleGroup</code> request to add <code>Rules</code> to the <code>RuleGroup</code>.</p> </li> <li> <p>Create and update a <code>WebACL</code> that contains the <code>RuleGroup</code>. See <a>CreateWebACL</a>.</p> </li> </ol> <p>If you want to replace one <code>Rule</code> with another, you delete the existing one and add the new one.</p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateRuleGroup :: forall eff. UpdateRuleGroupRequest -> Aff (exception :: EXCEPTION | eff) UpdateRuleGroupResponse
updateRuleGroup = Request.request serviceName "updateRuleGroup" 


-- | <p>Inserts or deletes <a>SizeConstraint</a> objects (filters) in a <a>SizeConstraintSet</a>. For each <code>SizeConstraint</code> object, you specify the following values: </p> <ul> <li> <p>Whether to insert or delete the object from the array. If you want to change a <code>SizeConstraintSetUpdate</code> object, you delete the existing object and add a new one.</p> </li> <li> <p>The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the <code>User-Agent</code> header.</p> </li> <li> <p>Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first <code>8192</code> bytes of your request to AWS WAF.</p> </li> <li> <p>A <code>ComparisonOperator</code> used for evaluating the selected part of the request against the specified <code>Size</code>, such as equals, greater than, less than, and so on.</p> </li> <li> <p>The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.</p> </li> </ul> <p>For example, you can add a <code>SizeConstraintSetUpdate</code> object that matches web requests in which the length of the <code>User-Agent</code> header is greater than 100 bytes. You can then configure AWS WAF to block those requests.</p> <p>To create and configure a <code>SizeConstraintSet</code>, perform the following steps:</p> <ol> <li> <p>Create a <code>SizeConstraintSet.</code> For more information, see <a>CreateSizeConstraintSet</a>.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <code>UpdateSizeConstraintSet</code> request.</p> </li> <li> <p>Submit an <code>UpdateSizeConstraintSet</code> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSizeConstraintSet :: forall eff. UpdateSizeConstraintSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateSizeConstraintSetResponse
updateSizeConstraintSet = Request.request serviceName "updateSizeConstraintSet" 


-- | <p>Inserts or deletes <a>SqlInjectionMatchTuple</a> objects (filters) in a <a>SqlInjectionMatchSet</a>. For each <code>SqlInjectionMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>SqlInjectionMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.</p> </li> </ul> <p>You use <code>SqlInjectionMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a <code>SqlInjectionMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure a <code>SqlInjectionMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateSqlInjectionMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateSqlInjectionMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateSqlInjectionMatchSet :: forall eff. UpdateSqlInjectionMatchSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateSqlInjectionMatchSetResponse
updateSqlInjectionMatchSet = Request.request serviceName "updateSqlInjectionMatchSet" 


-- | <p>Inserts or deletes <a>ActivatedRule</a> objects in a <code>WebACL</code>. Each <code>Rule</code> identifies web requests that you want to allow, block, or count. When you update a <code>WebACL</code>, you specify the following values:</p> <ul> <li> <p>A default action for the <code>WebACL</code>, either <code>ALLOW</code> or <code>BLOCK</code>. AWS WAF performs the default action if a request doesn't match the criteria in any of the <code>Rules</code> in a <code>WebACL</code>.</p> </li> <li> <p>The <code>Rules</code> that you want to add and/or delete. If you want to replace one <code>Rule</code> with another, you delete the existing <code>Rule</code> and add the new one.</p> </li> <li> <p>For each <code>Rule</code>, whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the <code>Rule</code>.</p> </li> <li> <p>The order in which you want AWS WAF to evaluate the <code>Rules</code> in a <code>WebACL</code>. If you add more than one <code>Rule</code> to a <code>WebACL</code>, AWS WAF evaluates each request against the <code>Rules</code> in order based on the value of <code>Priority</code>. (The <code>Rule</code> that has the lowest value for <code>Priority</code> is evaluated first.) When a web request matches all of the predicates (such as <code>ByteMatchSets</code> and <code>IPSets</code>) in a <code>Rule</code>, AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining <code>Rules</code> in the <code>WebACL</code>, if any. </p> </li> </ul> <p>To create and configure a <code>WebACL</code>, perform the following steps:</p> <ol> <li> <p>Create and update the predicates that you want to include in <code>Rules</code>. For more information, see <a>CreateByteMatchSet</a>, <a>UpdateByteMatchSet</a>, <a>CreateIPSet</a>, <a>UpdateIPSet</a>, <a>CreateSqlInjectionMatchSet</a>, and <a>UpdateSqlInjectionMatchSet</a>.</p> </li> <li> <p>Create and update the <code>Rules</code> that you want to include in the <code>WebACL</code>. For more information, see <a>CreateRule</a> and <a>UpdateRule</a>.</p> </li> <li> <p>Create a <code>WebACL</code>. See <a>CreateWebACL</a>.</p> </li> <li> <p>Use <code>GetChangeToken</code> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateWebACL</a> request.</p> </li> <li> <p>Submit an <code>UpdateWebACL</code> request to specify the <code>Rules</code> that you want to include in the <code>WebACL</code>, to specify the default action, and to associate the <code>WebACL</code> with a CloudFront distribution. </p> </li> </ol> <p>Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the <a>UpdateWebACL</a> request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist. </p> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateWebACL :: forall eff. UpdateWebACLRequest -> Aff (exception :: EXCEPTION | eff) UpdateWebACLResponse
updateWebACL = Request.request serviceName "updateWebACL" 


-- | <p>Inserts or deletes <a>XssMatchTuple</a> objects (filters) in an <a>XssMatchSet</a>. For each <code>XssMatchTuple</code> object, you specify the following values:</p> <ul> <li> <p> <code>Action</code>: Whether to insert the object into or delete the object from the array. To change a <code>XssMatchTuple</code>, you delete the existing object and add a new one.</p> </li> <li> <p> <code>FieldToMatch</code>: The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.</p> </li> <li> <p> <code>TextTransformation</code>: Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.</p> </li> </ul> <p>You use <code>XssMatchSet</code> objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an <code>XssMatchSet</code> with the applicable settings, and then configure AWS WAF to block the requests. </p> <p>To create and configure an <code>XssMatchSet</code>, perform the following steps:</p> <ol> <li> <p>Submit a <a>CreateXssMatchSet</a> request.</p> </li> <li> <p>Use <a>GetChangeToken</a> to get the change token that you provide in the <code>ChangeToken</code> parameter of an <a>UpdateIPSet</a> request.</p> </li> <li> <p>Submit an <code>UpdateXssMatchSet</code> request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.</p> </li> </ol> <p>For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF Developer Guide</a>.</p>
updateXssMatchSet :: forall eff. UpdateXssMatchSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateXssMatchSetResponse
updateXssMatchSet = Request.request serviceName "updateXssMatchSet" 


newtype Action = Action String
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ActivatedRule</code> object in an <a>UpdateWebACL</a> request specifies a <code>Rule</code> that you want to insert or delete, the priority of the <code>Rule</code> in the <code>WebACL</code>, and the action that you want AWS WAF to take when a web request matches the <code>Rule</code> (<code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>).</p> <p>To specify whether to insert or delete a <code>Rule</code>, use the <code>Action</code> parameter in the <a>WebACLUpdate</a> data type.</p>
newtype ActivatedRule = ActivatedRule 
  { "Priority" :: (RulePriority)
  , "RuleId" :: (ResourceId)
  , "Action" :: NullOrUndefined.NullOrUndefined (WafAction)
  , "OverrideAction" :: NullOrUndefined.NullOrUndefined (WafOverrideAction)
  , "Type" :: NullOrUndefined.NullOrUndefined (WafRuleType)
  }
derive instance newtypeActivatedRule :: Newtype ActivatedRule _
derive instance repGenericActivatedRule :: Generic ActivatedRule _
instance showActivatedRule :: Show ActivatedRule where
  show = genericShow
instance decodeActivatedRule :: Decode ActivatedRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivatedRule :: Encode ActivatedRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivatedRules = ActivatedRules (Array ActivatedRule)
derive instance newtypeActivatedRules :: Newtype ActivatedRules _
derive instance repGenericActivatedRules :: Generic ActivatedRules _
instance showActivatedRules :: Show ActivatedRules where
  show = genericShow
instance decodeActivatedRules :: Decode ActivatedRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivatedRules :: Encode ActivatedRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateWebACLRequest = AssociateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeAssociateWebACLRequest :: Newtype AssociateWebACLRequest _
derive instance repGenericAssociateWebACLRequest :: Generic AssociateWebACLRequest _
instance showAssociateWebACLRequest :: Show AssociateWebACLRequest where
  show = genericShow
instance decodeAssociateWebACLRequest :: Decode AssociateWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateWebACLRequest :: Encode AssociateWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateWebACLResponse = AssociateWebACLResponse Types.NoArguments
derive instance newtypeAssociateWebACLResponse :: Newtype AssociateWebACLResponse _
derive instance repGenericAssociateWebACLResponse :: Generic AssociateWebACLResponse _
instance showAssociateWebACLResponse :: Show AssociateWebACLResponse where
  show = genericShow
instance decodeAssociateWebACLResponse :: Decode AssociateWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateWebACLResponse :: Encode AssociateWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In a <a>GetByteMatchSet</a> request, <code>ByteMatchSet</code> is a complex type that contains the <code>ByteMatchSetId</code> and <code>Name</code> of a <code>ByteMatchSet</code>, and the values that you specified when you updated the <code>ByteMatchSet</code>. </p> <p>A complex type that contains <code>ByteMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>ByteMatchSet</code> contains more than one <code>ByteMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype ByteMatchSet = ByteMatchSet 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "ByteMatchTuples" :: (ByteMatchTuples)
  }
derive instance newtypeByteMatchSet :: Newtype ByteMatchSet _
derive instance repGenericByteMatchSet :: Generic ByteMatchSet _
instance showByteMatchSet :: Show ByteMatchSet where
  show = genericShow
instance decodeByteMatchSet :: Decode ByteMatchSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchSet :: Encode ByteMatchSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ByteMatchSetSummaries = ByteMatchSetSummaries (Array ByteMatchSetSummary)
derive instance newtypeByteMatchSetSummaries :: Newtype ByteMatchSetSummaries _
derive instance repGenericByteMatchSetSummaries :: Generic ByteMatchSetSummaries _
instance showByteMatchSetSummaries :: Show ByteMatchSetSummaries where
  show = genericShow
instance decodeByteMatchSetSummaries :: Decode ByteMatchSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchSetSummaries :: Encode ByteMatchSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned by <a>ListByteMatchSets</a>. Each <code>ByteMatchSetSummary</code> object includes the <code>Name</code> and <code>ByteMatchSetId</code> for one <a>ByteMatchSet</a>.</p>
newtype ByteMatchSetSummary = ByteMatchSetSummary 
  { "ByteMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeByteMatchSetSummary :: Newtype ByteMatchSetSummary _
derive instance repGenericByteMatchSetSummary :: Generic ByteMatchSetSummary _
instance showByteMatchSetSummary :: Show ByteMatchSetSummary where
  show = genericShow
instance decodeByteMatchSetSummary :: Decode ByteMatchSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchSetSummary :: Encode ByteMatchSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In an <a>UpdateByteMatchSet</a> request, <code>ByteMatchSetUpdate</code> specifies whether to insert or delete a <a>ByteMatchTuple</a> and includes the settings for the <code>ByteMatchTuple</code>.</p>
newtype ByteMatchSetUpdate = ByteMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "ByteMatchTuple" :: (ByteMatchTuple)
  }
derive instance newtypeByteMatchSetUpdate :: Newtype ByteMatchSetUpdate _
derive instance repGenericByteMatchSetUpdate :: Generic ByteMatchSetUpdate _
instance showByteMatchSetUpdate :: Show ByteMatchSetUpdate where
  show = genericShow
instance decodeByteMatchSetUpdate :: Decode ByteMatchSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchSetUpdate :: Encode ByteMatchSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ByteMatchSetUpdates = ByteMatchSetUpdates (Array ByteMatchSetUpdate)
derive instance newtypeByteMatchSetUpdates :: Newtype ByteMatchSetUpdates _
derive instance repGenericByteMatchSetUpdates :: Generic ByteMatchSetUpdates _
instance showByteMatchSetUpdates :: Show ByteMatchSetUpdates where
  show = genericShow
instance decodeByteMatchSetUpdates :: Decode ByteMatchSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchSetUpdates :: Encode ByteMatchSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ByteMatchTargetString = ByteMatchTargetString String
derive instance newtypeByteMatchTargetString :: Newtype ByteMatchTargetString _
derive instance repGenericByteMatchTargetString :: Generic ByteMatchTargetString _
instance showByteMatchTargetString :: Show ByteMatchTargetString where
  show = genericShow
instance decodeByteMatchTargetString :: Decode ByteMatchTargetString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchTargetString :: Encode ByteMatchTargetString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.</p>
newtype ByteMatchTuple = ByteMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TargetString" :: (ByteMatchTargetString)
  , "TextTransformation" :: (TextTransformation)
  , "PositionalConstraint" :: (PositionalConstraint)
  }
derive instance newtypeByteMatchTuple :: Newtype ByteMatchTuple _
derive instance repGenericByteMatchTuple :: Generic ByteMatchTuple _
instance showByteMatchTuple :: Show ByteMatchTuple where
  show = genericShow
instance decodeByteMatchTuple :: Decode ByteMatchTuple where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchTuple :: Encode ByteMatchTuple where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ByteMatchTuples = ByteMatchTuples (Array ByteMatchTuple)
derive instance newtypeByteMatchTuples :: Newtype ByteMatchTuples _
derive instance repGenericByteMatchTuples :: Generic ByteMatchTuples _
instance showByteMatchTuples :: Show ByteMatchTuples where
  show = genericShow
instance decodeByteMatchTuples :: Decode ByteMatchTuples where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteMatchTuples :: Encode ByteMatchTuples where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _
derive instance repGenericChangeAction :: Generic ChangeAction _
instance showChangeAction :: Show ChangeAction where
  show = genericShow
instance decodeChangeAction :: Decode ChangeAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeAction :: Encode ChangeAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeToken = ChangeToken String
derive instance newtypeChangeToken :: Newtype ChangeToken _
derive instance repGenericChangeToken :: Generic ChangeToken _
instance showChangeToken :: Show ChangeToken where
  show = genericShow
instance decodeChangeToken :: Decode ChangeToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeToken :: Encode ChangeToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeTokenStatus = ChangeTokenStatus String
derive instance newtypeChangeTokenStatus :: Newtype ChangeTokenStatus _
derive instance repGenericChangeTokenStatus :: Generic ChangeTokenStatus _
instance showChangeTokenStatus :: Show ChangeTokenStatus where
  show = genericShow
instance decodeChangeTokenStatus :: Decode ChangeTokenStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeTokenStatus :: Encode ChangeTokenStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _
derive instance repGenericComparisonOperator :: Generic ComparisonOperator _
instance showComparisonOperator :: Show ComparisonOperator where
  show = genericShow
instance decodeComparisonOperator :: Decode ComparisonOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperator :: Encode ComparisonOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Country = Country String
derive instance newtypeCountry :: Newtype Country _
derive instance repGenericCountry :: Generic Country _
instance showCountry :: Show Country where
  show = genericShow
instance decodeCountry :: Decode Country where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountry :: Encode Country where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateByteMatchSetRequest = CreateByteMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateByteMatchSetRequest :: Newtype CreateByteMatchSetRequest _
derive instance repGenericCreateByteMatchSetRequest :: Generic CreateByteMatchSetRequest _
instance showCreateByteMatchSetRequest :: Show CreateByteMatchSetRequest where
  show = genericShow
instance decodeCreateByteMatchSetRequest :: Decode CreateByteMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateByteMatchSetRequest :: Encode CreateByteMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateByteMatchSetResponse = CreateByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined.NullOrUndefined (ByteMatchSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateByteMatchSetResponse :: Newtype CreateByteMatchSetResponse _
derive instance repGenericCreateByteMatchSetResponse :: Generic CreateByteMatchSetResponse _
instance showCreateByteMatchSetResponse :: Show CreateByteMatchSetResponse where
  show = genericShow
instance decodeCreateByteMatchSetResponse :: Decode CreateByteMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateByteMatchSetResponse :: Encode CreateByteMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGeoMatchSetRequest = CreateGeoMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateGeoMatchSetRequest :: Newtype CreateGeoMatchSetRequest _
derive instance repGenericCreateGeoMatchSetRequest :: Generic CreateGeoMatchSetRequest _
instance showCreateGeoMatchSetRequest :: Show CreateGeoMatchSetRequest where
  show = genericShow
instance decodeCreateGeoMatchSetRequest :: Decode CreateGeoMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGeoMatchSetRequest :: Encode CreateGeoMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGeoMatchSetResponse = CreateGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined.NullOrUndefined (GeoMatchSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateGeoMatchSetResponse :: Newtype CreateGeoMatchSetResponse _
derive instance repGenericCreateGeoMatchSetResponse :: Generic CreateGeoMatchSetResponse _
instance showCreateGeoMatchSetResponse :: Show CreateGeoMatchSetResponse where
  show = genericShow
instance decodeCreateGeoMatchSetResponse :: Decode CreateGeoMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGeoMatchSetResponse :: Encode CreateGeoMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateIPSetRequest :: Newtype CreateIPSetRequest _
derive instance repGenericCreateIPSetRequest :: Generic CreateIPSetRequest _
instance showCreateIPSetRequest :: Show CreateIPSetRequest where
  show = genericShow
instance decodeCreateIPSetRequest :: Decode CreateIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIPSetRequest :: Encode CreateIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IPSet" :: NullOrUndefined.NullOrUndefined (IPSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateIPSetResponse :: Newtype CreateIPSetResponse _
derive instance repGenericCreateIPSetResponse :: Generic CreateIPSetResponse _
instance showCreateIPSetResponse :: Show CreateIPSetResponse where
  show = genericShow
instance decodeCreateIPSetResponse :: Decode CreateIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIPSetResponse :: Encode CreateIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRateBasedRuleRequest = CreateRateBasedRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRateBasedRuleRequest :: Newtype CreateRateBasedRuleRequest _
derive instance repGenericCreateRateBasedRuleRequest :: Generic CreateRateBasedRuleRequest _
instance showCreateRateBasedRuleRequest :: Show CreateRateBasedRuleRequest where
  show = genericShow
instance decodeCreateRateBasedRuleRequest :: Decode CreateRateBasedRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRateBasedRuleRequest :: Encode CreateRateBasedRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRateBasedRuleResponse = CreateRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined.NullOrUndefined (RateBasedRule)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRateBasedRuleResponse :: Newtype CreateRateBasedRuleResponse _
derive instance repGenericCreateRateBasedRuleResponse :: Generic CreateRateBasedRuleResponse _
instance showCreateRateBasedRuleResponse :: Show CreateRateBasedRuleResponse where
  show = genericShow
instance decodeCreateRateBasedRuleResponse :: Decode CreateRateBasedRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRateBasedRuleResponse :: Encode CreateRateBasedRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRegexMatchSetRequest = CreateRegexMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRegexMatchSetRequest :: Newtype CreateRegexMatchSetRequest _
derive instance repGenericCreateRegexMatchSetRequest :: Generic CreateRegexMatchSetRequest _
instance showCreateRegexMatchSetRequest :: Show CreateRegexMatchSetRequest where
  show = genericShow
instance decodeCreateRegexMatchSetRequest :: Decode CreateRegexMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRegexMatchSetRequest :: Encode CreateRegexMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRegexMatchSetResponse = CreateRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined.NullOrUndefined (RegexMatchSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRegexMatchSetResponse :: Newtype CreateRegexMatchSetResponse _
derive instance repGenericCreateRegexMatchSetResponse :: Generic CreateRegexMatchSetResponse _
instance showCreateRegexMatchSetResponse :: Show CreateRegexMatchSetResponse where
  show = genericShow
instance decodeCreateRegexMatchSetResponse :: Decode CreateRegexMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRegexMatchSetResponse :: Encode CreateRegexMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRegexPatternSetRequest = CreateRegexPatternSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRegexPatternSetRequest :: Newtype CreateRegexPatternSetRequest _
derive instance repGenericCreateRegexPatternSetRequest :: Generic CreateRegexPatternSetRequest _
instance showCreateRegexPatternSetRequest :: Show CreateRegexPatternSetRequest where
  show = genericShow
instance decodeCreateRegexPatternSetRequest :: Decode CreateRegexPatternSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRegexPatternSetRequest :: Encode CreateRegexPatternSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRegexPatternSetResponse = CreateRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined.NullOrUndefined (RegexPatternSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRegexPatternSetResponse :: Newtype CreateRegexPatternSetResponse _
derive instance repGenericCreateRegexPatternSetResponse :: Generic CreateRegexPatternSetResponse _
instance showCreateRegexPatternSetResponse :: Show CreateRegexPatternSetResponse where
  show = genericShow
instance decodeCreateRegexPatternSetResponse :: Decode CreateRegexPatternSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRegexPatternSetResponse :: Encode CreateRegexPatternSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleGroupRequest = CreateRuleGroupRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRuleGroupRequest :: Newtype CreateRuleGroupRequest _
derive instance repGenericCreateRuleGroupRequest :: Generic CreateRuleGroupRequest _
instance showCreateRuleGroupRequest :: Show CreateRuleGroupRequest where
  show = genericShow
instance decodeCreateRuleGroupRequest :: Decode CreateRuleGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleGroupRequest :: Encode CreateRuleGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleGroupResponse = CreateRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined.NullOrUndefined (RuleGroup)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRuleGroupResponse :: Newtype CreateRuleGroupResponse _
derive instance repGenericCreateRuleGroupResponse :: Generic CreateRuleGroupResponse _
instance showCreateRuleGroupResponse :: Show CreateRuleGroupResponse where
  show = genericShow
instance decodeCreateRuleGroupResponse :: Decode CreateRuleGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleGroupResponse :: Encode CreateRuleGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleRequest = CreateRuleRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateRuleRequest :: Newtype CreateRuleRequest _
derive instance repGenericCreateRuleRequest :: Generic CreateRuleRequest _
instance showCreateRuleRequest :: Show CreateRuleRequest where
  show = genericShow
instance decodeCreateRuleRequest :: Decode CreateRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleRequest :: Encode CreateRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleResponse = CreateRuleResponse 
  { "Rule" :: NullOrUndefined.NullOrUndefined (Rule)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateRuleResponse :: Newtype CreateRuleResponse _
derive instance repGenericCreateRuleResponse :: Generic CreateRuleResponse _
instance showCreateRuleResponse :: Show CreateRuleResponse where
  show = genericShow
instance decodeCreateRuleResponse :: Decode CreateRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleResponse :: Encode CreateRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSizeConstraintSetRequest = CreateSizeConstraintSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateSizeConstraintSetRequest :: Newtype CreateSizeConstraintSetRequest _
derive instance repGenericCreateSizeConstraintSetRequest :: Generic CreateSizeConstraintSetRequest _
instance showCreateSizeConstraintSetRequest :: Show CreateSizeConstraintSetRequest where
  show = genericShow
instance decodeCreateSizeConstraintSetRequest :: Decode CreateSizeConstraintSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSizeConstraintSetRequest :: Encode CreateSizeConstraintSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined.NullOrUndefined (SizeConstraintSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateSizeConstraintSetResponse :: Newtype CreateSizeConstraintSetResponse _
derive instance repGenericCreateSizeConstraintSetResponse :: Generic CreateSizeConstraintSetResponse _
instance showCreateSizeConstraintSetResponse :: Show CreateSizeConstraintSetResponse where
  show = genericShow
instance decodeCreateSizeConstraintSetResponse :: Decode CreateSizeConstraintSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSizeConstraintSetResponse :: Encode CreateSizeConstraintSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to create a <a>SqlInjectionMatchSet</a>.</p>
newtype CreateSqlInjectionMatchSetRequest = CreateSqlInjectionMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateSqlInjectionMatchSetRequest :: Newtype CreateSqlInjectionMatchSetRequest _
derive instance repGenericCreateSqlInjectionMatchSetRequest :: Generic CreateSqlInjectionMatchSetRequest _
instance showCreateSqlInjectionMatchSetRequest :: Show CreateSqlInjectionMatchSetRequest where
  show = genericShow
instance decodeCreateSqlInjectionMatchSetRequest :: Decode CreateSqlInjectionMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSqlInjectionMatchSetRequest :: Encode CreateSqlInjectionMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <code>CreateSqlInjectionMatchSet</code> request.</p>
newtype CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined.NullOrUndefined (SqlInjectionMatchSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateSqlInjectionMatchSetResponse :: Newtype CreateSqlInjectionMatchSetResponse _
derive instance repGenericCreateSqlInjectionMatchSetResponse :: Generic CreateSqlInjectionMatchSetResponse _
instance showCreateSqlInjectionMatchSetResponse :: Show CreateSqlInjectionMatchSetResponse where
  show = genericShow
instance decodeCreateSqlInjectionMatchSetResponse :: Decode CreateSqlInjectionMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSqlInjectionMatchSetResponse :: Encode CreateSqlInjectionMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWebACLRequest = CreateWebACLRequest 
  { "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  , "DefaultAction" :: (WafAction)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateWebACLRequest :: Newtype CreateWebACLRequest _
derive instance repGenericCreateWebACLRequest :: Generic CreateWebACLRequest _
instance showCreateWebACLRequest :: Show CreateWebACLRequest where
  show = genericShow
instance decodeCreateWebACLRequest :: Decode CreateWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWebACLRequest :: Encode CreateWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWebACLResponse = CreateWebACLResponse 
  { "WebACL" :: NullOrUndefined.NullOrUndefined (WebACL)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateWebACLResponse :: Newtype CreateWebACLResponse _
derive instance repGenericCreateWebACLResponse :: Generic CreateWebACLResponse _
instance showCreateWebACLResponse :: Show CreateWebACLResponse where
  show = genericShow
instance decodeCreateWebACLResponse :: Decode CreateWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWebACLResponse :: Encode CreateWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to create an <a>XssMatchSet</a>.</p>
newtype CreateXssMatchSetRequest = CreateXssMatchSetRequest 
  { "Name" :: (ResourceName)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeCreateXssMatchSetRequest :: Newtype CreateXssMatchSetRequest _
derive instance repGenericCreateXssMatchSetRequest :: Generic CreateXssMatchSetRequest _
instance showCreateXssMatchSetRequest :: Show CreateXssMatchSetRequest where
  show = genericShow
instance decodeCreateXssMatchSetRequest :: Decode CreateXssMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateXssMatchSetRequest :: Encode CreateXssMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <code>CreateXssMatchSet</code> request.</p>
newtype CreateXssMatchSetResponse = CreateXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined.NullOrUndefined (XssMatchSet)
  , "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeCreateXssMatchSetResponse :: Newtype CreateXssMatchSetResponse _
derive instance repGenericCreateXssMatchSetResponse :: Generic CreateXssMatchSetResponse _
instance showCreateXssMatchSetResponse :: Show CreateXssMatchSetResponse where
  show = genericShow
instance decodeCreateXssMatchSetResponse :: Decode CreateXssMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateXssMatchSetResponse :: Encode CreateXssMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteByteMatchSetRequest = DeleteByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteByteMatchSetRequest :: Newtype DeleteByteMatchSetRequest _
derive instance repGenericDeleteByteMatchSetRequest :: Generic DeleteByteMatchSetRequest _
instance showDeleteByteMatchSetRequest :: Show DeleteByteMatchSetRequest where
  show = genericShow
instance decodeDeleteByteMatchSetRequest :: Decode DeleteByteMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteByteMatchSetRequest :: Encode DeleteByteMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteByteMatchSetResponse = DeleteByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteByteMatchSetResponse :: Newtype DeleteByteMatchSetResponse _
derive instance repGenericDeleteByteMatchSetResponse :: Generic DeleteByteMatchSetResponse _
instance showDeleteByteMatchSetResponse :: Show DeleteByteMatchSetResponse where
  show = genericShow
instance decodeDeleteByteMatchSetResponse :: Decode DeleteByteMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteByteMatchSetResponse :: Encode DeleteByteMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGeoMatchSetRequest = DeleteGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteGeoMatchSetRequest :: Newtype DeleteGeoMatchSetRequest _
derive instance repGenericDeleteGeoMatchSetRequest :: Generic DeleteGeoMatchSetRequest _
instance showDeleteGeoMatchSetRequest :: Show DeleteGeoMatchSetRequest where
  show = genericShow
instance decodeDeleteGeoMatchSetRequest :: Decode DeleteGeoMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGeoMatchSetRequest :: Encode DeleteGeoMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteGeoMatchSetResponse :: Newtype DeleteGeoMatchSetResponse _
derive instance repGenericDeleteGeoMatchSetResponse :: Generic DeleteGeoMatchSetResponse _
instance showDeleteGeoMatchSetResponse :: Show DeleteGeoMatchSetResponse where
  show = genericShow
instance decodeDeleteGeoMatchSetResponse :: Decode DeleteGeoMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGeoMatchSetResponse :: Encode DeleteGeoMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteIPSetRequest :: Newtype DeleteIPSetRequest _
derive instance repGenericDeleteIPSetRequest :: Generic DeleteIPSetRequest _
instance showDeleteIPSetRequest :: Show DeleteIPSetRequest where
  show = genericShow
instance decodeDeleteIPSetRequest :: Decode DeleteIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIPSetRequest :: Encode DeleteIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIPSetResponse = DeleteIPSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteIPSetResponse :: Newtype DeleteIPSetResponse _
derive instance repGenericDeleteIPSetResponse :: Generic DeleteIPSetResponse _
instance showDeleteIPSetResponse :: Show DeleteIPSetResponse where
  show = genericShow
instance decodeDeleteIPSetResponse :: Decode DeleteIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIPSetResponse :: Encode DeleteIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePermissionPolicyRequest = DeletePermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeDeletePermissionPolicyRequest :: Newtype DeletePermissionPolicyRequest _
derive instance repGenericDeletePermissionPolicyRequest :: Generic DeletePermissionPolicyRequest _
instance showDeletePermissionPolicyRequest :: Show DeletePermissionPolicyRequest where
  show = genericShow
instance decodeDeletePermissionPolicyRequest :: Decode DeletePermissionPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePermissionPolicyRequest :: Encode DeletePermissionPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse Types.NoArguments
derive instance newtypeDeletePermissionPolicyResponse :: Newtype DeletePermissionPolicyResponse _
derive instance repGenericDeletePermissionPolicyResponse :: Generic DeletePermissionPolicyResponse _
instance showDeletePermissionPolicyResponse :: Show DeletePermissionPolicyResponse where
  show = genericShow
instance decodeDeletePermissionPolicyResponse :: Decode DeletePermissionPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePermissionPolicyResponse :: Encode DeletePermissionPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRateBasedRuleRequest = DeleteRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRateBasedRuleRequest :: Newtype DeleteRateBasedRuleRequest _
derive instance repGenericDeleteRateBasedRuleRequest :: Generic DeleteRateBasedRuleRequest _
instance showDeleteRateBasedRuleRequest :: Show DeleteRateBasedRuleRequest where
  show = genericShow
instance decodeDeleteRateBasedRuleRequest :: Decode DeleteRateBasedRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRateBasedRuleRequest :: Encode DeleteRateBasedRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRateBasedRuleResponse :: Newtype DeleteRateBasedRuleResponse _
derive instance repGenericDeleteRateBasedRuleResponse :: Generic DeleteRateBasedRuleResponse _
instance showDeleteRateBasedRuleResponse :: Show DeleteRateBasedRuleResponse where
  show = genericShow
instance decodeDeleteRateBasedRuleResponse :: Decode DeleteRateBasedRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRateBasedRuleResponse :: Encode DeleteRateBasedRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRegexMatchSetRequest = DeleteRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRegexMatchSetRequest :: Newtype DeleteRegexMatchSetRequest _
derive instance repGenericDeleteRegexMatchSetRequest :: Generic DeleteRegexMatchSetRequest _
instance showDeleteRegexMatchSetRequest :: Show DeleteRegexMatchSetRequest where
  show = genericShow
instance decodeDeleteRegexMatchSetRequest :: Decode DeleteRegexMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegexMatchSetRequest :: Encode DeleteRegexMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRegexMatchSetResponse :: Newtype DeleteRegexMatchSetResponse _
derive instance repGenericDeleteRegexMatchSetResponse :: Generic DeleteRegexMatchSetResponse _
instance showDeleteRegexMatchSetResponse :: Show DeleteRegexMatchSetResponse where
  show = genericShow
instance decodeDeleteRegexMatchSetResponse :: Decode DeleteRegexMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegexMatchSetResponse :: Encode DeleteRegexMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRegexPatternSetRequest = DeleteRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRegexPatternSetRequest :: Newtype DeleteRegexPatternSetRequest _
derive instance repGenericDeleteRegexPatternSetRequest :: Generic DeleteRegexPatternSetRequest _
instance showDeleteRegexPatternSetRequest :: Show DeleteRegexPatternSetRequest where
  show = genericShow
instance decodeDeleteRegexPatternSetRequest :: Decode DeleteRegexPatternSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegexPatternSetRequest :: Encode DeleteRegexPatternSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRegexPatternSetResponse :: Newtype DeleteRegexPatternSetResponse _
derive instance repGenericDeleteRegexPatternSetResponse :: Generic DeleteRegexPatternSetResponse _
instance showDeleteRegexPatternSetResponse :: Show DeleteRegexPatternSetResponse where
  show = genericShow
instance decodeDeleteRegexPatternSetResponse :: Decode DeleteRegexPatternSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRegexPatternSetResponse :: Encode DeleteRegexPatternSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleGroupRequest = DeleteRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRuleGroupRequest :: Newtype DeleteRuleGroupRequest _
derive instance repGenericDeleteRuleGroupRequest :: Generic DeleteRuleGroupRequest _
instance showDeleteRuleGroupRequest :: Show DeleteRuleGroupRequest where
  show = genericShow
instance decodeDeleteRuleGroupRequest :: Decode DeleteRuleGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleGroupRequest :: Encode DeleteRuleGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleGroupResponse = DeleteRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRuleGroupResponse :: Newtype DeleteRuleGroupResponse _
derive instance repGenericDeleteRuleGroupResponse :: Generic DeleteRuleGroupResponse _
instance showDeleteRuleGroupResponse :: Show DeleteRuleGroupResponse where
  show = genericShow
instance decodeDeleteRuleGroupResponse :: Decode DeleteRuleGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleGroupResponse :: Encode DeleteRuleGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteRuleRequest :: Newtype DeleteRuleRequest _
derive instance repGenericDeleteRuleRequest :: Generic DeleteRuleRequest _
instance showDeleteRuleRequest :: Show DeleteRuleRequest where
  show = genericShow
instance decodeDeleteRuleRequest :: Decode DeleteRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleRequest :: Encode DeleteRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleResponse = DeleteRuleResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteRuleResponse :: Newtype DeleteRuleResponse _
derive instance repGenericDeleteRuleResponse :: Generic DeleteRuleResponse _
instance showDeleteRuleResponse :: Show DeleteRuleResponse where
  show = genericShow
instance decodeDeleteRuleResponse :: Decode DeleteRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleResponse :: Encode DeleteRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSizeConstraintSetRequest = DeleteSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteSizeConstraintSetRequest :: Newtype DeleteSizeConstraintSetRequest _
derive instance repGenericDeleteSizeConstraintSetRequest :: Generic DeleteSizeConstraintSetRequest _
instance showDeleteSizeConstraintSetRequest :: Show DeleteSizeConstraintSetRequest where
  show = genericShow
instance decodeDeleteSizeConstraintSetRequest :: Decode DeleteSizeConstraintSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSizeConstraintSetRequest :: Encode DeleteSizeConstraintSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteSizeConstraintSetResponse :: Newtype DeleteSizeConstraintSetResponse _
derive instance repGenericDeleteSizeConstraintSetResponse :: Generic DeleteSizeConstraintSetResponse _
instance showDeleteSizeConstraintSetResponse :: Show DeleteSizeConstraintSetResponse where
  show = genericShow
instance decodeDeleteSizeConstraintSetResponse :: Decode DeleteSizeConstraintSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSizeConstraintSetResponse :: Encode DeleteSizeConstraintSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetRequest = DeleteSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteSqlInjectionMatchSetRequest :: Newtype DeleteSqlInjectionMatchSetRequest _
derive instance repGenericDeleteSqlInjectionMatchSetRequest :: Generic DeleteSqlInjectionMatchSetRequest _
instance showDeleteSqlInjectionMatchSetRequest :: Show DeleteSqlInjectionMatchSetRequest where
  show = genericShow
instance decodeDeleteSqlInjectionMatchSetRequest :: Decode DeleteSqlInjectionMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSqlInjectionMatchSetRequest :: Encode DeleteSqlInjectionMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a request to delete a <a>SqlInjectionMatchSet</a> from AWS WAF.</p>
newtype DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteSqlInjectionMatchSetResponse :: Newtype DeleteSqlInjectionMatchSetResponse _
derive instance repGenericDeleteSqlInjectionMatchSetResponse :: Generic DeleteSqlInjectionMatchSetResponse _
instance showDeleteSqlInjectionMatchSetResponse :: Show DeleteSqlInjectionMatchSetResponse where
  show = genericShow
instance decodeDeleteSqlInjectionMatchSetResponse :: Decode DeleteSqlInjectionMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSqlInjectionMatchSetResponse :: Encode DeleteSqlInjectionMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteWebACLRequest = DeleteWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteWebACLRequest :: Newtype DeleteWebACLRequest _
derive instance repGenericDeleteWebACLRequest :: Generic DeleteWebACLRequest _
instance showDeleteWebACLRequest :: Show DeleteWebACLRequest where
  show = genericShow
instance decodeDeleteWebACLRequest :: Decode DeleteWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteWebACLRequest :: Encode DeleteWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteWebACLResponse = DeleteWebACLResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteWebACLResponse :: Newtype DeleteWebACLResponse _
derive instance repGenericDeleteWebACLResponse :: Generic DeleteWebACLResponse _
instance showDeleteWebACLResponse :: Show DeleteWebACLResponse where
  show = genericShow
instance decodeDeleteWebACLResponse :: Decode DeleteWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteWebACLResponse :: Encode DeleteWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetRequest = DeleteXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeDeleteXssMatchSetRequest :: Newtype DeleteXssMatchSetRequest _
derive instance repGenericDeleteXssMatchSetRequest :: Generic DeleteXssMatchSetRequest _
instance showDeleteXssMatchSetRequest :: Show DeleteXssMatchSetRequest where
  show = genericShow
instance decodeDeleteXssMatchSetRequest :: Decode DeleteXssMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteXssMatchSetRequest :: Encode DeleteXssMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a request to delete an <a>XssMatchSet</a> from AWS WAF.</p>
newtype DeleteXssMatchSetResponse = DeleteXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeDeleteXssMatchSetResponse :: Newtype DeleteXssMatchSetResponse _
derive instance repGenericDeleteXssMatchSetResponse :: Generic DeleteXssMatchSetResponse _
instance showDeleteXssMatchSetResponse :: Show DeleteXssMatchSetResponse where
  show = genericShow
instance decodeDeleteXssMatchSetResponse :: Decode DeleteXssMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteXssMatchSetResponse :: Encode DeleteXssMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateWebACLRequest = DisassociateWebACLRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeDisassociateWebACLRequest :: Newtype DisassociateWebACLRequest _
derive instance repGenericDisassociateWebACLRequest :: Generic DisassociateWebACLRequest _
instance showDisassociateWebACLRequest :: Show DisassociateWebACLRequest where
  show = genericShow
instance decodeDisassociateWebACLRequest :: Decode DisassociateWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateWebACLRequest :: Encode DisassociateWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateWebACLResponse = DisassociateWebACLResponse Types.NoArguments
derive instance newtypeDisassociateWebACLResponse :: Newtype DisassociateWebACLResponse _
derive instance repGenericDisassociateWebACLResponse :: Generic DisassociateWebACLResponse _
instance showDisassociateWebACLResponse :: Show DisassociateWebACLResponse where
  show = genericShow
instance decodeDisassociateWebACLResponse :: Decode DisassociateWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateWebACLResponse :: Encode DisassociateWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies where in a web request to look for <code>TargetString</code>.</p>
newtype FieldToMatch = FieldToMatch 
  { "Type" :: (MatchFieldType)
  , "Data" :: NullOrUndefined.NullOrUndefined (MatchFieldData)
  }
derive instance newtypeFieldToMatch :: Newtype FieldToMatch _
derive instance repGenericFieldToMatch :: Generic FieldToMatch _
instance showFieldToMatch :: Show FieldToMatch where
  show = genericShow
instance decodeFieldToMatch :: Decode FieldToMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldToMatch :: Encode FieldToMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The country from which web requests originate that you want AWS WAF to search for.</p>
newtype GeoMatchConstraint = GeoMatchConstraint 
  { "Type" :: (GeoMatchConstraintType)
  , "Value" :: (GeoMatchConstraintValue)
  }
derive instance newtypeGeoMatchConstraint :: Newtype GeoMatchConstraint _
derive instance repGenericGeoMatchConstraint :: Generic GeoMatchConstraint _
instance showGeoMatchConstraint :: Show GeoMatchConstraint where
  show = genericShow
instance decodeGeoMatchConstraint :: Decode GeoMatchConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchConstraint :: Encode GeoMatchConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoMatchConstraintType = GeoMatchConstraintType String
derive instance newtypeGeoMatchConstraintType :: Newtype GeoMatchConstraintType _
derive instance repGenericGeoMatchConstraintType :: Generic GeoMatchConstraintType _
instance showGeoMatchConstraintType :: Show GeoMatchConstraintType where
  show = genericShow
instance decodeGeoMatchConstraintType :: Decode GeoMatchConstraintType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchConstraintType :: Encode GeoMatchConstraintType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoMatchConstraintValue = GeoMatchConstraintValue String
derive instance newtypeGeoMatchConstraintValue :: Newtype GeoMatchConstraintValue _
derive instance repGenericGeoMatchConstraintValue :: Generic GeoMatchConstraintValue _
instance showGeoMatchConstraintValue :: Show GeoMatchConstraintValue where
  show = genericShow
instance decodeGeoMatchConstraintValue :: Decode GeoMatchConstraintValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchConstraintValue :: Encode GeoMatchConstraintValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoMatchConstraints = GeoMatchConstraints (Array GeoMatchConstraint)
derive instance newtypeGeoMatchConstraints :: Newtype GeoMatchConstraints _
derive instance repGenericGeoMatchConstraints :: Generic GeoMatchConstraints _
instance showGeoMatchConstraints :: Show GeoMatchConstraints where
  show = genericShow
instance decodeGeoMatchConstraints :: Decode GeoMatchConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchConstraints :: Encode GeoMatchConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains one or more countries that AWS WAF will search for.</p>
newtype GeoMatchSet = GeoMatchSet 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "GeoMatchConstraints" :: (GeoMatchConstraints)
  }
derive instance newtypeGeoMatchSet :: Newtype GeoMatchSet _
derive instance repGenericGeoMatchSet :: Generic GeoMatchSet _
instance showGeoMatchSet :: Show GeoMatchSet where
  show = genericShow
instance decodeGeoMatchSet :: Decode GeoMatchSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchSet :: Encode GeoMatchSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoMatchSetSummaries = GeoMatchSetSummaries (Array GeoMatchSetSummary)
derive instance newtypeGeoMatchSetSummaries :: Newtype GeoMatchSetSummaries _
derive instance repGenericGeoMatchSetSummaries :: Generic GeoMatchSetSummaries _
instance showGeoMatchSetSummaries :: Show GeoMatchSetSummaries where
  show = genericShow
instance decodeGeoMatchSetSummaries :: Decode GeoMatchSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchSetSummaries :: Encode GeoMatchSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the identifier and the name of the <code>GeoMatchSet</code>.</p>
newtype GeoMatchSetSummary = GeoMatchSetSummary 
  { "GeoMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeGeoMatchSetSummary :: Newtype GeoMatchSetSummary _
derive instance repGenericGeoMatchSetSummary :: Generic GeoMatchSetSummary _
instance showGeoMatchSetSummary :: Show GeoMatchSetSummary where
  show = genericShow
instance decodeGeoMatchSetSummary :: Decode GeoMatchSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchSetSummary :: Encode GeoMatchSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the type of update to perform to an <a>GeoMatchSet</a> with <a>UpdateGeoMatchSet</a>.</p>
newtype GeoMatchSetUpdate = GeoMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "GeoMatchConstraint" :: (GeoMatchConstraint)
  }
derive instance newtypeGeoMatchSetUpdate :: Newtype GeoMatchSetUpdate _
derive instance repGenericGeoMatchSetUpdate :: Generic GeoMatchSetUpdate _
instance showGeoMatchSetUpdate :: Show GeoMatchSetUpdate where
  show = genericShow
instance decodeGeoMatchSetUpdate :: Decode GeoMatchSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchSetUpdate :: Encode GeoMatchSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoMatchSetUpdates = GeoMatchSetUpdates (Array GeoMatchSetUpdate)
derive instance newtypeGeoMatchSetUpdates :: Newtype GeoMatchSetUpdates _
derive instance repGenericGeoMatchSetUpdates :: Generic GeoMatchSetUpdates _
instance showGeoMatchSetUpdates :: Show GeoMatchSetUpdates where
  show = genericShow
instance decodeGeoMatchSetUpdates :: Decode GeoMatchSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoMatchSetUpdates :: Encode GeoMatchSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetByteMatchSetRequest = GetByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetByteMatchSetRequest :: Newtype GetByteMatchSetRequest _
derive instance repGenericGetByteMatchSetRequest :: Generic GetByteMatchSetRequest _
instance showGetByteMatchSetRequest :: Show GetByteMatchSetRequest where
  show = genericShow
instance decodeGetByteMatchSetRequest :: Decode GetByteMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetByteMatchSetRequest :: Encode GetByteMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetByteMatchSetResponse = GetByteMatchSetResponse 
  { "ByteMatchSet" :: NullOrUndefined.NullOrUndefined (ByteMatchSet)
  }
derive instance newtypeGetByteMatchSetResponse :: Newtype GetByteMatchSetResponse _
derive instance repGenericGetByteMatchSetResponse :: Generic GetByteMatchSetResponse _
instance showGetByteMatchSetResponse :: Show GetByteMatchSetResponse where
  show = genericShow
instance decodeGetByteMatchSetResponse :: Decode GetByteMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetByteMatchSetResponse :: Encode GetByteMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetChangeTokenRequest = GetChangeTokenRequest Types.NoArguments
derive instance newtypeGetChangeTokenRequest :: Newtype GetChangeTokenRequest _
derive instance repGenericGetChangeTokenRequest :: Generic GetChangeTokenRequest _
instance showGetChangeTokenRequest :: Show GetChangeTokenRequest where
  show = genericShow
instance decodeGetChangeTokenRequest :: Decode GetChangeTokenRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeTokenRequest :: Encode GetChangeTokenRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetChangeTokenResponse = GetChangeTokenResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeGetChangeTokenResponse :: Newtype GetChangeTokenResponse _
derive instance repGenericGetChangeTokenResponse :: Generic GetChangeTokenResponse _
instance showGetChangeTokenResponse :: Show GetChangeTokenResponse where
  show = genericShow
instance decodeGetChangeTokenResponse :: Decode GetChangeTokenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeTokenResponse :: Encode GetChangeTokenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetChangeTokenStatusRequest = GetChangeTokenStatusRequest 
  { "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeGetChangeTokenStatusRequest :: Newtype GetChangeTokenStatusRequest _
derive instance repGenericGetChangeTokenStatusRequest :: Generic GetChangeTokenStatusRequest _
instance showGetChangeTokenStatusRequest :: Show GetChangeTokenStatusRequest where
  show = genericShow
instance decodeGetChangeTokenStatusRequest :: Decode GetChangeTokenStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeTokenStatusRequest :: Encode GetChangeTokenStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetChangeTokenStatusResponse = GetChangeTokenStatusResponse 
  { "ChangeTokenStatus" :: NullOrUndefined.NullOrUndefined (ChangeTokenStatus)
  }
derive instance newtypeGetChangeTokenStatusResponse :: Newtype GetChangeTokenStatusResponse _
derive instance repGenericGetChangeTokenStatusResponse :: Generic GetChangeTokenStatusResponse _
instance showGetChangeTokenStatusResponse :: Show GetChangeTokenStatusResponse where
  show = genericShow
instance decodeGetChangeTokenStatusResponse :: Decode GetChangeTokenStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeTokenStatusResponse :: Encode GetChangeTokenStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGeoMatchSetRequest = GetGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetGeoMatchSetRequest :: Newtype GetGeoMatchSetRequest _
derive instance repGenericGetGeoMatchSetRequest :: Generic GetGeoMatchSetRequest _
instance showGetGeoMatchSetRequest :: Show GetGeoMatchSetRequest where
  show = genericShow
instance decodeGetGeoMatchSetRequest :: Decode GetGeoMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGeoMatchSetRequest :: Encode GetGeoMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGeoMatchSetResponse = GetGeoMatchSetResponse 
  { "GeoMatchSet" :: NullOrUndefined.NullOrUndefined (GeoMatchSet)
  }
derive instance newtypeGetGeoMatchSetResponse :: Newtype GetGeoMatchSetResponse _
derive instance repGenericGetGeoMatchSetResponse :: Generic GetGeoMatchSetResponse _
instance showGetGeoMatchSetResponse :: Show GetGeoMatchSetResponse where
  show = genericShow
instance decodeGetGeoMatchSetResponse :: Decode GetGeoMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGeoMatchSetResponse :: Encode GetGeoMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIPSetRequest = GetIPSetRequest 
  { "IPSetId" :: (ResourceId)
  }
derive instance newtypeGetIPSetRequest :: Newtype GetIPSetRequest _
derive instance repGenericGetIPSetRequest :: Generic GetIPSetRequest _
instance showGetIPSetRequest :: Show GetIPSetRequest where
  show = genericShow
instance decodeGetIPSetRequest :: Decode GetIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIPSetRequest :: Encode GetIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIPSetResponse = GetIPSetResponse 
  { "IPSet" :: NullOrUndefined.NullOrUndefined (IPSet)
  }
derive instance newtypeGetIPSetResponse :: Newtype GetIPSetResponse _
derive instance repGenericGetIPSetResponse :: Generic GetIPSetResponse _
instance showGetIPSetResponse :: Show GetIPSetResponse where
  show = genericShow
instance decodeGetIPSetResponse :: Decode GetIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIPSetResponse :: Encode GetIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPermissionPolicyRequest = GetPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeGetPermissionPolicyRequest :: Newtype GetPermissionPolicyRequest _
derive instance repGenericGetPermissionPolicyRequest :: Generic GetPermissionPolicyRequest _
instance showGetPermissionPolicyRequest :: Show GetPermissionPolicyRequest where
  show = genericShow
instance decodeGetPermissionPolicyRequest :: Decode GetPermissionPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPermissionPolicyRequest :: Encode GetPermissionPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPermissionPolicyResponse = GetPermissionPolicyResponse 
  { "Policy" :: NullOrUndefined.NullOrUndefined (PolicyString)
  }
derive instance newtypeGetPermissionPolicyResponse :: Newtype GetPermissionPolicyResponse _
derive instance repGenericGetPermissionPolicyResponse :: Generic GetPermissionPolicyResponse _
instance showGetPermissionPolicyResponse :: Show GetPermissionPolicyResponse where
  show = genericShow
instance decodeGetPermissionPolicyResponse :: Decode GetPermissionPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPermissionPolicyResponse :: Encode GetPermissionPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRateBasedRuleManagedKeysRequest = GetRateBasedRuleManagedKeysRequest 
  { "RuleId" :: (ResourceId)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  }
derive instance newtypeGetRateBasedRuleManagedKeysRequest :: Newtype GetRateBasedRuleManagedKeysRequest _
derive instance repGenericGetRateBasedRuleManagedKeysRequest :: Generic GetRateBasedRuleManagedKeysRequest _
instance showGetRateBasedRuleManagedKeysRequest :: Show GetRateBasedRuleManagedKeysRequest where
  show = genericShow
instance decodeGetRateBasedRuleManagedKeysRequest :: Decode GetRateBasedRuleManagedKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRateBasedRuleManagedKeysRequest :: Encode GetRateBasedRuleManagedKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRateBasedRuleManagedKeysResponse = GetRateBasedRuleManagedKeysResponse 
  { "ManagedKeys" :: NullOrUndefined.NullOrUndefined (ManagedKeys)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  }
derive instance newtypeGetRateBasedRuleManagedKeysResponse :: Newtype GetRateBasedRuleManagedKeysResponse _
derive instance repGenericGetRateBasedRuleManagedKeysResponse :: Generic GetRateBasedRuleManagedKeysResponse _
instance showGetRateBasedRuleManagedKeysResponse :: Show GetRateBasedRuleManagedKeysResponse where
  show = genericShow
instance decodeGetRateBasedRuleManagedKeysResponse :: Decode GetRateBasedRuleManagedKeysResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRateBasedRuleManagedKeysResponse :: Encode GetRateBasedRuleManagedKeysResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRateBasedRuleRequest = GetRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  }
derive instance newtypeGetRateBasedRuleRequest :: Newtype GetRateBasedRuleRequest _
derive instance repGenericGetRateBasedRuleRequest :: Generic GetRateBasedRuleRequest _
instance showGetRateBasedRuleRequest :: Show GetRateBasedRuleRequest where
  show = genericShow
instance decodeGetRateBasedRuleRequest :: Decode GetRateBasedRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRateBasedRuleRequest :: Encode GetRateBasedRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRateBasedRuleResponse = GetRateBasedRuleResponse 
  { "Rule" :: NullOrUndefined.NullOrUndefined (RateBasedRule)
  }
derive instance newtypeGetRateBasedRuleResponse :: Newtype GetRateBasedRuleResponse _
derive instance repGenericGetRateBasedRuleResponse :: Generic GetRateBasedRuleResponse _
instance showGetRateBasedRuleResponse :: Show GetRateBasedRuleResponse where
  show = genericShow
instance decodeGetRateBasedRuleResponse :: Decode GetRateBasedRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRateBasedRuleResponse :: Encode GetRateBasedRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegexMatchSetRequest = GetRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetRegexMatchSetRequest :: Newtype GetRegexMatchSetRequest _
derive instance repGenericGetRegexMatchSetRequest :: Generic GetRegexMatchSetRequest _
instance showGetRegexMatchSetRequest :: Show GetRegexMatchSetRequest where
  show = genericShow
instance decodeGetRegexMatchSetRequest :: Decode GetRegexMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegexMatchSetRequest :: Encode GetRegexMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegexMatchSetResponse = GetRegexMatchSetResponse 
  { "RegexMatchSet" :: NullOrUndefined.NullOrUndefined (RegexMatchSet)
  }
derive instance newtypeGetRegexMatchSetResponse :: Newtype GetRegexMatchSetResponse _
derive instance repGenericGetRegexMatchSetResponse :: Generic GetRegexMatchSetResponse _
instance showGetRegexMatchSetResponse :: Show GetRegexMatchSetResponse where
  show = genericShow
instance decodeGetRegexMatchSetResponse :: Decode GetRegexMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegexMatchSetResponse :: Encode GetRegexMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegexPatternSetRequest = GetRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  }
derive instance newtypeGetRegexPatternSetRequest :: Newtype GetRegexPatternSetRequest _
derive instance repGenericGetRegexPatternSetRequest :: Generic GetRegexPatternSetRequest _
instance showGetRegexPatternSetRequest :: Show GetRegexPatternSetRequest where
  show = genericShow
instance decodeGetRegexPatternSetRequest :: Decode GetRegexPatternSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegexPatternSetRequest :: Encode GetRegexPatternSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRegexPatternSetResponse = GetRegexPatternSetResponse 
  { "RegexPatternSet" :: NullOrUndefined.NullOrUndefined (RegexPatternSet)
  }
derive instance newtypeGetRegexPatternSetResponse :: Newtype GetRegexPatternSetResponse _
derive instance repGenericGetRegexPatternSetResponse :: Generic GetRegexPatternSetResponse _
instance showGetRegexPatternSetResponse :: Show GetRegexPatternSetResponse where
  show = genericShow
instance decodeGetRegexPatternSetResponse :: Decode GetRegexPatternSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRegexPatternSetResponse :: Encode GetRegexPatternSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRuleGroupRequest = GetRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  }
derive instance newtypeGetRuleGroupRequest :: Newtype GetRuleGroupRequest _
derive instance repGenericGetRuleGroupRequest :: Generic GetRuleGroupRequest _
instance showGetRuleGroupRequest :: Show GetRuleGroupRequest where
  show = genericShow
instance decodeGetRuleGroupRequest :: Decode GetRuleGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRuleGroupRequest :: Encode GetRuleGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRuleGroupResponse = GetRuleGroupResponse 
  { "RuleGroup" :: NullOrUndefined.NullOrUndefined (RuleGroup)
  }
derive instance newtypeGetRuleGroupResponse :: Newtype GetRuleGroupResponse _
derive instance repGenericGetRuleGroupResponse :: Generic GetRuleGroupResponse _
instance showGetRuleGroupResponse :: Show GetRuleGroupResponse where
  show = genericShow
instance decodeGetRuleGroupResponse :: Decode GetRuleGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRuleGroupResponse :: Encode GetRuleGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRuleRequest = GetRuleRequest 
  { "RuleId" :: (ResourceId)
  }
derive instance newtypeGetRuleRequest :: Newtype GetRuleRequest _
derive instance repGenericGetRuleRequest :: Generic GetRuleRequest _
instance showGetRuleRequest :: Show GetRuleRequest where
  show = genericShow
instance decodeGetRuleRequest :: Decode GetRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRuleRequest :: Encode GetRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetRuleResponse = GetRuleResponse 
  { "Rule" :: NullOrUndefined.NullOrUndefined (Rule)
  }
derive instance newtypeGetRuleResponse :: Newtype GetRuleResponse _
derive instance repGenericGetRuleResponse :: Generic GetRuleResponse _
instance showGetRuleResponse :: Show GetRuleResponse where
  show = genericShow
instance decodeGetRuleResponse :: Decode GetRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRuleResponse :: Encode GetRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSampledRequestsMaxItems = GetSampledRequestsMaxItems Number
derive instance newtypeGetSampledRequestsMaxItems :: Newtype GetSampledRequestsMaxItems _
derive instance repGenericGetSampledRequestsMaxItems :: Generic GetSampledRequestsMaxItems _
instance showGetSampledRequestsMaxItems :: Show GetSampledRequestsMaxItems where
  show = genericShow
instance decodeGetSampledRequestsMaxItems :: Decode GetSampledRequestsMaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSampledRequestsMaxItems :: Encode GetSampledRequestsMaxItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSampledRequestsRequest = GetSampledRequestsRequest 
  { "WebAclId" :: (ResourceId)
  , "RuleId" :: (ResourceId)
  , "TimeWindow" :: (TimeWindow)
  , "MaxItems" :: (GetSampledRequestsMaxItems)
  }
derive instance newtypeGetSampledRequestsRequest :: Newtype GetSampledRequestsRequest _
derive instance repGenericGetSampledRequestsRequest :: Generic GetSampledRequestsRequest _
instance showGetSampledRequestsRequest :: Show GetSampledRequestsRequest where
  show = genericShow
instance decodeGetSampledRequestsRequest :: Decode GetSampledRequestsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSampledRequestsRequest :: Encode GetSampledRequestsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSampledRequestsResponse = GetSampledRequestsResponse 
  { "SampledRequests" :: NullOrUndefined.NullOrUndefined (SampledHTTPRequests)
  , "PopulationSize" :: NullOrUndefined.NullOrUndefined (PopulationSize)
  , "TimeWindow" :: NullOrUndefined.NullOrUndefined (TimeWindow)
  }
derive instance newtypeGetSampledRequestsResponse :: Newtype GetSampledRequestsResponse _
derive instance repGenericGetSampledRequestsResponse :: Generic GetSampledRequestsResponse _
instance showGetSampledRequestsResponse :: Show GetSampledRequestsResponse where
  show = genericShow
instance decodeGetSampledRequestsResponse :: Decode GetSampledRequestsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSampledRequestsResponse :: Encode GetSampledRequestsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSizeConstraintSetRequest = GetSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  }
derive instance newtypeGetSizeConstraintSetRequest :: Newtype GetSizeConstraintSetRequest _
derive instance repGenericGetSizeConstraintSetRequest :: Generic GetSizeConstraintSetRequest _
instance showGetSizeConstraintSetRequest :: Show GetSizeConstraintSetRequest where
  show = genericShow
instance decodeGetSizeConstraintSetRequest :: Decode GetSizeConstraintSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSizeConstraintSetRequest :: Encode GetSizeConstraintSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSizeConstraintSetResponse = GetSizeConstraintSetResponse 
  { "SizeConstraintSet" :: NullOrUndefined.NullOrUndefined (SizeConstraintSet)
  }
derive instance newtypeGetSizeConstraintSetResponse :: Newtype GetSizeConstraintSetResponse _
derive instance repGenericGetSizeConstraintSetResponse :: Generic GetSizeConstraintSetResponse _
instance showGetSizeConstraintSetResponse :: Show GetSizeConstraintSetResponse where
  show = genericShow
instance decodeGetSizeConstraintSetResponse :: Decode GetSizeConstraintSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSizeConstraintSetResponse :: Encode GetSizeConstraintSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get a <a>SqlInjectionMatchSet</a>.</p>
newtype GetSqlInjectionMatchSetRequest = GetSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetSqlInjectionMatchSetRequest :: Newtype GetSqlInjectionMatchSetRequest _
derive instance repGenericGetSqlInjectionMatchSetRequest :: Generic GetSqlInjectionMatchSetRequest _
instance showGetSqlInjectionMatchSetRequest :: Show GetSqlInjectionMatchSetRequest where
  show = genericShow
instance decodeGetSqlInjectionMatchSetRequest :: Decode GetSqlInjectionMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSqlInjectionMatchSetRequest :: Encode GetSqlInjectionMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <a>GetSqlInjectionMatchSet</a> request.</p>
newtype GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse 
  { "SqlInjectionMatchSet" :: NullOrUndefined.NullOrUndefined (SqlInjectionMatchSet)
  }
derive instance newtypeGetSqlInjectionMatchSetResponse :: Newtype GetSqlInjectionMatchSetResponse _
derive instance repGenericGetSqlInjectionMatchSetResponse :: Generic GetSqlInjectionMatchSetResponse _
instance showGetSqlInjectionMatchSetResponse :: Show GetSqlInjectionMatchSetResponse where
  show = genericShow
instance decodeGetSqlInjectionMatchSetResponse :: Decode GetSqlInjectionMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSqlInjectionMatchSetResponse :: Encode GetSqlInjectionMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetWebACLForResourceRequest = GetWebACLForResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeGetWebACLForResourceRequest :: Newtype GetWebACLForResourceRequest _
derive instance repGenericGetWebACLForResourceRequest :: Generic GetWebACLForResourceRequest _
instance showGetWebACLForResourceRequest :: Show GetWebACLForResourceRequest where
  show = genericShow
instance decodeGetWebACLForResourceRequest :: Decode GetWebACLForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetWebACLForResourceRequest :: Encode GetWebACLForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetWebACLForResourceResponse = GetWebACLForResourceResponse 
  { "WebACLSummary" :: NullOrUndefined.NullOrUndefined (WebACLSummary)
  }
derive instance newtypeGetWebACLForResourceResponse :: Newtype GetWebACLForResourceResponse _
derive instance repGenericGetWebACLForResourceResponse :: Generic GetWebACLForResourceResponse _
instance showGetWebACLForResourceResponse :: Show GetWebACLForResourceResponse where
  show = genericShow
instance decodeGetWebACLForResourceResponse :: Decode GetWebACLForResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetWebACLForResourceResponse :: Encode GetWebACLForResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetWebACLRequest = GetWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }
derive instance newtypeGetWebACLRequest :: Newtype GetWebACLRequest _
derive instance repGenericGetWebACLRequest :: Generic GetWebACLRequest _
instance showGetWebACLRequest :: Show GetWebACLRequest where
  show = genericShow
instance decodeGetWebACLRequest :: Decode GetWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetWebACLRequest :: Encode GetWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetWebACLResponse = GetWebACLResponse 
  { "WebACL" :: NullOrUndefined.NullOrUndefined (WebACL)
  }
derive instance newtypeGetWebACLResponse :: Newtype GetWebACLResponse _
derive instance repGenericGetWebACLResponse :: Generic GetWebACLResponse _
instance showGetWebACLResponse :: Show GetWebACLResponse where
  show = genericShow
instance decodeGetWebACLResponse :: Decode GetWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetWebACLResponse :: Encode GetWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get an <a>XssMatchSet</a>.</p>
newtype GetXssMatchSetRequest = GetXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  }
derive instance newtypeGetXssMatchSetRequest :: Newtype GetXssMatchSetRequest _
derive instance repGenericGetXssMatchSetRequest :: Generic GetXssMatchSetRequest _
instance showGetXssMatchSetRequest :: Show GetXssMatchSetRequest where
  show = genericShow
instance decodeGetXssMatchSetRequest :: Decode GetXssMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetXssMatchSetRequest :: Encode GetXssMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <a>GetXssMatchSet</a> request.</p>
newtype GetXssMatchSetResponse = GetXssMatchSetResponse 
  { "XssMatchSet" :: NullOrUndefined.NullOrUndefined (XssMatchSet)
  }
derive instance newtypeGetXssMatchSetResponse :: Newtype GetXssMatchSetResponse _
derive instance repGenericGetXssMatchSetResponse :: Generic GetXssMatchSetResponse _
instance showGetXssMatchSetResponse :: Show GetXssMatchSetResponse where
  show = genericShow
instance decodeGetXssMatchSetResponse :: Decode GetXssMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetXssMatchSetResponse :: Encode GetXssMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPHeader</code> complex type that appears as <code>Headers</code> in the response syntax. <code>HTTPHeader</code> contains the names and values of all of the headers that appear in one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPHeader = HTTPHeader 
  { "Name" :: NullOrUndefined.NullOrUndefined (HeaderName)
  , "Value" :: NullOrUndefined.NullOrUndefined (HeaderValue)
  }
derive instance newtypeHTTPHeader :: Newtype HTTPHeader _
derive instance repGenericHTTPHeader :: Generic HTTPHeader _
instance showHTTPHeader :: Show HTTPHeader where
  show = genericShow
instance decodeHTTPHeader :: Decode HTTPHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHTTPHeader :: Encode HTTPHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HTTPHeaders = HTTPHeaders (Array HTTPHeader)
derive instance newtypeHTTPHeaders :: Newtype HTTPHeaders _
derive instance repGenericHTTPHeaders :: Generic HTTPHeaders _
instance showHTTPHeaders :: Show HTTPHeaders where
  show = genericShow
instance decodeHTTPHeaders :: Decode HTTPHeaders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHTTPHeaders :: Encode HTTPHeaders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HTTPMethod = HTTPMethod String
derive instance newtypeHTTPMethod :: Newtype HTTPMethod _
derive instance repGenericHTTPMethod :: Generic HTTPMethod _
instance showHTTPMethod :: Show HTTPMethod where
  show = genericShow
instance decodeHTTPMethod :: Decode HTTPMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHTTPMethod :: Encode HTTPMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from a <a>GetSampledRequests</a> request includes an <code>HTTPRequest</code> complex type that appears as <code>Request</code> in the response syntax. <code>HTTPRequest</code> contains information about one of the web requests that were returned by <code>GetSampledRequests</code>. </p>
newtype HTTPRequest = HTTPRequest 
  { "ClientIP" :: NullOrUndefined.NullOrUndefined (IPString)
  , "Country" :: NullOrUndefined.NullOrUndefined (Country)
  , "URI" :: NullOrUndefined.NullOrUndefined (URIString)
  , "Method" :: NullOrUndefined.NullOrUndefined (HTTPMethod)
  , "HTTPVersion" :: NullOrUndefined.NullOrUndefined (HTTPVersion)
  , "Headers" :: NullOrUndefined.NullOrUndefined (HTTPHeaders)
  }
derive instance newtypeHTTPRequest :: Newtype HTTPRequest _
derive instance repGenericHTTPRequest :: Generic HTTPRequest _
instance showHTTPRequest :: Show HTTPRequest where
  show = genericShow
instance decodeHTTPRequest :: Decode HTTPRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHTTPRequest :: Encode HTTPRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HTTPVersion = HTTPVersion String
derive instance newtypeHTTPVersion :: Newtype HTTPVersion _
derive instance repGenericHTTPVersion :: Generic HTTPVersion _
instance showHTTPVersion :: Show HTTPVersion where
  show = genericShow
instance decodeHTTPVersion :: Decode HTTPVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHTTPVersion :: Encode HTTPVersion where
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


-- | <p>Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6.</p> <p>To specify an individual IP address, you specify the four-part IP address followed by a <code>/32</code>, for example, 192.0.2.0/31. To block a range of IP addresses, you can specify a <code>/128</code>, <code>/64</code>, <code>/56</code>, <code>/48</code>, <code>/32</code>, <code>/24</code>, <code>/16</code>, or <code>/8</code> CIDR. For more information about CIDR notation, see the Wikipedia entry <a href="https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing">Classless Inter-Domain Routing</a>. </p>
newtype IPSet = IPSet 
  { "IPSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "IPSetDescriptors" :: (IPSetDescriptors)
  }
derive instance newtypeIPSet :: Newtype IPSet _
derive instance repGenericIPSet :: Generic IPSet _
instance showIPSet :: Show IPSet where
  show = genericShow
instance decodeIPSet :: Decode IPSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSet :: Encode IPSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the IP address type (<code>IPV4</code> or <code>IPV6</code>) and the IP address range (in CIDR format) that web requests originate from.</p>
newtype IPSetDescriptor = IPSetDescriptor 
  { "Type" :: (IPSetDescriptorType)
  , "Value" :: (IPSetDescriptorValue)
  }
derive instance newtypeIPSetDescriptor :: Newtype IPSetDescriptor _
derive instance repGenericIPSetDescriptor :: Generic IPSetDescriptor _
instance showIPSetDescriptor :: Show IPSetDescriptor where
  show = genericShow
instance decodeIPSetDescriptor :: Decode IPSetDescriptor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetDescriptor :: Encode IPSetDescriptor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPSetDescriptorType = IPSetDescriptorType String
derive instance newtypeIPSetDescriptorType :: Newtype IPSetDescriptorType _
derive instance repGenericIPSetDescriptorType :: Generic IPSetDescriptorType _
instance showIPSetDescriptorType :: Show IPSetDescriptorType where
  show = genericShow
instance decodeIPSetDescriptorType :: Decode IPSetDescriptorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetDescriptorType :: Encode IPSetDescriptorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPSetDescriptorValue = IPSetDescriptorValue String
derive instance newtypeIPSetDescriptorValue :: Newtype IPSetDescriptorValue _
derive instance repGenericIPSetDescriptorValue :: Generic IPSetDescriptorValue _
instance showIPSetDescriptorValue :: Show IPSetDescriptorValue where
  show = genericShow
instance decodeIPSetDescriptorValue :: Decode IPSetDescriptorValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetDescriptorValue :: Encode IPSetDescriptorValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPSetDescriptors = IPSetDescriptors (Array IPSetDescriptor)
derive instance newtypeIPSetDescriptors :: Newtype IPSetDescriptors _
derive instance repGenericIPSetDescriptors :: Generic IPSetDescriptors _
instance showIPSetDescriptors :: Show IPSetDescriptors where
  show = genericShow
instance decodeIPSetDescriptors :: Decode IPSetDescriptors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetDescriptors :: Encode IPSetDescriptors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPSetSummaries = IPSetSummaries (Array IPSetSummary)
derive instance newtypeIPSetSummaries :: Newtype IPSetSummaries _
derive instance repGenericIPSetSummaries :: Generic IPSetSummaries _
instance showIPSetSummaries :: Show IPSetSummaries where
  show = genericShow
instance decodeIPSetSummaries :: Decode IPSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetSummaries :: Encode IPSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the identifier and the name of the <code>IPSet</code>.</p>
newtype IPSetSummary = IPSetSummary 
  { "IPSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeIPSetSummary :: Newtype IPSetSummary _
derive instance repGenericIPSetSummary :: Generic IPSetSummary _
instance showIPSetSummary :: Show IPSetSummary where
  show = genericShow
instance decodeIPSetSummary :: Decode IPSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetSummary :: Encode IPSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the type of update to perform to an <a>IPSet</a> with <a>UpdateIPSet</a>.</p>
newtype IPSetUpdate = IPSetUpdate 
  { "Action" :: (ChangeAction)
  , "IPSetDescriptor" :: (IPSetDescriptor)
  }
derive instance newtypeIPSetUpdate :: Newtype IPSetUpdate _
derive instance repGenericIPSetUpdate :: Generic IPSetUpdate _
instance showIPSetUpdate :: Show IPSetUpdate where
  show = genericShow
instance decodeIPSetUpdate :: Decode IPSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetUpdate :: Encode IPSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPSetUpdates = IPSetUpdates (Array IPSetUpdate)
derive instance newtypeIPSetUpdates :: Newtype IPSetUpdates _
derive instance repGenericIPSetUpdates :: Generic IPSetUpdates _
instance showIPSetUpdates :: Show IPSetUpdates where
  show = genericShow
instance decodeIPSetUpdates :: Decode IPSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPSetUpdates :: Encode IPSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPString = IPString String
derive instance newtypeIPString :: Newtype IPString _
derive instance repGenericIPString :: Generic IPString _
instance showIPString :: Show IPString where
  show = genericShow
instance decodeIPString :: Decode IPString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPString :: Encode IPString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListActivatedRulesInRuleGroupRequest = ListActivatedRulesInRuleGroupRequest 
  { "RuleGroupId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListActivatedRulesInRuleGroupRequest :: Newtype ListActivatedRulesInRuleGroupRequest _
derive instance repGenericListActivatedRulesInRuleGroupRequest :: Generic ListActivatedRulesInRuleGroupRequest _
instance showListActivatedRulesInRuleGroupRequest :: Show ListActivatedRulesInRuleGroupRequest where
  show = genericShow
instance decodeListActivatedRulesInRuleGroupRequest :: Decode ListActivatedRulesInRuleGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActivatedRulesInRuleGroupRequest :: Encode ListActivatedRulesInRuleGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "ActivatedRules" :: NullOrUndefined.NullOrUndefined (ActivatedRules)
  }
derive instance newtypeListActivatedRulesInRuleGroupResponse :: Newtype ListActivatedRulesInRuleGroupResponse _
derive instance repGenericListActivatedRulesInRuleGroupResponse :: Generic ListActivatedRulesInRuleGroupResponse _
instance showListActivatedRulesInRuleGroupResponse :: Show ListActivatedRulesInRuleGroupResponse where
  show = genericShow
instance decodeListActivatedRulesInRuleGroupResponse :: Decode ListActivatedRulesInRuleGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActivatedRulesInRuleGroupResponse :: Encode ListActivatedRulesInRuleGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListByteMatchSetsRequest = ListByteMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListByteMatchSetsRequest :: Newtype ListByteMatchSetsRequest _
derive instance repGenericListByteMatchSetsRequest :: Generic ListByteMatchSetsRequest _
instance showListByteMatchSetsRequest :: Show ListByteMatchSetsRequest where
  show = genericShow
instance decodeListByteMatchSetsRequest :: Decode ListByteMatchSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListByteMatchSetsRequest :: Encode ListByteMatchSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListByteMatchSetsResponse = ListByteMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "ByteMatchSets" :: NullOrUndefined.NullOrUndefined (ByteMatchSetSummaries)
  }
derive instance newtypeListByteMatchSetsResponse :: Newtype ListByteMatchSetsResponse _
derive instance repGenericListByteMatchSetsResponse :: Generic ListByteMatchSetsResponse _
instance showListByteMatchSetsResponse :: Show ListByteMatchSetsResponse where
  show = genericShow
instance decodeListByteMatchSetsResponse :: Decode ListByteMatchSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListByteMatchSetsResponse :: Encode ListByteMatchSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGeoMatchSetsRequest = ListGeoMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListGeoMatchSetsRequest :: Newtype ListGeoMatchSetsRequest _
derive instance repGenericListGeoMatchSetsRequest :: Generic ListGeoMatchSetsRequest _
instance showListGeoMatchSetsRequest :: Show ListGeoMatchSetsRequest where
  show = genericShow
instance decodeListGeoMatchSetsRequest :: Decode ListGeoMatchSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGeoMatchSetsRequest :: Encode ListGeoMatchSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGeoMatchSetsResponse = ListGeoMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "GeoMatchSets" :: NullOrUndefined.NullOrUndefined (GeoMatchSetSummaries)
  }
derive instance newtypeListGeoMatchSetsResponse :: Newtype ListGeoMatchSetsResponse _
derive instance repGenericListGeoMatchSetsResponse :: Generic ListGeoMatchSetsResponse _
instance showListGeoMatchSetsResponse :: Show ListGeoMatchSetsResponse where
  show = genericShow
instance decodeListGeoMatchSetsResponse :: Decode ListGeoMatchSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGeoMatchSetsResponse :: Encode ListGeoMatchSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListIPSetsRequest :: Newtype ListIPSetsRequest _
derive instance repGenericListIPSetsRequest :: Generic ListIPSetsRequest _
instance showListIPSetsRequest :: Show ListIPSetsRequest where
  show = genericShow
instance decodeListIPSetsRequest :: Decode ListIPSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIPSetsRequest :: Encode ListIPSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "IPSets" :: NullOrUndefined.NullOrUndefined (IPSetSummaries)
  }
derive instance newtypeListIPSetsResponse :: Newtype ListIPSetsResponse _
derive instance repGenericListIPSetsResponse :: Generic ListIPSetsResponse _
instance showListIPSetsResponse :: Show ListIPSetsResponse where
  show = genericShow
instance decodeListIPSetsResponse :: Decode ListIPSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIPSetsResponse :: Encode ListIPSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRateBasedRulesRequest = ListRateBasedRulesRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRateBasedRulesRequest :: Newtype ListRateBasedRulesRequest _
derive instance repGenericListRateBasedRulesRequest :: Generic ListRateBasedRulesRequest _
instance showListRateBasedRulesRequest :: Show ListRateBasedRulesRequest where
  show = genericShow
instance decodeListRateBasedRulesRequest :: Decode ListRateBasedRulesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRateBasedRulesRequest :: Encode ListRateBasedRulesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRateBasedRulesResponse = ListRateBasedRulesResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined.NullOrUndefined (RuleSummaries)
  }
derive instance newtypeListRateBasedRulesResponse :: Newtype ListRateBasedRulesResponse _
derive instance repGenericListRateBasedRulesResponse :: Generic ListRateBasedRulesResponse _
instance showListRateBasedRulesResponse :: Show ListRateBasedRulesResponse where
  show = genericShow
instance decodeListRateBasedRulesResponse :: Decode ListRateBasedRulesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRateBasedRulesResponse :: Encode ListRateBasedRulesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRegexMatchSetsRequest = ListRegexMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRegexMatchSetsRequest :: Newtype ListRegexMatchSetsRequest _
derive instance repGenericListRegexMatchSetsRequest :: Generic ListRegexMatchSetsRequest _
instance showListRegexMatchSetsRequest :: Show ListRegexMatchSetsRequest where
  show = genericShow
instance decodeListRegexMatchSetsRequest :: Decode ListRegexMatchSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRegexMatchSetsRequest :: Encode ListRegexMatchSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRegexMatchSetsResponse = ListRegexMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "RegexMatchSets" :: NullOrUndefined.NullOrUndefined (RegexMatchSetSummaries)
  }
derive instance newtypeListRegexMatchSetsResponse :: Newtype ListRegexMatchSetsResponse _
derive instance repGenericListRegexMatchSetsResponse :: Generic ListRegexMatchSetsResponse _
instance showListRegexMatchSetsResponse :: Show ListRegexMatchSetsResponse where
  show = genericShow
instance decodeListRegexMatchSetsResponse :: Decode ListRegexMatchSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRegexMatchSetsResponse :: Encode ListRegexMatchSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRegexPatternSetsRequest = ListRegexPatternSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRegexPatternSetsRequest :: Newtype ListRegexPatternSetsRequest _
derive instance repGenericListRegexPatternSetsRequest :: Generic ListRegexPatternSetsRequest _
instance showListRegexPatternSetsRequest :: Show ListRegexPatternSetsRequest where
  show = genericShow
instance decodeListRegexPatternSetsRequest :: Decode ListRegexPatternSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRegexPatternSetsRequest :: Encode ListRegexPatternSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRegexPatternSetsResponse = ListRegexPatternSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "RegexPatternSets" :: NullOrUndefined.NullOrUndefined (RegexPatternSetSummaries)
  }
derive instance newtypeListRegexPatternSetsResponse :: Newtype ListRegexPatternSetsResponse _
derive instance repGenericListRegexPatternSetsResponse :: Generic ListRegexPatternSetsResponse _
instance showListRegexPatternSetsResponse :: Show ListRegexPatternSetsResponse where
  show = genericShow
instance decodeListRegexPatternSetsResponse :: Decode ListRegexPatternSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRegexPatternSetsResponse :: Encode ListRegexPatternSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourcesForWebACLRequest = ListResourcesForWebACLRequest 
  { "WebACLId" :: (ResourceId)
  }
derive instance newtypeListResourcesForWebACLRequest :: Newtype ListResourcesForWebACLRequest _
derive instance repGenericListResourcesForWebACLRequest :: Generic ListResourcesForWebACLRequest _
instance showListResourcesForWebACLRequest :: Show ListResourcesForWebACLRequest where
  show = genericShow
instance decodeListResourcesForWebACLRequest :: Decode ListResourcesForWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourcesForWebACLRequest :: Encode ListResourcesForWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourcesForWebACLResponse = ListResourcesForWebACLResponse 
  { "ResourceArns" :: NullOrUndefined.NullOrUndefined (ResourceArns)
  }
derive instance newtypeListResourcesForWebACLResponse :: Newtype ListResourcesForWebACLResponse _
derive instance repGenericListResourcesForWebACLResponse :: Generic ListResourcesForWebACLResponse _
instance showListResourcesForWebACLResponse :: Show ListResourcesForWebACLResponse where
  show = genericShow
instance decodeListResourcesForWebACLResponse :: Decode ListResourcesForWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourcesForWebACLResponse :: Encode ListResourcesForWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRuleGroupsRequest = ListRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRuleGroupsRequest :: Newtype ListRuleGroupsRequest _
derive instance repGenericListRuleGroupsRequest :: Generic ListRuleGroupsRequest _
instance showListRuleGroupsRequest :: Show ListRuleGroupsRequest where
  show = genericShow
instance decodeListRuleGroupsRequest :: Decode ListRuleGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRuleGroupsRequest :: Encode ListRuleGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRuleGroupsResponse = ListRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined.NullOrUndefined (RuleGroupSummaries)
  }
derive instance newtypeListRuleGroupsResponse :: Newtype ListRuleGroupsResponse _
derive instance repGenericListRuleGroupsResponse :: Generic ListRuleGroupsResponse _
instance showListRuleGroupsResponse :: Show ListRuleGroupsResponse where
  show = genericShow
instance decodeListRuleGroupsResponse :: Decode ListRuleGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRuleGroupsResponse :: Encode ListRuleGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRulesRequest = ListRulesRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListRulesRequest :: Newtype ListRulesRequest _
derive instance repGenericListRulesRequest :: Generic ListRulesRequest _
instance showListRulesRequest :: Show ListRulesRequest where
  show = genericShow
instance decodeListRulesRequest :: Decode ListRulesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRulesRequest :: Encode ListRulesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRulesResponse = ListRulesResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Rules" :: NullOrUndefined.NullOrUndefined (RuleSummaries)
  }
derive instance newtypeListRulesResponse :: Newtype ListRulesResponse _
derive instance repGenericListRulesResponse :: Generic ListRulesResponse _
instance showListRulesResponse :: Show ListRulesResponse where
  show = genericShow
instance decodeListRulesResponse :: Decode ListRulesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRulesResponse :: Encode ListRulesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSizeConstraintSetsRequest = ListSizeConstraintSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSizeConstraintSetsRequest :: Newtype ListSizeConstraintSetsRequest _
derive instance repGenericListSizeConstraintSetsRequest :: Generic ListSizeConstraintSetsRequest _
instance showListSizeConstraintSetsRequest :: Show ListSizeConstraintSetsRequest where
  show = genericShow
instance decodeListSizeConstraintSetsRequest :: Decode ListSizeConstraintSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSizeConstraintSetsRequest :: Encode ListSizeConstraintSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "SizeConstraintSets" :: NullOrUndefined.NullOrUndefined (SizeConstraintSetSummaries)
  }
derive instance newtypeListSizeConstraintSetsResponse :: Newtype ListSizeConstraintSetsResponse _
derive instance repGenericListSizeConstraintSetsResponse :: Generic ListSizeConstraintSetsResponse _
instance showListSizeConstraintSetsResponse :: Show ListSizeConstraintSetsResponse where
  show = genericShow
instance decodeListSizeConstraintSetsResponse :: Decode ListSizeConstraintSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSizeConstraintSetsResponse :: Encode ListSizeConstraintSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to list the <a>SqlInjectionMatchSet</a> objects created by the current AWS account.</p>
newtype ListSqlInjectionMatchSetsRequest = ListSqlInjectionMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSqlInjectionMatchSetsRequest :: Newtype ListSqlInjectionMatchSetsRequest _
derive instance repGenericListSqlInjectionMatchSetsRequest :: Generic ListSqlInjectionMatchSetsRequest _
instance showListSqlInjectionMatchSetsRequest :: Show ListSqlInjectionMatchSetsRequest where
  show = genericShow
instance decodeListSqlInjectionMatchSetsRequest :: Decode ListSqlInjectionMatchSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSqlInjectionMatchSetsRequest :: Encode ListSqlInjectionMatchSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <a>ListSqlInjectionMatchSets</a> request.</p>
newtype ListSqlInjectionMatchSetsResponse = ListSqlInjectionMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "SqlInjectionMatchSets" :: NullOrUndefined.NullOrUndefined (SqlInjectionMatchSetSummaries)
  }
derive instance newtypeListSqlInjectionMatchSetsResponse :: Newtype ListSqlInjectionMatchSetsResponse _
derive instance repGenericListSqlInjectionMatchSetsResponse :: Generic ListSqlInjectionMatchSetsResponse _
instance showListSqlInjectionMatchSetsResponse :: Show ListSqlInjectionMatchSetsResponse where
  show = genericShow
instance decodeListSqlInjectionMatchSetsResponse :: Decode ListSqlInjectionMatchSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSqlInjectionMatchSetsResponse :: Encode ListSqlInjectionMatchSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscribedRuleGroupsRequest = ListSubscribedRuleGroupsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListSubscribedRuleGroupsRequest :: Newtype ListSubscribedRuleGroupsRequest _
derive instance repGenericListSubscribedRuleGroupsRequest :: Generic ListSubscribedRuleGroupsRequest _
instance showListSubscribedRuleGroupsRequest :: Show ListSubscribedRuleGroupsRequest where
  show = genericShow
instance decodeListSubscribedRuleGroupsRequest :: Decode ListSubscribedRuleGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscribedRuleGroupsRequest :: Encode ListSubscribedRuleGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "RuleGroups" :: NullOrUndefined.NullOrUndefined (SubscribedRuleGroupSummaries)
  }
derive instance newtypeListSubscribedRuleGroupsResponse :: Newtype ListSubscribedRuleGroupsResponse _
derive instance repGenericListSubscribedRuleGroupsResponse :: Generic ListSubscribedRuleGroupsResponse _
instance showListSubscribedRuleGroupsResponse :: Show ListSubscribedRuleGroupsResponse where
  show = genericShow
instance decodeListSubscribedRuleGroupsResponse :: Decode ListSubscribedRuleGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscribedRuleGroupsResponse :: Encode ListSubscribedRuleGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListWebACLsRequest = ListWebACLsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListWebACLsRequest :: Newtype ListWebACLsRequest _
derive instance repGenericListWebACLsRequest :: Generic ListWebACLsRequest _
instance showListWebACLsRequest :: Show ListWebACLsRequest where
  show = genericShow
instance decodeListWebACLsRequest :: Decode ListWebACLsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListWebACLsRequest :: Encode ListWebACLsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListWebACLsResponse = ListWebACLsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "WebACLs" :: NullOrUndefined.NullOrUndefined (WebACLSummaries)
  }
derive instance newtypeListWebACLsResponse :: Newtype ListWebACLsResponse _
derive instance repGenericListWebACLsResponse :: Generic ListWebACLsResponse _
instance showListWebACLsResponse :: Show ListWebACLsResponse where
  show = genericShow
instance decodeListWebACLsResponse :: Decode ListWebACLsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListWebACLsResponse :: Encode ListWebACLsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to list the <a>XssMatchSet</a> objects created by the current AWS account.</p>
newtype ListXssMatchSetsRequest = ListXssMatchSetsRequest 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PaginationLimit)
  }
derive instance newtypeListXssMatchSetsRequest :: Newtype ListXssMatchSetsRequest _
derive instance repGenericListXssMatchSetsRequest :: Generic ListXssMatchSetsRequest _
instance showListXssMatchSetsRequest :: Show ListXssMatchSetsRequest where
  show = genericShow
instance decodeListXssMatchSetsRequest :: Decode ListXssMatchSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListXssMatchSetsRequest :: Encode ListXssMatchSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a <a>ListXssMatchSets</a> request.</p>
newtype ListXssMatchSetsResponse = ListXssMatchSetsResponse 
  { "NextMarker" :: NullOrUndefined.NullOrUndefined (NextMarker)
  , "XssMatchSets" :: NullOrUndefined.NullOrUndefined (XssMatchSetSummaries)
  }
derive instance newtypeListXssMatchSetsResponse :: Newtype ListXssMatchSetsResponse _
derive instance repGenericListXssMatchSetsResponse :: Generic ListXssMatchSetsResponse _
instance showListXssMatchSetsResponse :: Show ListXssMatchSetsResponse where
  show = genericShow
instance decodeListXssMatchSetsResponse :: Decode ListXssMatchSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListXssMatchSetsResponse :: Encode ListXssMatchSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManagedKey = ManagedKey String
derive instance newtypeManagedKey :: Newtype ManagedKey _
derive instance repGenericManagedKey :: Generic ManagedKey _
instance showManagedKey :: Show ManagedKey where
  show = genericShow
instance decodeManagedKey :: Decode ManagedKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedKey :: Encode ManagedKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManagedKeys = ManagedKeys (Array ManagedKey)
derive instance newtypeManagedKeys :: Newtype ManagedKeys _
derive instance repGenericManagedKeys :: Generic ManagedKeys _
instance showManagedKeys :: Show ManagedKeys where
  show = genericShow
instance decodeManagedKeys :: Decode ManagedKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedKeys :: Encode ManagedKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchFieldData = MatchFieldData String
derive instance newtypeMatchFieldData :: Newtype MatchFieldData _
derive instance repGenericMatchFieldData :: Generic MatchFieldData _
instance showMatchFieldData :: Show MatchFieldData where
  show = genericShow
instance decodeMatchFieldData :: Decode MatchFieldData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchFieldData :: Encode MatchFieldData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchFieldType = MatchFieldType String
derive instance newtypeMatchFieldType :: Newtype MatchFieldType _
derive instance repGenericMatchFieldType :: Generic MatchFieldType _
instance showMatchFieldType :: Show MatchFieldType where
  show = genericShow
instance decodeMatchFieldType :: Decode MatchFieldType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchFieldType :: Encode MatchFieldType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Negated = Negated Boolean
derive instance newtypeNegated :: Newtype Negated _
derive instance repGenericNegated :: Generic Negated _
instance showNegated :: Show Negated where
  show = genericShow
instance decodeNegated :: Decode Negated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNegated :: Encode Negated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextMarker = NextMarker String
derive instance newtypeNextMarker :: Newtype NextMarker _
derive instance repGenericNextMarker :: Generic NextMarker _
instance showNextMarker :: Show NextMarker where
  show = genericShow
instance decodeNextMarker :: Decode NextMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextMarker :: Encode NextMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationLimit = PaginationLimit Int
derive instance newtypePaginationLimit :: Newtype PaginationLimit _
derive instance repGenericPaginationLimit :: Generic PaginationLimit _
instance showPaginationLimit :: Show PaginationLimit where
  show = genericShow
instance decodePaginationLimit :: Decode PaginationLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationLimit :: Encode PaginationLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterExceptionField = ParameterExceptionField String
derive instance newtypeParameterExceptionField :: Newtype ParameterExceptionField _
derive instance repGenericParameterExceptionField :: Generic ParameterExceptionField _
instance showParameterExceptionField :: Show ParameterExceptionField where
  show = genericShow
instance decodeParameterExceptionField :: Decode ParameterExceptionField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterExceptionField :: Encode ParameterExceptionField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterExceptionParameter = ParameterExceptionParameter String
derive instance newtypeParameterExceptionParameter :: Newtype ParameterExceptionParameter _
derive instance repGenericParameterExceptionParameter :: Generic ParameterExceptionParameter _
instance showParameterExceptionParameter :: Show ParameterExceptionParameter where
  show = genericShow
instance decodeParameterExceptionParameter :: Decode ParameterExceptionParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterExceptionParameter :: Encode ParameterExceptionParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterExceptionReason = ParameterExceptionReason String
derive instance newtypeParameterExceptionReason :: Newtype ParameterExceptionReason _
derive instance repGenericParameterExceptionReason :: Generic ParameterExceptionReason _
instance showParameterExceptionReason :: Show ParameterExceptionReason where
  show = genericShow
instance decodeParameterExceptionReason :: Decode ParameterExceptionReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterExceptionReason :: Encode ParameterExceptionReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyString = PolicyString String
derive instance newtypePolicyString :: Newtype PolicyString _
derive instance repGenericPolicyString :: Generic PolicyString _
instance showPolicyString :: Show PolicyString where
  show = genericShow
instance decodePolicyString :: Decode PolicyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyString :: Encode PolicyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PopulationSize = PopulationSize Number
derive instance newtypePopulationSize :: Newtype PopulationSize _
derive instance repGenericPopulationSize :: Generic PopulationSize _
instance showPopulationSize :: Show PopulationSize where
  show = genericShow
instance decodePopulationSize :: Decode PopulationSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePopulationSize :: Encode PopulationSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositionalConstraint = PositionalConstraint String
derive instance newtypePositionalConstraint :: Newtype PositionalConstraint _
derive instance repGenericPositionalConstraint :: Generic PositionalConstraint _
instance showPositionalConstraint :: Show PositionalConstraint where
  show = genericShow
instance decodePositionalConstraint :: Decode PositionalConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositionalConstraint :: Encode PositionalConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the <a>ByteMatchSet</a>, <a>IPSet</a>, <a>SqlInjectionMatchSet</a>, <a>XssMatchSet</a>, <a>RegexMatchSet</a>, <a>GeoMatchSet</a>, and <a>SizeConstraintSet</a> objects that you want to add to a <code>Rule</code> and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44. </p>
newtype Predicate = Predicate 
  { "Negated" :: (Negated)
  , "Type" :: (PredicateType)
  , "DataId" :: (ResourceId)
  }
derive instance newtypePredicate :: Newtype Predicate _
derive instance repGenericPredicate :: Generic Predicate _
instance showPredicate :: Show Predicate where
  show = genericShow
instance decodePredicate :: Decode Predicate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredicate :: Encode Predicate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PredicateType = PredicateType String
derive instance newtypePredicateType :: Newtype PredicateType _
derive instance repGenericPredicateType :: Generic PredicateType _
instance showPredicateType :: Show PredicateType where
  show = genericShow
instance decodePredicateType :: Decode PredicateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredicateType :: Encode PredicateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Predicates = Predicates (Array Predicate)
derive instance newtypePredicates :: Newtype Predicates _
derive instance repGenericPredicates :: Generic Predicates _
instance showPredicates :: Show Predicates where
  show = genericShow
instance decodePredicates :: Decode Predicates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredicates :: Encode Predicates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutPermissionPolicyRequest = PutPermissionPolicyRequest 
  { "ResourceArn" :: (ResourceArn)
  , "Policy" :: (PolicyString)
  }
derive instance newtypePutPermissionPolicyRequest :: Newtype PutPermissionPolicyRequest _
derive instance repGenericPutPermissionPolicyRequest :: Generic PutPermissionPolicyRequest _
instance showPutPermissionPolicyRequest :: Show PutPermissionPolicyRequest where
  show = genericShow
instance decodePutPermissionPolicyRequest :: Decode PutPermissionPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutPermissionPolicyRequest :: Encode PutPermissionPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse Types.NoArguments
derive instance newtypePutPermissionPolicyResponse :: Newtype PutPermissionPolicyResponse _
derive instance repGenericPutPermissionPolicyResponse :: Generic PutPermissionPolicyResponse _
instance showPutPermissionPolicyResponse :: Show PutPermissionPolicyResponse where
  show = genericShow
instance decodePutPermissionPolicyResponse :: Decode PutPermissionPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutPermissionPolicyResponse :: Encode PutPermissionPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A <code>RateBasedRule</code> is identical to a regular <a>Rule</a>, with one addition: a <code>RateBasedRule</code> counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a <code>RateBasedRule</code> that includes the following conditions: </p> <ul> <li> <p>The requests come from 192.0.2.44.</p> </li> <li> <p>They contain the value <code>BadBot</code> in the <code>User-Agent</code> header.</p> </li> </ul> <p>In the rule, you also define the rate limit as 15,000.</p> <p>Requests that meet both of these conditions and exceed 15,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.</p>
newtype RateBasedRule = RateBasedRule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "MatchPredicates" :: (Predicates)
  , "RateKey" :: (RateKey)
  , "RateLimit" :: (RateLimit)
  }
derive instance newtypeRateBasedRule :: Newtype RateBasedRule _
derive instance repGenericRateBasedRule :: Generic RateBasedRule _
instance showRateBasedRule :: Show RateBasedRule where
  show = genericShow
instance decodeRateBasedRule :: Decode RateBasedRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRateBasedRule :: Encode RateBasedRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RateKey = RateKey String
derive instance newtypeRateKey :: Newtype RateKey _
derive instance repGenericRateKey :: Generic RateKey _
instance showRateKey :: Show RateKey where
  show = genericShow
instance decodeRateKey :: Decode RateKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRateKey :: Encode RateKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RateLimit = RateLimit Number
derive instance newtypeRateLimit :: Newtype RateLimit _
derive instance repGenericRateLimit :: Generic RateLimit _
instance showRateLimit :: Show RateLimit where
  show = genericShow
instance decodeRateLimit :: Decode RateLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRateLimit :: Encode RateLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In a <a>GetRegexMatchSet</a> request, <code>RegexMatchSet</code> is a complex type that contains the <code>RegexMatchSetId</code> and <code>Name</code> of a <code>RegexMatchSet</code>, and the values that you specified when you updated the <code>RegexMatchSet</code>.</p> <p> The values are contained in a <code>RegexMatchTuple</code> object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a <code>RegexMatchSet</code> contains more than one <code>RegexMatchTuple</code> object, a request needs to match the settings in only one <code>ByteMatchTuple</code> to be considered a match.</p>
newtype RegexMatchSet = RegexMatchSet 
  { "RegexMatchSetId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "RegexMatchTuples" :: NullOrUndefined.NullOrUndefined (RegexMatchTuples)
  }
derive instance newtypeRegexMatchSet :: Newtype RegexMatchSet _
derive instance repGenericRegexMatchSet :: Generic RegexMatchSet _
instance showRegexMatchSet :: Show RegexMatchSet where
  show = genericShow
instance decodeRegexMatchSet :: Decode RegexMatchSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchSet :: Encode RegexMatchSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexMatchSetSummaries = RegexMatchSetSummaries (Array RegexMatchSetSummary)
derive instance newtypeRegexMatchSetSummaries :: Newtype RegexMatchSetSummaries _
derive instance repGenericRegexMatchSetSummaries :: Generic RegexMatchSetSummaries _
instance showRegexMatchSetSummaries :: Show RegexMatchSetSummaries where
  show = genericShow
instance decodeRegexMatchSetSummaries :: Decode RegexMatchSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchSetSummaries :: Encode RegexMatchSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned by <a>ListRegexMatchSets</a>. Each <code>RegexMatchSetSummary</code> object includes the <code>Name</code> and <code>RegexMatchSetId</code> for one <a>RegexMatchSet</a>.</p>
newtype RegexMatchSetSummary = RegexMatchSetSummary 
  { "RegexMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRegexMatchSetSummary :: Newtype RegexMatchSetSummary _
derive instance repGenericRegexMatchSetSummary :: Generic RegexMatchSetSummary _
instance showRegexMatchSetSummary :: Show RegexMatchSetSummary where
  show = genericShow
instance decodeRegexMatchSetSummary :: Decode RegexMatchSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchSetSummary :: Encode RegexMatchSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In an <a>UpdateRegexMatchSet</a> request, <code>RegexMatchSetUpdate</code> specifies whether to insert or delete a <a>RegexMatchTuple</a> and includes the settings for the <code>RegexMatchTuple</code>.</p>
newtype RegexMatchSetUpdate = RegexMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexMatchTuple" :: (RegexMatchTuple)
  }
derive instance newtypeRegexMatchSetUpdate :: Newtype RegexMatchSetUpdate _
derive instance repGenericRegexMatchSetUpdate :: Generic RegexMatchSetUpdate _
instance showRegexMatchSetUpdate :: Show RegexMatchSetUpdate where
  show = genericShow
instance decodeRegexMatchSetUpdate :: Decode RegexMatchSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchSetUpdate :: Encode RegexMatchSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexMatchSetUpdates = RegexMatchSetUpdates (Array RegexMatchSetUpdate)
derive instance newtypeRegexMatchSetUpdates :: Newtype RegexMatchSetUpdates _
derive instance repGenericRegexMatchSetUpdates :: Generic RegexMatchSetUpdates _
instance showRegexMatchSetUpdates :: Show RegexMatchSetUpdates where
  show = genericShow
instance decodeRegexMatchSetUpdates :: Decode RegexMatchSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchSetUpdates :: Encode RegexMatchSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The regular expression pattern that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings. Each <code>RegexMatchTuple</code> object contains: </p> <ul> <li> <p>The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the <code>User-Agent</code> header. </p> </li> <li> <p>The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see <a>RegexPatternSet</a>. </p> </li> <li> <p>Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.</p> </li> </ul>
newtype RegexMatchTuple = RegexMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "RegexPatternSetId" :: (ResourceId)
  }
derive instance newtypeRegexMatchTuple :: Newtype RegexMatchTuple _
derive instance repGenericRegexMatchTuple :: Generic RegexMatchTuple _
instance showRegexMatchTuple :: Show RegexMatchTuple where
  show = genericShow
instance decodeRegexMatchTuple :: Decode RegexMatchTuple where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchTuple :: Encode RegexMatchTuple where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexMatchTuples = RegexMatchTuples (Array RegexMatchTuple)
derive instance newtypeRegexMatchTuples :: Newtype RegexMatchTuples _
derive instance repGenericRegexMatchTuples :: Generic RegexMatchTuples _
instance showRegexMatchTuples :: Show RegexMatchTuples where
  show = genericShow
instance decodeRegexMatchTuples :: Decode RegexMatchTuples where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexMatchTuples :: Encode RegexMatchTuples where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>RegexPatternSet</code> specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as <code>B[a@]dB[o0]t</code>. You can then configure AWS WAF to reject those requests.</p>
newtype RegexPatternSet = RegexPatternSet 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "RegexPatternStrings" :: (RegexPatternStrings)
  }
derive instance newtypeRegexPatternSet :: Newtype RegexPatternSet _
derive instance repGenericRegexPatternSet :: Generic RegexPatternSet _
instance showRegexPatternSet :: Show RegexPatternSet where
  show = genericShow
instance decodeRegexPatternSet :: Decode RegexPatternSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternSet :: Encode RegexPatternSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexPatternSetSummaries = RegexPatternSetSummaries (Array RegexPatternSetSummary)
derive instance newtypeRegexPatternSetSummaries :: Newtype RegexPatternSetSummaries _
derive instance repGenericRegexPatternSetSummaries :: Generic RegexPatternSetSummaries _
instance showRegexPatternSetSummaries :: Show RegexPatternSetSummaries where
  show = genericShow
instance decodeRegexPatternSetSummaries :: Decode RegexPatternSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternSetSummaries :: Encode RegexPatternSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned by <a>ListRegexPatternSets</a>. Each <code>RegexPatternSetSummary</code> object includes the <code>Name</code> and <code>RegexPatternSetId</code> for one <a>RegexPatternSet</a>.</p>
newtype RegexPatternSetSummary = RegexPatternSetSummary 
  { "RegexPatternSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRegexPatternSetSummary :: Newtype RegexPatternSetSummary _
derive instance repGenericRegexPatternSetSummary :: Generic RegexPatternSetSummary _
instance showRegexPatternSetSummary :: Show RegexPatternSetSummary where
  show = genericShow
instance decodeRegexPatternSetSummary :: Decode RegexPatternSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternSetSummary :: Encode RegexPatternSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In an <a>UpdateRegexPatternSet</a> request, <code>RegexPatternSetUpdate</code> specifies whether to insert or delete a <code>RegexPatternString</code> and includes the settings for the <code>RegexPatternString</code>.</p>
newtype RegexPatternSetUpdate = RegexPatternSetUpdate 
  { "Action" :: (ChangeAction)
  , "RegexPatternString" :: (RegexPatternString)
  }
derive instance newtypeRegexPatternSetUpdate :: Newtype RegexPatternSetUpdate _
derive instance repGenericRegexPatternSetUpdate :: Generic RegexPatternSetUpdate _
instance showRegexPatternSetUpdate :: Show RegexPatternSetUpdate where
  show = genericShow
instance decodeRegexPatternSetUpdate :: Decode RegexPatternSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternSetUpdate :: Encode RegexPatternSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexPatternSetUpdates = RegexPatternSetUpdates (Array RegexPatternSetUpdate)
derive instance newtypeRegexPatternSetUpdates :: Newtype RegexPatternSetUpdates _
derive instance repGenericRegexPatternSetUpdates :: Generic RegexPatternSetUpdates _
instance showRegexPatternSetUpdates :: Show RegexPatternSetUpdates where
  show = genericShow
instance decodeRegexPatternSetUpdates :: Decode RegexPatternSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternSetUpdates :: Encode RegexPatternSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexPatternString = RegexPatternString String
derive instance newtypeRegexPatternString :: Newtype RegexPatternString _
derive instance repGenericRegexPatternString :: Generic RegexPatternString _
instance showRegexPatternString :: Show RegexPatternString where
  show = genericShow
instance decodeRegexPatternString :: Decode RegexPatternString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternString :: Encode RegexPatternString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexPatternStrings = RegexPatternStrings (Array RegexPatternString)
derive instance newtypeRegexPatternStrings :: Newtype RegexPatternStrings _
derive instance repGenericRegexPatternStrings :: Generic RegexPatternStrings _
instance showRegexPatternStrings :: Show RegexPatternStrings where
  show = genericShow
instance decodeRegexPatternStrings :: Decode RegexPatternStrings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPatternStrings :: Encode RegexPatternStrings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArns = ResourceArns (Array ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _
derive instance repGenericResourceArns :: Generic ResourceArns _
instance showResourceArns :: Show ResourceArns where
  show = genericShow
instance decodeResourceArns :: Decode ResourceArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArns :: Encode ResourceArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A combination of <a>ByteMatchSet</a>, <a>IPSet</a>, and/or <a>SqlInjectionMatchSet</a> objects that identify the web requests that you want to allow, block, or count. For example, you might create a <code>Rule</code> that includes the following predicates:</p> <ul> <li> <p>An <code>IPSet</code> that causes AWS WAF to search for web requests that originate from the IP address <code>192.0.2.44</code> </p> </li> <li> <p>A <code>ByteMatchSet</code> that causes AWS WAF to search for web requests for which the value of the <code>User-Agent</code> header is <code>BadBot</code>.</p> </li> </ul> <p>To match the settings in this <code>Rule</code>, a request must originate from <code>192.0.2.44</code> AND include a <code>User-Agent</code> header for which the value is <code>BadBot</code>.</p>
newtype Rule = Rule 
  { "RuleId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "Predicates" :: (Predicates)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of predefined rules that you can add to a web ACL.</p> <p>Rule groups are subject to the following limits:</p> <ul> <li> <p>Three rule groups per account. You can request an increase to this limit by contacting customer support.</p> </li> <li> <p>One rule group per web ACL.</p> </li> <li> <p>Ten rules per rule group.</p> </li> </ul>
newtype RuleGroup = RuleGroup 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  }
derive instance newtypeRuleGroup :: Newtype RuleGroup _
derive instance repGenericRuleGroup :: Generic RuleGroup _
instance showRuleGroup :: Show RuleGroup where
  show = genericShow
instance decodeRuleGroup :: Decode RuleGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleGroup :: Encode RuleGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleGroupSummaries = RuleGroupSummaries (Array RuleGroupSummary)
derive instance newtypeRuleGroupSummaries :: Newtype RuleGroupSummaries _
derive instance repGenericRuleGroupSummaries :: Generic RuleGroupSummaries _
instance showRuleGroupSummaries :: Show RuleGroupSummaries where
  show = genericShow
instance decodeRuleGroupSummaries :: Decode RuleGroupSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleGroupSummaries :: Encode RuleGroupSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the identifier and the friendly name or description of the <code>RuleGroup</code>.</p>
newtype RuleGroupSummary = RuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRuleGroupSummary :: Newtype RuleGroupSummary _
derive instance repGenericRuleGroupSummary :: Generic RuleGroupSummary _
instance showRuleGroupSummary :: Show RuleGroupSummary where
  show = genericShow
instance decodeRuleGroupSummary :: Decode RuleGroupSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleGroupSummary :: Encode RuleGroupSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies an <code>ActivatedRule</code> and indicates whether you want to add it to a <code>RuleGroup</code> or delete it from a <code>RuleGroup</code>.</p>
newtype RuleGroupUpdate = RuleGroupUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }
derive instance newtypeRuleGroupUpdate :: Newtype RuleGroupUpdate _
derive instance repGenericRuleGroupUpdate :: Generic RuleGroupUpdate _
instance showRuleGroupUpdate :: Show RuleGroupUpdate where
  show = genericShow
instance decodeRuleGroupUpdate :: Decode RuleGroupUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleGroupUpdate :: Encode RuleGroupUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleGroupUpdates = RuleGroupUpdates (Array RuleGroupUpdate)
derive instance newtypeRuleGroupUpdates :: Newtype RuleGroupUpdates _
derive instance repGenericRuleGroupUpdates :: Generic RuleGroupUpdates _
instance showRuleGroupUpdates :: Show RuleGroupUpdates where
  show = genericShow
instance decodeRuleGroupUpdates :: Decode RuleGroupUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleGroupUpdates :: Encode RuleGroupUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RulePriority = RulePriority Int
derive instance newtypeRulePriority :: Newtype RulePriority _
derive instance repGenericRulePriority :: Generic RulePriority _
instance showRulePriority :: Show RulePriority where
  show = genericShow
instance decodeRulePriority :: Decode RulePriority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRulePriority :: Encode RulePriority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleSummaries = RuleSummaries (Array RuleSummary)
derive instance newtypeRuleSummaries :: Newtype RuleSummaries _
derive instance repGenericRuleSummaries :: Generic RuleSummaries _
instance showRuleSummaries :: Show RuleSummaries where
  show = genericShow
instance decodeRuleSummaries :: Decode RuleSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleSummaries :: Encode RuleSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the identifier and the friendly name or description of the <code>Rule</code>.</p>
newtype RuleSummary = RuleSummary 
  { "RuleId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeRuleSummary :: Newtype RuleSummary _
derive instance repGenericRuleSummary :: Generic RuleSummary _
instance showRuleSummary :: Show RuleSummary where
  show = genericShow
instance decodeRuleSummary :: Decode RuleSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleSummary :: Encode RuleSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a <code>Predicate</code> (such as an <code>IPSet</code>) and indicates whether you want to add it to a <code>Rule</code> or delete it from a <code>Rule</code>.</p>
newtype RuleUpdate = RuleUpdate 
  { "Action" :: (ChangeAction)
  , "Predicate" :: (Predicate)
  }
derive instance newtypeRuleUpdate :: Newtype RuleUpdate _
derive instance repGenericRuleUpdate :: Generic RuleUpdate _
instance showRuleUpdate :: Show RuleUpdate where
  show = genericShow
instance decodeRuleUpdate :: Decode RuleUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleUpdate :: Encode RuleUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleUpdates = RuleUpdates (Array RuleUpdate)
derive instance newtypeRuleUpdates :: Newtype RuleUpdates _
derive instance repGenericRuleUpdates :: Generic RuleUpdates _
instance showRuleUpdates :: Show RuleUpdates where
  show = genericShow
instance decodeRuleUpdates :: Decode RuleUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleUpdates :: Encode RuleUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SampleWeight = SampleWeight Number
derive instance newtypeSampleWeight :: Newtype SampleWeight _
derive instance repGenericSampleWeight :: Generic SampleWeight _
instance showSampleWeight :: Show SampleWeight where
  show = genericShow
instance decodeSampleWeight :: Decode SampleWeight where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampleWeight :: Encode SampleWeight where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from a <a>GetSampledRequests</a> request includes a <code>SampledHTTPRequests</code> complex type that appears as <code>SampledRequests</code> in the response syntax. <code>SampledHTTPRequests</code> contains one <code>SampledHTTPRequest</code> object for each web request that is returned by <code>GetSampledRequests</code>.</p>
newtype SampledHTTPRequest = SampledHTTPRequest 
  { "Request" :: (HTTPRequest)
  , "Weight" :: (SampleWeight)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "RuleWithinRuleGroup" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeSampledHTTPRequest :: Newtype SampledHTTPRequest _
derive instance repGenericSampledHTTPRequest :: Generic SampledHTTPRequest _
instance showSampledHTTPRequest :: Show SampledHTTPRequest where
  show = genericShow
instance decodeSampledHTTPRequest :: Decode SampledHTTPRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampledHTTPRequest :: Encode SampledHTTPRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SampledHTTPRequests = SampledHTTPRequests (Array SampledHTTPRequest)
derive instance newtypeSampledHTTPRequests :: Newtype SampledHTTPRequests _
derive instance repGenericSampledHTTPRequests :: Generic SampledHTTPRequests _
instance showSampledHTTPRequests :: Show SampledHTTPRequests where
  show = genericShow
instance decodeSampledHTTPRequests :: Decode SampledHTTPRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampledHTTPRequests :: Encode SampledHTTPRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Size = Size Number
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where
  show = genericShow
instance decodeSize :: Decode Size where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSize :: Encode Size where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a constraint on the size of a part of the web request. AWS WAF uses the <code>Size</code>, <code>ComparisonOperator</code>, and <code>FieldToMatch</code> to build an expression in the form of "<code>Size</code> <code>ComparisonOperator</code> size in bytes of <code>FieldToMatch</code>". If that expression is true, the <code>SizeConstraint</code> is considered to match.</p>
newtype SizeConstraint = SizeConstraint 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Size" :: (Size)
  }
derive instance newtypeSizeConstraint :: Newtype SizeConstraint _
derive instance repGenericSizeConstraint :: Generic SizeConstraint _
instance showSizeConstraint :: Show SizeConstraint where
  show = genericShow
instance decodeSizeConstraint :: Decode SizeConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraint :: Encode SizeConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains <code>SizeConstraint</code> objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a <code>SizeConstraintSet</code> contains more than one <code>SizeConstraint</code> object, a request only needs to match one constraint to be considered a match.</p>
newtype SizeConstraintSet = SizeConstraintSet 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "SizeConstraints" :: (SizeConstraints)
  }
derive instance newtypeSizeConstraintSet :: Newtype SizeConstraintSet _
derive instance repGenericSizeConstraintSet :: Generic SizeConstraintSet _
instance showSizeConstraintSet :: Show SizeConstraintSet where
  show = genericShow
instance decodeSizeConstraintSet :: Decode SizeConstraintSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraintSet :: Encode SizeConstraintSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizeConstraintSetSummaries = SizeConstraintSetSummaries (Array SizeConstraintSetSummary)
derive instance newtypeSizeConstraintSetSummaries :: Newtype SizeConstraintSetSummaries _
derive instance repGenericSizeConstraintSetSummaries :: Generic SizeConstraintSetSummaries _
instance showSizeConstraintSetSummaries :: Show SizeConstraintSetSummaries where
  show = genericShow
instance decodeSizeConstraintSetSummaries :: Decode SizeConstraintSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraintSetSummaries :: Encode SizeConstraintSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetSummary = SizeConstraintSetSummary 
  { "SizeConstraintSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeSizeConstraintSetSummary :: Newtype SizeConstraintSetSummary _
derive instance repGenericSizeConstraintSetSummary :: Generic SizeConstraintSetSummary _
instance showSizeConstraintSetSummary :: Show SizeConstraintSetSummary where
  show = genericShow
instance decodeSizeConstraintSetSummary :: Decode SizeConstraintSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraintSetSummary :: Encode SizeConstraintSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a <a>SizeConstraintSet</a> or delete it from a <code>SizeConstraintSet</code>.</p>
newtype SizeConstraintSetUpdate = SizeConstraintSetUpdate 
  { "Action" :: (ChangeAction)
  , "SizeConstraint" :: (SizeConstraint)
  }
derive instance newtypeSizeConstraintSetUpdate :: Newtype SizeConstraintSetUpdate _
derive instance repGenericSizeConstraintSetUpdate :: Generic SizeConstraintSetUpdate _
instance showSizeConstraintSetUpdate :: Show SizeConstraintSetUpdate where
  show = genericShow
instance decodeSizeConstraintSetUpdate :: Decode SizeConstraintSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraintSetUpdate :: Encode SizeConstraintSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizeConstraintSetUpdates = SizeConstraintSetUpdates (Array SizeConstraintSetUpdate)
derive instance newtypeSizeConstraintSetUpdates :: Newtype SizeConstraintSetUpdates _
derive instance repGenericSizeConstraintSetUpdates :: Generic SizeConstraintSetUpdates _
instance showSizeConstraintSetUpdates :: Show SizeConstraintSetUpdates where
  show = genericShow
instance decodeSizeConstraintSetUpdates :: Decode SizeConstraintSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraintSetUpdates :: Encode SizeConstraintSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizeConstraints = SizeConstraints (Array SizeConstraint)
derive instance newtypeSizeConstraints :: Newtype SizeConstraints _
derive instance repGenericSizeConstraints :: Generic SizeConstraints _
instance showSizeConstraints :: Show SizeConstraints where
  show = genericShow
instance decodeSizeConstraints :: Decode SizeConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeConstraints :: Encode SizeConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains <code>SqlInjectionMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a <code>SqlInjectionMatchSet</code> contains more than one <code>SqlInjectionMatchTuple</code> object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.</p>
newtype SqlInjectionMatchSet = SqlInjectionMatchSet 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "SqlInjectionMatchTuples" :: (SqlInjectionMatchTuples)
  }
derive instance newtypeSqlInjectionMatchSet :: Newtype SqlInjectionMatchSet _
derive instance repGenericSqlInjectionMatchSet :: Generic SqlInjectionMatchSet _
instance showSqlInjectionMatchSet :: Show SqlInjectionMatchSet where
  show = genericShow
instance decodeSqlInjectionMatchSet :: Decode SqlInjectionMatchSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchSet :: Encode SqlInjectionMatchSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SqlInjectionMatchSetSummaries = SqlInjectionMatchSetSummaries (Array SqlInjectionMatchSetSummary)
derive instance newtypeSqlInjectionMatchSetSummaries :: Newtype SqlInjectionMatchSetSummaries _
derive instance repGenericSqlInjectionMatchSetSummaries :: Generic SqlInjectionMatchSetSummaries _
instance showSqlInjectionMatchSetSummaries :: Show SqlInjectionMatchSetSummaries where
  show = genericShow
instance decodeSqlInjectionMatchSetSummaries :: Decode SqlInjectionMatchSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchSetSummaries :: Encode SqlInjectionMatchSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Id</code> and <code>Name</code> of a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeSqlInjectionMatchSetSummary :: Newtype SqlInjectionMatchSetSummary _
derive instance repGenericSqlInjectionMatchSetSummary :: Generic SqlInjectionMatchSetSummary _
instance showSqlInjectionMatchSetSummary :: Show SqlInjectionMatchSetSummary where
  show = genericShow
instance decodeSqlInjectionMatchSetSummary :: Decode SqlInjectionMatchSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchSetSummary :: Encode SqlInjectionMatchSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a <a>SqlInjectionMatchSet</a> or delete it from a <code>SqlInjectionMatchSet</code>.</p>
newtype SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "SqlInjectionMatchTuple" :: (SqlInjectionMatchTuple)
  }
derive instance newtypeSqlInjectionMatchSetUpdate :: Newtype SqlInjectionMatchSetUpdate _
derive instance repGenericSqlInjectionMatchSetUpdate :: Generic SqlInjectionMatchSetUpdate _
instance showSqlInjectionMatchSetUpdate :: Show SqlInjectionMatchSetUpdate where
  show = genericShow
instance decodeSqlInjectionMatchSetUpdate :: Decode SqlInjectionMatchSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchSetUpdate :: Encode SqlInjectionMatchSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SqlInjectionMatchSetUpdates = SqlInjectionMatchSetUpdates (Array SqlInjectionMatchSetUpdate)
derive instance newtypeSqlInjectionMatchSetUpdates :: Newtype SqlInjectionMatchSetUpdates _
derive instance repGenericSqlInjectionMatchSetUpdates :: Generic SqlInjectionMatchSetUpdates _
instance showSqlInjectionMatchSetUpdates :: Show SqlInjectionMatchSetUpdates where
  show = genericShow
instance decodeSqlInjectionMatchSetUpdates :: Decode SqlInjectionMatchSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchSetUpdates :: Encode SqlInjectionMatchSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype SqlInjectionMatchTuple = SqlInjectionMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }
derive instance newtypeSqlInjectionMatchTuple :: Newtype SqlInjectionMatchTuple _
derive instance repGenericSqlInjectionMatchTuple :: Generic SqlInjectionMatchTuple _
instance showSqlInjectionMatchTuple :: Show SqlInjectionMatchTuple where
  show = genericShow
instance decodeSqlInjectionMatchTuple :: Decode SqlInjectionMatchTuple where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchTuple :: Encode SqlInjectionMatchTuple where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SqlInjectionMatchTuples = SqlInjectionMatchTuples (Array SqlInjectionMatchTuple)
derive instance newtypeSqlInjectionMatchTuples :: Newtype SqlInjectionMatchTuples _
derive instance repGenericSqlInjectionMatchTuples :: Generic SqlInjectionMatchTuples _
instance showSqlInjectionMatchTuples :: Show SqlInjectionMatchTuples where
  show = genericShow
instance decodeSqlInjectionMatchTuples :: Decode SqlInjectionMatchTuples where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSqlInjectionMatchTuples :: Encode SqlInjectionMatchTuples where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscribedRuleGroupSummaries = SubscribedRuleGroupSummaries (Array SubscribedRuleGroupSummary)
derive instance newtypeSubscribedRuleGroupSummaries :: Newtype SubscribedRuleGroupSummaries _
derive instance repGenericSubscribedRuleGroupSummaries :: Generic SubscribedRuleGroupSummaries _
instance showSubscribedRuleGroupSummaries :: Show SubscribedRuleGroupSummaries where
  show = genericShow
instance decodeSubscribedRuleGroupSummaries :: Decode SubscribedRuleGroupSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribedRuleGroupSummaries :: Encode SubscribedRuleGroupSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of the rule groups you are subscribed to.</p>
newtype SubscribedRuleGroupSummary = SubscribedRuleGroupSummary 
  { "RuleGroupId" :: (ResourceId)
  , "Name" :: (ResourceName)
  , "MetricName" :: (MetricName)
  }
derive instance newtypeSubscribedRuleGroupSummary :: Newtype SubscribedRuleGroupSummary _
derive instance repGenericSubscribedRuleGroupSummary :: Generic SubscribedRuleGroupSummary _
instance showSubscribedRuleGroupSummary :: Show SubscribedRuleGroupSummary where
  show = genericShow
instance decodeSubscribedRuleGroupSummary :: Decode SubscribedRuleGroupSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribedRuleGroupSummary :: Encode SubscribedRuleGroupSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TextTransformation = TextTransformation String
derive instance newtypeTextTransformation :: Newtype TextTransformation _
derive instance repGenericTextTransformation :: Generic TextTransformation _
instance showTextTransformation :: Show TextTransformation where
  show = genericShow
instance decodeTextTransformation :: Decode TextTransformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextTransformation :: Encode TextTransformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>In a <a>GetSampledRequests</a> request, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which you want AWS WAF to return a sample of web requests.</p> <p>In a <a>GetSampledRequests</a> response, the <code>StartTime</code> and <code>EndTime</code> objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, <code>EndTime</code> is the time that AWS WAF received the 5,000th request. </p>
newtype TimeWindow = TimeWindow 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  }
derive instance newtypeTimeWindow :: Newtype TimeWindow _
derive instance repGenericTimeWindow :: Generic TimeWindow _
instance showTimeWindow :: Show TimeWindow where
  show = genericShow
instance decodeTimeWindow :: Decode TimeWindow where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeWindow :: Encode TimeWindow where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype URIString = URIString String
derive instance newtypeURIString :: Newtype URIString _
derive instance repGenericURIString :: Generic URIString _
instance showURIString :: Show URIString where
  show = genericShow
instance decodeURIString :: Decode URIString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeURIString :: Encode URIString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateByteMatchSetRequest = UpdateByteMatchSetRequest 
  { "ByteMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (ByteMatchSetUpdates)
  }
derive instance newtypeUpdateByteMatchSetRequest :: Newtype UpdateByteMatchSetRequest _
derive instance repGenericUpdateByteMatchSetRequest :: Generic UpdateByteMatchSetRequest _
instance showUpdateByteMatchSetRequest :: Show UpdateByteMatchSetRequest where
  show = genericShow
instance decodeUpdateByteMatchSetRequest :: Decode UpdateByteMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateByteMatchSetRequest :: Encode UpdateByteMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateByteMatchSetResponse = UpdateByteMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateByteMatchSetResponse :: Newtype UpdateByteMatchSetResponse _
derive instance repGenericUpdateByteMatchSetResponse :: Generic UpdateByteMatchSetResponse _
instance showUpdateByteMatchSetResponse :: Show UpdateByteMatchSetResponse where
  show = genericShow
instance decodeUpdateByteMatchSetResponse :: Decode UpdateByteMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateByteMatchSetResponse :: Encode UpdateByteMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGeoMatchSetRequest = UpdateGeoMatchSetRequest 
  { "GeoMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (GeoMatchSetUpdates)
  }
derive instance newtypeUpdateGeoMatchSetRequest :: Newtype UpdateGeoMatchSetRequest _
derive instance repGenericUpdateGeoMatchSetRequest :: Generic UpdateGeoMatchSetRequest _
instance showUpdateGeoMatchSetRequest :: Show UpdateGeoMatchSetRequest where
  show = genericShow
instance decodeUpdateGeoMatchSetRequest :: Decode UpdateGeoMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGeoMatchSetRequest :: Encode UpdateGeoMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateGeoMatchSetResponse :: Newtype UpdateGeoMatchSetResponse _
derive instance repGenericUpdateGeoMatchSetResponse :: Generic UpdateGeoMatchSetResponse _
instance showUpdateGeoMatchSetResponse :: Show UpdateGeoMatchSetResponse where
  show = genericShow
instance decodeUpdateGeoMatchSetResponse :: Decode UpdateGeoMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGeoMatchSetResponse :: Encode UpdateGeoMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "IPSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (IPSetUpdates)
  }
derive instance newtypeUpdateIPSetRequest :: Newtype UpdateIPSetRequest _
derive instance repGenericUpdateIPSetRequest :: Generic UpdateIPSetRequest _
instance showUpdateIPSetRequest :: Show UpdateIPSetRequest where
  show = genericShow
instance decodeUpdateIPSetRequest :: Decode UpdateIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIPSetRequest :: Encode UpdateIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIPSetResponse = UpdateIPSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateIPSetResponse :: Newtype UpdateIPSetResponse _
derive instance repGenericUpdateIPSetResponse :: Generic UpdateIPSetResponse _
instance showUpdateIPSetResponse :: Show UpdateIPSetResponse where
  show = genericShow
instance decodeUpdateIPSetResponse :: Decode UpdateIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIPSetResponse :: Encode UpdateIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRateBasedRuleRequest = UpdateRateBasedRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  , "RateLimit" :: (RateLimit)
  }
derive instance newtypeUpdateRateBasedRuleRequest :: Newtype UpdateRateBasedRuleRequest _
derive instance repGenericUpdateRateBasedRuleRequest :: Generic UpdateRateBasedRuleRequest _
instance showUpdateRateBasedRuleRequest :: Show UpdateRateBasedRuleRequest where
  show = genericShow
instance decodeUpdateRateBasedRuleRequest :: Decode UpdateRateBasedRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRateBasedRuleRequest :: Encode UpdateRateBasedRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRateBasedRuleResponse :: Newtype UpdateRateBasedRuleResponse _
derive instance repGenericUpdateRateBasedRuleResponse :: Generic UpdateRateBasedRuleResponse _
instance showUpdateRateBasedRuleResponse :: Show UpdateRateBasedRuleResponse where
  show = genericShow
instance decodeUpdateRateBasedRuleResponse :: Decode UpdateRateBasedRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRateBasedRuleResponse :: Encode UpdateRateBasedRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRegexMatchSetRequest = UpdateRegexMatchSetRequest 
  { "RegexMatchSetId" :: (ResourceId)
  , "Updates" :: (RegexMatchSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRegexMatchSetRequest :: Newtype UpdateRegexMatchSetRequest _
derive instance repGenericUpdateRegexMatchSetRequest :: Generic UpdateRegexMatchSetRequest _
instance showUpdateRegexMatchSetRequest :: Show UpdateRegexMatchSetRequest where
  show = genericShow
instance decodeUpdateRegexMatchSetRequest :: Decode UpdateRegexMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRegexMatchSetRequest :: Encode UpdateRegexMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRegexMatchSetResponse :: Newtype UpdateRegexMatchSetResponse _
derive instance repGenericUpdateRegexMatchSetResponse :: Generic UpdateRegexMatchSetResponse _
instance showUpdateRegexMatchSetResponse :: Show UpdateRegexMatchSetResponse where
  show = genericShow
instance decodeUpdateRegexMatchSetResponse :: Decode UpdateRegexMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRegexMatchSetResponse :: Encode UpdateRegexMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRegexPatternSetRequest = UpdateRegexPatternSetRequest 
  { "RegexPatternSetId" :: (ResourceId)
  , "Updates" :: (RegexPatternSetUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRegexPatternSetRequest :: Newtype UpdateRegexPatternSetRequest _
derive instance repGenericUpdateRegexPatternSetRequest :: Generic UpdateRegexPatternSetRequest _
instance showUpdateRegexPatternSetRequest :: Show UpdateRegexPatternSetRequest where
  show = genericShow
instance decodeUpdateRegexPatternSetRequest :: Decode UpdateRegexPatternSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRegexPatternSetRequest :: Encode UpdateRegexPatternSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRegexPatternSetResponse :: Newtype UpdateRegexPatternSetResponse _
derive instance repGenericUpdateRegexPatternSetResponse :: Generic UpdateRegexPatternSetResponse _
instance showUpdateRegexPatternSetResponse :: Show UpdateRegexPatternSetResponse where
  show = genericShow
instance decodeUpdateRegexPatternSetResponse :: Decode UpdateRegexPatternSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRegexPatternSetResponse :: Encode UpdateRegexPatternSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRuleGroupRequest = UpdateRuleGroupRequest 
  { "RuleGroupId" :: (ResourceId)
  , "Updates" :: (RuleGroupUpdates)
  , "ChangeToken" :: (ChangeToken)
  }
derive instance newtypeUpdateRuleGroupRequest :: Newtype UpdateRuleGroupRequest _
derive instance repGenericUpdateRuleGroupRequest :: Generic UpdateRuleGroupRequest _
instance showUpdateRuleGroupRequest :: Show UpdateRuleGroupRequest where
  show = genericShow
instance decodeUpdateRuleGroupRequest :: Decode UpdateRuleGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuleGroupRequest :: Encode UpdateRuleGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRuleGroupResponse = UpdateRuleGroupResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRuleGroupResponse :: Newtype UpdateRuleGroupResponse _
derive instance repGenericUpdateRuleGroupResponse :: Generic UpdateRuleGroupResponse _
instance showUpdateRuleGroupResponse :: Show UpdateRuleGroupResponse where
  show = genericShow
instance decodeUpdateRuleGroupResponse :: Decode UpdateRuleGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuleGroupResponse :: Encode UpdateRuleGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRuleRequest = UpdateRuleRequest 
  { "RuleId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (RuleUpdates)
  }
derive instance newtypeUpdateRuleRequest :: Newtype UpdateRuleRequest _
derive instance repGenericUpdateRuleRequest :: Generic UpdateRuleRequest _
instance showUpdateRuleRequest :: Show UpdateRuleRequest where
  show = genericShow
instance decodeUpdateRuleRequest :: Decode UpdateRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuleRequest :: Encode UpdateRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateRuleResponse = UpdateRuleResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateRuleResponse :: Newtype UpdateRuleResponse _
derive instance repGenericUpdateRuleResponse :: Generic UpdateRuleResponse _
instance showUpdateRuleResponse :: Show UpdateRuleResponse where
  show = genericShow
instance decodeUpdateRuleResponse :: Decode UpdateRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRuleResponse :: Encode UpdateRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSizeConstraintSetRequest = UpdateSizeConstraintSetRequest 
  { "SizeConstraintSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SizeConstraintSetUpdates)
  }
derive instance newtypeUpdateSizeConstraintSetRequest :: Newtype UpdateSizeConstraintSetRequest _
derive instance repGenericUpdateSizeConstraintSetRequest :: Generic UpdateSizeConstraintSetRequest _
instance showUpdateSizeConstraintSetRequest :: Show UpdateSizeConstraintSetRequest where
  show = genericShow
instance decodeUpdateSizeConstraintSetRequest :: Decode UpdateSizeConstraintSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSizeConstraintSetRequest :: Encode UpdateSizeConstraintSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateSizeConstraintSetResponse :: Newtype UpdateSizeConstraintSetResponse _
derive instance repGenericUpdateSizeConstraintSetResponse :: Generic UpdateSizeConstraintSetResponse _
instance showUpdateSizeConstraintSetResponse :: Show UpdateSizeConstraintSetResponse where
  show = genericShow
instance decodeUpdateSizeConstraintSetResponse :: Decode UpdateSizeConstraintSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSizeConstraintSetResponse :: Encode UpdateSizeConstraintSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to update a <a>SqlInjectionMatchSet</a>.</p>
newtype UpdateSqlInjectionMatchSetRequest = UpdateSqlInjectionMatchSetRequest 
  { "SqlInjectionMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (SqlInjectionMatchSetUpdates)
  }
derive instance newtypeUpdateSqlInjectionMatchSetRequest :: Newtype UpdateSqlInjectionMatchSetRequest _
derive instance repGenericUpdateSqlInjectionMatchSetRequest :: Generic UpdateSqlInjectionMatchSetRequest _
instance showUpdateSqlInjectionMatchSetRequest :: Show UpdateSqlInjectionMatchSetRequest where
  show = genericShow
instance decodeUpdateSqlInjectionMatchSetRequest :: Decode UpdateSqlInjectionMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSqlInjectionMatchSetRequest :: Encode UpdateSqlInjectionMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to an <a>UpdateSqlInjectionMatchSets</a> request.</p>
newtype UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateSqlInjectionMatchSetResponse :: Newtype UpdateSqlInjectionMatchSetResponse _
derive instance repGenericUpdateSqlInjectionMatchSetResponse :: Generic UpdateSqlInjectionMatchSetResponse _
instance showUpdateSqlInjectionMatchSetResponse :: Show UpdateSqlInjectionMatchSetResponse where
  show = genericShow
instance decodeUpdateSqlInjectionMatchSetResponse :: Decode UpdateSqlInjectionMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSqlInjectionMatchSetResponse :: Encode UpdateSqlInjectionMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateWebACLRequest = UpdateWebACLRequest 
  { "WebACLId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: NullOrUndefined.NullOrUndefined (WebACLUpdates)
  , "DefaultAction" :: NullOrUndefined.NullOrUndefined (WafAction)
  }
derive instance newtypeUpdateWebACLRequest :: Newtype UpdateWebACLRequest _
derive instance repGenericUpdateWebACLRequest :: Generic UpdateWebACLRequest _
instance showUpdateWebACLRequest :: Show UpdateWebACLRequest where
  show = genericShow
instance decodeUpdateWebACLRequest :: Decode UpdateWebACLRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateWebACLRequest :: Encode UpdateWebACLRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateWebACLResponse = UpdateWebACLResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateWebACLResponse :: Newtype UpdateWebACLResponse _
derive instance repGenericUpdateWebACLResponse :: Generic UpdateWebACLResponse _
instance showUpdateWebACLResponse :: Show UpdateWebACLResponse where
  show = genericShow
instance decodeUpdateWebACLResponse :: Decode UpdateWebACLResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateWebACLResponse :: Encode UpdateWebACLResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to update an <a>XssMatchSet</a>.</p>
newtype UpdateXssMatchSetRequest = UpdateXssMatchSetRequest 
  { "XssMatchSetId" :: (ResourceId)
  , "ChangeToken" :: (ChangeToken)
  , "Updates" :: (XssMatchSetUpdates)
  }
derive instance newtypeUpdateXssMatchSetRequest :: Newtype UpdateXssMatchSetRequest _
derive instance repGenericUpdateXssMatchSetRequest :: Generic UpdateXssMatchSetRequest _
instance showUpdateXssMatchSetRequest :: Show UpdateXssMatchSetRequest where
  show = genericShow
instance decodeUpdateXssMatchSetRequest :: Decode UpdateXssMatchSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateXssMatchSetRequest :: Encode UpdateXssMatchSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to an <a>UpdateXssMatchSets</a> request.</p>
newtype UpdateXssMatchSetResponse = UpdateXssMatchSetResponse 
  { "ChangeToken" :: NullOrUndefined.NullOrUndefined (ChangeToken)
  }
derive instance newtypeUpdateXssMatchSetResponse :: Newtype UpdateXssMatchSetResponse _
derive instance repGenericUpdateXssMatchSetResponse :: Generic UpdateXssMatchSetResponse _
instance showUpdateXssMatchSetResponse :: Show UpdateXssMatchSetResponse where
  show = genericShow
instance decodeUpdateXssMatchSetResponse :: Decode UpdateXssMatchSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateXssMatchSetResponse :: Encode UpdateXssMatchSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name specified is invalid.</p>
newtype WAFDisallowedNameException = WAFDisallowedNameException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFDisallowedNameException :: Newtype WAFDisallowedNameException _
derive instance repGenericWAFDisallowedNameException :: Generic WAFDisallowedNameException _
instance showWAFDisallowedNameException :: Show WAFDisallowedNameException where
  show = genericShow
instance decodeWAFDisallowedNameException :: Decode WAFDisallowedNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFDisallowedNameException :: Encode WAFDisallowedNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because of a system problem, even though the request was valid. Retry your request.</p>
newtype WAFInternalErrorException = WAFInternalErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInternalErrorException :: Newtype WAFInternalErrorException _
derive instance repGenericWAFInternalErrorException :: Generic WAFInternalErrorException _
instance showWAFInternalErrorException :: Show WAFInternalErrorException where
  show = genericShow
instance decodeWAFInternalErrorException :: Decode WAFInternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInternalErrorException :: Encode WAFInternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.</p>
newtype WAFInvalidAccountException = WAFInvalidAccountException Types.NoArguments
derive instance newtypeWAFInvalidAccountException :: Newtype WAFInvalidAccountException _
derive instance repGenericWAFInvalidAccountException :: Generic WAFInvalidAccountException _
instance showWAFInvalidAccountException :: Show WAFInvalidAccountException where
  show = genericShow
instance decodeWAFInvalidAccountException :: Decode WAFInvalidAccountException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInvalidAccountException :: Encode WAFInvalidAccountException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because there was nothing to do. For example:</p> <ul> <li> <p>You tried to remove a <code>Rule</code> from a <code>WebACL</code>, but the <code>Rule</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to remove an IP address from an <code>IPSet</code>, but the IP address isn't in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to remove a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> isn't in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add a <code>Rule</code> to a <code>WebACL</code>, but the <code>Rule</code> already exists in the specified <code>WebACL</code>.</p> </li> <li> <p>You tried to add an IP address to an <code>IPSet</code>, but the IP address already exists in the specified <code>IPSet</code>.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to a <code>ByteMatchSet</code>, but the <code>ByteMatchTuple</code> already exists in the specified <code>WebACL</code>.</p> </li> </ul>
newtype WAFInvalidOperationException = WAFInvalidOperationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidOperationException :: Newtype WAFInvalidOperationException _
derive instance repGenericWAFInvalidOperationException :: Generic WAFInvalidOperationException _
instance showWAFInvalidOperationException :: Show WAFInvalidOperationException where
  show = genericShow
instance decodeWAFInvalidOperationException :: Decode WAFInvalidOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInvalidOperationException :: Encode WAFInvalidOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because AWS WAF didn't recognize a parameter in the request. For example:</p> <ul> <li> <p>You specified an invalid parameter name.</p> </li> <li> <p>You specified an invalid value.</p> </li> <li> <p>You tried to update an object (<code>ByteMatchSet</code>, <code>IPSet</code>, <code>Rule</code>, or <code>WebACL</code>) using an action other than <code>INSERT</code> or <code>DELETE</code>.</p> </li> <li> <p>You tried to create a <code>WebACL</code> with a <code>DefaultAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to create a <code>RateBasedRule</code> with a <code>RateKey</code> value other than <code>IP</code>.</p> </li> <li> <p>You tried to update a <code>WebACL</code> with a <code>WafAction</code> <code>Type</code> other than <code>ALLOW</code>, <code>BLOCK</code>, or <code>COUNT</code>.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>FieldToMatch</code> <code>Type</code> other than HEADER, METHOD, QUERY_STRING, URI, or BODY.</p> </li> <li> <p>You tried to update a <code>ByteMatchSet</code> with a <code>Field</code> of <code>HEADER</code> but no value for <code>Data</code>.</p> </li> <li> <p>Your request references an ARN that is malformed, or corresponds to a resource with which a web ACL cannot be associated.</p> </li> </ul>
newtype WAFInvalidParameterException = WAFInvalidParameterException 
  { "Field'" :: NullOrUndefined.NullOrUndefined (ParameterExceptionField)
  , "Parameter'" :: NullOrUndefined.NullOrUndefined (ParameterExceptionParameter)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (ParameterExceptionReason)
  }
derive instance newtypeWAFInvalidParameterException :: Newtype WAFInvalidParameterException _
derive instance repGenericWAFInvalidParameterException :: Generic WAFInvalidParameterException _
instance showWAFInvalidParameterException :: Show WAFInvalidParameterException where
  show = genericShow
instance decodeWAFInvalidParameterException :: Decode WAFInvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInvalidParameterException :: Encode WAFInvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because the specified policy is not in the proper format. </p> <p>The policy is subject to the following restrictions:</p> <ul> <li> <p>You can attach only one policy with each <code>PutPermissionPolicy</code> request.</p> </li> <li> <p>The policy must include an <code>Effect</code>, <code>Action</code> and <code>Principal</code>. </p> </li> <li> <p> <code>Effect</code> must specify <code>Allow</code>.</p> </li> <li> <p>The <code>Action</code> in the policy must be <code>waf:UpdateWebACL</code> or <code>waf-regional:UpdateWebACL</code>. Any extra or wildcard actions in the policy will be rejected.</p> </li> <li> <p>The policy cannot include a <code>Resource</code> parameter.</p> </li> <li> <p>The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.</p> </li> <li> <p>The user making the request must be the owner of the RuleGroup.</p> </li> <li> <p>Your policy must be composed using IAM Policy version 2012-10-17.</p> </li> </ul>
newtype WAFInvalidPermissionPolicyException = WAFInvalidPermissionPolicyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidPermissionPolicyException :: Newtype WAFInvalidPermissionPolicyException _
derive instance repGenericWAFInvalidPermissionPolicyException :: Generic WAFInvalidPermissionPolicyException _
instance showWAFInvalidPermissionPolicyException :: Show WAFInvalidPermissionPolicyException where
  show = genericShow
instance decodeWAFInvalidPermissionPolicyException :: Decode WAFInvalidPermissionPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInvalidPermissionPolicyException :: Encode WAFInvalidPermissionPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The regular expression (regex) you specified in <code>RegexPatternString</code> is invalid.</p>
newtype WAFInvalidRegexPatternException = WAFInvalidRegexPatternException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFInvalidRegexPatternException :: Newtype WAFInvalidRegexPatternException _
derive instance repGenericWAFInvalidRegexPatternException :: Generic WAFInvalidRegexPatternException _
instance showWAFInvalidRegexPatternException :: Show WAFInvalidRegexPatternException where
  show = genericShow
instance decodeWAFInvalidRegexPatternException :: Decode WAFInvalidRegexPatternException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFInvalidRegexPatternException :: Encode WAFInvalidRegexPatternException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation exceeds a resource limit, for example, the maximum number of <code>WebACL</code> objects that you can create for an AWS account. For more information, see <a href="http://docs.aws.amazon.com/waf/latest/developerguide/limits.html">Limits</a> in the <i>AWS WAF Developer Guide</i>.</p>
newtype WAFLimitsExceededException = WAFLimitsExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFLimitsExceededException :: Newtype WAFLimitsExceededException _
derive instance repGenericWAFLimitsExceededException :: Generic WAFLimitsExceededException _
instance showWAFLimitsExceededException :: Show WAFLimitsExceededException where
  show = genericShow
instance decodeWAFLimitsExceededException :: Decode WAFLimitsExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFLimitsExceededException :: Encode WAFLimitsExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because you tried to delete an object that isn't empty. For example:</p> <ul> <li> <p>You tried to delete a <code>WebACL</code> that still contains one or more <code>Rule</code> objects.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that still contains one or more <code>ByteMatchSet</code> objects or other predicates.</p> </li> <li> <p>You tried to delete a <code>ByteMatchSet</code> that contains one or more <code>ByteMatchTuple</code> objects.</p> </li> <li> <p>You tried to delete an <code>IPSet</code> that references one or more IP addresses.</p> </li> </ul>
newtype WAFNonEmptyEntityException = WAFNonEmptyEntityException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonEmptyEntityException :: Newtype WAFNonEmptyEntityException _
derive instance repGenericWAFNonEmptyEntityException :: Generic WAFNonEmptyEntityException _
instance showWAFNonEmptyEntityException :: Show WAFNonEmptyEntityException where
  show = genericShow
instance decodeWAFNonEmptyEntityException :: Decode WAFNonEmptyEntityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFNonEmptyEntityException :: Encode WAFNonEmptyEntityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:</p> <ul> <li> <p>You tried to add a <code>Rule</code> to or delete a <code>Rule</code> from a <code>WebACL</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchSet</code> to or delete a <code>ByteMatchSet</code> from a <code>Rule</code> that doesn't exist.</p> </li> <li> <p>You tried to add an IP address to or delete an IP address from an <code>IPSet</code> that doesn't exist.</p> </li> <li> <p>You tried to add a <code>ByteMatchTuple</code> to or delete a <code>ByteMatchTuple</code> from a <code>ByteMatchSet</code> that doesn't exist.</p> </li> </ul>
newtype WAFNonexistentContainerException = WAFNonexistentContainerException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonexistentContainerException :: Newtype WAFNonexistentContainerException _
derive instance repGenericWAFNonexistentContainerException :: Generic WAFNonexistentContainerException _
instance showWAFNonexistentContainerException :: Show WAFNonexistentContainerException where
  show = genericShow
instance decodeWAFNonexistentContainerException :: Decode WAFNonexistentContainerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFNonexistentContainerException :: Encode WAFNonexistentContainerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because the referenced object doesn't exist.</p>
newtype WAFNonexistentItemException = WAFNonexistentItemException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFNonexistentItemException :: Newtype WAFNonexistentItemException _
derive instance repGenericWAFNonexistentItemException :: Generic WAFNonexistentItemException _
instance showWAFNonexistentItemException :: Show WAFNonexistentItemException where
  show = genericShow
instance decodeWAFNonexistentItemException :: Decode WAFNonexistentItemException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFNonexistentItemException :: Encode WAFNonexistentItemException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because you tried to delete an object that is still in use. For example:</p> <ul> <li> <p>You tried to delete a <code>ByteMatchSet</code> that is still referenced by a <code>Rule</code>.</p> </li> <li> <p>You tried to delete a <code>Rule</code> that is still referenced by a <code>WebACL</code>.</p> </li> </ul>
newtype WAFReferencedItemException = WAFReferencedItemException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFReferencedItemException :: Newtype WAFReferencedItemException _
derive instance repGenericWAFReferencedItemException :: Generic WAFReferencedItemException _
instance showWAFReferencedItemException :: Show WAFReferencedItemException where
  show = genericShow
instance decodeWAFReferencedItemException :: Decode WAFReferencedItemException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFReferencedItemException :: Encode WAFReferencedItemException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.</p>
newtype WAFStaleDataException = WAFStaleDataException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFStaleDataException :: Newtype WAFStaleDataException _
derive instance repGenericWAFStaleDataException :: Generic WAFStaleDataException _
instance showWAFStaleDataException :: Show WAFStaleDataException where
  show = genericShow
instance decodeWAFStaleDataException :: Decode WAFStaleDataException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFStaleDataException :: Encode WAFStaleDataException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified subscription does not exist.</p>
newtype WAFSubscriptionNotFoundException = WAFSubscriptionNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFSubscriptionNotFoundException :: Newtype WAFSubscriptionNotFoundException _
derive instance repGenericWAFSubscriptionNotFoundException :: Generic WAFSubscriptionNotFoundException _
instance showWAFSubscriptionNotFoundException :: Show WAFSubscriptionNotFoundException where
  show = genericShow
instance decodeWAFSubscriptionNotFoundException :: Decode WAFSubscriptionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFSubscriptionNotFoundException :: Encode WAFSubscriptionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation failed because the entity referenced is temporarily unavailable. Retry your request.</p>
newtype WAFUnavailableEntityException = WAFUnavailableEntityException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeWAFUnavailableEntityException :: Newtype WAFUnavailableEntityException _
derive instance repGenericWAFUnavailableEntityException :: Generic WAFUnavailableEntityException _
instance showWAFUnavailableEntityException :: Show WAFUnavailableEntityException where
  show = genericShow
instance decodeWAFUnavailableEntityException :: Decode WAFUnavailableEntityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWAFUnavailableEntityException :: Encode WAFUnavailableEntityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For the action that is associated with a rule in a <code>WebACL</code>, specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a <code>WebACL</code>, specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a <code>WebACL</code>. </p>
newtype WafAction = WafAction 
  { "Type" :: (WafActionType)
  }
derive instance newtypeWafAction :: Newtype WafAction _
derive instance repGenericWafAction :: Generic WafAction _
instance showWafAction :: Show WafAction where
  show = genericShow
instance decodeWafAction :: Decode WafAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWafAction :: Encode WafAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WafActionType = WafActionType String
derive instance newtypeWafActionType :: Newtype WafActionType _
derive instance repGenericWafActionType :: Generic WafActionType _
instance showWafActionType :: Show WafActionType where
  show = genericShow
instance decodeWafActionType :: Decode WafActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWafActionType :: Encode WafActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The action to take if any rule within the <code>RuleGroup</code> matches a request. </p>
newtype WafOverrideAction = WafOverrideAction 
  { "Type" :: (WafOverrideActionType)
  }
derive instance newtypeWafOverrideAction :: Newtype WafOverrideAction _
derive instance repGenericWafOverrideAction :: Generic WafOverrideAction _
instance showWafOverrideAction :: Show WafOverrideAction where
  show = genericShow
instance decodeWafOverrideAction :: Decode WafOverrideAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWafOverrideAction :: Encode WafOverrideAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WafOverrideActionType = WafOverrideActionType String
derive instance newtypeWafOverrideActionType :: Newtype WafOverrideActionType _
derive instance repGenericWafOverrideActionType :: Generic WafOverrideActionType _
instance showWafOverrideActionType :: Show WafOverrideActionType where
  show = genericShow
instance decodeWafOverrideActionType :: Decode WafOverrideActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWafOverrideActionType :: Encode WafOverrideActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WafRuleType = WafRuleType String
derive instance newtypeWafRuleType :: Newtype WafRuleType _
derive instance repGenericWafRuleType :: Generic WafRuleType _
instance showWafRuleType :: Show WafRuleType where
  show = genericShow
instance decodeWafRuleType :: Decode WafRuleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWafRuleType :: Encode WafRuleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the <code>Rules</code> that identify the requests that you want to allow, block, or count. In a <code>WebACL</code>, you also specify a default action (<code>ALLOW</code> or <code>BLOCK</code>), and the action for each <code>Rule</code> that you add to a <code>WebACL</code>, for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the <code>WebACL</code> with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one <code>Rule</code> to a <code>WebACL</code>, a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see <a>UpdateWebACL</a>.</p>
newtype WebACL = WebACL 
  { "WebACLId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "DefaultAction" :: (WafAction)
  , "Rules" :: (ActivatedRules)
  }
derive instance newtypeWebACL :: Newtype WebACL _
derive instance repGenericWebACL :: Generic WebACL _
instance showWebACL :: Show WebACL where
  show = genericShow
instance decodeWebACL :: Decode WebACL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebACL :: Encode WebACL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WebACLSummaries = WebACLSummaries (Array WebACLSummary)
derive instance newtypeWebACLSummaries :: Newtype WebACLSummaries _
derive instance repGenericWebACLSummaries :: Generic WebACLSummaries _
instance showWebACLSummaries :: Show WebACLSummaries where
  show = genericShow
instance decodeWebACLSummaries :: Decode WebACLSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebACLSummaries :: Encode WebACLSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the identifier and the name or description of the <a>WebACL</a>.</p>
newtype WebACLSummary = WebACLSummary 
  { "WebACLId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeWebACLSummary :: Newtype WebACLSummary _
derive instance repGenericWebACLSummary :: Generic WebACLSummary _
instance showWebACLSummary :: Show WebACLSummary where
  show = genericShow
instance decodeWebACLSummary :: Decode WebACLSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebACLSummary :: Encode WebACLSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies whether to insert a <code>Rule</code> into or delete a <code>Rule</code> from a <code>WebACL</code>.</p>
newtype WebACLUpdate = WebACLUpdate 
  { "Action" :: (ChangeAction)
  , "ActivatedRule" :: (ActivatedRule)
  }
derive instance newtypeWebACLUpdate :: Newtype WebACLUpdate _
derive instance repGenericWebACLUpdate :: Generic WebACLUpdate _
instance showWebACLUpdate :: Show WebACLUpdate where
  show = genericShow
instance decodeWebACLUpdate :: Decode WebACLUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebACLUpdate :: Encode WebACLUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WebACLUpdates = WebACLUpdates (Array WebACLUpdate)
derive instance newtypeWebACLUpdates :: Newtype WebACLUpdates _
derive instance repGenericWebACLUpdates :: Generic WebACLUpdates _
instance showWebACLUpdates :: Show WebACLUpdates where
  show = genericShow
instance decodeWebACLUpdates :: Decode WebACLUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebACLUpdates :: Encode WebACLUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains <code>XssMatchTuple</code> objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a <code>XssMatchSet</code> contains more than one <code>XssMatchTuple</code> object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.</p>
newtype XssMatchSet = XssMatchSet 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "XssMatchTuples" :: (XssMatchTuples)
  }
derive instance newtypeXssMatchSet :: Newtype XssMatchSet _
derive instance repGenericXssMatchSet :: Generic XssMatchSet _
instance showXssMatchSet :: Show XssMatchSet where
  show = genericShow
instance decodeXssMatchSet :: Decode XssMatchSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchSet :: Encode XssMatchSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XssMatchSetSummaries = XssMatchSetSummaries (Array XssMatchSetSummary)
derive instance newtypeXssMatchSetSummaries :: Newtype XssMatchSetSummaries _
derive instance repGenericXssMatchSetSummaries :: Generic XssMatchSetSummaries _
instance showXssMatchSetSummaries :: Show XssMatchSetSummaries where
  show = genericShow
instance decodeXssMatchSetSummaries :: Decode XssMatchSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchSetSummaries :: Encode XssMatchSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Id</code> and <code>Name</code> of an <code>XssMatchSet</code>.</p>
newtype XssMatchSetSummary = XssMatchSetSummary 
  { "XssMatchSetId" :: (ResourceId)
  , "Name" :: (ResourceName)
  }
derive instance newtypeXssMatchSetSummary :: Newtype XssMatchSetSummary _
derive instance repGenericXssMatchSetSummary :: Generic XssMatchSetSummary _
instance showXssMatchSetSummary :: Show XssMatchSetSummary where
  show = genericShow
instance decodeXssMatchSetSummary :: Decode XssMatchSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchSetSummary :: Encode XssMatchSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an <a>XssMatchSet</a> or delete it from an <code>XssMatchSet</code>.</p>
newtype XssMatchSetUpdate = XssMatchSetUpdate 
  { "Action" :: (ChangeAction)
  , "XssMatchTuple" :: (XssMatchTuple)
  }
derive instance newtypeXssMatchSetUpdate :: Newtype XssMatchSetUpdate _
derive instance repGenericXssMatchSetUpdate :: Generic XssMatchSetUpdate _
instance showXssMatchSetUpdate :: Show XssMatchSetUpdate where
  show = genericShow
instance decodeXssMatchSetUpdate :: Decode XssMatchSetUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchSetUpdate :: Encode XssMatchSetUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XssMatchSetUpdates = XssMatchSetUpdates (Array XssMatchSetUpdate)
derive instance newtypeXssMatchSetUpdates :: Newtype XssMatchSetUpdates _
derive instance repGenericXssMatchSetUpdates :: Generic XssMatchSetUpdates _
instance showXssMatchSetUpdates :: Show XssMatchSetUpdates where
  show = genericShow
instance decodeXssMatchSetUpdates :: Decode XssMatchSetUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchSetUpdates :: Encode XssMatchSetUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.</p>
newtype XssMatchTuple = XssMatchTuple 
  { "FieldToMatch" :: (FieldToMatch)
  , "TextTransformation" :: (TextTransformation)
  }
derive instance newtypeXssMatchTuple :: Newtype XssMatchTuple _
derive instance repGenericXssMatchTuple :: Generic XssMatchTuple _
instance showXssMatchTuple :: Show XssMatchTuple where
  show = genericShow
instance decodeXssMatchTuple :: Decode XssMatchTuple where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchTuple :: Encode XssMatchTuple where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XssMatchTuples = XssMatchTuples (Array XssMatchTuple)
derive instance newtypeXssMatchTuples :: Newtype XssMatchTuples _
derive instance repGenericXssMatchTuples :: Generic XssMatchTuples _
instance showXssMatchTuples :: Show XssMatchTuples where
  show = genericShow
instance decodeXssMatchTuples :: Decode XssMatchTuples where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXssMatchTuples :: Encode XssMatchTuples where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
