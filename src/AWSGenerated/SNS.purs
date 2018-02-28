

-- | <fullname>Amazon Simple Notification Service</fullname> <p>Amazon Simple Notification Service (Amazon SNS) is a web service that enables you to build distributed web-enabled applications. Applications can use Amazon SNS to easily push real-time notification messages to interested subscribers over multiple delivery protocols. For more information about this product see <a href="http://aws.amazon.com/sns/">http://aws.amazon.com/sns</a>. For detailed information about Amazon SNS features and their associated API calls, see the <a href="http://docs.aws.amazon.com/sns/latest/dg/">Amazon SNS Developer Guide</a>. </p> <p>We also provide SDKs that enable you to access Amazon SNS from your preferred programming language. The SDKs contain functionality that automatically takes care of tasks such as: cryptographically signing your service requests, retrying requests, and handling error responses. For a list of available SDKs, go to <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>. </p>
module AWS.SNS where

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

serviceName = "SNS" :: String


-- | <p>Adds a statement to a topic's access control policy, granting access for the specified AWS accounts to the specified actions.</p>
addPermission :: forall eff. AddPermissionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addPermission = Request.request serviceName "addPermission" 


-- | <p>Accepts a phone number and indicates whether the phone holder has opted out of receiving SMS messages from your account. You cannot send SMS messages to a number that is opted out.</p> <p>To resume sending messages, you can opt in the number by using the <code>OptInPhoneNumber</code> action.</p>
checkIfPhoneNumberIsOptedOut :: forall eff. CheckIfPhoneNumberIsOptedOutInput -> Aff (exception :: EXCEPTION | eff) CheckIfPhoneNumberIsOptedOutResponse
checkIfPhoneNumberIsOptedOut = Request.request serviceName "checkIfPhoneNumberIsOptedOut" 


-- | <p>Verifies an endpoint owner's intent to receive messages by validating the token sent to the endpoint by an earlier <code>Subscribe</code> action. If the token is valid, the action creates a new subscription and returns its Amazon Resource Name (ARN). This call requires an AWS signature only when the <code>AuthenticateOnUnsubscribe</code> flag is set to "true".</p>
confirmSubscription :: forall eff. ConfirmSubscriptionInput -> Aff (exception :: EXCEPTION | eff) ConfirmSubscriptionResponse
confirmSubscription = Request.request serviceName "confirmSubscription" 


-- | <p>Creates a platform application object for one of the supported push notification services, such as APNS and GCM, to which devices and mobile apps may register. You must specify PlatformPrincipal and PlatformCredential attributes when using the <code>CreatePlatformApplication</code> action. The PlatformPrincipal is received from the notification service. For APNS/APNS_SANDBOX, PlatformPrincipal is "SSL certificate". For GCM, PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client id". The PlatformCredential is also received from the notification service. For WNS, PlatformPrincipal is "Package Security Identifier". For MPNS, PlatformPrincipal is "TLS certificate". For Baidu, PlatformPrincipal is "API key".</p> <p>For APNS/APNS_SANDBOX, PlatformCredential is "private key". For GCM, PlatformCredential is "API key". For ADM, PlatformCredential is "client secret". For WNS, PlatformCredential is "secret key". For MPNS, PlatformCredential is "private key". For Baidu, PlatformCredential is "secret key". The PlatformApplicationArn that is returned when using <code>CreatePlatformApplication</code> is then used as an attribute for the <code>CreatePlatformEndpoint</code> action. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. For more information about obtaining the PlatformPrincipal and PlatformCredential for each of the supported push notification services, see <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-apns.html">Getting Started with Apple Push Notification Service</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-adm.html">Getting Started with Amazon Device Messaging</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-baidu.html">Getting Started with Baidu Cloud Push</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-gcm.html">Getting Started with Google Cloud Messaging for Android</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-mpns.html">Getting Started with MPNS</a>, or <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-wns.html">Getting Started with WNS</a>. </p>
createPlatformApplication :: forall eff. CreatePlatformApplicationInput -> Aff (exception :: EXCEPTION | eff) CreatePlatformApplicationResponse
createPlatformApplication = Request.request serviceName "createPlatformApplication" 


-- | <p>Creates an endpoint for a device and mobile app on one of the supported push notification services, such as GCM and APNS. <code>CreatePlatformEndpoint</code> requires the PlatformApplicationArn that is returned from <code>CreatePlatformApplication</code>. The EndpointArn that is returned when using <code>CreatePlatformEndpoint</code> can then be used by the <code>Publish</code> action to send a message to a mobile app or by the <code>Subscribe</code> action for subscription to a topic. The <code>CreatePlatformEndpoint</code> action is idempotent, so if the requester already owns an endpoint with the same device token and attributes, that endpoint's ARN is returned without creating a new endpoint. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p> <p>When using <code>CreatePlatformEndpoint</code> with Baidu, two attributes must be provided: ChannelId and UserId. The token field must also contain the ChannelId. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePushBaiduEndpoint.html">Creating an Amazon SNS Endpoint for Baidu</a>. </p>
createPlatformEndpoint :: forall eff. CreatePlatformEndpointInput -> Aff (exception :: EXCEPTION | eff) CreateEndpointResponse
createPlatformEndpoint = Request.request serviceName "createPlatformEndpoint" 


-- | <p>Creates a topic to which notifications can be published. Users can create at most 100,000 topics. For more information, see <a href="http://aws.amazon.com/sns/">http://aws.amazon.com/sns</a>. This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.</p>
createTopic :: forall eff. CreateTopicInput -> Aff (exception :: EXCEPTION | eff) CreateTopicResponse
createTopic = Request.request serviceName "createTopic" 


-- | <p>Deletes the endpoint for a device and mobile app from Amazon SNS. This action is idempotent. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p> <p>When you delete an endpoint that is also subscribed to a topic, then you must also unsubscribe the endpoint from the topic.</p>
deleteEndpoint :: forall eff. DeleteEndpointInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteEndpoint = Request.request serviceName "deleteEndpoint" 


-- | <p>Deletes a platform application object for one of the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
deletePlatformApplication :: forall eff. DeletePlatformApplicationInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deletePlatformApplication = Request.request serviceName "deletePlatformApplication" 


-- | <p>Deletes a topic and all its subscriptions. Deleting a topic might prevent some messages previously sent to the topic from being delivered to subscribers. This action is idempotent, so deleting a topic that does not exist does not result in an error.</p>
deleteTopic :: forall eff. DeleteTopicInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteTopic = Request.request serviceName "deleteTopic" 


-- | <p>Retrieves the endpoint attributes for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
getEndpointAttributes :: forall eff. GetEndpointAttributesInput -> Aff (exception :: EXCEPTION | eff) GetEndpointAttributesResponse
getEndpointAttributes = Request.request serviceName "getEndpointAttributes" 


-- | <p>Retrieves the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
getPlatformApplicationAttributes :: forall eff. GetPlatformApplicationAttributesInput -> Aff (exception :: EXCEPTION | eff) GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributes = Request.request serviceName "getPlatformApplicationAttributes" 


-- | <p>Returns the settings for sending SMS messages from your account.</p> <p>These settings are set with the <code>SetSMSAttributes</code> action.</p>
getSMSAttributes :: forall eff. GetSMSAttributesInput -> Aff (exception :: EXCEPTION | eff) GetSMSAttributesResponse
getSMSAttributes = Request.request serviceName "getSMSAttributes" 


-- | <p>Returns all of the properties of a subscription.</p>
getSubscriptionAttributes :: forall eff. GetSubscriptionAttributesInput -> Aff (exception :: EXCEPTION | eff) GetSubscriptionAttributesResponse
getSubscriptionAttributes = Request.request serviceName "getSubscriptionAttributes" 


-- | <p>Returns all of the properties of a topic. Topic properties returned might differ based on the authorization of the user.</p>
getTopicAttributes :: forall eff. GetTopicAttributesInput -> Aff (exception :: EXCEPTION | eff) GetTopicAttributesResponse
getTopicAttributes = Request.request serviceName "getTopicAttributes" 


-- | <p>Lists the endpoints and endpoint attributes for devices in a supported push notification service, such as GCM and APNS. The results for <code>ListEndpointsByPlatformApplication</code> are paginated and return a limited list of endpoints, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call <code>ListEndpointsByPlatformApplication</code> again using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
listEndpointsByPlatformApplication :: forall eff. ListEndpointsByPlatformApplicationInput -> Aff (exception :: EXCEPTION | eff) ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplication = Request.request serviceName "listEndpointsByPlatformApplication" 


-- | <p>Returns a list of phone numbers that are opted out, meaning you cannot send SMS messages to them.</p> <p>The results for <code>ListPhoneNumbersOptedOut</code> are paginated, and each page returns up to 100 phone numbers. If additional phone numbers are available after the first page of results, then a <code>NextToken</code> string will be returned. To receive the next page, you call <code>ListPhoneNumbersOptedOut</code> again using the <code>NextToken</code> string received from the previous call. When there are no more records to return, <code>NextToken</code> will be null.</p>
listPhoneNumbersOptedOut :: forall eff. ListPhoneNumbersOptedOutInput -> Aff (exception :: EXCEPTION | eff) ListPhoneNumbersOptedOutResponse
listPhoneNumbersOptedOut = Request.request serviceName "listPhoneNumbersOptedOut" 


-- | <p>Lists the platform application objects for the supported push notification services, such as APNS and GCM. The results for <code>ListPlatformApplications</code> are paginated and return a limited list of applications, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call <code>ListPlatformApplications</code> using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
listPlatformApplications :: forall eff. ListPlatformApplicationsInput -> Aff (exception :: EXCEPTION | eff) ListPlatformApplicationsResponse
listPlatformApplications = Request.request serviceName "listPlatformApplications" 


-- | <p>Returns a list of the requester's subscriptions. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListSubscriptions</code> call to get further results.</p>
listSubscriptions :: forall eff. ListSubscriptionsInput -> Aff (exception :: EXCEPTION | eff) ListSubscriptionsResponse
listSubscriptions = Request.request serviceName "listSubscriptions" 


-- | <p>Returns a list of the subscriptions to a specific topic. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListSubscriptionsByTopic</code> call to get further results.</p>
listSubscriptionsByTopic :: forall eff. ListSubscriptionsByTopicInput -> Aff (exception :: EXCEPTION | eff) ListSubscriptionsByTopicResponse
listSubscriptionsByTopic = Request.request serviceName "listSubscriptionsByTopic" 


-- | <p>Returns a list of the requester's topics. Each call returns a limited list of topics, up to 100. If there are more topics, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListTopics</code> call to get further results.</p>
listTopics :: forall eff. ListTopicsInput -> Aff (exception :: EXCEPTION | eff) ListTopicsResponse
listTopics = Request.request serviceName "listTopics" 


-- | <p>Use this request to opt in a phone number that is opted out, which enables you to resume sending SMS messages to the number.</p> <p>You can opt in a phone number only once every 30 days.</p>
optInPhoneNumber :: forall eff. OptInPhoneNumberInput -> Aff (exception :: EXCEPTION | eff) OptInPhoneNumberResponse
optInPhoneNumber = Request.request serviceName "optInPhoneNumber" 


-- | <p>Sends a message to all of a topic's subscribed endpoints. When a <code>messageId</code> is returned, the message has been saved and Amazon SNS will attempt to deliver it to the topic's subscribers shortly. The format of the outgoing message to each subscribed endpoint depends on the notification protocol.</p> <p>To use the <code>Publish</code> action for sending a message to a mobile endpoint, such as an app on a Kindle device or mobile phone, you must specify the EndpointArn for the TargetArn parameter. The EndpointArn is returned when making a call with the <code>CreatePlatformEndpoint</code> action. </p> <p>For more information about formatting messages, see <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-send-custommessage.html">Send Custom Platform-Specific Payloads in Messages to Mobile Devices</a>. </p>
publish :: forall eff. PublishInput -> Aff (exception :: EXCEPTION | eff) PublishResponse
publish = Request.request serviceName "publish" 


-- | <p>Removes a statement from a topic's access control policy.</p>
removePermission :: forall eff. RemovePermissionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removePermission = Request.request serviceName "removePermission" 


-- | <p>Sets the attributes for an endpoint for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
setEndpointAttributes :: forall eff. SetEndpointAttributesInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setEndpointAttributes = Request.request serviceName "setEndpointAttributes" 


-- | <p>Sets the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. For information on configuring attributes for message delivery status, see <a href="http://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html">Using Amazon SNS Application Attributes for Message Delivery Status</a>. </p>
setPlatformApplicationAttributes :: forall eff. SetPlatformApplicationAttributesInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setPlatformApplicationAttributes = Request.request serviceName "setPlatformApplicationAttributes" 


-- | <p>Use this request to set the default settings for sending SMS messages and receiving daily SMS usage reports.</p> <p>You can override some of these settings for a single message when you use the <code>Publish</code> action with the <code>MessageAttributes.entry.N</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html">Sending an SMS Message</a> in the <i>Amazon SNS Developer Guide</i>.</p>
setSMSAttributes :: forall eff. SetSMSAttributesInput -> Aff (exception :: EXCEPTION | eff) SetSMSAttributesResponse
setSMSAttributes = Request.request serviceName "setSMSAttributes" 


-- | <p>Allows a subscription owner to set an attribute of the topic to a new value.</p>
setSubscriptionAttributes :: forall eff. SetSubscriptionAttributesInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setSubscriptionAttributes = Request.request serviceName "setSubscriptionAttributes" 


-- | <p>Allows a topic owner to set an attribute of the topic to a new value.</p>
setTopicAttributes :: forall eff. SetTopicAttributesInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setTopicAttributes = Request.request serviceName "setTopicAttributes" 


-- | <p>Prepares to subscribe an endpoint by sending the endpoint a confirmation message. To actually create a subscription, the endpoint owner must call the <code>ConfirmSubscription</code> action with the token from the confirmation message. Confirmation tokens are valid for three days.</p>
subscribe :: forall eff. SubscribeInput -> Aff (exception :: EXCEPTION | eff) SubscribeResponse
subscribe = Request.request serviceName "subscribe" 


-- | <p>Deletes a subscription. If the subscription requires authentication for deletion, only the owner of the subscription or the topic's owner can unsubscribe, and an AWS signature is required. If the <code>Unsubscribe</code> call does not require authentication and the requester is not the subscription owner, a final cancellation message is delivered to the endpoint, so that the endpoint owner can easily resubscribe to the topic if the <code>Unsubscribe</code> request was unintended.</p>
unsubscribe :: forall eff. UnsubscribeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
unsubscribe = Request.request serviceName "unsubscribe" 


newtype ActionsList = ActionsList (Array Action')
derive instance newtypeActionsList :: Newtype ActionsList _
derive instance repGenericActionsList :: Generic ActionsList _
instance showActionsList :: Show ActionsList where
  show = genericShow
instance decodeActionsList :: Decode ActionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionsList :: Encode ActionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddPermissionInput = AddPermissionInput 
  { "TopicArn" :: (TopicARN')
  , "Label" :: (Label')
  , "AWSAccountId" :: (DelegatesList)
  , "ActionName" :: (ActionsList)
  }
derive instance newtypeAddPermissionInput :: Newtype AddPermissionInput _
derive instance repGenericAddPermissionInput :: Generic AddPermissionInput _
instance showAddPermissionInput :: Show AddPermissionInput where
  show = genericShow
instance decodeAddPermissionInput :: Decode AddPermissionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddPermissionInput :: Encode AddPermissionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the user has been denied access to the requested resource.</p>
newtype AuthorizationErrorException = AuthorizationErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAuthorizationErrorException :: Newtype AuthorizationErrorException _
derive instance repGenericAuthorizationErrorException :: Generic AuthorizationErrorException _
instance showAuthorizationErrorException :: Show AuthorizationErrorException where
  show = genericShow
instance decodeAuthorizationErrorException :: Decode AuthorizationErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationErrorException :: Encode AuthorizationErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Binary = Binary String
derive instance newtypeBinary :: Newtype Binary _
derive instance repGenericBinary :: Generic Binary _
instance showBinary :: Show Binary where
  show = genericShow
instance decodeBinary :: Decode Binary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinary :: Encode Binary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <code>CheckIfPhoneNumberIsOptedOut</code> action.</p>
newtype CheckIfPhoneNumberIsOptedOutInput = CheckIfPhoneNumberIsOptedOutInput 
  { "PhoneNumber'" :: (PhoneNumber)
  }
derive instance newtypeCheckIfPhoneNumberIsOptedOutInput :: Newtype CheckIfPhoneNumberIsOptedOutInput _
derive instance repGenericCheckIfPhoneNumberIsOptedOutInput :: Generic CheckIfPhoneNumberIsOptedOutInput _
instance showCheckIfPhoneNumberIsOptedOutInput :: Show CheckIfPhoneNumberIsOptedOutInput where
  show = genericShow
instance decodeCheckIfPhoneNumberIsOptedOutInput :: Decode CheckIfPhoneNumberIsOptedOutInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCheckIfPhoneNumberIsOptedOutInput :: Encode CheckIfPhoneNumberIsOptedOutInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the <code>CheckIfPhoneNumberIsOptedOut</code> action.</p>
newtype CheckIfPhoneNumberIsOptedOutResponse = CheckIfPhoneNumberIsOptedOutResponse 
  { "IsOptedOut'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCheckIfPhoneNumberIsOptedOutResponse :: Newtype CheckIfPhoneNumberIsOptedOutResponse _
derive instance repGenericCheckIfPhoneNumberIsOptedOutResponse :: Generic CheckIfPhoneNumberIsOptedOutResponse _
instance showCheckIfPhoneNumberIsOptedOutResponse :: Show CheckIfPhoneNumberIsOptedOutResponse where
  show = genericShow
instance decodeCheckIfPhoneNumberIsOptedOutResponse :: Decode CheckIfPhoneNumberIsOptedOutResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCheckIfPhoneNumberIsOptedOutResponse :: Encode CheckIfPhoneNumberIsOptedOutResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for ConfirmSubscription action.</p>
newtype ConfirmSubscriptionInput = ConfirmSubscriptionInput 
  { "TopicArn" :: (TopicARN')
  , "Token" :: (Token')
  , "AuthenticateOnUnsubscribe" :: NullOrUndefined.NullOrUndefined (AuthenticateOnUnsubscribe')
  }
derive instance newtypeConfirmSubscriptionInput :: Newtype ConfirmSubscriptionInput _
derive instance repGenericConfirmSubscriptionInput :: Generic ConfirmSubscriptionInput _
instance showConfirmSubscriptionInput :: Show ConfirmSubscriptionInput where
  show = genericShow
instance decodeConfirmSubscriptionInput :: Decode ConfirmSubscriptionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmSubscriptionInput :: Encode ConfirmSubscriptionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ConfirmSubscriptions action.</p>
newtype ConfirmSubscriptionResponse = ConfirmSubscriptionResponse 
  { "SubscriptionArn" :: NullOrUndefined.NullOrUndefined (SubscriptionARN')
  }
derive instance newtypeConfirmSubscriptionResponse :: Newtype ConfirmSubscriptionResponse _
derive instance repGenericConfirmSubscriptionResponse :: Generic ConfirmSubscriptionResponse _
instance showConfirmSubscriptionResponse :: Show ConfirmSubscriptionResponse where
  show = genericShow
instance decodeConfirmSubscriptionResponse :: Decode ConfirmSubscriptionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmSubscriptionResponse :: Encode ConfirmSubscriptionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response from CreateEndpoint action.</p>
newtype CreateEndpointResponse = CreateEndpointResponse 
  { "EndpointArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateEndpointResponse :: Newtype CreateEndpointResponse _
derive instance repGenericCreateEndpointResponse :: Generic CreateEndpointResponse _
instance showCreateEndpointResponse :: Show CreateEndpointResponse where
  show = genericShow
instance decodeCreateEndpointResponse :: Decode CreateEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEndpointResponse :: Encode CreateEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for CreatePlatformApplication action.</p>
newtype CreatePlatformApplicationInput = CreatePlatformApplicationInput 
  { "Name" :: (String)
  , "Platform" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeCreatePlatformApplicationInput :: Newtype CreatePlatformApplicationInput _
derive instance repGenericCreatePlatformApplicationInput :: Generic CreatePlatformApplicationInput _
instance showCreatePlatformApplicationInput :: Show CreatePlatformApplicationInput where
  show = genericShow
instance decodeCreatePlatformApplicationInput :: Decode CreatePlatformApplicationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlatformApplicationInput :: Encode CreatePlatformApplicationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response from CreatePlatformApplication action.</p>
newtype CreatePlatformApplicationResponse = CreatePlatformApplicationResponse 
  { "PlatformApplicationArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreatePlatformApplicationResponse :: Newtype CreatePlatformApplicationResponse _
derive instance repGenericCreatePlatformApplicationResponse :: Generic CreatePlatformApplicationResponse _
instance showCreatePlatformApplicationResponse :: Show CreatePlatformApplicationResponse where
  show = genericShow
instance decodeCreatePlatformApplicationResponse :: Decode CreatePlatformApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlatformApplicationResponse :: Encode CreatePlatformApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for CreatePlatformEndpoint action.</p>
newtype CreatePlatformEndpointInput = CreatePlatformEndpointInput 
  { "PlatformApplicationArn" :: (String)
  , "Token" :: (String)
  , "CustomUserData" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypeCreatePlatformEndpointInput :: Newtype CreatePlatformEndpointInput _
derive instance repGenericCreatePlatformEndpointInput :: Generic CreatePlatformEndpointInput _
instance showCreatePlatformEndpointInput :: Show CreatePlatformEndpointInput where
  show = genericShow
instance decodeCreatePlatformEndpointInput :: Decode CreatePlatformEndpointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlatformEndpointInput :: Encode CreatePlatformEndpointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for CreateTopic action.</p>
newtype CreateTopicInput = CreateTopicInput 
  { "Name" :: (TopicName')
  }
derive instance newtypeCreateTopicInput :: Newtype CreateTopicInput _
derive instance repGenericCreateTopicInput :: Generic CreateTopicInput _
instance showCreateTopicInput :: Show CreateTopicInput where
  show = genericShow
instance decodeCreateTopicInput :: Decode CreateTopicInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTopicInput :: Encode CreateTopicInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response from CreateTopic action.</p>
newtype CreateTopicResponse = CreateTopicResponse 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (TopicARN')
  }
derive instance newtypeCreateTopicResponse :: Newtype CreateTopicResponse _
derive instance repGenericCreateTopicResponse :: Generic CreateTopicResponse _
instance showCreateTopicResponse :: Show CreateTopicResponse where
  show = genericShow
instance decodeCreateTopicResponse :: Decode CreateTopicResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTopicResponse :: Encode CreateTopicResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DelegatesList = DelegatesList (Array Delegate')
derive instance newtypeDelegatesList :: Newtype DelegatesList _
derive instance repGenericDelegatesList :: Generic DelegatesList _
instance showDelegatesList :: Show DelegatesList where
  show = genericShow
instance decodeDelegatesList :: Decode DelegatesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegatesList :: Encode DelegatesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for DeleteEndpoint action.</p>
newtype DeleteEndpointInput = DeleteEndpointInput 
  { "EndpointArn" :: (String)
  }
derive instance newtypeDeleteEndpointInput :: Newtype DeleteEndpointInput _
derive instance repGenericDeleteEndpointInput :: Generic DeleteEndpointInput _
instance showDeleteEndpointInput :: Show DeleteEndpointInput where
  show = genericShow
instance decodeDeleteEndpointInput :: Decode DeleteEndpointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEndpointInput :: Encode DeleteEndpointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for DeletePlatformApplication action.</p>
newtype DeletePlatformApplicationInput = DeletePlatformApplicationInput 
  { "PlatformApplicationArn" :: (String)
  }
derive instance newtypeDeletePlatformApplicationInput :: Newtype DeletePlatformApplicationInput _
derive instance repGenericDeletePlatformApplicationInput :: Generic DeletePlatformApplicationInput _
instance showDeletePlatformApplicationInput :: Show DeletePlatformApplicationInput where
  show = genericShow
instance decodeDeletePlatformApplicationInput :: Decode DeletePlatformApplicationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePlatformApplicationInput :: Encode DeletePlatformApplicationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTopicInput = DeleteTopicInput 
  { "TopicArn" :: (TopicARN')
  }
derive instance newtypeDeleteTopicInput :: Newtype DeleteTopicInput _
derive instance repGenericDeleteTopicInput :: Generic DeleteTopicInput _
instance showDeleteTopicInput :: Show DeleteTopicInput where
  show = genericShow
instance decodeDeleteTopicInput :: Decode DeleteTopicInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTopicInput :: Encode DeleteTopicInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Endpoint for mobile app and device.</p>
newtype Endpoint = Endpoint 
  { "EndpointArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _
derive instance repGenericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where
  show = genericShow
instance decodeEndpoint :: Decode Endpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint :: Encode Endpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exception error indicating endpoint disabled.</p>
newtype EndpointDisabledException = EndpointDisabledException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointDisabledException :: Newtype EndpointDisabledException _
derive instance repGenericEndpointDisabledException :: Generic EndpointDisabledException _
instance showEndpointDisabledException :: Show EndpointDisabledException where
  show = genericShow
instance decodeEndpointDisabledException :: Decode EndpointDisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointDisabledException :: Encode EndpointDisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetEndpointAttributes action.</p>
newtype GetEndpointAttributesInput = GetEndpointAttributesInput 
  { "EndpointArn" :: (String)
  }
derive instance newtypeGetEndpointAttributesInput :: Newtype GetEndpointAttributesInput _
derive instance repGenericGetEndpointAttributesInput :: Generic GetEndpointAttributesInput _
instance showGetEndpointAttributesInput :: Show GetEndpointAttributesInput where
  show = genericShow
instance decodeGetEndpointAttributesInput :: Decode GetEndpointAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEndpointAttributesInput :: Encode GetEndpointAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response from GetEndpointAttributes of the EndpointArn.</p>
newtype GetEndpointAttributesResponse = GetEndpointAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetEndpointAttributesResponse :: Newtype GetEndpointAttributesResponse _
derive instance repGenericGetEndpointAttributesResponse :: Generic GetEndpointAttributesResponse _
instance showGetEndpointAttributesResponse :: Show GetEndpointAttributesResponse where
  show = genericShow
instance decodeGetEndpointAttributesResponse :: Decode GetEndpointAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEndpointAttributesResponse :: Encode GetEndpointAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetPlatformApplicationAttributes action.</p>
newtype GetPlatformApplicationAttributesInput = GetPlatformApplicationAttributesInput 
  { "PlatformApplicationArn" :: (String)
  }
derive instance newtypeGetPlatformApplicationAttributesInput :: Newtype GetPlatformApplicationAttributesInput _
derive instance repGenericGetPlatformApplicationAttributesInput :: Generic GetPlatformApplicationAttributesInput _
instance showGetPlatformApplicationAttributesInput :: Show GetPlatformApplicationAttributesInput where
  show = genericShow
instance decodeGetPlatformApplicationAttributesInput :: Decode GetPlatformApplicationAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPlatformApplicationAttributesInput :: Encode GetPlatformApplicationAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for GetPlatformApplicationAttributes action.</p>
newtype GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetPlatformApplicationAttributesResponse :: Newtype GetPlatformApplicationAttributesResponse _
derive instance repGenericGetPlatformApplicationAttributesResponse :: Generic GetPlatformApplicationAttributesResponse _
instance showGetPlatformApplicationAttributesResponse :: Show GetPlatformApplicationAttributesResponse where
  show = genericShow
instance decodeGetPlatformApplicationAttributesResponse :: Decode GetPlatformApplicationAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPlatformApplicationAttributesResponse :: Encode GetPlatformApplicationAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <code>GetSMSAttributes</code> request.</p>
newtype GetSMSAttributesInput = GetSMSAttributesInput 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (ListString)
  }
derive instance newtypeGetSMSAttributesInput :: Newtype GetSMSAttributesInput _
derive instance repGenericGetSMSAttributesInput :: Generic GetSMSAttributesInput _
instance showGetSMSAttributesInput :: Show GetSMSAttributesInput where
  show = genericShow
instance decodeGetSMSAttributesInput :: Decode GetSMSAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSMSAttributesInput :: Encode GetSMSAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the <code>GetSMSAttributes</code> request.</p>
newtype GetSMSAttributesResponse = GetSMSAttributesResponse 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetSMSAttributesResponse :: Newtype GetSMSAttributesResponse _
derive instance repGenericGetSMSAttributesResponse :: Generic GetSMSAttributesResponse _
instance showGetSMSAttributesResponse :: Show GetSMSAttributesResponse where
  show = genericShow
instance decodeGetSMSAttributesResponse :: Decode GetSMSAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSMSAttributesResponse :: Encode GetSMSAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetSubscriptionAttributes.</p>
newtype GetSubscriptionAttributesInput = GetSubscriptionAttributesInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  }
derive instance newtypeGetSubscriptionAttributesInput :: Newtype GetSubscriptionAttributesInput _
derive instance repGenericGetSubscriptionAttributesInput :: Generic GetSubscriptionAttributesInput _
instance showGetSubscriptionAttributesInput :: Show GetSubscriptionAttributesInput where
  show = genericShow
instance decodeGetSubscriptionAttributesInput :: Decode GetSubscriptionAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionAttributesInput :: Encode GetSubscriptionAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for GetSubscriptionAttributes action.</p>
newtype GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (SubscriptionAttributesMap)
  }
derive instance newtypeGetSubscriptionAttributesResponse :: Newtype GetSubscriptionAttributesResponse _
derive instance repGenericGetSubscriptionAttributesResponse :: Generic GetSubscriptionAttributesResponse _
instance showGetSubscriptionAttributesResponse :: Show GetSubscriptionAttributesResponse where
  show = genericShow
instance decodeGetSubscriptionAttributesResponse :: Decode GetSubscriptionAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSubscriptionAttributesResponse :: Encode GetSubscriptionAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for GetTopicAttributes action.</p>
newtype GetTopicAttributesInput = GetTopicAttributesInput 
  { "TopicArn" :: (TopicARN')
  }
derive instance newtypeGetTopicAttributesInput :: Newtype GetTopicAttributesInput _
derive instance repGenericGetTopicAttributesInput :: Generic GetTopicAttributesInput _
instance showGetTopicAttributesInput :: Show GetTopicAttributesInput where
  show = genericShow
instance decodeGetTopicAttributesInput :: Decode GetTopicAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTopicAttributesInput :: Encode GetTopicAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for GetTopicAttributes action.</p>
newtype GetTopicAttributesResponse = GetTopicAttributesResponse 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (TopicAttributesMap)
  }
derive instance newtypeGetTopicAttributesResponse :: Newtype GetTopicAttributesResponse _
derive instance repGenericGetTopicAttributesResponse :: Generic GetTopicAttributesResponse _
instance showGetTopicAttributesResponse :: Show GetTopicAttributesResponse where
  show = genericShow
instance decodeGetTopicAttributesResponse :: Decode GetTopicAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTopicAttributesResponse :: Encode GetTopicAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates an internal service error.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a request parameter does not comply with the associated constraints.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that a request parameter does not comply with the associated constraints.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for ListEndpointsByPlatformApplication action.</p>
newtype ListEndpointsByPlatformApplicationInput = ListEndpointsByPlatformApplicationInput 
  { "PlatformApplicationArn" :: (String)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListEndpointsByPlatformApplicationInput :: Newtype ListEndpointsByPlatformApplicationInput _
derive instance repGenericListEndpointsByPlatformApplicationInput :: Generic ListEndpointsByPlatformApplicationInput _
instance showListEndpointsByPlatformApplicationInput :: Show ListEndpointsByPlatformApplicationInput where
  show = genericShow
instance decodeListEndpointsByPlatformApplicationInput :: Decode ListEndpointsByPlatformApplicationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListEndpointsByPlatformApplicationInput :: Encode ListEndpointsByPlatformApplicationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ListEndpointsByPlatformApplication action.</p>
newtype ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse 
  { "Endpoints" :: NullOrUndefined.NullOrUndefined (ListOfEndpoints)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListEndpointsByPlatformApplicationResponse :: Newtype ListEndpointsByPlatformApplicationResponse _
derive instance repGenericListEndpointsByPlatformApplicationResponse :: Generic ListEndpointsByPlatformApplicationResponse _
instance showListEndpointsByPlatformApplicationResponse :: Show ListEndpointsByPlatformApplicationResponse where
  show = genericShow
instance decodeListEndpointsByPlatformApplicationResponse :: Decode ListEndpointsByPlatformApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListEndpointsByPlatformApplicationResponse :: Encode ListEndpointsByPlatformApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfEndpoints = ListOfEndpoints (Array Endpoint)
derive instance newtypeListOfEndpoints :: Newtype ListOfEndpoints _
derive instance repGenericListOfEndpoints :: Generic ListOfEndpoints _
instance showListOfEndpoints :: Show ListOfEndpoints where
  show = genericShow
instance decodeListOfEndpoints :: Decode ListOfEndpoints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfEndpoints :: Encode ListOfEndpoints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfPlatformApplications = ListOfPlatformApplications (Array PlatformApplication)
derive instance newtypeListOfPlatformApplications :: Newtype ListOfPlatformApplications _
derive instance repGenericListOfPlatformApplications :: Generic ListOfPlatformApplications _
instance showListOfPlatformApplications :: Show ListOfPlatformApplications where
  show = genericShow
instance decodeListOfPlatformApplications :: Decode ListOfPlatformApplications where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfPlatformApplications :: Encode ListOfPlatformApplications where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <code>ListPhoneNumbersOptedOut</code> action.</p>
newtype ListPhoneNumbersOptedOutInput = ListPhoneNumbersOptedOutInput 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPhoneNumbersOptedOutInput :: Newtype ListPhoneNumbersOptedOutInput _
derive instance repGenericListPhoneNumbersOptedOutInput :: Generic ListPhoneNumbersOptedOutInput _
instance showListPhoneNumbersOptedOutInput :: Show ListPhoneNumbersOptedOutInput where
  show = genericShow
instance decodeListPhoneNumbersOptedOutInput :: Decode ListPhoneNumbersOptedOutInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPhoneNumbersOptedOutInput :: Encode ListPhoneNumbersOptedOutInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the <code>ListPhoneNumbersOptedOut</code> action.</p>
newtype ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse 
  { "PhoneNumbers'" :: NullOrUndefined.NullOrUndefined (PhoneNumberList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPhoneNumbersOptedOutResponse :: Newtype ListPhoneNumbersOptedOutResponse _
derive instance repGenericListPhoneNumbersOptedOutResponse :: Generic ListPhoneNumbersOptedOutResponse _
instance showListPhoneNumbersOptedOutResponse :: Show ListPhoneNumbersOptedOutResponse where
  show = genericShow
instance decodeListPhoneNumbersOptedOutResponse :: Decode ListPhoneNumbersOptedOutResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPhoneNumbersOptedOutResponse :: Encode ListPhoneNumbersOptedOutResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for ListPlatformApplications action.</p>
newtype ListPlatformApplicationsInput = ListPlatformApplicationsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPlatformApplicationsInput :: Newtype ListPlatformApplicationsInput _
derive instance repGenericListPlatformApplicationsInput :: Generic ListPlatformApplicationsInput _
instance showListPlatformApplicationsInput :: Show ListPlatformApplicationsInput where
  show = genericShow
instance decodeListPlatformApplicationsInput :: Decode ListPlatformApplicationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPlatformApplicationsInput :: Encode ListPlatformApplicationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ListPlatformApplications action.</p>
newtype ListPlatformApplicationsResponse = ListPlatformApplicationsResponse 
  { "PlatformApplications" :: NullOrUndefined.NullOrUndefined (ListOfPlatformApplications)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPlatformApplicationsResponse :: Newtype ListPlatformApplicationsResponse _
derive instance repGenericListPlatformApplicationsResponse :: Generic ListPlatformApplicationsResponse _
instance showListPlatformApplicationsResponse :: Show ListPlatformApplicationsResponse where
  show = genericShow
instance decodeListPlatformApplicationsResponse :: Decode ListPlatformApplicationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPlatformApplicationsResponse :: Encode ListPlatformApplicationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListString = ListString (Array String)
derive instance newtypeListString :: Newtype ListString _
derive instance repGenericListString :: Generic ListString _
instance showListString :: Show ListString where
  show = genericShow
instance decodeListString :: Decode ListString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListString :: Encode ListString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for ListSubscriptionsByTopic action.</p>
newtype ListSubscriptionsByTopicInput = ListSubscriptionsByTopicInput 
  { "TopicArn" :: (TopicARN')
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsByTopicInput :: Newtype ListSubscriptionsByTopicInput _
derive instance repGenericListSubscriptionsByTopicInput :: Generic ListSubscriptionsByTopicInput _
instance showListSubscriptionsByTopicInput :: Show ListSubscriptionsByTopicInput where
  show = genericShow
instance decodeListSubscriptionsByTopicInput :: Decode ListSubscriptionsByTopicInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionsByTopicInput :: Encode ListSubscriptionsByTopicInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ListSubscriptionsByTopic action.</p>
newtype ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse 
  { "Subscriptions" :: NullOrUndefined.NullOrUndefined (SubscriptionsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsByTopicResponse :: Newtype ListSubscriptionsByTopicResponse _
derive instance repGenericListSubscriptionsByTopicResponse :: Generic ListSubscriptionsByTopicResponse _
instance showListSubscriptionsByTopicResponse :: Show ListSubscriptionsByTopicResponse where
  show = genericShow
instance decodeListSubscriptionsByTopicResponse :: Decode ListSubscriptionsByTopicResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionsByTopicResponse :: Encode ListSubscriptionsByTopicResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for ListSubscriptions action.</p>
newtype ListSubscriptionsInput = ListSubscriptionsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsInput :: Newtype ListSubscriptionsInput _
derive instance repGenericListSubscriptionsInput :: Generic ListSubscriptionsInput _
instance showListSubscriptionsInput :: Show ListSubscriptionsInput where
  show = genericShow
instance decodeListSubscriptionsInput :: Decode ListSubscriptionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionsInput :: Encode ListSubscriptionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ListSubscriptions action</p>
newtype ListSubscriptionsResponse = ListSubscriptionsResponse 
  { "Subscriptions" :: NullOrUndefined.NullOrUndefined (SubscriptionsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsResponse :: Newtype ListSubscriptionsResponse _
derive instance repGenericListSubscriptionsResponse :: Generic ListSubscriptionsResponse _
instance showListSubscriptionsResponse :: Show ListSubscriptionsResponse where
  show = genericShow
instance decodeListSubscriptionsResponse :: Decode ListSubscriptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSubscriptionsResponse :: Encode ListSubscriptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTopicsInput = ListTopicsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListTopicsInput :: Newtype ListTopicsInput _
derive instance repGenericListTopicsInput :: Generic ListTopicsInput _
instance showListTopicsInput :: Show ListTopicsInput where
  show = genericShow
instance decodeListTopicsInput :: Decode ListTopicsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTopicsInput :: Encode ListTopicsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for ListTopics action.</p>
newtype ListTopicsResponse = ListTopicsResponse 
  { "Topics" :: NullOrUndefined.NullOrUndefined (TopicsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeListTopicsResponse :: Newtype ListTopicsResponse _
derive instance repGenericListTopicsResponse :: Generic ListTopicsResponse _
instance showListTopicsResponse :: Show ListTopicsResponse where
  show = genericShow
instance decodeListTopicsResponse :: Decode ListTopicsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTopicsResponse :: Encode ListTopicsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapStringToString = MapStringToString (StrMap.StrMap String)
derive instance newtypeMapStringToString :: Newtype MapStringToString _
derive instance repGenericMapStringToString :: Generic MapStringToString _
instance showMapStringToString :: Show MapStringToString where
  show = genericShow
instance decodeMapStringToString :: Decode MapStringToString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapStringToString :: Encode MapStringToString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageAttributeMap = MessageAttributeMap (StrMap.StrMap MessageAttributeValue)
derive instance newtypeMessageAttributeMap :: Newtype MessageAttributeMap _
derive instance repGenericMessageAttributeMap :: Generic MessageAttributeMap _
instance showMessageAttributeMap :: Show MessageAttributeMap where
  show = genericShow
instance decodeMessageAttributeMap :: Decode MessageAttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageAttributeMap :: Encode MessageAttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/api/API_Publish.html">Publish</a>.</p> <p>Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html">Using Amazon SNS Message Attributes</a>.</p>
newtype MessageAttributeValue = MessageAttributeValue 
  { "DataType" :: (String)
  , "StringValue" :: NullOrUndefined.NullOrUndefined (String)
  , "BinaryValue" :: NullOrUndefined.NullOrUndefined (Binary)
  }
derive instance newtypeMessageAttributeValue :: Newtype MessageAttributeValue _
derive instance repGenericMessageAttributeValue :: Generic MessageAttributeValue _
instance showMessageAttributeValue :: Show MessageAttributeValue where
  show = genericShow
instance decodeMessageAttributeValue :: Decode MessageAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageAttributeValue :: Encode MessageAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the requested resource does not exist.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for the OptInPhoneNumber action.</p>
newtype OptInPhoneNumberInput = OptInPhoneNumberInput 
  { "PhoneNumber'" :: (PhoneNumber)
  }
derive instance newtypeOptInPhoneNumberInput :: Newtype OptInPhoneNumberInput _
derive instance repGenericOptInPhoneNumberInput :: Generic OptInPhoneNumberInput _
instance showOptInPhoneNumberInput :: Show OptInPhoneNumberInput where
  show = genericShow
instance decodeOptInPhoneNumberInput :: Decode OptInPhoneNumberInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptInPhoneNumberInput :: Encode OptInPhoneNumberInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response for the OptInPhoneNumber action.</p>
newtype OptInPhoneNumberResponse = OptInPhoneNumberResponse Types.NoArguments
derive instance newtypeOptInPhoneNumberResponse :: Newtype OptInPhoneNumberResponse _
derive instance repGenericOptInPhoneNumberResponse :: Generic OptInPhoneNumberResponse _
instance showOptInPhoneNumberResponse :: Show OptInPhoneNumberResponse where
  show = genericShow
instance decodeOptInPhoneNumberResponse :: Decode OptInPhoneNumberResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptInPhoneNumberResponse :: Encode OptInPhoneNumberResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PhoneNumber = PhoneNumber String
derive instance newtypePhoneNumber :: Newtype PhoneNumber _
derive instance repGenericPhoneNumber :: Generic PhoneNumber _
instance showPhoneNumber :: Show PhoneNumber where
  show = genericShow
instance decodePhoneNumber :: Decode PhoneNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhoneNumber :: Encode PhoneNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PhoneNumberList = PhoneNumberList (Array PhoneNumber)
derive instance newtypePhoneNumberList :: Newtype PhoneNumberList _
derive instance repGenericPhoneNumberList :: Generic PhoneNumberList _
instance showPhoneNumberList :: Show PhoneNumberList where
  show = genericShow
instance decodePhoneNumberList :: Decode PhoneNumberList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhoneNumberList :: Encode PhoneNumberList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Platform application object.</p>
newtype PlatformApplication = PlatformApplication 
  { "PlatformApplicationArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapStringToString)
  }
derive instance newtypePlatformApplication :: Newtype PlatformApplication _
derive instance repGenericPlatformApplication :: Generic PlatformApplication _
instance showPlatformApplication :: Show PlatformApplication where
  show = genericShow
instance decodePlatformApplication :: Decode PlatformApplication where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformApplication :: Encode PlatformApplication where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exception error indicating platform application disabled.</p>
newtype PlatformApplicationDisabledException = PlatformApplicationDisabledException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePlatformApplicationDisabledException :: Newtype PlatformApplicationDisabledException _
derive instance repGenericPlatformApplicationDisabledException :: Generic PlatformApplicationDisabledException _
instance showPlatformApplicationDisabledException :: Show PlatformApplicationDisabledException where
  show = genericShow
instance decodePlatformApplicationDisabledException :: Decode PlatformApplicationDisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformApplicationDisabledException :: Encode PlatformApplicationDisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for Publish action.</p>
newtype PublishInput = PublishInput 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (TopicARN')
  , "TargetArn" :: NullOrUndefined.NullOrUndefined (String)
  , "PhoneNumber" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: (Message')
  , "Subject" :: NullOrUndefined.NullOrUndefined (Subject')
  , "MessageStructure" :: NullOrUndefined.NullOrUndefined (MessageStructure')
  , "MessageAttributes" :: NullOrUndefined.NullOrUndefined (MessageAttributeMap)
  }
derive instance newtypePublishInput :: Newtype PublishInput _
derive instance repGenericPublishInput :: Generic PublishInput _
instance showPublishInput :: Show PublishInput where
  show = genericShow
instance decodePublishInput :: Decode PublishInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublishInput :: Encode PublishInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for Publish action.</p>
newtype PublishResponse = PublishResponse 
  { "MessageId" :: NullOrUndefined.NullOrUndefined (MessageId')
  }
derive instance newtypePublishResponse :: Newtype PublishResponse _
derive instance repGenericPublishResponse :: Generic PublishResponse _
instance showPublishResponse :: Show PublishResponse where
  show = genericShow
instance decodePublishResponse :: Decode PublishResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublishResponse :: Encode PublishResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for RemovePermission action.</p>
newtype RemovePermissionInput = RemovePermissionInput 
  { "TopicArn" :: (TopicARN')
  , "Label" :: (Label')
  }
derive instance newtypeRemovePermissionInput :: Newtype RemovePermissionInput _
derive instance repGenericRemovePermissionInput :: Generic RemovePermissionInput _
instance showRemovePermissionInput :: Show RemovePermissionInput where
  show = genericShow
instance decodeRemovePermissionInput :: Decode RemovePermissionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemovePermissionInput :: Encode RemovePermissionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for SetEndpointAttributes action.</p>
newtype SetEndpointAttributesInput = SetEndpointAttributesInput 
  { "EndpointArn" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeSetEndpointAttributesInput :: Newtype SetEndpointAttributesInput _
derive instance repGenericSetEndpointAttributesInput :: Generic SetEndpointAttributesInput _
instance showSetEndpointAttributesInput :: Show SetEndpointAttributesInput where
  show = genericShow
instance decodeSetEndpointAttributesInput :: Decode SetEndpointAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetEndpointAttributesInput :: Encode SetEndpointAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for SetPlatformApplicationAttributes action.</p>
newtype SetPlatformApplicationAttributesInput = SetPlatformApplicationAttributesInput 
  { "PlatformApplicationArn" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeSetPlatformApplicationAttributesInput :: Newtype SetPlatformApplicationAttributesInput _
derive instance repGenericSetPlatformApplicationAttributesInput :: Generic SetPlatformApplicationAttributesInput _
instance showSetPlatformApplicationAttributesInput :: Show SetPlatformApplicationAttributesInput where
  show = genericShow
instance decodeSetPlatformApplicationAttributesInput :: Decode SetPlatformApplicationAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetPlatformApplicationAttributesInput :: Encode SetPlatformApplicationAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the SetSMSAttributes action.</p>
newtype SetSMSAttributesInput = SetSMSAttributesInput 
  { "Attributes'" :: (MapStringToString)
  }
derive instance newtypeSetSMSAttributesInput :: Newtype SetSMSAttributesInput _
derive instance repGenericSetSMSAttributesInput :: Generic SetSMSAttributesInput _
instance showSetSMSAttributesInput :: Show SetSMSAttributesInput where
  show = genericShow
instance decodeSetSMSAttributesInput :: Decode SetSMSAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSMSAttributesInput :: Encode SetSMSAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response for the SetSMSAttributes action.</p>
newtype SetSMSAttributesResponse = SetSMSAttributesResponse Types.NoArguments
derive instance newtypeSetSMSAttributesResponse :: Newtype SetSMSAttributesResponse _
derive instance repGenericSetSMSAttributesResponse :: Generic SetSMSAttributesResponse _
instance showSetSMSAttributesResponse :: Show SetSMSAttributesResponse where
  show = genericShow
instance decodeSetSMSAttributesResponse :: Decode SetSMSAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSMSAttributesResponse :: Encode SetSMSAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for SetSubscriptionAttributes action.</p>
newtype SetSubscriptionAttributesInput = SetSubscriptionAttributesInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  , "AttributeName" :: (AttributeName')
  , "AttributeValue" :: NullOrUndefined.NullOrUndefined (AttributeValue')
  }
derive instance newtypeSetSubscriptionAttributesInput :: Newtype SetSubscriptionAttributesInput _
derive instance repGenericSetSubscriptionAttributesInput :: Generic SetSubscriptionAttributesInput _
instance showSetSubscriptionAttributesInput :: Show SetSubscriptionAttributesInput where
  show = genericShow
instance decodeSetSubscriptionAttributesInput :: Decode SetSubscriptionAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSubscriptionAttributesInput :: Encode SetSubscriptionAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for SetTopicAttributes action.</p>
newtype SetTopicAttributesInput = SetTopicAttributesInput 
  { "TopicArn" :: (TopicARN')
  , "AttributeName" :: (AttributeName')
  , "AttributeValue" :: NullOrUndefined.NullOrUndefined (AttributeValue')
  }
derive instance newtypeSetTopicAttributesInput :: Newtype SetTopicAttributesInput _
derive instance repGenericSetTopicAttributesInput :: Generic SetTopicAttributesInput _
instance showSetTopicAttributesInput :: Show SetTopicAttributesInput where
  show = genericShow
instance decodeSetTopicAttributesInput :: Decode SetTopicAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetTopicAttributesInput :: Encode SetTopicAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for Subscribe action.</p>
newtype SubscribeInput = SubscribeInput 
  { "TopicArn" :: (TopicARN')
  , "Protocol" :: (Protocol')
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint')
  }
derive instance newtypeSubscribeInput :: Newtype SubscribeInput _
derive instance repGenericSubscribeInput :: Generic SubscribeInput _
instance showSubscribeInput :: Show SubscribeInput where
  show = genericShow
instance decodeSubscribeInput :: Decode SubscribeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribeInput :: Encode SubscribeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response for Subscribe action.</p>
newtype SubscribeResponse = SubscribeResponse 
  { "SubscriptionArn" :: NullOrUndefined.NullOrUndefined (SubscriptionARN')
  }
derive instance newtypeSubscribeResponse :: Newtype SubscribeResponse _
derive instance repGenericSubscribeResponse :: Generic SubscribeResponse _
instance showSubscribeResponse :: Show SubscribeResponse where
  show = genericShow
instance decodeSubscribeResponse :: Decode SubscribeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscribeResponse :: Encode SubscribeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A wrapper type for the attributes of an Amazon SNS subscription.</p>
newtype Subscription = Subscription 
  { "SubscriptionArn" :: NullOrUndefined.NullOrUndefined (SubscriptionARN')
  , "Owner" :: NullOrUndefined.NullOrUndefined (Account')
  , "Protocol" :: NullOrUndefined.NullOrUndefined (Protocol')
  , "Endpoint" :: NullOrUndefined.NullOrUndefined (Endpoint')
  , "TopicArn" :: NullOrUndefined.NullOrUndefined (TopicARN')
  }
derive instance newtypeSubscription :: Newtype Subscription _
derive instance repGenericSubscription :: Generic Subscription _
instance showSubscription :: Show Subscription where
  show = genericShow
instance decodeSubscription :: Decode Subscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscription :: Encode Subscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionAttributesMap = SubscriptionAttributesMap (StrMap.StrMap AttributeValue')
derive instance newtypeSubscriptionAttributesMap :: Newtype SubscriptionAttributesMap _
derive instance repGenericSubscriptionAttributesMap :: Generic SubscriptionAttributesMap _
instance showSubscriptionAttributesMap :: Show SubscriptionAttributesMap where
  show = genericShow
instance decodeSubscriptionAttributesMap :: Decode SubscriptionAttributesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionAttributesMap :: Encode SubscriptionAttributesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the customer already owns the maximum allowed number of subscriptions.</p>
newtype SubscriptionLimitExceededException = SubscriptionLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubscriptionLimitExceededException :: Newtype SubscriptionLimitExceededException _
derive instance repGenericSubscriptionLimitExceededException :: Generic SubscriptionLimitExceededException _
instance showSubscriptionLimitExceededException :: Show SubscriptionLimitExceededException where
  show = genericShow
instance decodeSubscriptionLimitExceededException :: Decode SubscriptionLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionLimitExceededException :: Encode SubscriptionLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionsList = SubscriptionsList (Array Subscription)
derive instance newtypeSubscriptionsList :: Newtype SubscriptionsList _
derive instance repGenericSubscriptionsList :: Generic SubscriptionsList _
instance showSubscriptionsList :: Show SubscriptionsList where
  show = genericShow
instance decodeSubscriptionsList :: Decode SubscriptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionsList :: Encode SubscriptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the rate at which requests have been submitted for this action exceeds the limit for your account.</p>
newtype ThrottledException = ThrottledException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeThrottledException :: Newtype ThrottledException _
derive instance repGenericThrottledException :: Generic ThrottledException _
instance showThrottledException :: Show ThrottledException where
  show = genericShow
instance decodeThrottledException :: Decode ThrottledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottledException :: Encode ThrottledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use <code>GetTopicAttributes</code>.</p>
newtype Topic = Topic 
  { "TopicArn" :: NullOrUndefined.NullOrUndefined (TopicARN')
  }
derive instance newtypeTopic :: Newtype Topic _
derive instance repGenericTopic :: Generic Topic _
instance showTopic :: Show Topic where
  show = genericShow
instance decodeTopic :: Decode Topic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopic :: Encode Topic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicAttributesMap = TopicAttributesMap (StrMap.StrMap AttributeValue')
derive instance newtypeTopicAttributesMap :: Newtype TopicAttributesMap _
derive instance repGenericTopicAttributesMap :: Generic TopicAttributesMap _
instance showTopicAttributesMap :: Show TopicAttributesMap where
  show = genericShow
instance decodeTopicAttributesMap :: Decode TopicAttributesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicAttributesMap :: Encode TopicAttributesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the customer already owns the maximum allowed number of topics.</p>
newtype TopicLimitExceededException = TopicLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTopicLimitExceededException :: Newtype TopicLimitExceededException _
derive instance repGenericTopicLimitExceededException :: Generic TopicLimitExceededException _
instance showTopicLimitExceededException :: Show TopicLimitExceededException where
  show = genericShow
instance decodeTopicLimitExceededException :: Decode TopicLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicLimitExceededException :: Encode TopicLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicsList = TopicsList (Array Topic)
derive instance newtypeTopicsList :: Newtype TopicsList _
derive instance repGenericTopicsList :: Generic TopicsList _
instance showTopicsList :: Show TopicsList where
  show = genericShow
instance decodeTopicsList :: Decode TopicsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicsList :: Encode TopicsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input for Unsubscribe action.</p>
newtype UnsubscribeInput = UnsubscribeInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  }
derive instance newtypeUnsubscribeInput :: Newtype UnsubscribeInput _
derive instance repGenericUnsubscribeInput :: Generic UnsubscribeInput _
instance showUnsubscribeInput :: Show UnsubscribeInput where
  show = genericShow
instance decodeUnsubscribeInput :: Decode UnsubscribeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsubscribeInput :: Encode UnsubscribeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Account' = Account' String
derive instance newtypeAccount' :: Newtype Account' _
derive instance repGenericAccount' :: Generic Account' _
instance showAccount' :: Show Account' where
  show = genericShow
instance decodeAccount' :: Decode Account' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccount' :: Encode Account' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Action' = Action' String
derive instance newtypeAction' :: Newtype Action' _
derive instance repGenericAction' :: Generic Action' _
instance showAction' :: Show Action' where
  show = genericShow
instance decodeAction' :: Decode Action' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction' :: Encode Action' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName' = AttributeName' String
derive instance newtypeAttributeName' :: Newtype AttributeName' _
derive instance repGenericAttributeName' :: Generic AttributeName' _
instance showAttributeName' :: Show AttributeName' where
  show = genericShow
instance decodeAttributeName' :: Decode AttributeName' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName' :: Encode AttributeName' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue' = AttributeValue' String
derive instance newtypeAttributeValue' :: Newtype AttributeValue' _
derive instance repGenericAttributeValue' :: Generic AttributeValue' _
instance showAttributeValue' :: Show AttributeValue' where
  show = genericShow
instance decodeAttributeValue' :: Decode AttributeValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue' :: Encode AttributeValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthenticateOnUnsubscribe' = AuthenticateOnUnsubscribe' String
derive instance newtypeAuthenticateOnUnsubscribe' :: Newtype AuthenticateOnUnsubscribe' _
derive instance repGenericAuthenticateOnUnsubscribe' :: Generic AuthenticateOnUnsubscribe' _
instance showAuthenticateOnUnsubscribe' :: Show AuthenticateOnUnsubscribe' where
  show = genericShow
instance decodeAuthenticateOnUnsubscribe' :: Decode AuthenticateOnUnsubscribe' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthenticateOnUnsubscribe' :: Encode AuthenticateOnUnsubscribe' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Delegate' = Delegate' String
derive instance newtypeDelegate' :: Newtype Delegate' _
derive instance repGenericDelegate' :: Generic Delegate' _
instance showDelegate' :: Show Delegate' where
  show = genericShow
instance decodeDelegate' :: Decode Delegate' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegate' :: Encode Delegate' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Endpoint' = Endpoint' String
derive instance newtypeEndpoint' :: Newtype Endpoint' _
derive instance repGenericEndpoint' :: Generic Endpoint' _
instance showEndpoint' :: Show Endpoint' where
  show = genericShow
instance decodeEndpoint' :: Decode Endpoint' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint' :: Encode Endpoint' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Label' = Label' String
derive instance newtypeLabel' :: Newtype Label' _
derive instance repGenericLabel' :: Generic Label' _
instance showLabel' :: Show Label' where
  show = genericShow
instance decodeLabel' :: Decode Label' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabel' :: Encode Label' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message' = Message' String
derive instance newtypeMessage' :: Newtype Message' _
derive instance repGenericMessage' :: Generic Message' _
instance showMessage' :: Show Message' where
  show = genericShow
instance decodeMessage' :: Decode Message' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage' :: Encode Message' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageId' = MessageId' String
derive instance newtypeMessageId' :: Newtype MessageId' _
derive instance repGenericMessageId' :: Generic MessageId' _
instance showMessageId' :: Show MessageId' where
  show = genericShow
instance decodeMessageId' :: Decode MessageId' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageId' :: Encode MessageId' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageStructure' = MessageStructure' String
derive instance newtypeMessageStructure' :: Newtype MessageStructure' _
derive instance repGenericMessageStructure' :: Generic MessageStructure' _
instance showMessageStructure' :: Show MessageStructure' where
  show = genericShow
instance decodeMessageStructure' :: Decode MessageStructure' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageStructure' :: Encode MessageStructure' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken' = NextToken' String
derive instance newtypeNextToken' :: Newtype NextToken' _
derive instance repGenericNextToken' :: Generic NextToken' _
instance showNextToken' :: Show NextToken' where
  show = genericShow
instance decodeNextToken' :: Decode NextToken' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken' :: Encode NextToken' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Protocol' = Protocol' String
derive instance newtypeProtocol' :: Newtype Protocol' _
derive instance repGenericProtocol' :: Generic Protocol' _
instance showProtocol' :: Show Protocol' where
  show = genericShow
instance decodeProtocol' :: Decode Protocol' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProtocol' :: Encode Protocol' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Subject' = Subject' String
derive instance newtypeSubject' :: Newtype Subject' _
derive instance repGenericSubject' :: Generic Subject' _
instance showSubject' :: Show Subject' where
  show = genericShow
instance decodeSubject' :: Decode Subject' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubject' :: Encode Subject' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionARN' = SubscriptionARN' String
derive instance newtypeSubscriptionARN' :: Newtype SubscriptionARN' _
derive instance repGenericSubscriptionARN' :: Generic SubscriptionARN' _
instance showSubscriptionARN' :: Show SubscriptionARN' where
  show = genericShow
instance decodeSubscriptionARN' :: Decode SubscriptionARN' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionARN' :: Encode SubscriptionARN' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token' = Token' String
derive instance newtypeToken' :: Newtype Token' _
derive instance repGenericToken' :: Generic Token' _
instance showToken' :: Show Token' where
  show = genericShow
instance decodeToken' :: Decode Token' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken' :: Encode Token' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicARN' = TopicARN' String
derive instance newtypeTopicARN' :: Newtype TopicARN' _
derive instance repGenericTopicARN' :: Generic TopicARN' _
instance showTopicARN' :: Show TopicARN' where
  show = genericShow
instance decodeTopicARN' :: Decode TopicARN' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicARN' :: Encode TopicARN' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicName' = TopicName' String
derive instance newtypeTopicName' :: Newtype TopicName' _
derive instance repGenericTopicName' :: Generic TopicName' _
instance showTopicName' :: Show TopicName' where
  show = genericShow
instance decodeTopicName' :: Decode TopicName' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicName' :: Encode TopicName' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
