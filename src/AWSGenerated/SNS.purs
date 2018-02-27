

-- | <fullname>Amazon Simple Notification Service</fullname> <p>Amazon Simple Notification Service (Amazon SNS) is a web service that enables you to build distributed web-enabled applications. Applications can use Amazon SNS to easily push real-time notification messages to interested subscribers over multiple delivery protocols. For more information about this product see <a href="http://aws.amazon.com/sns/">http://aws.amazon.com/sns</a>. For detailed information about Amazon SNS features and their associated API calls, see the <a href="http://docs.aws.amazon.com/sns/latest/dg/">Amazon SNS Developer Guide</a>. </p> <p>We also provide SDKs that enable you to access Amazon SNS from your preferred programming language. The SDKs contain functionality that automatically takes care of tasks such as: cryptographically signing your service requests, retrying requests, and handling error responses. For a list of available SDKs, go to <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>. </p>
module AWS.SNS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SNS" :: String


-- | <p>Adds a statement to a topic's access control policy, granting access for the specified AWS accounts to the specified actions.</p>
addPermission :: forall eff. AddPermissionInput -> Aff (err :: AWS.RequestError | eff) Unit
addPermission = AWS.request serviceName "addPermission" 


-- | <p>Accepts a phone number and indicates whether the phone holder has opted out of receiving SMS messages from your account. You cannot send SMS messages to a number that is opted out.</p> <p>To resume sending messages, you can opt in the number by using the <code>OptInPhoneNumber</code> action.</p>
checkIfPhoneNumberIsOptedOut :: forall eff. CheckIfPhoneNumberIsOptedOutInput -> Aff (err :: AWS.RequestError | eff) CheckIfPhoneNumberIsOptedOutResponse
checkIfPhoneNumberIsOptedOut = AWS.request serviceName "checkIfPhoneNumberIsOptedOut" 


-- | <p>Verifies an endpoint owner's intent to receive messages by validating the token sent to the endpoint by an earlier <code>Subscribe</code> action. If the token is valid, the action creates a new subscription and returns its Amazon Resource Name (ARN). This call requires an AWS signature only when the <code>AuthenticateOnUnsubscribe</code> flag is set to "true".</p>
confirmSubscription :: forall eff. ConfirmSubscriptionInput -> Aff (err :: AWS.RequestError | eff) ConfirmSubscriptionResponse
confirmSubscription = AWS.request serviceName "confirmSubscription" 


-- | <p>Creates a platform application object for one of the supported push notification services, such as APNS and GCM, to which devices and mobile apps may register. You must specify PlatformPrincipal and PlatformCredential attributes when using the <code>CreatePlatformApplication</code> action. The PlatformPrincipal is received from the notification service. For APNS/APNS_SANDBOX, PlatformPrincipal is "SSL certificate". For GCM, PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client id". The PlatformCredential is also received from the notification service. For WNS, PlatformPrincipal is "Package Security Identifier". For MPNS, PlatformPrincipal is "TLS certificate". For Baidu, PlatformPrincipal is "API key".</p> <p>For APNS/APNS_SANDBOX, PlatformCredential is "private key". For GCM, PlatformCredential is "API key". For ADM, PlatformCredential is "client secret". For WNS, PlatformCredential is "secret key". For MPNS, PlatformCredential is "private key". For Baidu, PlatformCredential is "secret key". The PlatformApplicationArn that is returned when using <code>CreatePlatformApplication</code> is then used as an attribute for the <code>CreatePlatformEndpoint</code> action. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. For more information about obtaining the PlatformPrincipal and PlatformCredential for each of the supported push notification services, see <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-apns.html">Getting Started with Apple Push Notification Service</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-adm.html">Getting Started with Amazon Device Messaging</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-baidu.html">Getting Started with Baidu Cloud Push</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-gcm.html">Getting Started with Google Cloud Messaging for Android</a>, <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-mpns.html">Getting Started with MPNS</a>, or <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-wns.html">Getting Started with WNS</a>. </p>
createPlatformApplication :: forall eff. CreatePlatformApplicationInput -> Aff (err :: AWS.RequestError | eff) CreatePlatformApplicationResponse
createPlatformApplication = AWS.request serviceName "createPlatformApplication" 


-- | <p>Creates an endpoint for a device and mobile app on one of the supported push notification services, such as GCM and APNS. <code>CreatePlatformEndpoint</code> requires the PlatformApplicationArn that is returned from <code>CreatePlatformApplication</code>. The EndpointArn that is returned when using <code>CreatePlatformEndpoint</code> can then be used by the <code>Publish</code> action to send a message to a mobile app or by the <code>Subscribe</code> action for subscription to a topic. The <code>CreatePlatformEndpoint</code> action is idempotent, so if the requester already owns an endpoint with the same device token and attributes, that endpoint's ARN is returned without creating a new endpoint. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p> <p>When using <code>CreatePlatformEndpoint</code> with Baidu, two attributes must be provided: ChannelId and UserId. The token field must also contain the ChannelId. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePushBaiduEndpoint.html">Creating an Amazon SNS Endpoint for Baidu</a>. </p>
createPlatformEndpoint :: forall eff. CreatePlatformEndpointInput -> Aff (err :: AWS.RequestError | eff) CreateEndpointResponse
createPlatformEndpoint = AWS.request serviceName "createPlatformEndpoint" 


-- | <p>Creates a topic to which notifications can be published. Users can create at most 100,000 topics. For more information, see <a href="http://aws.amazon.com/sns/">http://aws.amazon.com/sns</a>. This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.</p>
createTopic :: forall eff. CreateTopicInput -> Aff (err :: AWS.RequestError | eff) CreateTopicResponse
createTopic = AWS.request serviceName "createTopic" 


-- | <p>Deletes the endpoint for a device and mobile app from Amazon SNS. This action is idempotent. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p> <p>When you delete an endpoint that is also subscribed to a topic, then you must also unsubscribe the endpoint from the topic.</p>
deleteEndpoint :: forall eff. DeleteEndpointInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteEndpoint = AWS.request serviceName "deleteEndpoint" 


-- | <p>Deletes a platform application object for one of the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
deletePlatformApplication :: forall eff. DeletePlatformApplicationInput -> Aff (err :: AWS.RequestError | eff) Unit
deletePlatformApplication = AWS.request serviceName "deletePlatformApplication" 


-- | <p>Deletes a topic and all its subscriptions. Deleting a topic might prevent some messages previously sent to the topic from being delivered to subscribers. This action is idempotent, so deleting a topic that does not exist does not result in an error.</p>
deleteTopic :: forall eff. DeleteTopicInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteTopic = AWS.request serviceName "deleteTopic" 


-- | <p>Retrieves the endpoint attributes for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
getEndpointAttributes :: forall eff. GetEndpointAttributesInput -> Aff (err :: AWS.RequestError | eff) GetEndpointAttributesResponse
getEndpointAttributes = AWS.request serviceName "getEndpointAttributes" 


-- | <p>Retrieves the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
getPlatformApplicationAttributes :: forall eff. GetPlatformApplicationAttributesInput -> Aff (err :: AWS.RequestError | eff) GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributes = AWS.request serviceName "getPlatformApplicationAttributes" 


-- | <p>Returns the settings for sending SMS messages from your account.</p> <p>These settings are set with the <code>SetSMSAttributes</code> action.</p>
getSMSAttributes :: forall eff. GetSMSAttributesInput -> Aff (err :: AWS.RequestError | eff) GetSMSAttributesResponse
getSMSAttributes = AWS.request serviceName "getSMSAttributes" 


-- | <p>Returns all of the properties of a subscription.</p>
getSubscriptionAttributes :: forall eff. GetSubscriptionAttributesInput -> Aff (err :: AWS.RequestError | eff) GetSubscriptionAttributesResponse
getSubscriptionAttributes = AWS.request serviceName "getSubscriptionAttributes" 


-- | <p>Returns all of the properties of a topic. Topic properties returned might differ based on the authorization of the user.</p>
getTopicAttributes :: forall eff. GetTopicAttributesInput -> Aff (err :: AWS.RequestError | eff) GetTopicAttributesResponse
getTopicAttributes = AWS.request serviceName "getTopicAttributes" 


-- | <p>Lists the endpoints and endpoint attributes for devices in a supported push notification service, such as GCM and APNS. The results for <code>ListEndpointsByPlatformApplication</code> are paginated and return a limited list of endpoints, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call <code>ListEndpointsByPlatformApplication</code> again using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
listEndpointsByPlatformApplication :: forall eff. ListEndpointsByPlatformApplicationInput -> Aff (err :: AWS.RequestError | eff) ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplication = AWS.request serviceName "listEndpointsByPlatformApplication" 


-- | <p>Returns a list of phone numbers that are opted out, meaning you cannot send SMS messages to them.</p> <p>The results for <code>ListPhoneNumbersOptedOut</code> are paginated, and each page returns up to 100 phone numbers. If additional phone numbers are available after the first page of results, then a <code>NextToken</code> string will be returned. To receive the next page, you call <code>ListPhoneNumbersOptedOut</code> again using the <code>NextToken</code> string received from the previous call. When there are no more records to return, <code>NextToken</code> will be null.</p>
listPhoneNumbersOptedOut :: forall eff. ListPhoneNumbersOptedOutInput -> Aff (err :: AWS.RequestError | eff) ListPhoneNumbersOptedOutResponse
listPhoneNumbersOptedOut = AWS.request serviceName "listPhoneNumbersOptedOut" 


-- | <p>Lists the platform application objects for the supported push notification services, such as APNS and GCM. The results for <code>ListPlatformApplications</code> are paginated and return a limited list of applications, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call <code>ListPlatformApplications</code> using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
listPlatformApplications :: forall eff. ListPlatformApplicationsInput -> Aff (err :: AWS.RequestError | eff) ListPlatformApplicationsResponse
listPlatformApplications = AWS.request serviceName "listPlatformApplications" 


-- | <p>Returns a list of the requester's subscriptions. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListSubscriptions</code> call to get further results.</p>
listSubscriptions :: forall eff. ListSubscriptionsInput -> Aff (err :: AWS.RequestError | eff) ListSubscriptionsResponse
listSubscriptions = AWS.request serviceName "listSubscriptions" 


-- | <p>Returns a list of the subscriptions to a specific topic. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListSubscriptionsByTopic</code> call to get further results.</p>
listSubscriptionsByTopic :: forall eff. ListSubscriptionsByTopicInput -> Aff (err :: AWS.RequestError | eff) ListSubscriptionsByTopicResponse
listSubscriptionsByTopic = AWS.request serviceName "listSubscriptionsByTopic" 


-- | <p>Returns a list of the requester's topics. Each call returns a limited list of topics, up to 100. If there are more topics, a <code>NextToken</code> is also returned. Use the <code>NextToken</code> parameter in a new <code>ListTopics</code> call to get further results.</p>
listTopics :: forall eff. ListTopicsInput -> Aff (err :: AWS.RequestError | eff) ListTopicsResponse
listTopics = AWS.request serviceName "listTopics" 


-- | <p>Use this request to opt in a phone number that is opted out, which enables you to resume sending SMS messages to the number.</p> <p>You can opt in a phone number only once every 30 days.</p>
optInPhoneNumber :: forall eff. OptInPhoneNumberInput -> Aff (err :: AWS.RequestError | eff) OptInPhoneNumberResponse
optInPhoneNumber = AWS.request serviceName "optInPhoneNumber" 


-- | <p>Sends a message to all of a topic's subscribed endpoints. When a <code>messageId</code> is returned, the message has been saved and Amazon SNS will attempt to deliver it to the topic's subscribers shortly. The format of the outgoing message to each subscribed endpoint depends on the notification protocol.</p> <p>To use the <code>Publish</code> action for sending a message to a mobile endpoint, such as an app on a Kindle device or mobile phone, you must specify the EndpointArn for the TargetArn parameter. The EndpointArn is returned when making a call with the <code>CreatePlatformEndpoint</code> action. </p> <p>For more information about formatting messages, see <a href="http://docs.aws.amazon.com/sns/latest/dg/mobile-push-send-custommessage.html">Send Custom Platform-Specific Payloads in Messages to Mobile Devices</a>. </p>
publish :: forall eff. PublishInput -> Aff (err :: AWS.RequestError | eff) PublishResponse
publish = AWS.request serviceName "publish" 


-- | <p>Removes a statement from a topic's access control policy.</p>
removePermission :: forall eff. RemovePermissionInput -> Aff (err :: AWS.RequestError | eff) Unit
removePermission = AWS.request serviceName "removePermission" 


-- | <p>Sets the attributes for an endpoint for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. </p>
setEndpointAttributes :: forall eff. SetEndpointAttributesInput -> Aff (err :: AWS.RequestError | eff) Unit
setEndpointAttributes = AWS.request serviceName "setEndpointAttributes" 


-- | <p>Sets the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html">Using Amazon SNS Mobile Push Notifications</a>. For information on configuring attributes for message delivery status, see <a href="http://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html">Using Amazon SNS Application Attributes for Message Delivery Status</a>. </p>
setPlatformApplicationAttributes :: forall eff. SetPlatformApplicationAttributesInput -> Aff (err :: AWS.RequestError | eff) Unit
setPlatformApplicationAttributes = AWS.request serviceName "setPlatformApplicationAttributes" 


-- | <p>Use this request to set the default settings for sending SMS messages and receiving daily SMS usage reports.</p> <p>You can override some of these settings for a single message when you use the <code>Publish</code> action with the <code>MessageAttributes.entry.N</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/sms_publish-to-phone.html">Sending an SMS Message</a> in the <i>Amazon SNS Developer Guide</i>.</p>
setSMSAttributes :: forall eff. SetSMSAttributesInput -> Aff (err :: AWS.RequestError | eff) SetSMSAttributesResponse
setSMSAttributes = AWS.request serviceName "setSMSAttributes" 


-- | <p>Allows a subscription owner to set an attribute of the topic to a new value.</p>
setSubscriptionAttributes :: forall eff. SetSubscriptionAttributesInput -> Aff (err :: AWS.RequestError | eff) Unit
setSubscriptionAttributes = AWS.request serviceName "setSubscriptionAttributes" 


-- | <p>Allows a topic owner to set an attribute of the topic to a new value.</p>
setTopicAttributes :: forall eff. SetTopicAttributesInput -> Aff (err :: AWS.RequestError | eff) Unit
setTopicAttributes = AWS.request serviceName "setTopicAttributes" 


-- | <p>Prepares to subscribe an endpoint by sending the endpoint a confirmation message. To actually create a subscription, the endpoint owner must call the <code>ConfirmSubscription</code> action with the token from the confirmation message. Confirmation tokens are valid for three days.</p>
subscribe :: forall eff. SubscribeInput -> Aff (err :: AWS.RequestError | eff) SubscribeResponse
subscribe = AWS.request serviceName "subscribe" 


-- | <p>Deletes a subscription. If the subscription requires authentication for deletion, only the owner of the subscription or the topic's owner can unsubscribe, and an AWS signature is required. If the <code>Unsubscribe</code> call does not require authentication and the requester is not the subscription owner, a final cancellation message is delivered to the endpoint, so that the endpoint owner can easily resubscribe to the topic if the <code>Unsubscribe</code> request was unintended.</p>
unsubscribe :: forall eff. UnsubscribeInput -> Aff (err :: AWS.RequestError | eff) Unit
unsubscribe = AWS.request serviceName "unsubscribe" 


newtype ActionsList = ActionsList (Array Action')
derive instance newtypeActionsList :: Newtype ActionsList _


newtype AddPermissionInput = AddPermissionInput 
  { "TopicArn" :: (TopicARN')
  , "Label" :: (Label')
  , "AWSAccountId" :: (DelegatesList)
  , "ActionName" :: (ActionsList)
  }
derive instance newtypeAddPermissionInput :: Newtype AddPermissionInput _


-- | <p>Indicates that the user has been denied access to the requested resource.</p>
newtype AuthorizationErrorException = AuthorizationErrorException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeAuthorizationErrorException :: Newtype AuthorizationErrorException _


newtype Binary = Binary String
derive instance newtypeBinary :: Newtype Binary _


-- | <p>The input for the <code>CheckIfPhoneNumberIsOptedOut</code> action.</p>
newtype CheckIfPhoneNumberIsOptedOutInput = CheckIfPhoneNumberIsOptedOutInput 
  { "PhoneNumber'" :: (PhoneNumber)
  }
derive instance newtypeCheckIfPhoneNumberIsOptedOutInput :: Newtype CheckIfPhoneNumberIsOptedOutInput _


-- | <p>The response from the <code>CheckIfPhoneNumberIsOptedOut</code> action.</p>
newtype CheckIfPhoneNumberIsOptedOutResponse = CheckIfPhoneNumberIsOptedOutResponse 
  { "IsOptedOut'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCheckIfPhoneNumberIsOptedOutResponse :: Newtype CheckIfPhoneNumberIsOptedOutResponse _


-- | <p>Input for ConfirmSubscription action.</p>
newtype ConfirmSubscriptionInput = ConfirmSubscriptionInput 
  { "TopicArn" :: (TopicARN')
  , "Token" :: (Token')
  , "AuthenticateOnUnsubscribe" :: NullOrUndefined (AuthenticateOnUnsubscribe')
  }
derive instance newtypeConfirmSubscriptionInput :: Newtype ConfirmSubscriptionInput _


-- | <p>Response for ConfirmSubscriptions action.</p>
newtype ConfirmSubscriptionResponse = ConfirmSubscriptionResponse 
  { "SubscriptionArn" :: NullOrUndefined (SubscriptionARN')
  }
derive instance newtypeConfirmSubscriptionResponse :: Newtype ConfirmSubscriptionResponse _


-- | <p>Response from CreateEndpoint action.</p>
newtype CreateEndpointResponse = CreateEndpointResponse 
  { "EndpointArn" :: NullOrUndefined (String)
  }
derive instance newtypeCreateEndpointResponse :: Newtype CreateEndpointResponse _


-- | <p>Input for CreatePlatformApplication action.</p>
newtype CreatePlatformApplicationInput = CreatePlatformApplicationInput 
  { "Name" :: (String)
  , "Platform" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeCreatePlatformApplicationInput :: Newtype CreatePlatformApplicationInput _


-- | <p>Response from CreatePlatformApplication action.</p>
newtype CreatePlatformApplicationResponse = CreatePlatformApplicationResponse 
  { "PlatformApplicationArn" :: NullOrUndefined (String)
  }
derive instance newtypeCreatePlatformApplicationResponse :: Newtype CreatePlatformApplicationResponse _


-- | <p>Input for CreatePlatformEndpoint action.</p>
newtype CreatePlatformEndpointInput = CreatePlatformEndpointInput 
  { "PlatformApplicationArn" :: (String)
  , "Token" :: (String)
  , "CustomUserData" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypeCreatePlatformEndpointInput :: Newtype CreatePlatformEndpointInput _


-- | <p>Input for CreateTopic action.</p>
newtype CreateTopicInput = CreateTopicInput 
  { "Name" :: (TopicName')
  }
derive instance newtypeCreateTopicInput :: Newtype CreateTopicInput _


-- | <p>Response from CreateTopic action.</p>
newtype CreateTopicResponse = CreateTopicResponse 
  { "TopicArn" :: NullOrUndefined (TopicARN')
  }
derive instance newtypeCreateTopicResponse :: Newtype CreateTopicResponse _


newtype DelegatesList = DelegatesList (Array Delegate')
derive instance newtypeDelegatesList :: Newtype DelegatesList _


-- | <p>Input for DeleteEndpoint action.</p>
newtype DeleteEndpointInput = DeleteEndpointInput 
  { "EndpointArn" :: (String)
  }
derive instance newtypeDeleteEndpointInput :: Newtype DeleteEndpointInput _


-- | <p>Input for DeletePlatformApplication action.</p>
newtype DeletePlatformApplicationInput = DeletePlatformApplicationInput 
  { "PlatformApplicationArn" :: (String)
  }
derive instance newtypeDeletePlatformApplicationInput :: Newtype DeletePlatformApplicationInput _


newtype DeleteTopicInput = DeleteTopicInput 
  { "TopicArn" :: (TopicARN')
  }
derive instance newtypeDeleteTopicInput :: Newtype DeleteTopicInput _


-- | <p>Endpoint for mobile app and device.</p>
newtype Endpoint = Endpoint 
  { "EndpointArn" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _


-- | <p>Exception error indicating endpoint disabled.</p>
newtype EndpointDisabledException = EndpointDisabledException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeEndpointDisabledException :: Newtype EndpointDisabledException _


-- | <p>Input for GetEndpointAttributes action.</p>
newtype GetEndpointAttributesInput = GetEndpointAttributesInput 
  { "EndpointArn" :: (String)
  }
derive instance newtypeGetEndpointAttributesInput :: Newtype GetEndpointAttributesInput _


-- | <p>Response from GetEndpointAttributes of the EndpointArn.</p>
newtype GetEndpointAttributesResponse = GetEndpointAttributesResponse 
  { "Attributes" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetEndpointAttributesResponse :: Newtype GetEndpointAttributesResponse _


-- | <p>Input for GetPlatformApplicationAttributes action.</p>
newtype GetPlatformApplicationAttributesInput = GetPlatformApplicationAttributesInput 
  { "PlatformApplicationArn" :: (String)
  }
derive instance newtypeGetPlatformApplicationAttributesInput :: Newtype GetPlatformApplicationAttributesInput _


-- | <p>Response for GetPlatformApplicationAttributes action.</p>
newtype GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse 
  { "Attributes" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetPlatformApplicationAttributesResponse :: Newtype GetPlatformApplicationAttributesResponse _


-- | <p>The input for the <code>GetSMSAttributes</code> request.</p>
newtype GetSMSAttributesInput = GetSMSAttributesInput 
  { "Attributes'" :: NullOrUndefined (ListString)
  }
derive instance newtypeGetSMSAttributesInput :: Newtype GetSMSAttributesInput _


-- | <p>The response from the <code>GetSMSAttributes</code> request.</p>
newtype GetSMSAttributesResponse = GetSMSAttributesResponse 
  { "Attributes'" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypeGetSMSAttributesResponse :: Newtype GetSMSAttributesResponse _


-- | <p>Input for GetSubscriptionAttributes.</p>
newtype GetSubscriptionAttributesInput = GetSubscriptionAttributesInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  }
derive instance newtypeGetSubscriptionAttributesInput :: Newtype GetSubscriptionAttributesInput _


-- | <p>Response for GetSubscriptionAttributes action.</p>
newtype GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse 
  { "Attributes" :: NullOrUndefined (SubscriptionAttributesMap)
  }
derive instance newtypeGetSubscriptionAttributesResponse :: Newtype GetSubscriptionAttributesResponse _


-- | <p>Input for GetTopicAttributes action.</p>
newtype GetTopicAttributesInput = GetTopicAttributesInput 
  { "TopicArn" :: (TopicARN')
  }
derive instance newtypeGetTopicAttributesInput :: Newtype GetTopicAttributesInput _


-- | <p>Response for GetTopicAttributes action.</p>
newtype GetTopicAttributesResponse = GetTopicAttributesResponse 
  { "Attributes" :: NullOrUndefined (TopicAttributesMap)
  }
derive instance newtypeGetTopicAttributesResponse :: Newtype GetTopicAttributesResponse _


-- | <p>Indicates an internal service error.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _


-- | <p>Indicates that a request parameter does not comply with the associated constraints.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>Indicates that a request parameter does not comply with the associated constraints.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _


-- | <p>Input for ListEndpointsByPlatformApplication action.</p>
newtype ListEndpointsByPlatformApplicationInput = ListEndpointsByPlatformApplicationInput 
  { "PlatformApplicationArn" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListEndpointsByPlatformApplicationInput :: Newtype ListEndpointsByPlatformApplicationInput _


-- | <p>Response for ListEndpointsByPlatformApplication action.</p>
newtype ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse 
  { "Endpoints" :: NullOrUndefined (ListOfEndpoints)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListEndpointsByPlatformApplicationResponse :: Newtype ListEndpointsByPlatformApplicationResponse _


newtype ListOfEndpoints = ListOfEndpoints (Array Endpoint)
derive instance newtypeListOfEndpoints :: Newtype ListOfEndpoints _


newtype ListOfPlatformApplications = ListOfPlatformApplications (Array PlatformApplication)
derive instance newtypeListOfPlatformApplications :: Newtype ListOfPlatformApplications _


-- | <p>The input for the <code>ListPhoneNumbersOptedOut</code> action.</p>
newtype ListPhoneNumbersOptedOutInput = ListPhoneNumbersOptedOutInput 
  { "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListPhoneNumbersOptedOutInput :: Newtype ListPhoneNumbersOptedOutInput _


-- | <p>The response from the <code>ListPhoneNumbersOptedOut</code> action.</p>
newtype ListPhoneNumbersOptedOutResponse = ListPhoneNumbersOptedOutResponse 
  { "PhoneNumbers'" :: NullOrUndefined (PhoneNumberList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListPhoneNumbersOptedOutResponse :: Newtype ListPhoneNumbersOptedOutResponse _


-- | <p>Input for ListPlatformApplications action.</p>
newtype ListPlatformApplicationsInput = ListPlatformApplicationsInput 
  { "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListPlatformApplicationsInput :: Newtype ListPlatformApplicationsInput _


-- | <p>Response for ListPlatformApplications action.</p>
newtype ListPlatformApplicationsResponse = ListPlatformApplicationsResponse 
  { "PlatformApplications" :: NullOrUndefined (ListOfPlatformApplications)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListPlatformApplicationsResponse :: Newtype ListPlatformApplicationsResponse _


newtype ListString = ListString (Array String)
derive instance newtypeListString :: Newtype ListString _


-- | <p>Input for ListSubscriptionsByTopic action.</p>
newtype ListSubscriptionsByTopicInput = ListSubscriptionsByTopicInput 
  { "TopicArn" :: (TopicARN')
  , "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsByTopicInput :: Newtype ListSubscriptionsByTopicInput _


-- | <p>Response for ListSubscriptionsByTopic action.</p>
newtype ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse 
  { "Subscriptions" :: NullOrUndefined (SubscriptionsList)
  , "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsByTopicResponse :: Newtype ListSubscriptionsByTopicResponse _


-- | <p>Input for ListSubscriptions action.</p>
newtype ListSubscriptionsInput = ListSubscriptionsInput 
  { "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsInput :: Newtype ListSubscriptionsInput _


-- | <p>Response for ListSubscriptions action</p>
newtype ListSubscriptionsResponse = ListSubscriptionsResponse 
  { "Subscriptions" :: NullOrUndefined (SubscriptionsList)
  , "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListSubscriptionsResponse :: Newtype ListSubscriptionsResponse _


newtype ListTopicsInput = ListTopicsInput 
  { "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListTopicsInput :: Newtype ListTopicsInput _


-- | <p>Response for ListTopics action.</p>
newtype ListTopicsResponse = ListTopicsResponse 
  { "Topics" :: NullOrUndefined (TopicsList)
  , "NextToken" :: NullOrUndefined (NextToken')
  }
derive instance newtypeListTopicsResponse :: Newtype ListTopicsResponse _


newtype MapStringToString = MapStringToString (Map String String)
derive instance newtypeMapStringToString :: Newtype MapStringToString _


newtype MessageAttributeMap = MessageAttributeMap (Map String MessageAttributeValue)
derive instance newtypeMessageAttributeMap :: Newtype MessageAttributeMap _


-- | <p>The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/api/API_Publish.html">Publish</a>.</p> <p>Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html">Using Amazon SNS Message Attributes</a>.</p>
newtype MessageAttributeValue = MessageAttributeValue 
  { "DataType" :: (String)
  , "StringValue" :: NullOrUndefined (String)
  , "BinaryValue" :: NullOrUndefined (Binary)
  }
derive instance newtypeMessageAttributeValue :: Newtype MessageAttributeValue _


-- | <p>Indicates that the requested resource does not exist.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | <p>Input for the OptInPhoneNumber action.</p>
newtype OptInPhoneNumberInput = OptInPhoneNumberInput 
  { "PhoneNumber'" :: (PhoneNumber)
  }
derive instance newtypeOptInPhoneNumberInput :: Newtype OptInPhoneNumberInput _


-- | <p>The response for the OptInPhoneNumber action.</p>
newtype OptInPhoneNumberResponse = OptInPhoneNumberResponse 
  { 
  }
derive instance newtypeOptInPhoneNumberResponse :: Newtype OptInPhoneNumberResponse _


newtype PhoneNumber = PhoneNumber String
derive instance newtypePhoneNumber :: Newtype PhoneNumber _


newtype PhoneNumberList = PhoneNumberList (Array PhoneNumber)
derive instance newtypePhoneNumberList :: Newtype PhoneNumberList _


-- | <p>Platform application object.</p>
newtype PlatformApplication = PlatformApplication 
  { "PlatformApplicationArn" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapStringToString)
  }
derive instance newtypePlatformApplication :: Newtype PlatformApplication _


-- | <p>Exception error indicating platform application disabled.</p>
newtype PlatformApplicationDisabledException = PlatformApplicationDisabledException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypePlatformApplicationDisabledException :: Newtype PlatformApplicationDisabledException _


-- | <p>Input for Publish action.</p>
newtype PublishInput = PublishInput 
  { "TopicArn" :: NullOrUndefined (TopicARN')
  , "TargetArn" :: NullOrUndefined (String)
  , "PhoneNumber" :: NullOrUndefined (String)
  , "Message" :: (Message')
  , "Subject" :: NullOrUndefined (Subject')
  , "MessageStructure" :: NullOrUndefined (MessageStructure')
  , "MessageAttributes" :: NullOrUndefined (MessageAttributeMap)
  }
derive instance newtypePublishInput :: Newtype PublishInput _


-- | <p>Response for Publish action.</p>
newtype PublishResponse = PublishResponse 
  { "MessageId" :: NullOrUndefined (MessageId')
  }
derive instance newtypePublishResponse :: Newtype PublishResponse _


-- | <p>Input for RemovePermission action.</p>
newtype RemovePermissionInput = RemovePermissionInput 
  { "TopicArn" :: (TopicARN')
  , "Label" :: (Label')
  }
derive instance newtypeRemovePermissionInput :: Newtype RemovePermissionInput _


-- | <p>Input for SetEndpointAttributes action.</p>
newtype SetEndpointAttributesInput = SetEndpointAttributesInput 
  { "EndpointArn" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeSetEndpointAttributesInput :: Newtype SetEndpointAttributesInput _


-- | <p>Input for SetPlatformApplicationAttributes action.</p>
newtype SetPlatformApplicationAttributesInput = SetPlatformApplicationAttributesInput 
  { "PlatformApplicationArn" :: (String)
  , "Attributes" :: (MapStringToString)
  }
derive instance newtypeSetPlatformApplicationAttributesInput :: Newtype SetPlatformApplicationAttributesInput _


-- | <p>The input for the SetSMSAttributes action.</p>
newtype SetSMSAttributesInput = SetSMSAttributesInput 
  { "Attributes'" :: (MapStringToString)
  }
derive instance newtypeSetSMSAttributesInput :: Newtype SetSMSAttributesInput _


-- | <p>The response for the SetSMSAttributes action.</p>
newtype SetSMSAttributesResponse = SetSMSAttributesResponse 
  { 
  }
derive instance newtypeSetSMSAttributesResponse :: Newtype SetSMSAttributesResponse _


-- | <p>Input for SetSubscriptionAttributes action.</p>
newtype SetSubscriptionAttributesInput = SetSubscriptionAttributesInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  , "AttributeName" :: (AttributeName')
  , "AttributeValue" :: NullOrUndefined (AttributeValue')
  }
derive instance newtypeSetSubscriptionAttributesInput :: Newtype SetSubscriptionAttributesInput _


-- | <p>Input for SetTopicAttributes action.</p>
newtype SetTopicAttributesInput = SetTopicAttributesInput 
  { "TopicArn" :: (TopicARN')
  , "AttributeName" :: (AttributeName')
  , "AttributeValue" :: NullOrUndefined (AttributeValue')
  }
derive instance newtypeSetTopicAttributesInput :: Newtype SetTopicAttributesInput _


-- | <p>Input for Subscribe action.</p>
newtype SubscribeInput = SubscribeInput 
  { "TopicArn" :: (TopicARN')
  , "Protocol" :: (Protocol')
  , "Endpoint" :: NullOrUndefined (Endpoint')
  }
derive instance newtypeSubscribeInput :: Newtype SubscribeInput _


-- | <p>Response for Subscribe action.</p>
newtype SubscribeResponse = SubscribeResponse 
  { "SubscriptionArn" :: NullOrUndefined (SubscriptionARN')
  }
derive instance newtypeSubscribeResponse :: Newtype SubscribeResponse _


-- | <p>A wrapper type for the attributes of an Amazon SNS subscription.</p>
newtype Subscription = Subscription 
  { "SubscriptionArn" :: NullOrUndefined (SubscriptionARN')
  , "Owner" :: NullOrUndefined (Account')
  , "Protocol" :: NullOrUndefined (Protocol')
  , "Endpoint" :: NullOrUndefined (Endpoint')
  , "TopicArn" :: NullOrUndefined (TopicARN')
  }
derive instance newtypeSubscription :: Newtype Subscription _


newtype SubscriptionAttributesMap = SubscriptionAttributesMap (Map AttributeName' AttributeValue')
derive instance newtypeSubscriptionAttributesMap :: Newtype SubscriptionAttributesMap _


-- | <p>Indicates that the customer already owns the maximum allowed number of subscriptions.</p>
newtype SubscriptionLimitExceededException = SubscriptionLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeSubscriptionLimitExceededException :: Newtype SubscriptionLimitExceededException _


newtype SubscriptionsList = SubscriptionsList (Array Subscription)
derive instance newtypeSubscriptionsList :: Newtype SubscriptionsList _


-- | <p>Indicates that the rate at which requests have been submitted for this action exceeds the limit for your account.</p>
newtype ThrottledException = ThrottledException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeThrottledException :: Newtype ThrottledException _


-- | <p>A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use <code>GetTopicAttributes</code>.</p>
newtype Topic = Topic 
  { "TopicArn" :: NullOrUndefined (TopicARN')
  }
derive instance newtypeTopic :: Newtype Topic _


newtype TopicAttributesMap = TopicAttributesMap (Map AttributeName' AttributeValue')
derive instance newtypeTopicAttributesMap :: Newtype TopicAttributesMap _


-- | <p>Indicates that the customer already owns the maximum allowed number of topics.</p>
newtype TopicLimitExceededException = TopicLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeTopicLimitExceededException :: Newtype TopicLimitExceededException _


newtype TopicsList = TopicsList (Array Topic)
derive instance newtypeTopicsList :: Newtype TopicsList _


-- | <p>Input for Unsubscribe action.</p>
newtype UnsubscribeInput = UnsubscribeInput 
  { "SubscriptionArn" :: (SubscriptionARN')
  }
derive instance newtypeUnsubscribeInput :: Newtype UnsubscribeInput _


newtype Account' = Account' String
derive instance newtypeAccount' :: Newtype Account' _


newtype Action' = Action' String
derive instance newtypeAction' :: Newtype Action' _


newtype AttributeName' = AttributeName' String
derive instance newtypeAttributeName' :: Newtype AttributeName' _


newtype AttributeValue' = AttributeValue' String
derive instance newtypeAttributeValue' :: Newtype AttributeValue' _


newtype AuthenticateOnUnsubscribe' = AuthenticateOnUnsubscribe' String
derive instance newtypeAuthenticateOnUnsubscribe' :: Newtype AuthenticateOnUnsubscribe' _


newtype Delegate' = Delegate' String
derive instance newtypeDelegate' :: Newtype Delegate' _


newtype Endpoint' = Endpoint' String
derive instance newtypeEndpoint' :: Newtype Endpoint' _


newtype Label' = Label' String
derive instance newtypeLabel' :: Newtype Label' _


newtype Message' = Message' String
derive instance newtypeMessage' :: Newtype Message' _


newtype MessageId' = MessageId' String
derive instance newtypeMessageId' :: Newtype MessageId' _


newtype MessageStructure' = MessageStructure' String
derive instance newtypeMessageStructure' :: Newtype MessageStructure' _


newtype NextToken' = NextToken' String
derive instance newtypeNextToken' :: Newtype NextToken' _


newtype Protocol' = Protocol' String
derive instance newtypeProtocol' :: Newtype Protocol' _


newtype Subject' = Subject' String
derive instance newtypeSubject' :: Newtype Subject' _


newtype SubscriptionARN' = SubscriptionARN' String
derive instance newtypeSubscriptionARN' :: Newtype SubscriptionARN' _


newtype Token' = Token' String
derive instance newtypeToken' :: Newtype Token' _


newtype TopicARN' = TopicARN' String
derive instance newtypeTopicARN' :: Newtype TopicARN' _


newtype TopicName' = TopicName' String
derive instance newtypeTopicName' :: Newtype TopicName' _
