

module AWS.Pinpoint where

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

serviceName = "Pinpoint" :: String


-- | Creates or updates an app.
createApp :: forall eff. CreateAppRequest -> Aff (exception :: EXCEPTION | eff) CreateAppResponse
createApp = Request.request serviceName "createApp" 


-- | Creates or updates a campaign.
createCampaign :: forall eff. CreateCampaignRequest -> Aff (exception :: EXCEPTION | eff) CreateCampaignResponse
createCampaign = Request.request serviceName "createCampaign" 


-- | Creates or updates an import job.
createImportJob :: forall eff. CreateImportJobRequest -> Aff (exception :: EXCEPTION | eff) CreateImportJobResponse
createImportJob = Request.request serviceName "createImportJob" 


-- | Used to create or update a segment.
createSegment :: forall eff. CreateSegmentRequest -> Aff (exception :: EXCEPTION | eff) CreateSegmentResponse
createSegment = Request.request serviceName "createSegment" 


-- | Delete an ADM channel
deleteAdmChannel :: forall eff. DeleteAdmChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteAdmChannelResponse
deleteAdmChannel = Request.request serviceName "deleteAdmChannel" 


-- | Deletes the APNs channel for an app.
deleteApnsChannel :: forall eff. DeleteApnsChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteApnsChannelResponse
deleteApnsChannel = Request.request serviceName "deleteApnsChannel" 


-- | Delete an APNS sandbox channel
deleteApnsSandboxChannel :: forall eff. DeleteApnsSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteApnsSandboxChannelResponse
deleteApnsSandboxChannel = Request.request serviceName "deleteApnsSandboxChannel" 


-- | Delete an APNS VoIP channel
deleteApnsVoipChannel :: forall eff. DeleteApnsVoipChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteApnsVoipChannelResponse
deleteApnsVoipChannel = Request.request serviceName "deleteApnsVoipChannel" 


-- | Delete an APNS VoIP sandbox channel
deleteApnsVoipSandboxChannel :: forall eff. DeleteApnsVoipSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteApnsVoipSandboxChannelResponse
deleteApnsVoipSandboxChannel = Request.request serviceName "deleteApnsVoipSandboxChannel" 


-- | Deletes an app.
deleteApp :: forall eff. DeleteAppRequest -> Aff (exception :: EXCEPTION | eff) DeleteAppResponse
deleteApp = Request.request serviceName "deleteApp" 


-- | Delete a BAIDU GCM channel
deleteBaiduChannel :: forall eff. DeleteBaiduChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteBaiduChannelResponse
deleteBaiduChannel = Request.request serviceName "deleteBaiduChannel" 


-- | Deletes a campaign.
deleteCampaign :: forall eff. DeleteCampaignRequest -> Aff (exception :: EXCEPTION | eff) DeleteCampaignResponse
deleteCampaign = Request.request serviceName "deleteCampaign" 


-- | Delete an email channel
deleteEmailChannel :: forall eff. DeleteEmailChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteEmailChannelResponse
deleteEmailChannel = Request.request serviceName "deleteEmailChannel" 


-- | Deletes the event stream for an app.
deleteEventStream :: forall eff. DeleteEventStreamRequest -> Aff (exception :: EXCEPTION | eff) DeleteEventStreamResponse
deleteEventStream = Request.request serviceName "deleteEventStream" 


-- | Deletes the GCM channel for an app.
deleteGcmChannel :: forall eff. DeleteGcmChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteGcmChannelResponse
deleteGcmChannel = Request.request serviceName "deleteGcmChannel" 


-- | Deletes a segment.
deleteSegment :: forall eff. DeleteSegmentRequest -> Aff (exception :: EXCEPTION | eff) DeleteSegmentResponse
deleteSegment = Request.request serviceName "deleteSegment" 


-- | Delete an SMS channel
deleteSmsChannel :: forall eff. DeleteSmsChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteSmsChannelResponse
deleteSmsChannel = Request.request serviceName "deleteSmsChannel" 


-- | Get an ADM channel
getAdmChannel :: forall eff. GetAdmChannelRequest -> Aff (exception :: EXCEPTION | eff) GetAdmChannelResponse
getAdmChannel = Request.request serviceName "getAdmChannel" 


-- | Returns information about the APNs channel for an app.
getApnsChannel :: forall eff. GetApnsChannelRequest -> Aff (exception :: EXCEPTION | eff) GetApnsChannelResponse
getApnsChannel = Request.request serviceName "getApnsChannel" 


-- | Get an APNS sandbox channel
getApnsSandboxChannel :: forall eff. GetApnsSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) GetApnsSandboxChannelResponse
getApnsSandboxChannel = Request.request serviceName "getApnsSandboxChannel" 


-- | Get an APNS VoIP channel
getApnsVoipChannel :: forall eff. GetApnsVoipChannelRequest -> Aff (exception :: EXCEPTION | eff) GetApnsVoipChannelResponse
getApnsVoipChannel = Request.request serviceName "getApnsVoipChannel" 


-- | Get an APNS VoipSandbox channel
getApnsVoipSandboxChannel :: forall eff. GetApnsVoipSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) GetApnsVoipSandboxChannelResponse
getApnsVoipSandboxChannel = Request.request serviceName "getApnsVoipSandboxChannel" 


-- | Returns information about an app.
getApp :: forall eff. GetAppRequest -> Aff (exception :: EXCEPTION | eff) GetAppResponse
getApp = Request.request serviceName "getApp" 


-- | Used to request the settings for an app.
getApplicationSettings :: forall eff. GetApplicationSettingsRequest -> Aff (exception :: EXCEPTION | eff) GetApplicationSettingsResponse
getApplicationSettings = Request.request serviceName "getApplicationSettings" 


-- | Returns information about your apps.
getApps :: forall eff. GetAppsRequest -> Aff (exception :: EXCEPTION | eff) GetAppsResponse
getApps = Request.request serviceName "getApps" 


-- | Get a BAIDU GCM channel
getBaiduChannel :: forall eff. GetBaiduChannelRequest -> Aff (exception :: EXCEPTION | eff) GetBaiduChannelResponse
getBaiduChannel = Request.request serviceName "getBaiduChannel" 


-- | Returns information about a campaign.
getCampaign :: forall eff. GetCampaignRequest -> Aff (exception :: EXCEPTION | eff) GetCampaignResponse
getCampaign = Request.request serviceName "getCampaign" 


-- | Returns information about the activity performed by a campaign.
getCampaignActivities :: forall eff. GetCampaignActivitiesRequest -> Aff (exception :: EXCEPTION | eff) GetCampaignActivitiesResponse
getCampaignActivities = Request.request serviceName "getCampaignActivities" 


-- | Returns information about a specific version of a campaign.
getCampaignVersion :: forall eff. GetCampaignVersionRequest -> Aff (exception :: EXCEPTION | eff) GetCampaignVersionResponse
getCampaignVersion = Request.request serviceName "getCampaignVersion" 


-- | Returns information about your campaign versions.
getCampaignVersions :: forall eff. GetCampaignVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetCampaignVersionsResponse
getCampaignVersions = Request.request serviceName "getCampaignVersions" 


-- | Returns information about your campaigns.
getCampaigns :: forall eff. GetCampaignsRequest -> Aff (exception :: EXCEPTION | eff) GetCampaignsResponse
getCampaigns = Request.request serviceName "getCampaigns" 


-- | Get an email channel
getEmailChannel :: forall eff. GetEmailChannelRequest -> Aff (exception :: EXCEPTION | eff) GetEmailChannelResponse
getEmailChannel = Request.request serviceName "getEmailChannel" 


-- | Returns information about an endpoint.
getEndpoint :: forall eff. GetEndpointRequest -> Aff (exception :: EXCEPTION | eff) GetEndpointResponse
getEndpoint = Request.request serviceName "getEndpoint" 


-- | Returns the event stream for an app.
getEventStream :: forall eff. GetEventStreamRequest -> Aff (exception :: EXCEPTION | eff) GetEventStreamResponse
getEventStream = Request.request serviceName "getEventStream" 


-- | Returns information about the GCM channel for an app.
getGcmChannel :: forall eff. GetGcmChannelRequest -> Aff (exception :: EXCEPTION | eff) GetGcmChannelResponse
getGcmChannel = Request.request serviceName "getGcmChannel" 


-- | Returns information about an import job.
getImportJob :: forall eff. GetImportJobRequest -> Aff (exception :: EXCEPTION | eff) GetImportJobResponse
getImportJob = Request.request serviceName "getImportJob" 


-- | Returns information about your import jobs.
getImportJobs :: forall eff. GetImportJobsRequest -> Aff (exception :: EXCEPTION | eff) GetImportJobsResponse
getImportJobs = Request.request serviceName "getImportJobs" 


-- | Returns information about a segment.
getSegment :: forall eff. GetSegmentRequest -> Aff (exception :: EXCEPTION | eff) GetSegmentResponse
getSegment = Request.request serviceName "getSegment" 


-- | Returns a list of import jobs for a specific segment.
getSegmentImportJobs :: forall eff. GetSegmentImportJobsRequest -> Aff (exception :: EXCEPTION | eff) GetSegmentImportJobsResponse
getSegmentImportJobs = Request.request serviceName "getSegmentImportJobs" 


-- | Returns information about a segment version.
getSegmentVersion :: forall eff. GetSegmentVersionRequest -> Aff (exception :: EXCEPTION | eff) GetSegmentVersionResponse
getSegmentVersion = Request.request serviceName "getSegmentVersion" 


-- | Returns information about your segment versions.
getSegmentVersions :: forall eff. GetSegmentVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetSegmentVersionsResponse
getSegmentVersions = Request.request serviceName "getSegmentVersions" 


-- | Used to get information about your segments.
getSegments :: forall eff. GetSegmentsRequest -> Aff (exception :: EXCEPTION | eff) GetSegmentsResponse
getSegments = Request.request serviceName "getSegments" 


-- | Get an SMS channel
getSmsChannel :: forall eff. GetSmsChannelRequest -> Aff (exception :: EXCEPTION | eff) GetSmsChannelResponse
getSmsChannel = Request.request serviceName "getSmsChannel" 


-- | Use to create or update the event stream for an app.
putEventStream :: forall eff. PutEventStreamRequest -> Aff (exception :: EXCEPTION | eff) PutEventStreamResponse
putEventStream = Request.request serviceName "putEventStream" 


-- | Send a batch of messages
sendMessages :: forall eff. SendMessagesRequest -> Aff (exception :: EXCEPTION | eff) SendMessagesResponse
sendMessages = Request.request serviceName "sendMessages" 


-- | Send a batch of messages to users
sendUsersMessages :: forall eff. SendUsersMessagesRequest -> Aff (exception :: EXCEPTION | eff) SendUsersMessagesResponse
sendUsersMessages = Request.request serviceName "sendUsersMessages" 


-- | Update an ADM channel
updateAdmChannel :: forall eff. UpdateAdmChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateAdmChannelResponse
updateAdmChannel = Request.request serviceName "updateAdmChannel" 


-- | Use to update the APNs channel for an app.
updateApnsChannel :: forall eff. UpdateApnsChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateApnsChannelResponse
updateApnsChannel = Request.request serviceName "updateApnsChannel" 


-- | Update an APNS sandbox channel
updateApnsSandboxChannel :: forall eff. UpdateApnsSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateApnsSandboxChannelResponse
updateApnsSandboxChannel = Request.request serviceName "updateApnsSandboxChannel" 


-- | Update an APNS VoIP channel
updateApnsVoipChannel :: forall eff. UpdateApnsVoipChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateApnsVoipChannelResponse
updateApnsVoipChannel = Request.request serviceName "updateApnsVoipChannel" 


-- | Update an APNS VoIP sandbox channel
updateApnsVoipSandboxChannel :: forall eff. UpdateApnsVoipSandboxChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateApnsVoipSandboxChannelResponse
updateApnsVoipSandboxChannel = Request.request serviceName "updateApnsVoipSandboxChannel" 


-- | Used to update the settings for an app.
updateApplicationSettings :: forall eff. UpdateApplicationSettingsRequest -> Aff (exception :: EXCEPTION | eff) UpdateApplicationSettingsResponse
updateApplicationSettings = Request.request serviceName "updateApplicationSettings" 


-- | Update a BAIDU GCM channel
updateBaiduChannel :: forall eff. UpdateBaiduChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateBaiduChannelResponse
updateBaiduChannel = Request.request serviceName "updateBaiduChannel" 


-- | Use to update a campaign.
updateCampaign :: forall eff. UpdateCampaignRequest -> Aff (exception :: EXCEPTION | eff) UpdateCampaignResponse
updateCampaign = Request.request serviceName "updateCampaign" 


-- | Update an email channel
updateEmailChannel :: forall eff. UpdateEmailChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateEmailChannelResponse
updateEmailChannel = Request.request serviceName "updateEmailChannel" 


-- | Use to update an endpoint.
updateEndpoint :: forall eff. UpdateEndpointRequest -> Aff (exception :: EXCEPTION | eff) UpdateEndpointResponse
updateEndpoint = Request.request serviceName "updateEndpoint" 


-- | Use to update a batch of endpoints.
updateEndpointsBatch :: forall eff. UpdateEndpointsBatchRequest -> Aff (exception :: EXCEPTION | eff) UpdateEndpointsBatchResponse
updateEndpointsBatch = Request.request serviceName "updateEndpointsBatch" 


-- | Use to update the GCM channel for an app.
updateGcmChannel :: forall eff. UpdateGcmChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateGcmChannelResponse
updateGcmChannel = Request.request serviceName "updateGcmChannel" 


-- | Use to update a segment.
updateSegment :: forall eff. UpdateSegmentRequest -> Aff (exception :: EXCEPTION | eff) UpdateSegmentResponse
updateSegment = Request.request serviceName "updateSegment" 


-- | Update an SMS channel
updateSmsChannel :: forall eff. UpdateSmsChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateSmsChannelResponse
updateSmsChannel = Request.request serviceName "updateSmsChannel" 


-- | Amazon Device Messaging channel definition.
newtype ADMChannelRequest = ADMChannelRequest 
  { "ClientId" :: NullOrUndefined.NullOrUndefined (String)
  , "ClientSecret" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeADMChannelRequest :: Newtype ADMChannelRequest _
derive instance repGenericADMChannelRequest :: Generic ADMChannelRequest _
instance showADMChannelRequest :: Show ADMChannelRequest where
  show = genericShow
instance decodeADMChannelRequest :: Decode ADMChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeADMChannelRequest :: Encode ADMChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Amazon Device Messaging channel definition.
newtype ADMChannelResponse = ADMChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeADMChannelResponse :: Newtype ADMChannelResponse _
derive instance repGenericADMChannelResponse :: Generic ADMChannelResponse _
instance showADMChannelResponse :: Show ADMChannelResponse where
  show = genericShow
instance decodeADMChannelResponse :: Decode ADMChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeADMChannelResponse :: Encode ADMChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | ADM Message.
newtype ADMMessage = ADMMessage 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "ConsolidationKey" :: NullOrUndefined.NullOrUndefined (String)
  , "Data" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "ExpiresAfter" :: NullOrUndefined.NullOrUndefined (String)
  , "IconReference" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "MD5" :: NullOrUndefined.NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Sound" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeADMMessage :: Newtype ADMMessage _
derive instance repGenericADMMessage :: Generic ADMMessage _
instance showADMMessage :: Show ADMMessage where
  show = genericShow
instance decodeADMMessage :: Decode ADMMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeADMMessage :: Encode ADMMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple Push Notification Service channel definition.
newtype APNSChannelRequest = APNSChannelRequest 
  { "BundleId" :: NullOrUndefined.NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAPNSChannelRequest :: Newtype APNSChannelRequest _
derive instance repGenericAPNSChannelRequest :: Generic APNSChannelRequest _
instance showAPNSChannelRequest :: Show APNSChannelRequest where
  show = genericShow
instance decodeAPNSChannelRequest :: Decode APNSChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSChannelRequest :: Encode APNSChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple Distribution Push Notification Service channel definition.
newtype APNSChannelResponse = APNSChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAPNSChannelResponse :: Newtype APNSChannelResponse _
derive instance repGenericAPNSChannelResponse :: Generic APNSChannelResponse _
instance showAPNSChannelResponse :: Show APNSChannelResponse where
  show = genericShow
instance decodeAPNSChannelResponse :: Decode APNSChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSChannelResponse :: Encode APNSChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | APNS Message.
newtype APNSMessage = APNSMessage 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Badge" :: NullOrUndefined.NullOrUndefined (Int)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "CollapseId" :: NullOrUndefined.NullOrUndefined (String)
  , "Data" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "MediaUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "PreferredAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Priority" :: NullOrUndefined.NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Sound" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "ThreadId" :: NullOrUndefined.NullOrUndefined (String)
  , "TimeToLive" :: NullOrUndefined.NullOrUndefined (Int)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAPNSMessage :: Newtype APNSMessage _
derive instance repGenericAPNSMessage :: Generic APNSMessage _
instance showAPNSMessage :: Show APNSMessage where
  show = genericShow
instance decodeAPNSMessage :: Decode APNSMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSMessage :: Encode APNSMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple Development Push Notification Service channel definition.
newtype APNSSandboxChannelRequest = APNSSandboxChannelRequest 
  { "BundleId" :: NullOrUndefined.NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAPNSSandboxChannelRequest :: Newtype APNSSandboxChannelRequest _
derive instance repGenericAPNSSandboxChannelRequest :: Generic APNSSandboxChannelRequest _
instance showAPNSSandboxChannelRequest :: Show APNSSandboxChannelRequest where
  show = genericShow
instance decodeAPNSSandboxChannelRequest :: Decode APNSSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSSandboxChannelRequest :: Encode APNSSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple Development Push Notification Service channel definition.
newtype APNSSandboxChannelResponse = APNSSandboxChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAPNSSandboxChannelResponse :: Newtype APNSSandboxChannelResponse _
derive instance repGenericAPNSSandboxChannelResponse :: Generic APNSSandboxChannelResponse _
instance showAPNSSandboxChannelResponse :: Show APNSSandboxChannelResponse where
  show = genericShow
instance decodeAPNSSandboxChannelResponse :: Decode APNSSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSSandboxChannelResponse :: Encode APNSSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple VoIP Push Notification Service channel definition.
newtype APNSVoipChannelRequest = APNSVoipChannelRequest 
  { "BundleId" :: NullOrUndefined.NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAPNSVoipChannelRequest :: Newtype APNSVoipChannelRequest _
derive instance repGenericAPNSVoipChannelRequest :: Generic APNSVoipChannelRequest _
instance showAPNSVoipChannelRequest :: Show APNSVoipChannelRequest where
  show = genericShow
instance decodeAPNSVoipChannelRequest :: Decode APNSVoipChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSVoipChannelRequest :: Encode APNSVoipChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple VoIP Push Notification Service channel definition.
newtype APNSVoipChannelResponse = APNSVoipChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAPNSVoipChannelResponse :: Newtype APNSVoipChannelResponse _
derive instance repGenericAPNSVoipChannelResponse :: Generic APNSVoipChannelResponse _
instance showAPNSVoipChannelResponse :: Show APNSVoipChannelResponse where
  show = genericShow
instance decodeAPNSVoipChannelResponse :: Decode APNSVoipChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSVoipChannelResponse :: Encode APNSVoipChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple VoIP Developer Push Notification Service channel definition.
newtype APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest 
  { "BundleId" :: NullOrUndefined.NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined.NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAPNSVoipSandboxChannelRequest :: Newtype APNSVoipSandboxChannelRequest _
derive instance repGenericAPNSVoipSandboxChannelRequest :: Generic APNSVoipSandboxChannelRequest _
instance showAPNSVoipSandboxChannelRequest :: Show APNSVoipSandboxChannelRequest where
  show = genericShow
instance decodeAPNSVoipSandboxChannelRequest :: Decode APNSVoipSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSVoipSandboxChannelRequest :: Encode APNSVoipSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Apple VoIP Developer Push Notification Service channel definition.
newtype APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAPNSVoipSandboxChannelResponse :: Newtype APNSVoipSandboxChannelResponse _
derive instance repGenericAPNSVoipSandboxChannelResponse :: Generic APNSVoipSandboxChannelResponse _
instance showAPNSVoipSandboxChannelResponse :: Show APNSVoipSandboxChannelResponse where
  show = genericShow
instance decodeAPNSVoipSandboxChannelResponse :: Decode APNSVoipSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAPNSVoipSandboxChannelResponse :: Encode APNSVoipSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Action = Action String
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Activities for campaign.
newtype ActivitiesResponse = ActivitiesResponse 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfActivityResponse)
  }
derive instance newtypeActivitiesResponse :: Newtype ActivitiesResponse _
derive instance repGenericActivitiesResponse :: Generic ActivitiesResponse _
instance showActivitiesResponse :: Show ActivitiesResponse where
  show = genericShow
instance decodeActivitiesResponse :: Decode ActivitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivitiesResponse :: Encode ActivitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Activity definition
newtype ActivityResponse = ActivityResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CampaignId" :: NullOrUndefined.NullOrUndefined (String)
  , "End" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Result" :: NullOrUndefined.NullOrUndefined (String)
  , "ScheduledStart" :: NullOrUndefined.NullOrUndefined (String)
  , "Start" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "SuccessfulEndpointCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimezonesCompletedCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimezonesTotalCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "TotalEndpointCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "TreatmentId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeActivityResponse :: Newtype ActivityResponse _
derive instance repGenericActivityResponse :: Generic ActivityResponse _
instance showActivityResponse :: Show ActivityResponse where
  show = genericShow
instance decodeActivityResponse :: Decode ActivityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityResponse :: Encode ActivityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Address configuration.
newtype AddressConfiguration = AddressConfiguration 
  { "BodyOverride" :: NullOrUndefined.NullOrUndefined (String)
  , "ChannelType" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "Context" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAddressConfiguration :: Newtype AddressConfiguration _
derive instance repGenericAddressConfiguration :: Generic AddressConfiguration _
instance showAddressConfiguration :: Show AddressConfiguration where
  show = genericShow
instance decodeAddressConfiguration :: Decode AddressConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddressConfiguration :: Encode AddressConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Application Response.
newtype ApplicationResponse = ApplicationResponse 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplicationResponse :: Newtype ApplicationResponse _
derive instance repGenericApplicationResponse :: Generic ApplicationResponse _
instance showApplicationResponse :: Show ApplicationResponse where
  show = genericShow
instance decodeApplicationResponse :: Decode ApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationResponse :: Encode ApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Application settings.
newtype ApplicationSettingsResource = ApplicationSettingsResource 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Limits" :: NullOrUndefined.NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined.NullOrUndefined (QuietTime)
  }
derive instance newtypeApplicationSettingsResource :: Newtype ApplicationSettingsResource _
derive instance repGenericApplicationSettingsResource :: Generic ApplicationSettingsResource _
instance showApplicationSettingsResource :: Show ApplicationSettingsResource where
  show = genericShow
instance decodeApplicationSettingsResource :: Decode ApplicationSettingsResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationSettingsResource :: Encode ApplicationSettingsResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Get Applications Result.
newtype ApplicationsResponse = ApplicationsResponse 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfApplicationResponse)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplicationsResponse :: Newtype ApplicationsResponse _
derive instance repGenericApplicationsResponse :: Generic ApplicationsResponse _
instance showApplicationsResponse :: Show ApplicationsResponse where
  show = genericShow
instance decodeApplicationsResponse :: Decode ApplicationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationsResponse :: Encode ApplicationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Custom attibute dimension
newtype AttributeDimension = AttributeDimension 
  { "AttributeType" :: NullOrUndefined.NullOrUndefined (AttributeType)
  , "Values" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeAttributeDimension :: Newtype AttributeDimension _
derive instance repGenericAttributeDimension :: Generic AttributeDimension _
instance showAttributeDimension :: Show AttributeDimension where
  show = genericShow
instance decodeAttributeDimension :: Decode AttributeDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeDimension :: Encode AttributeDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeType = AttributeType String
derive instance newtypeAttributeType :: Newtype AttributeType _
derive instance repGenericAttributeType :: Generic AttributeType _
instance showAttributeType :: Show AttributeType where
  show = genericShow
instance decodeAttributeType :: Decode AttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeType :: Encode AttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Baidu Cloud Push credentials
newtype BaiduChannelRequest = BaiduChannelRequest 
  { "ApiKey" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SecretKey" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBaiduChannelRequest :: Newtype BaiduChannelRequest _
derive instance repGenericBaiduChannelRequest :: Generic BaiduChannelRequest _
instance showBaiduChannelRequest :: Show BaiduChannelRequest where
  show = genericShow
instance decodeBaiduChannelRequest :: Decode BaiduChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaiduChannelRequest :: Encode BaiduChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Baidu Cloud Messaging channel definition
newtype BaiduChannelResponse = BaiduChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Credential" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeBaiduChannelResponse :: Newtype BaiduChannelResponse _
derive instance repGenericBaiduChannelResponse :: Generic BaiduChannelResponse _
instance showBaiduChannelResponse :: Show BaiduChannelResponse where
  show = genericShow
instance decodeBaiduChannelResponse :: Decode BaiduChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaiduChannelResponse :: Encode BaiduChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Baidu Message.
newtype BaiduMessage = BaiduMessage 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "Data" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "IconReference" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Sound" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBaiduMessage :: Newtype BaiduMessage _
derive instance repGenericBaiduMessage :: Generic BaiduMessage _
instance showBaiduMessage :: Show BaiduMessage where
  show = genericShow
instance decodeBaiduMessage :: Decode BaiduMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaiduMessage :: Encode BaiduMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The email message configuration.
newtype CampaignEmailMessage = CampaignEmailMessage 
  { "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "FromAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "HtmlBody" :: NullOrUndefined.NullOrUndefined (String)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCampaignEmailMessage :: Newtype CampaignEmailMessage _
derive instance repGenericCampaignEmailMessage :: Generic CampaignEmailMessage _
instance showCampaignEmailMessage :: Show CampaignEmailMessage where
  show = genericShow
instance decodeCampaignEmailMessage :: Decode CampaignEmailMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignEmailMessage :: Encode CampaignEmailMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Campaign Limits are used to limit the number of messages that can be sent to a user.
newtype CampaignLimits = CampaignLimits 
  { "Daily" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaximumDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "MessagesPerSecond" :: NullOrUndefined.NullOrUndefined (Int)
  , "Total" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeCampaignLimits :: Newtype CampaignLimits _
derive instance repGenericCampaignLimits :: Generic CampaignLimits _
instance showCampaignLimits :: Show CampaignLimits where
  show = genericShow
instance decodeCampaignLimits :: Decode CampaignLimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignLimits :: Encode CampaignLimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Campaign definition
newtype CampaignResponse = CampaignResponse 
  { "AdditionalTreatments" :: NullOrUndefined.NullOrUndefined (ListOfTreatmentResource)
  , "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultState" :: NullOrUndefined.NullOrUndefined (CampaignState)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HoldoutPercent" :: NullOrUndefined.NullOrUndefined (Int)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsPaused" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Limits" :: NullOrUndefined.NullOrUndefined (CampaignLimits)
  , "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (MessageConfiguration)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (Schedule)
  , "SegmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentVersion" :: NullOrUndefined.NullOrUndefined (Int)
  , "State" :: NullOrUndefined.NullOrUndefined (CampaignState)
  , "TreatmentDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeCampaignResponse :: Newtype CampaignResponse _
derive instance repGenericCampaignResponse :: Generic CampaignResponse _
instance showCampaignResponse :: Show CampaignResponse where
  show = genericShow
instance decodeCampaignResponse :: Decode CampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignResponse :: Encode CampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | SMS message configuration.
newtype CampaignSmsMessage = CampaignSmsMessage 
  { "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined.NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCampaignSmsMessage :: Newtype CampaignSmsMessage _
derive instance repGenericCampaignSmsMessage :: Generic CampaignSmsMessage _
instance showCampaignSmsMessage :: Show CampaignSmsMessage where
  show = genericShow
instance decodeCampaignSmsMessage :: Decode CampaignSmsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignSmsMessage :: Encode CampaignSmsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | State of the Campaign
newtype CampaignState = CampaignState 
  { "CampaignStatus" :: NullOrUndefined.NullOrUndefined (CampaignStatus)
  }
derive instance newtypeCampaignState :: Newtype CampaignState _
derive instance repGenericCampaignState :: Generic CampaignState _
instance showCampaignState :: Show CampaignState where
  show = genericShow
instance decodeCampaignState :: Decode CampaignState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignState :: Encode CampaignState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CampaignStatus = CampaignStatus String
derive instance newtypeCampaignStatus :: Newtype CampaignStatus _
derive instance repGenericCampaignStatus :: Generic CampaignStatus _
instance showCampaignStatus :: Show CampaignStatus where
  show = genericShow
instance decodeCampaignStatus :: Decode CampaignStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignStatus :: Encode CampaignStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of available campaigns.
newtype CampaignsResponse = CampaignsResponse 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfCampaignResponse)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCampaignsResponse :: Newtype CampaignsResponse _
derive instance repGenericCampaignsResponse :: Generic CampaignsResponse _
instance showCampaignsResponse :: Show CampaignsResponse where
  show = genericShow
instance decodeCampaignsResponse :: Decode CampaignsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCampaignsResponse :: Encode CampaignsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChannelType = ChannelType String
derive instance newtypeChannelType :: Newtype ChannelType _
derive instance repGenericChannelType :: Generic ChannelType _
instance showChannelType :: Show ChannelType where
  show = genericShow
instance decodeChannelType :: Decode ChannelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelType :: Encode ChannelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAppRequest = CreateAppRequest 
  { "CreateApplicationRequest" :: (CreateApplicationRequest)
  }
derive instance newtypeCreateAppRequest :: Newtype CreateAppRequest _
derive instance repGenericCreateAppRequest :: Generic CreateAppRequest _
instance showCreateAppRequest :: Show CreateAppRequest where
  show = genericShow
instance decodeCreateAppRequest :: Decode CreateAppRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAppRequest :: Encode CreateAppRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAppResponse = CreateAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeCreateAppResponse :: Newtype CreateAppResponse _
derive instance repGenericCreateAppResponse :: Generic CreateAppResponse _
instance showCreateAppResponse :: Show CreateAppResponse where
  show = genericShow
instance decodeCreateAppResponse :: Decode CreateAppResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAppResponse :: Encode CreateAppResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Application Request.
newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _
derive instance repGenericCreateApplicationRequest :: Generic CreateApplicationRequest _
instance showCreateApplicationRequest :: Show CreateApplicationRequest where
  show = genericShow
instance decodeCreateApplicationRequest :: Decode CreateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationRequest :: Encode CreateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCampaignRequest = CreateCampaignRequest 
  { "ApplicationId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }
derive instance newtypeCreateCampaignRequest :: Newtype CreateCampaignRequest _
derive instance repGenericCreateCampaignRequest :: Generic CreateCampaignRequest _
instance showCreateCampaignRequest :: Show CreateCampaignRequest where
  show = genericShow
instance decodeCreateCampaignRequest :: Decode CreateCampaignRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCampaignRequest :: Encode CreateCampaignRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCampaignResponse = CreateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeCreateCampaignResponse :: Newtype CreateCampaignResponse _
derive instance repGenericCreateCampaignResponse :: Generic CreateCampaignResponse _
instance showCreateCampaignResponse :: Show CreateCampaignResponse where
  show = genericShow
instance decodeCreateCampaignResponse :: Decode CreateCampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCampaignResponse :: Encode CreateCampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateImportJobRequest = CreateImportJobRequest 
  { "ApplicationId" :: (String)
  , "ImportJobRequest" :: (ImportJobRequest)
  }
derive instance newtypeCreateImportJobRequest :: Newtype CreateImportJobRequest _
derive instance repGenericCreateImportJobRequest :: Generic CreateImportJobRequest _
instance showCreateImportJobRequest :: Show CreateImportJobRequest where
  show = genericShow
instance decodeCreateImportJobRequest :: Decode CreateImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateImportJobRequest :: Encode CreateImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateImportJobResponse = CreateImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }
derive instance newtypeCreateImportJobResponse :: Newtype CreateImportJobResponse _
derive instance repGenericCreateImportJobResponse :: Generic CreateImportJobResponse _
instance showCreateImportJobResponse :: Show CreateImportJobResponse where
  show = genericShow
instance decodeCreateImportJobResponse :: Decode CreateImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateImportJobResponse :: Encode CreateImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSegmentRequest = CreateSegmentRequest 
  { "ApplicationId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }
derive instance newtypeCreateSegmentRequest :: Newtype CreateSegmentRequest _
derive instance repGenericCreateSegmentRequest :: Generic CreateSegmentRequest _
instance showCreateSegmentRequest :: Show CreateSegmentRequest where
  show = genericShow
instance decodeCreateSegmentRequest :: Decode CreateSegmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSegmentRequest :: Encode CreateSegmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSegmentResponse = CreateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeCreateSegmentResponse :: Newtype CreateSegmentResponse _
derive instance repGenericCreateSegmentResponse :: Generic CreateSegmentResponse _
instance showCreateSegmentResponse :: Show CreateSegmentResponse where
  show = genericShow
instance decodeCreateSegmentResponse :: Decode CreateSegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSegmentResponse :: Encode CreateSegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Default Message across push notification, email, and sms.
newtype DefaultMessage = DefaultMessage 
  { "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  }
derive instance newtypeDefaultMessage :: Newtype DefaultMessage _
derive instance repGenericDefaultMessage :: Generic DefaultMessage _
instance showDefaultMessage :: Show DefaultMessage where
  show = genericShow
instance decodeDefaultMessage :: Decode DefaultMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultMessage :: Encode DefaultMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Default Push Notification Message.
newtype DefaultPushNotificationMessage = DefaultPushNotificationMessage 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "Data" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDefaultPushNotificationMessage :: Newtype DefaultPushNotificationMessage _
derive instance repGenericDefaultPushNotificationMessage :: Generic DefaultPushNotificationMessage _
instance showDefaultPushNotificationMessage :: Show DefaultPushNotificationMessage where
  show = genericShow
instance decodeDefaultPushNotificationMessage :: Decode DefaultPushNotificationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultPushNotificationMessage :: Encode DefaultPushNotificationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAdmChannelRequest = DeleteAdmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteAdmChannelRequest :: Newtype DeleteAdmChannelRequest _
derive instance repGenericDeleteAdmChannelRequest :: Generic DeleteAdmChannelRequest _
instance showDeleteAdmChannelRequest :: Show DeleteAdmChannelRequest where
  show = genericShow
instance decodeDeleteAdmChannelRequest :: Decode DeleteAdmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAdmChannelRequest :: Encode DeleteAdmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAdmChannelResponse = DeleteAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeDeleteAdmChannelResponse :: Newtype DeleteAdmChannelResponse _
derive instance repGenericDeleteAdmChannelResponse :: Generic DeleteAdmChannelResponse _
instance showDeleteAdmChannelResponse :: Show DeleteAdmChannelResponse where
  show = genericShow
instance decodeDeleteAdmChannelResponse :: Decode DeleteAdmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAdmChannelResponse :: Encode DeleteAdmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsChannelRequest = DeleteApnsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsChannelRequest :: Newtype DeleteApnsChannelRequest _
derive instance repGenericDeleteApnsChannelRequest :: Generic DeleteApnsChannelRequest _
instance showDeleteApnsChannelRequest :: Show DeleteApnsChannelRequest where
  show = genericShow
instance decodeDeleteApnsChannelRequest :: Decode DeleteApnsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsChannelRequest :: Encode DeleteApnsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsChannelResponse = DeleteApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeDeleteApnsChannelResponse :: Newtype DeleteApnsChannelResponse _
derive instance repGenericDeleteApnsChannelResponse :: Generic DeleteApnsChannelResponse _
instance showDeleteApnsChannelResponse :: Show DeleteApnsChannelResponse where
  show = genericShow
instance decodeDeleteApnsChannelResponse :: Decode DeleteApnsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsChannelResponse :: Encode DeleteApnsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsSandboxChannelRequest = DeleteApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsSandboxChannelRequest :: Newtype DeleteApnsSandboxChannelRequest _
derive instance repGenericDeleteApnsSandboxChannelRequest :: Generic DeleteApnsSandboxChannelRequest _
instance showDeleteApnsSandboxChannelRequest :: Show DeleteApnsSandboxChannelRequest where
  show = genericShow
instance decodeDeleteApnsSandboxChannelRequest :: Decode DeleteApnsSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsSandboxChannelRequest :: Encode DeleteApnsSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeDeleteApnsSandboxChannelResponse :: Newtype DeleteApnsSandboxChannelResponse _
derive instance repGenericDeleteApnsSandboxChannelResponse :: Generic DeleteApnsSandboxChannelResponse _
instance showDeleteApnsSandboxChannelResponse :: Show DeleteApnsSandboxChannelResponse where
  show = genericShow
instance decodeDeleteApnsSandboxChannelResponse :: Decode DeleteApnsSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsSandboxChannelResponse :: Encode DeleteApnsSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsVoipChannelRequest = DeleteApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsVoipChannelRequest :: Newtype DeleteApnsVoipChannelRequest _
derive instance repGenericDeleteApnsVoipChannelRequest :: Generic DeleteApnsVoipChannelRequest _
instance showDeleteApnsVoipChannelRequest :: Show DeleteApnsVoipChannelRequest where
  show = genericShow
instance decodeDeleteApnsVoipChannelRequest :: Decode DeleteApnsVoipChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsVoipChannelRequest :: Encode DeleteApnsVoipChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsVoipChannelResponse = DeleteApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeDeleteApnsVoipChannelResponse :: Newtype DeleteApnsVoipChannelResponse _
derive instance repGenericDeleteApnsVoipChannelResponse :: Generic DeleteApnsVoipChannelResponse _
instance showDeleteApnsVoipChannelResponse :: Show DeleteApnsVoipChannelResponse where
  show = genericShow
instance decodeDeleteApnsVoipChannelResponse :: Decode DeleteApnsVoipChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsVoipChannelResponse :: Encode DeleteApnsVoipChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsVoipSandboxChannelRequest = DeleteApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsVoipSandboxChannelRequest :: Newtype DeleteApnsVoipSandboxChannelRequest _
derive instance repGenericDeleteApnsVoipSandboxChannelRequest :: Generic DeleteApnsVoipSandboxChannelRequest _
instance showDeleteApnsVoipSandboxChannelRequest :: Show DeleteApnsVoipSandboxChannelRequest where
  show = genericShow
instance decodeDeleteApnsVoipSandboxChannelRequest :: Decode DeleteApnsVoipSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsVoipSandboxChannelRequest :: Encode DeleteApnsVoipSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeDeleteApnsVoipSandboxChannelResponse :: Newtype DeleteApnsVoipSandboxChannelResponse _
derive instance repGenericDeleteApnsVoipSandboxChannelResponse :: Generic DeleteApnsVoipSandboxChannelResponse _
instance showDeleteApnsVoipSandboxChannelResponse :: Show DeleteApnsVoipSandboxChannelResponse where
  show = genericShow
instance decodeDeleteApnsVoipSandboxChannelResponse :: Decode DeleteApnsVoipSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApnsVoipSandboxChannelResponse :: Encode DeleteApnsVoipSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAppRequest = DeleteAppRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteAppRequest :: Newtype DeleteAppRequest _
derive instance repGenericDeleteAppRequest :: Generic DeleteAppRequest _
instance showDeleteAppRequest :: Show DeleteAppRequest where
  show = genericShow
instance decodeDeleteAppRequest :: Decode DeleteAppRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAppRequest :: Encode DeleteAppRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAppResponse = DeleteAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeDeleteAppResponse :: Newtype DeleteAppResponse _
derive instance repGenericDeleteAppResponse :: Generic DeleteAppResponse _
instance showDeleteAppResponse :: Show DeleteAppResponse where
  show = genericShow
instance decodeDeleteAppResponse :: Decode DeleteAppResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAppResponse :: Encode DeleteAppResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBaiduChannelRequest = DeleteBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteBaiduChannelRequest :: Newtype DeleteBaiduChannelRequest _
derive instance repGenericDeleteBaiduChannelRequest :: Generic DeleteBaiduChannelRequest _
instance showDeleteBaiduChannelRequest :: Show DeleteBaiduChannelRequest where
  show = genericShow
instance decodeDeleteBaiduChannelRequest :: Decode DeleteBaiduChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBaiduChannelRequest :: Encode DeleteBaiduChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBaiduChannelResponse = DeleteBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeDeleteBaiduChannelResponse :: Newtype DeleteBaiduChannelResponse _
derive instance repGenericDeleteBaiduChannelResponse :: Generic DeleteBaiduChannelResponse _
instance showDeleteBaiduChannelResponse :: Show DeleteBaiduChannelResponse where
  show = genericShow
instance decodeDeleteBaiduChannelResponse :: Decode DeleteBaiduChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBaiduChannelResponse :: Encode DeleteBaiduChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCampaignRequest = DeleteCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }
derive instance newtypeDeleteCampaignRequest :: Newtype DeleteCampaignRequest _
derive instance repGenericDeleteCampaignRequest :: Generic DeleteCampaignRequest _
instance showDeleteCampaignRequest :: Show DeleteCampaignRequest where
  show = genericShow
instance decodeDeleteCampaignRequest :: Decode DeleteCampaignRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCampaignRequest :: Encode DeleteCampaignRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCampaignResponse = DeleteCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeDeleteCampaignResponse :: Newtype DeleteCampaignResponse _
derive instance repGenericDeleteCampaignResponse :: Generic DeleteCampaignResponse _
instance showDeleteCampaignResponse :: Show DeleteCampaignResponse where
  show = genericShow
instance decodeDeleteCampaignResponse :: Decode DeleteCampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCampaignResponse :: Encode DeleteCampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEmailChannelRequest = DeleteEmailChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteEmailChannelRequest :: Newtype DeleteEmailChannelRequest _
derive instance repGenericDeleteEmailChannelRequest :: Generic DeleteEmailChannelRequest _
instance showDeleteEmailChannelRequest :: Show DeleteEmailChannelRequest where
  show = genericShow
instance decodeDeleteEmailChannelRequest :: Decode DeleteEmailChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEmailChannelRequest :: Encode DeleteEmailChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEmailChannelResponse = DeleteEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeDeleteEmailChannelResponse :: Newtype DeleteEmailChannelResponse _
derive instance repGenericDeleteEmailChannelResponse :: Generic DeleteEmailChannelResponse _
instance showDeleteEmailChannelResponse :: Show DeleteEmailChannelResponse where
  show = genericShow
instance decodeDeleteEmailChannelResponse :: Decode DeleteEmailChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEmailChannelResponse :: Encode DeleteEmailChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DeleteEventStream Request
newtype DeleteEventStreamRequest = DeleteEventStreamRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteEventStreamRequest :: Newtype DeleteEventStreamRequest _
derive instance repGenericDeleteEventStreamRequest :: Generic DeleteEventStreamRequest _
instance showDeleteEventStreamRequest :: Show DeleteEventStreamRequest where
  show = genericShow
instance decodeDeleteEventStreamRequest :: Decode DeleteEventStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEventStreamRequest :: Encode DeleteEventStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEventStreamResponse = DeleteEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypeDeleteEventStreamResponse :: Newtype DeleteEventStreamResponse _
derive instance repGenericDeleteEventStreamResponse :: Generic DeleteEventStreamResponse _
instance showDeleteEventStreamResponse :: Show DeleteEventStreamResponse where
  show = genericShow
instance decodeDeleteEventStreamResponse :: Decode DeleteEventStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEventStreamResponse :: Encode DeleteEventStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGcmChannelRequest = DeleteGcmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteGcmChannelRequest :: Newtype DeleteGcmChannelRequest _
derive instance repGenericDeleteGcmChannelRequest :: Generic DeleteGcmChannelRequest _
instance showDeleteGcmChannelRequest :: Show DeleteGcmChannelRequest where
  show = genericShow
instance decodeDeleteGcmChannelRequest :: Decode DeleteGcmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGcmChannelRequest :: Encode DeleteGcmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGcmChannelResponse = DeleteGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeDeleteGcmChannelResponse :: Newtype DeleteGcmChannelResponse _
derive instance repGenericDeleteGcmChannelResponse :: Generic DeleteGcmChannelResponse _
instance showDeleteGcmChannelResponse :: Show DeleteGcmChannelResponse where
  show = genericShow
instance decodeDeleteGcmChannelResponse :: Decode DeleteGcmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGcmChannelResponse :: Encode DeleteGcmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSegmentRequest = DeleteSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }
derive instance newtypeDeleteSegmentRequest :: Newtype DeleteSegmentRequest _
derive instance repGenericDeleteSegmentRequest :: Generic DeleteSegmentRequest _
instance showDeleteSegmentRequest :: Show DeleteSegmentRequest where
  show = genericShow
instance decodeDeleteSegmentRequest :: Decode DeleteSegmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSegmentRequest :: Encode DeleteSegmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSegmentResponse = DeleteSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeDeleteSegmentResponse :: Newtype DeleteSegmentResponse _
derive instance repGenericDeleteSegmentResponse :: Generic DeleteSegmentResponse _
instance showDeleteSegmentResponse :: Show DeleteSegmentResponse where
  show = genericShow
instance decodeDeleteSegmentResponse :: Decode DeleteSegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSegmentResponse :: Encode DeleteSegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSmsChannelRequest = DeleteSmsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteSmsChannelRequest :: Newtype DeleteSmsChannelRequest _
derive instance repGenericDeleteSmsChannelRequest :: Generic DeleteSmsChannelRequest _
instance showDeleteSmsChannelRequest :: Show DeleteSmsChannelRequest where
  show = genericShow
instance decodeDeleteSmsChannelRequest :: Decode DeleteSmsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSmsChannelRequest :: Encode DeleteSmsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSmsChannelResponse = DeleteSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeDeleteSmsChannelResponse :: Newtype DeleteSmsChannelResponse _
derive instance repGenericDeleteSmsChannelResponse :: Generic DeleteSmsChannelResponse _
instance showDeleteSmsChannelResponse :: Show DeleteSmsChannelResponse where
  show = genericShow
instance decodeDeleteSmsChannelResponse :: Decode DeleteSmsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSmsChannelResponse :: Encode DeleteSmsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStatus = DeliveryStatus String
derive instance newtypeDeliveryStatus :: Newtype DeliveryStatus _
derive instance repGenericDeliveryStatus :: Generic DeliveryStatus _
instance showDeliveryStatus :: Show DeliveryStatus where
  show = genericShow
instance decodeDeliveryStatus :: Decode DeliveryStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStatus :: Encode DeliveryStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionType = DimensionType String
derive instance newtypeDimensionType :: Newtype DimensionType _
derive instance repGenericDimensionType :: Generic DimensionType _
instance showDimensionType :: Show DimensionType where
  show = genericShow
instance decodeDimensionType :: Decode DimensionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionType :: Encode DimensionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The message configuration.
newtype DirectMessageConfiguration = DirectMessageConfiguration 
  { "ADMMessage" :: NullOrUndefined.NullOrUndefined (ADMMessage)
  , "APNSMessage" :: NullOrUndefined.NullOrUndefined (APNSMessage)
  , "BaiduMessage" :: NullOrUndefined.NullOrUndefined (BaiduMessage)
  , "DefaultMessage" :: NullOrUndefined.NullOrUndefined (DefaultMessage)
  , "DefaultPushNotificationMessage" :: NullOrUndefined.NullOrUndefined (DefaultPushNotificationMessage)
  , "GCMMessage" :: NullOrUndefined.NullOrUndefined (GCMMessage)
  , "SMSMessage" :: NullOrUndefined.NullOrUndefined (SMSMessage)
  }
derive instance newtypeDirectMessageConfiguration :: Newtype DirectMessageConfiguration _
derive instance repGenericDirectMessageConfiguration :: Generic DirectMessageConfiguration _
instance showDirectMessageConfiguration :: Show DirectMessageConfiguration where
  show = genericShow
instance decodeDirectMessageConfiguration :: Decode DirectMessageConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectMessageConfiguration :: Encode DirectMessageConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Duration = Duration String
derive instance newtypeDuration :: Newtype Duration _
derive instance repGenericDuration :: Generic Duration _
instance showDuration :: Show Duration where
  show = genericShow
instance decodeDuration :: Decode Duration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuration :: Encode Duration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Email Channel Request
newtype EmailChannelRequest = EmailChannelRequest 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FromAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "Identity" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEmailChannelRequest :: Newtype EmailChannelRequest _
derive instance repGenericEmailChannelRequest :: Generic EmailChannelRequest _
instance showEmailChannelRequest :: Show EmailChannelRequest where
  show = genericShow
instance decodeEmailChannelRequest :: Decode EmailChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailChannelRequest :: Encode EmailChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Email Channel Response.
newtype EmailChannelResponse = EmailChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "FromAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Identity" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeEmailChannelResponse :: Newtype EmailChannelResponse _
derive instance repGenericEmailChannelResponse :: Generic EmailChannelResponse _
instance showEmailChannelResponse :: Show EmailChannelResponse where
  show = genericShow
instance decodeEmailChannelResponse :: Decode EmailChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailChannelResponse :: Encode EmailChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint update request
newtype EndpointBatchItem = EndpointBatchItem 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "Demographic" :: NullOrUndefined.NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Location" :: NullOrUndefined.NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined.NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "User" :: NullOrUndefined.NullOrUndefined (EndpointUser)
  }
derive instance newtypeEndpointBatchItem :: Newtype EndpointBatchItem _
derive instance repGenericEndpointBatchItem :: Generic EndpointBatchItem _
instance showEndpointBatchItem :: Show EndpointBatchItem where
  show = genericShow
instance decodeEndpointBatchItem :: Decode EndpointBatchItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointBatchItem :: Encode EndpointBatchItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint batch update request.
newtype EndpointBatchRequest = EndpointBatchRequest 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfEndpointBatchItem)
  }
derive instance newtypeEndpointBatchRequest :: Newtype EndpointBatchRequest _
derive instance repGenericEndpointBatchRequest :: Generic EndpointBatchRequest _
instance showEndpointBatchRequest :: Show EndpointBatchRequest where
  show = genericShow
instance decodeEndpointBatchRequest :: Decode EndpointBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointBatchRequest :: Encode EndpointBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint demographic data
newtype EndpointDemographic = EndpointDemographic 
  { "AppVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Locale" :: NullOrUndefined.NullOrUndefined (String)
  , "Make" :: NullOrUndefined.NullOrUndefined (String)
  , "Model" :: NullOrUndefined.NullOrUndefined (String)
  , "ModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "PlatformVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Timezone" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointDemographic :: Newtype EndpointDemographic _
derive instance repGenericEndpointDemographic :: Generic EndpointDemographic _
instance showEndpointDemographic :: Show EndpointDemographic where
  show = genericShow
instance decodeEndpointDemographic :: Decode EndpointDemographic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointDemographic :: Encode EndpointDemographic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint location data
newtype EndpointLocation = EndpointLocation 
  { "City" :: NullOrUndefined.NullOrUndefined (String)
  , "Country" :: NullOrUndefined.NullOrUndefined (String)
  , "Latitude" :: NullOrUndefined.NullOrUndefined (Number)
  , "Longitude" :: NullOrUndefined.NullOrUndefined (Number)
  , "PostalCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Region" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointLocation :: Newtype EndpointLocation _
derive instance repGenericEndpointLocation :: Generic EndpointLocation _
instance showEndpointLocation :: Show EndpointLocation where
  show = genericShow
instance decodeEndpointLocation :: Decode EndpointLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointLocation :: Encode EndpointLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The result from sending a message to an endpoint.
newtype EndpointMessageResult = EndpointMessageResult 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "DeliveryStatus" :: NullOrUndefined.NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined.NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointMessageResult :: Newtype EndpointMessageResult _
derive instance repGenericEndpointMessageResult :: Generic EndpointMessageResult _
instance showEndpointMessageResult :: Show EndpointMessageResult where
  show = genericShow
instance decodeEndpointMessageResult :: Decode EndpointMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointMessageResult :: Encode EndpointMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint update request
newtype EndpointRequest = EndpointRequest 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "Demographic" :: NullOrUndefined.NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Location" :: NullOrUndefined.NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined.NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "User" :: NullOrUndefined.NullOrUndefined (EndpointUser)
  }
derive instance newtypeEndpointRequest :: Newtype EndpointRequest _
derive instance repGenericEndpointRequest :: Generic EndpointRequest _
instance showEndpointRequest :: Show EndpointRequest where
  show = genericShow
instance decodeEndpointRequest :: Decode EndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointRequest :: Encode EndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint response
newtype EndpointResponse = EndpointResponse 
  { "Address" :: NullOrUndefined.NullOrUndefined (String)
  , "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined.NullOrUndefined (ChannelType)
  , "CohortId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Demographic" :: NullOrUndefined.NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Location" :: NullOrUndefined.NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined.NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "User" :: NullOrUndefined.NullOrUndefined (EndpointUser)
  }
derive instance newtypeEndpointResponse :: Newtype EndpointResponse _
derive instance repGenericEndpointResponse :: Generic EndpointResponse _
instance showEndpointResponse :: Show EndpointResponse where
  show = genericShow
instance decodeEndpointResponse :: Decode EndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointResponse :: Encode EndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint send configuration.
newtype EndpointSendConfiguration = EndpointSendConfiguration 
  { "BodyOverride" :: NullOrUndefined.NullOrUndefined (String)
  , "Context" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointSendConfiguration :: Newtype EndpointSendConfiguration _
derive instance repGenericEndpointSendConfiguration :: Generic EndpointSendConfiguration _
instance showEndpointSendConfiguration :: Show EndpointSendConfiguration where
  show = genericShow
instance decodeEndpointSendConfiguration :: Decode EndpointSendConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointSendConfiguration :: Encode EndpointSendConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint user specific custom userAttributes
newtype EndpointUser = EndpointUser 
  { "UserAttributes" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "UserId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpointUser :: Newtype EndpointUser _
derive instance repGenericEndpointUser :: Generic EndpointUser _
instance showEndpointUser :: Show EndpointUser where
  show = genericShow
instance decodeEndpointUser :: Decode EndpointUser where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointUser :: Encode EndpointUser where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Model for an event publishing subscription export.
newtype EventStream = EventStream 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "DestinationStreamArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdatedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEventStream :: Newtype EventStream _
derive instance repGenericEventStream :: Generic EventStream _
instance showEventStream :: Show EventStream where
  show = genericShow
instance decodeEventStream :: Decode EventStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventStream :: Encode EventStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _
derive instance repGenericForbiddenException :: Generic ForbiddenException _
instance showForbiddenException :: Show ForbiddenException where
  show = genericShow
instance decodeForbiddenException :: Decode ForbiddenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForbiddenException :: Encode ForbiddenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Format = Format String
derive instance newtypeFormat :: Newtype Format _
derive instance repGenericFormat :: Generic Format _
instance showFormat :: Show Format where
  show = genericShow
instance decodeFormat :: Decode Format where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFormat :: Encode Format where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Frequency = Frequency String
derive instance newtypeFrequency :: Newtype Frequency _
derive instance repGenericFrequency :: Generic Frequency _
instance showFrequency :: Show Frequency where
  show = genericShow
instance decodeFrequency :: Decode Frequency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFrequency :: Encode Frequency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Google Cloud Messaging credentials
newtype GCMChannelRequest = GCMChannelRequest 
  { "ApiKey" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGCMChannelRequest :: Newtype GCMChannelRequest _
derive instance repGenericGCMChannelRequest :: Generic GCMChannelRequest _
instance showGCMChannelRequest :: Show GCMChannelRequest where
  show = genericShow
instance decodeGCMChannelRequest :: Decode GCMChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGCMChannelRequest :: Encode GCMChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Google Cloud Messaging channel definition
newtype GCMChannelResponse = GCMChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Credential" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeGCMChannelResponse :: Newtype GCMChannelResponse _
derive instance repGenericGCMChannelResponse :: Generic GCMChannelResponse _
instance showGCMChannelResponse :: Show GCMChannelResponse where
  show = genericShow
instance decodeGCMChannelResponse :: Decode GCMChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGCMChannelResponse :: Encode GCMChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | GCM Message.
newtype GCMMessage = GCMMessage 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "CollapseKey" :: NullOrUndefined.NullOrUndefined (String)
  , "Data" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "IconReference" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Priority" :: NullOrUndefined.NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "RestrictedPackageName" :: NullOrUndefined.NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Sound" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  , "TimeToLive" :: NullOrUndefined.NullOrUndefined (Int)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGCMMessage :: Newtype GCMMessage _
derive instance repGenericGCMMessage :: Generic GCMMessage _
instance showGCMMessage :: Show GCMMessage where
  show = genericShow
instance decodeGCMMessage :: Decode GCMMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGCMMessage :: Encode GCMMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAdmChannelRequest = GetAdmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetAdmChannelRequest :: Newtype GetAdmChannelRequest _
derive instance repGenericGetAdmChannelRequest :: Generic GetAdmChannelRequest _
instance showGetAdmChannelRequest :: Show GetAdmChannelRequest where
  show = genericShow
instance decodeGetAdmChannelRequest :: Decode GetAdmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAdmChannelRequest :: Encode GetAdmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAdmChannelResponse = GetAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeGetAdmChannelResponse :: Newtype GetAdmChannelResponse _
derive instance repGenericGetAdmChannelResponse :: Generic GetAdmChannelResponse _
instance showGetAdmChannelResponse :: Show GetAdmChannelResponse where
  show = genericShow
instance decodeGetAdmChannelResponse :: Decode GetAdmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAdmChannelResponse :: Encode GetAdmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsChannelRequest = GetApnsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsChannelRequest :: Newtype GetApnsChannelRequest _
derive instance repGenericGetApnsChannelRequest :: Generic GetApnsChannelRequest _
instance showGetApnsChannelRequest :: Show GetApnsChannelRequest where
  show = genericShow
instance decodeGetApnsChannelRequest :: Decode GetApnsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsChannelRequest :: Encode GetApnsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsChannelResponse = GetApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeGetApnsChannelResponse :: Newtype GetApnsChannelResponse _
derive instance repGenericGetApnsChannelResponse :: Generic GetApnsChannelResponse _
instance showGetApnsChannelResponse :: Show GetApnsChannelResponse where
  show = genericShow
instance decodeGetApnsChannelResponse :: Decode GetApnsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsChannelResponse :: Encode GetApnsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsSandboxChannelRequest = GetApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsSandboxChannelRequest :: Newtype GetApnsSandboxChannelRequest _
derive instance repGenericGetApnsSandboxChannelRequest :: Generic GetApnsSandboxChannelRequest _
instance showGetApnsSandboxChannelRequest :: Show GetApnsSandboxChannelRequest where
  show = genericShow
instance decodeGetApnsSandboxChannelRequest :: Decode GetApnsSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsSandboxChannelRequest :: Encode GetApnsSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeGetApnsSandboxChannelResponse :: Newtype GetApnsSandboxChannelResponse _
derive instance repGenericGetApnsSandboxChannelResponse :: Generic GetApnsSandboxChannelResponse _
instance showGetApnsSandboxChannelResponse :: Show GetApnsSandboxChannelResponse where
  show = genericShow
instance decodeGetApnsSandboxChannelResponse :: Decode GetApnsSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsSandboxChannelResponse :: Encode GetApnsSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsVoipChannelRequest = GetApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsVoipChannelRequest :: Newtype GetApnsVoipChannelRequest _
derive instance repGenericGetApnsVoipChannelRequest :: Generic GetApnsVoipChannelRequest _
instance showGetApnsVoipChannelRequest :: Show GetApnsVoipChannelRequest where
  show = genericShow
instance decodeGetApnsVoipChannelRequest :: Decode GetApnsVoipChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsVoipChannelRequest :: Encode GetApnsVoipChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsVoipChannelResponse = GetApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeGetApnsVoipChannelResponse :: Newtype GetApnsVoipChannelResponse _
derive instance repGenericGetApnsVoipChannelResponse :: Generic GetApnsVoipChannelResponse _
instance showGetApnsVoipChannelResponse :: Show GetApnsVoipChannelResponse where
  show = genericShow
instance decodeGetApnsVoipChannelResponse :: Decode GetApnsVoipChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsVoipChannelResponse :: Encode GetApnsVoipChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsVoipSandboxChannelRequest = GetApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsVoipSandboxChannelRequest :: Newtype GetApnsVoipSandboxChannelRequest _
derive instance repGenericGetApnsVoipSandboxChannelRequest :: Generic GetApnsVoipSandboxChannelRequest _
instance showGetApnsVoipSandboxChannelRequest :: Show GetApnsVoipSandboxChannelRequest where
  show = genericShow
instance decodeGetApnsVoipSandboxChannelRequest :: Decode GetApnsVoipSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsVoipSandboxChannelRequest :: Encode GetApnsVoipSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeGetApnsVoipSandboxChannelResponse :: Newtype GetApnsVoipSandboxChannelResponse _
derive instance repGenericGetApnsVoipSandboxChannelResponse :: Generic GetApnsVoipSandboxChannelResponse _
instance showGetApnsVoipSandboxChannelResponse :: Show GetApnsVoipSandboxChannelResponse where
  show = genericShow
instance decodeGetApnsVoipSandboxChannelResponse :: Decode GetApnsVoipSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApnsVoipSandboxChannelResponse :: Encode GetApnsVoipSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppRequest = GetAppRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetAppRequest :: Newtype GetAppRequest _
derive instance repGenericGetAppRequest :: Generic GetAppRequest _
instance showGetAppRequest :: Show GetAppRequest where
  show = genericShow
instance decodeGetAppRequest :: Decode GetAppRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppRequest :: Encode GetAppRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppResponse = GetAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeGetAppResponse :: Newtype GetAppResponse _
derive instance repGenericGetAppResponse :: Generic GetAppResponse _
instance showGetAppResponse :: Show GetAppResponse where
  show = genericShow
instance decodeGetAppResponse :: Decode GetAppResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppResponse :: Encode GetAppResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationSettingsRequest = GetApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApplicationSettingsRequest :: Newtype GetApplicationSettingsRequest _
derive instance repGenericGetApplicationSettingsRequest :: Generic GetApplicationSettingsRequest _
instance showGetApplicationSettingsRequest :: Show GetApplicationSettingsRequest where
  show = genericShow
instance decodeGetApplicationSettingsRequest :: Decode GetApplicationSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationSettingsRequest :: Encode GetApplicationSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationSettingsResponse = GetApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }
derive instance newtypeGetApplicationSettingsResponse :: Newtype GetApplicationSettingsResponse _
derive instance repGenericGetApplicationSettingsResponse :: Generic GetApplicationSettingsResponse _
instance showGetApplicationSettingsResponse :: Show GetApplicationSettingsResponse where
  show = genericShow
instance decodeGetApplicationSettingsResponse :: Decode GetApplicationSettingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationSettingsResponse :: Encode GetApplicationSettingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppsRequest = GetAppsRequest 
  { "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetAppsRequest :: Newtype GetAppsRequest _
derive instance repGenericGetAppsRequest :: Generic GetAppsRequest _
instance showGetAppsRequest :: Show GetAppsRequest where
  show = genericShow
instance decodeGetAppsRequest :: Decode GetAppsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppsRequest :: Encode GetAppsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAppsResponse = GetAppsResponse 
  { "ApplicationsResponse" :: (ApplicationsResponse)
  }
derive instance newtypeGetAppsResponse :: Newtype GetAppsResponse _
derive instance repGenericGetAppsResponse :: Generic GetAppsResponse _
instance showGetAppsResponse :: Show GetAppsResponse where
  show = genericShow
instance decodeGetAppsResponse :: Decode GetAppsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAppsResponse :: Encode GetAppsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBaiduChannelRequest = GetBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetBaiduChannelRequest :: Newtype GetBaiduChannelRequest _
derive instance repGenericGetBaiduChannelRequest :: Generic GetBaiduChannelRequest _
instance showGetBaiduChannelRequest :: Show GetBaiduChannelRequest where
  show = genericShow
instance decodeGetBaiduChannelRequest :: Decode GetBaiduChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBaiduChannelRequest :: Encode GetBaiduChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBaiduChannelResponse = GetBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeGetBaiduChannelResponse :: Newtype GetBaiduChannelResponse _
derive instance repGenericGetBaiduChannelResponse :: Generic GetBaiduChannelResponse _
instance showGetBaiduChannelResponse :: Show GetBaiduChannelResponse where
  show = genericShow
instance decodeGetBaiduChannelResponse :: Decode GetBaiduChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBaiduChannelResponse :: Encode GetBaiduChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignActivitiesRequest = GetCampaignActivitiesRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCampaignActivitiesRequest :: Newtype GetCampaignActivitiesRequest _
derive instance repGenericGetCampaignActivitiesRequest :: Generic GetCampaignActivitiesRequest _
instance showGetCampaignActivitiesRequest :: Show GetCampaignActivitiesRequest where
  show = genericShow
instance decodeGetCampaignActivitiesRequest :: Decode GetCampaignActivitiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignActivitiesRequest :: Encode GetCampaignActivitiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignActivitiesResponse = GetCampaignActivitiesResponse 
  { "ActivitiesResponse" :: (ActivitiesResponse)
  }
derive instance newtypeGetCampaignActivitiesResponse :: Newtype GetCampaignActivitiesResponse _
derive instance repGenericGetCampaignActivitiesResponse :: Generic GetCampaignActivitiesResponse _
instance showGetCampaignActivitiesResponse :: Show GetCampaignActivitiesResponse where
  show = genericShow
instance decodeGetCampaignActivitiesResponse :: Decode GetCampaignActivitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignActivitiesResponse :: Encode GetCampaignActivitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignRequest = GetCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }
derive instance newtypeGetCampaignRequest :: Newtype GetCampaignRequest _
derive instance repGenericGetCampaignRequest :: Generic GetCampaignRequest _
instance showGetCampaignRequest :: Show GetCampaignRequest where
  show = genericShow
instance decodeGetCampaignRequest :: Decode GetCampaignRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignRequest :: Encode GetCampaignRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignResponse = GetCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeGetCampaignResponse :: Newtype GetCampaignResponse _
derive instance repGenericGetCampaignResponse :: Generic GetCampaignResponse _
instance showGetCampaignResponse :: Show GetCampaignResponse where
  show = genericShow
instance decodeGetCampaignResponse :: Decode GetCampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignResponse :: Encode GetCampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignVersionRequest = GetCampaignVersionRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "Version" :: (String)
  }
derive instance newtypeGetCampaignVersionRequest :: Newtype GetCampaignVersionRequest _
derive instance repGenericGetCampaignVersionRequest :: Generic GetCampaignVersionRequest _
instance showGetCampaignVersionRequest :: Show GetCampaignVersionRequest where
  show = genericShow
instance decodeGetCampaignVersionRequest :: Decode GetCampaignVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignVersionRequest :: Encode GetCampaignVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignVersionResponse = GetCampaignVersionResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeGetCampaignVersionResponse :: Newtype GetCampaignVersionResponse _
derive instance repGenericGetCampaignVersionResponse :: Generic GetCampaignVersionResponse _
instance showGetCampaignVersionResponse :: Show GetCampaignVersionResponse where
  show = genericShow
instance decodeGetCampaignVersionResponse :: Decode GetCampaignVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignVersionResponse :: Encode GetCampaignVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignVersionsRequest = GetCampaignVersionsRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCampaignVersionsRequest :: Newtype GetCampaignVersionsRequest _
derive instance repGenericGetCampaignVersionsRequest :: Generic GetCampaignVersionsRequest _
instance showGetCampaignVersionsRequest :: Show GetCampaignVersionsRequest where
  show = genericShow
instance decodeGetCampaignVersionsRequest :: Decode GetCampaignVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignVersionsRequest :: Encode GetCampaignVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignVersionsResponse = GetCampaignVersionsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }
derive instance newtypeGetCampaignVersionsResponse :: Newtype GetCampaignVersionsResponse _
derive instance repGenericGetCampaignVersionsResponse :: Generic GetCampaignVersionsResponse _
instance showGetCampaignVersionsResponse :: Show GetCampaignVersionsResponse where
  show = genericShow
instance decodeGetCampaignVersionsResponse :: Decode GetCampaignVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignVersionsResponse :: Encode GetCampaignVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignsRequest = GetCampaignsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCampaignsRequest :: Newtype GetCampaignsRequest _
derive instance repGenericGetCampaignsRequest :: Generic GetCampaignsRequest _
instance showGetCampaignsRequest :: Show GetCampaignsRequest where
  show = genericShow
instance decodeGetCampaignsRequest :: Decode GetCampaignsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignsRequest :: Encode GetCampaignsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCampaignsResponse = GetCampaignsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }
derive instance newtypeGetCampaignsResponse :: Newtype GetCampaignsResponse _
derive instance repGenericGetCampaignsResponse :: Generic GetCampaignsResponse _
instance showGetCampaignsResponse :: Show GetCampaignsResponse where
  show = genericShow
instance decodeGetCampaignsResponse :: Decode GetCampaignsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCampaignsResponse :: Encode GetCampaignsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEmailChannelRequest = GetEmailChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetEmailChannelRequest :: Newtype GetEmailChannelRequest _
derive instance repGenericGetEmailChannelRequest :: Generic GetEmailChannelRequest _
instance showGetEmailChannelRequest :: Show GetEmailChannelRequest where
  show = genericShow
instance decodeGetEmailChannelRequest :: Decode GetEmailChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEmailChannelRequest :: Encode GetEmailChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEmailChannelResponse = GetEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeGetEmailChannelResponse :: Newtype GetEmailChannelResponse _
derive instance repGenericGetEmailChannelResponse :: Generic GetEmailChannelResponse _
instance showGetEmailChannelResponse :: Show GetEmailChannelResponse where
  show = genericShow
instance decodeGetEmailChannelResponse :: Decode GetEmailChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEmailChannelResponse :: Encode GetEmailChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEndpointRequest = GetEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  }
derive instance newtypeGetEndpointRequest :: Newtype GetEndpointRequest _
derive instance repGenericGetEndpointRequest :: Generic GetEndpointRequest _
instance showGetEndpointRequest :: Show GetEndpointRequest where
  show = genericShow
instance decodeGetEndpointRequest :: Decode GetEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEndpointRequest :: Encode GetEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEndpointResponse = GetEndpointResponse 
  { "EndpointResponse" :: (EndpointResponse)
  }
derive instance newtypeGetEndpointResponse :: Newtype GetEndpointResponse _
derive instance repGenericGetEndpointResponse :: Generic GetEndpointResponse _
instance showGetEndpointResponse :: Show GetEndpointResponse where
  show = genericShow
instance decodeGetEndpointResponse :: Decode GetEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEndpointResponse :: Encode GetEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | GetEventStreamRequest
newtype GetEventStreamRequest = GetEventStreamRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetEventStreamRequest :: Newtype GetEventStreamRequest _
derive instance repGenericGetEventStreamRequest :: Generic GetEventStreamRequest _
instance showGetEventStreamRequest :: Show GetEventStreamRequest where
  show = genericShow
instance decodeGetEventStreamRequest :: Decode GetEventStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEventStreamRequest :: Encode GetEventStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEventStreamResponse = GetEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypeGetEventStreamResponse :: Newtype GetEventStreamResponse _
derive instance repGenericGetEventStreamResponse :: Generic GetEventStreamResponse _
instance showGetEventStreamResponse :: Show GetEventStreamResponse where
  show = genericShow
instance decodeGetEventStreamResponse :: Decode GetEventStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEventStreamResponse :: Encode GetEventStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGcmChannelRequest = GetGcmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetGcmChannelRequest :: Newtype GetGcmChannelRequest _
derive instance repGenericGetGcmChannelRequest :: Generic GetGcmChannelRequest _
instance showGetGcmChannelRequest :: Show GetGcmChannelRequest where
  show = genericShow
instance decodeGetGcmChannelRequest :: Decode GetGcmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGcmChannelRequest :: Encode GetGcmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGcmChannelResponse = GetGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeGetGcmChannelResponse :: Newtype GetGcmChannelResponse _
derive instance repGenericGetGcmChannelResponse :: Generic GetGcmChannelResponse _
instance showGetGcmChannelResponse :: Show GetGcmChannelResponse where
  show = genericShow
instance decodeGetGcmChannelResponse :: Decode GetGcmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGcmChannelResponse :: Encode GetGcmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportJobRequest = GetImportJobRequest 
  { "ApplicationId" :: (String)
  , "JobId" :: (String)
  }
derive instance newtypeGetImportJobRequest :: Newtype GetImportJobRequest _
derive instance repGenericGetImportJobRequest :: Generic GetImportJobRequest _
instance showGetImportJobRequest :: Show GetImportJobRequest where
  show = genericShow
instance decodeGetImportJobRequest :: Decode GetImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportJobRequest :: Encode GetImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportJobResponse = GetImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }
derive instance newtypeGetImportJobResponse :: Newtype GetImportJobResponse _
derive instance repGenericGetImportJobResponse :: Generic GetImportJobResponse _
instance showGetImportJobResponse :: Show GetImportJobResponse where
  show = genericShow
instance decodeGetImportJobResponse :: Decode GetImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportJobResponse :: Encode GetImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportJobsRequest = GetImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetImportJobsRequest :: Newtype GetImportJobsRequest _
derive instance repGenericGetImportJobsRequest :: Generic GetImportJobsRequest _
instance showGetImportJobsRequest :: Show GetImportJobsRequest where
  show = genericShow
instance decodeGetImportJobsRequest :: Decode GetImportJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportJobsRequest :: Encode GetImportJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetImportJobsResponse = GetImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }
derive instance newtypeGetImportJobsResponse :: Newtype GetImportJobsResponse _
derive instance repGenericGetImportJobsResponse :: Generic GetImportJobsResponse _
instance showGetImportJobsResponse :: Show GetImportJobsResponse where
  show = genericShow
instance decodeGetImportJobsResponse :: Decode GetImportJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetImportJobsResponse :: Encode GetImportJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentImportJobsRequest = GetSegmentImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSegmentImportJobsRequest :: Newtype GetSegmentImportJobsRequest _
derive instance repGenericGetSegmentImportJobsRequest :: Generic GetSegmentImportJobsRequest _
instance showGetSegmentImportJobsRequest :: Show GetSegmentImportJobsRequest where
  show = genericShow
instance decodeGetSegmentImportJobsRequest :: Decode GetSegmentImportJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentImportJobsRequest :: Encode GetSegmentImportJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentImportJobsResponse = GetSegmentImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }
derive instance newtypeGetSegmentImportJobsResponse :: Newtype GetSegmentImportJobsResponse _
derive instance repGenericGetSegmentImportJobsResponse :: Generic GetSegmentImportJobsResponse _
instance showGetSegmentImportJobsResponse :: Show GetSegmentImportJobsResponse where
  show = genericShow
instance decodeGetSegmentImportJobsResponse :: Decode GetSegmentImportJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentImportJobsResponse :: Encode GetSegmentImportJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentRequest = GetSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }
derive instance newtypeGetSegmentRequest :: Newtype GetSegmentRequest _
derive instance repGenericGetSegmentRequest :: Generic GetSegmentRequest _
instance showGetSegmentRequest :: Show GetSegmentRequest where
  show = genericShow
instance decodeGetSegmentRequest :: Decode GetSegmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentRequest :: Encode GetSegmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentResponse = GetSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeGetSegmentResponse :: Newtype GetSegmentResponse _
derive instance repGenericGetSegmentResponse :: Generic GetSegmentResponse _
instance showGetSegmentResponse :: Show GetSegmentResponse where
  show = genericShow
instance decodeGetSegmentResponse :: Decode GetSegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentResponse :: Encode GetSegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentVersionRequest = GetSegmentVersionRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "Version" :: (String)
  }
derive instance newtypeGetSegmentVersionRequest :: Newtype GetSegmentVersionRequest _
derive instance repGenericGetSegmentVersionRequest :: Generic GetSegmentVersionRequest _
instance showGetSegmentVersionRequest :: Show GetSegmentVersionRequest where
  show = genericShow
instance decodeGetSegmentVersionRequest :: Decode GetSegmentVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentVersionRequest :: Encode GetSegmentVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentVersionResponse = GetSegmentVersionResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeGetSegmentVersionResponse :: Newtype GetSegmentVersionResponse _
derive instance repGenericGetSegmentVersionResponse :: Generic GetSegmentVersionResponse _
instance showGetSegmentVersionResponse :: Show GetSegmentVersionResponse where
  show = genericShow
instance decodeGetSegmentVersionResponse :: Decode GetSegmentVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentVersionResponse :: Encode GetSegmentVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentVersionsRequest = GetSegmentVersionsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSegmentVersionsRequest :: Newtype GetSegmentVersionsRequest _
derive instance repGenericGetSegmentVersionsRequest :: Generic GetSegmentVersionsRequest _
instance showGetSegmentVersionsRequest :: Show GetSegmentVersionsRequest where
  show = genericShow
instance decodeGetSegmentVersionsRequest :: Decode GetSegmentVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentVersionsRequest :: Encode GetSegmentVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentVersionsResponse = GetSegmentVersionsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }
derive instance newtypeGetSegmentVersionsResponse :: Newtype GetSegmentVersionsResponse _
derive instance repGenericGetSegmentVersionsResponse :: Generic GetSegmentVersionsResponse _
instance showGetSegmentVersionsResponse :: Show GetSegmentVersionsResponse where
  show = genericShow
instance decodeGetSegmentVersionsResponse :: Decode GetSegmentVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentVersionsResponse :: Encode GetSegmentVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentsRequest = GetSegmentsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetSegmentsRequest :: Newtype GetSegmentsRequest _
derive instance repGenericGetSegmentsRequest :: Generic GetSegmentsRequest _
instance showGetSegmentsRequest :: Show GetSegmentsRequest where
  show = genericShow
instance decodeGetSegmentsRequest :: Decode GetSegmentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentsRequest :: Encode GetSegmentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSegmentsResponse = GetSegmentsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }
derive instance newtypeGetSegmentsResponse :: Newtype GetSegmentsResponse _
derive instance repGenericGetSegmentsResponse :: Generic GetSegmentsResponse _
instance showGetSegmentsResponse :: Show GetSegmentsResponse where
  show = genericShow
instance decodeGetSegmentsResponse :: Decode GetSegmentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSegmentsResponse :: Encode GetSegmentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSmsChannelRequest = GetSmsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetSmsChannelRequest :: Newtype GetSmsChannelRequest _
derive instance repGenericGetSmsChannelRequest :: Generic GetSmsChannelRequest _
instance showGetSmsChannelRequest :: Show GetSmsChannelRequest where
  show = genericShow
instance decodeGetSmsChannelRequest :: Decode GetSmsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSmsChannelRequest :: Encode GetSmsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetSmsChannelResponse = GetSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeGetSmsChannelResponse :: Newtype GetSmsChannelResponse _
derive instance repGenericGetSmsChannelResponse :: Generic GetSmsChannelResponse _
instance showGetSmsChannelResponse :: Show GetSmsChannelResponse where
  show = genericShow
instance decodeGetSmsChannelResponse :: Decode GetSmsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSmsChannelResponse :: Encode GetSmsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportJobRequest = ImportJobRequest 
  { "DefineSegment" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (String)
  , "Format" :: NullOrUndefined.NullOrUndefined (Format)
  , "RegisterEndpoints" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeImportJobRequest :: Newtype ImportJobRequest _
derive instance repGenericImportJobRequest :: Generic ImportJobRequest _
instance showImportJobRequest :: Show ImportJobRequest where
  show = genericShow
instance decodeImportJobRequest :: Decode ImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportJobRequest :: Encode ImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportJobResource = ImportJobResource 
  { "DefineSegment" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (String)
  , "Format" :: NullOrUndefined.NullOrUndefined (Format)
  , "RegisterEndpoints" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeImportJobResource :: Newtype ImportJobResource _
derive instance repGenericImportJobResource :: Generic ImportJobResource _
instance showImportJobResource :: Show ImportJobResource where
  show = genericShow
instance decodeImportJobResource :: Decode ImportJobResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportJobResource :: Encode ImportJobResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportJobResponse = ImportJobResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CompletedPieces" :: NullOrUndefined.NullOrUndefined (Int)
  , "CompletionDate" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Definition" :: NullOrUndefined.NullOrUndefined (ImportJobResource)
  , "FailedPieces" :: NullOrUndefined.NullOrUndefined (Int)
  , "Failures" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "JobStatus" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "TotalFailures" :: NullOrUndefined.NullOrUndefined (Int)
  , "TotalPieces" :: NullOrUndefined.NullOrUndefined (Int)
  , "TotalProcessed" :: NullOrUndefined.NullOrUndefined (Int)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeImportJobResponse :: Newtype ImportJobResponse _
derive instance repGenericImportJobResponse :: Generic ImportJobResponse _
instance showImportJobResponse :: Show ImportJobResponse where
  show = genericShow
instance decodeImportJobResponse :: Decode ImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportJobResponse :: Encode ImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Import job list.
newtype ImportJobsResponse = ImportJobsResponse 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfImportJobResponse)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeImportJobsResponse :: Newtype ImportJobsResponse _
derive instance repGenericImportJobsResponse :: Generic ImportJobsResponse _
instance showImportJobsResponse :: Show ImportJobsResponse where
  show = genericShow
instance decodeImportJobsResponse :: Decode ImportJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportJobsResponse :: Encode ImportJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfActivityResponse = ListOfActivityResponse (Array ActivityResponse)
derive instance newtypeListOfActivityResponse :: Newtype ListOfActivityResponse _
derive instance repGenericListOfActivityResponse :: Generic ListOfActivityResponse _
instance showListOfActivityResponse :: Show ListOfActivityResponse where
  show = genericShow
instance decodeListOfActivityResponse :: Decode ListOfActivityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfActivityResponse :: Encode ListOfActivityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfApplicationResponse = ListOfApplicationResponse (Array ApplicationResponse)
derive instance newtypeListOfApplicationResponse :: Newtype ListOfApplicationResponse _
derive instance repGenericListOfApplicationResponse :: Generic ListOfApplicationResponse _
instance showListOfApplicationResponse :: Show ListOfApplicationResponse where
  show = genericShow
instance decodeListOfApplicationResponse :: Decode ListOfApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfApplicationResponse :: Encode ListOfApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfCampaignResponse = ListOfCampaignResponse (Array CampaignResponse)
derive instance newtypeListOfCampaignResponse :: Newtype ListOfCampaignResponse _
derive instance repGenericListOfCampaignResponse :: Generic ListOfCampaignResponse _
instance showListOfCampaignResponse :: Show ListOfCampaignResponse where
  show = genericShow
instance decodeListOfCampaignResponse :: Decode ListOfCampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCampaignResponse :: Encode ListOfCampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfEndpointBatchItem = ListOfEndpointBatchItem (Array EndpointBatchItem)
derive instance newtypeListOfEndpointBatchItem :: Newtype ListOfEndpointBatchItem _
derive instance repGenericListOfEndpointBatchItem :: Generic ListOfEndpointBatchItem _
instance showListOfEndpointBatchItem :: Show ListOfEndpointBatchItem where
  show = genericShow
instance decodeListOfEndpointBatchItem :: Decode ListOfEndpointBatchItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfEndpointBatchItem :: Encode ListOfEndpointBatchItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfImportJobResponse = ListOfImportJobResponse (Array ImportJobResponse)
derive instance newtypeListOfImportJobResponse :: Newtype ListOfImportJobResponse _
derive instance repGenericListOfImportJobResponse :: Generic ListOfImportJobResponse _
instance showListOfImportJobResponse :: Show ListOfImportJobResponse where
  show = genericShow
instance decodeListOfImportJobResponse :: Decode ListOfImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfImportJobResponse :: Encode ListOfImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfSegmentResponse = ListOfSegmentResponse (Array SegmentResponse)
derive instance newtypeListOfSegmentResponse :: Newtype ListOfSegmentResponse _
derive instance repGenericListOfSegmentResponse :: Generic ListOfSegmentResponse _
instance showListOfSegmentResponse :: Show ListOfSegmentResponse where
  show = genericShow
instance decodeListOfSegmentResponse :: Decode ListOfSegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfSegmentResponse :: Encode ListOfSegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfTreatmentResource = ListOfTreatmentResource (Array TreatmentResource)
derive instance newtypeListOfTreatmentResource :: Newtype ListOfTreatmentResource _
derive instance repGenericListOfTreatmentResource :: Generic ListOfTreatmentResource _
instance showListOfTreatmentResource :: Show ListOfTreatmentResource where
  show = genericShow
instance decodeListOfTreatmentResource :: Decode ListOfTreatmentResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfTreatmentResource :: Encode ListOfTreatmentResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfWriteTreatmentResource = ListOfWriteTreatmentResource (Array WriteTreatmentResource)
derive instance newtypeListOfWriteTreatmentResource :: Newtype ListOfWriteTreatmentResource _
derive instance repGenericListOfWriteTreatmentResource :: Generic ListOfWriteTreatmentResource _
instance showListOfWriteTreatmentResource :: Show ListOfWriteTreatmentResource where
  show = genericShow
instance decodeListOfWriteTreatmentResource :: Decode ListOfWriteTreatmentResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfWriteTreatmentResource :: Encode ListOfWriteTreatmentResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _
derive instance repGenericListOf__string :: Generic ListOf__string _
instance showListOf__string :: Show ListOf__string where
  show = genericShow
instance decodeListOf__string :: Decode ListOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOf__string :: Encode ListOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfAddressConfiguration = MapOfAddressConfiguration (StrMap.StrMap AddressConfiguration)
derive instance newtypeMapOfAddressConfiguration :: Newtype MapOfAddressConfiguration _
derive instance repGenericMapOfAddressConfiguration :: Generic MapOfAddressConfiguration _
instance showMapOfAddressConfiguration :: Show MapOfAddressConfiguration where
  show = genericShow
instance decodeMapOfAddressConfiguration :: Decode MapOfAddressConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfAddressConfiguration :: Encode MapOfAddressConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfAttributeDimension = MapOfAttributeDimension (StrMap.StrMap AttributeDimension)
derive instance newtypeMapOfAttributeDimension :: Newtype MapOfAttributeDimension _
derive instance repGenericMapOfAttributeDimension :: Generic MapOfAttributeDimension _
instance showMapOfAttributeDimension :: Show MapOfAttributeDimension where
  show = genericShow
instance decodeMapOfAttributeDimension :: Decode MapOfAttributeDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfAttributeDimension :: Encode MapOfAttributeDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfEndpointMessageResult = MapOfEndpointMessageResult (StrMap.StrMap EndpointMessageResult)
derive instance newtypeMapOfEndpointMessageResult :: Newtype MapOfEndpointMessageResult _
derive instance repGenericMapOfEndpointMessageResult :: Generic MapOfEndpointMessageResult _
instance showMapOfEndpointMessageResult :: Show MapOfEndpointMessageResult where
  show = genericShow
instance decodeMapOfEndpointMessageResult :: Decode MapOfEndpointMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfEndpointMessageResult :: Encode MapOfEndpointMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfEndpointSendConfiguration = MapOfEndpointSendConfiguration (StrMap.StrMap EndpointSendConfiguration)
derive instance newtypeMapOfEndpointSendConfiguration :: Newtype MapOfEndpointSendConfiguration _
derive instance repGenericMapOfEndpointSendConfiguration :: Generic MapOfEndpointSendConfiguration _
instance showMapOfEndpointSendConfiguration :: Show MapOfEndpointSendConfiguration where
  show = genericShow
instance decodeMapOfEndpointSendConfiguration :: Decode MapOfEndpointSendConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfEndpointSendConfiguration :: Encode MapOfEndpointSendConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfListOf__string = MapOfListOf__string (StrMap.StrMap ListOf__string)
derive instance newtypeMapOfListOf__string :: Newtype MapOfListOf__string _
derive instance repGenericMapOfListOf__string :: Generic MapOfListOf__string _
instance showMapOfListOf__string :: Show MapOfListOf__string where
  show = genericShow
instance decodeMapOfListOf__string :: Decode MapOfListOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfListOf__string :: Encode MapOfListOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMapOfEndpointMessageResult = MapOfMapOfEndpointMessageResult (StrMap.StrMap MapOfEndpointMessageResult)
derive instance newtypeMapOfMapOfEndpointMessageResult :: Newtype MapOfMapOfEndpointMessageResult _
derive instance repGenericMapOfMapOfEndpointMessageResult :: Generic MapOfMapOfEndpointMessageResult _
instance showMapOfMapOfEndpointMessageResult :: Show MapOfMapOfEndpointMessageResult where
  show = genericShow
instance decodeMapOfMapOfEndpointMessageResult :: Decode MapOfMapOfEndpointMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMapOfEndpointMessageResult :: Encode MapOfMapOfEndpointMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfMessageResult = MapOfMessageResult (StrMap.StrMap MessageResult)
derive instance newtypeMapOfMessageResult :: Newtype MapOfMessageResult _
derive instance repGenericMapOfMessageResult :: Generic MapOfMessageResult _
instance showMapOfMessageResult :: Show MapOfMessageResult where
  show = genericShow
instance decodeMapOfMessageResult :: Decode MapOfMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfMessageResult :: Encode MapOfMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOf__double = MapOf__double (StrMap.StrMap Number)
derive instance newtypeMapOf__double :: Newtype MapOf__double _
derive instance repGenericMapOf__double :: Generic MapOf__double _
instance showMapOf__double :: Show MapOf__double where
  show = genericShow
instance decodeMapOf__double :: Decode MapOf__double where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOf__double :: Encode MapOf__double where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOf__integer = MapOf__integer (StrMap.StrMap Int)
derive instance newtypeMapOf__integer :: Newtype MapOf__integer _
derive instance repGenericMapOf__integer :: Generic MapOf__integer _
instance showMapOf__integer :: Show MapOf__integer where
  show = genericShow
instance decodeMapOf__integer :: Decode MapOf__integer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOf__integer :: Encode MapOf__integer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOf__string = MapOf__string (StrMap.StrMap String)
derive instance newtypeMapOf__string :: Newtype MapOf__string _
derive instance repGenericMapOf__string :: Generic MapOf__string _
instance showMapOf__string :: Show MapOf__string where
  show = genericShow
instance decodeMapOf__string :: Decode MapOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOf__string :: Encode MapOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageSmallIconUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "JsonBody" :: NullOrUndefined.NullOrUndefined (String)
  , "MediaUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined.NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype MessageBody = MessageBody 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMessageBody :: Newtype MessageBody _
derive instance repGenericMessageBody :: Generic MessageBody _
instance showMessageBody :: Show MessageBody where
  show = genericShow
instance decodeMessageBody :: Decode MessageBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageBody :: Encode MessageBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Message configuration for a campaign.
newtype MessageConfiguration = MessageConfiguration 
  { "ADMMessage" :: NullOrUndefined.NullOrUndefined (Message)
  , "APNSMessage" :: NullOrUndefined.NullOrUndefined (Message)
  , "BaiduMessage" :: NullOrUndefined.NullOrUndefined (Message)
  , "DefaultMessage" :: NullOrUndefined.NullOrUndefined (Message)
  , "EmailMessage" :: NullOrUndefined.NullOrUndefined (CampaignEmailMessage)
  , "GCMMessage" :: NullOrUndefined.NullOrUndefined (Message)
  , "SMSMessage" :: NullOrUndefined.NullOrUndefined (CampaignSmsMessage)
  }
derive instance newtypeMessageConfiguration :: Newtype MessageConfiguration _
derive instance repGenericMessageConfiguration :: Generic MessageConfiguration _
instance showMessageConfiguration :: Show MessageConfiguration where
  show = genericShow
instance decodeMessageConfiguration :: Decode MessageConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageConfiguration :: Encode MessageConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Send message request.
newtype MessageRequest = MessageRequest 
  { "Addresses" :: NullOrUndefined.NullOrUndefined (MapOfAddressConfiguration)
  , "Context" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "Endpoints" :: NullOrUndefined.NullOrUndefined (MapOfEndpointSendConfiguration)
  , "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (DirectMessageConfiguration)
  }
derive instance newtypeMessageRequest :: Newtype MessageRequest _
derive instance repGenericMessageRequest :: Generic MessageRequest _
instance showMessageRequest :: Show MessageRequest where
  show = genericShow
instance decodeMessageRequest :: Decode MessageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageRequest :: Encode MessageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Send message response.
newtype MessageResponse = MessageResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "EndpointResult" :: NullOrUndefined.NullOrUndefined (MapOfEndpointMessageResult)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Result" :: NullOrUndefined.NullOrUndefined (MapOfMessageResult)
  }
derive instance newtypeMessageResponse :: Newtype MessageResponse _
derive instance repGenericMessageResponse :: Generic MessageResponse _
instance showMessageResponse :: Show MessageResponse where
  show = genericShow
instance decodeMessageResponse :: Decode MessageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageResponse :: Encode MessageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The result from sending a message to an address.
newtype MessageResult = MessageResult 
  { "DeliveryStatus" :: NullOrUndefined.NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined.NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMessageResult :: Newtype MessageResult _
derive instance repGenericMessageResult :: Generic MessageResult _
instance showMessageResult :: Show MessageResult where
  show = genericShow
instance decodeMessageResult :: Decode MessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageResult :: Encode MessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _
derive instance repGenericMessageType :: Generic MessageType _
instance showMessageType :: Show MessageType where
  show = genericShow
instance decodeMessageType :: Decode MessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageType :: Encode MessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMethodNotAllowedException :: Newtype MethodNotAllowedException _
derive instance repGenericMethodNotAllowedException :: Generic MethodNotAllowedException _
instance showMethodNotAllowedException :: Show MethodNotAllowedException where
  show = genericShow
instance decodeMethodNotAllowedException :: Decode MethodNotAllowedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMethodNotAllowedException :: Encode MethodNotAllowedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventStreamRequest = PutEventStreamRequest 
  { "ApplicationId" :: (String)
  , "WriteEventStream" :: (WriteEventStream)
  }
derive instance newtypePutEventStreamRequest :: Newtype PutEventStreamRequest _
derive instance repGenericPutEventStreamRequest :: Generic PutEventStreamRequest _
instance showPutEventStreamRequest :: Show PutEventStreamRequest where
  show = genericShow
instance decodePutEventStreamRequest :: Decode PutEventStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventStreamRequest :: Encode PutEventStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventStreamResponse = PutEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypePutEventStreamResponse :: Newtype PutEventStreamResponse _
derive instance repGenericPutEventStreamResponse :: Generic PutEventStreamResponse _
instance showPutEventStreamResponse :: Show PutEventStreamResponse where
  show = genericShow
instance decodePutEventStreamResponse :: Decode PutEventStreamResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventStreamResponse :: Encode PutEventStreamResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Quiet Time
newtype QuietTime = QuietTime 
  { "End" :: NullOrUndefined.NullOrUndefined (String)
  , "Start" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeQuietTime :: Newtype QuietTime _
derive instance repGenericQuietTime :: Generic QuietTime _
instance showQuietTime :: Show QuietTime where
  show = genericShow
instance decodeQuietTime :: Decode QuietTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQuietTime :: Encode QuietTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Define how a segment based on recency of use.
newtype RecencyDimension = RecencyDimension 
  { "Duration" :: NullOrUndefined.NullOrUndefined (Duration)
  , "RecencyType" :: NullOrUndefined.NullOrUndefined (RecencyType)
  }
derive instance newtypeRecencyDimension :: Newtype RecencyDimension _
derive instance repGenericRecencyDimension :: Generic RecencyDimension _
instance showRecencyDimension :: Show RecencyDimension where
  show = genericShow
instance decodeRecencyDimension :: Decode RecencyDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecencyDimension :: Encode RecencyDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecencyType = RecencyType String
derive instance newtypeRecencyType :: Newtype RecencyType _
derive instance repGenericRecencyType :: Generic RecencyType _
instance showRecencyType :: Show RecencyType where
  show = genericShow
instance decodeRecencyType :: Decode RecencyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecencyType :: Encode RecencyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | SMS Channel Request
newtype SMSChannelRequest = SMSChannelRequest 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SenderId" :: NullOrUndefined.NullOrUndefined (String)
  , "ShortCode" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSMSChannelRequest :: Newtype SMSChannelRequest _
derive instance repGenericSMSChannelRequest :: Generic SMSChannelRequest _
instance showSMSChannelRequest :: Show SMSChannelRequest where
  show = genericShow
instance decodeSMSChannelRequest :: Decode SMSChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSMSChannelRequest :: Encode SMSChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | SMS Channel Response.
newtype SMSChannelResponse = SMSChannelResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "SenderId" :: NullOrUndefined.NullOrUndefined (String)
  , "ShortCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeSMSChannelResponse :: Newtype SMSChannelResponse _
derive instance repGenericSMSChannelResponse :: Generic SMSChannelResponse _
instance showSMSChannelResponse :: Show SMSChannelResponse where
  show = genericShow
instance decodeSMSChannelResponse :: Decode SMSChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSMSChannelResponse :: Encode SMSChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | SMS Message.
newtype SMSMessage = SMSMessage 
  { "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined.NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined.NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined.NullOrUndefined (MapOfListOf__string)
  }
derive instance newtypeSMSMessage :: Newtype SMSMessage _
derive instance repGenericSMSMessage :: Generic SMSMessage _
instance showSMSMessage :: Show SMSMessage where
  show = genericShow
instance decodeSMSMessage :: Decode SMSMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSMSMessage :: Encode SMSMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Shcedule that defines when a campaign is run.
newtype Schedule = Schedule 
  { "EndTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Frequency" :: NullOrUndefined.NullOrUndefined (Frequency)
  , "IsLocalTime" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "QuietTime" :: NullOrUndefined.NullOrUndefined (QuietTime)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Timezone" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSchedule :: Newtype Schedule _
derive instance repGenericSchedule :: Generic Schedule _
instance showSchedule :: Show Schedule where
  show = genericShow
instance decodeSchedule :: Decode Schedule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchedule :: Encode Schedule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment behavior dimensions
newtype SegmentBehaviors = SegmentBehaviors 
  { "Recency" :: NullOrUndefined.NullOrUndefined (RecencyDimension)
  }
derive instance newtypeSegmentBehaviors :: Newtype SegmentBehaviors _
derive instance repGenericSegmentBehaviors :: Generic SegmentBehaviors _
instance showSegmentBehaviors :: Show SegmentBehaviors where
  show = genericShow
instance decodeSegmentBehaviors :: Decode SegmentBehaviors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentBehaviors :: Encode SegmentBehaviors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment demographic dimensions
newtype SegmentDemographics = SegmentDemographics 
  { "AppVersion" :: NullOrUndefined.NullOrUndefined (SetDimension)
  , "Channel" :: NullOrUndefined.NullOrUndefined (SetDimension)
  , "DeviceType" :: NullOrUndefined.NullOrUndefined (SetDimension)
  , "Make" :: NullOrUndefined.NullOrUndefined (SetDimension)
  , "Model" :: NullOrUndefined.NullOrUndefined (SetDimension)
  , "Platform" :: NullOrUndefined.NullOrUndefined (SetDimension)
  }
derive instance newtypeSegmentDemographics :: Newtype SegmentDemographics _
derive instance repGenericSegmentDemographics :: Generic SegmentDemographics _
instance showSegmentDemographics :: Show SegmentDemographics where
  show = genericShow
instance decodeSegmentDemographics :: Decode SegmentDemographics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentDemographics :: Encode SegmentDemographics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment dimensions
newtype SegmentDimensions = SegmentDimensions 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (MapOfAttributeDimension)
  , "Behavior" :: NullOrUndefined.NullOrUndefined (SegmentBehaviors)
  , "Demographic" :: NullOrUndefined.NullOrUndefined (SegmentDemographics)
  , "Location" :: NullOrUndefined.NullOrUndefined (SegmentLocation)
  , "UserAttributes" :: NullOrUndefined.NullOrUndefined (MapOfAttributeDimension)
  }
derive instance newtypeSegmentDimensions :: Newtype SegmentDimensions _
derive instance repGenericSegmentDimensions :: Generic SegmentDimensions _
instance showSegmentDimensions :: Show SegmentDimensions where
  show = genericShow
instance decodeSegmentDimensions :: Decode SegmentDimensions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentDimensions :: Encode SegmentDimensions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment import definition.
newtype SegmentImportResource = SegmentImportResource 
  { "ChannelCounts" :: NullOrUndefined.NullOrUndefined (MapOf__integer)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (String)
  , "Format" :: NullOrUndefined.NullOrUndefined (Format)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Size" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeSegmentImportResource :: Newtype SegmentImportResource _
derive instance repGenericSegmentImportResource :: Generic SegmentImportResource _
instance showSegmentImportResource :: Show SegmentImportResource where
  show = genericShow
instance decodeSegmentImportResource :: Decode SegmentImportResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentImportResource :: Encode SegmentImportResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment location dimensions
newtype SegmentLocation = SegmentLocation 
  { "Country" :: NullOrUndefined.NullOrUndefined (SetDimension)
  }
derive instance newtypeSegmentLocation :: Newtype SegmentLocation _
derive instance repGenericSegmentLocation :: Generic SegmentLocation _
instance showSegmentLocation :: Show SegmentLocation where
  show = genericShow
instance decodeSegmentLocation :: Decode SegmentLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentLocation :: Encode SegmentLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment definition.
newtype SegmentResponse = SegmentResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (SegmentDimensions)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ImportDefinition" :: NullOrUndefined.NullOrUndefined (SegmentImportResource)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentType" :: NullOrUndefined.NullOrUndefined (SegmentType)
  , "Version" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeSegmentResponse :: Newtype SegmentResponse _
derive instance repGenericSegmentResponse :: Generic SegmentResponse _
instance showSegmentResponse :: Show SegmentResponse where
  show = genericShow
instance decodeSegmentResponse :: Decode SegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentResponse :: Encode SegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SegmentType = SegmentType String
derive instance newtypeSegmentType :: Newtype SegmentType _
derive instance repGenericSegmentType :: Generic SegmentType _
instance showSegmentType :: Show SegmentType where
  show = genericShow
instance decodeSegmentType :: Decode SegmentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentType :: Encode SegmentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segments in your account.
newtype SegmentsResponse = SegmentsResponse 
  { "Item" :: NullOrUndefined.NullOrUndefined (ListOfSegmentResponse)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSegmentsResponse :: Newtype SegmentsResponse _
derive instance repGenericSegmentsResponse :: Generic SegmentsResponse _
instance showSegmentsResponse :: Show SegmentsResponse where
  show = genericShow
instance decodeSegmentsResponse :: Decode SegmentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentsResponse :: Encode SegmentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendMessagesRequest = SendMessagesRequest 
  { "ApplicationId" :: (String)
  , "MessageRequest" :: (MessageRequest)
  }
derive instance newtypeSendMessagesRequest :: Newtype SendMessagesRequest _
derive instance repGenericSendMessagesRequest :: Generic SendMessagesRequest _
instance showSendMessagesRequest :: Show SendMessagesRequest where
  show = genericShow
instance decodeSendMessagesRequest :: Decode SendMessagesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessagesRequest :: Encode SendMessagesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendMessagesResponse = SendMessagesResponse 
  { "MessageResponse" :: (MessageResponse)
  }
derive instance newtypeSendMessagesResponse :: Newtype SendMessagesResponse _
derive instance repGenericSendMessagesResponse :: Generic SendMessagesResponse _
instance showSendMessagesResponse :: Show SendMessagesResponse where
  show = genericShow
instance decodeSendMessagesResponse :: Decode SendMessagesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessagesResponse :: Encode SendMessagesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Send message request.
newtype SendUsersMessageRequest = SendUsersMessageRequest 
  { "Context" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  , "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (DirectMessageConfiguration)
  , "Users" :: NullOrUndefined.NullOrUndefined (MapOfEndpointSendConfiguration)
  }
derive instance newtypeSendUsersMessageRequest :: Newtype SendUsersMessageRequest _
derive instance repGenericSendUsersMessageRequest :: Generic SendUsersMessageRequest _
instance showSendUsersMessageRequest :: Show SendUsersMessageRequest where
  show = genericShow
instance decodeSendUsersMessageRequest :: Decode SendUsersMessageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendUsersMessageRequest :: Encode SendUsersMessageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | User send message response.
newtype SendUsersMessageResponse = SendUsersMessageResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Result" :: NullOrUndefined.NullOrUndefined (MapOfMapOfEndpointMessageResult)
  }
derive instance newtypeSendUsersMessageResponse :: Newtype SendUsersMessageResponse _
derive instance repGenericSendUsersMessageResponse :: Generic SendUsersMessageResponse _
instance showSendUsersMessageResponse :: Show SendUsersMessageResponse where
  show = genericShow
instance decodeSendUsersMessageResponse :: Decode SendUsersMessageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendUsersMessageResponse :: Encode SendUsersMessageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendUsersMessagesRequest = SendUsersMessagesRequest 
  { "ApplicationId" :: (String)
  , "SendUsersMessageRequest" :: (SendUsersMessageRequest)
  }
derive instance newtypeSendUsersMessagesRequest :: Newtype SendUsersMessagesRequest _
derive instance repGenericSendUsersMessagesRequest :: Generic SendUsersMessagesRequest _
instance showSendUsersMessagesRequest :: Show SendUsersMessagesRequest where
  show = genericShow
instance decodeSendUsersMessagesRequest :: Decode SendUsersMessagesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendUsersMessagesRequest :: Encode SendUsersMessagesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendUsersMessagesResponse = SendUsersMessagesResponse 
  { "SendUsersMessageResponse" :: (SendUsersMessageResponse)
  }
derive instance newtypeSendUsersMessagesResponse :: Newtype SendUsersMessagesResponse _
derive instance repGenericSendUsersMessagesResponse :: Generic SendUsersMessagesResponse _
instance showSendUsersMessagesResponse :: Show SendUsersMessagesResponse where
  show = genericShow
instance decodeSendUsersMessagesResponse :: Decode SendUsersMessagesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendUsersMessagesResponse :: Encode SendUsersMessagesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Dimension specification of a segment.
newtype SetDimension = SetDimension 
  { "DimensionType" :: NullOrUndefined.NullOrUndefined (DimensionType)
  , "Values" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeSetDimension :: Newtype SetDimension _
derive instance repGenericSetDimension :: Generic SetDimension _
instance showSetDimension :: Show SetDimension where
  show = genericShow
instance decodeSetDimension :: Decode SetDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetDimension :: Encode SetDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Simple message object.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Treatment resource
newtype TreatmentResource = TreatmentResource 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (MessageConfiguration)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (Schedule)
  , "SizePercent" :: NullOrUndefined.NullOrUndefined (Int)
  , "State" :: NullOrUndefined.NullOrUndefined (CampaignState)
  , "TreatmentDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTreatmentResource :: Newtype TreatmentResource _
derive instance repGenericTreatmentResource :: Generic TreatmentResource _
instance showTreatmentResource :: Show TreatmentResource where
  show = genericShow
instance decodeTreatmentResource :: Decode TreatmentResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTreatmentResource :: Encode TreatmentResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAdmChannelRequest = UpdateAdmChannelRequest 
  { "ADMChannelRequest" :: (ADMChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateAdmChannelRequest :: Newtype UpdateAdmChannelRequest _
derive instance repGenericUpdateAdmChannelRequest :: Generic UpdateAdmChannelRequest _
instance showUpdateAdmChannelRequest :: Show UpdateAdmChannelRequest where
  show = genericShow
instance decodeUpdateAdmChannelRequest :: Decode UpdateAdmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAdmChannelRequest :: Encode UpdateAdmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAdmChannelResponse = UpdateAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeUpdateAdmChannelResponse :: Newtype UpdateAdmChannelResponse _
derive instance repGenericUpdateAdmChannelResponse :: Generic UpdateAdmChannelResponse _
instance showUpdateAdmChannelResponse :: Show UpdateAdmChannelResponse where
  show = genericShow
instance decodeUpdateAdmChannelResponse :: Decode UpdateAdmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAdmChannelResponse :: Encode UpdateAdmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsChannelRequest = UpdateApnsChannelRequest 
  { "APNSChannelRequest" :: (APNSChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsChannelRequest :: Newtype UpdateApnsChannelRequest _
derive instance repGenericUpdateApnsChannelRequest :: Generic UpdateApnsChannelRequest _
instance showUpdateApnsChannelRequest :: Show UpdateApnsChannelRequest where
  show = genericShow
instance decodeUpdateApnsChannelRequest :: Decode UpdateApnsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsChannelRequest :: Encode UpdateApnsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsChannelResponse = UpdateApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeUpdateApnsChannelResponse :: Newtype UpdateApnsChannelResponse _
derive instance repGenericUpdateApnsChannelResponse :: Generic UpdateApnsChannelResponse _
instance showUpdateApnsChannelResponse :: Show UpdateApnsChannelResponse where
  show = genericShow
instance decodeUpdateApnsChannelResponse :: Decode UpdateApnsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsChannelResponse :: Encode UpdateApnsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsSandboxChannelRequest = UpdateApnsSandboxChannelRequest 
  { "APNSSandboxChannelRequest" :: (APNSSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsSandboxChannelRequest :: Newtype UpdateApnsSandboxChannelRequest _
derive instance repGenericUpdateApnsSandboxChannelRequest :: Generic UpdateApnsSandboxChannelRequest _
instance showUpdateApnsSandboxChannelRequest :: Show UpdateApnsSandboxChannelRequest where
  show = genericShow
instance decodeUpdateApnsSandboxChannelRequest :: Decode UpdateApnsSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsSandboxChannelRequest :: Encode UpdateApnsSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeUpdateApnsSandboxChannelResponse :: Newtype UpdateApnsSandboxChannelResponse _
derive instance repGenericUpdateApnsSandboxChannelResponse :: Generic UpdateApnsSandboxChannelResponse _
instance showUpdateApnsSandboxChannelResponse :: Show UpdateApnsSandboxChannelResponse where
  show = genericShow
instance decodeUpdateApnsSandboxChannelResponse :: Decode UpdateApnsSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsSandboxChannelResponse :: Encode UpdateApnsSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsVoipChannelRequest = UpdateApnsVoipChannelRequest 
  { "APNSVoipChannelRequest" :: (APNSVoipChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsVoipChannelRequest :: Newtype UpdateApnsVoipChannelRequest _
derive instance repGenericUpdateApnsVoipChannelRequest :: Generic UpdateApnsVoipChannelRequest _
instance showUpdateApnsVoipChannelRequest :: Show UpdateApnsVoipChannelRequest where
  show = genericShow
instance decodeUpdateApnsVoipChannelRequest :: Decode UpdateApnsVoipChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsVoipChannelRequest :: Encode UpdateApnsVoipChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsVoipChannelResponse = UpdateApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeUpdateApnsVoipChannelResponse :: Newtype UpdateApnsVoipChannelResponse _
derive instance repGenericUpdateApnsVoipChannelResponse :: Generic UpdateApnsVoipChannelResponse _
instance showUpdateApnsVoipChannelResponse :: Show UpdateApnsVoipChannelResponse where
  show = genericShow
instance decodeUpdateApnsVoipChannelResponse :: Decode UpdateApnsVoipChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsVoipChannelResponse :: Encode UpdateApnsVoipChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsVoipSandboxChannelRequest = UpdateApnsVoipSandboxChannelRequest 
  { "APNSVoipSandboxChannelRequest" :: (APNSVoipSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsVoipSandboxChannelRequest :: Newtype UpdateApnsVoipSandboxChannelRequest _
derive instance repGenericUpdateApnsVoipSandboxChannelRequest :: Generic UpdateApnsVoipSandboxChannelRequest _
instance showUpdateApnsVoipSandboxChannelRequest :: Show UpdateApnsVoipSandboxChannelRequest where
  show = genericShow
instance decodeUpdateApnsVoipSandboxChannelRequest :: Decode UpdateApnsVoipSandboxChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsVoipSandboxChannelRequest :: Encode UpdateApnsVoipSandboxChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeUpdateApnsVoipSandboxChannelResponse :: Newtype UpdateApnsVoipSandboxChannelResponse _
derive instance repGenericUpdateApnsVoipSandboxChannelResponse :: Generic UpdateApnsVoipSandboxChannelResponse _
instance showUpdateApnsVoipSandboxChannelResponse :: Show UpdateApnsVoipSandboxChannelResponse where
  show = genericShow
instance decodeUpdateApnsVoipSandboxChannelResponse :: Decode UpdateApnsVoipSandboxChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApnsVoipSandboxChannelResponse :: Encode UpdateApnsVoipSandboxChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationSettingsRequest = UpdateApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  , "WriteApplicationSettingsRequest" :: (WriteApplicationSettingsRequest)
  }
derive instance newtypeUpdateApplicationSettingsRequest :: Newtype UpdateApplicationSettingsRequest _
derive instance repGenericUpdateApplicationSettingsRequest :: Generic UpdateApplicationSettingsRequest _
instance showUpdateApplicationSettingsRequest :: Show UpdateApplicationSettingsRequest where
  show = genericShow
instance decodeUpdateApplicationSettingsRequest :: Decode UpdateApplicationSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationSettingsRequest :: Encode UpdateApplicationSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }
derive instance newtypeUpdateApplicationSettingsResponse :: Newtype UpdateApplicationSettingsResponse _
derive instance repGenericUpdateApplicationSettingsResponse :: Generic UpdateApplicationSettingsResponse _
instance showUpdateApplicationSettingsResponse :: Show UpdateApplicationSettingsResponse where
  show = genericShow
instance decodeUpdateApplicationSettingsResponse :: Decode UpdateApplicationSettingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationSettingsResponse :: Encode UpdateApplicationSettingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateBaiduChannelRequest = UpdateBaiduChannelRequest 
  { "ApplicationId" :: (String)
  , "BaiduChannelRequest" :: (BaiduChannelRequest)
  }
derive instance newtypeUpdateBaiduChannelRequest :: Newtype UpdateBaiduChannelRequest _
derive instance repGenericUpdateBaiduChannelRequest :: Generic UpdateBaiduChannelRequest _
instance showUpdateBaiduChannelRequest :: Show UpdateBaiduChannelRequest where
  show = genericShow
instance decodeUpdateBaiduChannelRequest :: Decode UpdateBaiduChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBaiduChannelRequest :: Encode UpdateBaiduChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateBaiduChannelResponse = UpdateBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeUpdateBaiduChannelResponse :: Newtype UpdateBaiduChannelResponse _
derive instance repGenericUpdateBaiduChannelResponse :: Generic UpdateBaiduChannelResponse _
instance showUpdateBaiduChannelResponse :: Show UpdateBaiduChannelResponse where
  show = genericShow
instance decodeUpdateBaiduChannelResponse :: Decode UpdateBaiduChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBaiduChannelResponse :: Encode UpdateBaiduChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCampaignRequest = UpdateCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }
derive instance newtypeUpdateCampaignRequest :: Newtype UpdateCampaignRequest _
derive instance repGenericUpdateCampaignRequest :: Generic UpdateCampaignRequest _
instance showUpdateCampaignRequest :: Show UpdateCampaignRequest where
  show = genericShow
instance decodeUpdateCampaignRequest :: Decode UpdateCampaignRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCampaignRequest :: Encode UpdateCampaignRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCampaignResponse = UpdateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeUpdateCampaignResponse :: Newtype UpdateCampaignResponse _
derive instance repGenericUpdateCampaignResponse :: Generic UpdateCampaignResponse _
instance showUpdateCampaignResponse :: Show UpdateCampaignResponse where
  show = genericShow
instance decodeUpdateCampaignResponse :: Decode UpdateCampaignResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCampaignResponse :: Encode UpdateCampaignResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEmailChannelRequest = UpdateEmailChannelRequest 
  { "ApplicationId" :: (String)
  , "EmailChannelRequest" :: (EmailChannelRequest)
  }
derive instance newtypeUpdateEmailChannelRequest :: Newtype UpdateEmailChannelRequest _
derive instance repGenericUpdateEmailChannelRequest :: Generic UpdateEmailChannelRequest _
instance showUpdateEmailChannelRequest :: Show UpdateEmailChannelRequest where
  show = genericShow
instance decodeUpdateEmailChannelRequest :: Decode UpdateEmailChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEmailChannelRequest :: Encode UpdateEmailChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEmailChannelResponse = UpdateEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeUpdateEmailChannelResponse :: Newtype UpdateEmailChannelResponse _
derive instance repGenericUpdateEmailChannelResponse :: Generic UpdateEmailChannelResponse _
instance showUpdateEmailChannelResponse :: Show UpdateEmailChannelResponse where
  show = genericShow
instance decodeUpdateEmailChannelResponse :: Decode UpdateEmailChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEmailChannelResponse :: Encode UpdateEmailChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEndpointRequest = UpdateEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  , "EndpointRequest" :: (EndpointRequest)
  }
derive instance newtypeUpdateEndpointRequest :: Newtype UpdateEndpointRequest _
derive instance repGenericUpdateEndpointRequest :: Generic UpdateEndpointRequest _
instance showUpdateEndpointRequest :: Show UpdateEndpointRequest where
  show = genericShow
instance decodeUpdateEndpointRequest :: Decode UpdateEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEndpointRequest :: Encode UpdateEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEndpointResponse = UpdateEndpointResponse 
  { "MessageBody" :: (MessageBody)
  }
derive instance newtypeUpdateEndpointResponse :: Newtype UpdateEndpointResponse _
derive instance repGenericUpdateEndpointResponse :: Generic UpdateEndpointResponse _
instance showUpdateEndpointResponse :: Show UpdateEndpointResponse where
  show = genericShow
instance decodeUpdateEndpointResponse :: Decode UpdateEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEndpointResponse :: Encode UpdateEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEndpointsBatchRequest = UpdateEndpointsBatchRequest 
  { "ApplicationId" :: (String)
  , "EndpointBatchRequest" :: (EndpointBatchRequest)
  }
derive instance newtypeUpdateEndpointsBatchRequest :: Newtype UpdateEndpointsBatchRequest _
derive instance repGenericUpdateEndpointsBatchRequest :: Generic UpdateEndpointsBatchRequest _
instance showUpdateEndpointsBatchRequest :: Show UpdateEndpointsBatchRequest where
  show = genericShow
instance decodeUpdateEndpointsBatchRequest :: Decode UpdateEndpointsBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEndpointsBatchRequest :: Encode UpdateEndpointsBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse 
  { "MessageBody" :: (MessageBody)
  }
derive instance newtypeUpdateEndpointsBatchResponse :: Newtype UpdateEndpointsBatchResponse _
derive instance repGenericUpdateEndpointsBatchResponse :: Generic UpdateEndpointsBatchResponse _
instance showUpdateEndpointsBatchResponse :: Show UpdateEndpointsBatchResponse where
  show = genericShow
instance decodeUpdateEndpointsBatchResponse :: Decode UpdateEndpointsBatchResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEndpointsBatchResponse :: Encode UpdateEndpointsBatchResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGcmChannelRequest = UpdateGcmChannelRequest 
  { "ApplicationId" :: (String)
  , "GCMChannelRequest" :: (GCMChannelRequest)
  }
derive instance newtypeUpdateGcmChannelRequest :: Newtype UpdateGcmChannelRequest _
derive instance repGenericUpdateGcmChannelRequest :: Generic UpdateGcmChannelRequest _
instance showUpdateGcmChannelRequest :: Show UpdateGcmChannelRequest where
  show = genericShow
instance decodeUpdateGcmChannelRequest :: Decode UpdateGcmChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGcmChannelRequest :: Encode UpdateGcmChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGcmChannelResponse = UpdateGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeUpdateGcmChannelResponse :: Newtype UpdateGcmChannelResponse _
derive instance repGenericUpdateGcmChannelResponse :: Generic UpdateGcmChannelResponse _
instance showUpdateGcmChannelResponse :: Show UpdateGcmChannelResponse where
  show = genericShow
instance decodeUpdateGcmChannelResponse :: Decode UpdateGcmChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGcmChannelResponse :: Encode UpdateGcmChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSegmentRequest = UpdateSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }
derive instance newtypeUpdateSegmentRequest :: Newtype UpdateSegmentRequest _
derive instance repGenericUpdateSegmentRequest :: Generic UpdateSegmentRequest _
instance showUpdateSegmentRequest :: Show UpdateSegmentRequest where
  show = genericShow
instance decodeUpdateSegmentRequest :: Decode UpdateSegmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSegmentRequest :: Encode UpdateSegmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSegmentResponse = UpdateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeUpdateSegmentResponse :: Newtype UpdateSegmentResponse _
derive instance repGenericUpdateSegmentResponse :: Generic UpdateSegmentResponse _
instance showUpdateSegmentResponse :: Show UpdateSegmentResponse where
  show = genericShow
instance decodeUpdateSegmentResponse :: Decode UpdateSegmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSegmentResponse :: Encode UpdateSegmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSmsChannelRequest = UpdateSmsChannelRequest 
  { "ApplicationId" :: (String)
  , "SMSChannelRequest" :: (SMSChannelRequest)
  }
derive instance newtypeUpdateSmsChannelRequest :: Newtype UpdateSmsChannelRequest _
derive instance repGenericUpdateSmsChannelRequest :: Generic UpdateSmsChannelRequest _
instance showUpdateSmsChannelRequest :: Show UpdateSmsChannelRequest where
  show = genericShow
instance decodeUpdateSmsChannelRequest :: Decode UpdateSmsChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSmsChannelRequest :: Encode UpdateSmsChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSmsChannelResponse = UpdateSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeUpdateSmsChannelResponse :: Newtype UpdateSmsChannelResponse _
derive instance repGenericUpdateSmsChannelResponse :: Generic UpdateSmsChannelResponse _
instance showUpdateSmsChannelResponse :: Show UpdateSmsChannelResponse where
  show = genericShow
instance decodeUpdateSmsChannelResponse :: Decode UpdateSmsChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSmsChannelResponse :: Encode UpdateSmsChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Creating application setting request
newtype WriteApplicationSettingsRequest = WriteApplicationSettingsRequest 
  { "Limits" :: NullOrUndefined.NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined.NullOrUndefined (QuietTime)
  }
derive instance newtypeWriteApplicationSettingsRequest :: Newtype WriteApplicationSettingsRequest _
derive instance repGenericWriteApplicationSettingsRequest :: Generic WriteApplicationSettingsRequest _
instance showWriteApplicationSettingsRequest :: Show WriteApplicationSettingsRequest where
  show = genericShow
instance decodeWriteApplicationSettingsRequest :: Decode WriteApplicationSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWriteApplicationSettingsRequest :: Encode WriteApplicationSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Used to create a campaign.
newtype WriteCampaignRequest = WriteCampaignRequest 
  { "AdditionalTreatments" :: NullOrUndefined.NullOrUndefined (ListOfWriteTreatmentResource)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HoldoutPercent" :: NullOrUndefined.NullOrUndefined (Int)
  , "IsPaused" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Limits" :: NullOrUndefined.NullOrUndefined (CampaignLimits)
  , "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (MessageConfiguration)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (Schedule)
  , "SegmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentVersion" :: NullOrUndefined.NullOrUndefined (Int)
  , "TreatmentDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeWriteCampaignRequest :: Newtype WriteCampaignRequest _
derive instance repGenericWriteCampaignRequest :: Generic WriteCampaignRequest _
instance showWriteCampaignRequest :: Show WriteCampaignRequest where
  show = genericShow
instance decodeWriteCampaignRequest :: Decode WriteCampaignRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWriteCampaignRequest :: Encode WriteCampaignRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request to save an EventStream.
newtype WriteEventStream = WriteEventStream 
  { "DestinationStreamArn" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeWriteEventStream :: Newtype WriteEventStream _
derive instance repGenericWriteEventStream :: Generic WriteEventStream _
instance showWriteEventStream :: Show WriteEventStream where
  show = genericShow
instance decodeWriteEventStream :: Decode WriteEventStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWriteEventStream :: Encode WriteEventStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Segment definition.
newtype WriteSegmentRequest = WriteSegmentRequest 
  { "Dimensions" :: NullOrUndefined.NullOrUndefined (SegmentDimensions)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeWriteSegmentRequest :: Newtype WriteSegmentRequest _
derive instance repGenericWriteSegmentRequest :: Generic WriteSegmentRequest _
instance showWriteSegmentRequest :: Show WriteSegmentRequest where
  show = genericShow
instance decodeWriteSegmentRequest :: Decode WriteSegmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWriteSegmentRequest :: Encode WriteSegmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Used to create a campaign treatment.
newtype WriteTreatmentResource = WriteTreatmentResource 
  { "MessageConfiguration" :: NullOrUndefined.NullOrUndefined (MessageConfiguration)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (Schedule)
  , "SizePercent" :: NullOrUndefined.NullOrUndefined (Int)
  , "TreatmentDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeWriteTreatmentResource :: Newtype WriteTreatmentResource _
derive instance repGenericWriteTreatmentResource :: Generic WriteTreatmentResource _
instance showWriteTreatmentResource :: Show WriteTreatmentResource where
  show = genericShow
instance decodeWriteTreatmentResource :: Decode WriteTreatmentResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWriteTreatmentResource :: Encode WriteTreatmentResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
