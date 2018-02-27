

-- | <p>Alexa for Business makes it easy for you to use Alexa in your organization. Alexa for Business gives you the tools you need to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit, and the Alexa for Business APIs, and you can make these available as private skills for your organization. Alexa for Business also makes it easy to voice-enable your products and services, providing context-aware voice experiences for your customers.</p>
module AWS.AlexaForBusiness where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AlexaForBusiness" :: String


-- | <p>Associates a device to a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or a manual sync is required. </p>
associateDeviceWithRoom :: forall eff. AssociateDeviceWithRoomRequest -> Aff (err :: AWS.RequestError | eff) AssociateDeviceWithRoomResponse
associateDeviceWithRoom = AWS.request serviceName "associateDeviceWithRoom" 


-- | <p>Associates a skill group to a given room. This enables all skills in the associated skill group on all devices in the room.</p>
associateSkillGroupWithRoom :: forall eff. AssociateSkillGroupWithRoomRequest -> Aff (err :: AWS.RequestError | eff) AssociateSkillGroupWithRoomResponse
associateSkillGroupWithRoom = AWS.request serviceName "associateSkillGroupWithRoom" 


-- | <p>Creates a new room profile with the specified details.</p>
createProfile :: forall eff. CreateProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateProfileResponse
createProfile = AWS.request serviceName "createProfile" 


-- | <p>Creates a room with the specified details.</p>
createRoom :: forall eff. CreateRoomRequest -> Aff (err :: AWS.RequestError | eff) CreateRoomResponse
createRoom = AWS.request serviceName "createRoom" 


-- | <p>Creates a skill group with a specified name and description.</p>
createSkillGroup :: forall eff. CreateSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateSkillGroupResponse
createSkillGroup = AWS.request serviceName "createSkillGroup" 


-- | <p>Creates a user.</p>
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "createUser" 


-- | <p>Deletes a room profile by the profile ARN.</p>
deleteProfile :: forall eff. DeleteProfileRequest -> Aff (err :: AWS.RequestError | eff) DeleteProfileResponse
deleteProfile = AWS.request serviceName "deleteProfile" 


-- | <p>Deletes a room by the room ARN.</p>
deleteRoom :: forall eff. DeleteRoomRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoomResponse
deleteRoom = AWS.request serviceName "deleteRoom" 


-- | <p>Deletes room skill parameter details by room, skill, and parameter key ID.</p>
deleteRoomSkillParameter :: forall eff. DeleteRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoomSkillParameterResponse
deleteRoomSkillParameter = AWS.request serviceName "deleteRoomSkillParameter" 


-- | <p>Deletes a skill group by skill group ARN.</p>
deleteSkillGroup :: forall eff. DeleteSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteSkillGroupResponse
deleteSkillGroup = AWS.request serviceName "deleteSkillGroup" 


-- | <p>Deletes a specified user by user ARN and enrollment ARN.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserResponse
deleteUser = AWS.request serviceName "deleteUser" 


-- | <p>Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.</p>
disassociateDeviceFromRoom :: forall eff. DisassociateDeviceFromRoomRequest -> Aff (err :: AWS.RequestError | eff) DisassociateDeviceFromRoomResponse
disassociateDeviceFromRoom = AWS.request serviceName "disassociateDeviceFromRoom" 


-- | <p>Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.</p>
disassociateSkillGroupFromRoom :: forall eff. DisassociateSkillGroupFromRoomRequest -> Aff (err :: AWS.RequestError | eff) DisassociateSkillGroupFromRoomResponse
disassociateSkillGroupFromRoom = AWS.request serviceName "disassociateSkillGroupFromRoom" 


-- | <p>Gets the details of a device by device ARN.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceResponse
getDevice = AWS.request serviceName "getDevice" 


-- | <p>Gets the details of a room profile by profile ARN.</p>
getProfile :: forall eff. GetProfileRequest -> Aff (err :: AWS.RequestError | eff) GetProfileResponse
getProfile = AWS.request serviceName "getProfile" 


-- | <p>Gets room details by room ARN.</p>
getRoom :: forall eff. GetRoomRequest -> Aff (err :: AWS.RequestError | eff) GetRoomResponse
getRoom = AWS.request serviceName "getRoom" 


-- | <p>Gets room skill parameter details by room, skill, and parameter key ARN.</p>
getRoomSkillParameter :: forall eff. GetRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) GetRoomSkillParameterResponse
getRoomSkillParameter = AWS.request serviceName "getRoomSkillParameter" 


-- | <p>Gets skill group details by skill group ARN.</p>
getSkillGroup :: forall eff. GetSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) GetSkillGroupResponse
getSkillGroup = AWS.request serviceName "getSkillGroup" 


-- | <p>Lists all enabled skills in a specific skill group.</p>
listSkills :: forall eff. ListSkillsRequest -> Aff (err :: AWS.RequestError | eff) ListSkillsResponse
listSkills = AWS.request serviceName "listSkills" 


-- | <p>Lists all tags for a specific resource.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResponse
listTags = AWS.request serviceName "listTags" 


-- | <p>Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.</p>
putRoomSkillParameter :: forall eff. PutRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) PutRoomSkillParameterResponse
putRoomSkillParameter = AWS.request serviceName "putRoomSkillParameter" 


-- | <p>Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.</p>
resolveRoom :: forall eff. ResolveRoomRequest -> Aff (err :: AWS.RequestError | eff) ResolveRoomResponse
resolveRoom = AWS.request serviceName "resolveRoom" 


-- | <p>Revokes an invitation and invalidates the enrollment URL.</p>
revokeInvitation :: forall eff. RevokeInvitationRequest -> Aff (err :: AWS.RequestError | eff) RevokeInvitationResponse
revokeInvitation = AWS.request serviceName "revokeInvitation" 


-- | <p>Searches devices and lists the ones that meet a set of filter criteria.</p>
searchDevices :: forall eff. SearchDevicesRequest -> Aff (err :: AWS.RequestError | eff) SearchDevicesResponse
searchDevices = AWS.request serviceName "searchDevices" 


-- | <p>Searches room profiles and lists the ones that meet a set of filter criteria.</p>
searchProfiles :: forall eff. SearchProfilesRequest -> Aff (err :: AWS.RequestError | eff) SearchProfilesResponse
searchProfiles = AWS.request serviceName "searchProfiles" 


-- | <p>Searches rooms and lists the ones that meet a set of filter and sort criteria.</p>
searchRooms :: forall eff. SearchRoomsRequest -> Aff (err :: AWS.RequestError | eff) SearchRoomsResponse
searchRooms = AWS.request serviceName "searchRooms" 


-- | <p>Searches skill groups and lists the ones that meet a set of filter and sort criteria.</p>
searchSkillGroups :: forall eff. SearchSkillGroupsRequest -> Aff (err :: AWS.RequestError | eff) SearchSkillGroupsResponse
searchSkillGroups = AWS.request serviceName "searchSkillGroups" 


-- | <p>Searches users and lists the ones that meet a set of filter and sort criteria.</p>
searchUsers :: forall eff. SearchUsersRequest -> Aff (err :: AWS.RequestError | eff) SearchUsersResponse
searchUsers = AWS.request serviceName "searchUsers" 


-- | <p>Sends an enrollment invitation email with a URL to a user. The URL is valid for 72 hours or until you call this operation again, whichever comes first. </p>
sendInvitation :: forall eff. SendInvitationRequest -> Aff (err :: AWS.RequestError | eff) SendInvitationResponse
sendInvitation = AWS.request serviceName "sendInvitation" 


-- | <p>Resets a device and its account to the known default settings by clearing all information and settings set by previous users.</p>
startDeviceSync :: forall eff. StartDeviceSyncRequest -> Aff (err :: AWS.RequestError | eff) StartDeviceSyncResponse
startDeviceSync = AWS.request serviceName "startDeviceSync" 


-- | <p>Adds metadata tags to a specified resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "tagResource" 


-- | <p>Removes metadata tags from a specified resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "untagResource" 


-- | <p>Updates the device name by device ARN.</p>
updateDevice :: forall eff. UpdateDeviceRequest -> Aff (err :: AWS.RequestError | eff) UpdateDeviceResponse
updateDevice = AWS.request serviceName "updateDevice" 


-- | <p>Updates an existing room profile by room profile ARN.</p>
updateProfile :: forall eff. UpdateProfileRequest -> Aff (err :: AWS.RequestError | eff) UpdateProfileResponse
updateProfile = AWS.request serviceName "updateProfile" 


-- | <p>Updates room details by room ARN.</p>
updateRoom :: forall eff. UpdateRoomRequest -> Aff (err :: AWS.RequestError | eff) UpdateRoomResponse
updateRoom = AWS.request serviceName "updateRoom" 


-- | <p>Updates skill group details by skill group ARN.</p>
updateSkillGroup :: forall eff. UpdateSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateSkillGroupResponse
updateSkillGroup = AWS.request serviceName "updateSkillGroup" 


newtype Address = Address String
derive instance newtypeAddress :: Newtype Address _


-- | <p>The resource being created already exists. HTTP Status Code: 400</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


newtype AssociateDeviceWithRoomRequest = AssociateDeviceWithRoomRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeAssociateDeviceWithRoomRequest :: Newtype AssociateDeviceWithRoomRequest _


newtype AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse 
  { 
  }
derive instance newtypeAssociateDeviceWithRoomResponse :: Newtype AssociateDeviceWithRoomResponse _


newtype AssociateSkillGroupWithRoomRequest = AssociateSkillGroupWithRoomRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeAssociateSkillGroupWithRoomRequest :: Newtype AssociateSkillGroupWithRoomRequest _


newtype AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse 
  { 
  }
derive instance newtypeAssociateSkillGroupWithRoomResponse :: Newtype AssociateSkillGroupWithRoomResponse _


-- | User specified token that is used to support idempotency during Create Resource
newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _


newtype CreateProfileRequest = CreateProfileRequest 
  { "ProfileName" :: (ProfileName)
  , "Timezone" :: (Timezone)
  , "Address" :: (Address)
  , "DistanceUnit" :: (DistanceUnit)
  , "TemperatureUnit" :: (TemperatureUnit)
  , "WakeWord" :: (WakeWord)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCreateProfileRequest :: Newtype CreateProfileRequest _


newtype CreateProfileResponse = CreateProfileResponse 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeCreateProfileResponse :: Newtype CreateProfileResponse _


newtype CreateRoomRequest = CreateRoomRequest 
  { "RoomName" :: (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProfileArn" :: NullOrUndefined (Arn)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateRoomRequest :: Newtype CreateRoomRequest _


newtype CreateRoomResponse = CreateRoomResponse 
  { "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeCreateRoomResponse :: Newtype CreateRoomResponse _


newtype CreateSkillGroupRequest = CreateSkillGroupRequest 
  { "SkillGroupName" :: (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateSkillGroupRequest :: Newtype CreateSkillGroupRequest _


newtype CreateSkillGroupResponse = CreateSkillGroupResponse 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeCreateSkillGroupResponse :: Newtype CreateSkillGroupResponse _


newtype CreateUserRequest = CreateUserRequest 
  { "UserId" :: (User_UserId')
  , "FirstName" :: NullOrUndefined (User_FirstName')
  , "LastName" :: NullOrUndefined (User_LastName')
  , "Email" :: NullOrUndefined (Email)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _


newtype CreateUserResponse = CreateUserResponse 
  { "UserArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _


newtype DeleteProfileRequest = DeleteProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDeleteProfileRequest :: Newtype DeleteProfileRequest _


newtype DeleteProfileResponse = DeleteProfileResponse 
  { 
  }
derive instance newtypeDeleteProfileResponse :: Newtype DeleteProfileResponse _


newtype DeleteRoomRequest = DeleteRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDeleteRoomRequest :: Newtype DeleteRoomRequest _


newtype DeleteRoomResponse = DeleteRoomResponse 
  { 
  }
derive instance newtypeDeleteRoomResponse :: Newtype DeleteRoomResponse _


newtype DeleteRoomSkillParameterRequest = DeleteRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "ParameterKey" :: (RoomSkillParameterKey)
  }
derive instance newtypeDeleteRoomSkillParameterRequest :: Newtype DeleteRoomSkillParameterRequest _


newtype DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse 
  { 
  }
derive instance newtypeDeleteRoomSkillParameterResponse :: Newtype DeleteRoomSkillParameterResponse _


newtype DeleteSkillGroupRequest = DeleteSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDeleteSkillGroupRequest :: Newtype DeleteSkillGroupRequest _


newtype DeleteSkillGroupResponse = DeleteSkillGroupResponse 
  { 
  }
derive instance newtypeDeleteSkillGroupResponse :: Newtype DeleteSkillGroupResponse _


newtype DeleteUserRequest = DeleteUserRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  , "EnrollmentId" :: (EnrollmentId)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _


newtype DeleteUserResponse = DeleteUserResponse 
  { 
  }
derive instance newtypeDeleteUserResponse :: Newtype DeleteUserResponse _


-- | <p>A device with attributes.</p>
newtype Device = Device 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber)
  , "DeviceType" :: NullOrUndefined (DeviceType)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  , "SoftwareVersion" :: NullOrUndefined (SoftwareVersion)
  , "MacAddress" :: NullOrUndefined (MacAddress)
  , "RoomArn" :: NullOrUndefined (Arn)
  , "DeviceStatus" :: NullOrUndefined (DeviceStatus)
  , "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo)
  }
derive instance newtypeDevice :: Newtype Device _


-- | <p>Device attributes.</p>
newtype DeviceData = DeviceData 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber)
  , "DeviceType" :: NullOrUndefined (DeviceType)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  , "SoftwareVersion" :: NullOrUndefined (SoftwareVersion)
  , "MacAddress" :: NullOrUndefined (MacAddress)
  , "DeviceStatus" :: NullOrUndefined (DeviceStatus)
  , "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo)
  }
derive instance newtypeDeviceData :: Newtype DeviceData _


newtype DeviceDataList = DeviceDataList (Array DeviceData)
derive instance newtypeDeviceDataList :: Newtype DeviceDataList _


newtype DeviceName = DeviceName String
derive instance newtypeDeviceName :: Newtype DeviceName _


newtype DeviceSerialNumber = DeviceSerialNumber String
derive instance newtypeDeviceSerialNumber :: Newtype DeviceSerialNumber _


newtype DeviceStatus = DeviceStatus String
derive instance newtypeDeviceStatus :: Newtype DeviceStatus _


-- | <p>Details of a device’s status.</p>
newtype DeviceStatusDetail = DeviceStatusDetail 
  { "Code" :: NullOrUndefined (DeviceStatusDetailCode)
  }
derive instance newtypeDeviceStatusDetail :: Newtype DeviceStatusDetail _


newtype DeviceStatusDetailCode = DeviceStatusDetailCode String
derive instance newtypeDeviceStatusDetailCode :: Newtype DeviceStatusDetailCode _


newtype DeviceStatusDetails = DeviceStatusDetails (Array DeviceStatusDetail)
derive instance newtypeDeviceStatusDetails :: Newtype DeviceStatusDetails _


-- | <p>Detailed information about a device's status.</p>
newtype DeviceStatusInfo = DeviceStatusInfo 
  { "DeviceStatusDetails" :: NullOrUndefined (DeviceStatusDetails)
  }
derive instance newtypeDeviceStatusInfo :: Newtype DeviceStatusInfo _


newtype DeviceType = DeviceType String
derive instance newtypeDeviceType :: Newtype DeviceType _


newtype DisassociateDeviceFromRoomRequest = DisassociateDeviceFromRoomRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDisassociateDeviceFromRoomRequest :: Newtype DisassociateDeviceFromRoomRequest _


newtype DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse 
  { 
  }
derive instance newtypeDisassociateDeviceFromRoomResponse :: Newtype DisassociateDeviceFromRoomResponse _


newtype DisassociateSkillGroupFromRoomRequest = DisassociateSkillGroupFromRoomRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeDisassociateSkillGroupFromRoomRequest :: Newtype DisassociateSkillGroupFromRoomRequest _


newtype DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse 
  { 
  }
derive instance newtypeDisassociateSkillGroupFromRoomResponse :: Newtype DisassociateSkillGroupFromRoomResponse _


newtype DistanceUnit = DistanceUnit String
derive instance newtypeDistanceUnit :: Newtype DistanceUnit _


newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


newtype EnrollmentId = EnrollmentId String
derive instance newtypeEnrollmentId :: Newtype EnrollmentId _


newtype EnrollmentStatus = EnrollmentStatus String
derive instance newtypeEnrollmentStatus :: Newtype EnrollmentStatus _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype Feature = Feature String
derive instance newtypeFeature :: Newtype Feature _


newtype Features = Features (Array Feature)
derive instance newtypeFeatures :: Newtype Features _


-- | <p>A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.</p>
newtype Filter = Filter 
  { "Key" :: (FilterKey)
  , "Values" :: (FilterValueList)
  }
derive instance newtypeFilter :: Newtype Filter _


newtype FilterKey = FilterKey String
derive instance newtypeFilterKey :: Newtype FilterKey _


newtype FilterList = FilterList (Array Filter)
derive instance newtypeFilterList :: Newtype FilterList _


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _


newtype FilterValueList = FilterValueList (Array FilterValue)
derive instance newtypeFilterValueList :: Newtype FilterValueList _


newtype GetDeviceRequest = GetDeviceRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeGetDeviceRequest :: Newtype GetDeviceRequest _


newtype GetDeviceResponse = GetDeviceResponse 
  { "Device" :: NullOrUndefined (Device)
  }
derive instance newtypeGetDeviceResponse :: Newtype GetDeviceResponse _


newtype GetProfileRequest = GetProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeGetProfileRequest :: Newtype GetProfileRequest _


newtype GetProfileResponse = GetProfileResponse 
  { "Profile" :: NullOrUndefined (Profile)
  }
derive instance newtypeGetProfileResponse :: Newtype GetProfileResponse _


newtype GetRoomRequest = GetRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeGetRoomRequest :: Newtype GetRoomRequest _


newtype GetRoomResponse = GetRoomResponse 
  { "Room" :: NullOrUndefined (Room)
  }
derive instance newtypeGetRoomResponse :: Newtype GetRoomResponse _


newtype GetRoomSkillParameterRequest = GetRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "ParameterKey" :: (RoomSkillParameterKey)
  }
derive instance newtypeGetRoomSkillParameterRequest :: Newtype GetRoomSkillParameterRequest _


newtype GetRoomSkillParameterResponse = GetRoomSkillParameterResponse 
  { "RoomSkillParameter" :: NullOrUndefined (RoomSkillParameter)
  }
derive instance newtypeGetRoomSkillParameterResponse :: Newtype GetRoomSkillParameterResponse _


newtype GetSkillGroupRequest = GetSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeGetSkillGroupRequest :: Newtype GetSkillGroupRequest _


newtype GetSkillGroupResponse = GetSkillGroupResponse 
  { "SkillGroup" :: NullOrUndefined (SkillGroup)
  }
derive instance newtypeGetSkillGroupResponse :: Newtype GetSkillGroupResponse _


-- | <p>The attempt to update a user is invalid due to the user's current status. HTTP Status Code: 400</p>
newtype InvalidUserStatusException = InvalidUserStatusException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidUserStatusException :: Newtype InvalidUserStatusException _


-- | <p>You are performing an action that would put you beyond your account's limits. HTTP Status Code: 400</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListSkillsRequest = ListSkillsRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (SkillListMaxResults)
  }
derive instance newtypeListSkillsRequest :: Newtype ListSkillsRequest _


newtype ListSkillsResponse = ListSkillsResponse 
  { "SkillSummaries" :: NullOrUndefined (SkillSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListSkillsResponse :: Newtype ListSkillsResponse _


newtype ListTagsRequest = ListTagsRequest 
  { "Arn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _


newtype ListTagsResponse = ListTagsResponse 
  { "Tags" :: NullOrUndefined (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _


newtype MacAddress = MacAddress String
derive instance newtypeMacAddress :: Newtype MacAddress _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MaxVolumeLimit = MaxVolumeLimit Int
derive instance newtypeMaxVolumeLimit :: Newtype MaxVolumeLimit _


-- | <p>The name sent in the request is already in use. HTTP Status Code: 400</p>
newtype NameInUseException = NameInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNameInUseException :: Newtype NameInUseException _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The resource is not found. HTTP Status Code: 400</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | <p>A room profile with attributes.</p>
newtype Profile = Profile 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Address" :: NullOrUndefined (Address)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeProfile :: Newtype Profile _


-- | <p>The data of a room profile.</p>
newtype ProfileData = ProfileData 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Address" :: NullOrUndefined (Address)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  }
derive instance newtypeProfileData :: Newtype ProfileData _


newtype ProfileDataList = ProfileDataList (Array ProfileData)
derive instance newtypeProfileDataList :: Newtype ProfileDataList _


newtype ProfileName = ProfileName String
derive instance newtypeProfileName :: Newtype ProfileName _


newtype ProviderCalendarId = ProviderCalendarId String
derive instance newtypeProviderCalendarId :: Newtype ProviderCalendarId _


newtype PutRoomSkillParameterRequest = PutRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "RoomSkillParameter" :: (RoomSkillParameter)
  }
derive instance newtypePutRoomSkillParameterRequest :: Newtype PutRoomSkillParameterRequest _


newtype PutRoomSkillParameterResponse = PutRoomSkillParameterResponse 
  { 
  }
derive instance newtypePutRoomSkillParameterResponse :: Newtype PutRoomSkillParameterResponse _


newtype ResolveRoomRequest = ResolveRoomRequest 
  { "UserId" :: (UserId)
  , "SkillId" :: (SkillId)
  }
derive instance newtypeResolveRoomRequest :: Newtype ResolveRoomRequest _


newtype ResolveRoomResponse = ResolveRoomResponse 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "RoomSkillParameters" :: NullOrUndefined (RoomSkillParameters)
  }
derive instance newtypeResolveRoomResponse :: Newtype ResolveRoomResponse _


-- | <p>The resource in the request is already in use. HTTP Status Code: 400</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


newtype RevokeInvitationRequest = RevokeInvitationRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  , "EnrollmentId" :: NullOrUndefined (EnrollmentId)
  }
derive instance newtypeRevokeInvitationRequest :: Newtype RevokeInvitationRequest _


newtype RevokeInvitationResponse = RevokeInvitationResponse 
  { 
  }
derive instance newtypeRevokeInvitationResponse :: Newtype RevokeInvitationResponse _


-- | <p>A room with attributes.</p>
newtype Room = Room 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeRoom :: Newtype Room _


-- | <p>The data of a room.</p>
newtype RoomData = RoomData 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  }
derive instance newtypeRoomData :: Newtype RoomData _


newtype RoomDataList = RoomDataList (Array RoomData)
derive instance newtypeRoomDataList :: Newtype RoomDataList _


newtype RoomDescription = RoomDescription String
derive instance newtypeRoomDescription :: Newtype RoomDescription _


newtype RoomName = RoomName String
derive instance newtypeRoomName :: Newtype RoomName _


-- | <p>A skill parameter associated with a room.</p>
newtype RoomSkillParameter = RoomSkillParameter 
  { "ParameterKey" :: (RoomSkillParameterKey)
  , "ParameterValue" :: (RoomSkillParameterValue)
  }
derive instance newtypeRoomSkillParameter :: Newtype RoomSkillParameter _


newtype RoomSkillParameterKey = RoomSkillParameterKey String
derive instance newtypeRoomSkillParameterKey :: Newtype RoomSkillParameterKey _


newtype RoomSkillParameterValue = RoomSkillParameterValue String
derive instance newtypeRoomSkillParameterValue :: Newtype RoomSkillParameterValue _


newtype RoomSkillParameters = RoomSkillParameters (Array RoomSkillParameter)
derive instance newtypeRoomSkillParameters :: Newtype RoomSkillParameters _


newtype SearchDevicesRequest = SearchDevicesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }
derive instance newtypeSearchDevicesRequest :: Newtype SearchDevicesRequest _


newtype SearchDevicesResponse = SearchDevicesResponse 
  { "Devices" :: NullOrUndefined (DeviceDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }
derive instance newtypeSearchDevicesResponse :: Newtype SearchDevicesResponse _


newtype SearchProfilesRequest = SearchProfilesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }
derive instance newtypeSearchProfilesRequest :: Newtype SearchProfilesRequest _


newtype SearchProfilesResponse = SearchProfilesResponse 
  { "Profiles" :: NullOrUndefined (ProfileDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }
derive instance newtypeSearchProfilesResponse :: Newtype SearchProfilesResponse _


newtype SearchRoomsRequest = SearchRoomsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }
derive instance newtypeSearchRoomsRequest :: Newtype SearchRoomsRequest _


newtype SearchRoomsResponse = SearchRoomsResponse 
  { "Rooms" :: NullOrUndefined (RoomDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }
derive instance newtypeSearchRoomsResponse :: Newtype SearchRoomsResponse _


newtype SearchSkillGroupsRequest = SearchSkillGroupsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }
derive instance newtypeSearchSkillGroupsRequest :: Newtype SearchSkillGroupsRequest _


newtype SearchSkillGroupsResponse = SearchSkillGroupsResponse 
  { "SkillGroups" :: NullOrUndefined (SkillGroupDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }
derive instance newtypeSearchSkillGroupsResponse :: Newtype SearchSkillGroupsResponse _


newtype SearchUsersRequest = SearchUsersRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }
derive instance newtypeSearchUsersRequest :: Newtype SearchUsersRequest _


newtype SearchUsersResponse = SearchUsersResponse 
  { "Users" :: NullOrUndefined (UserDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }
derive instance newtypeSearchUsersResponse :: Newtype SearchUsersResponse _


newtype SendInvitationRequest = SendInvitationRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeSendInvitationRequest :: Newtype SendInvitationRequest _


newtype SendInvitationResponse = SendInvitationResponse 
  { 
  }
derive instance newtypeSendInvitationResponse :: Newtype SendInvitationResponse _


-- | <p>A skill group with attributes.</p>
newtype SkillGroup = SkillGroup 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }
derive instance newtypeSkillGroup :: Newtype SkillGroup _


-- | <p>The attributes of a skill group.</p>
newtype SkillGroupData = SkillGroupData 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }
derive instance newtypeSkillGroupData :: Newtype SkillGroupData _


newtype SkillGroupDataList = SkillGroupDataList (Array SkillGroupData)
derive instance newtypeSkillGroupDataList :: Newtype SkillGroupDataList _


newtype SkillGroupDescription = SkillGroupDescription String
derive instance newtypeSkillGroupDescription :: Newtype SkillGroupDescription _


newtype SkillGroupName = SkillGroupName String
derive instance newtypeSkillGroupName :: Newtype SkillGroupName _


newtype SkillId = SkillId String
derive instance newtypeSkillId :: Newtype SkillId _


newtype SkillListMaxResults = SkillListMaxResults Int
derive instance newtypeSkillListMaxResults :: Newtype SkillListMaxResults _


newtype SkillName = SkillName String
derive instance newtypeSkillName :: Newtype SkillName _


-- | <p>The summary of skills.</p>
newtype SkillSummary = SkillSummary 
  { "SkillId" :: NullOrUndefined (SkillId)
  , "SkillName" :: NullOrUndefined (SkillName)
  , "SupportsLinking" :: NullOrUndefined (Boolean)
  }
derive instance newtypeSkillSummary :: Newtype SkillSummary _


newtype SkillSummaryList = SkillSummaryList (Array SkillSummary)
derive instance newtypeSkillSummaryList :: Newtype SkillSummaryList _


newtype SoftwareVersion = SoftwareVersion String
derive instance newtypeSoftwareVersion :: Newtype SoftwareVersion _


-- | <p>An object representing a sort criteria. </p>
newtype Sort = Sort 
  { "Key" :: (SortKey)
  , "Value" :: (SortValue)
  }
derive instance newtypeSort :: Newtype Sort _


newtype SortKey = SortKey String
derive instance newtypeSortKey :: Newtype SortKey _


newtype SortList = SortList (Array Sort)
derive instance newtypeSortList :: Newtype SortList _


newtype SortValue = SortValue String
derive instance newtypeSortValue :: Newtype SortValue _


newtype StartDeviceSyncRequest = StartDeviceSyncRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "DeviceArn" :: NullOrUndefined (Arn)
  , "Features" :: (Features)
  }
derive instance newtypeStartDeviceSyncRequest :: Newtype StartDeviceSyncRequest _


newtype StartDeviceSyncResponse = StartDeviceSyncResponse 
  { 
  }
derive instance newtypeStartDeviceSyncResponse :: Newtype StartDeviceSyncResponse _


-- | <p>A key-value pair that can be associated with a resource. </p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagResourceRequest = TagResourceRequest 
  { "Arn" :: (Arn)
  , "Tags" :: (TagList)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _


newtype TagResourceResponse = TagResourceResponse 
  { 
  }
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype TemperatureUnit = TemperatureUnit String
derive instance newtypeTemperatureUnit :: Newtype TemperatureUnit _


newtype Timezone = Timezone String
derive instance newtypeTimezone :: Newtype Timezone _


newtype TotalCount = TotalCount Int
derive instance newtypeTotalCount :: Newtype TotalCount _


newtype UntagResourceRequest = UntagResourceRequest 
  { "Arn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _


newtype UpdateDeviceRequest = UpdateDeviceRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  }
derive instance newtypeUpdateDeviceRequest :: Newtype UpdateDeviceRequest _


newtype UpdateDeviceResponse = UpdateDeviceResponse 
  { 
  }
derive instance newtypeUpdateDeviceResponse :: Newtype UpdateDeviceResponse _


newtype UpdateProfileRequest = UpdateProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "Address" :: NullOrUndefined (Address)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateProfileRequest :: Newtype UpdateProfileRequest _


newtype UpdateProfileResponse = UpdateProfileResponse 
  { 
  }
derive instance newtypeUpdateProfileResponse :: Newtype UpdateProfileResponse _


newtype UpdateRoomRequest = UpdateRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  }
derive instance newtypeUpdateRoomRequest :: Newtype UpdateRoomRequest _


newtype UpdateRoomResponse = UpdateRoomResponse 
  { 
  }
derive instance newtypeUpdateRoomResponse :: Newtype UpdateRoomResponse _


newtype UpdateSkillGroupRequest = UpdateSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }
derive instance newtypeUpdateSkillGroupRequest :: Newtype UpdateSkillGroupRequest _


newtype UpdateSkillGroupResponse = UpdateSkillGroupResponse 
  { 
  }
derive instance newtypeUpdateSkillGroupResponse :: Newtype UpdateSkillGroupResponse _


-- | <p>Information related to a user.</p>
newtype UserData = UserData 
  { "UserArn" :: NullOrUndefined (Arn)
  , "FirstName" :: NullOrUndefined (User_FirstName')
  , "LastName" :: NullOrUndefined (User_LastName')
  , "Email" :: NullOrUndefined (Email)
  , "EnrollmentStatus" :: NullOrUndefined (EnrollmentStatus)
  , "EnrollmentId" :: NullOrUndefined (EnrollmentId)
  }
derive instance newtypeUserData :: Newtype UserData _


newtype UserDataList = UserDataList (Array UserData)
derive instance newtypeUserDataList :: Newtype UserDataList _


newtype UserId = UserId String
derive instance newtypeUserId :: Newtype UserId _


newtype WakeWord = WakeWord String
derive instance newtypeWakeWord :: Newtype WakeWord _


newtype User_FirstName' = User_FirstName' String
derive instance newtypeUser_FirstName' :: Newtype User_FirstName' _


newtype User_LastName' = User_LastName' String
derive instance newtypeUser_LastName' :: Newtype User_LastName' _


newtype User_UserId' = User_UserId' String
derive instance newtypeUser_UserId' :: Newtype User_UserId' _
