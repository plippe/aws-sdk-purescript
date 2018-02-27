## Module AWS.AlexaForBusiness

<p>Alexa for Business makes it easy for you to use Alexa in your organization. Alexa for Business gives you the tools you need to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit, and the Alexa for Business APIs, and you can make these available as private skills for your organization. Alexa for Business also makes it easy to voice-enable your products and services, providing context-aware voice experiences for your customers.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateDeviceWithRoom`

``` purescript
associateDeviceWithRoom :: forall eff. AssociateDeviceWithRoomRequest -> Aff (err :: RequestError | eff) AssociateDeviceWithRoomResponse
```

<p>Associates a device to a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or a manual sync is required. </p>

#### `associateSkillGroupWithRoom`

``` purescript
associateSkillGroupWithRoom :: forall eff. AssociateSkillGroupWithRoomRequest -> Aff (err :: RequestError | eff) AssociateSkillGroupWithRoomResponse
```

<p>Associates a skill group to a given room. This enables all skills in the associated skill group on all devices in the room.</p>

#### `createProfile`

``` purescript
createProfile :: forall eff. CreateProfileRequest -> Aff (err :: RequestError | eff) CreateProfileResponse
```

<p>Creates a new room profile with the specified details.</p>

#### `createRoom`

``` purescript
createRoom :: forall eff. CreateRoomRequest -> Aff (err :: RequestError | eff) CreateRoomResponse
```

<p>Creates a room with the specified details.</p>

#### `createSkillGroup`

``` purescript
createSkillGroup :: forall eff. CreateSkillGroupRequest -> Aff (err :: RequestError | eff) CreateSkillGroupResponse
```

<p>Creates a skill group with a specified name and description.</p>

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

<p>Creates a user.</p>

#### `deleteProfile`

``` purescript
deleteProfile :: forall eff. DeleteProfileRequest -> Aff (err :: RequestError | eff) DeleteProfileResponse
```

<p>Deletes a room profile by the profile ARN.</p>

#### `deleteRoom`

``` purescript
deleteRoom :: forall eff. DeleteRoomRequest -> Aff (err :: RequestError | eff) DeleteRoomResponse
```

<p>Deletes a room by the room ARN.</p>

#### `deleteRoomSkillParameter`

``` purescript
deleteRoomSkillParameter :: forall eff. DeleteRoomSkillParameterRequest -> Aff (err :: RequestError | eff) DeleteRoomSkillParameterResponse
```

<p>Deletes room skill parameter details by room, skill, and parameter key ID.</p>

#### `deleteSkillGroup`

``` purescript
deleteSkillGroup :: forall eff. DeleteSkillGroupRequest -> Aff (err :: RequestError | eff) DeleteSkillGroupResponse
```

<p>Deletes a skill group by skill group ARN.</p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) DeleteUserResponse
```

<p>Deletes a specified user by user ARN and enrollment ARN.</p>

#### `disassociateDeviceFromRoom`

``` purescript
disassociateDeviceFromRoom :: forall eff. DisassociateDeviceFromRoomRequest -> Aff (err :: RequestError | eff) DisassociateDeviceFromRoomResponse
```

<p>Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.</p>

#### `disassociateSkillGroupFromRoom`

``` purescript
disassociateSkillGroupFromRoom :: forall eff. DisassociateSkillGroupFromRoomRequest -> Aff (err :: RequestError | eff) DisassociateSkillGroupFromRoomResponse
```

<p>Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.</p>

#### `getDevice`

``` purescript
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: RequestError | eff) GetDeviceResponse
```

<p>Gets the details of a device by device ARN.</p>

#### `getProfile`

``` purescript
getProfile :: forall eff. GetProfileRequest -> Aff (err :: RequestError | eff) GetProfileResponse
```

<p>Gets the details of a room profile by profile ARN.</p>

#### `getRoom`

``` purescript
getRoom :: forall eff. GetRoomRequest -> Aff (err :: RequestError | eff) GetRoomResponse
```

<p>Gets room details by room ARN.</p>

#### `getRoomSkillParameter`

``` purescript
getRoomSkillParameter :: forall eff. GetRoomSkillParameterRequest -> Aff (err :: RequestError | eff) GetRoomSkillParameterResponse
```

<p>Gets room skill parameter details by room, skill, and parameter key ARN.</p>

#### `getSkillGroup`

``` purescript
getSkillGroup :: forall eff. GetSkillGroupRequest -> Aff (err :: RequestError | eff) GetSkillGroupResponse
```

<p>Gets skill group details by skill group ARN.</p>

#### `listSkills`

``` purescript
listSkills :: forall eff. ListSkillsRequest -> Aff (err :: RequestError | eff) ListSkillsResponse
```

<p>Lists all enabled skills in a specific skill group.</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResponse
```

<p>Lists all tags for a specific resource.</p>

#### `putRoomSkillParameter`

``` purescript
putRoomSkillParameter :: forall eff. PutRoomSkillParameterRequest -> Aff (err :: RequestError | eff) PutRoomSkillParameterResponse
```

<p>Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.</p>

#### `resolveRoom`

``` purescript
resolveRoom :: forall eff. ResolveRoomRequest -> Aff (err :: RequestError | eff) ResolveRoomResponse
```

<p>Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.</p>

#### `revokeInvitation`

``` purescript
revokeInvitation :: forall eff. RevokeInvitationRequest -> Aff (err :: RequestError | eff) RevokeInvitationResponse
```

<p>Revokes an invitation and invalidates the enrollment URL.</p>

#### `searchDevices`

``` purescript
searchDevices :: forall eff. SearchDevicesRequest -> Aff (err :: RequestError | eff) SearchDevicesResponse
```

<p>Searches devices and lists the ones that meet a set of filter criteria.</p>

#### `searchProfiles`

``` purescript
searchProfiles :: forall eff. SearchProfilesRequest -> Aff (err :: RequestError | eff) SearchProfilesResponse
```

<p>Searches room profiles and lists the ones that meet a set of filter criteria.</p>

#### `searchRooms`

``` purescript
searchRooms :: forall eff. SearchRoomsRequest -> Aff (err :: RequestError | eff) SearchRoomsResponse
```

<p>Searches rooms and lists the ones that meet a set of filter and sort criteria.</p>

#### `searchSkillGroups`

``` purescript
searchSkillGroups :: forall eff. SearchSkillGroupsRequest -> Aff (err :: RequestError | eff) SearchSkillGroupsResponse
```

<p>Searches skill groups and lists the ones that meet a set of filter and sort criteria.</p>

#### `searchUsers`

``` purescript
searchUsers :: forall eff. SearchUsersRequest -> Aff (err :: RequestError | eff) SearchUsersResponse
```

<p>Searches users and lists the ones that meet a set of filter and sort criteria.</p>

#### `sendInvitation`

``` purescript
sendInvitation :: forall eff. SendInvitationRequest -> Aff (err :: RequestError | eff) SendInvitationResponse
```

<p>Sends an enrollment invitation email with a URL to a user. The URL is valid for 72 hours or until you call this operation again, whichever comes first. </p>

#### `startDeviceSync`

``` purescript
startDeviceSync :: forall eff. StartDeviceSyncRequest -> Aff (err :: RequestError | eff) StartDeviceSyncResponse
```

<p>Resets a device and its account to the known default settings by clearing all information and settings set by previous users.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Adds metadata tags to a specified resource.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Removes metadata tags from a specified resource.</p>

#### `updateDevice`

``` purescript
updateDevice :: forall eff. UpdateDeviceRequest -> Aff (err :: RequestError | eff) UpdateDeviceResponse
```

<p>Updates the device name by device ARN.</p>

#### `updateProfile`

``` purescript
updateProfile :: forall eff. UpdateProfileRequest -> Aff (err :: RequestError | eff) UpdateProfileResponse
```

<p>Updates an existing room profile by room profile ARN.</p>

#### `updateRoom`

``` purescript
updateRoom :: forall eff. UpdateRoomRequest -> Aff (err :: RequestError | eff) UpdateRoomResponse
```

<p>Updates room details by room ARN.</p>

#### `updateSkillGroup`

``` purescript
updateSkillGroup :: forall eff. UpdateSkillGroupRequest -> Aff (err :: RequestError | eff) UpdateSkillGroupResponse
```

<p>Updates skill group details by skill group ARN.</p>

#### `Address`

``` purescript
newtype Address
  = Address String
```

##### Instances
``` purescript
Newtype Address _
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource being created already exists. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype AlreadyExistsException _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `AssociateDeviceWithRoomRequest`

``` purescript
newtype AssociateDeviceWithRoomRequest
  = AssociateDeviceWithRoomRequest { "DeviceArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype AssociateDeviceWithRoomRequest _
```

#### `AssociateDeviceWithRoomResponse`

``` purescript
newtype AssociateDeviceWithRoomResponse
  = AssociateDeviceWithRoomResponse {  }
```

##### Instances
``` purescript
Newtype AssociateDeviceWithRoomResponse _
```

#### `AssociateSkillGroupWithRoomRequest`

``` purescript
newtype AssociateSkillGroupWithRoomRequest
  = AssociateSkillGroupWithRoomRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype AssociateSkillGroupWithRoomRequest _
```

#### `AssociateSkillGroupWithRoomResponse`

``` purescript
newtype AssociateSkillGroupWithRoomResponse
  = AssociateSkillGroupWithRoomResponse {  }
```

##### Instances
``` purescript
Newtype AssociateSkillGroupWithRoomResponse _
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

User specified token that is used to support idempotency during Create Resource

##### Instances
``` purescript
Newtype ClientRequestToken _
```

#### `CreateProfileRequest`

``` purescript
newtype CreateProfileRequest
  = CreateProfileRequest { "ProfileName" :: ProfileName, "Timezone" :: Timezone, "Address" :: Address, "DistanceUnit" :: DistanceUnit, "TemperatureUnit" :: TemperatureUnit, "WakeWord" :: WakeWord, "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype CreateProfileRequest _
```

#### `CreateProfileResponse`

``` purescript
newtype CreateProfileResponse
  = CreateProfileResponse { "ProfileArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype CreateProfileResponse _
```

#### `CreateRoomRequest`

``` purescript
newtype CreateRoomRequest
  = CreateRoomRequest { "RoomName" :: RoomName, "Description" :: NullOrUndefined (RoomDescription), "ProfileArn" :: NullOrUndefined (Arn), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype CreateRoomRequest _
```

#### `CreateRoomResponse`

``` purescript
newtype CreateRoomResponse
  = CreateRoomResponse { "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype CreateRoomResponse _
```

#### `CreateSkillGroupRequest`

``` purescript
newtype CreateSkillGroupRequest
  = CreateSkillGroupRequest { "SkillGroupName" :: SkillGroupName, "Description" :: NullOrUndefined (SkillGroupDescription), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype CreateSkillGroupRequest _
```

#### `CreateSkillGroupResponse`

``` purescript
newtype CreateSkillGroupResponse
  = CreateSkillGroupResponse { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype CreateSkillGroupResponse _
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "UserId" :: User_UserId', "FirstName" :: NullOrUndefined (User_FirstName'), "LastName" :: NullOrUndefined (User_LastName'), "Email" :: NullOrUndefined (Email), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "Tags" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype CreateUserRequest _
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse { "UserArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype CreateUserResponse _
```

#### `DeleteProfileRequest`

``` purescript
newtype DeleteProfileRequest
  = DeleteProfileRequest { "ProfileArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DeleteProfileRequest _
```

#### `DeleteProfileResponse`

``` purescript
newtype DeleteProfileResponse
  = DeleteProfileResponse {  }
```

##### Instances
``` purescript
Newtype DeleteProfileResponse _
```

#### `DeleteRoomRequest`

``` purescript
newtype DeleteRoomRequest
  = DeleteRoomRequest { "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DeleteRoomRequest _
```

#### `DeleteRoomResponse`

``` purescript
newtype DeleteRoomResponse
  = DeleteRoomResponse {  }
```

##### Instances
``` purescript
Newtype DeleteRoomResponse _
```

#### `DeleteRoomSkillParameterRequest`

``` purescript
newtype DeleteRoomSkillParameterRequest
  = DeleteRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "ParameterKey" :: RoomSkillParameterKey }
```

##### Instances
``` purescript
Newtype DeleteRoomSkillParameterRequest _
```

#### `DeleteRoomSkillParameterResponse`

``` purescript
newtype DeleteRoomSkillParameterResponse
  = DeleteRoomSkillParameterResponse {  }
```

##### Instances
``` purescript
Newtype DeleteRoomSkillParameterResponse _
```

#### `DeleteSkillGroupRequest`

``` purescript
newtype DeleteSkillGroupRequest
  = DeleteSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DeleteSkillGroupRequest _
```

#### `DeleteSkillGroupResponse`

``` purescript
newtype DeleteSkillGroupResponse
  = DeleteSkillGroupResponse {  }
```

##### Instances
``` purescript
Newtype DeleteSkillGroupResponse _
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "UserArn" :: NullOrUndefined (Arn), "EnrollmentId" :: EnrollmentId }
```

##### Instances
``` purescript
Newtype DeleteUserRequest _
```

#### `DeleteUserResponse`

``` purescript
newtype DeleteUserResponse
  = DeleteUserResponse {  }
```

##### Instances
``` purescript
Newtype DeleteUserResponse _
```

#### `Device`

``` purescript
newtype Device
  = Device { "DeviceArn" :: NullOrUndefined (Arn), "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber), "DeviceType" :: NullOrUndefined (DeviceType), "DeviceName" :: NullOrUndefined (DeviceName), "SoftwareVersion" :: NullOrUndefined (SoftwareVersion), "MacAddress" :: NullOrUndefined (MacAddress), "RoomArn" :: NullOrUndefined (Arn), "DeviceStatus" :: NullOrUndefined (DeviceStatus), "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo) }
```

<p>A device with attributes.</p>

##### Instances
``` purescript
Newtype Device _
```

#### `DeviceData`

``` purescript
newtype DeviceData
  = DeviceData { "DeviceArn" :: NullOrUndefined (Arn), "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber), "DeviceType" :: NullOrUndefined (DeviceType), "DeviceName" :: NullOrUndefined (DeviceName), "SoftwareVersion" :: NullOrUndefined (SoftwareVersion), "MacAddress" :: NullOrUndefined (MacAddress), "DeviceStatus" :: NullOrUndefined (DeviceStatus), "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo) }
```

<p>Device attributes.</p>

##### Instances
``` purescript
Newtype DeviceData _
```

#### `DeviceDataList`

``` purescript
newtype DeviceDataList
  = DeviceDataList (Array DeviceData)
```

##### Instances
``` purescript
Newtype DeviceDataList _
```

#### `DeviceName`

``` purescript
newtype DeviceName
  = DeviceName String
```

##### Instances
``` purescript
Newtype DeviceName _
```

#### `DeviceSerialNumber`

``` purescript
newtype DeviceSerialNumber
  = DeviceSerialNumber String
```

##### Instances
``` purescript
Newtype DeviceSerialNumber _
```

#### `DeviceStatus`

``` purescript
newtype DeviceStatus
  = DeviceStatus String
```

##### Instances
``` purescript
Newtype DeviceStatus _
```

#### `DeviceStatusDetail`

``` purescript
newtype DeviceStatusDetail
  = DeviceStatusDetail { "Code" :: NullOrUndefined (DeviceStatusDetailCode) }
```

<p>Details of a device’s status.</p>

##### Instances
``` purescript
Newtype DeviceStatusDetail _
```

#### `DeviceStatusDetailCode`

``` purescript
newtype DeviceStatusDetailCode
  = DeviceStatusDetailCode String
```

##### Instances
``` purescript
Newtype DeviceStatusDetailCode _
```

#### `DeviceStatusDetails`

``` purescript
newtype DeviceStatusDetails
  = DeviceStatusDetails (Array DeviceStatusDetail)
```

##### Instances
``` purescript
Newtype DeviceStatusDetails _
```

#### `DeviceStatusInfo`

``` purescript
newtype DeviceStatusInfo
  = DeviceStatusInfo { "DeviceStatusDetails" :: NullOrUndefined (DeviceStatusDetails) }
```

<p>Detailed information about a device's status.</p>

##### Instances
``` purescript
Newtype DeviceStatusInfo _
```

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType String
```

##### Instances
``` purescript
Newtype DeviceType _
```

#### `DisassociateDeviceFromRoomRequest`

``` purescript
newtype DisassociateDeviceFromRoomRequest
  = DisassociateDeviceFromRoomRequest { "DeviceArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DisassociateDeviceFromRoomRequest _
```

#### `DisassociateDeviceFromRoomResponse`

``` purescript
newtype DisassociateDeviceFromRoomResponse
  = DisassociateDeviceFromRoomResponse {  }
```

##### Instances
``` purescript
Newtype DisassociateDeviceFromRoomResponse _
```

#### `DisassociateSkillGroupFromRoomRequest`

``` purescript
newtype DisassociateSkillGroupFromRoomRequest
  = DisassociateSkillGroupFromRoomRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype DisassociateSkillGroupFromRoomRequest _
```

#### `DisassociateSkillGroupFromRoomResponse`

``` purescript
newtype DisassociateSkillGroupFromRoomResponse
  = DisassociateSkillGroupFromRoomResponse {  }
```

##### Instances
``` purescript
Newtype DisassociateSkillGroupFromRoomResponse _
```

#### `DistanceUnit`

``` purescript
newtype DistanceUnit
  = DistanceUnit String
```

##### Instances
``` purescript
Newtype DistanceUnit _
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

##### Instances
``` purescript
Newtype Email _
```

#### `EnrollmentId`

``` purescript
newtype EnrollmentId
  = EnrollmentId String
```

##### Instances
``` purescript
Newtype EnrollmentId _
```

#### `EnrollmentStatus`

``` purescript
newtype EnrollmentStatus
  = EnrollmentStatus String
```

##### Instances
``` purescript
Newtype EnrollmentStatus _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `Feature`

``` purescript
newtype Feature
  = Feature String
```

##### Instances
``` purescript
Newtype Feature _
```

#### `Features`

``` purescript
newtype Features
  = Features (Array Feature)
```

##### Instances
``` purescript
Newtype Features _
```

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Key" :: FilterKey, "Values" :: FilterValueList }
```

<p>A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.</p>

##### Instances
``` purescript
Newtype Filter _
```

#### `FilterKey`

``` purescript
newtype FilterKey
  = FilterKey String
```

##### Instances
``` purescript
Newtype FilterKey _
```

#### `FilterList`

``` purescript
newtype FilterList
  = FilterList (Array Filter)
```

##### Instances
``` purescript
Newtype FilterList _
```

#### `FilterValue`

``` purescript
newtype FilterValue
  = FilterValue String
```

##### Instances
``` purescript
Newtype FilterValue _
```

#### `FilterValueList`

``` purescript
newtype FilterValueList
  = FilterValueList (Array FilterValue)
```

##### Instances
``` purescript
Newtype FilterValueList _
```

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "DeviceArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype GetDeviceRequest _
```

#### `GetDeviceResponse`

``` purescript
newtype GetDeviceResponse
  = GetDeviceResponse { "Device" :: NullOrUndefined (Device) }
```

##### Instances
``` purescript
Newtype GetDeviceResponse _
```

#### `GetProfileRequest`

``` purescript
newtype GetProfileRequest
  = GetProfileRequest { "ProfileArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype GetProfileRequest _
```

#### `GetProfileResponse`

``` purescript
newtype GetProfileResponse
  = GetProfileResponse { "Profile" :: NullOrUndefined (Profile) }
```

##### Instances
``` purescript
Newtype GetProfileResponse _
```

#### `GetRoomRequest`

``` purescript
newtype GetRoomRequest
  = GetRoomRequest { "RoomArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype GetRoomRequest _
```

#### `GetRoomResponse`

``` purescript
newtype GetRoomResponse
  = GetRoomResponse { "Room" :: NullOrUndefined (Room) }
```

##### Instances
``` purescript
Newtype GetRoomResponse _
```

#### `GetRoomSkillParameterRequest`

``` purescript
newtype GetRoomSkillParameterRequest
  = GetRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "ParameterKey" :: RoomSkillParameterKey }
```

##### Instances
``` purescript
Newtype GetRoomSkillParameterRequest _
```

#### `GetRoomSkillParameterResponse`

``` purescript
newtype GetRoomSkillParameterResponse
  = GetRoomSkillParameterResponse { "RoomSkillParameter" :: NullOrUndefined (RoomSkillParameter) }
```

##### Instances
``` purescript
Newtype GetRoomSkillParameterResponse _
```

#### `GetSkillGroupRequest`

``` purescript
newtype GetSkillGroupRequest
  = GetSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype GetSkillGroupRequest _
```

#### `GetSkillGroupResponse`

``` purescript
newtype GetSkillGroupResponse
  = GetSkillGroupResponse { "SkillGroup" :: NullOrUndefined (SkillGroup) }
```

##### Instances
``` purescript
Newtype GetSkillGroupResponse _
```

#### `InvalidUserStatusException`

``` purescript
newtype InvalidUserStatusException
  = InvalidUserStatusException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The attempt to update a user is invalid due to the user's current status. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype InvalidUserStatusException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>You are performing an action that would put you beyond your account's limits. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListSkillsRequest`

``` purescript
newtype ListSkillsRequest
  = ListSkillsRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (SkillListMaxResults) }
```

##### Instances
``` purescript
Newtype ListSkillsRequest _
```

#### `ListSkillsResponse`

``` purescript
newtype ListSkillsResponse
  = ListSkillsResponse { "SkillSummaries" :: NullOrUndefined (SkillSummaryList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListSkillsResponse _
```

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "Arn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTagsRequest _
```

#### `ListTagsResponse`

``` purescript
newtype ListTagsResponse
  = ListTagsResponse { "Tags" :: NullOrUndefined (TagList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTagsResponse _
```

#### `MacAddress`

``` purescript
newtype MacAddress
  = MacAddress String
```

##### Instances
``` purescript
Newtype MacAddress _
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

#### `MaxVolumeLimit`

``` purescript
newtype MaxVolumeLimit
  = MaxVolumeLimit Int
```

##### Instances
``` purescript
Newtype MaxVolumeLimit _
```

#### `NameInUseException`

``` purescript
newtype NameInUseException
  = NameInUseException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The name sent in the request is already in use. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype NameInUseException _
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

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource is not found. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Profile`

``` purescript
newtype Profile
  = Profile { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Address" :: NullOrUndefined (Address), "Timezone" :: NullOrUndefined (Timezone), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

<p>A room profile with attributes.</p>

##### Instances
``` purescript
Newtype Profile _
```

#### `ProfileData`

``` purescript
newtype ProfileData
  = ProfileData { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Address" :: NullOrUndefined (Address), "Timezone" :: NullOrUndefined (Timezone), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord) }
```

<p>The data of a room profile.</p>

##### Instances
``` purescript
Newtype ProfileData _
```

#### `ProfileDataList`

``` purescript
newtype ProfileDataList
  = ProfileDataList (Array ProfileData)
```

##### Instances
``` purescript
Newtype ProfileDataList _
```

#### `ProfileName`

``` purescript
newtype ProfileName
  = ProfileName String
```

##### Instances
``` purescript
Newtype ProfileName _
```

#### `ProviderCalendarId`

``` purescript
newtype ProviderCalendarId
  = ProviderCalendarId String
```

##### Instances
``` purescript
Newtype ProviderCalendarId _
```

#### `PutRoomSkillParameterRequest`

``` purescript
newtype PutRoomSkillParameterRequest
  = PutRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "RoomSkillParameter" :: RoomSkillParameter }
```

##### Instances
``` purescript
Newtype PutRoomSkillParameterRequest _
```

#### `PutRoomSkillParameterResponse`

``` purescript
newtype PutRoomSkillParameterResponse
  = PutRoomSkillParameterResponse {  }
```

##### Instances
``` purescript
Newtype PutRoomSkillParameterResponse _
```

#### `ResolveRoomRequest`

``` purescript
newtype ResolveRoomRequest
  = ResolveRoomRequest { "UserId" :: UserId, "SkillId" :: SkillId }
```

##### Instances
``` purescript
Newtype ResolveRoomRequest _
```

#### `ResolveRoomResponse`

``` purescript
newtype ResolveRoomResponse
  = ResolveRoomResponse { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "RoomSkillParameters" :: NullOrUndefined (RoomSkillParameters) }
```

##### Instances
``` purescript
Newtype ResolveRoomResponse _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (ErrorMessage), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The resource in the request is already in use. HTTP Status Code: 400</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `RevokeInvitationRequest`

``` purescript
newtype RevokeInvitationRequest
  = RevokeInvitationRequest { "UserArn" :: NullOrUndefined (Arn), "EnrollmentId" :: NullOrUndefined (EnrollmentId) }
```

##### Instances
``` purescript
Newtype RevokeInvitationRequest _
```

#### `RevokeInvitationResponse`

``` purescript
newtype RevokeInvitationResponse
  = RevokeInvitationResponse {  }
```

##### Instances
``` purescript
Newtype RevokeInvitationResponse _
```

#### `Room`

``` purescript
newtype Room
  = Room { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn) }
```

<p>A room with attributes.</p>

##### Instances
``` purescript
Newtype Room _
```

#### `RoomData`

``` purescript
newtype RoomData
  = RoomData { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName) }
```

<p>The data of a room.</p>

##### Instances
``` purescript
Newtype RoomData _
```

#### `RoomDataList`

``` purescript
newtype RoomDataList
  = RoomDataList (Array RoomData)
```

##### Instances
``` purescript
Newtype RoomDataList _
```

#### `RoomDescription`

``` purescript
newtype RoomDescription
  = RoomDescription String
```

##### Instances
``` purescript
Newtype RoomDescription _
```

#### `RoomName`

``` purescript
newtype RoomName
  = RoomName String
```

##### Instances
``` purescript
Newtype RoomName _
```

#### `RoomSkillParameter`

``` purescript
newtype RoomSkillParameter
  = RoomSkillParameter { "ParameterKey" :: RoomSkillParameterKey, "ParameterValue" :: RoomSkillParameterValue }
```

<p>A skill parameter associated with a room.</p>

##### Instances
``` purescript
Newtype RoomSkillParameter _
```

#### `RoomSkillParameterKey`

``` purescript
newtype RoomSkillParameterKey
  = RoomSkillParameterKey String
```

##### Instances
``` purescript
Newtype RoomSkillParameterKey _
```

#### `RoomSkillParameterValue`

``` purescript
newtype RoomSkillParameterValue
  = RoomSkillParameterValue String
```

##### Instances
``` purescript
Newtype RoomSkillParameterValue _
```

#### `RoomSkillParameters`

``` purescript
newtype RoomSkillParameters
  = RoomSkillParameters (Array RoomSkillParameter)
```

##### Instances
``` purescript
Newtype RoomSkillParameters _
```

#### `SearchDevicesRequest`

``` purescript
newtype SearchDevicesRequest
  = SearchDevicesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

##### Instances
``` purescript
Newtype SearchDevicesRequest _
```

#### `SearchDevicesResponse`

``` purescript
newtype SearchDevicesResponse
  = SearchDevicesResponse { "Devices" :: NullOrUndefined (DeviceDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

##### Instances
``` purescript
Newtype SearchDevicesResponse _
```

#### `SearchProfilesRequest`

``` purescript
newtype SearchProfilesRequest
  = SearchProfilesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

##### Instances
``` purescript
Newtype SearchProfilesRequest _
```

#### `SearchProfilesResponse`

``` purescript
newtype SearchProfilesResponse
  = SearchProfilesResponse { "Profiles" :: NullOrUndefined (ProfileDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

##### Instances
``` purescript
Newtype SearchProfilesResponse _
```

#### `SearchRoomsRequest`

``` purescript
newtype SearchRoomsRequest
  = SearchRoomsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

##### Instances
``` purescript
Newtype SearchRoomsRequest _
```

#### `SearchRoomsResponse`

``` purescript
newtype SearchRoomsResponse
  = SearchRoomsResponse { "Rooms" :: NullOrUndefined (RoomDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

##### Instances
``` purescript
Newtype SearchRoomsResponse _
```

#### `SearchSkillGroupsRequest`

``` purescript
newtype SearchSkillGroupsRequest
  = SearchSkillGroupsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

##### Instances
``` purescript
Newtype SearchSkillGroupsRequest _
```

#### `SearchSkillGroupsResponse`

``` purescript
newtype SearchSkillGroupsResponse
  = SearchSkillGroupsResponse { "SkillGroups" :: NullOrUndefined (SkillGroupDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

##### Instances
``` purescript
Newtype SearchSkillGroupsResponse _
```

#### `SearchUsersRequest`

``` purescript
newtype SearchUsersRequest
  = SearchUsersRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

##### Instances
``` purescript
Newtype SearchUsersRequest _
```

#### `SearchUsersResponse`

``` purescript
newtype SearchUsersResponse
  = SearchUsersResponse { "Users" :: NullOrUndefined (UserDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

##### Instances
``` purescript
Newtype SearchUsersResponse _
```

#### `SendInvitationRequest`

``` purescript
newtype SendInvitationRequest
  = SendInvitationRequest { "UserArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype SendInvitationRequest _
```

#### `SendInvitationResponse`

``` purescript
newtype SendInvitationResponse
  = SendInvitationResponse {  }
```

##### Instances
``` purescript
Newtype SendInvitationResponse _
```

#### `SkillGroup`

``` purescript
newtype SkillGroup
  = SkillGroup { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

<p>A skill group with attributes.</p>

##### Instances
``` purescript
Newtype SkillGroup _
```

#### `SkillGroupData`

``` purescript
newtype SkillGroupData
  = SkillGroupData { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

<p>The attributes of a skill group.</p>

##### Instances
``` purescript
Newtype SkillGroupData _
```

#### `SkillGroupDataList`

``` purescript
newtype SkillGroupDataList
  = SkillGroupDataList (Array SkillGroupData)
```

##### Instances
``` purescript
Newtype SkillGroupDataList _
```

#### `SkillGroupDescription`

``` purescript
newtype SkillGroupDescription
  = SkillGroupDescription String
```

##### Instances
``` purescript
Newtype SkillGroupDescription _
```

#### `SkillGroupName`

``` purescript
newtype SkillGroupName
  = SkillGroupName String
```

##### Instances
``` purescript
Newtype SkillGroupName _
```

#### `SkillId`

``` purescript
newtype SkillId
  = SkillId String
```

##### Instances
``` purescript
Newtype SkillId _
```

#### `SkillListMaxResults`

``` purescript
newtype SkillListMaxResults
  = SkillListMaxResults Int
```

##### Instances
``` purescript
Newtype SkillListMaxResults _
```

#### `SkillName`

``` purescript
newtype SkillName
  = SkillName String
```

##### Instances
``` purescript
Newtype SkillName _
```

#### `SkillSummary`

``` purescript
newtype SkillSummary
  = SkillSummary { "SkillId" :: NullOrUndefined (SkillId), "SkillName" :: NullOrUndefined (SkillName), "SupportsLinking" :: NullOrUndefined (Boolean) }
```

<p>The summary of skills.</p>

##### Instances
``` purescript
Newtype SkillSummary _
```

#### `SkillSummaryList`

``` purescript
newtype SkillSummaryList
  = SkillSummaryList (Array SkillSummary)
```

##### Instances
``` purescript
Newtype SkillSummaryList _
```

#### `SoftwareVersion`

``` purescript
newtype SoftwareVersion
  = SoftwareVersion String
```

##### Instances
``` purescript
Newtype SoftwareVersion _
```

#### `Sort`

``` purescript
newtype Sort
  = Sort { "Key" :: SortKey, "Value" :: SortValue }
```

<p>An object representing a sort criteria. </p>

##### Instances
``` purescript
Newtype Sort _
```

#### `SortKey`

``` purescript
newtype SortKey
  = SortKey String
```

##### Instances
``` purescript
Newtype SortKey _
```

#### `SortList`

``` purescript
newtype SortList
  = SortList (Array Sort)
```

##### Instances
``` purescript
Newtype SortList _
```

#### `SortValue`

``` purescript
newtype SortValue
  = SortValue String
```

##### Instances
``` purescript
Newtype SortValue _
```

#### `StartDeviceSyncRequest`

``` purescript
newtype StartDeviceSyncRequest
  = StartDeviceSyncRequest { "RoomArn" :: NullOrUndefined (Arn), "DeviceArn" :: NullOrUndefined (Arn), "Features" :: Features }
```

##### Instances
``` purescript
Newtype StartDeviceSyncRequest _
```

#### `StartDeviceSyncResponse`

``` purescript
newtype StartDeviceSyncResponse
  = StartDeviceSyncResponse {  }
```

##### Instances
``` purescript
Newtype StartDeviceSyncResponse _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>A key-value pair that can be associated with a resource. </p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "Arn" :: Arn, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

##### Instances
``` purescript
Newtype TagResourceResponse _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `TemperatureUnit`

``` purescript
newtype TemperatureUnit
  = TemperatureUnit String
```

##### Instances
``` purescript
Newtype TemperatureUnit _
```

#### `Timezone`

``` purescript
newtype Timezone
  = Timezone String
```

##### Instances
``` purescript
Newtype Timezone _
```

#### `TotalCount`

``` purescript
newtype TotalCount
  = TotalCount Int
```

##### Instances
``` purescript
Newtype TotalCount _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "Arn" :: Arn, "TagKeys" :: TagKeyList }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `UpdateDeviceRequest`

``` purescript
newtype UpdateDeviceRequest
  = UpdateDeviceRequest { "DeviceArn" :: NullOrUndefined (Arn), "DeviceName" :: NullOrUndefined (DeviceName) }
```

##### Instances
``` purescript
Newtype UpdateDeviceRequest _
```

#### `UpdateDeviceResponse`

``` purescript
newtype UpdateDeviceResponse
  = UpdateDeviceResponse {  }
```

##### Instances
``` purescript
Newtype UpdateDeviceResponse _
```

#### `UpdateProfileRequest`

``` purescript
newtype UpdateProfileRequest
  = UpdateProfileRequest { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Timezone" :: NullOrUndefined (Timezone), "Address" :: NullOrUndefined (Address), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdateProfileRequest _
```

#### `UpdateProfileResponse`

``` purescript
newtype UpdateProfileResponse
  = UpdateProfileResponse {  }
```

##### Instances
``` purescript
Newtype UpdateProfileResponse _
```

#### `UpdateRoomRequest`

``` purescript
newtype UpdateRoomRequest
  = UpdateRoomRequest { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype UpdateRoomRequest _
```

#### `UpdateRoomResponse`

``` purescript
newtype UpdateRoomResponse
  = UpdateRoomResponse {  }
```

##### Instances
``` purescript
Newtype UpdateRoomResponse _
```

#### `UpdateSkillGroupRequest`

``` purescript
newtype UpdateSkillGroupRequest
  = UpdateSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

##### Instances
``` purescript
Newtype UpdateSkillGroupRequest _
```

#### `UpdateSkillGroupResponse`

``` purescript
newtype UpdateSkillGroupResponse
  = UpdateSkillGroupResponse {  }
```

##### Instances
``` purescript
Newtype UpdateSkillGroupResponse _
```

#### `UserData`

``` purescript
newtype UserData
  = UserData { "UserArn" :: NullOrUndefined (Arn), "FirstName" :: NullOrUndefined (User_FirstName'), "LastName" :: NullOrUndefined (User_LastName'), "Email" :: NullOrUndefined (Email), "EnrollmentStatus" :: NullOrUndefined (EnrollmentStatus), "EnrollmentId" :: NullOrUndefined (EnrollmentId) }
```

<p>Information related to a user.</p>

##### Instances
``` purescript
Newtype UserData _
```

#### `UserDataList`

``` purescript
newtype UserDataList
  = UserDataList (Array UserData)
```

##### Instances
``` purescript
Newtype UserDataList _
```

#### `UserId`

``` purescript
newtype UserId
  = UserId String
```

##### Instances
``` purescript
Newtype UserId _
```

#### `WakeWord`

``` purescript
newtype WakeWord
  = WakeWord String
```

##### Instances
``` purescript
Newtype WakeWord _
```

#### `User_FirstName'`

``` purescript
newtype User_FirstName'
  = User_FirstName' String
```

##### Instances
``` purescript
Newtype User_FirstName' _
```

#### `User_LastName'`

``` purescript
newtype User_LastName'
  = User_LastName' String
```

##### Instances
``` purescript
Newtype User_LastName' _
```

#### `User_UserId'`

``` purescript
newtype User_UserId'
  = User_UserId' String
```

##### Instances
``` purescript
Newtype User_UserId' _
```


