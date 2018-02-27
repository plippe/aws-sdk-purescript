## Module AWS.CognitoIdentityServiceProvider

<p>Using the Amazon Cognito User Pools API, you can create a user pool to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.</p> <p>This API reference provides information about user pools in Amazon Cognito User Pools.</p> <p>For more information, see the Amazon Cognito Documentation.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addCustomAttributes`

``` purescript
addCustomAttributes :: forall eff. AddCustomAttributesRequest -> Aff (err :: RequestError | eff) AddCustomAttributesResponse
```

<p>Adds additional user attributes to the user pool schema.</p>

#### `adminAddUserToGroup`

``` purescript
adminAddUserToGroup :: forall eff. AdminAddUserToGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds the specified user to the specified group.</p> <p>Requires developer credentials.</p>

#### `adminConfirmSignUp`

``` purescript
adminConfirmSignUp :: forall eff. AdminConfirmSignUpRequest -> Aff (err :: RequestError | eff) AdminConfirmSignUpResponse
```

<p>Confirms user registration as an admin without using a confirmation code. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminCreateUser`

``` purescript
adminCreateUser :: forall eff. AdminCreateUserRequest -> Aff (err :: RequestError | eff) AdminCreateUserResponse
```

<p>Creates a new user in the specified user pool.</p> <p>If <code>MessageAction</code> is not set, the default is to send a welcome message via email or phone (SMS).</p> <note> <p>This message is based on a template that you configured in your call to or . This template includes your custom sign-up instructions and placeholders for user name and temporary password.</p> </note> <p>Alternatively, you can call AdminCreateUser with “SUPPRESS” for the <code>MessageAction</code> parameter, and Amazon Cognito will not send any email. </p> <p>In either case, the user will be in the <code>FORCE_CHANGE_PASSWORD</code> state until they sign in and change their password.</p> <p>AdminCreateUser requires developer credentials.</p>

#### `adminDeleteUser`

``` purescript
adminDeleteUser :: forall eff. AdminDeleteUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminDeleteUserAttributes`

``` purescript
adminDeleteUserAttributes :: forall eff. AdminDeleteUserAttributesRequest -> Aff (err :: RequestError | eff) AdminDeleteUserAttributesResponse
```

<p>Deletes the user attributes in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminDisableProviderForUser`

``` purescript
adminDisableProviderForUser :: forall eff. AdminDisableProviderForUserRequest -> Aff (err :: RequestError | eff) AdminDisableProviderForUserResponse
```

<p>Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked <code>DestinationUser</code>) signs in, they must create a new user account. See .</p> <p>This action is enabled only for admin access and requires developer credentials.</p> <p>The <code>ProviderName</code> must match the value specified when creating an IdP for the pool. </p> <p>To disable a native username + password user, the <code>ProviderName</code> value must be <code>Cognito</code> and the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code>, with the <code>ProviderAttributeValue</code> being the name that is used in the user pool for the user.</p> <p>The <code>ProviderAttributeName</code> must always be <code>Cognito_Subject</code> for social identity providers. The <code>ProviderAttributeValue</code> must always be the exact subject that was used when the user was originally linked as a source user.</p> <p>For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the <code>ProviderAttributeName</code> and <code>ProviderAttributeValue</code> must be the same values that were used for the <code>SourceUser</code> when the identities were originally linked in the call. (If the linking was done with <code>ProviderAttributeName</code> set to <code>Cognito_Subject</code>, the same applies here). However, if the user has already signed in, the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code> and <code>ProviderAttributeValue</code> must be the subject of the SAML assertion.</p>

#### `adminDisableUser`

``` purescript
adminDisableUser :: forall eff. AdminDisableUserRequest -> Aff (err :: RequestError | eff) AdminDisableUserResponse
```

<p>Disables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminEnableUser`

``` purescript
adminEnableUser :: forall eff. AdminEnableUserRequest -> Aff (err :: RequestError | eff) AdminEnableUserResponse
```

<p>Enables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminForgetDevice`

``` purescript
adminForgetDevice :: forall eff. AdminForgetDeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Forgets the device, as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminGetDevice`

``` purescript
adminGetDevice :: forall eff. AdminGetDeviceRequest -> Aff (err :: RequestError | eff) AdminGetDeviceResponse
```

<p>Gets the device, as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminGetUser`

``` purescript
adminGetUser :: forall eff. AdminGetUserRequest -> Aff (err :: RequestError | eff) AdminGetUserResponse
```

<p>Gets the specified user by user name in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminInitiateAuth`

``` purescript
adminInitiateAuth :: forall eff. AdminInitiateAuthRequest -> Aff (err :: RequestError | eff) AdminInitiateAuthResponse
```

<p>Initiates the authentication flow, as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminLinkProviderForUser`

``` purescript
adminLinkProviderForUser :: forall eff. AdminLinkProviderForUserRequest -> Aff (err :: RequestError | eff) AdminLinkProviderForUserResponse
```

<p>Links an existing user account in a user pool (<code>DestinationUser</code>) to an identity from an external identity provider (<code>SourceUser</code>) based on a specified attribute name and value from the external identity provider. This allows you to create a link from the existing user account to an external federated user identity that has not yet been used to sign in, so that the federated user identity can be used to sign in as the existing user account. </p> <p> For example, if there is an existing user with a username and password, this API links that user to a federated user identity, so that when the federated user identity is used, the user signs in as the existing user account. </p> <important> <p>Because this API allows a user with an external federated identity to sign in as an existing user in the user pool, it is critical that it only be used with external identity providers and provider attributes that have been trusted by the application owner.</p> </important> <p>See also .</p> <p>This action is enabled only for admin access and requires developer credentials.</p>

#### `adminListDevices`

``` purescript
adminListDevices :: forall eff. AdminListDevicesRequest -> Aff (err :: RequestError | eff) AdminListDevicesResponse
```

<p>Lists devices, as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminListGroupsForUser`

``` purescript
adminListGroupsForUser :: forall eff. AdminListGroupsForUserRequest -> Aff (err :: RequestError | eff) AdminListGroupsForUserResponse
```

<p>Lists the groups that the user belongs to.</p> <p>Requires developer credentials.</p>

#### `adminListUserAuthEvents`

``` purescript
adminListUserAuthEvents :: forall eff. AdminListUserAuthEventsRequest -> Aff (err :: RequestError | eff) AdminListUserAuthEventsResponse
```

<p>Lists a history of user activity and any risks detected as part of Amazon Cognito advanced security.</p>

#### `adminRemoveUserFromGroup`

``` purescript
adminRemoveUserFromGroup :: forall eff. AdminRemoveUserFromGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified user from the specified group.</p> <p>Requires developer credentials.</p>

#### `adminResetUserPassword`

``` purescript
adminResetUserPassword :: forall eff. AdminResetUserPasswordRequest -> Aff (err :: RequestError | eff) AdminResetUserPasswordResponse
```

<p>Resets the specified user's password in a user pool as an administrator. Works on any user.</p> <p>When a developer calls this API, the current password is invalidated, so it must be changed. If a user tries to sign in after the API is called, the app will get a PasswordResetRequiredException exception back and should direct the user down the flow to reset the password, which is the same as the forgot password flow. In addition, if the user pool has phone verification selected and a verified phone number exists for the user, or if email verification is selected and a verified email exists for the user, calling this API will also result in sending a message to the end user with the code to change their password.</p> <p>Requires developer credentials.</p>

#### `adminRespondToAuthChallenge`

``` purescript
adminRespondToAuthChallenge :: forall eff. AdminRespondToAuthChallengeRequest -> Aff (err :: RequestError | eff) AdminRespondToAuthChallengeResponse
```

<p>Responds to an authentication challenge, as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminSetUserMFAPreference`

``` purescript
adminSetUserMFAPreference :: forall eff. AdminSetUserMFAPreferenceRequest -> Aff (err :: RequestError | eff) AdminSetUserMFAPreferenceResponse
```

<p>Sets the user's multi-factor authentication (MFA) preference.</p>

#### `adminSetUserSettings`

``` purescript
adminSetUserSettings :: forall eff. AdminSetUserSettingsRequest -> Aff (err :: RequestError | eff) AdminSetUserSettingsResponse
```

<p>Sets all the user settings for a specified user name. Works on any user.</p> <p>Requires developer credentials.</p>

#### `adminUpdateAuthEventFeedback`

``` purescript
adminUpdateAuthEventFeedback :: forall eff. AdminUpdateAuthEventFeedbackRequest -> Aff (err :: RequestError | eff) AdminUpdateAuthEventFeedbackResponse
```

<p>Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>

#### `adminUpdateDeviceStatus`

``` purescript
adminUpdateDeviceStatus :: forall eff. AdminUpdateDeviceStatusRequest -> Aff (err :: RequestError | eff) AdminUpdateDeviceStatusResponse
```

<p>Updates the device status as an administrator.</p> <p>Requires developer credentials.</p>

#### `adminUpdateUserAttributes`

``` purescript
adminUpdateUserAttributes :: forall eff. AdminUpdateUserAttributesRequest -> Aff (err :: RequestError | eff) AdminUpdateUserAttributesResponse
```

<p>Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.</p> <p>For custom attributes, you must prepend the <code>custom:</code> prefix to the attribute name.</p> <p>In addition to updating user attributes, this API can also be used to mark phone and email as verified.</p> <p>Requires developer credentials.</p>

#### `adminUserGlobalSignOut`

``` purescript
adminUserGlobalSignOut :: forall eff. AdminUserGlobalSignOutRequest -> Aff (err :: RequestError | eff) AdminUserGlobalSignOutResponse
```

<p>Signs out users from all devices, as an administrator.</p> <p>Requires developer credentials.</p>

#### `associateSoftwareToken`

``` purescript
associateSoftwareToken :: forall eff. AssociateSoftwareTokenRequest -> Aff (err :: RequestError | eff) AssociateSoftwareTokenResponse
```

<p>Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.</p>

#### `changePassword`

``` purescript
changePassword :: forall eff. ChangePasswordRequest -> Aff (err :: RequestError | eff) ChangePasswordResponse
```

<p>Changes the password for a specified user in a user pool.</p>

#### `confirmDevice`

``` purescript
confirmDevice :: forall eff. ConfirmDeviceRequest -> Aff (err :: RequestError | eff) ConfirmDeviceResponse
```

<p>Confirms tracking of the device. This API call is the call that begins device tracking.</p>

#### `confirmForgotPassword`

``` purescript
confirmForgotPassword :: forall eff. ConfirmForgotPasswordRequest -> Aff (err :: RequestError | eff) ConfirmForgotPasswordResponse
```

<p>Allows a user to enter a confirmation code to reset a forgotten password.</p>

#### `confirmSignUp`

``` purescript
confirmSignUp :: forall eff. ConfirmSignUpRequest -> Aff (err :: RequestError | eff) ConfirmSignUpResponse
```

<p>Confirms registration of a user and handles the existing alias from a previous user.</p>

#### `createGroup`

``` purescript
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: RequestError | eff) CreateGroupResponse
```

<p>Creates a new group in the specified user pool.</p> <p>Requires developer credentials.</p>

#### `createIdentityProvider`

``` purescript
createIdentityProvider :: forall eff. CreateIdentityProviderRequest -> Aff (err :: RequestError | eff) CreateIdentityProviderResponse
```

<p>Creates an identity provider for a user pool.</p>

#### `createResourceServer`

``` purescript
createResourceServer :: forall eff. CreateResourceServerRequest -> Aff (err :: RequestError | eff) CreateResourceServerResponse
```

<p>Creates a new OAuth2.0 resource server and defines custom scopes in it.</p>

#### `createUserImportJob`

``` purescript
createUserImportJob :: forall eff. CreateUserImportJobRequest -> Aff (err :: RequestError | eff) CreateUserImportJobResponse
```

<p>Creates the user import job.</p>

#### `createUserPool`

``` purescript
createUserPool :: forall eff. CreateUserPoolRequest -> Aff (err :: RequestError | eff) CreateUserPoolResponse
```

<p>Creates a new Amazon Cognito user pool and sets the password policy for the pool.</p>

#### `createUserPoolClient`

``` purescript
createUserPoolClient :: forall eff. CreateUserPoolClientRequest -> Aff (err :: RequestError | eff) CreateUserPoolClientResponse
```

<p>Creates the user pool client.</p>

#### `createUserPoolDomain`

``` purescript
createUserPoolDomain :: forall eff. CreateUserPoolDomainRequest -> Aff (err :: RequestError | eff) CreateUserPoolDomainResponse
```

<p>Creates a new domain for a user pool.</p>

#### `deleteGroup`

``` purescript
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a group. Currently only groups with no members can be deleted.</p> <p>Requires developer credentials.</p>

#### `deleteIdentityProvider`

``` purescript
deleteIdentityProvider :: forall eff. DeleteIdentityProviderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an identity provider for a user pool.</p>

#### `deleteResourceServer`

``` purescript
deleteResourceServer :: forall eff. DeleteResourceServerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a resource server.</p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Allows a user to delete himself or herself.</p>

#### `deleteUserAttributes`

``` purescript
deleteUserAttributes :: forall eff. DeleteUserAttributesRequest -> Aff (err :: RequestError | eff) DeleteUserAttributesResponse
```

<p>Deletes the attributes for a user.</p>

#### `deleteUserPool`

``` purescript
deleteUserPool :: forall eff. DeleteUserPoolRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified Amazon Cognito user pool.</p>

#### `deleteUserPoolClient`

``` purescript
deleteUserPoolClient :: forall eff. DeleteUserPoolClientRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Allows the developer to delete the user pool client.</p>

#### `deleteUserPoolDomain`

``` purescript
deleteUserPoolDomain :: forall eff. DeleteUserPoolDomainRequest -> Aff (err :: RequestError | eff) DeleteUserPoolDomainResponse
```

<p>Deletes a domain for a user pool.</p>

#### `describeIdentityProvider`

``` purescript
describeIdentityProvider :: forall eff. DescribeIdentityProviderRequest -> Aff (err :: RequestError | eff) DescribeIdentityProviderResponse
```

<p>Gets information about a specific identity provider.</p>

#### `describeResourceServer`

``` purescript
describeResourceServer :: forall eff. DescribeResourceServerRequest -> Aff (err :: RequestError | eff) DescribeResourceServerResponse
```

<p>Describes a resource server.</p>

#### `describeRiskConfiguration`

``` purescript
describeRiskConfiguration :: forall eff. DescribeRiskConfigurationRequest -> Aff (err :: RequestError | eff) DescribeRiskConfigurationResponse
```

<p>Describes the risk configuration.</p>

#### `describeUserImportJob`

``` purescript
describeUserImportJob :: forall eff. DescribeUserImportJobRequest -> Aff (err :: RequestError | eff) DescribeUserImportJobResponse
```

<p>Describes the user import job.</p>

#### `describeUserPool`

``` purescript
describeUserPool :: forall eff. DescribeUserPoolRequest -> Aff (err :: RequestError | eff) DescribeUserPoolResponse
```

<p>Returns the configuration information and metadata of the specified user pool.</p>

#### `describeUserPoolClient`

``` purescript
describeUserPoolClient :: forall eff. DescribeUserPoolClientRequest -> Aff (err :: RequestError | eff) DescribeUserPoolClientResponse
```

<p>Client method for returning the configuration information and metadata of the specified user pool client.</p>

#### `describeUserPoolDomain`

``` purescript
describeUserPoolDomain :: forall eff. DescribeUserPoolDomainRequest -> Aff (err :: RequestError | eff) DescribeUserPoolDomainResponse
```

<p>Gets information about a domain.</p>

#### `forgetDevice`

``` purescript
forgetDevice :: forall eff. ForgetDeviceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Forgets the specified device.</p>

#### `forgotPassword`

``` purescript
forgotPassword :: forall eff. ForgotPasswordRequest -> Aff (err :: RequestError | eff) ForgotPasswordResponse
```

<p>Calling this API causes a message to be sent to the end user with a confirmation code that is required to change the user's password. For the <code>Username</code> parameter, you can use the username or user alias. If a verified phone number exists for the user, the confirmation code is sent to the phone number. Otherwise, if a verified email exists, the confirmation code is sent to the email. If neither a verified phone number nor a verified email exists, <code>InvalidParameterException</code> is thrown. To use the confirmation code for resetting the password, call .</p>

#### `getCSVHeader`

``` purescript
getCSVHeader :: forall eff. GetCSVHeaderRequest -> Aff (err :: RequestError | eff) GetCSVHeaderResponse
```

<p>Gets the header information for the .csv file to be used as input for the user import job.</p>

#### `getDevice`

``` purescript
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: RequestError | eff) GetDeviceResponse
```

<p>Gets the device.</p>

#### `getGroup`

``` purescript
getGroup :: forall eff. GetGroupRequest -> Aff (err :: RequestError | eff) GetGroupResponse
```

<p>Gets a group.</p> <p>Requires developer credentials.</p>

#### `getIdentityProviderByIdentifier`

``` purescript
getIdentityProviderByIdentifier :: forall eff. GetIdentityProviderByIdentifierRequest -> Aff (err :: RequestError | eff) GetIdentityProviderByIdentifierResponse
```

<p>Gets the specified identity provider.</p>

#### `getSigningCertificate`

``` purescript
getSigningCertificate :: forall eff. GetSigningCertificateRequest -> Aff (err :: RequestError | eff) GetSigningCertificateResponse
```

<p>This method takes a user pool ID, and returns the signing certificate.</p>

#### `getUICustomization`

``` purescript
getUICustomization :: forall eff. GetUICustomizationRequest -> Aff (err :: RequestError | eff) GetUICustomizationResponse
```

<p>Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app <code>clientId</code> will be <code>ALL</code>), then that is returned. If nothing is present, then an empty shape is returned.</p>

#### `getUser`

``` purescript
getUser :: forall eff. GetUserRequest -> Aff (err :: RequestError | eff) GetUserResponse
```

<p>Gets the user attributes and metadata for a user.</p>

#### `getUserAttributeVerificationCode`

``` purescript
getUserAttributeVerificationCode :: forall eff. GetUserAttributeVerificationCodeRequest -> Aff (err :: RequestError | eff) GetUserAttributeVerificationCodeResponse
```

<p>Gets the user attribute verification code for the specified attribute name.</p>

#### `getUserPoolMfaConfig`

``` purescript
getUserPoolMfaConfig :: forall eff. GetUserPoolMfaConfigRequest -> Aff (err :: RequestError | eff) GetUserPoolMfaConfigResponse
```

<p>Gets the user pool multi-factor authentication (MFA) configuration.</p>

#### `globalSignOut`

``` purescript
globalSignOut :: forall eff. GlobalSignOutRequest -> Aff (err :: RequestError | eff) GlobalSignOutResponse
```

<p>Signs out users from all devices.</p>

#### `initiateAuth`

``` purescript
initiateAuth :: forall eff. InitiateAuthRequest -> Aff (err :: RequestError | eff) InitiateAuthResponse
```

<p>Initiates the authentication flow.</p>

#### `listDevices`

``` purescript
listDevices :: forall eff. ListDevicesRequest -> Aff (err :: RequestError | eff) ListDevicesResponse
```

<p>Lists the devices.</p>

#### `listGroups`

``` purescript
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: RequestError | eff) ListGroupsResponse
```

<p>Lists the groups associated with a user pool.</p> <p>Requires developer credentials.</p>

#### `listIdentityProviders`

``` purescript
listIdentityProviders :: forall eff. ListIdentityProvidersRequest -> Aff (err :: RequestError | eff) ListIdentityProvidersResponse
```

<p>Lists information about all identity providers for a user pool.</p>

#### `listResourceServers`

``` purescript
listResourceServers :: forall eff. ListResourceServersRequest -> Aff (err :: RequestError | eff) ListResourceServersResponse
```

<p>Lists the resource servers for a user pool.</p>

#### `listUserImportJobs`

``` purescript
listUserImportJobs :: forall eff. ListUserImportJobsRequest -> Aff (err :: RequestError | eff) ListUserImportJobsResponse
```

<p>Lists the user import jobs.</p>

#### `listUserPoolClients`

``` purescript
listUserPoolClients :: forall eff. ListUserPoolClientsRequest -> Aff (err :: RequestError | eff) ListUserPoolClientsResponse
```

<p>Lists the clients that have been created for the specified user pool.</p>

#### `listUserPools`

``` purescript
listUserPools :: forall eff. ListUserPoolsRequest -> Aff (err :: RequestError | eff) ListUserPoolsResponse
```

<p>Lists the user pools associated with an AWS account.</p>

#### `listUsers`

``` purescript
listUsers :: forall eff. ListUsersRequest -> Aff (err :: RequestError | eff) ListUsersResponse
```

<p>Lists the users in the Amazon Cognito user pool.</p>

#### `listUsersInGroup`

``` purescript
listUsersInGroup :: forall eff. ListUsersInGroupRequest -> Aff (err :: RequestError | eff) ListUsersInGroupResponse
```

<p>Lists the users in the specified group.</p> <p>Requires developer credentials.</p>

#### `resendConfirmationCode`

``` purescript
resendConfirmationCode :: forall eff. ResendConfirmationCodeRequest -> Aff (err :: RequestError | eff) ResendConfirmationCodeResponse
```

<p>Resends the confirmation (for confirmation of registration) to a specific user in the user pool.</p>

#### `respondToAuthChallenge`

``` purescript
respondToAuthChallenge :: forall eff. RespondToAuthChallengeRequest -> Aff (err :: RequestError | eff) RespondToAuthChallengeResponse
```

<p>Responds to the authentication challenge.</p>

#### `setRiskConfiguration`

``` purescript
setRiskConfiguration :: forall eff. SetRiskConfigurationRequest -> Aff (err :: RequestError | eff) SetRiskConfigurationResponse
```

<p>Configures actions on detected risks. To delete the risk configuration for <code>UserPoolId</code> or <code>ClientId</code>, pass null values for all four configuration types.</p> <p>To enable Amazon Cognito advanced security features, update the user pool to include the <code>UserPoolAddOns</code> key<code>AdvancedSecurityMode</code>.</p> <p>See .</p>

#### `setUICustomization`

``` purescript
setUICustomization :: forall eff. SetUICustomizationRequest -> Aff (err :: RequestError | eff) SetUICustomizationResponse
```

<p>Sets the UI customization information for a user pool's built-in app UI.</p> <p>You can specify app UI customization settings for a single client (with a specific <code>clientId</code>) or for all clients (by setting the <code>clientId</code> to <code>ALL</code>). If you specify <code>ALL</code>, the default configuration will be used for every client that has no UI customization set previously. If you specify UI customization settings for a particular client, it will no longer fall back to the <code>ALL</code> configuration. </p> <note> <p>To use this API, your user pool must have a domain associated with it. Otherwise, there is no place to host the app's pages, and the service will throw an error.</p> </note>

#### `setUserMFAPreference`

``` purescript
setUserMFAPreference :: forall eff. SetUserMFAPreferenceRequest -> Aff (err :: RequestError | eff) SetUserMFAPreferenceResponse
```

<p>Set the user's multi-factor authentication (MFA) method preference.</p>

#### `setUserPoolMfaConfig`

``` purescript
setUserPoolMfaConfig :: forall eff. SetUserPoolMfaConfigRequest -> Aff (err :: RequestError | eff) SetUserPoolMfaConfigResponse
```

<p>Set the user pool MFA configuration.</p>

#### `setUserSettings`

``` purescript
setUserSettings :: forall eff. SetUserSettingsRequest -> Aff (err :: RequestError | eff) SetUserSettingsResponse
```

<p>Sets the user settings like multi-factor authentication (MFA). If MFA is to be removed for a particular attribute pass the attribute with code delivery as null. If null list is passed, all MFA options are removed.</p>

#### `signUp`

``` purescript
signUp :: forall eff. SignUpRequest -> Aff (err :: RequestError | eff) SignUpResponse
```

<p>Registers the user in the specified user pool and creates a user name, password, and user attributes.</p>

#### `startUserImportJob`

``` purescript
startUserImportJob :: forall eff. StartUserImportJobRequest -> Aff (err :: RequestError | eff) StartUserImportJobResponse
```

<p>Starts the user import.</p>

#### `stopUserImportJob`

``` purescript
stopUserImportJob :: forall eff. StopUserImportJobRequest -> Aff (err :: RequestError | eff) StopUserImportJobResponse
```

<p>Stops the user import job.</p>

#### `updateAuthEventFeedback`

``` purescript
updateAuthEventFeedback :: forall eff. UpdateAuthEventFeedbackRequest -> Aff (err :: RequestError | eff) UpdateAuthEventFeedbackResponse
```

<p>Provides the feedback for an authentication event whether it was from a valid user or not. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>

#### `updateDeviceStatus`

``` purescript
updateDeviceStatus :: forall eff. UpdateDeviceStatusRequest -> Aff (err :: RequestError | eff) UpdateDeviceStatusResponse
```

<p>Updates the device status.</p>

#### `updateGroup`

``` purescript
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: RequestError | eff) UpdateGroupResponse
```

<p>Updates the specified group with the specified attributes.</p> <p>Requires developer credentials.</p>

#### `updateIdentityProvider`

``` purescript
updateIdentityProvider :: forall eff. UpdateIdentityProviderRequest -> Aff (err :: RequestError | eff) UpdateIdentityProviderResponse
```

<p>Updates identity provider information for a user pool.</p>

#### `updateResourceServer`

``` purescript
updateResourceServer :: forall eff. UpdateResourceServerRequest -> Aff (err :: RequestError | eff) UpdateResourceServerResponse
```

<p>Updates the name and scopes of resource server. All other fields are read-only.</p>

#### `updateUserAttributes`

``` purescript
updateUserAttributes :: forall eff. UpdateUserAttributesRequest -> Aff (err :: RequestError | eff) UpdateUserAttributesResponse
```

<p>Allows a user to update a specific attribute (one at a time).</p>

#### `updateUserPool`

``` purescript
updateUserPool :: forall eff. UpdateUserPoolRequest -> Aff (err :: RequestError | eff) UpdateUserPoolResponse
```

<p>Updates the specified user pool with the specified attributes.</p>

#### `updateUserPoolClient`

``` purescript
updateUserPoolClient :: forall eff. UpdateUserPoolClientRequest -> Aff (err :: RequestError | eff) UpdateUserPoolClientResponse
```

<p>Allows the developer to update the specified user pool client and password policy.</p>

#### `verifySoftwareToken`

``` purescript
verifySoftwareToken :: forall eff. VerifySoftwareTokenRequest -> Aff (err :: RequestError | eff) VerifySoftwareTokenResponse
```

<p>Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful,</p>

#### `verifyUserAttribute`

``` purescript
verifyUserAttribute :: forall eff. VerifyUserAttributeRequest -> Aff (err :: RequestError | eff) VerifyUserAttributeResponse
```

<p>Verifies the specified user attributes in the user pool.</p>

#### `AWSAccountIdType`

``` purescript
newtype AWSAccountIdType
  = AWSAccountIdType String
```

##### Instances
``` purescript
Newtype AWSAccountIdType _
```

#### `AccountTakeoverActionNotifyType`

``` purescript
newtype AccountTakeoverActionNotifyType
  = AccountTakeoverActionNotifyType Boolean
```

##### Instances
``` purescript
Newtype AccountTakeoverActionNotifyType _
```

#### `AccountTakeoverActionType`

``` purescript
newtype AccountTakeoverActionType
  = AccountTakeoverActionType { "Notify" :: AccountTakeoverActionNotifyType, "EventAction" :: AccountTakeoverEventActionType }
```

<p>Account takeover action type.</p>

##### Instances
``` purescript
Newtype AccountTakeoverActionType _
```

#### `AccountTakeoverActionsType`

``` purescript
newtype AccountTakeoverActionsType
  = AccountTakeoverActionsType { "LowAction" :: NullOrUndefined (AccountTakeoverActionType), "MediumAction" :: NullOrUndefined (AccountTakeoverActionType), "HighAction" :: NullOrUndefined (AccountTakeoverActionType) }
```

<p>Account takeover actions type.</p>

##### Instances
``` purescript
Newtype AccountTakeoverActionsType _
```

#### `AccountTakeoverEventActionType`

``` purescript
newtype AccountTakeoverEventActionType
  = AccountTakeoverEventActionType String
```

##### Instances
``` purescript
Newtype AccountTakeoverEventActionType _
```

#### `AccountTakeoverRiskConfigurationType`

``` purescript
newtype AccountTakeoverRiskConfigurationType
  = AccountTakeoverRiskConfigurationType { "NotifyConfiguration" :: NullOrUndefined (NotifyConfigurationType), "Actions" :: AccountTakeoverActionsType }
```

<p>Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.</p>

##### Instances
``` purescript
Newtype AccountTakeoverRiskConfigurationType _
```

#### `AddCustomAttributesRequest`

``` purescript
newtype AddCustomAttributesRequest
  = AddCustomAttributesRequest { "UserPoolId" :: UserPoolIdType, "CustomAttributes" :: CustomAttributesListType }
```

<p>Represents the request to add custom attributes.</p>

##### Instances
``` purescript
Newtype AddCustomAttributesRequest _
```

#### `AddCustomAttributesResponse`

``` purescript
newtype AddCustomAttributesResponse
  = AddCustomAttributesResponse {  }
```

<p>Represents the response from the server for the request to add custom attributes.</p>

##### Instances
``` purescript
Newtype AddCustomAttributesResponse _
```

#### `AdminAddUserToGroupRequest`

``` purescript
newtype AdminAddUserToGroupRequest
  = AdminAddUserToGroupRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "GroupName" :: GroupNameType }
```

##### Instances
``` purescript
Newtype AdminAddUserToGroupRequest _
```

#### `AdminConfirmSignUpRequest`

``` purescript
newtype AdminConfirmSignUpRequest
  = AdminConfirmSignUpRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to confirm user registration.</p>

##### Instances
``` purescript
Newtype AdminConfirmSignUpRequest _
```

#### `AdminConfirmSignUpResponse`

``` purescript
newtype AdminConfirmSignUpResponse
  = AdminConfirmSignUpResponse {  }
```

<p>Represents the response from the server for the request to confirm registration.</p>

##### Instances
``` purescript
Newtype AdminConfirmSignUpResponse _
```

#### `AdminCreateUserConfigType`

``` purescript
newtype AdminCreateUserConfigType
  = AdminCreateUserConfigType { "AllowAdminCreateUserOnly" :: NullOrUndefined (BooleanType), "UnusedAccountValidityDays" :: NullOrUndefined (AdminCreateUserUnusedAccountValidityDaysType), "InviteMessageTemplate" :: NullOrUndefined (MessageTemplateType) }
```

<p>The configuration for creating a new user profile.</p>

##### Instances
``` purescript
Newtype AdminCreateUserConfigType _
```

#### `AdminCreateUserRequest`

``` purescript
newtype AdminCreateUserRequest
  = AdminCreateUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributes" :: NullOrUndefined (AttributeListType), "ValidationData" :: NullOrUndefined (AttributeListType), "TemporaryPassword" :: NullOrUndefined (PasswordType), "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation), "MessageAction" :: NullOrUndefined (MessageActionType), "DesiredDeliveryMediums" :: NullOrUndefined (DeliveryMediumListType) }
```

<p>Represents the request to create a user in the specified user pool.</p>

##### Instances
``` purescript
Newtype AdminCreateUserRequest _
```

#### `AdminCreateUserResponse`

``` purescript
newtype AdminCreateUserResponse
  = AdminCreateUserResponse { "User" :: NullOrUndefined (UserType) }
```

<p>Represents the response from the server to the request to create the user.</p>

##### Instances
``` purescript
Newtype AdminCreateUserResponse _
```

#### `AdminCreateUserUnusedAccountValidityDaysType`

``` purescript
newtype AdminCreateUserUnusedAccountValidityDaysType
  = AdminCreateUserUnusedAccountValidityDaysType Int
```

##### Instances
``` purescript
Newtype AdminCreateUserUnusedAccountValidityDaysType _
```

#### `AdminDeleteUserAttributesRequest`

``` purescript
newtype AdminDeleteUserAttributesRequest
  = AdminDeleteUserAttributesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributeNames" :: AttributeNameListType }
```

<p>Represents the request to delete user attributes as an administrator.</p>

##### Instances
``` purescript
Newtype AdminDeleteUserAttributesRequest _
```

#### `AdminDeleteUserAttributesResponse`

``` purescript
newtype AdminDeleteUserAttributesResponse
  = AdminDeleteUserAttributesResponse {  }
```

<p>Represents the response received from the server for a request to delete user attributes.</p>

##### Instances
``` purescript
Newtype AdminDeleteUserAttributesResponse _
```

#### `AdminDeleteUserRequest`

``` purescript
newtype AdminDeleteUserRequest
  = AdminDeleteUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to delete a user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminDeleteUserRequest _
```

#### `AdminDisableProviderForUserRequest`

``` purescript
newtype AdminDisableProviderForUserRequest
  = AdminDisableProviderForUserRequest { "UserPoolId" :: StringType, "User" :: ProviderUserIdentifierType }
```

##### Instances
``` purescript
Newtype AdminDisableProviderForUserRequest _
```

#### `AdminDisableProviderForUserResponse`

``` purescript
newtype AdminDisableProviderForUserResponse
  = AdminDisableProviderForUserResponse {  }
```

##### Instances
``` purescript
Newtype AdminDisableProviderForUserResponse _
```

#### `AdminDisableUserRequest`

``` purescript
newtype AdminDisableUserRequest
  = AdminDisableUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to disable any user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminDisableUserRequest _
```

#### `AdminDisableUserResponse`

``` purescript
newtype AdminDisableUserResponse
  = AdminDisableUserResponse {  }
```

<p>Represents the response received from the server to disable the user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminDisableUserResponse _
```

#### `AdminEnableUserRequest`

``` purescript
newtype AdminEnableUserRequest
  = AdminEnableUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request that enables the user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminEnableUserRequest _
```

#### `AdminEnableUserResponse`

``` purescript
newtype AdminEnableUserResponse
  = AdminEnableUserResponse {  }
```

<p>Represents the response from the server for the request to enable a user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminEnableUserResponse _
```

#### `AdminForgetDeviceRequest`

``` purescript
newtype AdminForgetDeviceRequest
  = AdminForgetDeviceRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "DeviceKey" :: DeviceKeyType }
```

<p>Sends the forgot device request, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminForgetDeviceRequest _
```

#### `AdminGetDeviceRequest`

``` purescript
newtype AdminGetDeviceRequest
  = AdminGetDeviceRequest { "DeviceKey" :: DeviceKeyType, "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to get the device, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminGetDeviceRequest _
```

#### `AdminGetDeviceResponse`

``` purescript
newtype AdminGetDeviceResponse
  = AdminGetDeviceResponse { "Device" :: DeviceType }
```

<p>Gets the device response, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminGetDeviceResponse _
```

#### `AdminGetUserRequest`

``` purescript
newtype AdminGetUserRequest
  = AdminGetUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to get the specified user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminGetUserRequest _
```

#### `AdminGetUserResponse`

``` purescript
newtype AdminGetUserResponse
  = AdminGetUserResponse { "Username" :: UsernameType, "UserAttributes" :: NullOrUndefined (AttributeListType), "UserCreateDate" :: NullOrUndefined (DateType), "UserLastModifiedDate" :: NullOrUndefined (DateType), "Enabled" :: NullOrUndefined (BooleanType), "UserStatus" :: NullOrUndefined (UserStatusType), "MFAOptions" :: NullOrUndefined (MFAOptionListType), "PreferredMfaSetting" :: NullOrUndefined (StringType), "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType) }
```

<p>Represents the response from the server from the request to get the specified user as an administrator.</p>

##### Instances
``` purescript
Newtype AdminGetUserResponse _
```

#### `AdminInitiateAuthRequest`

``` purescript
newtype AdminInitiateAuthRequest
  = AdminInitiateAuthRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "AuthFlow" :: AuthFlowType, "AuthParameters" :: NullOrUndefined (AuthParametersType), "ClientMetadata" :: NullOrUndefined (ClientMetadataType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "ContextData" :: NullOrUndefined (ContextDataType) }
```

<p>Initiates the authorization request, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminInitiateAuthRequest _
```

#### `AdminInitiateAuthResponse`

``` purescript
newtype AdminInitiateAuthResponse
  = AdminInitiateAuthResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Initiates the authentication response, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminInitiateAuthResponse _
```

#### `AdminLinkProviderForUserRequest`

``` purescript
newtype AdminLinkProviderForUserRequest
  = AdminLinkProviderForUserRequest { "UserPoolId" :: StringType, "DestinationUser" :: ProviderUserIdentifierType, "SourceUser" :: ProviderUserIdentifierType }
```

##### Instances
``` purescript
Newtype AdminLinkProviderForUserRequest _
```

#### `AdminLinkProviderForUserResponse`

``` purescript
newtype AdminLinkProviderForUserResponse
  = AdminLinkProviderForUserResponse {  }
```

##### Instances
``` purescript
Newtype AdminLinkProviderForUserResponse _
```

#### `AdminListDevicesRequest`

``` purescript
newtype AdminListDevicesRequest
  = AdminListDevicesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the request to list devices, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminListDevicesRequest _
```

#### `AdminListDevicesResponse`

``` purescript
newtype AdminListDevicesResponse
  = AdminListDevicesResponse { "Devices" :: NullOrUndefined (DeviceListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Lists the device's response, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminListDevicesResponse _
```

#### `AdminListGroupsForUserRequest`

``` purescript
newtype AdminListGroupsForUserRequest
  = AdminListGroupsForUserRequest { "Username" :: UsernameType, "UserPoolId" :: UserPoolIdType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype AdminListGroupsForUserRequest _
```

#### `AdminListGroupsForUserResponse`

``` purescript
newtype AdminListGroupsForUserResponse
  = AdminListGroupsForUserResponse { "Groups" :: NullOrUndefined (GroupListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype AdminListGroupsForUserResponse _
```

#### `AdminListUserAuthEventsRequest`

``` purescript
newtype AdminListUserAuthEventsRequest
  = AdminListUserAuthEventsRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "MaxResults" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype AdminListUserAuthEventsRequest _
```

#### `AdminListUserAuthEventsResponse`

``` purescript
newtype AdminListUserAuthEventsResponse
  = AdminListUserAuthEventsResponse { "AuthEvents" :: NullOrUndefined (AuthEventsType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype AdminListUserAuthEventsResponse _
```

#### `AdminRemoveUserFromGroupRequest`

``` purescript
newtype AdminRemoveUserFromGroupRequest
  = AdminRemoveUserFromGroupRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "GroupName" :: GroupNameType }
```

##### Instances
``` purescript
Newtype AdminRemoveUserFromGroupRequest _
```

#### `AdminResetUserPasswordRequest`

``` purescript
newtype AdminResetUserPasswordRequest
  = AdminResetUserPasswordRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to reset a user's password as an administrator.</p>

##### Instances
``` purescript
Newtype AdminResetUserPasswordRequest _
```

#### `AdminResetUserPasswordResponse`

``` purescript
newtype AdminResetUserPasswordResponse
  = AdminResetUserPasswordResponse {  }
```

<p>Represents the response from the server to reset a user password as an administrator.</p>

##### Instances
``` purescript
Newtype AdminResetUserPasswordResponse _
```

#### `AdminRespondToAuthChallengeRequest`

``` purescript
newtype AdminRespondToAuthChallengeRequest
  = AdminRespondToAuthChallengeRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "ChallengeName" :: ChallengeNameType, "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType), "Session" :: NullOrUndefined (SessionType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "ContextData" :: NullOrUndefined (ContextDataType) }
```

<p>The request to respond to the authentication challenge, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminRespondToAuthChallengeRequest _
```

#### `AdminRespondToAuthChallengeResponse`

``` purescript
newtype AdminRespondToAuthChallengeResponse
  = AdminRespondToAuthChallengeResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Responds to the authentication challenge, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminRespondToAuthChallengeResponse _
```

#### `AdminSetUserMFAPreferenceRequest`

``` purescript
newtype AdminSetUserMFAPreferenceRequest
  = AdminSetUserMFAPreferenceRequest { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType), "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType), "Username" :: UsernameType, "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype AdminSetUserMFAPreferenceRequest _
```

#### `AdminSetUserMFAPreferenceResponse`

``` purescript
newtype AdminSetUserMFAPreferenceResponse
  = AdminSetUserMFAPreferenceResponse {  }
```

##### Instances
``` purescript
Newtype AdminSetUserMFAPreferenceResponse _
```

#### `AdminSetUserSettingsRequest`

``` purescript
newtype AdminSetUserSettingsRequest
  = AdminSetUserSettingsRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "MFAOptions" :: MFAOptionListType }
```

<p>Represents the request to set user settings as an administrator.</p>

##### Instances
``` purescript
Newtype AdminSetUserSettingsRequest _
```

#### `AdminSetUserSettingsResponse`

``` purescript
newtype AdminSetUserSettingsResponse
  = AdminSetUserSettingsResponse {  }
```

<p>Represents the response from the server to set user settings as an administrator.</p>

##### Instances
``` purescript
Newtype AdminSetUserSettingsResponse _
```

#### `AdminUpdateAuthEventFeedbackRequest`

``` purescript
newtype AdminUpdateAuthEventFeedbackRequest
  = AdminUpdateAuthEventFeedbackRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "EventId" :: EventIdType, "FeedbackValue" :: FeedbackValueType }
```

##### Instances
``` purescript
Newtype AdminUpdateAuthEventFeedbackRequest _
```

#### `AdminUpdateAuthEventFeedbackResponse`

``` purescript
newtype AdminUpdateAuthEventFeedbackResponse
  = AdminUpdateAuthEventFeedbackResponse {  }
```

##### Instances
``` purescript
Newtype AdminUpdateAuthEventFeedbackResponse _
```

#### `AdminUpdateDeviceStatusRequest`

``` purescript
newtype AdminUpdateDeviceStatusRequest
  = AdminUpdateDeviceStatusRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "DeviceKey" :: DeviceKeyType, "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType) }
```

<p>The request to update the device status, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUpdateDeviceStatusRequest _
```

#### `AdminUpdateDeviceStatusResponse`

``` purescript
newtype AdminUpdateDeviceStatusResponse
  = AdminUpdateDeviceStatusResponse {  }
```

<p>The status response from the request to update the device, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUpdateDeviceStatusResponse _
```

#### `AdminUpdateUserAttributesRequest`

``` purescript
newtype AdminUpdateUserAttributesRequest
  = AdminUpdateUserAttributesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributes" :: AttributeListType }
```

<p>Represents the request to update the user's attributes as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUpdateUserAttributesRequest _
```

#### `AdminUpdateUserAttributesResponse`

``` purescript
newtype AdminUpdateUserAttributesResponse
  = AdminUpdateUserAttributesResponse {  }
```

<p>Represents the response from the server for the request to update user attributes as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUpdateUserAttributesResponse _
```

#### `AdminUserGlobalSignOutRequest`

``` purescript
newtype AdminUserGlobalSignOutRequest
  = AdminUserGlobalSignOutRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>The request to sign out of all devices, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUserGlobalSignOutRequest _
```

#### `AdminUserGlobalSignOutResponse`

``` purescript
newtype AdminUserGlobalSignOutResponse
  = AdminUserGlobalSignOutResponse {  }
```

<p>The global sign-out response, as an administrator.</p>

##### Instances
``` purescript
Newtype AdminUserGlobalSignOutResponse _
```

#### `AdvancedSecurityModeType`

``` purescript
newtype AdvancedSecurityModeType
  = AdvancedSecurityModeType String
```

##### Instances
``` purescript
Newtype AdvancedSecurityModeType _
```

#### `AliasAttributeType`

``` purescript
newtype AliasAttributeType
  = AliasAttributeType String
```

##### Instances
``` purescript
Newtype AliasAttributeType _
```

#### `AliasAttributesListType`

``` purescript
newtype AliasAttributesListType
  = AliasAttributesListType (Array AliasAttributeType)
```

##### Instances
``` purescript
Newtype AliasAttributesListType _
```

#### `AliasExistsException`

``` purescript
newtype AliasExistsException
  = AliasExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.</p>

##### Instances
``` purescript
Newtype AliasExistsException _
```

#### `AnalyticsConfigurationType`

``` purescript
newtype AnalyticsConfigurationType
  = AnalyticsConfigurationType { "ApplicationId" :: HexStringType, "RoleArn" :: ArnType, "ExternalId" :: StringType, "UserDataShared" :: NullOrUndefined (BooleanType) }
```

<p>The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.</p>

##### Instances
``` purescript
Newtype AnalyticsConfigurationType _
```

#### `AnalyticsMetadataType`

``` purescript
newtype AnalyticsMetadataType
  = AnalyticsMetadataType { "AnalyticsEndpointId" :: NullOrUndefined (StringType) }
```

<p>An Amazon Pinpoint analytics endpoint.</p> <p>An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.</p>

##### Instances
``` purescript
Newtype AnalyticsMetadataType _
```

#### `ArnType`

``` purescript
newtype ArnType
  = ArnType String
```

##### Instances
``` purescript
Newtype ArnType _
```

#### `AssociateSoftwareTokenRequest`

``` purescript
newtype AssociateSoftwareTokenRequest
  = AssociateSoftwareTokenRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "Session" :: NullOrUndefined (SessionType) }
```

##### Instances
``` purescript
Newtype AssociateSoftwareTokenRequest _
```

#### `AssociateSoftwareTokenResponse`

``` purescript
newtype AssociateSoftwareTokenResponse
  = AssociateSoftwareTokenResponse { "SecretCode" :: NullOrUndefined (SecretCodeType), "Session" :: NullOrUndefined (SessionType) }
```

##### Instances
``` purescript
Newtype AssociateSoftwareTokenResponse _
```

#### `AttributeDataType`

``` purescript
newtype AttributeDataType
  = AttributeDataType String
```

##### Instances
``` purescript
Newtype AttributeDataType _
```

#### `AttributeListType`

``` purescript
newtype AttributeListType
  = AttributeListType (Array AttributeType)
```

##### Instances
``` purescript
Newtype AttributeListType _
```

#### `AttributeMappingKeyType`

``` purescript
newtype AttributeMappingKeyType
  = AttributeMappingKeyType String
```

##### Instances
``` purescript
Newtype AttributeMappingKeyType _
```

#### `AttributeMappingType`

``` purescript
newtype AttributeMappingType
  = AttributeMappingType (Map AttributeMappingKeyType StringType)
```

##### Instances
``` purescript
Newtype AttributeMappingType _
```

#### `AttributeNameListType`

``` purescript
newtype AttributeNameListType
  = AttributeNameListType (Array AttributeNameType)
```

##### Instances
``` purescript
Newtype AttributeNameListType _
```

#### `AttributeNameType`

``` purescript
newtype AttributeNameType
  = AttributeNameType String
```

##### Instances
``` purescript
Newtype AttributeNameType _
```

#### `AttributeType`

``` purescript
newtype AttributeType
  = AttributeType { "Name" :: AttributeNameType, "Value" :: NullOrUndefined (AttributeValueType) }
```

<p>Specifies whether the attribute is standard or custom.</p>

##### Instances
``` purescript
Newtype AttributeType _
```

#### `AttributeValueType`

``` purescript
newtype AttributeValueType
  = AttributeValueType String
```

##### Instances
``` purescript
Newtype AttributeValueType _
```

#### `AuthEventType`

``` purescript
newtype AuthEventType
  = AuthEventType { "EventId" :: NullOrUndefined (StringType), "EventType" :: NullOrUndefined (EventType), "CreationDate" :: NullOrUndefined (DateType), "EventResponse" :: NullOrUndefined (EventResponseType), "EventRisk" :: NullOrUndefined (EventRiskType), "ChallengeResponses" :: NullOrUndefined (ChallengeResponseListType), "EventContextData" :: NullOrUndefined (EventContextDataType), "EventFeedback" :: NullOrUndefined (EventFeedbackType) }
```

<p>The authentication event type.</p>

##### Instances
``` purescript
Newtype AuthEventType _
```

#### `AuthEventsType`

``` purescript
newtype AuthEventsType
  = AuthEventsType (Array AuthEventType)
```

##### Instances
``` purescript
Newtype AuthEventsType _
```

#### `AuthFlowType`

``` purescript
newtype AuthFlowType
  = AuthFlowType String
```

##### Instances
``` purescript
Newtype AuthFlowType _
```

#### `AuthParametersType`

``` purescript
newtype AuthParametersType
  = AuthParametersType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype AuthParametersType _
```

#### `AuthenticationResultType`

``` purescript
newtype AuthenticationResultType
  = AuthenticationResultType { "AccessToken" :: NullOrUndefined (TokenModelType), "ExpiresIn" :: NullOrUndefined (IntegerType), "TokenType" :: NullOrUndefined (StringType), "RefreshToken" :: NullOrUndefined (TokenModelType), "IdToken" :: NullOrUndefined (TokenModelType), "NewDeviceMetadata" :: NullOrUndefined (NewDeviceMetadataType) }
```

<p>The authentication result.</p>

##### Instances
``` purescript
Newtype AuthenticationResultType _
```

#### `BlockedIPRangeListType`

``` purescript
newtype BlockedIPRangeListType
  = BlockedIPRangeListType (Array StringType)
```

##### Instances
``` purescript
Newtype BlockedIPRangeListType _
```

#### `BooleanType`

``` purescript
newtype BooleanType
  = BooleanType Boolean
```

##### Instances
``` purescript
Newtype BooleanType _
```

#### `CSSType`

``` purescript
newtype CSSType
  = CSSType String
```

##### Instances
``` purescript
Newtype CSSType _
```

#### `CSSVersionType`

``` purescript
newtype CSSVersionType
  = CSSVersionType String
```

##### Instances
``` purescript
Newtype CSSVersionType _
```

#### `CallbackURLsListType`

``` purescript
newtype CallbackURLsListType
  = CallbackURLsListType (Array RedirectUrlType)
```

##### Instances
``` purescript
Newtype CallbackURLsListType _
```

#### `ChallengeName`

``` purescript
newtype ChallengeName
  = ChallengeName String
```

##### Instances
``` purescript
Newtype ChallengeName _
```

#### `ChallengeNameType`

``` purescript
newtype ChallengeNameType
  = ChallengeNameType String
```

##### Instances
``` purescript
Newtype ChallengeNameType _
```

#### `ChallengeParametersType`

``` purescript
newtype ChallengeParametersType
  = ChallengeParametersType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype ChallengeParametersType _
```

#### `ChallengeResponse`

``` purescript
newtype ChallengeResponse
  = ChallengeResponse String
```

##### Instances
``` purescript
Newtype ChallengeResponse _
```

#### `ChallengeResponseListType`

``` purescript
newtype ChallengeResponseListType
  = ChallengeResponseListType (Array ChallengeResponseType)
```

##### Instances
``` purescript
Newtype ChallengeResponseListType _
```

#### `ChallengeResponseType`

``` purescript
newtype ChallengeResponseType
  = ChallengeResponseType { "ChallengeName" :: NullOrUndefined (ChallengeName), "ChallengeResponse" :: NullOrUndefined (ChallengeResponse) }
```

<p>The challenge response type.</p>

##### Instances
``` purescript
Newtype ChallengeResponseType _
```

#### `ChallengeResponsesType`

``` purescript
newtype ChallengeResponsesType
  = ChallengeResponsesType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype ChallengeResponsesType _
```

#### `ChangePasswordRequest`

``` purescript
newtype ChangePasswordRequest
  = ChangePasswordRequest { "PreviousPassword" :: PasswordType, "ProposedPassword" :: PasswordType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to change a user password.</p>

##### Instances
``` purescript
Newtype ChangePasswordRequest _
```

#### `ChangePasswordResponse`

``` purescript
newtype ChangePasswordResponse
  = ChangePasswordResponse {  }
```

<p>The response from the server to the change password request.</p>

##### Instances
``` purescript
Newtype ChangePasswordResponse _
```

#### `ClientIdType`

``` purescript
newtype ClientIdType
  = ClientIdType String
```

##### Instances
``` purescript
Newtype ClientIdType _
```

#### `ClientMetadataType`

``` purescript
newtype ClientMetadataType
  = ClientMetadataType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype ClientMetadataType _
```

#### `ClientNameType`

``` purescript
newtype ClientNameType
  = ClientNameType String
```

##### Instances
``` purescript
Newtype ClientNameType _
```

#### `ClientPermissionListType`

``` purescript
newtype ClientPermissionListType
  = ClientPermissionListType (Array ClientPermissionType)
```

##### Instances
``` purescript
Newtype ClientPermissionListType _
```

#### `ClientPermissionType`

``` purescript
newtype ClientPermissionType
  = ClientPermissionType String
```

##### Instances
``` purescript
Newtype ClientPermissionType _
```

#### `ClientSecretType`

``` purescript
newtype ClientSecretType
  = ClientSecretType String
```

##### Instances
``` purescript
Newtype ClientSecretType _
```

#### `CodeDeliveryDetailsListType`

``` purescript
newtype CodeDeliveryDetailsListType
  = CodeDeliveryDetailsListType (Array CodeDeliveryDetailsType)
```

##### Instances
``` purescript
Newtype CodeDeliveryDetailsListType _
```

#### `CodeDeliveryDetailsType`

``` purescript
newtype CodeDeliveryDetailsType
  = CodeDeliveryDetailsType { "Destination" :: NullOrUndefined (StringType), "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType), "AttributeName" :: NullOrUndefined (AttributeNameType) }
```

<p>The code delivery details being returned from the server.</p>

##### Instances
``` purescript
Newtype CodeDeliveryDetailsType _
```

#### `CodeDeliveryFailureException`

``` purescript
newtype CodeDeliveryFailureException
  = CodeDeliveryFailureException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a verification code fails to deliver successfully.</p>

##### Instances
``` purescript
Newtype CodeDeliveryFailureException _
```

#### `CodeMismatchException`

``` purescript
newtype CodeMismatchException
  = CodeMismatchException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if the provided code does not match what the server was expecting.</p>

##### Instances
``` purescript
Newtype CodeMismatchException _
```

#### `CompletionMessageType`

``` purescript
newtype CompletionMessageType
  = CompletionMessageType String
```

##### Instances
``` purescript
Newtype CompletionMessageType _
```

#### `CompromisedCredentialsActionsType`

``` purescript
newtype CompromisedCredentialsActionsType
  = CompromisedCredentialsActionsType { "EventAction" :: CompromisedCredentialsEventActionType }
```

<p>The compromised credentials actions type</p>

##### Instances
``` purescript
Newtype CompromisedCredentialsActionsType _
```

#### `CompromisedCredentialsEventActionType`

``` purescript
newtype CompromisedCredentialsEventActionType
  = CompromisedCredentialsEventActionType String
```

##### Instances
``` purescript
Newtype CompromisedCredentialsEventActionType _
```

#### `CompromisedCredentialsRiskConfigurationType`

``` purescript
newtype CompromisedCredentialsRiskConfigurationType
  = CompromisedCredentialsRiskConfigurationType { "EventFilter" :: NullOrUndefined (EventFiltersType), "Actions" :: CompromisedCredentialsActionsType }
```

<p>The compromised credentials risk configuration type.</p>

##### Instances
``` purescript
Newtype CompromisedCredentialsRiskConfigurationType _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if two or more modifications are happening concurrently.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `ConfirmDeviceRequest`

``` purescript
newtype ConfirmDeviceRequest
  = ConfirmDeviceRequest { "AccessToken" :: TokenModelType, "DeviceKey" :: DeviceKeyType, "DeviceSecretVerifierConfig" :: NullOrUndefined (DeviceSecretVerifierConfigType), "DeviceName" :: NullOrUndefined (DeviceNameType) }
```

<p>Confirms the device request.</p>

##### Instances
``` purescript
Newtype ConfirmDeviceRequest _
```

#### `ConfirmDeviceResponse`

``` purescript
newtype ConfirmDeviceResponse
  = ConfirmDeviceResponse { "UserConfirmationNecessary" :: NullOrUndefined (BooleanType) }
```

<p>Confirms the device response.</p>

##### Instances
``` purescript
Newtype ConfirmDeviceResponse _
```

#### `ConfirmForgotPasswordRequest`

``` purescript
newtype ConfirmForgotPasswordRequest
  = ConfirmForgotPasswordRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "ConfirmationCode" :: ConfirmationCodeType, "Password" :: PasswordType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>The request representing the confirmation for a password reset.</p>

##### Instances
``` purescript
Newtype ConfirmForgotPasswordRequest _
```

#### `ConfirmForgotPasswordResponse`

``` purescript
newtype ConfirmForgotPasswordResponse
  = ConfirmForgotPasswordResponse {  }
```

<p>The response from the server that results from a user's request to retrieve a forgotten password.</p>

##### Instances
``` purescript
Newtype ConfirmForgotPasswordResponse _
```

#### `ConfirmSignUpRequest`

``` purescript
newtype ConfirmSignUpRequest
  = ConfirmSignUpRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "ConfirmationCode" :: ConfirmationCodeType, "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Represents the request to confirm registration of a user.</p>

##### Instances
``` purescript
Newtype ConfirmSignUpRequest _
```

#### `ConfirmSignUpResponse`

``` purescript
newtype ConfirmSignUpResponse
  = ConfirmSignUpResponse {  }
```

<p>Represents the response from the server for the registration confirmation.</p>

##### Instances
``` purescript
Newtype ConfirmSignUpResponse _
```

#### `ConfirmationCodeType`

``` purescript
newtype ConfirmationCodeType
  = ConfirmationCodeType String
```

##### Instances
``` purescript
Newtype ConfirmationCodeType _
```

#### `ContextDataType`

``` purescript
newtype ContextDataType
  = ContextDataType { "IpAddress" :: StringType, "ServerName" :: StringType, "ServerPath" :: StringType, "HttpHeaders" :: HttpHeaderList, "EncodedData" :: NullOrUndefined (StringType) }
```

<p>Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>

##### Instances
``` purescript
Newtype ContextDataType _
```

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType, "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType) }
```

##### Instances
``` purescript
Newtype CreateGroupRequest _
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

##### Instances
``` purescript
Newtype CreateGroupResponse _
```

#### `CreateIdentityProviderRequest`

``` purescript
newtype CreateIdentityProviderRequest
  = CreateIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameTypeV1, "ProviderType" :: IdentityProviderTypeType, "ProviderDetails" :: ProviderDetailsType, "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType) }
```

##### Instances
``` purescript
Newtype CreateIdentityProviderRequest _
```

#### `CreateIdentityProviderResponse`

``` purescript
newtype CreateIdentityProviderResponse
  = CreateIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

##### Instances
``` purescript
Newtype CreateIdentityProviderResponse _
```

#### `CreateResourceServerRequest`

``` purescript
newtype CreateResourceServerRequest
  = CreateResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType, "Name" :: ResourceServerNameType, "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

##### Instances
``` purescript
Newtype CreateResourceServerRequest _
```

#### `CreateResourceServerResponse`

``` purescript
newtype CreateResourceServerResponse
  = CreateResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

##### Instances
``` purescript
Newtype CreateResourceServerResponse _
```

#### `CreateUserImportJobRequest`

``` purescript
newtype CreateUserImportJobRequest
  = CreateUserImportJobRequest { "JobName" :: UserImportJobNameType, "UserPoolId" :: UserPoolIdType, "CloudWatchLogsRoleArn" :: ArnType }
```

<p>Represents the request to create the user import job.</p>

##### Instances
``` purescript
Newtype CreateUserImportJobRequest _
```

#### `CreateUserImportJobResponse`

``` purescript
newtype CreateUserImportJobResponse
  = CreateUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to create the user import job.</p>

##### Instances
``` purescript
Newtype CreateUserImportJobResponse _
```

#### `CreateUserPoolClientRequest`

``` purescript
newtype CreateUserPoolClientRequest
  = CreateUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientName" :: ClientNameType, "GenerateSecret" :: NullOrUndefined (GenerateSecret), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Represents the request to create a user pool client.</p>

##### Instances
``` purescript
Newtype CreateUserPoolClientRequest _
```

#### `CreateUserPoolClientResponse`

``` purescript
newtype CreateUserPoolClientResponse
  = CreateUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server to create a user pool client.</p>

##### Instances
``` purescript
Newtype CreateUserPoolClientResponse _
```

#### `CreateUserPoolDomainRequest`

``` purescript
newtype CreateUserPoolDomainRequest
  = CreateUserPoolDomainRequest { "Domain" :: DomainType, "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype CreateUserPoolDomainRequest _
```

#### `CreateUserPoolDomainResponse`

``` purescript
newtype CreateUserPoolDomainResponse
  = CreateUserPoolDomainResponse {  }
```

##### Instances
``` purescript
Newtype CreateUserPoolDomainResponse _
```

#### `CreateUserPoolRequest`

``` purescript
newtype CreateUserPoolRequest
  = CreateUserPoolRequest { "PoolName" :: UserPoolNameType, "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "AliasAttributes" :: NullOrUndefined (AliasAttributesListType), "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "Schema" :: NullOrUndefined (SchemaAttributesListType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>Represents the request to create a user pool.</p>

##### Instances
``` purescript
Newtype CreateUserPoolRequest _
```

#### `CreateUserPoolResponse`

``` purescript
newtype CreateUserPoolResponse
  = CreateUserPoolResponse { "UserPool" :: NullOrUndefined (UserPoolType) }
```

<p>Represents the response from the server for the request to create a user pool.</p>

##### Instances
``` purescript
Newtype CreateUserPoolResponse _
```

#### `CustomAttributeNameType`

``` purescript
newtype CustomAttributeNameType
  = CustomAttributeNameType String
```

##### Instances
``` purescript
Newtype CustomAttributeNameType _
```

#### `CustomAttributesListType`

``` purescript
newtype CustomAttributesListType
  = CustomAttributesListType (Array SchemaAttributeType)
```

##### Instances
``` purescript
Newtype CustomAttributesListType _
```

#### `DateType`

``` purescript
newtype DateType
  = DateType Number
```

##### Instances
``` purescript
Newtype DateType _
```

#### `DefaultEmailOptionType`

``` purescript
newtype DefaultEmailOptionType
  = DefaultEmailOptionType String
```

##### Instances
``` purescript
Newtype DefaultEmailOptionType _
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype DeleteGroupRequest _
```

#### `DeleteIdentityProviderRequest`

``` purescript
newtype DeleteIdentityProviderRequest
  = DeleteIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType }
```

##### Instances
``` purescript
Newtype DeleteIdentityProviderRequest _
```

#### `DeleteResourceServerRequest`

``` purescript
newtype DeleteResourceServerRequest
  = DeleteResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType }
```

##### Instances
``` purescript
Newtype DeleteResourceServerRequest _
```

#### `DeleteUserAttributesRequest`

``` purescript
newtype DeleteUserAttributesRequest
  = DeleteUserAttributesRequest { "UserAttributeNames" :: AttributeNameListType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to delete user attributes.</p>

##### Instances
``` purescript
Newtype DeleteUserAttributesRequest _
```

#### `DeleteUserAttributesResponse`

``` purescript
newtype DeleteUserAttributesResponse
  = DeleteUserAttributesResponse {  }
```

<p>Represents the response from the server to delete user attributes.</p>

##### Instances
``` purescript
Newtype DeleteUserAttributesResponse _
```

#### `DeleteUserPoolClientRequest`

``` purescript
newtype DeleteUserPoolClientRequest
  = DeleteUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType }
```

<p>Represents the request to delete a user pool client.</p>

##### Instances
``` purescript
Newtype DeleteUserPoolClientRequest _
```

#### `DeleteUserPoolDomainRequest`

``` purescript
newtype DeleteUserPoolDomainRequest
  = DeleteUserPoolDomainRequest { "Domain" :: DomainType, "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype DeleteUserPoolDomainRequest _
```

#### `DeleteUserPoolDomainResponse`

``` purescript
newtype DeleteUserPoolDomainResponse
  = DeleteUserPoolDomainResponse {  }
```

##### Instances
``` purescript
Newtype DeleteUserPoolDomainResponse _
```

#### `DeleteUserPoolRequest`

``` purescript
newtype DeleteUserPoolRequest
  = DeleteUserPoolRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to delete a user pool.</p>

##### Instances
``` purescript
Newtype DeleteUserPoolRequest _
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to delete a user.</p>

##### Instances
``` purescript
Newtype DeleteUserRequest _
```

#### `DeliveryMediumListType`

``` purescript
newtype DeliveryMediumListType
  = DeliveryMediumListType (Array DeliveryMediumType)
```

##### Instances
``` purescript
Newtype DeliveryMediumListType _
```

#### `DeliveryMediumType`

``` purescript
newtype DeliveryMediumType
  = DeliveryMediumType String
```

##### Instances
``` purescript
Newtype DeliveryMediumType _
```

#### `DescribeIdentityProviderRequest`

``` purescript
newtype DescribeIdentityProviderRequest
  = DescribeIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType }
```

##### Instances
``` purescript
Newtype DescribeIdentityProviderRequest _
```

#### `DescribeIdentityProviderResponse`

``` purescript
newtype DescribeIdentityProviderResponse
  = DescribeIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

##### Instances
``` purescript
Newtype DescribeIdentityProviderResponse _
```

#### `DescribeResourceServerRequest`

``` purescript
newtype DescribeResourceServerRequest
  = DescribeResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType }
```

##### Instances
``` purescript
Newtype DescribeResourceServerRequest _
```

#### `DescribeResourceServerResponse`

``` purescript
newtype DescribeResourceServerResponse
  = DescribeResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

##### Instances
``` purescript
Newtype DescribeResourceServerResponse _
```

#### `DescribeRiskConfigurationRequest`

``` purescript
newtype DescribeRiskConfigurationRequest
  = DescribeRiskConfigurationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType) }
```

##### Instances
``` purescript
Newtype DescribeRiskConfigurationRequest _
```

#### `DescribeRiskConfigurationResponse`

``` purescript
newtype DescribeRiskConfigurationResponse
  = DescribeRiskConfigurationResponse { "RiskConfiguration" :: RiskConfigurationType }
```

##### Instances
``` purescript
Newtype DescribeRiskConfigurationResponse _
```

#### `DescribeUserImportJobRequest`

``` purescript
newtype DescribeUserImportJobRequest
  = DescribeUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to describe the user import job.</p>

##### Instances
``` purescript
Newtype DescribeUserImportJobRequest _
```

#### `DescribeUserImportJobResponse`

``` purescript
newtype DescribeUserImportJobResponse
  = DescribeUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to describe the user import job.</p>

##### Instances
``` purescript
Newtype DescribeUserImportJobResponse _
```

#### `DescribeUserPoolClientRequest`

``` purescript
newtype DescribeUserPoolClientRequest
  = DescribeUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType }
```

<p>Represents the request to describe a user pool client.</p>

##### Instances
``` purescript
Newtype DescribeUserPoolClientRequest _
```

#### `DescribeUserPoolClientResponse`

``` purescript
newtype DescribeUserPoolClientResponse
  = DescribeUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server from a request to describe the user pool client.</p>

##### Instances
``` purescript
Newtype DescribeUserPoolClientResponse _
```

#### `DescribeUserPoolDomainRequest`

``` purescript
newtype DescribeUserPoolDomainRequest
  = DescribeUserPoolDomainRequest { "Domain" :: DomainType }
```

##### Instances
``` purescript
Newtype DescribeUserPoolDomainRequest _
```

#### `DescribeUserPoolDomainResponse`

``` purescript
newtype DescribeUserPoolDomainResponse
  = DescribeUserPoolDomainResponse { "DomainDescription" :: NullOrUndefined (DomainDescriptionType) }
```

##### Instances
``` purescript
Newtype DescribeUserPoolDomainResponse _
```

#### `DescribeUserPoolRequest`

``` purescript
newtype DescribeUserPoolRequest
  = DescribeUserPoolRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to describe the user pool.</p>

##### Instances
``` purescript
Newtype DescribeUserPoolRequest _
```

#### `DescribeUserPoolResponse`

``` purescript
newtype DescribeUserPoolResponse
  = DescribeUserPoolResponse { "UserPool" :: NullOrUndefined (UserPoolType) }
```

<p>Represents the response to describe the user pool.</p>

##### Instances
``` purescript
Newtype DescribeUserPoolResponse _
```

#### `DescriptionType`

``` purescript
newtype DescriptionType
  = DescriptionType String
```

##### Instances
``` purescript
Newtype DescriptionType _
```

#### `DeviceConfigurationType`

``` purescript
newtype DeviceConfigurationType
  = DeviceConfigurationType { "ChallengeRequiredOnNewDevice" :: NullOrUndefined (BooleanType), "DeviceOnlyRememberedOnUserPrompt" :: NullOrUndefined (BooleanType) }
```

<p>The configuration for the user pool's device tracking.</p>

##### Instances
``` purescript
Newtype DeviceConfigurationType _
```

#### `DeviceKeyType`

``` purescript
newtype DeviceKeyType
  = DeviceKeyType String
```

##### Instances
``` purescript
Newtype DeviceKeyType _
```

#### `DeviceListType`

``` purescript
newtype DeviceListType
  = DeviceListType (Array DeviceType)
```

##### Instances
``` purescript
Newtype DeviceListType _
```

#### `DeviceNameType`

``` purescript
newtype DeviceNameType
  = DeviceNameType String
```

##### Instances
``` purescript
Newtype DeviceNameType _
```

#### `DeviceRememberedStatusType`

``` purescript
newtype DeviceRememberedStatusType
  = DeviceRememberedStatusType String
```

##### Instances
``` purescript
Newtype DeviceRememberedStatusType _
```

#### `DeviceSecretVerifierConfigType`

``` purescript
newtype DeviceSecretVerifierConfigType
  = DeviceSecretVerifierConfigType { "PasswordVerifier" :: NullOrUndefined (StringType), "Salt" :: NullOrUndefined (StringType) }
```

<p>The device verifier against which it will be authenticated.</p>

##### Instances
``` purescript
Newtype DeviceSecretVerifierConfigType _
```

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType { "DeviceKey" :: NullOrUndefined (DeviceKeyType), "DeviceAttributes" :: NullOrUndefined (AttributeListType), "DeviceCreateDate" :: NullOrUndefined (DateType), "DeviceLastModifiedDate" :: NullOrUndefined (DateType), "DeviceLastAuthenticatedDate" :: NullOrUndefined (DateType) }
```

<p>The device type.</p>

##### Instances
``` purescript
Newtype DeviceType _
```

#### `DomainDescriptionType`

``` purescript
newtype DomainDescriptionType
  = DomainDescriptionType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "AWSAccountId" :: NullOrUndefined (AWSAccountIdType), "Domain" :: NullOrUndefined (DomainType), "S3Bucket" :: NullOrUndefined (S3BucketType), "CloudFrontDistribution" :: NullOrUndefined (ArnType), "Version" :: NullOrUndefined (DomainVersionType), "Status" :: NullOrUndefined (DomainStatusType) }
```

<p>A container for information about a domain.</p>

##### Instances
``` purescript
Newtype DomainDescriptionType _
```

#### `DomainStatusType`

``` purescript
newtype DomainStatusType
  = DomainStatusType String
```

##### Instances
``` purescript
Newtype DomainStatusType _
```

#### `DomainType`

``` purescript
newtype DomainType
  = DomainType String
```

##### Instances
``` purescript
Newtype DomainType _
```

#### `DomainVersionType`

``` purescript
newtype DomainVersionType
  = DomainVersionType String
```

##### Instances
``` purescript
Newtype DomainVersionType _
```

#### `DuplicateProviderException`

``` purescript
newtype DuplicateProviderException
  = DuplicateProviderException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the provider is already supported by the user pool.</p>

##### Instances
``` purescript
Newtype DuplicateProviderException _
```

#### `EmailAddressType`

``` purescript
newtype EmailAddressType
  = EmailAddressType String
```

##### Instances
``` purescript
Newtype EmailAddressType _
```

#### `EmailConfigurationType`

``` purescript
newtype EmailConfigurationType
  = EmailConfigurationType { "SourceArn" :: NullOrUndefined (ArnType), "ReplyToEmailAddress" :: NullOrUndefined (EmailAddressType) }
```

<p>The email configuration type.</p>

##### Instances
``` purescript
Newtype EmailConfigurationType _
```

#### `EmailNotificationBodyType`

``` purescript
newtype EmailNotificationBodyType
  = EmailNotificationBodyType String
```

##### Instances
``` purescript
Newtype EmailNotificationBodyType _
```

#### `EmailNotificationSubjectType`

``` purescript
newtype EmailNotificationSubjectType
  = EmailNotificationSubjectType String
```

##### Instances
``` purescript
Newtype EmailNotificationSubjectType _
```

#### `EmailVerificationMessageByLinkType`

``` purescript
newtype EmailVerificationMessageByLinkType
  = EmailVerificationMessageByLinkType String
```

##### Instances
``` purescript
Newtype EmailVerificationMessageByLinkType _
```

#### `EmailVerificationMessageType`

``` purescript
newtype EmailVerificationMessageType
  = EmailVerificationMessageType String
```

##### Instances
``` purescript
Newtype EmailVerificationMessageType _
```

#### `EmailVerificationSubjectByLinkType`

``` purescript
newtype EmailVerificationSubjectByLinkType
  = EmailVerificationSubjectByLinkType String
```

##### Instances
``` purescript
Newtype EmailVerificationSubjectByLinkType _
```

#### `EmailVerificationSubjectType`

``` purescript
newtype EmailVerificationSubjectType
  = EmailVerificationSubjectType String
```

##### Instances
``` purescript
Newtype EmailVerificationSubjectType _
```

#### `EnableSoftwareTokenMFAException`

``` purescript
newtype EnableSoftwareTokenMFAException
  = EnableSoftwareTokenMFAException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).</p>

##### Instances
``` purescript
Newtype EnableSoftwareTokenMFAException _
```

#### `EventContextDataType`

``` purescript
newtype EventContextDataType
  = EventContextDataType { "IpAddress" :: NullOrUndefined (StringType), "DeviceName" :: NullOrUndefined (StringType), "Timezone" :: NullOrUndefined (StringType), "City" :: NullOrUndefined (StringType), "Country" :: NullOrUndefined (StringType) }
```

<p>Specifies the user context data captured at the time of an event request.</p>

##### Instances
``` purescript
Newtype EventContextDataType _
```

#### `EventFeedbackType`

``` purescript
newtype EventFeedbackType
  = EventFeedbackType { "FeedbackValue" :: FeedbackValueType, "Provider" :: StringType, "FeedbackDate" :: NullOrUndefined (DateType) }
```

<p>Specifies the event feedback type.</p>

##### Instances
``` purescript
Newtype EventFeedbackType _
```

#### `EventFilterType`

``` purescript
newtype EventFilterType
  = EventFilterType String
```

##### Instances
``` purescript
Newtype EventFilterType _
```

#### `EventFiltersType`

``` purescript
newtype EventFiltersType
  = EventFiltersType (Array EventFilterType)
```

##### Instances
``` purescript
Newtype EventFiltersType _
```

#### `EventIdType`

``` purescript
newtype EventIdType
  = EventIdType String
```

##### Instances
``` purescript
Newtype EventIdType _
```

#### `EventResponseType`

``` purescript
newtype EventResponseType
  = EventResponseType String
```

##### Instances
``` purescript
Newtype EventResponseType _
```

#### `EventRiskType`

``` purescript
newtype EventRiskType
  = EventRiskType { "RiskDecision" :: NullOrUndefined (RiskDecisionType), "RiskLevel" :: NullOrUndefined (RiskLevelType) }
```

<p>The event risk type.</p>

##### Instances
``` purescript
Newtype EventRiskType _
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

#### `ExpiredCodeException`

``` purescript
newtype ExpiredCodeException
  = ExpiredCodeException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if a code has expired.</p>

##### Instances
``` purescript
Newtype ExpiredCodeException _
```

#### `ExplicitAuthFlowsListType`

``` purescript
newtype ExplicitAuthFlowsListType
  = ExplicitAuthFlowsListType (Array ExplicitAuthFlowsType)
```

##### Instances
``` purescript
Newtype ExplicitAuthFlowsListType _
```

#### `ExplicitAuthFlowsType`

``` purescript
newtype ExplicitAuthFlowsType
  = ExplicitAuthFlowsType String
```

##### Instances
``` purescript
Newtype ExplicitAuthFlowsType _
```

#### `FeedbackValueType`

``` purescript
newtype FeedbackValueType
  = FeedbackValueType String
```

##### Instances
``` purescript
Newtype FeedbackValueType _
```

#### `ForceAliasCreation`

``` purescript
newtype ForceAliasCreation
  = ForceAliasCreation Boolean
```

##### Instances
``` purescript
Newtype ForceAliasCreation _
```

#### `ForgetDeviceRequest`

``` purescript
newtype ForgetDeviceRequest
  = ForgetDeviceRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "DeviceKey" :: DeviceKeyType }
```

<p>Represents the request to forget the device.</p>

##### Instances
``` purescript
Newtype ForgetDeviceRequest _
```

#### `ForgotPasswordRequest`

``` purescript
newtype ForgotPasswordRequest
  = ForgotPasswordRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "UserContextData" :: NullOrUndefined (UserContextDataType), "Username" :: UsernameType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType) }
```

<p>Represents the request to reset a user's password.</p>

##### Instances
``` purescript
Newtype ForgotPasswordRequest _
```

#### `ForgotPasswordResponse`

``` purescript
newtype ForgotPasswordResponse
  = ForgotPasswordResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>Respresents the response from the server regarding the request to reset a password.</p>

##### Instances
``` purescript
Newtype ForgotPasswordResponse _
```

#### `GenerateSecret`

``` purescript
newtype GenerateSecret
  = GenerateSecret Boolean
```

##### Instances
``` purescript
Newtype GenerateSecret _
```

#### `GetCSVHeaderRequest`

``` purescript
newtype GetCSVHeaderRequest
  = GetCSVHeaderRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to get the header information for the .csv file for the user import job.</p>

##### Instances
``` purescript
Newtype GetCSVHeaderRequest _
```

#### `GetCSVHeaderResponse`

``` purescript
newtype GetCSVHeaderResponse
  = GetCSVHeaderResponse { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "CSVHeader" :: NullOrUndefined (ListOfStringTypes) }
```

<p>Represents the response from the server to the request to get the header information for the .csv file for the user import job.</p>

##### Instances
``` purescript
Newtype GetCSVHeaderResponse _
```

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "DeviceKey" :: DeviceKeyType, "AccessToken" :: NullOrUndefined (TokenModelType) }
```

<p>Represents the request to get the device.</p>

##### Instances
``` purescript
Newtype GetDeviceRequest _
```

#### `GetDeviceResponse`

``` purescript
newtype GetDeviceResponse
  = GetDeviceResponse { "Device" :: DeviceType }
```

<p>Gets the device response.</p>

##### Instances
``` purescript
Newtype GetDeviceResponse _
```

#### `GetGroupRequest`

``` purescript
newtype GetGroupRequest
  = GetGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype GetGroupRequest _
```

#### `GetGroupResponse`

``` purescript
newtype GetGroupResponse
  = GetGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

##### Instances
``` purescript
Newtype GetGroupResponse _
```

#### `GetIdentityProviderByIdentifierRequest`

``` purescript
newtype GetIdentityProviderByIdentifierRequest
  = GetIdentityProviderByIdentifierRequest { "UserPoolId" :: UserPoolIdType, "IdpIdentifier" :: IdpIdentifierType }
```

##### Instances
``` purescript
Newtype GetIdentityProviderByIdentifierRequest _
```

#### `GetIdentityProviderByIdentifierResponse`

``` purescript
newtype GetIdentityProviderByIdentifierResponse
  = GetIdentityProviderByIdentifierResponse { "IdentityProvider" :: IdentityProviderType }
```

##### Instances
``` purescript
Newtype GetIdentityProviderByIdentifierResponse _
```

#### `GetSigningCertificateRequest`

``` purescript
newtype GetSigningCertificateRequest
  = GetSigningCertificateRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Request to get a signing certificate from Cognito.</p>

##### Instances
``` purescript
Newtype GetSigningCertificateRequest _
```

#### `GetSigningCertificateResponse`

``` purescript
newtype GetSigningCertificateResponse
  = GetSigningCertificateResponse { "Certificate" :: NullOrUndefined (StringType) }
```

<p>Response from Cognito for a signing certificate request.</p>

##### Instances
``` purescript
Newtype GetSigningCertificateResponse _
```

#### `GetUICustomizationRequest`

``` purescript
newtype GetUICustomizationRequest
  = GetUICustomizationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType) }
```

##### Instances
``` purescript
Newtype GetUICustomizationRequest _
```

#### `GetUICustomizationResponse`

``` purescript
newtype GetUICustomizationResponse
  = GetUICustomizationResponse { "UICustomization" :: UICustomizationType }
```

##### Instances
``` purescript
Newtype GetUICustomizationResponse _
```

#### `GetUserAttributeVerificationCodeRequest`

``` purescript
newtype GetUserAttributeVerificationCodeRequest
  = GetUserAttributeVerificationCodeRequest { "AccessToken" :: TokenModelType, "AttributeName" :: AttributeNameType }
```

<p>Represents the request to get user attribute verification.</p>

##### Instances
``` purescript
Newtype GetUserAttributeVerificationCodeRequest _
```

#### `GetUserAttributeVerificationCodeResponse`

``` purescript
newtype GetUserAttributeVerificationCodeResponse
  = GetUserAttributeVerificationCodeResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>The verification code response returned by the server response to get the user attribute verification code.</p>

##### Instances
``` purescript
Newtype GetUserAttributeVerificationCodeResponse _
```

#### `GetUserPoolMfaConfigRequest`

``` purescript
newtype GetUserPoolMfaConfigRequest
  = GetUserPoolMfaConfigRequest { "UserPoolId" :: UserPoolIdType }
```

##### Instances
``` purescript
Newtype GetUserPoolMfaConfigRequest _
```

#### `GetUserPoolMfaConfigResponse`

``` purescript
newtype GetUserPoolMfaConfigResponse
  = GetUserPoolMfaConfigResponse { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

##### Instances
``` purescript
Newtype GetUserPoolMfaConfigResponse _
```

#### `GetUserRequest`

``` purescript
newtype GetUserRequest
  = GetUserRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to get information about the user.</p>

##### Instances
``` purescript
Newtype GetUserRequest _
```

#### `GetUserResponse`

``` purescript
newtype GetUserResponse
  = GetUserResponse { "Username" :: UsernameType, "UserAttributes" :: AttributeListType, "MFAOptions" :: NullOrUndefined (MFAOptionListType), "PreferredMfaSetting" :: NullOrUndefined (StringType), "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType) }
```

<p>Represents the response from the server from the request to get information about the user.</p>

##### Instances
``` purescript
Newtype GetUserResponse _
```

#### `GlobalSignOutRequest`

``` purescript
newtype GlobalSignOutRequest
  = GlobalSignOutRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to sign out all devices.</p>

##### Instances
``` purescript
Newtype GlobalSignOutRequest _
```

#### `GlobalSignOutResponse`

``` purescript
newtype GlobalSignOutResponse
  = GlobalSignOutResponse {  }
```

<p>The response to the request to sign out all devices.</p>

##### Instances
``` purescript
Newtype GlobalSignOutResponse _
```

#### `GroupExistsException`

``` purescript
newtype GroupExistsException
  = GroupExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.</p>

##### Instances
``` purescript
Newtype GroupExistsException _
```

#### `GroupListType`

``` purescript
newtype GroupListType
  = GroupListType (Array GroupType)
```

##### Instances
``` purescript
Newtype GroupListType _
```

#### `GroupNameType`

``` purescript
newtype GroupNameType
  = GroupNameType String
```

##### Instances
``` purescript
Newtype GroupNameType _
```

#### `GroupType`

``` purescript
newtype GroupType
  = GroupType { "GroupName" :: NullOrUndefined (GroupNameType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>The group type.</p>

##### Instances
``` purescript
Newtype GroupType _
```

#### `HexStringType`

``` purescript
newtype HexStringType
  = HexStringType String
```

##### Instances
``` purescript
Newtype HexStringType _
```

#### `HttpHeader`

``` purescript
newtype HttpHeader
  = HttpHeader { "HeaderName'" :: NullOrUndefined (StringType), "HeaderValue'" :: NullOrUndefined (StringType) }
```

<p>The HTTP header.</p>

##### Instances
``` purescript
Newtype HttpHeader _
```

#### `HttpHeaderList`

``` purescript
newtype HttpHeaderList
  = HttpHeaderList (Array HttpHeader)
```

##### Instances
``` purescript
Newtype HttpHeaderList _
```

#### `IdentityProviderType`

``` purescript
newtype IdentityProviderType
  = IdentityProviderType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderType" :: NullOrUndefined (IdentityProviderTypeType), "ProviderDetails" :: NullOrUndefined (ProviderDetailsType), "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for information about an identity provider.</p>

##### Instances
``` purescript
Newtype IdentityProviderType _
```

#### `IdentityProviderTypeType`

``` purescript
newtype IdentityProviderTypeType
  = IdentityProviderTypeType String
```

##### Instances
``` purescript
Newtype IdentityProviderTypeType _
```

#### `IdpIdentifierType`

``` purescript
newtype IdpIdentifierType
  = IdpIdentifierType String
```

##### Instances
``` purescript
Newtype IdpIdentifierType _
```

#### `IdpIdentifiersListType`

``` purescript
newtype IdpIdentifiersListType
  = IdpIdentifiersListType (Array IdpIdentifierType)
```

##### Instances
``` purescript
Newtype IdpIdentifiersListType _
```

#### `ImageFileType`

``` purescript
newtype ImageFileType
  = ImageFileType String
```

##### Instances
``` purescript
Newtype ImageFileType _
```

#### `ImageUrlType`

``` purescript
newtype ImageUrlType
  = ImageUrlType String
```

##### Instances
``` purescript
Newtype ImageUrlType _
```

#### `InitiateAuthRequest`

``` purescript
newtype InitiateAuthRequest
  = InitiateAuthRequest { "AuthFlow" :: AuthFlowType, "AuthParameters" :: NullOrUndefined (AuthParametersType), "ClientMetadata" :: NullOrUndefined (ClientMetadataType), "ClientId" :: ClientIdType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Initiates the authentication request.</p>

##### Instances
``` purescript
Newtype InitiateAuthRequest _
```

#### `InitiateAuthResponse`

``` purescript
newtype InitiateAuthResponse
  = InitiateAuthResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Initiates the authentication response.</p>

##### Instances
``` purescript
Newtype InitiateAuthResponse _
```

#### `IntegerType`

``` purescript
newtype IntegerType
  = IntegerType Int
```

##### Instances
``` purescript
Newtype IntegerType _
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters an internal error.</p>

##### Instances
``` purescript
Newtype InternalErrorException _
```

#### `InvalidEmailRoleAccessPolicyException`

``` purescript
newtype InvalidEmailRoleAccessPolicyException
  = InvalidEmailRoleAccessPolicyException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.</p>

##### Instances
``` purescript
Newtype InvalidEmailRoleAccessPolicyException _
```

#### `InvalidLambdaResponseException`

``` purescript
newtype InvalidLambdaResponseException
  = InvalidLambdaResponseException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.</p>

##### Instances
``` purescript
Newtype InvalidLambdaResponseException _
```

#### `InvalidOAuthFlowException`

``` purescript
newtype InvalidOAuthFlowException
  = InvalidOAuthFlowException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified OAuth flow is invalid.</p>

##### Instances
``` purescript
Newtype InvalidOAuthFlowException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid parameter.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `InvalidPasswordException`

``` purescript
newtype InvalidPasswordException
  = InvalidPasswordException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid password.</p>

##### Instances
``` purescript
Newtype InvalidPasswordException _
```

#### `InvalidSmsRoleAccessPolicyException`

``` purescript
newtype InvalidSmsRoleAccessPolicyException
  = InvalidSmsRoleAccessPolicyException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.</p>

##### Instances
``` purescript
Newtype InvalidSmsRoleAccessPolicyException _
```

#### `InvalidSmsRoleTrustRelationshipException`

``` purescript
newtype InvalidSmsRoleTrustRelationshipException
  = InvalidSmsRoleTrustRelationshipException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust <b>cognito-idp.amazonaws.com</b> or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.</p>

##### Instances
``` purescript
Newtype InvalidSmsRoleTrustRelationshipException _
```

#### `InvalidUserPoolConfigurationException`

``` purescript
newtype InvalidUserPoolConfigurationException
  = InvalidUserPoolConfigurationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user pool configuration is invalid.</p>

##### Instances
``` purescript
Newtype InvalidUserPoolConfigurationException _
```

#### `LambdaConfigType`

``` purescript
newtype LambdaConfigType
  = LambdaConfigType { "PreSignUp" :: NullOrUndefined (ArnType), "CustomMessage" :: NullOrUndefined (ArnType), "PostConfirmation" :: NullOrUndefined (ArnType), "PreAuthentication" :: NullOrUndefined (ArnType), "PostAuthentication" :: NullOrUndefined (ArnType), "DefineAuthChallenge" :: NullOrUndefined (ArnType), "CreateAuthChallenge" :: NullOrUndefined (ArnType), "VerifyAuthChallengeResponse" :: NullOrUndefined (ArnType), "PreTokenGeneration" :: NullOrUndefined (ArnType), "UserMigration" :: NullOrUndefined (ArnType) }
```

<p>Specifies the configuration for AWS Lambda triggers.</p>

##### Instances
``` purescript
Newtype LambdaConfigType _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user exceeds the limit for a requested AWS resource.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListDevicesRequest`

``` purescript
newtype ListDevicesRequest
  = ListDevicesRequest { "AccessToken" :: TokenModelType, "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the request to list the devices.</p>

##### Instances
``` purescript
Newtype ListDevicesRequest _
```

#### `ListDevicesResponse`

``` purescript
newtype ListDevicesResponse
  = ListDevicesResponse { "Devices" :: NullOrUndefined (DeviceListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the response to list devices.</p>

##### Instances
``` purescript
Newtype ListDevicesResponse _
```

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "UserPoolId" :: UserPoolIdType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype ListGroupsRequest _
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: NullOrUndefined (GroupListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype ListGroupsResponse _
```

#### `ListIdentityProvidersRequest`

``` purescript
newtype ListIdentityProvidersRequest
  = ListIdentityProvidersRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (ListProvidersLimitType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

##### Instances
``` purescript
Newtype ListIdentityProvidersRequest _
```

#### `ListIdentityProvidersResponse`

``` purescript
newtype ListIdentityProvidersResponse
  = ListIdentityProvidersResponse { "Providers" :: ProvidersListType, "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

##### Instances
``` purescript
Newtype ListIdentityProvidersResponse _
```

#### `ListOfStringTypes`

``` purescript
newtype ListOfStringTypes
  = ListOfStringTypes (Array StringType)
```

##### Instances
``` purescript
Newtype ListOfStringTypes _
```

#### `ListProvidersLimitType`

``` purescript
newtype ListProvidersLimitType
  = ListProvidersLimitType Int
```

##### Instances
``` purescript
Newtype ListProvidersLimitType _
```

#### `ListResourceServersLimitType`

``` purescript
newtype ListResourceServersLimitType
  = ListResourceServersLimitType Int
```

##### Instances
``` purescript
Newtype ListResourceServersLimitType _
```

#### `ListResourceServersRequest`

``` purescript
newtype ListResourceServersRequest
  = ListResourceServersRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (ListResourceServersLimitType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

##### Instances
``` purescript
Newtype ListResourceServersRequest _
```

#### `ListResourceServersResponse`

``` purescript
newtype ListResourceServersResponse
  = ListResourceServersResponse { "ResourceServers" :: ResourceServersListType, "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

##### Instances
``` purescript
Newtype ListResourceServersResponse _
```

#### `ListUserImportJobsRequest`

``` purescript
newtype ListUserImportJobsRequest
  = ListUserImportJobsRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: PoolQueryLimitType, "PaginationToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the request to list the user import jobs.</p>

##### Instances
``` purescript
Newtype ListUserImportJobsRequest _
```

#### `ListUserImportJobsResponse`

``` purescript
newtype ListUserImportJobsResponse
  = ListUserImportJobsResponse { "UserImportJobs" :: NullOrUndefined (UserImportJobsListType), "PaginationToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the response from the server to the request to list the user import jobs.</p>

##### Instances
``` purescript
Newtype ListUserImportJobsResponse _
```

#### `ListUserPoolClientsRequest`

``` purescript
newtype ListUserPoolClientsRequest
  = ListUserPoolClientsRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (QueryLimit), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Represents the request to list the user pool clients.</p>

##### Instances
``` purescript
Newtype ListUserPoolClientsRequest _
```

#### `ListUserPoolClientsResponse`

``` purescript
newtype ListUserPoolClientsResponse
  = ListUserPoolClientsResponse { "UserPoolClients" :: NullOrUndefined (UserPoolClientListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Represents the response from the server that lists user pool clients.</p>

##### Instances
``` purescript
Newtype ListUserPoolClientsResponse _
```

#### `ListUserPoolsRequest`

``` purescript
newtype ListUserPoolsRequest
  = ListUserPoolsRequest { "NextToken" :: NullOrUndefined (PaginationKeyType), "MaxResults" :: PoolQueryLimitType }
```

<p>Represents the request to list user pools.</p>

##### Instances
``` purescript
Newtype ListUserPoolsRequest _
```

#### `ListUserPoolsResponse`

``` purescript
newtype ListUserPoolsResponse
  = ListUserPoolsResponse { "UserPools" :: NullOrUndefined (UserPoolListType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the response to list user pools.</p>

##### Instances
``` purescript
Newtype ListUserPoolsResponse _
```

#### `ListUsersInGroupRequest`

``` purescript
newtype ListUsersInGroupRequest
  = ListUsersInGroupRequest { "UserPoolId" :: UserPoolIdType, "GroupName" :: GroupNameType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype ListUsersInGroupRequest _
```

#### `ListUsersInGroupResponse`

``` purescript
newtype ListUsersInGroupResponse
  = ListUsersInGroupResponse { "Users" :: NullOrUndefined (UsersListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

##### Instances
``` purescript
Newtype ListUsersInGroupResponse _
```

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "UserPoolId" :: UserPoolIdType, "AttributesToGet" :: NullOrUndefined (SearchedAttributeNamesListType), "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType), "Filter" :: NullOrUndefined (UserFilterType) }
```

<p>Represents the request to list users.</p>

##### Instances
``` purescript
Newtype ListUsersRequest _
```

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "Users" :: NullOrUndefined (UsersListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>The response from the request to list users.</p>

##### Instances
``` purescript
Newtype ListUsersResponse _
```

#### `LogoutURLsListType`

``` purescript
newtype LogoutURLsListType
  = LogoutURLsListType (Array RedirectUrlType)
```

##### Instances
``` purescript
Newtype LogoutURLsListType _
```

#### `LongType`

``` purescript
newtype LongType
  = LongType Number
```

##### Instances
``` purescript
Newtype LongType _
```

#### `MFAMethodNotFoundException`

``` purescript
newtype MFAMethodNotFoundException
  = MFAMethodNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.</p>

##### Instances
``` purescript
Newtype MFAMethodNotFoundException _
```

#### `MFAOptionListType`

``` purescript
newtype MFAOptionListType
  = MFAOptionListType (Array MFAOptionType)
```

##### Instances
``` purescript
Newtype MFAOptionListType _
```

#### `MFAOptionType`

``` purescript
newtype MFAOptionType
  = MFAOptionType { "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType), "AttributeName" :: NullOrUndefined (AttributeNameType) }
```

<p>Specifies the different settings for multi-factor authentication (MFA).</p>

##### Instances
``` purescript
Newtype MFAOptionType _
```

#### `MessageActionType`

``` purescript
newtype MessageActionType
  = MessageActionType String
```

##### Instances
``` purescript
Newtype MessageActionType _
```

#### `MessageTemplateType`

``` purescript
newtype MessageTemplateType
  = MessageTemplateType { "SMSMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType) }
```

<p>The message template structure.</p>

##### Instances
``` purescript
Newtype MessageTemplateType _
```

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

##### Instances
``` purescript
Newtype MessageType _
```

#### `NewDeviceMetadataType`

``` purescript
newtype NewDeviceMetadataType
  = NewDeviceMetadataType { "DeviceKey" :: NullOrUndefined (DeviceKeyType), "DeviceGroupKey" :: NullOrUndefined (StringType) }
```

<p>The new device metadata type.</p>

##### Instances
``` purescript
Newtype NewDeviceMetadataType _
```

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not authorized.</p>

##### Instances
``` purescript
Newtype NotAuthorizedException _
```

#### `NotifyConfigurationType`

``` purescript
newtype NotifyConfigurationType
  = NotifyConfigurationType { "From" :: NullOrUndefined (StringType), "ReplyTo" :: NullOrUndefined (StringType), "SourceArn" :: ArnType, "BlockEmail" :: NullOrUndefined (NotifyEmailType), "NoActionEmail" :: NullOrUndefined (NotifyEmailType), "MfaEmail" :: NullOrUndefined (NotifyEmailType) }
```

<p>The notify configuration type.</p>

##### Instances
``` purescript
Newtype NotifyConfigurationType _
```

#### `NotifyEmailType`

``` purescript
newtype NotifyEmailType
  = NotifyEmailType { "Subject" :: EmailNotificationSubjectType, "HtmlBody" :: NullOrUndefined (EmailNotificationBodyType), "TextBody" :: NullOrUndefined (EmailNotificationBodyType) }
```

<p>The notify email type.</p>

##### Instances
``` purescript
Newtype NotifyEmailType _
```

#### `NumberAttributeConstraintsType`

``` purescript
newtype NumberAttributeConstraintsType
  = NumberAttributeConstraintsType { "MinValue" :: NullOrUndefined (StringType), "MaxValue" :: NullOrUndefined (StringType) }
```

<p>The minimum and maximum value of an attribute that is of the number data type.</p>

##### Instances
``` purescript
Newtype NumberAttributeConstraintsType _
```

#### `OAuthFlowType`

``` purescript
newtype OAuthFlowType
  = OAuthFlowType String
```

##### Instances
``` purescript
Newtype OAuthFlowType _
```

#### `OAuthFlowsType`

``` purescript
newtype OAuthFlowsType
  = OAuthFlowsType (Array OAuthFlowType)
```

##### Instances
``` purescript
Newtype OAuthFlowsType _
```

#### `PaginationKey`

``` purescript
newtype PaginationKey
  = PaginationKey String
```

##### Instances
``` purescript
Newtype PaginationKey _
```

#### `PaginationKeyType`

``` purescript
newtype PaginationKeyType
  = PaginationKeyType String
```

##### Instances
``` purescript
Newtype PaginationKeyType _
```

#### `PasswordPolicyMinLengthType`

``` purescript
newtype PasswordPolicyMinLengthType
  = PasswordPolicyMinLengthType Int
```

##### Instances
``` purescript
Newtype PasswordPolicyMinLengthType _
```

#### `PasswordPolicyType`

``` purescript
newtype PasswordPolicyType
  = PasswordPolicyType { "MinimumLength" :: NullOrUndefined (PasswordPolicyMinLengthType), "RequireUppercase" :: NullOrUndefined (BooleanType), "RequireLowercase" :: NullOrUndefined (BooleanType), "RequireNumbers" :: NullOrUndefined (BooleanType), "RequireSymbols" :: NullOrUndefined (BooleanType) }
```

<p>The password policy type.</p>

##### Instances
``` purescript
Newtype PasswordPolicyType _
```

#### `PasswordResetRequiredException`

``` purescript
newtype PasswordResetRequiredException
  = PasswordResetRequiredException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a password reset is required.</p>

##### Instances
``` purescript
Newtype PasswordResetRequiredException _
```

#### `PasswordType`

``` purescript
newtype PasswordType
  = PasswordType String
```

##### Instances
``` purescript
Newtype PasswordType _
```

#### `PoolQueryLimitType`

``` purescript
newtype PoolQueryLimitType
  = PoolQueryLimitType Int
```

##### Instances
``` purescript
Newtype PoolQueryLimitType _
```

#### `PreSignedUrlType`

``` purescript
newtype PreSignedUrlType
  = PreSignedUrlType String
```

##### Instances
``` purescript
Newtype PreSignedUrlType _
```

#### `PrecedenceType`

``` purescript
newtype PrecedenceType
  = PrecedenceType Int
```

##### Instances
``` purescript
Newtype PrecedenceType _
```

#### `PreconditionNotMetException`

``` purescript
newtype PreconditionNotMetException
  = PreconditionNotMetException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a precondition is not met.</p>

##### Instances
``` purescript
Newtype PreconditionNotMetException _
```

#### `ProviderDescription`

``` purescript
newtype ProviderDescription
  = ProviderDescription { "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderType" :: NullOrUndefined (IdentityProviderTypeType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for identity provider details.</p>

##### Instances
``` purescript
Newtype ProviderDescription _
```

#### `ProviderDetailsType`

``` purescript
newtype ProviderDetailsType
  = ProviderDetailsType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype ProviderDetailsType _
```

#### `ProviderNameType`

``` purescript
newtype ProviderNameType
  = ProviderNameType String
```

##### Instances
``` purescript
Newtype ProviderNameType _
```

#### `ProviderNameTypeV1`

``` purescript
newtype ProviderNameTypeV1
  = ProviderNameTypeV1 String
```

##### Instances
``` purescript
Newtype ProviderNameTypeV1 _
```

#### `ProviderUserIdentifierType`

``` purescript
newtype ProviderUserIdentifierType
  = ProviderUserIdentifierType { "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderAttributeName" :: NullOrUndefined (StringType), "ProviderAttributeValue" :: NullOrUndefined (StringType) }
```

<p>A container for information about an identity provider for a user pool.</p>

##### Instances
``` purescript
Newtype ProviderUserIdentifierType _
```

#### `ProvidersListType`

``` purescript
newtype ProvidersListType
  = ProvidersListType (Array ProviderDescription)
```

##### Instances
``` purescript
Newtype ProvidersListType _
```

#### `QueryLimit`

``` purescript
newtype QueryLimit
  = QueryLimit Int
```

##### Instances
``` purescript
Newtype QueryLimit _
```

#### `QueryLimitType`

``` purescript
newtype QueryLimitType
  = QueryLimitType Int
```

##### Instances
``` purescript
Newtype QueryLimitType _
```

#### `RedirectUrlType`

``` purescript
newtype RedirectUrlType
  = RedirectUrlType String
```

##### Instances
``` purescript
Newtype RedirectUrlType _
```

#### `RefreshTokenValidityType`

``` purescript
newtype RefreshTokenValidityType
  = RefreshTokenValidityType Int
```

##### Instances
``` purescript
Newtype RefreshTokenValidityType _
```

#### `ResendConfirmationCodeRequest`

``` purescript
newtype ResendConfirmationCodeRequest
  = ResendConfirmationCodeRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "UserContextData" :: NullOrUndefined (UserContextDataType), "Username" :: UsernameType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType) }
```

<p>Represents the request to resend the confirmation code.</p>

##### Instances
``` purescript
Newtype ResendConfirmationCodeRequest _
```

#### `ResendConfirmationCodeResponse`

``` purescript
newtype ResendConfirmationCodeResponse
  = ResendConfirmationCodeResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.</p>

##### Instances
``` purescript
Newtype ResendConfirmationCodeResponse _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service cannot find the requested resource.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceServerIdentifierType`

``` purescript
newtype ResourceServerIdentifierType
  = ResourceServerIdentifierType String
```

##### Instances
``` purescript
Newtype ResourceServerIdentifierType _
```

#### `ResourceServerNameType`

``` purescript
newtype ResourceServerNameType
  = ResourceServerNameType String
```

##### Instances
``` purescript
Newtype ResourceServerNameType _
```

#### `ResourceServerScopeDescriptionType`

``` purescript
newtype ResourceServerScopeDescriptionType
  = ResourceServerScopeDescriptionType String
```

##### Instances
``` purescript
Newtype ResourceServerScopeDescriptionType _
```

#### `ResourceServerScopeListType`

``` purescript
newtype ResourceServerScopeListType
  = ResourceServerScopeListType (Array ResourceServerScopeType)
```

##### Instances
``` purescript
Newtype ResourceServerScopeListType _
```

#### `ResourceServerScopeNameType`

``` purescript
newtype ResourceServerScopeNameType
  = ResourceServerScopeNameType String
```

##### Instances
``` purescript
Newtype ResourceServerScopeNameType _
```

#### `ResourceServerScopeType`

``` purescript
newtype ResourceServerScopeType
  = ResourceServerScopeType { "ScopeName" :: ResourceServerScopeNameType, "ScopeDescription" :: ResourceServerScopeDescriptionType }
```

<p>A resource server scope.</p>

##### Instances
``` purescript
Newtype ResourceServerScopeType _
```

#### `ResourceServerType`

``` purescript
newtype ResourceServerType
  = ResourceServerType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "Identifier" :: NullOrUndefined (ResourceServerIdentifierType), "Name" :: NullOrUndefined (ResourceServerNameType), "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

<p>A container for information about a resource server for a user pool.</p>

##### Instances
``` purescript
Newtype ResourceServerType _
```

#### `ResourceServersListType`

``` purescript
newtype ResourceServersListType
  = ResourceServersListType (Array ResourceServerType)
```

##### Instances
``` purescript
Newtype ResourceServersListType _
```

#### `RespondToAuthChallengeRequest`

``` purescript
newtype RespondToAuthChallengeRequest
  = RespondToAuthChallengeRequest { "ClientId" :: ClientIdType, "ChallengeName" :: ChallengeNameType, "Session" :: NullOrUndefined (SessionType), "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>The request to respond to an authentication challenge.</p>

##### Instances
``` purescript
Newtype RespondToAuthChallengeRequest _
```

#### `RespondToAuthChallengeResponse`

``` purescript
newtype RespondToAuthChallengeResponse
  = RespondToAuthChallengeResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>The response to respond to the authentication challenge.</p>

##### Instances
``` purescript
Newtype RespondToAuthChallengeResponse _
```

#### `RiskConfigurationType`

``` purescript
newtype RiskConfigurationType
  = RiskConfigurationType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientId" :: NullOrUndefined (ClientIdType), "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType), "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType), "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType), "LastModifiedDate" :: NullOrUndefined (DateType) }
```

<p>The risk configuration type.</p>

##### Instances
``` purescript
Newtype RiskConfigurationType _
```

#### `RiskDecisionType`

``` purescript
newtype RiskDecisionType
  = RiskDecisionType String
```

##### Instances
``` purescript
Newtype RiskDecisionType _
```

#### `RiskExceptionConfigurationType`

``` purescript
newtype RiskExceptionConfigurationType
  = RiskExceptionConfigurationType { "BlockedIPRangeList" :: NullOrUndefined (BlockedIPRangeListType), "SkippedIPRangeList" :: NullOrUndefined (SkippedIPRangeListType) }
```

<p>The type of the configuration to override the risk decision.</p>

##### Instances
``` purescript
Newtype RiskExceptionConfigurationType _
```

#### `RiskLevelType`

``` purescript
newtype RiskLevelType
  = RiskLevelType String
```

##### Instances
``` purescript
Newtype RiskLevelType _
```

#### `S3BucketType`

``` purescript
newtype S3BucketType
  = S3BucketType String
```

##### Instances
``` purescript
Newtype S3BucketType _
```

#### `SMSMfaSettingsType`

``` purescript
newtype SMSMfaSettingsType
  = SMSMfaSettingsType { "Enabled" :: NullOrUndefined (BooleanType), "PreferredMfa" :: NullOrUndefined (BooleanType) }
```

<p>The SMS multi-factor authentication (MFA) settings type.</p>

##### Instances
``` purescript
Newtype SMSMfaSettingsType _
```

#### `SchemaAttributeType`

``` purescript
newtype SchemaAttributeType
  = SchemaAttributeType { "Name" :: NullOrUndefined (CustomAttributeNameType), "AttributeDataType" :: NullOrUndefined (AttributeDataType), "DeveloperOnlyAttribute" :: NullOrUndefined (BooleanType), "Mutable" :: NullOrUndefined (BooleanType), "Required" :: NullOrUndefined (BooleanType), "NumberAttributeConstraints" :: NullOrUndefined (NumberAttributeConstraintsType), "StringAttributeConstraints" :: NullOrUndefined (StringAttributeConstraintsType) }
```

<p>Contains information about the schema attribute.</p>

##### Instances
``` purescript
Newtype SchemaAttributeType _
```

#### `SchemaAttributesListType`

``` purescript
newtype SchemaAttributesListType
  = SchemaAttributesListType (Array SchemaAttributeType)
```

##### Instances
``` purescript
Newtype SchemaAttributesListType _
```

#### `ScopeDoesNotExistException`

``` purescript
newtype ScopeDoesNotExistException
  = ScopeDoesNotExistException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified scope does not exist.</p>

##### Instances
``` purescript
Newtype ScopeDoesNotExistException _
```

#### `ScopeListType`

``` purescript
newtype ScopeListType
  = ScopeListType (Array ScopeType)
```

##### Instances
``` purescript
Newtype ScopeListType _
```

#### `ScopeType`

``` purescript
newtype ScopeType
  = ScopeType String
```

##### Instances
``` purescript
Newtype ScopeType _
```

#### `SearchPaginationTokenType`

``` purescript
newtype SearchPaginationTokenType
  = SearchPaginationTokenType String
```

##### Instances
``` purescript
Newtype SearchPaginationTokenType _
```

#### `SearchedAttributeNamesListType`

``` purescript
newtype SearchedAttributeNamesListType
  = SearchedAttributeNamesListType (Array AttributeNameType)
```

##### Instances
``` purescript
Newtype SearchedAttributeNamesListType _
```

#### `SecretCodeType`

``` purescript
newtype SecretCodeType
  = SecretCodeType String
```

##### Instances
``` purescript
Newtype SecretCodeType _
```

#### `SecretHashType`

``` purescript
newtype SecretHashType
  = SecretHashType String
```

##### Instances
``` purescript
Newtype SecretHashType _
```

#### `SessionType`

``` purescript
newtype SessionType
  = SessionType String
```

##### Instances
``` purescript
Newtype SessionType _
```

#### `SetRiskConfigurationRequest`

``` purescript
newtype SetRiskConfigurationRequest
  = SetRiskConfigurationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType), "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType), "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType), "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType) }
```

##### Instances
``` purescript
Newtype SetRiskConfigurationRequest _
```

#### `SetRiskConfigurationResponse`

``` purescript
newtype SetRiskConfigurationResponse
  = SetRiskConfigurationResponse { "RiskConfiguration" :: RiskConfigurationType }
```

##### Instances
``` purescript
Newtype SetRiskConfigurationResponse _
```

#### `SetUICustomizationRequest`

``` purescript
newtype SetUICustomizationRequest
  = SetUICustomizationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType), "CSS" :: NullOrUndefined (CSSType), "ImageFile" :: NullOrUndefined (ImageFileType) }
```

##### Instances
``` purescript
Newtype SetUICustomizationRequest _
```

#### `SetUICustomizationResponse`

``` purescript
newtype SetUICustomizationResponse
  = SetUICustomizationResponse { "UICustomization" :: UICustomizationType }
```

##### Instances
``` purescript
Newtype SetUICustomizationResponse _
```

#### `SetUserMFAPreferenceRequest`

``` purescript
newtype SetUserMFAPreferenceRequest
  = SetUserMFAPreferenceRequest { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType), "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType), "AccessToken" :: TokenModelType }
```

##### Instances
``` purescript
Newtype SetUserMFAPreferenceRequest _
```

#### `SetUserMFAPreferenceResponse`

``` purescript
newtype SetUserMFAPreferenceResponse
  = SetUserMFAPreferenceResponse {  }
```

##### Instances
``` purescript
Newtype SetUserMFAPreferenceResponse _
```

#### `SetUserPoolMfaConfigRequest`

``` purescript
newtype SetUserPoolMfaConfigRequest
  = SetUserPoolMfaConfigRequest { "UserPoolId" :: UserPoolIdType, "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

##### Instances
``` purescript
Newtype SetUserPoolMfaConfigRequest _
```

#### `SetUserPoolMfaConfigResponse`

``` purescript
newtype SetUserPoolMfaConfigResponse
  = SetUserPoolMfaConfigResponse { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

##### Instances
``` purescript
Newtype SetUserPoolMfaConfigResponse _
```

#### `SetUserSettingsRequest`

``` purescript
newtype SetUserSettingsRequest
  = SetUserSettingsRequest { "AccessToken" :: TokenModelType, "MFAOptions" :: MFAOptionListType }
```

<p>Represents the request to set user settings.</p>

##### Instances
``` purescript
Newtype SetUserSettingsRequest _
```

#### `SetUserSettingsResponse`

``` purescript
newtype SetUserSettingsResponse
  = SetUserSettingsResponse {  }
```

<p>The response from the server for a set user settings request.</p>

##### Instances
``` purescript
Newtype SetUserSettingsResponse _
```

#### `SignUpRequest`

``` purescript
newtype SignUpRequest
  = SignUpRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "Password" :: PasswordType, "UserAttributes" :: NullOrUndefined (AttributeListType), "ValidationData" :: NullOrUndefined (AttributeListType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Represents the request to register a user.</p>

##### Instances
``` purescript
Newtype SignUpRequest _
```

#### `SignUpResponse`

``` purescript
newtype SignUpResponse
  = SignUpResponse { "UserConfirmed" :: BooleanType, "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType), "UserSub" :: StringType }
```

<p>The response from the server for a registration request.</p>

##### Instances
``` purescript
Newtype SignUpResponse _
```

#### `SkippedIPRangeListType`

``` purescript
newtype SkippedIPRangeListType
  = SkippedIPRangeListType (Array StringType)
```

##### Instances
``` purescript
Newtype SkippedIPRangeListType _
```

#### `SmsConfigurationType`

``` purescript
newtype SmsConfigurationType
  = SmsConfigurationType { "SnsCallerArn" :: ArnType, "ExternalId" :: NullOrUndefined (StringType) }
```

<p>The SMS configuration type.</p>

##### Instances
``` purescript
Newtype SmsConfigurationType _
```

#### `SmsMfaConfigType`

``` purescript
newtype SmsMfaConfigType
  = SmsMfaConfigType { "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType) }
```

<p>The SMS text message multi-factor authentication (MFA) configuration type.</p>

##### Instances
``` purescript
Newtype SmsMfaConfigType _
```

#### `SmsVerificationMessageType`

``` purescript
newtype SmsVerificationMessageType
  = SmsVerificationMessageType String
```

##### Instances
``` purescript
Newtype SmsVerificationMessageType _
```

#### `SoftwareTokenMFANotFoundException`

``` purescript
newtype SoftwareTokenMFANotFoundException
  = SoftwareTokenMFANotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.</p>

##### Instances
``` purescript
Newtype SoftwareTokenMFANotFoundException _
```

#### `SoftwareTokenMFAUserCodeType`

``` purescript
newtype SoftwareTokenMFAUserCodeType
  = SoftwareTokenMFAUserCodeType String
```

##### Instances
``` purescript
Newtype SoftwareTokenMFAUserCodeType _
```

#### `SoftwareTokenMfaConfigType`

``` purescript
newtype SoftwareTokenMfaConfigType
  = SoftwareTokenMfaConfigType { "Enabled" :: NullOrUndefined (BooleanType) }
```

<p>The type used for enabling software token MFA at the user pool level.</p>

##### Instances
``` purescript
Newtype SoftwareTokenMfaConfigType _
```

#### `SoftwareTokenMfaSettingsType`

``` purescript
newtype SoftwareTokenMfaSettingsType
  = SoftwareTokenMfaSettingsType { "Enabled" :: NullOrUndefined (BooleanType), "PreferredMfa" :: NullOrUndefined (BooleanType) }
```

<p>The type used for enabling software token MFA at the user level.</p>

##### Instances
``` purescript
Newtype SoftwareTokenMfaSettingsType _
```

#### `StartUserImportJobRequest`

``` purescript
newtype StartUserImportJobRequest
  = StartUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to start the user import job.</p>

##### Instances
``` purescript
Newtype StartUserImportJobRequest _
```

#### `StartUserImportJobResponse`

``` purescript
newtype StartUserImportJobResponse
  = StartUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to start the user import job.</p>

##### Instances
``` purescript
Newtype StartUserImportJobResponse _
```

#### `StatusType`

``` purescript
newtype StatusType
  = StatusType String
```

##### Instances
``` purescript
Newtype StatusType _
```

#### `StopUserImportJobRequest`

``` purescript
newtype StopUserImportJobRequest
  = StopUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to stop the user import job.</p>

##### Instances
``` purescript
Newtype StopUserImportJobRequest _
```

#### `StopUserImportJobResponse`

``` purescript
newtype StopUserImportJobResponse
  = StopUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to stop the user import job.</p>

##### Instances
``` purescript
Newtype StopUserImportJobResponse _
```

#### `StringAttributeConstraintsType`

``` purescript
newtype StringAttributeConstraintsType
  = StringAttributeConstraintsType { "MinLength" :: NullOrUndefined (StringType), "MaxLength" :: NullOrUndefined (StringType) }
```

<p>The constraints associated with a string attribute.</p>

##### Instances
``` purescript
Newtype StringAttributeConstraintsType _
```

#### `StringType`

``` purescript
newtype StringType
  = StringType String
```

##### Instances
``` purescript
Newtype StringType _
```

#### `SupportedIdentityProvidersListType`

``` purescript
newtype SupportedIdentityProvidersListType
  = SupportedIdentityProvidersListType (Array ProviderNameType)
```

##### Instances
``` purescript
Newtype SupportedIdentityProvidersListType _
```

#### `TokenModelType`

``` purescript
newtype TokenModelType
  = TokenModelType String
```

##### Instances
``` purescript
Newtype TokenModelType _
```

#### `TooManyFailedAttemptsException`

``` purescript
newtype TooManyFailedAttemptsException
  = TooManyFailedAttemptsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).</p>

##### Instances
``` purescript
Newtype TooManyFailedAttemptsException _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user has made too many requests for a given operation.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UICustomizationType`

``` purescript
newtype UICustomizationType
  = UICustomizationType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientId" :: NullOrUndefined (ClientIdType), "ImageUrl" :: NullOrUndefined (ImageUrlType), "CSS" :: NullOrUndefined (CSSType), "CSSVersion" :: NullOrUndefined (CSSVersionType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for the UI customization information for a user pool's built-in app UI.</p>

##### Instances
``` purescript
Newtype UICustomizationType _
```

#### `UnexpectedLambdaException`

``` purescript
newtype UnexpectedLambdaException
  = UnexpectedLambdaException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.</p>

##### Instances
``` purescript
Newtype UnexpectedLambdaException _
```

#### `UnsupportedIdentityProviderException`

``` purescript
newtype UnsupportedIdentityProviderException
  = UnsupportedIdentityProviderException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified identifier is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedIdentityProviderException _
```

#### `UnsupportedUserStateException`

``` purescript
newtype UnsupportedUserStateException
  = UnsupportedUserStateException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>The request failed because the user is in an unsupported state.</p>

##### Instances
``` purescript
Newtype UnsupportedUserStateException _
```

#### `UpdateAuthEventFeedbackRequest`

``` purescript
newtype UpdateAuthEventFeedbackRequest
  = UpdateAuthEventFeedbackRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "EventId" :: EventIdType, "FeedbackToken" :: TokenModelType, "FeedbackValue" :: FeedbackValueType }
```

##### Instances
``` purescript
Newtype UpdateAuthEventFeedbackRequest _
```

#### `UpdateAuthEventFeedbackResponse`

``` purescript
newtype UpdateAuthEventFeedbackResponse
  = UpdateAuthEventFeedbackResponse {  }
```

##### Instances
``` purescript
Newtype UpdateAuthEventFeedbackResponse _
```

#### `UpdateDeviceStatusRequest`

``` purescript
newtype UpdateDeviceStatusRequest
  = UpdateDeviceStatusRequest { "AccessToken" :: TokenModelType, "DeviceKey" :: DeviceKeyType, "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType) }
```

<p>Represents the request to update the device status.</p>

##### Instances
``` purescript
Newtype UpdateDeviceStatusRequest _
```

#### `UpdateDeviceStatusResponse`

``` purescript
newtype UpdateDeviceStatusResponse
  = UpdateDeviceStatusResponse {  }
```

<p>The response to the request to update the device status.</p>

##### Instances
``` purescript
Newtype UpdateDeviceStatusResponse _
```

#### `UpdateGroupRequest`

``` purescript
newtype UpdateGroupRequest
  = UpdateGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType, "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType) }
```

##### Instances
``` purescript
Newtype UpdateGroupRequest _
```

#### `UpdateGroupResponse`

``` purescript
newtype UpdateGroupResponse
  = UpdateGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

##### Instances
``` purescript
Newtype UpdateGroupResponse _
```

#### `UpdateIdentityProviderRequest`

``` purescript
newtype UpdateIdentityProviderRequest
  = UpdateIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType, "ProviderDetails" :: NullOrUndefined (ProviderDetailsType), "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType) }
```

##### Instances
``` purescript
Newtype UpdateIdentityProviderRequest _
```

#### `UpdateIdentityProviderResponse`

``` purescript
newtype UpdateIdentityProviderResponse
  = UpdateIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

##### Instances
``` purescript
Newtype UpdateIdentityProviderResponse _
```

#### `UpdateResourceServerRequest`

``` purescript
newtype UpdateResourceServerRequest
  = UpdateResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType, "Name" :: ResourceServerNameType, "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

##### Instances
``` purescript
Newtype UpdateResourceServerRequest _
```

#### `UpdateResourceServerResponse`

``` purescript
newtype UpdateResourceServerResponse
  = UpdateResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

##### Instances
``` purescript
Newtype UpdateResourceServerResponse _
```

#### `UpdateUserAttributesRequest`

``` purescript
newtype UpdateUserAttributesRequest
  = UpdateUserAttributesRequest { "UserAttributes" :: AttributeListType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to update user attributes.</p>

##### Instances
``` purescript
Newtype UpdateUserAttributesRequest _
```

#### `UpdateUserAttributesResponse`

``` purescript
newtype UpdateUserAttributesResponse
  = UpdateUserAttributesResponse { "CodeDeliveryDetailsList" :: NullOrUndefined (CodeDeliveryDetailsListType) }
```

<p>Represents the response from the server for the request to update user attributes.</p>

##### Instances
``` purescript
Newtype UpdateUserAttributesResponse _
```

#### `UpdateUserPoolClientRequest`

``` purescript
newtype UpdateUserPoolClientRequest
  = UpdateUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "ClientName" :: NullOrUndefined (ClientNameType), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Represents the request to update the user pool client.</p>

##### Instances
``` purescript
Newtype UpdateUserPoolClientRequest _
```

#### `UpdateUserPoolClientResponse`

``` purescript
newtype UpdateUserPoolClientResponse
  = UpdateUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server to the request to update the user pool client.</p>

##### Instances
``` purescript
Newtype UpdateUserPoolClientResponse _
```

#### `UpdateUserPoolRequest`

``` purescript
newtype UpdateUserPoolRequest
  = UpdateUserPoolRequest { "UserPoolId" :: UserPoolIdType, "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>Represents the request to update the user pool.</p>

##### Instances
``` purescript
Newtype UpdateUserPoolRequest _
```

#### `UpdateUserPoolResponse`

``` purescript
newtype UpdateUserPoolResponse
  = UpdateUserPoolResponse {  }
```

<p>Represents the response from the server when you make a request to update the user pool.</p>

##### Instances
``` purescript
Newtype UpdateUserPoolResponse _
```

#### `UserContextDataType`

``` purescript
newtype UserContextDataType
  = UserContextDataType { "EncodedData" :: NullOrUndefined (StringType) }
```

<p>Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>

##### Instances
``` purescript
Newtype UserContextDataType _
```

#### `UserFilterType`

``` purescript
newtype UserFilterType
  = UserFilterType String
```

##### Instances
``` purescript
Newtype UserFilterType _
```

#### `UserImportInProgressException`

``` purescript
newtype UserImportInProgressException
  = UserImportInProgressException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.</p>

##### Instances
``` purescript
Newtype UserImportInProgressException _
```

#### `UserImportJobIdType`

``` purescript
newtype UserImportJobIdType
  = UserImportJobIdType String
```

##### Instances
``` purescript
Newtype UserImportJobIdType _
```

#### `UserImportJobNameType`

``` purescript
newtype UserImportJobNameType
  = UserImportJobNameType String
```

##### Instances
``` purescript
Newtype UserImportJobNameType _
```

#### `UserImportJobStatusType`

``` purescript
newtype UserImportJobStatusType
  = UserImportJobStatusType String
```

##### Instances
``` purescript
Newtype UserImportJobStatusType _
```

#### `UserImportJobType`

``` purescript
newtype UserImportJobType
  = UserImportJobType { "JobName" :: NullOrUndefined (UserImportJobNameType), "JobId" :: NullOrUndefined (UserImportJobIdType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "PreSignedUrl" :: NullOrUndefined (PreSignedUrlType), "CreationDate" :: NullOrUndefined (DateType), "StartDate" :: NullOrUndefined (DateType), "CompletionDate" :: NullOrUndefined (DateType), "Status" :: NullOrUndefined (UserImportJobStatusType), "CloudWatchLogsRoleArn" :: NullOrUndefined (ArnType), "ImportedUsers" :: NullOrUndefined (LongType), "SkippedUsers" :: NullOrUndefined (LongType), "FailedUsers" :: NullOrUndefined (LongType), "CompletionMessage" :: NullOrUndefined (CompletionMessageType) }
```

<p>The user import job type.</p>

##### Instances
``` purescript
Newtype UserImportJobType _
```

#### `UserImportJobsListType`

``` purescript
newtype UserImportJobsListType
  = UserImportJobsListType (Array UserImportJobType)
```

##### Instances
``` purescript
Newtype UserImportJobsListType _
```

#### `UserLambdaValidationException`

``` purescript
newtype UserLambdaValidationException
  = UserLambdaValidationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.</p>

##### Instances
``` purescript
Newtype UserLambdaValidationException _
```

#### `UserMFASettingListType`

``` purescript
newtype UserMFASettingListType
  = UserMFASettingListType (Array StringType)
```

##### Instances
``` purescript
Newtype UserMFASettingListType _
```

#### `UserNotConfirmedException`

``` purescript
newtype UserNotConfirmedException
  = UserNotConfirmedException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not confirmed successfully.</p>

##### Instances
``` purescript
Newtype UserNotConfirmedException _
```

#### `UserNotFoundException`

``` purescript
newtype UserNotFoundException
  = UserNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not found.</p>

##### Instances
``` purescript
Newtype UserNotFoundException _
```

#### `UserPoolAddOnNotEnabledException`

``` purescript
newtype UserPoolAddOnNotEnabledException
  = UserPoolAddOnNotEnabledException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when user pool add-ons are not enabled.</p>

##### Instances
``` purescript
Newtype UserPoolAddOnNotEnabledException _
```

#### `UserPoolAddOnsType`

``` purescript
newtype UserPoolAddOnsType
  = UserPoolAddOnsType { "AdvancedSecurityMode" :: AdvancedSecurityModeType }
```

<p>The user pool add-ons type.</p>

##### Instances
``` purescript
Newtype UserPoolAddOnsType _
```

#### `UserPoolClientDescription`

``` purescript
newtype UserPoolClientDescription
  = UserPoolClientDescription { "ClientId" :: NullOrUndefined (ClientIdType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientName" :: NullOrUndefined (ClientNameType) }
```

<p>The description of the user pool client.</p>

##### Instances
``` purescript
Newtype UserPoolClientDescription _
```

#### `UserPoolClientListType`

``` purescript
newtype UserPoolClientListType
  = UserPoolClientListType (Array UserPoolClientDescription)
```

##### Instances
``` purescript
Newtype UserPoolClientListType _
```

#### `UserPoolClientType`

``` purescript
newtype UserPoolClientType
  = UserPoolClientType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientName" :: NullOrUndefined (ClientNameType), "ClientId" :: NullOrUndefined (ClientIdType), "ClientSecret" :: NullOrUndefined (ClientSecretType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Contains information about a user pool client.</p>

##### Instances
``` purescript
Newtype UserPoolClientType _
```

#### `UserPoolDescriptionType`

``` purescript
newtype UserPoolDescriptionType
  = UserPoolDescriptionType { "Id" :: NullOrUndefined (UserPoolIdType), "Name" :: NullOrUndefined (UserPoolNameType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "Status" :: NullOrUndefined (StatusType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A user pool description.</p>

##### Instances
``` purescript
Newtype UserPoolDescriptionType _
```

#### `UserPoolIdType`

``` purescript
newtype UserPoolIdType
  = UserPoolIdType String
```

##### Instances
``` purescript
Newtype UserPoolIdType _
```

#### `UserPoolListType`

``` purescript
newtype UserPoolListType
  = UserPoolListType (Array UserPoolDescriptionType)
```

##### Instances
``` purescript
Newtype UserPoolListType _
```

#### `UserPoolMfaType`

``` purescript
newtype UserPoolMfaType
  = UserPoolMfaType String
```

##### Instances
``` purescript
Newtype UserPoolMfaType _
```

#### `UserPoolNameType`

``` purescript
newtype UserPoolNameType
  = UserPoolNameType String
```

##### Instances
``` purescript
Newtype UserPoolNameType _
```

#### `UserPoolPolicyType`

``` purescript
newtype UserPoolPolicyType
  = UserPoolPolicyType { "PasswordPolicy" :: NullOrUndefined (PasswordPolicyType) }
```

<p>The policy associated with a user pool.</p>

##### Instances
``` purescript
Newtype UserPoolPolicyType _
```

#### `UserPoolTaggingException`

``` purescript
newtype UserPoolTaggingException
  = UserPoolTaggingException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user pool tag cannot be set or updated.</p>

##### Instances
``` purescript
Newtype UserPoolTaggingException _
```

#### `UserPoolTagsType`

``` purescript
newtype UserPoolTagsType
  = UserPoolTagsType (Map StringType StringType)
```

##### Instances
``` purescript
Newtype UserPoolTagsType _
```

#### `UserPoolType`

``` purescript
newtype UserPoolType
  = UserPoolType { "Id" :: NullOrUndefined (UserPoolIdType), "Name" :: NullOrUndefined (UserPoolNameType), "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "Status" :: NullOrUndefined (StatusType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType), "SchemaAttributes" :: NullOrUndefined (SchemaAttributesListType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "AliasAttributes" :: NullOrUndefined (AliasAttributesListType), "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EstimatedNumberOfUsers" :: NullOrUndefined (IntegerType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "SmsConfigurationFailure" :: NullOrUndefined (StringType), "EmailConfigurationFailure" :: NullOrUndefined (StringType), "Domain" :: NullOrUndefined (DomainType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>A container for information about the user pool.</p>

##### Instances
``` purescript
Newtype UserPoolType _
```

#### `UserStatusType`

``` purescript
newtype UserStatusType
  = UserStatusType String
```

##### Instances
``` purescript
Newtype UserStatusType _
```

#### `UserType`

``` purescript
newtype UserType
  = UserType { "Username" :: NullOrUndefined (UsernameType), "Attributes" :: NullOrUndefined (AttributeListType), "UserCreateDate" :: NullOrUndefined (DateType), "UserLastModifiedDate" :: NullOrUndefined (DateType), "Enabled" :: NullOrUndefined (BooleanType), "UserStatus" :: NullOrUndefined (UserStatusType), "MFAOptions" :: NullOrUndefined (MFAOptionListType) }
```

<p>The user type.</p>

##### Instances
``` purescript
Newtype UserType _
```

#### `UsernameAttributeType`

``` purescript
newtype UsernameAttributeType
  = UsernameAttributeType String
```

##### Instances
``` purescript
Newtype UsernameAttributeType _
```

#### `UsernameAttributesListType`

``` purescript
newtype UsernameAttributesListType
  = UsernameAttributesListType (Array UsernameAttributeType)
```

##### Instances
``` purescript
Newtype UsernameAttributesListType _
```

#### `UsernameExistsException`

``` purescript
newtype UsernameExistsException
  = UsernameExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.</p>

##### Instances
``` purescript
Newtype UsernameExistsException _
```

#### `UsernameType`

``` purescript
newtype UsernameType
  = UsernameType String
```

##### Instances
``` purescript
Newtype UsernameType _
```

#### `UsersListType`

``` purescript
newtype UsersListType
  = UsersListType (Array UserType)
```

##### Instances
``` purescript
Newtype UsersListType _
```

#### `VerificationMessageTemplateType`

``` purescript
newtype VerificationMessageTemplateType
  = VerificationMessageTemplateType { "SmsMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType), "EmailMessageByLink" :: NullOrUndefined (EmailVerificationMessageByLinkType), "EmailSubjectByLink" :: NullOrUndefined (EmailVerificationSubjectByLinkType), "DefaultEmailOption" :: NullOrUndefined (DefaultEmailOptionType) }
```

<p>The template for verification messages.</p>

##### Instances
``` purescript
Newtype VerificationMessageTemplateType _
```

#### `VerifiedAttributeType`

``` purescript
newtype VerifiedAttributeType
  = VerifiedAttributeType String
```

##### Instances
``` purescript
Newtype VerifiedAttributeType _
```

#### `VerifiedAttributesListType`

``` purescript
newtype VerifiedAttributesListType
  = VerifiedAttributesListType (Array VerifiedAttributeType)
```

##### Instances
``` purescript
Newtype VerifiedAttributesListType _
```

#### `VerifySoftwareTokenRequest`

``` purescript
newtype VerifySoftwareTokenRequest
  = VerifySoftwareTokenRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "Session" :: NullOrUndefined (SessionType), "UserCode" :: SoftwareTokenMFAUserCodeType, "FriendlyDeviceName" :: NullOrUndefined (StringType) }
```

##### Instances
``` purescript
Newtype VerifySoftwareTokenRequest _
```

#### `VerifySoftwareTokenResponse`

``` purescript
newtype VerifySoftwareTokenResponse
  = VerifySoftwareTokenResponse { "Status" :: NullOrUndefined (VerifySoftwareTokenResponseType), "Session" :: NullOrUndefined (SessionType) }
```

##### Instances
``` purescript
Newtype VerifySoftwareTokenResponse _
```

#### `VerifySoftwareTokenResponseType`

``` purescript
newtype VerifySoftwareTokenResponseType
  = VerifySoftwareTokenResponseType String
```

##### Instances
``` purescript
Newtype VerifySoftwareTokenResponseType _
```

#### `VerifyUserAttributeRequest`

``` purescript
newtype VerifyUserAttributeRequest
  = VerifyUserAttributeRequest { "AccessToken" :: TokenModelType, "AttributeName" :: AttributeNameType, "Code" :: ConfirmationCodeType }
```

<p>Represents the request to verify user attributes.</p>

##### Instances
``` purescript
Newtype VerifyUserAttributeRequest _
```

#### `VerifyUserAttributeResponse`

``` purescript
newtype VerifyUserAttributeResponse
  = VerifyUserAttributeResponse {  }
```

<p>A container representing the response from the server from the request to verify user attributes.</p>

##### Instances
``` purescript
Newtype VerifyUserAttributeResponse _
```


