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

#### `AccountTakeoverActionNotifyType`

``` purescript
newtype AccountTakeoverActionNotifyType
  = AccountTakeoverActionNotifyType Boolean
```

#### `AccountTakeoverActionType`

``` purescript
newtype AccountTakeoverActionType
  = AccountTakeoverActionType { "Notify" :: AccountTakeoverActionNotifyType, "EventAction" :: AccountTakeoverEventActionType }
```

<p>Account takeover action type.</p>

#### `AccountTakeoverActionsType`

``` purescript
newtype AccountTakeoverActionsType
  = AccountTakeoverActionsType { "LowAction" :: NullOrUndefined (AccountTakeoverActionType), "MediumAction" :: NullOrUndefined (AccountTakeoverActionType), "HighAction" :: NullOrUndefined (AccountTakeoverActionType) }
```

<p>Account takeover actions type.</p>

#### `AccountTakeoverEventActionType`

``` purescript
newtype AccountTakeoverEventActionType
  = AccountTakeoverEventActionType String
```

#### `AccountTakeoverRiskConfigurationType`

``` purescript
newtype AccountTakeoverRiskConfigurationType
  = AccountTakeoverRiskConfigurationType { "NotifyConfiguration" :: NullOrUndefined (NotifyConfigurationType), "Actions" :: AccountTakeoverActionsType }
```

<p>Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.</p>

#### `AddCustomAttributesRequest`

``` purescript
newtype AddCustomAttributesRequest
  = AddCustomAttributesRequest { "UserPoolId" :: UserPoolIdType, "CustomAttributes" :: CustomAttributesListType }
```

<p>Represents the request to add custom attributes.</p>

#### `AddCustomAttributesResponse`

``` purescript
newtype AddCustomAttributesResponse
  = AddCustomAttributesResponse {  }
```

<p>Represents the response from the server for the request to add custom attributes.</p>

#### `AdminAddUserToGroupRequest`

``` purescript
newtype AdminAddUserToGroupRequest
  = AdminAddUserToGroupRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "GroupName" :: GroupNameType }
```

#### `AdminConfirmSignUpRequest`

``` purescript
newtype AdminConfirmSignUpRequest
  = AdminConfirmSignUpRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to confirm user registration.</p>

#### `AdminConfirmSignUpResponse`

``` purescript
newtype AdminConfirmSignUpResponse
  = AdminConfirmSignUpResponse {  }
```

<p>Represents the response from the server for the request to confirm registration.</p>

#### `AdminCreateUserConfigType`

``` purescript
newtype AdminCreateUserConfigType
  = AdminCreateUserConfigType { "AllowAdminCreateUserOnly" :: NullOrUndefined (BooleanType), "UnusedAccountValidityDays" :: NullOrUndefined (AdminCreateUserUnusedAccountValidityDaysType), "InviteMessageTemplate" :: NullOrUndefined (MessageTemplateType) }
```

<p>The configuration for creating a new user profile.</p>

#### `AdminCreateUserRequest`

``` purescript
newtype AdminCreateUserRequest
  = AdminCreateUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributes" :: NullOrUndefined (AttributeListType), "ValidationData" :: NullOrUndefined (AttributeListType), "TemporaryPassword" :: NullOrUndefined (PasswordType), "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation), "MessageAction" :: NullOrUndefined (MessageActionType), "DesiredDeliveryMediums" :: NullOrUndefined (DeliveryMediumListType) }
```

<p>Represents the request to create a user in the specified user pool.</p>

#### `AdminCreateUserResponse`

``` purescript
newtype AdminCreateUserResponse
  = AdminCreateUserResponse { "User" :: NullOrUndefined (UserType) }
```

<p>Represents the response from the server to the request to create the user.</p>

#### `AdminCreateUserUnusedAccountValidityDaysType`

``` purescript
newtype AdminCreateUserUnusedAccountValidityDaysType
  = AdminCreateUserUnusedAccountValidityDaysType Int
```

#### `AdminDeleteUserAttributesRequest`

``` purescript
newtype AdminDeleteUserAttributesRequest
  = AdminDeleteUserAttributesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributeNames" :: AttributeNameListType }
```

<p>Represents the request to delete user attributes as an administrator.</p>

#### `AdminDeleteUserAttributesResponse`

``` purescript
newtype AdminDeleteUserAttributesResponse
  = AdminDeleteUserAttributesResponse {  }
```

<p>Represents the response received from the server for a request to delete user attributes.</p>

#### `AdminDeleteUserRequest`

``` purescript
newtype AdminDeleteUserRequest
  = AdminDeleteUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to delete a user as an administrator.</p>

#### `AdminDisableProviderForUserRequest`

``` purescript
newtype AdminDisableProviderForUserRequest
  = AdminDisableProviderForUserRequest { "UserPoolId" :: StringType, "User" :: ProviderUserIdentifierType }
```

#### `AdminDisableProviderForUserResponse`

``` purescript
newtype AdminDisableProviderForUserResponse
  = AdminDisableProviderForUserResponse {  }
```

#### `AdminDisableUserRequest`

``` purescript
newtype AdminDisableUserRequest
  = AdminDisableUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to disable any user as an administrator.</p>

#### `AdminDisableUserResponse`

``` purescript
newtype AdminDisableUserResponse
  = AdminDisableUserResponse {  }
```

<p>Represents the response received from the server to disable the user as an administrator.</p>

#### `AdminEnableUserRequest`

``` purescript
newtype AdminEnableUserRequest
  = AdminEnableUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request that enables the user as an administrator.</p>

#### `AdminEnableUserResponse`

``` purescript
newtype AdminEnableUserResponse
  = AdminEnableUserResponse {  }
```

<p>Represents the response from the server for the request to enable a user as an administrator.</p>

#### `AdminForgetDeviceRequest`

``` purescript
newtype AdminForgetDeviceRequest
  = AdminForgetDeviceRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "DeviceKey" :: DeviceKeyType }
```

<p>Sends the forgot device request, as an administrator.</p>

#### `AdminGetDeviceRequest`

``` purescript
newtype AdminGetDeviceRequest
  = AdminGetDeviceRequest { "DeviceKey" :: DeviceKeyType, "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to get the device, as an administrator.</p>

#### `AdminGetDeviceResponse`

``` purescript
newtype AdminGetDeviceResponse
  = AdminGetDeviceResponse { "Device" :: DeviceType }
```

<p>Gets the device response, as an administrator.</p>

#### `AdminGetUserRequest`

``` purescript
newtype AdminGetUserRequest
  = AdminGetUserRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to get the specified user as an administrator.</p>

#### `AdminGetUserResponse`

``` purescript
newtype AdminGetUserResponse
  = AdminGetUserResponse { "Username" :: UsernameType, "UserAttributes" :: NullOrUndefined (AttributeListType), "UserCreateDate" :: NullOrUndefined (DateType), "UserLastModifiedDate" :: NullOrUndefined (DateType), "Enabled" :: NullOrUndefined (BooleanType), "UserStatus" :: NullOrUndefined (UserStatusType), "MFAOptions" :: NullOrUndefined (MFAOptionListType), "PreferredMfaSetting" :: NullOrUndefined (StringType), "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType) }
```

<p>Represents the response from the server from the request to get the specified user as an administrator.</p>

#### `AdminInitiateAuthRequest`

``` purescript
newtype AdminInitiateAuthRequest
  = AdminInitiateAuthRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "AuthFlow" :: AuthFlowType, "AuthParameters" :: NullOrUndefined (AuthParametersType), "ClientMetadata" :: NullOrUndefined (ClientMetadataType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "ContextData" :: NullOrUndefined (ContextDataType) }
```

<p>Initiates the authorization request, as an administrator.</p>

#### `AdminInitiateAuthResponse`

``` purescript
newtype AdminInitiateAuthResponse
  = AdminInitiateAuthResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Initiates the authentication response, as an administrator.</p>

#### `AdminLinkProviderForUserRequest`

``` purescript
newtype AdminLinkProviderForUserRequest
  = AdminLinkProviderForUserRequest { "UserPoolId" :: StringType, "DestinationUser" :: ProviderUserIdentifierType, "SourceUser" :: ProviderUserIdentifierType }
```

#### `AdminLinkProviderForUserResponse`

``` purescript
newtype AdminLinkProviderForUserResponse
  = AdminLinkProviderForUserResponse {  }
```

#### `AdminListDevicesRequest`

``` purescript
newtype AdminListDevicesRequest
  = AdminListDevicesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the request to list devices, as an administrator.</p>

#### `AdminListDevicesResponse`

``` purescript
newtype AdminListDevicesResponse
  = AdminListDevicesResponse { "Devices" :: NullOrUndefined (DeviceListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Lists the device's response, as an administrator.</p>

#### `AdminListGroupsForUserRequest`

``` purescript
newtype AdminListGroupsForUserRequest
  = AdminListGroupsForUserRequest { "Username" :: UsernameType, "UserPoolId" :: UserPoolIdType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `AdminListGroupsForUserResponse`

``` purescript
newtype AdminListGroupsForUserResponse
  = AdminListGroupsForUserResponse { "Groups" :: NullOrUndefined (GroupListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `AdminListUserAuthEventsRequest`

``` purescript
newtype AdminListUserAuthEventsRequest
  = AdminListUserAuthEventsRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "MaxResults" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `AdminListUserAuthEventsResponse`

``` purescript
newtype AdminListUserAuthEventsResponse
  = AdminListUserAuthEventsResponse { "AuthEvents" :: NullOrUndefined (AuthEventsType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `AdminRemoveUserFromGroupRequest`

``` purescript
newtype AdminRemoveUserFromGroupRequest
  = AdminRemoveUserFromGroupRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "GroupName" :: GroupNameType }
```

#### `AdminResetUserPasswordRequest`

``` purescript
newtype AdminResetUserPasswordRequest
  = AdminResetUserPasswordRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>Represents the request to reset a user's password as an administrator.</p>

#### `AdminResetUserPasswordResponse`

``` purescript
newtype AdminResetUserPasswordResponse
  = AdminResetUserPasswordResponse {  }
```

<p>Represents the response from the server to reset a user password as an administrator.</p>

#### `AdminRespondToAuthChallengeRequest`

``` purescript
newtype AdminRespondToAuthChallengeRequest
  = AdminRespondToAuthChallengeRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "ChallengeName" :: ChallengeNameType, "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType), "Session" :: NullOrUndefined (SessionType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "ContextData" :: NullOrUndefined (ContextDataType) }
```

<p>The request to respond to the authentication challenge, as an administrator.</p>

#### `AdminRespondToAuthChallengeResponse`

``` purescript
newtype AdminRespondToAuthChallengeResponse
  = AdminRespondToAuthChallengeResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Responds to the authentication challenge, as an administrator.</p>

#### `AdminSetUserMFAPreferenceRequest`

``` purescript
newtype AdminSetUserMFAPreferenceRequest
  = AdminSetUserMFAPreferenceRequest { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType), "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType), "Username" :: UsernameType, "UserPoolId" :: UserPoolIdType }
```

#### `AdminSetUserMFAPreferenceResponse`

``` purescript
newtype AdminSetUserMFAPreferenceResponse
  = AdminSetUserMFAPreferenceResponse {  }
```

#### `AdminSetUserSettingsRequest`

``` purescript
newtype AdminSetUserSettingsRequest
  = AdminSetUserSettingsRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "MFAOptions" :: MFAOptionListType }
```

<p>Represents the request to set user settings as an administrator.</p>

#### `AdminSetUserSettingsResponse`

``` purescript
newtype AdminSetUserSettingsResponse
  = AdminSetUserSettingsResponse {  }
```

<p>Represents the response from the server to set user settings as an administrator.</p>

#### `AdminUpdateAuthEventFeedbackRequest`

``` purescript
newtype AdminUpdateAuthEventFeedbackRequest
  = AdminUpdateAuthEventFeedbackRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "EventId" :: EventIdType, "FeedbackValue" :: FeedbackValueType }
```

#### `AdminUpdateAuthEventFeedbackResponse`

``` purescript
newtype AdminUpdateAuthEventFeedbackResponse
  = AdminUpdateAuthEventFeedbackResponse {  }
```

#### `AdminUpdateDeviceStatusRequest`

``` purescript
newtype AdminUpdateDeviceStatusRequest
  = AdminUpdateDeviceStatusRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "DeviceKey" :: DeviceKeyType, "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType) }
```

<p>The request to update the device status, as an administrator.</p>

#### `AdminUpdateDeviceStatusResponse`

``` purescript
newtype AdminUpdateDeviceStatusResponse
  = AdminUpdateDeviceStatusResponse {  }
```

<p>The status response from the request to update the device, as an administrator.</p>

#### `AdminUpdateUserAttributesRequest`

``` purescript
newtype AdminUpdateUserAttributesRequest
  = AdminUpdateUserAttributesRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "UserAttributes" :: AttributeListType }
```

<p>Represents the request to update the user's attributes as an administrator.</p>

#### `AdminUpdateUserAttributesResponse`

``` purescript
newtype AdminUpdateUserAttributesResponse
  = AdminUpdateUserAttributesResponse {  }
```

<p>Represents the response from the server for the request to update user attributes as an administrator.</p>

#### `AdminUserGlobalSignOutRequest`

``` purescript
newtype AdminUserGlobalSignOutRequest
  = AdminUserGlobalSignOutRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType }
```

<p>The request to sign out of all devices, as an administrator.</p>

#### `AdminUserGlobalSignOutResponse`

``` purescript
newtype AdminUserGlobalSignOutResponse
  = AdminUserGlobalSignOutResponse {  }
```

<p>The global sign-out response, as an administrator.</p>

#### `AdvancedSecurityModeType`

``` purescript
newtype AdvancedSecurityModeType
  = AdvancedSecurityModeType String
```

#### `AliasAttributeType`

``` purescript
newtype AliasAttributeType
  = AliasAttributeType String
```

#### `AliasAttributesListType`

``` purescript
newtype AliasAttributesListType
  = AliasAttributesListType (Array AliasAttributeType)
```

#### `AliasExistsException`

``` purescript
newtype AliasExistsException
  = AliasExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.</p>

#### `AnalyticsConfigurationType`

``` purescript
newtype AnalyticsConfigurationType
  = AnalyticsConfigurationType { "ApplicationId" :: HexStringType, "RoleArn" :: ArnType, "ExternalId" :: StringType, "UserDataShared" :: NullOrUndefined (BooleanType) }
```

<p>The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.</p>

#### `AnalyticsMetadataType`

``` purescript
newtype AnalyticsMetadataType
  = AnalyticsMetadataType { "AnalyticsEndpointId" :: NullOrUndefined (StringType) }
```

<p>An Amazon Pinpoint analytics endpoint.</p> <p>An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.</p>

#### `ArnType`

``` purescript
newtype ArnType
  = ArnType String
```

#### `AssociateSoftwareTokenRequest`

``` purescript
newtype AssociateSoftwareTokenRequest
  = AssociateSoftwareTokenRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "Session" :: NullOrUndefined (SessionType) }
```

#### `AssociateSoftwareTokenResponse`

``` purescript
newtype AssociateSoftwareTokenResponse
  = AssociateSoftwareTokenResponse { "SecretCode" :: NullOrUndefined (SecretCodeType), "Session" :: NullOrUndefined (SessionType) }
```

#### `AttributeDataType`

``` purescript
newtype AttributeDataType
  = AttributeDataType String
```

#### `AttributeListType`

``` purescript
newtype AttributeListType
  = AttributeListType (Array AttributeType)
```

#### `AttributeMappingKeyType`

``` purescript
newtype AttributeMappingKeyType
  = AttributeMappingKeyType String
```

#### `AttributeMappingType`

``` purescript
newtype AttributeMappingType
  = AttributeMappingType (Map AttributeMappingKeyType StringType)
```

#### `AttributeNameListType`

``` purescript
newtype AttributeNameListType
  = AttributeNameListType (Array AttributeNameType)
```

#### `AttributeNameType`

``` purescript
newtype AttributeNameType
  = AttributeNameType String
```

#### `AttributeType`

``` purescript
newtype AttributeType
  = AttributeType { "Name" :: AttributeNameType, "Value" :: NullOrUndefined (AttributeValueType) }
```

<p>Specifies whether the attribute is standard or custom.</p>

#### `AttributeValueType`

``` purescript
newtype AttributeValueType
  = AttributeValueType String
```

#### `AuthEventType`

``` purescript
newtype AuthEventType
  = AuthEventType { "EventId" :: NullOrUndefined (StringType), "EventType" :: NullOrUndefined (EventType), "CreationDate" :: NullOrUndefined (DateType), "EventResponse" :: NullOrUndefined (EventResponseType), "EventRisk" :: NullOrUndefined (EventRiskType), "ChallengeResponses" :: NullOrUndefined (ChallengeResponseListType), "EventContextData" :: NullOrUndefined (EventContextDataType), "EventFeedback" :: NullOrUndefined (EventFeedbackType) }
```

<p>The authentication event type.</p>

#### `AuthEventsType`

``` purescript
newtype AuthEventsType
  = AuthEventsType (Array AuthEventType)
```

#### `AuthFlowType`

``` purescript
newtype AuthFlowType
  = AuthFlowType String
```

#### `AuthParametersType`

``` purescript
newtype AuthParametersType
  = AuthParametersType (Map StringType StringType)
```

#### `AuthenticationResultType`

``` purescript
newtype AuthenticationResultType
  = AuthenticationResultType { "AccessToken" :: NullOrUndefined (TokenModelType), "ExpiresIn" :: NullOrUndefined (IntegerType), "TokenType" :: NullOrUndefined (StringType), "RefreshToken" :: NullOrUndefined (TokenModelType), "IdToken" :: NullOrUndefined (TokenModelType), "NewDeviceMetadata" :: NullOrUndefined (NewDeviceMetadataType) }
```

<p>The authentication result.</p>

#### `BlockedIPRangeListType`

``` purescript
newtype BlockedIPRangeListType
  = BlockedIPRangeListType (Array StringType)
```

#### `BooleanType`

``` purescript
newtype BooleanType
  = BooleanType Boolean
```

#### `CSSType`

``` purescript
newtype CSSType
  = CSSType String
```

#### `CSSVersionType`

``` purescript
newtype CSSVersionType
  = CSSVersionType String
```

#### `CallbackURLsListType`

``` purescript
newtype CallbackURLsListType
  = CallbackURLsListType (Array RedirectUrlType)
```

#### `ChallengeName`

``` purescript
newtype ChallengeName
  = ChallengeName String
```

#### `ChallengeNameType`

``` purescript
newtype ChallengeNameType
  = ChallengeNameType String
```

#### `ChallengeParametersType`

``` purescript
newtype ChallengeParametersType
  = ChallengeParametersType (Map StringType StringType)
```

#### `ChallengeResponse`

``` purescript
newtype ChallengeResponse
  = ChallengeResponse String
```

#### `ChallengeResponseListType`

``` purescript
newtype ChallengeResponseListType
  = ChallengeResponseListType (Array ChallengeResponseType)
```

#### `ChallengeResponseType`

``` purescript
newtype ChallengeResponseType
  = ChallengeResponseType { "ChallengeName" :: NullOrUndefined (ChallengeName), "ChallengeResponse" :: NullOrUndefined (ChallengeResponse) }
```

<p>The challenge response type.</p>

#### `ChallengeResponsesType`

``` purescript
newtype ChallengeResponsesType
  = ChallengeResponsesType (Map StringType StringType)
```

#### `ChangePasswordRequest`

``` purescript
newtype ChangePasswordRequest
  = ChangePasswordRequest { "PreviousPassword" :: PasswordType, "ProposedPassword" :: PasswordType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to change a user password.</p>

#### `ChangePasswordResponse`

``` purescript
newtype ChangePasswordResponse
  = ChangePasswordResponse {  }
```

<p>The response from the server to the change password request.</p>

#### `ClientIdType`

``` purescript
newtype ClientIdType
  = ClientIdType String
```

#### `ClientMetadataType`

``` purescript
newtype ClientMetadataType
  = ClientMetadataType (Map StringType StringType)
```

#### `ClientNameType`

``` purescript
newtype ClientNameType
  = ClientNameType String
```

#### `ClientPermissionListType`

``` purescript
newtype ClientPermissionListType
  = ClientPermissionListType (Array ClientPermissionType)
```

#### `ClientPermissionType`

``` purescript
newtype ClientPermissionType
  = ClientPermissionType String
```

#### `ClientSecretType`

``` purescript
newtype ClientSecretType
  = ClientSecretType String
```

#### `CodeDeliveryDetailsListType`

``` purescript
newtype CodeDeliveryDetailsListType
  = CodeDeliveryDetailsListType (Array CodeDeliveryDetailsType)
```

#### `CodeDeliveryDetailsType`

``` purescript
newtype CodeDeliveryDetailsType
  = CodeDeliveryDetailsType { "Destination" :: NullOrUndefined (StringType), "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType), "AttributeName" :: NullOrUndefined (AttributeNameType) }
```

<p>The code delivery details being returned from the server.</p>

#### `CodeDeliveryFailureException`

``` purescript
newtype CodeDeliveryFailureException
  = CodeDeliveryFailureException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a verification code fails to deliver successfully.</p>

#### `CodeMismatchException`

``` purescript
newtype CodeMismatchException
  = CodeMismatchException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if the provided code does not match what the server was expecting.</p>

#### `CompletionMessageType`

``` purescript
newtype CompletionMessageType
  = CompletionMessageType String
```

#### `CompromisedCredentialsActionsType`

``` purescript
newtype CompromisedCredentialsActionsType
  = CompromisedCredentialsActionsType { "EventAction" :: CompromisedCredentialsEventActionType }
```

<p>The compromised credentials actions type</p>

#### `CompromisedCredentialsEventActionType`

``` purescript
newtype CompromisedCredentialsEventActionType
  = CompromisedCredentialsEventActionType String
```

#### `CompromisedCredentialsRiskConfigurationType`

``` purescript
newtype CompromisedCredentialsRiskConfigurationType
  = CompromisedCredentialsRiskConfigurationType { "EventFilter" :: NullOrUndefined (EventFiltersType), "Actions" :: CompromisedCredentialsActionsType }
```

<p>The compromised credentials risk configuration type.</p>

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if two or more modifications are happening concurrently.</p>

#### `ConfirmDeviceRequest`

``` purescript
newtype ConfirmDeviceRequest
  = ConfirmDeviceRequest { "AccessToken" :: TokenModelType, "DeviceKey" :: DeviceKeyType, "DeviceSecretVerifierConfig" :: NullOrUndefined (DeviceSecretVerifierConfigType), "DeviceName" :: NullOrUndefined (DeviceNameType) }
```

<p>Confirms the device request.</p>

#### `ConfirmDeviceResponse`

``` purescript
newtype ConfirmDeviceResponse
  = ConfirmDeviceResponse { "UserConfirmationNecessary" :: NullOrUndefined (BooleanType) }
```

<p>Confirms the device response.</p>

#### `ConfirmForgotPasswordRequest`

``` purescript
newtype ConfirmForgotPasswordRequest
  = ConfirmForgotPasswordRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "ConfirmationCode" :: ConfirmationCodeType, "Password" :: PasswordType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>The request representing the confirmation for a password reset.</p>

#### `ConfirmForgotPasswordResponse`

``` purescript
newtype ConfirmForgotPasswordResponse
  = ConfirmForgotPasswordResponse {  }
```

<p>The response from the server that results from a user's request to retrieve a forgotten password.</p>

#### `ConfirmSignUpRequest`

``` purescript
newtype ConfirmSignUpRequest
  = ConfirmSignUpRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "ConfirmationCode" :: ConfirmationCodeType, "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Represents the request to confirm registration of a user.</p>

#### `ConfirmSignUpResponse`

``` purescript
newtype ConfirmSignUpResponse
  = ConfirmSignUpResponse {  }
```

<p>Represents the response from the server for the registration confirmation.</p>

#### `ConfirmationCodeType`

``` purescript
newtype ConfirmationCodeType
  = ConfirmationCodeType String
```

#### `ContextDataType`

``` purescript
newtype ContextDataType
  = ContextDataType { "IpAddress" :: StringType, "ServerName" :: StringType, "ServerPath" :: StringType, "HttpHeaders" :: HttpHeaderList, "EncodedData" :: NullOrUndefined (StringType) }
```

<p>Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>

#### `CreateGroupRequest`

``` purescript
newtype CreateGroupRequest
  = CreateGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType, "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType) }
```

#### `CreateGroupResponse`

``` purescript
newtype CreateGroupResponse
  = CreateGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

#### `CreateIdentityProviderRequest`

``` purescript
newtype CreateIdentityProviderRequest
  = CreateIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameTypeV1, "ProviderType" :: IdentityProviderTypeType, "ProviderDetails" :: ProviderDetailsType, "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType) }
```

#### `CreateIdentityProviderResponse`

``` purescript
newtype CreateIdentityProviderResponse
  = CreateIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

#### `CreateResourceServerRequest`

``` purescript
newtype CreateResourceServerRequest
  = CreateResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType, "Name" :: ResourceServerNameType, "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

#### `CreateResourceServerResponse`

``` purescript
newtype CreateResourceServerResponse
  = CreateResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

#### `CreateUserImportJobRequest`

``` purescript
newtype CreateUserImportJobRequest
  = CreateUserImportJobRequest { "JobName" :: UserImportJobNameType, "UserPoolId" :: UserPoolIdType, "CloudWatchLogsRoleArn" :: ArnType }
```

<p>Represents the request to create the user import job.</p>

#### `CreateUserImportJobResponse`

``` purescript
newtype CreateUserImportJobResponse
  = CreateUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to create the user import job.</p>

#### `CreateUserPoolClientRequest`

``` purescript
newtype CreateUserPoolClientRequest
  = CreateUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientName" :: ClientNameType, "GenerateSecret" :: NullOrUndefined (GenerateSecret), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Represents the request to create a user pool client.</p>

#### `CreateUserPoolClientResponse`

``` purescript
newtype CreateUserPoolClientResponse
  = CreateUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server to create a user pool client.</p>

#### `CreateUserPoolDomainRequest`

``` purescript
newtype CreateUserPoolDomainRequest
  = CreateUserPoolDomainRequest { "Domain" :: DomainType, "UserPoolId" :: UserPoolIdType }
```

#### `CreateUserPoolDomainResponse`

``` purescript
newtype CreateUserPoolDomainResponse
  = CreateUserPoolDomainResponse {  }
```

#### `CreateUserPoolRequest`

``` purescript
newtype CreateUserPoolRequest
  = CreateUserPoolRequest { "PoolName" :: UserPoolNameType, "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "AliasAttributes" :: NullOrUndefined (AliasAttributesListType), "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "Schema" :: NullOrUndefined (SchemaAttributesListType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>Represents the request to create a user pool.</p>

#### `CreateUserPoolResponse`

``` purescript
newtype CreateUserPoolResponse
  = CreateUserPoolResponse { "UserPool" :: NullOrUndefined (UserPoolType) }
```

<p>Represents the response from the server for the request to create a user pool.</p>

#### `CustomAttributeNameType`

``` purescript
newtype CustomAttributeNameType
  = CustomAttributeNameType String
```

#### `CustomAttributesListType`

``` purescript
newtype CustomAttributesListType
  = CustomAttributesListType (Array SchemaAttributeType)
```

#### `DateType`

``` purescript
newtype DateType
  = DateType Number
```

#### `DefaultEmailOptionType`

``` purescript
newtype DefaultEmailOptionType
  = DefaultEmailOptionType String
```

#### `DeleteGroupRequest`

``` purescript
newtype DeleteGroupRequest
  = DeleteGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType }
```

#### `DeleteIdentityProviderRequest`

``` purescript
newtype DeleteIdentityProviderRequest
  = DeleteIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType }
```

#### `DeleteResourceServerRequest`

``` purescript
newtype DeleteResourceServerRequest
  = DeleteResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType }
```

#### `DeleteUserAttributesRequest`

``` purescript
newtype DeleteUserAttributesRequest
  = DeleteUserAttributesRequest { "UserAttributeNames" :: AttributeNameListType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to delete user attributes.</p>

#### `DeleteUserAttributesResponse`

``` purescript
newtype DeleteUserAttributesResponse
  = DeleteUserAttributesResponse {  }
```

<p>Represents the response from the server to delete user attributes.</p>

#### `DeleteUserPoolClientRequest`

``` purescript
newtype DeleteUserPoolClientRequest
  = DeleteUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType }
```

<p>Represents the request to delete a user pool client.</p>

#### `DeleteUserPoolDomainRequest`

``` purescript
newtype DeleteUserPoolDomainRequest
  = DeleteUserPoolDomainRequest { "Domain" :: DomainType, "UserPoolId" :: UserPoolIdType }
```

#### `DeleteUserPoolDomainResponse`

``` purescript
newtype DeleteUserPoolDomainResponse
  = DeleteUserPoolDomainResponse {  }
```

#### `DeleteUserPoolRequest`

``` purescript
newtype DeleteUserPoolRequest
  = DeleteUserPoolRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to delete a user pool.</p>

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to delete a user.</p>

#### `DeliveryMediumListType`

``` purescript
newtype DeliveryMediumListType
  = DeliveryMediumListType (Array DeliveryMediumType)
```

#### `DeliveryMediumType`

``` purescript
newtype DeliveryMediumType
  = DeliveryMediumType String
```

#### `DescribeIdentityProviderRequest`

``` purescript
newtype DescribeIdentityProviderRequest
  = DescribeIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType }
```

#### `DescribeIdentityProviderResponse`

``` purescript
newtype DescribeIdentityProviderResponse
  = DescribeIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

#### `DescribeResourceServerRequest`

``` purescript
newtype DescribeResourceServerRequest
  = DescribeResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType }
```

#### `DescribeResourceServerResponse`

``` purescript
newtype DescribeResourceServerResponse
  = DescribeResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

#### `DescribeRiskConfigurationRequest`

``` purescript
newtype DescribeRiskConfigurationRequest
  = DescribeRiskConfigurationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType) }
```

#### `DescribeRiskConfigurationResponse`

``` purescript
newtype DescribeRiskConfigurationResponse
  = DescribeRiskConfigurationResponse { "RiskConfiguration" :: RiskConfigurationType }
```

#### `DescribeUserImportJobRequest`

``` purescript
newtype DescribeUserImportJobRequest
  = DescribeUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to describe the user import job.</p>

#### `DescribeUserImportJobResponse`

``` purescript
newtype DescribeUserImportJobResponse
  = DescribeUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to describe the user import job.</p>

#### `DescribeUserPoolClientRequest`

``` purescript
newtype DescribeUserPoolClientRequest
  = DescribeUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType }
```

<p>Represents the request to describe a user pool client.</p>

#### `DescribeUserPoolClientResponse`

``` purescript
newtype DescribeUserPoolClientResponse
  = DescribeUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server from a request to describe the user pool client.</p>

#### `DescribeUserPoolDomainRequest`

``` purescript
newtype DescribeUserPoolDomainRequest
  = DescribeUserPoolDomainRequest { "Domain" :: DomainType }
```

#### `DescribeUserPoolDomainResponse`

``` purescript
newtype DescribeUserPoolDomainResponse
  = DescribeUserPoolDomainResponse { "DomainDescription" :: NullOrUndefined (DomainDescriptionType) }
```

#### `DescribeUserPoolRequest`

``` purescript
newtype DescribeUserPoolRequest
  = DescribeUserPoolRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to describe the user pool.</p>

#### `DescribeUserPoolResponse`

``` purescript
newtype DescribeUserPoolResponse
  = DescribeUserPoolResponse { "UserPool" :: NullOrUndefined (UserPoolType) }
```

<p>Represents the response to describe the user pool.</p>

#### `DescriptionType`

``` purescript
newtype DescriptionType
  = DescriptionType String
```

#### `DeviceConfigurationType`

``` purescript
newtype DeviceConfigurationType
  = DeviceConfigurationType { "ChallengeRequiredOnNewDevice" :: NullOrUndefined (BooleanType), "DeviceOnlyRememberedOnUserPrompt" :: NullOrUndefined (BooleanType) }
```

<p>The configuration for the user pool's device tracking.</p>

#### `DeviceKeyType`

``` purescript
newtype DeviceKeyType
  = DeviceKeyType String
```

#### `DeviceListType`

``` purescript
newtype DeviceListType
  = DeviceListType (Array DeviceType)
```

#### `DeviceNameType`

``` purescript
newtype DeviceNameType
  = DeviceNameType String
```

#### `DeviceRememberedStatusType`

``` purescript
newtype DeviceRememberedStatusType
  = DeviceRememberedStatusType String
```

#### `DeviceSecretVerifierConfigType`

``` purescript
newtype DeviceSecretVerifierConfigType
  = DeviceSecretVerifierConfigType { "PasswordVerifier" :: NullOrUndefined (StringType), "Salt" :: NullOrUndefined (StringType) }
```

<p>The device verifier against which it will be authenticated.</p>

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType { "DeviceKey" :: NullOrUndefined (DeviceKeyType), "DeviceAttributes" :: NullOrUndefined (AttributeListType), "DeviceCreateDate" :: NullOrUndefined (DateType), "DeviceLastModifiedDate" :: NullOrUndefined (DateType), "DeviceLastAuthenticatedDate" :: NullOrUndefined (DateType) }
```

<p>The device type.</p>

#### `DomainDescriptionType`

``` purescript
newtype DomainDescriptionType
  = DomainDescriptionType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "AWSAccountId" :: NullOrUndefined (AWSAccountIdType), "Domain" :: NullOrUndefined (DomainType), "S3Bucket" :: NullOrUndefined (S3BucketType), "CloudFrontDistribution" :: NullOrUndefined (ArnType), "Version" :: NullOrUndefined (DomainVersionType), "Status" :: NullOrUndefined (DomainStatusType) }
```

<p>A container for information about a domain.</p>

#### `DomainStatusType`

``` purescript
newtype DomainStatusType
  = DomainStatusType String
```

#### `DomainType`

``` purescript
newtype DomainType
  = DomainType String
```

#### `DomainVersionType`

``` purescript
newtype DomainVersionType
  = DomainVersionType String
```

#### `DuplicateProviderException`

``` purescript
newtype DuplicateProviderException
  = DuplicateProviderException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the provider is already supported by the user pool.</p>

#### `EmailAddressType`

``` purescript
newtype EmailAddressType
  = EmailAddressType String
```

#### `EmailConfigurationType`

``` purescript
newtype EmailConfigurationType
  = EmailConfigurationType { "SourceArn" :: NullOrUndefined (ArnType), "ReplyToEmailAddress" :: NullOrUndefined (EmailAddressType) }
```

<p>The email configuration type.</p>

#### `EmailNotificationBodyType`

``` purescript
newtype EmailNotificationBodyType
  = EmailNotificationBodyType String
```

#### `EmailNotificationSubjectType`

``` purescript
newtype EmailNotificationSubjectType
  = EmailNotificationSubjectType String
```

#### `EmailVerificationMessageByLinkType`

``` purescript
newtype EmailVerificationMessageByLinkType
  = EmailVerificationMessageByLinkType String
```

#### `EmailVerificationMessageType`

``` purescript
newtype EmailVerificationMessageType
  = EmailVerificationMessageType String
```

#### `EmailVerificationSubjectByLinkType`

``` purescript
newtype EmailVerificationSubjectByLinkType
  = EmailVerificationSubjectByLinkType String
```

#### `EmailVerificationSubjectType`

``` purescript
newtype EmailVerificationSubjectType
  = EmailVerificationSubjectType String
```

#### `EnableSoftwareTokenMFAException`

``` purescript
newtype EnableSoftwareTokenMFAException
  = EnableSoftwareTokenMFAException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).</p>

#### `EventContextDataType`

``` purescript
newtype EventContextDataType
  = EventContextDataType { "IpAddress" :: NullOrUndefined (StringType), "DeviceName" :: NullOrUndefined (StringType), "Timezone" :: NullOrUndefined (StringType), "City" :: NullOrUndefined (StringType), "Country" :: NullOrUndefined (StringType) }
```

<p>Specifies the user context data captured at the time of an event request.</p>

#### `EventFeedbackType`

``` purescript
newtype EventFeedbackType
  = EventFeedbackType { "FeedbackValue" :: FeedbackValueType, "Provider" :: StringType, "FeedbackDate" :: NullOrUndefined (DateType) }
```

<p>Specifies the event feedback type.</p>

#### `EventFilterType`

``` purescript
newtype EventFilterType
  = EventFilterType String
```

#### `EventFiltersType`

``` purescript
newtype EventFiltersType
  = EventFiltersType (Array EventFilterType)
```

#### `EventIdType`

``` purescript
newtype EventIdType
  = EventIdType String
```

#### `EventResponseType`

``` purescript
newtype EventResponseType
  = EventResponseType String
```

#### `EventRiskType`

``` purescript
newtype EventRiskType
  = EventRiskType { "RiskDecision" :: NullOrUndefined (RiskDecisionType), "RiskLevel" :: NullOrUndefined (RiskLevelType) }
```

<p>The event risk type.</p>

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

#### `ExpiredCodeException`

``` purescript
newtype ExpiredCodeException
  = ExpiredCodeException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown if a code has expired.</p>

#### `ExplicitAuthFlowsListType`

``` purescript
newtype ExplicitAuthFlowsListType
  = ExplicitAuthFlowsListType (Array ExplicitAuthFlowsType)
```

#### `ExplicitAuthFlowsType`

``` purescript
newtype ExplicitAuthFlowsType
  = ExplicitAuthFlowsType String
```

#### `FeedbackValueType`

``` purescript
newtype FeedbackValueType
  = FeedbackValueType String
```

#### `ForceAliasCreation`

``` purescript
newtype ForceAliasCreation
  = ForceAliasCreation Boolean
```

#### `ForgetDeviceRequest`

``` purescript
newtype ForgetDeviceRequest
  = ForgetDeviceRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "DeviceKey" :: DeviceKeyType }
```

<p>Represents the request to forget the device.</p>

#### `ForgotPasswordRequest`

``` purescript
newtype ForgotPasswordRequest
  = ForgotPasswordRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "UserContextData" :: NullOrUndefined (UserContextDataType), "Username" :: UsernameType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType) }
```

<p>Represents the request to reset a user's password.</p>

#### `ForgotPasswordResponse`

``` purescript
newtype ForgotPasswordResponse
  = ForgotPasswordResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>Respresents the response from the server regarding the request to reset a password.</p>

#### `GenerateSecret`

``` purescript
newtype GenerateSecret
  = GenerateSecret Boolean
```

#### `GetCSVHeaderRequest`

``` purescript
newtype GetCSVHeaderRequest
  = GetCSVHeaderRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Represents the request to get the header information for the .csv file for the user import job.</p>

#### `GetCSVHeaderResponse`

``` purescript
newtype GetCSVHeaderResponse
  = GetCSVHeaderResponse { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "CSVHeader" :: NullOrUndefined (ListOfStringTypes) }
```

<p>Represents the response from the server to the request to get the header information for the .csv file for the user import job.</p>

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "DeviceKey" :: DeviceKeyType, "AccessToken" :: NullOrUndefined (TokenModelType) }
```

<p>Represents the request to get the device.</p>

#### `GetDeviceResponse`

``` purescript
newtype GetDeviceResponse
  = GetDeviceResponse { "Device" :: DeviceType }
```

<p>Gets the device response.</p>

#### `GetGroupRequest`

``` purescript
newtype GetGroupRequest
  = GetGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType }
```

#### `GetGroupResponse`

``` purescript
newtype GetGroupResponse
  = GetGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

#### `GetIdentityProviderByIdentifierRequest`

``` purescript
newtype GetIdentityProviderByIdentifierRequest
  = GetIdentityProviderByIdentifierRequest { "UserPoolId" :: UserPoolIdType, "IdpIdentifier" :: IdpIdentifierType }
```

#### `GetIdentityProviderByIdentifierResponse`

``` purescript
newtype GetIdentityProviderByIdentifierResponse
  = GetIdentityProviderByIdentifierResponse { "IdentityProvider" :: IdentityProviderType }
```

#### `GetSigningCertificateRequest`

``` purescript
newtype GetSigningCertificateRequest
  = GetSigningCertificateRequest { "UserPoolId" :: UserPoolIdType }
```

<p>Request to get a signing certificate from Cognito.</p>

#### `GetSigningCertificateResponse`

``` purescript
newtype GetSigningCertificateResponse
  = GetSigningCertificateResponse { "Certificate" :: NullOrUndefined (StringType) }
```

<p>Response from Cognito for a signing certificate request.</p>

#### `GetUICustomizationRequest`

``` purescript
newtype GetUICustomizationRequest
  = GetUICustomizationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType) }
```

#### `GetUICustomizationResponse`

``` purescript
newtype GetUICustomizationResponse
  = GetUICustomizationResponse { "UICustomization" :: UICustomizationType }
```

#### `GetUserAttributeVerificationCodeRequest`

``` purescript
newtype GetUserAttributeVerificationCodeRequest
  = GetUserAttributeVerificationCodeRequest { "AccessToken" :: TokenModelType, "AttributeName" :: AttributeNameType }
```

<p>Represents the request to get user attribute verification.</p>

#### `GetUserAttributeVerificationCodeResponse`

``` purescript
newtype GetUserAttributeVerificationCodeResponse
  = GetUserAttributeVerificationCodeResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>The verification code response returned by the server response to get the user attribute verification code.</p>

#### `GetUserPoolMfaConfigRequest`

``` purescript
newtype GetUserPoolMfaConfigRequest
  = GetUserPoolMfaConfigRequest { "UserPoolId" :: UserPoolIdType }
```

#### `GetUserPoolMfaConfigResponse`

``` purescript
newtype GetUserPoolMfaConfigResponse
  = GetUserPoolMfaConfigResponse { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

#### `GetUserRequest`

``` purescript
newtype GetUserRequest
  = GetUserRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to get information about the user.</p>

#### `GetUserResponse`

``` purescript
newtype GetUserResponse
  = GetUserResponse { "Username" :: UsernameType, "UserAttributes" :: AttributeListType, "MFAOptions" :: NullOrUndefined (MFAOptionListType), "PreferredMfaSetting" :: NullOrUndefined (StringType), "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType) }
```

<p>Represents the response from the server from the request to get information about the user.</p>

#### `GlobalSignOutRequest`

``` purescript
newtype GlobalSignOutRequest
  = GlobalSignOutRequest { "AccessToken" :: TokenModelType }
```

<p>Represents the request to sign out all devices.</p>

#### `GlobalSignOutResponse`

``` purescript
newtype GlobalSignOutResponse
  = GlobalSignOutResponse {  }
```

<p>The response to the request to sign out all devices.</p>

#### `GroupExistsException`

``` purescript
newtype GroupExistsException
  = GroupExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.</p>

#### `GroupListType`

``` purescript
newtype GroupListType
  = GroupListType (Array GroupType)
```

#### `GroupNameType`

``` purescript
newtype GroupNameType
  = GroupNameType String
```

#### `GroupType`

``` purescript
newtype GroupType
  = GroupType { "GroupName" :: NullOrUndefined (GroupNameType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>The group type.</p>

#### `HexStringType`

``` purescript
newtype HexStringType
  = HexStringType String
```

#### `HttpHeader`

``` purescript
newtype HttpHeader
  = HttpHeader { "HeaderName'" :: NullOrUndefined (StringType), "HeaderValue'" :: NullOrUndefined (StringType) }
```

<p>The HTTP header.</p>

#### `HttpHeaderList`

``` purescript
newtype HttpHeaderList
  = HttpHeaderList (Array HttpHeader)
```

#### `IdentityProviderType`

``` purescript
newtype IdentityProviderType
  = IdentityProviderType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderType" :: NullOrUndefined (IdentityProviderTypeType), "ProviderDetails" :: NullOrUndefined (ProviderDetailsType), "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for information about an identity provider.</p>

#### `IdentityProviderTypeType`

``` purescript
newtype IdentityProviderTypeType
  = IdentityProviderTypeType String
```

#### `IdpIdentifierType`

``` purescript
newtype IdpIdentifierType
  = IdpIdentifierType String
```

#### `IdpIdentifiersListType`

``` purescript
newtype IdpIdentifiersListType
  = IdpIdentifiersListType (Array IdpIdentifierType)
```

#### `ImageFileType`

``` purescript
newtype ImageFileType
  = ImageFileType String
```

#### `ImageUrlType`

``` purescript
newtype ImageUrlType
  = ImageUrlType String
```

#### `InitiateAuthRequest`

``` purescript
newtype InitiateAuthRequest
  = InitiateAuthRequest { "AuthFlow" :: AuthFlowType, "AuthParameters" :: NullOrUndefined (AuthParametersType), "ClientMetadata" :: NullOrUndefined (ClientMetadataType), "ClientId" :: ClientIdType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Initiates the authentication request.</p>

#### `InitiateAuthResponse`

``` purescript
newtype InitiateAuthResponse
  = InitiateAuthResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>Initiates the authentication response.</p>

#### `IntegerType`

``` purescript
newtype IntegerType
  = IntegerType Int
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters an internal error.</p>

#### `InvalidEmailRoleAccessPolicyException`

``` purescript
newtype InvalidEmailRoleAccessPolicyException
  = InvalidEmailRoleAccessPolicyException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.</p>

#### `InvalidLambdaResponseException`

``` purescript
newtype InvalidLambdaResponseException
  = InvalidLambdaResponseException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.</p>

#### `InvalidOAuthFlowException`

``` purescript
newtype InvalidOAuthFlowException
  = InvalidOAuthFlowException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified OAuth flow is invalid.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid parameter.</p>

#### `InvalidPasswordException`

``` purescript
newtype InvalidPasswordException
  = InvalidPasswordException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an invalid password.</p>

#### `InvalidSmsRoleAccessPolicyException`

``` purescript
newtype InvalidSmsRoleAccessPolicyException
  = InvalidSmsRoleAccessPolicyException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.</p>

#### `InvalidSmsRoleTrustRelationshipException`

``` purescript
newtype InvalidSmsRoleTrustRelationshipException
  = InvalidSmsRoleTrustRelationshipException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust <b>cognito-idp.amazonaws.com</b> or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.</p>

#### `InvalidUserPoolConfigurationException`

``` purescript
newtype InvalidUserPoolConfigurationException
  = InvalidUserPoolConfigurationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user pool configuration is invalid.</p>

#### `LambdaConfigType`

``` purescript
newtype LambdaConfigType
  = LambdaConfigType { "PreSignUp" :: NullOrUndefined (ArnType), "CustomMessage" :: NullOrUndefined (ArnType), "PostConfirmation" :: NullOrUndefined (ArnType), "PreAuthentication" :: NullOrUndefined (ArnType), "PostAuthentication" :: NullOrUndefined (ArnType), "DefineAuthChallenge" :: NullOrUndefined (ArnType), "CreateAuthChallenge" :: NullOrUndefined (ArnType), "VerifyAuthChallengeResponse" :: NullOrUndefined (ArnType), "PreTokenGeneration" :: NullOrUndefined (ArnType), "UserMigration" :: NullOrUndefined (ArnType) }
```

<p>Specifies the configuration for AWS Lambda triggers.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user exceeds the limit for a requested AWS resource.</p>

#### `ListDevicesRequest`

``` purescript
newtype ListDevicesRequest
  = ListDevicesRequest { "AccessToken" :: TokenModelType, "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the request to list the devices.</p>

#### `ListDevicesResponse`

``` purescript
newtype ListDevicesResponse
  = ListDevicesResponse { "Devices" :: NullOrUndefined (DeviceListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>Represents the response to list devices.</p>

#### `ListGroupsRequest`

``` purescript
newtype ListGroupsRequest
  = ListGroupsRequest { "UserPoolId" :: UserPoolIdType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `ListGroupsResponse`

``` purescript
newtype ListGroupsResponse
  = ListGroupsResponse { "Groups" :: NullOrUndefined (GroupListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `ListIdentityProvidersRequest`

``` purescript
newtype ListIdentityProvidersRequest
  = ListIdentityProvidersRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (ListProvidersLimitType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

#### `ListIdentityProvidersResponse`

``` purescript
newtype ListIdentityProvidersResponse
  = ListIdentityProvidersResponse { "Providers" :: ProvidersListType, "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

#### `ListOfStringTypes`

``` purescript
newtype ListOfStringTypes
  = ListOfStringTypes (Array StringType)
```

#### `ListProvidersLimitType`

``` purescript
newtype ListProvidersLimitType
  = ListProvidersLimitType Int
```

#### `ListResourceServersLimitType`

``` purescript
newtype ListResourceServersLimitType
  = ListResourceServersLimitType Int
```

#### `ListResourceServersRequest`

``` purescript
newtype ListResourceServersRequest
  = ListResourceServersRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (ListResourceServersLimitType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

#### `ListResourceServersResponse`

``` purescript
newtype ListResourceServersResponse
  = ListResourceServersResponse { "ResourceServers" :: ResourceServersListType, "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

#### `ListUserImportJobsRequest`

``` purescript
newtype ListUserImportJobsRequest
  = ListUserImportJobsRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: PoolQueryLimitType, "PaginationToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the request to list the user import jobs.</p>

#### `ListUserImportJobsResponse`

``` purescript
newtype ListUserImportJobsResponse
  = ListUserImportJobsResponse { "UserImportJobs" :: NullOrUndefined (UserImportJobsListType), "PaginationToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the response from the server to the request to list the user import jobs.</p>

#### `ListUserPoolClientsRequest`

``` purescript
newtype ListUserPoolClientsRequest
  = ListUserPoolClientsRequest { "UserPoolId" :: UserPoolIdType, "MaxResults" :: NullOrUndefined (QueryLimit), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Represents the request to list the user pool clients.</p>

#### `ListUserPoolClientsResponse`

``` purescript
newtype ListUserPoolClientsResponse
  = ListUserPoolClientsResponse { "UserPoolClients" :: NullOrUndefined (UserPoolClientListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Represents the response from the server that lists user pool clients.</p>

#### `ListUserPoolsRequest`

``` purescript
newtype ListUserPoolsRequest
  = ListUserPoolsRequest { "NextToken" :: NullOrUndefined (PaginationKeyType), "MaxResults" :: PoolQueryLimitType }
```

<p>Represents the request to list user pools.</p>

#### `ListUserPoolsResponse`

``` purescript
newtype ListUserPoolsResponse
  = ListUserPoolsResponse { "UserPools" :: NullOrUndefined (UserPoolListType), "NextToken" :: NullOrUndefined (PaginationKeyType) }
```

<p>Represents the response to list user pools.</p>

#### `ListUsersInGroupRequest`

``` purescript
newtype ListUsersInGroupRequest
  = ListUsersInGroupRequest { "UserPoolId" :: UserPoolIdType, "GroupName" :: GroupNameType, "Limit" :: NullOrUndefined (QueryLimitType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `ListUsersInGroupResponse`

``` purescript
newtype ListUsersInGroupResponse
  = ListUsersInGroupResponse { "Users" :: NullOrUndefined (UsersListType), "NextToken" :: NullOrUndefined (PaginationKey) }
```

#### `ListUsersRequest`

``` purescript
newtype ListUsersRequest
  = ListUsersRequest { "UserPoolId" :: UserPoolIdType, "AttributesToGet" :: NullOrUndefined (SearchedAttributeNamesListType), "Limit" :: NullOrUndefined (QueryLimitType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType), "Filter" :: NullOrUndefined (UserFilterType) }
```

<p>Represents the request to list users.</p>

#### `ListUsersResponse`

``` purescript
newtype ListUsersResponse
  = ListUsersResponse { "Users" :: NullOrUndefined (UsersListType), "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType) }
```

<p>The response from the request to list users.</p>

#### `LogoutURLsListType`

``` purescript
newtype LogoutURLsListType
  = LogoutURLsListType (Array RedirectUrlType)
```

#### `LongType`

``` purescript
newtype LongType
  = LongType Number
```

#### `MFAMethodNotFoundException`

``` purescript
newtype MFAMethodNotFoundException
  = MFAMethodNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.</p>

#### `MFAOptionListType`

``` purescript
newtype MFAOptionListType
  = MFAOptionListType (Array MFAOptionType)
```

#### `MFAOptionType`

``` purescript
newtype MFAOptionType
  = MFAOptionType { "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType), "AttributeName" :: NullOrUndefined (AttributeNameType) }
```

<p>Specifies the different settings for multi-factor authentication (MFA).</p>

#### `MessageActionType`

``` purescript
newtype MessageActionType
  = MessageActionType String
```

#### `MessageTemplateType`

``` purescript
newtype MessageTemplateType
  = MessageTemplateType { "SMSMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType) }
```

<p>The message template structure.</p>

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

#### `NewDeviceMetadataType`

``` purescript
newtype NewDeviceMetadataType
  = NewDeviceMetadataType { "DeviceKey" :: NullOrUndefined (DeviceKeyType), "DeviceGroupKey" :: NullOrUndefined (StringType) }
```

<p>The new device metadata type.</p>

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not authorized.</p>

#### `NotifyConfigurationType`

``` purescript
newtype NotifyConfigurationType
  = NotifyConfigurationType { "From" :: NullOrUndefined (StringType), "ReplyTo" :: NullOrUndefined (StringType), "SourceArn" :: ArnType, "BlockEmail" :: NullOrUndefined (NotifyEmailType), "NoActionEmail" :: NullOrUndefined (NotifyEmailType), "MfaEmail" :: NullOrUndefined (NotifyEmailType) }
```

<p>The notify configuration type.</p>

#### `NotifyEmailType`

``` purescript
newtype NotifyEmailType
  = NotifyEmailType { "Subject" :: EmailNotificationSubjectType, "HtmlBody" :: NullOrUndefined (EmailNotificationBodyType), "TextBody" :: NullOrUndefined (EmailNotificationBodyType) }
```

<p>The notify email type.</p>

#### `NumberAttributeConstraintsType`

``` purescript
newtype NumberAttributeConstraintsType
  = NumberAttributeConstraintsType { "MinValue" :: NullOrUndefined (StringType), "MaxValue" :: NullOrUndefined (StringType) }
```

<p>The minimum and maximum value of an attribute that is of the number data type.</p>

#### `OAuthFlowType`

``` purescript
newtype OAuthFlowType
  = OAuthFlowType String
```

#### `OAuthFlowsType`

``` purescript
newtype OAuthFlowsType
  = OAuthFlowsType (Array OAuthFlowType)
```

#### `PaginationKey`

``` purescript
newtype PaginationKey
  = PaginationKey String
```

#### `PaginationKeyType`

``` purescript
newtype PaginationKeyType
  = PaginationKeyType String
```

#### `PasswordPolicyMinLengthType`

``` purescript
newtype PasswordPolicyMinLengthType
  = PasswordPolicyMinLengthType Int
```

#### `PasswordPolicyType`

``` purescript
newtype PasswordPolicyType
  = PasswordPolicyType { "MinimumLength" :: NullOrUndefined (PasswordPolicyMinLengthType), "RequireUppercase" :: NullOrUndefined (BooleanType), "RequireLowercase" :: NullOrUndefined (BooleanType), "RequireNumbers" :: NullOrUndefined (BooleanType), "RequireSymbols" :: NullOrUndefined (BooleanType) }
```

<p>The password policy type.</p>

#### `PasswordResetRequiredException`

``` purescript
newtype PasswordResetRequiredException
  = PasswordResetRequiredException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a password reset is required.</p>

#### `PasswordType`

``` purescript
newtype PasswordType
  = PasswordType String
```

#### `PoolQueryLimitType`

``` purescript
newtype PoolQueryLimitType
  = PoolQueryLimitType Int
```

#### `PreSignedUrlType`

``` purescript
newtype PreSignedUrlType
  = PreSignedUrlType String
```

#### `PrecedenceType`

``` purescript
newtype PrecedenceType
  = PrecedenceType Int
```

#### `PreconditionNotMetException`

``` purescript
newtype PreconditionNotMetException
  = PreconditionNotMetException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a precondition is not met.</p>

#### `ProviderDescription`

``` purescript
newtype ProviderDescription
  = ProviderDescription { "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderType" :: NullOrUndefined (IdentityProviderTypeType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for identity provider details.</p>

#### `ProviderDetailsType`

``` purescript
newtype ProviderDetailsType
  = ProviderDetailsType (Map StringType StringType)
```

#### `ProviderNameType`

``` purescript
newtype ProviderNameType
  = ProviderNameType String
```

#### `ProviderNameTypeV1`

``` purescript
newtype ProviderNameTypeV1
  = ProviderNameTypeV1 String
```

#### `ProviderUserIdentifierType`

``` purescript
newtype ProviderUserIdentifierType
  = ProviderUserIdentifierType { "ProviderName" :: NullOrUndefined (ProviderNameType), "ProviderAttributeName" :: NullOrUndefined (StringType), "ProviderAttributeValue" :: NullOrUndefined (StringType) }
```

<p>A container for information about an identity provider for a user pool.</p>

#### `ProvidersListType`

``` purescript
newtype ProvidersListType
  = ProvidersListType (Array ProviderDescription)
```

#### `QueryLimit`

``` purescript
newtype QueryLimit
  = QueryLimit Int
```

#### `QueryLimitType`

``` purescript
newtype QueryLimitType
  = QueryLimitType Int
```

#### `RedirectUrlType`

``` purescript
newtype RedirectUrlType
  = RedirectUrlType String
```

#### `RefreshTokenValidityType`

``` purescript
newtype RefreshTokenValidityType
  = RefreshTokenValidityType Int
```

#### `ResendConfirmationCodeRequest`

``` purescript
newtype ResendConfirmationCodeRequest
  = ResendConfirmationCodeRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "UserContextData" :: NullOrUndefined (UserContextDataType), "Username" :: UsernameType, "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType) }
```

<p>Represents the request to resend the confirmation code.</p>

#### `ResendConfirmationCodeResponse`

``` purescript
newtype ResendConfirmationCodeResponse
  = ResendConfirmationCodeResponse { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType) }
```

<p>The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service cannot find the requested resource.</p>

#### `ResourceServerIdentifierType`

``` purescript
newtype ResourceServerIdentifierType
  = ResourceServerIdentifierType String
```

#### `ResourceServerNameType`

``` purescript
newtype ResourceServerNameType
  = ResourceServerNameType String
```

#### `ResourceServerScopeDescriptionType`

``` purescript
newtype ResourceServerScopeDescriptionType
  = ResourceServerScopeDescriptionType String
```

#### `ResourceServerScopeListType`

``` purescript
newtype ResourceServerScopeListType
  = ResourceServerScopeListType (Array ResourceServerScopeType)
```

#### `ResourceServerScopeNameType`

``` purescript
newtype ResourceServerScopeNameType
  = ResourceServerScopeNameType String
```

#### `ResourceServerScopeType`

``` purescript
newtype ResourceServerScopeType
  = ResourceServerScopeType { "ScopeName" :: ResourceServerScopeNameType, "ScopeDescription" :: ResourceServerScopeDescriptionType }
```

<p>A resource server scope.</p>

#### `ResourceServerType`

``` purescript
newtype ResourceServerType
  = ResourceServerType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "Identifier" :: NullOrUndefined (ResourceServerIdentifierType), "Name" :: NullOrUndefined (ResourceServerNameType), "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

<p>A container for information about a resource server for a user pool.</p>

#### `ResourceServersListType`

``` purescript
newtype ResourceServersListType
  = ResourceServersListType (Array ResourceServerType)
```

#### `RespondToAuthChallengeRequest`

``` purescript
newtype RespondToAuthChallengeRequest
  = RespondToAuthChallengeRequest { "ClientId" :: ClientIdType, "ChallengeName" :: ChallengeNameType, "Session" :: NullOrUndefined (SessionType), "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>The request to respond to an authentication challenge.</p>

#### `RespondToAuthChallengeResponse`

``` purescript
newtype RespondToAuthChallengeResponse
  = RespondToAuthChallengeResponse { "ChallengeName" :: NullOrUndefined (ChallengeNameType), "Session" :: NullOrUndefined (SessionType), "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType), "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType) }
```

<p>The response to respond to the authentication challenge.</p>

#### `RiskConfigurationType`

``` purescript
newtype RiskConfigurationType
  = RiskConfigurationType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientId" :: NullOrUndefined (ClientIdType), "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType), "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType), "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType), "LastModifiedDate" :: NullOrUndefined (DateType) }
```

<p>The risk configuration type.</p>

#### `RiskDecisionType`

``` purescript
newtype RiskDecisionType
  = RiskDecisionType String
```

#### `RiskExceptionConfigurationType`

``` purescript
newtype RiskExceptionConfigurationType
  = RiskExceptionConfigurationType { "BlockedIPRangeList" :: NullOrUndefined (BlockedIPRangeListType), "SkippedIPRangeList" :: NullOrUndefined (SkippedIPRangeListType) }
```

<p>The type of the configuration to override the risk decision.</p>

#### `RiskLevelType`

``` purescript
newtype RiskLevelType
  = RiskLevelType String
```

#### `S3BucketType`

``` purescript
newtype S3BucketType
  = S3BucketType String
```

#### `SMSMfaSettingsType`

``` purescript
newtype SMSMfaSettingsType
  = SMSMfaSettingsType { "Enabled" :: NullOrUndefined (BooleanType), "PreferredMfa" :: NullOrUndefined (BooleanType) }
```

<p>The SMS multi-factor authentication (MFA) settings type.</p>

#### `SchemaAttributeType`

``` purescript
newtype SchemaAttributeType
  = SchemaAttributeType { "Name" :: NullOrUndefined (CustomAttributeNameType), "AttributeDataType" :: NullOrUndefined (AttributeDataType), "DeveloperOnlyAttribute" :: NullOrUndefined (BooleanType), "Mutable" :: NullOrUndefined (BooleanType), "Required" :: NullOrUndefined (BooleanType), "NumberAttributeConstraints" :: NullOrUndefined (NumberAttributeConstraintsType), "StringAttributeConstraints" :: NullOrUndefined (StringAttributeConstraintsType) }
```

<p>Contains information about the schema attribute.</p>

#### `SchemaAttributesListType`

``` purescript
newtype SchemaAttributesListType
  = SchemaAttributesListType (Array SchemaAttributeType)
```

#### `ScopeDoesNotExistException`

``` purescript
newtype ScopeDoesNotExistException
  = ScopeDoesNotExistException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified scope does not exist.</p>

#### `ScopeListType`

``` purescript
newtype ScopeListType
  = ScopeListType (Array ScopeType)
```

#### `ScopeType`

``` purescript
newtype ScopeType
  = ScopeType String
```

#### `SearchPaginationTokenType`

``` purescript
newtype SearchPaginationTokenType
  = SearchPaginationTokenType String
```

#### `SearchedAttributeNamesListType`

``` purescript
newtype SearchedAttributeNamesListType
  = SearchedAttributeNamesListType (Array AttributeNameType)
```

#### `SecretCodeType`

``` purescript
newtype SecretCodeType
  = SecretCodeType String
```

#### `SecretHashType`

``` purescript
newtype SecretHashType
  = SecretHashType String
```

#### `SessionType`

``` purescript
newtype SessionType
  = SessionType String
```

#### `SetRiskConfigurationRequest`

``` purescript
newtype SetRiskConfigurationRequest
  = SetRiskConfigurationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType), "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType), "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType), "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType) }
```

#### `SetRiskConfigurationResponse`

``` purescript
newtype SetRiskConfigurationResponse
  = SetRiskConfigurationResponse { "RiskConfiguration" :: RiskConfigurationType }
```

#### `SetUICustomizationRequest`

``` purescript
newtype SetUICustomizationRequest
  = SetUICustomizationRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: NullOrUndefined (ClientIdType), "CSS" :: NullOrUndefined (CSSType), "ImageFile" :: NullOrUndefined (ImageFileType) }
```

#### `SetUICustomizationResponse`

``` purescript
newtype SetUICustomizationResponse
  = SetUICustomizationResponse { "UICustomization" :: UICustomizationType }
```

#### `SetUserMFAPreferenceRequest`

``` purescript
newtype SetUserMFAPreferenceRequest
  = SetUserMFAPreferenceRequest { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType), "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType), "AccessToken" :: TokenModelType }
```

#### `SetUserMFAPreferenceResponse`

``` purescript
newtype SetUserMFAPreferenceResponse
  = SetUserMFAPreferenceResponse {  }
```

#### `SetUserPoolMfaConfigRequest`

``` purescript
newtype SetUserPoolMfaConfigRequest
  = SetUserPoolMfaConfigRequest { "UserPoolId" :: UserPoolIdType, "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

#### `SetUserPoolMfaConfigResponse`

``` purescript
newtype SetUserPoolMfaConfigResponse
  = SetUserPoolMfaConfigResponse { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType), "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType) }
```

#### `SetUserSettingsRequest`

``` purescript
newtype SetUserSettingsRequest
  = SetUserSettingsRequest { "AccessToken" :: TokenModelType, "MFAOptions" :: MFAOptionListType }
```

<p>Represents the request to set user settings.</p>

#### `SetUserSettingsResponse`

``` purescript
newtype SetUserSettingsResponse
  = SetUserSettingsResponse {  }
```

<p>The response from the server for a set user settings request.</p>

#### `SignUpRequest`

``` purescript
newtype SignUpRequest
  = SignUpRequest { "ClientId" :: ClientIdType, "SecretHash" :: NullOrUndefined (SecretHashType), "Username" :: UsernameType, "Password" :: PasswordType, "UserAttributes" :: NullOrUndefined (AttributeListType), "ValidationData" :: NullOrUndefined (AttributeListType), "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType), "UserContextData" :: NullOrUndefined (UserContextDataType) }
```

<p>Represents the request to register a user.</p>

#### `SignUpResponse`

``` purescript
newtype SignUpResponse
  = SignUpResponse { "UserConfirmed" :: BooleanType, "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType), "UserSub" :: StringType }
```

<p>The response from the server for a registration request.</p>

#### `SkippedIPRangeListType`

``` purescript
newtype SkippedIPRangeListType
  = SkippedIPRangeListType (Array StringType)
```

#### `SmsConfigurationType`

``` purescript
newtype SmsConfigurationType
  = SmsConfigurationType { "SnsCallerArn" :: ArnType, "ExternalId" :: NullOrUndefined (StringType) }
```

<p>The SMS configuration type.</p>

#### `SmsMfaConfigType`

``` purescript
newtype SmsMfaConfigType
  = SmsMfaConfigType { "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType) }
```

<p>The SMS text message multi-factor authentication (MFA) configuration type.</p>

#### `SmsVerificationMessageType`

``` purescript
newtype SmsVerificationMessageType
  = SmsVerificationMessageType String
```

#### `SoftwareTokenMFANotFoundException`

``` purescript
newtype SoftwareTokenMFANotFoundException
  = SoftwareTokenMFANotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.</p>

#### `SoftwareTokenMFAUserCodeType`

``` purescript
newtype SoftwareTokenMFAUserCodeType
  = SoftwareTokenMFAUserCodeType String
```

#### `SoftwareTokenMfaConfigType`

``` purescript
newtype SoftwareTokenMfaConfigType
  = SoftwareTokenMfaConfigType { "Enabled" :: NullOrUndefined (BooleanType) }
```

<p>The type used for enabling software token MFA at the user pool level.</p>

#### `SoftwareTokenMfaSettingsType`

``` purescript
newtype SoftwareTokenMfaSettingsType
  = SoftwareTokenMfaSettingsType { "Enabled" :: NullOrUndefined (BooleanType), "PreferredMfa" :: NullOrUndefined (BooleanType) }
```

<p>The type used for enabling software token MFA at the user level.</p>

#### `StartUserImportJobRequest`

``` purescript
newtype StartUserImportJobRequest
  = StartUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to start the user import job.</p>

#### `StartUserImportJobResponse`

``` purescript
newtype StartUserImportJobResponse
  = StartUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to start the user import job.</p>

#### `StatusType`

``` purescript
newtype StatusType
  = StatusType String
```

#### `StopUserImportJobRequest`

``` purescript
newtype StopUserImportJobRequest
  = StopUserImportJobRequest { "UserPoolId" :: UserPoolIdType, "JobId" :: UserImportJobIdType }
```

<p>Represents the request to stop the user import job.</p>

#### `StopUserImportJobResponse`

``` purescript
newtype StopUserImportJobResponse
  = StopUserImportJobResponse { "UserImportJob" :: NullOrUndefined (UserImportJobType) }
```

<p>Represents the response from the server to the request to stop the user import job.</p>

#### `StringAttributeConstraintsType`

``` purescript
newtype StringAttributeConstraintsType
  = StringAttributeConstraintsType { "MinLength" :: NullOrUndefined (StringType), "MaxLength" :: NullOrUndefined (StringType) }
```

<p>The constraints associated with a string attribute.</p>

#### `StringType`

``` purescript
newtype StringType
  = StringType String
```

#### `SupportedIdentityProvidersListType`

``` purescript
newtype SupportedIdentityProvidersListType
  = SupportedIdentityProvidersListType (Array ProviderNameType)
```

#### `TokenModelType`

``` purescript
newtype TokenModelType
  = TokenModelType String
```

#### `TooManyFailedAttemptsException`

``` purescript
newtype TooManyFailedAttemptsException
  = TooManyFailedAttemptsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).</p>

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the user has made too many requests for a given operation.</p>

#### `UICustomizationType`

``` purescript
newtype UICustomizationType
  = UICustomizationType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientId" :: NullOrUndefined (ClientIdType), "ImageUrl" :: NullOrUndefined (ImageUrlType), "CSS" :: NullOrUndefined (CSSType), "CSSVersion" :: NullOrUndefined (CSSVersionType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A container for the UI customization information for a user pool's built-in app UI.</p>

#### `UnexpectedLambdaException`

``` purescript
newtype UnexpectedLambdaException
  = UnexpectedLambdaException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.</p>

#### `UnsupportedIdentityProviderException`

``` purescript
newtype UnsupportedIdentityProviderException
  = UnsupportedIdentityProviderException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the specified identifier is not supported.</p>

#### `UnsupportedUserStateException`

``` purescript
newtype UnsupportedUserStateException
  = UnsupportedUserStateException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>The request failed because the user is in an unsupported state.</p>

#### `UpdateAuthEventFeedbackRequest`

``` purescript
newtype UpdateAuthEventFeedbackRequest
  = UpdateAuthEventFeedbackRequest { "UserPoolId" :: UserPoolIdType, "Username" :: UsernameType, "EventId" :: EventIdType, "FeedbackToken" :: TokenModelType, "FeedbackValue" :: FeedbackValueType }
```

#### `UpdateAuthEventFeedbackResponse`

``` purescript
newtype UpdateAuthEventFeedbackResponse
  = UpdateAuthEventFeedbackResponse {  }
```

#### `UpdateDeviceStatusRequest`

``` purescript
newtype UpdateDeviceStatusRequest
  = UpdateDeviceStatusRequest { "AccessToken" :: TokenModelType, "DeviceKey" :: DeviceKeyType, "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType) }
```

<p>Represents the request to update the device status.</p>

#### `UpdateDeviceStatusResponse`

``` purescript
newtype UpdateDeviceStatusResponse
  = UpdateDeviceStatusResponse {  }
```

<p>The response to the request to update the device status.</p>

#### `UpdateGroupRequest`

``` purescript
newtype UpdateGroupRequest
  = UpdateGroupRequest { "GroupName" :: GroupNameType, "UserPoolId" :: UserPoolIdType, "Description" :: NullOrUndefined (DescriptionType), "RoleArn" :: NullOrUndefined (ArnType), "Precedence" :: NullOrUndefined (PrecedenceType) }
```

#### `UpdateGroupResponse`

``` purescript
newtype UpdateGroupResponse
  = UpdateGroupResponse { "Group" :: NullOrUndefined (GroupType) }
```

#### `UpdateIdentityProviderRequest`

``` purescript
newtype UpdateIdentityProviderRequest
  = UpdateIdentityProviderRequest { "UserPoolId" :: UserPoolIdType, "ProviderName" :: ProviderNameType, "ProviderDetails" :: NullOrUndefined (ProviderDetailsType), "AttributeMapping" :: NullOrUndefined (AttributeMappingType), "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType) }
```

#### `UpdateIdentityProviderResponse`

``` purescript
newtype UpdateIdentityProviderResponse
  = UpdateIdentityProviderResponse { "IdentityProvider" :: IdentityProviderType }
```

#### `UpdateResourceServerRequest`

``` purescript
newtype UpdateResourceServerRequest
  = UpdateResourceServerRequest { "UserPoolId" :: UserPoolIdType, "Identifier" :: ResourceServerIdentifierType, "Name" :: ResourceServerNameType, "Scopes" :: NullOrUndefined (ResourceServerScopeListType) }
```

#### `UpdateResourceServerResponse`

``` purescript
newtype UpdateResourceServerResponse
  = UpdateResourceServerResponse { "ResourceServer" :: ResourceServerType }
```

#### `UpdateUserAttributesRequest`

``` purescript
newtype UpdateUserAttributesRequest
  = UpdateUserAttributesRequest { "UserAttributes" :: AttributeListType, "AccessToken" :: TokenModelType }
```

<p>Represents the request to update user attributes.</p>

#### `UpdateUserAttributesResponse`

``` purescript
newtype UpdateUserAttributesResponse
  = UpdateUserAttributesResponse { "CodeDeliveryDetailsList" :: NullOrUndefined (CodeDeliveryDetailsListType) }
```

<p>Represents the response from the server for the request to update user attributes.</p>

#### `UpdateUserPoolClientRequest`

``` purescript
newtype UpdateUserPoolClientRequest
  = UpdateUserPoolClientRequest { "UserPoolId" :: UserPoolIdType, "ClientId" :: ClientIdType, "ClientName" :: NullOrUndefined (ClientNameType), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Represents the request to update the user pool client.</p>

#### `UpdateUserPoolClientResponse`

``` purescript
newtype UpdateUserPoolClientResponse
  = UpdateUserPoolClientResponse { "UserPoolClient" :: NullOrUndefined (UserPoolClientType) }
```

<p>Represents the response from the server to the request to update the user pool client.</p>

#### `UpdateUserPoolRequest`

``` purescript
newtype UpdateUserPoolRequest
  = UpdateUserPoolRequest { "UserPoolId" :: UserPoolIdType, "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>Represents the request to update the user pool.</p>

#### `UpdateUserPoolResponse`

``` purescript
newtype UpdateUserPoolResponse
  = UpdateUserPoolResponse {  }
```

<p>Represents the response from the server when you make a request to update the user pool.</p>

#### `UserContextDataType`

``` purescript
newtype UserContextDataType
  = UserContextDataType { "EncodedData" :: NullOrUndefined (StringType) }
```

<p>Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>

#### `UserFilterType`

``` purescript
newtype UserFilterType
  = UserFilterType String
```

#### `UserImportInProgressException`

``` purescript
newtype UserImportInProgressException
  = UserImportInProgressException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.</p>

#### `UserImportJobIdType`

``` purescript
newtype UserImportJobIdType
  = UserImportJobIdType String
```

#### `UserImportJobNameType`

``` purescript
newtype UserImportJobNameType
  = UserImportJobNameType String
```

#### `UserImportJobStatusType`

``` purescript
newtype UserImportJobStatusType
  = UserImportJobStatusType String
```

#### `UserImportJobType`

``` purescript
newtype UserImportJobType
  = UserImportJobType { "JobName" :: NullOrUndefined (UserImportJobNameType), "JobId" :: NullOrUndefined (UserImportJobIdType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "PreSignedUrl" :: NullOrUndefined (PreSignedUrlType), "CreationDate" :: NullOrUndefined (DateType), "StartDate" :: NullOrUndefined (DateType), "CompletionDate" :: NullOrUndefined (DateType), "Status" :: NullOrUndefined (UserImportJobStatusType), "CloudWatchLogsRoleArn" :: NullOrUndefined (ArnType), "ImportedUsers" :: NullOrUndefined (LongType), "SkippedUsers" :: NullOrUndefined (LongType), "FailedUsers" :: NullOrUndefined (LongType), "CompletionMessage" :: NullOrUndefined (CompletionMessageType) }
```

<p>The user import job type.</p>

#### `UserImportJobsListType`

``` purescript
newtype UserImportJobsListType
  = UserImportJobsListType (Array UserImportJobType)
```

#### `UserLambdaValidationException`

``` purescript
newtype UserLambdaValidationException
  = UserLambdaValidationException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.</p>

#### `UserMFASettingListType`

``` purescript
newtype UserMFASettingListType
  = UserMFASettingListType (Array StringType)
```

#### `UserNotConfirmedException`

``` purescript
newtype UserNotConfirmedException
  = UserNotConfirmedException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not confirmed successfully.</p>

#### `UserNotFoundException`

``` purescript
newtype UserNotFoundException
  = UserNotFoundException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user is not found.</p>

#### `UserPoolAddOnNotEnabledException`

``` purescript
newtype UserPoolAddOnNotEnabledException
  = UserPoolAddOnNotEnabledException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when user pool add-ons are not enabled.</p>

#### `UserPoolAddOnsType`

``` purescript
newtype UserPoolAddOnsType
  = UserPoolAddOnsType { "AdvancedSecurityMode" :: AdvancedSecurityModeType }
```

<p>The user pool add-ons type.</p>

#### `UserPoolClientDescription`

``` purescript
newtype UserPoolClientDescription
  = UserPoolClientDescription { "ClientId" :: NullOrUndefined (ClientIdType), "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientName" :: NullOrUndefined (ClientNameType) }
```

<p>The description of the user pool client.</p>

#### `UserPoolClientListType`

``` purescript
newtype UserPoolClientListType
  = UserPoolClientListType (Array UserPoolClientDescription)
```

#### `UserPoolClientType`

``` purescript
newtype UserPoolClientType
  = UserPoolClientType { "UserPoolId" :: NullOrUndefined (UserPoolIdType), "ClientName" :: NullOrUndefined (ClientNameType), "ClientId" :: NullOrUndefined (ClientIdType), "ClientSecret" :: NullOrUndefined (ClientSecretType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType), "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType), "ReadAttributes" :: NullOrUndefined (ClientPermissionListType), "WriteAttributes" :: NullOrUndefined (ClientPermissionListType), "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType), "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType), "CallbackURLs" :: NullOrUndefined (CallbackURLsListType), "LogoutURLs" :: NullOrUndefined (LogoutURLsListType), "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType), "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType), "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType), "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType), "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType) }
```

<p>Contains information about a user pool client.</p>

#### `UserPoolDescriptionType`

``` purescript
newtype UserPoolDescriptionType
  = UserPoolDescriptionType { "Id" :: NullOrUndefined (UserPoolIdType), "Name" :: NullOrUndefined (UserPoolNameType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "Status" :: NullOrUndefined (StatusType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType) }
```

<p>A user pool description.</p>

#### `UserPoolIdType`

``` purescript
newtype UserPoolIdType
  = UserPoolIdType String
```

#### `UserPoolListType`

``` purescript
newtype UserPoolListType
  = UserPoolListType (Array UserPoolDescriptionType)
```

#### `UserPoolMfaType`

``` purescript
newtype UserPoolMfaType
  = UserPoolMfaType String
```

#### `UserPoolNameType`

``` purescript
newtype UserPoolNameType
  = UserPoolNameType String
```

#### `UserPoolPolicyType`

``` purescript
newtype UserPoolPolicyType
  = UserPoolPolicyType { "PasswordPolicy" :: NullOrUndefined (PasswordPolicyType) }
```

<p>The policy associated with a user pool.</p>

#### `UserPoolTaggingException`

``` purescript
newtype UserPoolTaggingException
  = UserPoolTaggingException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when a user pool tag cannot be set or updated.</p>

#### `UserPoolTagsType`

``` purescript
newtype UserPoolTagsType
  = UserPoolTagsType (Map StringType StringType)
```

#### `UserPoolType`

``` purescript
newtype UserPoolType
  = UserPoolType { "Id" :: NullOrUndefined (UserPoolIdType), "Name" :: NullOrUndefined (UserPoolNameType), "Policies" :: NullOrUndefined (UserPoolPolicyType), "LambdaConfig" :: NullOrUndefined (LambdaConfigType), "Status" :: NullOrUndefined (StatusType), "LastModifiedDate" :: NullOrUndefined (DateType), "CreationDate" :: NullOrUndefined (DateType), "SchemaAttributes" :: NullOrUndefined (SchemaAttributesListType), "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType), "AliasAttributes" :: NullOrUndefined (AliasAttributesListType), "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType), "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType), "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType), "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType), "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType), "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType), "EstimatedNumberOfUsers" :: NullOrUndefined (IntegerType), "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType), "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType), "UserPoolTags" :: NullOrUndefined (UserPoolTagsType), "SmsConfigurationFailure" :: NullOrUndefined (StringType), "EmailConfigurationFailure" :: NullOrUndefined (StringType), "Domain" :: NullOrUndefined (DomainType), "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType), "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType) }
```

<p>A container for information about the user pool.</p>

#### `UserStatusType`

``` purescript
newtype UserStatusType
  = UserStatusType String
```

#### `UserType`

``` purescript
newtype UserType
  = UserType { "Username" :: NullOrUndefined (UsernameType), "Attributes" :: NullOrUndefined (AttributeListType), "UserCreateDate" :: NullOrUndefined (DateType), "UserLastModifiedDate" :: NullOrUndefined (DateType), "Enabled" :: NullOrUndefined (BooleanType), "UserStatus" :: NullOrUndefined (UserStatusType), "MFAOptions" :: NullOrUndefined (MFAOptionListType) }
```

<p>The user type.</p>

#### `UsernameAttributeType`

``` purescript
newtype UsernameAttributeType
  = UsernameAttributeType String
```

#### `UsernameAttributesListType`

``` purescript
newtype UsernameAttributesListType
  = UsernameAttributesListType (Array UsernameAttributeType)
```

#### `UsernameExistsException`

``` purescript
newtype UsernameExistsException
  = UsernameExistsException { "Message'" :: NullOrUndefined (MessageType) }
```

<p>This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.</p>

#### `UsernameType`

``` purescript
newtype UsernameType
  = UsernameType String
```

#### `UsersListType`

``` purescript
newtype UsersListType
  = UsersListType (Array UserType)
```

#### `VerificationMessageTemplateType`

``` purescript
newtype VerificationMessageTemplateType
  = VerificationMessageTemplateType { "SmsMessage" :: NullOrUndefined (SmsVerificationMessageType), "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType), "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType), "EmailMessageByLink" :: NullOrUndefined (EmailVerificationMessageByLinkType), "EmailSubjectByLink" :: NullOrUndefined (EmailVerificationSubjectByLinkType), "DefaultEmailOption" :: NullOrUndefined (DefaultEmailOptionType) }
```

<p>The template for verification messages.</p>

#### `VerifiedAttributeType`

``` purescript
newtype VerifiedAttributeType
  = VerifiedAttributeType String
```

#### `VerifiedAttributesListType`

``` purescript
newtype VerifiedAttributesListType
  = VerifiedAttributesListType (Array VerifiedAttributeType)
```

#### `VerifySoftwareTokenRequest`

``` purescript
newtype VerifySoftwareTokenRequest
  = VerifySoftwareTokenRequest { "AccessToken" :: NullOrUndefined (TokenModelType), "Session" :: NullOrUndefined (SessionType), "UserCode" :: SoftwareTokenMFAUserCodeType, "FriendlyDeviceName" :: NullOrUndefined (StringType) }
```

#### `VerifySoftwareTokenResponse`

``` purescript
newtype VerifySoftwareTokenResponse
  = VerifySoftwareTokenResponse { "Status" :: NullOrUndefined (VerifySoftwareTokenResponseType), "Session" :: NullOrUndefined (SessionType) }
```

#### `VerifySoftwareTokenResponseType`

``` purescript
newtype VerifySoftwareTokenResponseType
  = VerifySoftwareTokenResponseType String
```

#### `VerifyUserAttributeRequest`

``` purescript
newtype VerifyUserAttributeRequest
  = VerifyUserAttributeRequest { "AccessToken" :: TokenModelType, "AttributeName" :: AttributeNameType, "Code" :: ConfirmationCodeType }
```

<p>Represents the request to verify user attributes.</p>

#### `VerifyUserAttributeResponse`

``` purescript
newtype VerifyUserAttributeResponse
  = VerifyUserAttributeResponse {  }
```

<p>A container representing the response from the server from the request to verify user attributes.</p>


