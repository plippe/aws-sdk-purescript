

-- | <p>Using the Amazon Cognito User Pools API, you can create a user pool to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.</p> <p>This API reference provides information about user pools in Amazon Cognito User Pools.</p> <p>For more information, see the Amazon Cognito Documentation.</p>
module AWS.CognitoIdentityServiceProvider where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CognitoIdentityServiceProvider" :: String


-- | <p>Adds additional user attributes to the user pool schema.</p>
addCustomAttributes :: forall eff. AddCustomAttributesRequest -> Aff (err :: AWS.RequestError | eff) AddCustomAttributesResponse
addCustomAttributes = AWS.request serviceName "AddCustomAttributes" 


-- | <p>Adds the specified user to the specified group.</p> <p>Requires developer credentials.</p>
adminAddUserToGroup :: forall eff. AdminAddUserToGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
adminAddUserToGroup = AWS.request serviceName "AdminAddUserToGroup" 


-- | <p>Confirms user registration as an admin without using a confirmation code. Works on any user.</p> <p>Requires developer credentials.</p>
adminConfirmSignUp :: forall eff. AdminConfirmSignUpRequest -> Aff (err :: AWS.RequestError | eff) AdminConfirmSignUpResponse
adminConfirmSignUp = AWS.request serviceName "AdminConfirmSignUp" 


-- | <p>Creates a new user in the specified user pool.</p> <p>If <code>MessageAction</code> is not set, the default is to send a welcome message via email or phone (SMS).</p> <note> <p>This message is based on a template that you configured in your call to or . This template includes your custom sign-up instructions and placeholders for user name and temporary password.</p> </note> <p>Alternatively, you can call AdminCreateUser with “SUPPRESS” for the <code>MessageAction</code> parameter, and Amazon Cognito will not send any email. </p> <p>In either case, the user will be in the <code>FORCE_CHANGE_PASSWORD</code> state until they sign in and change their password.</p> <p>AdminCreateUser requires developer credentials.</p>
adminCreateUser :: forall eff. AdminCreateUserRequest -> Aff (err :: AWS.RequestError | eff) AdminCreateUserResponse
adminCreateUser = AWS.request serviceName "AdminCreateUser" 


-- | <p>Deletes a user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDeleteUser :: forall eff. AdminDeleteUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
adminDeleteUser = AWS.request serviceName "AdminDeleteUser" 


-- | <p>Deletes the user attributes in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDeleteUserAttributes :: forall eff. AdminDeleteUserAttributesRequest -> Aff (err :: AWS.RequestError | eff) AdminDeleteUserAttributesResponse
adminDeleteUserAttributes = AWS.request serviceName "AdminDeleteUserAttributes" 


-- | <p>Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked <code>DestinationUser</code>) signs in, they must create a new user account. See .</p> <p>This action is enabled only for admin access and requires developer credentials.</p> <p>The <code>ProviderName</code> must match the value specified when creating an IdP for the pool. </p> <p>To disable a native username + password user, the <code>ProviderName</code> value must be <code>Cognito</code> and the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code>, with the <code>ProviderAttributeValue</code> being the name that is used in the user pool for the user.</p> <p>The <code>ProviderAttributeName</code> must always be <code>Cognito_Subject</code> for social identity providers. The <code>ProviderAttributeValue</code> must always be the exact subject that was used when the user was originally linked as a source user.</p> <p>For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the <code>ProviderAttributeName</code> and <code>ProviderAttributeValue</code> must be the same values that were used for the <code>SourceUser</code> when the identities were originally linked in the call. (If the linking was done with <code>ProviderAttributeName</code> set to <code>Cognito_Subject</code>, the same applies here). However, if the user has already signed in, the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code> and <code>ProviderAttributeValue</code> must be the subject of the SAML assertion.</p>
adminDisableProviderForUser :: forall eff. AdminDisableProviderForUserRequest -> Aff (err :: AWS.RequestError | eff) AdminDisableProviderForUserResponse
adminDisableProviderForUser = AWS.request serviceName "AdminDisableProviderForUser" 


-- | <p>Disables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDisableUser :: forall eff. AdminDisableUserRequest -> Aff (err :: AWS.RequestError | eff) AdminDisableUserResponse
adminDisableUser = AWS.request serviceName "AdminDisableUser" 


-- | <p>Enables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminEnableUser :: forall eff. AdminEnableUserRequest -> Aff (err :: AWS.RequestError | eff) AdminEnableUserResponse
adminEnableUser = AWS.request serviceName "AdminEnableUser" 


-- | <p>Forgets the device, as an administrator.</p> <p>Requires developer credentials.</p>
adminForgetDevice :: forall eff. AdminForgetDeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
adminForgetDevice = AWS.request serviceName "AdminForgetDevice" 


-- | <p>Gets the device, as an administrator.</p> <p>Requires developer credentials.</p>
adminGetDevice :: forall eff. AdminGetDeviceRequest -> Aff (err :: AWS.RequestError | eff) AdminGetDeviceResponse
adminGetDevice = AWS.request serviceName "AdminGetDevice" 


-- | <p>Gets the specified user by user name in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminGetUser :: forall eff. AdminGetUserRequest -> Aff (err :: AWS.RequestError | eff) AdminGetUserResponse
adminGetUser = AWS.request serviceName "AdminGetUser" 


-- | <p>Initiates the authentication flow, as an administrator.</p> <p>Requires developer credentials.</p>
adminInitiateAuth :: forall eff. AdminInitiateAuthRequest -> Aff (err :: AWS.RequestError | eff) AdminInitiateAuthResponse
adminInitiateAuth = AWS.request serviceName "AdminInitiateAuth" 


-- | <p>Links an existing user account in a user pool (<code>DestinationUser</code>) to an identity from an external identity provider (<code>SourceUser</code>) based on a specified attribute name and value from the external identity provider. This allows you to create a link from the existing user account to an external federated user identity that has not yet been used to sign in, so that the federated user identity can be used to sign in as the existing user account. </p> <p> For example, if there is an existing user with a username and password, this API links that user to a federated user identity, so that when the federated user identity is used, the user signs in as the existing user account. </p> <important> <p>Because this API allows a user with an external federated identity to sign in as an existing user in the user pool, it is critical that it only be used with external identity providers and provider attributes that have been trusted by the application owner.</p> </important> <p>See also .</p> <p>This action is enabled only for admin access and requires developer credentials.</p>
adminLinkProviderForUser :: forall eff. AdminLinkProviderForUserRequest -> Aff (err :: AWS.RequestError | eff) AdminLinkProviderForUserResponse
adminLinkProviderForUser = AWS.request serviceName "AdminLinkProviderForUser" 


-- | <p>Lists devices, as an administrator.</p> <p>Requires developer credentials.</p>
adminListDevices :: forall eff. AdminListDevicesRequest -> Aff (err :: AWS.RequestError | eff) AdminListDevicesResponse
adminListDevices = AWS.request serviceName "AdminListDevices" 


-- | <p>Lists the groups that the user belongs to.</p> <p>Requires developer credentials.</p>
adminListGroupsForUser :: forall eff. AdminListGroupsForUserRequest -> Aff (err :: AWS.RequestError | eff) AdminListGroupsForUserResponse
adminListGroupsForUser = AWS.request serviceName "AdminListGroupsForUser" 


-- | <p>Lists a history of user activity and any risks detected as part of Amazon Cognito advanced security.</p>
adminListUserAuthEvents :: forall eff. AdminListUserAuthEventsRequest -> Aff (err :: AWS.RequestError | eff) AdminListUserAuthEventsResponse
adminListUserAuthEvents = AWS.request serviceName "AdminListUserAuthEvents" 


-- | <p>Removes the specified user from the specified group.</p> <p>Requires developer credentials.</p>
adminRemoveUserFromGroup :: forall eff. AdminRemoveUserFromGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
adminRemoveUserFromGroup = AWS.request serviceName "AdminRemoveUserFromGroup" 


-- | <p>Resets the specified user's password in a user pool as an administrator. Works on any user.</p> <p>When a developer calls this API, the current password is invalidated, so it must be changed. If a user tries to sign in after the API is called, the app will get a PasswordResetRequiredException exception back and should direct the user down the flow to reset the password, which is the same as the forgot password flow. In addition, if the user pool has phone verification selected and a verified phone number exists for the user, or if email verification is selected and a verified email exists for the user, calling this API will also result in sending a message to the end user with the code to change their password.</p> <p>Requires developer credentials.</p>
adminResetUserPassword :: forall eff. AdminResetUserPasswordRequest -> Aff (err :: AWS.RequestError | eff) AdminResetUserPasswordResponse
adminResetUserPassword = AWS.request serviceName "AdminResetUserPassword" 


-- | <p>Responds to an authentication challenge, as an administrator.</p> <p>Requires developer credentials.</p>
adminRespondToAuthChallenge :: forall eff. AdminRespondToAuthChallengeRequest -> Aff (err :: AWS.RequestError | eff) AdminRespondToAuthChallengeResponse
adminRespondToAuthChallenge = AWS.request serviceName "AdminRespondToAuthChallenge" 


-- | <p>Sets the user's multi-factor authentication (MFA) preference.</p>
adminSetUserMFAPreference :: forall eff. AdminSetUserMFAPreferenceRequest -> Aff (err :: AWS.RequestError | eff) AdminSetUserMFAPreferenceResponse
adminSetUserMFAPreference = AWS.request serviceName "AdminSetUserMFAPreference" 


-- | <p>Sets all the user settings for a specified user name. Works on any user.</p> <p>Requires developer credentials.</p>
adminSetUserSettings :: forall eff. AdminSetUserSettingsRequest -> Aff (err :: AWS.RequestError | eff) AdminSetUserSettingsResponse
adminSetUserSettings = AWS.request serviceName "AdminSetUserSettings" 


-- | <p>Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>
adminUpdateAuthEventFeedback :: forall eff. AdminUpdateAuthEventFeedbackRequest -> Aff (err :: AWS.RequestError | eff) AdminUpdateAuthEventFeedbackResponse
adminUpdateAuthEventFeedback = AWS.request serviceName "AdminUpdateAuthEventFeedback" 


-- | <p>Updates the device status as an administrator.</p> <p>Requires developer credentials.</p>
adminUpdateDeviceStatus :: forall eff. AdminUpdateDeviceStatusRequest -> Aff (err :: AWS.RequestError | eff) AdminUpdateDeviceStatusResponse
adminUpdateDeviceStatus = AWS.request serviceName "AdminUpdateDeviceStatus" 


-- | <p>Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.</p> <p>For custom attributes, you must prepend the <code>custom:</code> prefix to the attribute name.</p> <p>In addition to updating user attributes, this API can also be used to mark phone and email as verified.</p> <p>Requires developer credentials.</p>
adminUpdateUserAttributes :: forall eff. AdminUpdateUserAttributesRequest -> Aff (err :: AWS.RequestError | eff) AdminUpdateUserAttributesResponse
adminUpdateUserAttributes = AWS.request serviceName "AdminUpdateUserAttributes" 


-- | <p>Signs out users from all devices, as an administrator.</p> <p>Requires developer credentials.</p>
adminUserGlobalSignOut :: forall eff. AdminUserGlobalSignOutRequest -> Aff (err :: AWS.RequestError | eff) AdminUserGlobalSignOutResponse
adminUserGlobalSignOut = AWS.request serviceName "AdminUserGlobalSignOut" 


-- | <p>Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.</p>
associateSoftwareToken :: forall eff. AssociateSoftwareTokenRequest -> Aff (err :: AWS.RequestError | eff) AssociateSoftwareTokenResponse
associateSoftwareToken = AWS.request serviceName "AssociateSoftwareToken" 


-- | <p>Changes the password for a specified user in a user pool.</p>
changePassword :: forall eff. ChangePasswordRequest -> Aff (err :: AWS.RequestError | eff) ChangePasswordResponse
changePassword = AWS.request serviceName "ChangePassword" 


-- | <p>Confirms tracking of the device. This API call is the call that begins device tracking.</p>
confirmDevice :: forall eff. ConfirmDeviceRequest -> Aff (err :: AWS.RequestError | eff) ConfirmDeviceResponse
confirmDevice = AWS.request serviceName "ConfirmDevice" 


-- | <p>Allows a user to enter a confirmation code to reset a forgotten password.</p>
confirmForgotPassword :: forall eff. ConfirmForgotPasswordRequest -> Aff (err :: AWS.RequestError | eff) ConfirmForgotPasswordResponse
confirmForgotPassword = AWS.request serviceName "ConfirmForgotPassword" 


-- | <p>Confirms registration of a user and handles the existing alias from a previous user.</p>
confirmSignUp :: forall eff. ConfirmSignUpRequest -> Aff (err :: AWS.RequestError | eff) ConfirmSignUpResponse
confirmSignUp = AWS.request serviceName "ConfirmSignUp" 


-- | <p>Creates a new group in the specified user pool.</p> <p>Requires developer credentials.</p>
createGroup :: forall eff. CreateGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateGroupResponse
createGroup = AWS.request serviceName "CreateGroup" 


-- | <p>Creates an identity provider for a user pool.</p>
createIdentityProvider :: forall eff. CreateIdentityProviderRequest -> Aff (err :: AWS.RequestError | eff) CreateIdentityProviderResponse
createIdentityProvider = AWS.request serviceName "CreateIdentityProvider" 


-- | <p>Creates a new OAuth2.0 resource server and defines custom scopes in it.</p>
createResourceServer :: forall eff. CreateResourceServerRequest -> Aff (err :: AWS.RequestError | eff) CreateResourceServerResponse
createResourceServer = AWS.request serviceName "CreateResourceServer" 


-- | <p>Creates the user import job.</p>
createUserImportJob :: forall eff. CreateUserImportJobRequest -> Aff (err :: AWS.RequestError | eff) CreateUserImportJobResponse
createUserImportJob = AWS.request serviceName "CreateUserImportJob" 


-- | <p>Creates a new Amazon Cognito user pool and sets the password policy for the pool.</p>
createUserPool :: forall eff. CreateUserPoolRequest -> Aff (err :: AWS.RequestError | eff) CreateUserPoolResponse
createUserPool = AWS.request serviceName "CreateUserPool" 


-- | <p>Creates the user pool client.</p>
createUserPoolClient :: forall eff. CreateUserPoolClientRequest -> Aff (err :: AWS.RequestError | eff) CreateUserPoolClientResponse
createUserPoolClient = AWS.request serviceName "CreateUserPoolClient" 


-- | <p>Creates a new domain for a user pool.</p>
createUserPoolDomain :: forall eff. CreateUserPoolDomainRequest -> Aff (err :: AWS.RequestError | eff) CreateUserPoolDomainResponse
createUserPoolDomain = AWS.request serviceName "CreateUserPoolDomain" 


-- | <p>Deletes a group. Currently only groups with no members can be deleted.</p> <p>Requires developer credentials.</p>
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteGroup = AWS.request serviceName "DeleteGroup" 


-- | <p>Deletes an identity provider for a user pool.</p>
deleteIdentityProvider :: forall eff. DeleteIdentityProviderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteIdentityProvider = AWS.request serviceName "DeleteIdentityProvider" 


-- | <p>Deletes a resource server.</p>
deleteResourceServer :: forall eff. DeleteResourceServerRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteResourceServer = AWS.request serviceName "DeleteResourceServer" 


-- | <p>Allows a user to delete himself or herself.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUser = AWS.request serviceName "DeleteUser" 


-- | <p>Deletes the attributes for a user.</p>
deleteUserAttributes :: forall eff. DeleteUserAttributesRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserAttributesResponse
deleteUserAttributes = AWS.request serviceName "DeleteUserAttributes" 


-- | <p>Deletes the specified Amazon Cognito user pool.</p>
deleteUserPool :: forall eff. DeleteUserPoolRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUserPool = AWS.request serviceName "DeleteUserPool" 


-- | <p>Allows the developer to delete the user pool client.</p>
deleteUserPoolClient :: forall eff. DeleteUserPoolClientRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUserPoolClient = AWS.request serviceName "DeleteUserPoolClient" 


-- | <p>Deletes a domain for a user pool.</p>
deleteUserPoolDomain :: forall eff. DeleteUserPoolDomainRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserPoolDomainResponse
deleteUserPoolDomain = AWS.request serviceName "DeleteUserPoolDomain" 


-- | <p>Gets information about a specific identity provider.</p>
describeIdentityProvider :: forall eff. DescribeIdentityProviderRequest -> Aff (err :: AWS.RequestError | eff) DescribeIdentityProviderResponse
describeIdentityProvider = AWS.request serviceName "DescribeIdentityProvider" 


-- | <p>Describes a resource server.</p>
describeResourceServer :: forall eff. DescribeResourceServerRequest -> Aff (err :: AWS.RequestError | eff) DescribeResourceServerResponse
describeResourceServer = AWS.request serviceName "DescribeResourceServer" 


-- | <p>Describes the risk configuration.</p>
describeRiskConfiguration :: forall eff. DescribeRiskConfigurationRequest -> Aff (err :: AWS.RequestError | eff) DescribeRiskConfigurationResponse
describeRiskConfiguration = AWS.request serviceName "DescribeRiskConfiguration" 


-- | <p>Describes the user import job.</p>
describeUserImportJob :: forall eff. DescribeUserImportJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserImportJobResponse
describeUserImportJob = AWS.request serviceName "DescribeUserImportJob" 


-- | <p>Returns the configuration information and metadata of the specified user pool.</p>
describeUserPool :: forall eff. DescribeUserPoolRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserPoolResponse
describeUserPool = AWS.request serviceName "DescribeUserPool" 


-- | <p>Client method for returning the configuration information and metadata of the specified user pool client.</p>
describeUserPoolClient :: forall eff. DescribeUserPoolClientRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserPoolClientResponse
describeUserPoolClient = AWS.request serviceName "DescribeUserPoolClient" 


-- | <p>Gets information about a domain.</p>
describeUserPoolDomain :: forall eff. DescribeUserPoolDomainRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserPoolDomainResponse
describeUserPoolDomain = AWS.request serviceName "DescribeUserPoolDomain" 


-- | <p>Forgets the specified device.</p>
forgetDevice :: forall eff. ForgetDeviceRequest -> Aff (err :: AWS.RequestError | eff) Unit
forgetDevice = AWS.request serviceName "ForgetDevice" 


-- | <p>Calling this API causes a message to be sent to the end user with a confirmation code that is required to change the user's password. For the <code>Username</code> parameter, you can use the username or user alias. If a verified phone number exists for the user, the confirmation code is sent to the phone number. Otherwise, if a verified email exists, the confirmation code is sent to the email. If neither a verified phone number nor a verified email exists, <code>InvalidParameterException</code> is thrown. To use the confirmation code for resetting the password, call .</p>
forgotPassword :: forall eff. ForgotPasswordRequest -> Aff (err :: AWS.RequestError | eff) ForgotPasswordResponse
forgotPassword = AWS.request serviceName "ForgotPassword" 


-- | <p>Gets the header information for the .csv file to be used as input for the user import job.</p>
getCSVHeader :: forall eff. GetCSVHeaderRequest -> Aff (err :: AWS.RequestError | eff) GetCSVHeaderResponse
getCSVHeader = AWS.request serviceName "GetCSVHeader" 


-- | <p>Gets the device.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceResponse
getDevice = AWS.request serviceName "GetDevice" 


-- | <p>Gets a group.</p> <p>Requires developer credentials.</p>
getGroup :: forall eff. GetGroupRequest -> Aff (err :: AWS.RequestError | eff) GetGroupResponse
getGroup = AWS.request serviceName "GetGroup" 


-- | <p>Gets the specified identity provider.</p>
getIdentityProviderByIdentifier :: forall eff. GetIdentityProviderByIdentifierRequest -> Aff (err :: AWS.RequestError | eff) GetIdentityProviderByIdentifierResponse
getIdentityProviderByIdentifier = AWS.request serviceName "GetIdentityProviderByIdentifier" 


-- | <p>This method takes a user pool ID, and returns the signing certificate.</p>
getSigningCertificate :: forall eff. GetSigningCertificateRequest -> Aff (err :: AWS.RequestError | eff) GetSigningCertificateResponse
getSigningCertificate = AWS.request serviceName "GetSigningCertificate" 


-- | <p>Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app <code>clientId</code> will be <code>ALL</code>), then that is returned. If nothing is present, then an empty shape is returned.</p>
getUICustomization :: forall eff. GetUICustomizationRequest -> Aff (err :: AWS.RequestError | eff) GetUICustomizationResponse
getUICustomization = AWS.request serviceName "GetUICustomization" 


-- | <p>Gets the user attributes and metadata for a user.</p>
getUser :: forall eff. GetUserRequest -> Aff (err :: AWS.RequestError | eff) GetUserResponse
getUser = AWS.request serviceName "GetUser" 


-- | <p>Gets the user attribute verification code for the specified attribute name.</p>
getUserAttributeVerificationCode :: forall eff. GetUserAttributeVerificationCodeRequest -> Aff (err :: AWS.RequestError | eff) GetUserAttributeVerificationCodeResponse
getUserAttributeVerificationCode = AWS.request serviceName "GetUserAttributeVerificationCode" 


-- | <p>Gets the user pool multi-factor authentication (MFA) configuration.</p>
getUserPoolMfaConfig :: forall eff. GetUserPoolMfaConfigRequest -> Aff (err :: AWS.RequestError | eff) GetUserPoolMfaConfigResponse
getUserPoolMfaConfig = AWS.request serviceName "GetUserPoolMfaConfig" 


-- | <p>Signs out users from all devices.</p>
globalSignOut :: forall eff. GlobalSignOutRequest -> Aff (err :: AWS.RequestError | eff) GlobalSignOutResponse
globalSignOut = AWS.request serviceName "GlobalSignOut" 


-- | <p>Initiates the authentication flow.</p>
initiateAuth :: forall eff. InitiateAuthRequest -> Aff (err :: AWS.RequestError | eff) InitiateAuthResponse
initiateAuth = AWS.request serviceName "InitiateAuth" 


-- | <p>Lists the devices.</p>
listDevices :: forall eff. ListDevicesRequest -> Aff (err :: AWS.RequestError | eff) ListDevicesResponse
listDevices = AWS.request serviceName "ListDevices" 


-- | <p>Lists the groups associated with a user pool.</p> <p>Requires developer credentials.</p>
listGroups :: forall eff. ListGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListGroupsResponse
listGroups = AWS.request serviceName "ListGroups" 


-- | <p>Lists information about all identity providers for a user pool.</p>
listIdentityProviders :: forall eff. ListIdentityProvidersRequest -> Aff (err :: AWS.RequestError | eff) ListIdentityProvidersResponse
listIdentityProviders = AWS.request serviceName "ListIdentityProviders" 


-- | <p>Lists the resource servers for a user pool.</p>
listResourceServers :: forall eff. ListResourceServersRequest -> Aff (err :: AWS.RequestError | eff) ListResourceServersResponse
listResourceServers = AWS.request serviceName "ListResourceServers" 


-- | <p>Lists the user import jobs.</p>
listUserImportJobs :: forall eff. ListUserImportJobsRequest -> Aff (err :: AWS.RequestError | eff) ListUserImportJobsResponse
listUserImportJobs = AWS.request serviceName "ListUserImportJobs" 


-- | <p>Lists the clients that have been created for the specified user pool.</p>
listUserPoolClients :: forall eff. ListUserPoolClientsRequest -> Aff (err :: AWS.RequestError | eff) ListUserPoolClientsResponse
listUserPoolClients = AWS.request serviceName "ListUserPoolClients" 


-- | <p>Lists the user pools associated with an AWS account.</p>
listUserPools :: forall eff. ListUserPoolsRequest -> Aff (err :: AWS.RequestError | eff) ListUserPoolsResponse
listUserPools = AWS.request serviceName "ListUserPools" 


-- | <p>Lists the users in the Amazon Cognito user pool.</p>
listUsers :: forall eff. ListUsersRequest -> Aff (err :: AWS.RequestError | eff) ListUsersResponse
listUsers = AWS.request serviceName "ListUsers" 


-- | <p>Lists the users in the specified group.</p> <p>Requires developer credentials.</p>
listUsersInGroup :: forall eff. ListUsersInGroupRequest -> Aff (err :: AWS.RequestError | eff) ListUsersInGroupResponse
listUsersInGroup = AWS.request serviceName "ListUsersInGroup" 


-- | <p>Resends the confirmation (for confirmation of registration) to a specific user in the user pool.</p>
resendConfirmationCode :: forall eff. ResendConfirmationCodeRequest -> Aff (err :: AWS.RequestError | eff) ResendConfirmationCodeResponse
resendConfirmationCode = AWS.request serviceName "ResendConfirmationCode" 


-- | <p>Responds to the authentication challenge.</p>
respondToAuthChallenge :: forall eff. RespondToAuthChallengeRequest -> Aff (err :: AWS.RequestError | eff) RespondToAuthChallengeResponse
respondToAuthChallenge = AWS.request serviceName "RespondToAuthChallenge" 


-- | <p>Configures actions on detected risks. To delete the risk configuration for <code>UserPoolId</code> or <code>ClientId</code>, pass null values for all four configuration types.</p> <p>To enable Amazon Cognito advanced security features, update the user pool to include the <code>UserPoolAddOns</code> key<code>AdvancedSecurityMode</code>.</p> <p>See .</p>
setRiskConfiguration :: forall eff. SetRiskConfigurationRequest -> Aff (err :: AWS.RequestError | eff) SetRiskConfigurationResponse
setRiskConfiguration = AWS.request serviceName "SetRiskConfiguration" 


-- | <p>Sets the UI customization information for a user pool's built-in app UI.</p> <p>You can specify app UI customization settings for a single client (with a specific <code>clientId</code>) or for all clients (by setting the <code>clientId</code> to <code>ALL</code>). If you specify <code>ALL</code>, the default configuration will be used for every client that has no UI customization set previously. If you specify UI customization settings for a particular client, it will no longer fall back to the <code>ALL</code> configuration. </p> <note> <p>To use this API, your user pool must have a domain associated with it. Otherwise, there is no place to host the app's pages, and the service will throw an error.</p> </note>
setUICustomization :: forall eff. SetUICustomizationRequest -> Aff (err :: AWS.RequestError | eff) SetUICustomizationResponse
setUICustomization = AWS.request serviceName "SetUICustomization" 


-- | <p>Set the user's multi-factor authentication (MFA) method preference.</p>
setUserMFAPreference :: forall eff. SetUserMFAPreferenceRequest -> Aff (err :: AWS.RequestError | eff) SetUserMFAPreferenceResponse
setUserMFAPreference = AWS.request serviceName "SetUserMFAPreference" 


-- | <p>Set the user pool MFA configuration.</p>
setUserPoolMfaConfig :: forall eff. SetUserPoolMfaConfigRequest -> Aff (err :: AWS.RequestError | eff) SetUserPoolMfaConfigResponse
setUserPoolMfaConfig = AWS.request serviceName "SetUserPoolMfaConfig" 


-- | <p>Sets the user settings like multi-factor authentication (MFA). If MFA is to be removed for a particular attribute pass the attribute with code delivery as null. If null list is passed, all MFA options are removed.</p>
setUserSettings :: forall eff. SetUserSettingsRequest -> Aff (err :: AWS.RequestError | eff) SetUserSettingsResponse
setUserSettings = AWS.request serviceName "SetUserSettings" 


-- | <p>Registers the user in the specified user pool and creates a user name, password, and user attributes.</p>
signUp :: forall eff. SignUpRequest -> Aff (err :: AWS.RequestError | eff) SignUpResponse
signUp = AWS.request serviceName "SignUp" 


-- | <p>Starts the user import.</p>
startUserImportJob :: forall eff. StartUserImportJobRequest -> Aff (err :: AWS.RequestError | eff) StartUserImportJobResponse
startUserImportJob = AWS.request serviceName "StartUserImportJob" 


-- | <p>Stops the user import job.</p>
stopUserImportJob :: forall eff. StopUserImportJobRequest -> Aff (err :: AWS.RequestError | eff) StopUserImportJobResponse
stopUserImportJob = AWS.request serviceName "StopUserImportJob" 


-- | <p>Provides the feedback for an authentication event whether it was from a valid user or not. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>
updateAuthEventFeedback :: forall eff. UpdateAuthEventFeedbackRequest -> Aff (err :: AWS.RequestError | eff) UpdateAuthEventFeedbackResponse
updateAuthEventFeedback = AWS.request serviceName "UpdateAuthEventFeedback" 


-- | <p>Updates the device status.</p>
updateDeviceStatus :: forall eff. UpdateDeviceStatusRequest -> Aff (err :: AWS.RequestError | eff) UpdateDeviceStatusResponse
updateDeviceStatus = AWS.request serviceName "UpdateDeviceStatus" 


-- | <p>Updates the specified group with the specified attributes.</p> <p>Requires developer credentials.</p>
updateGroup :: forall eff. UpdateGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateGroupResponse
updateGroup = AWS.request serviceName "UpdateGroup" 


-- | <p>Updates identity provider information for a user pool.</p>
updateIdentityProvider :: forall eff. UpdateIdentityProviderRequest -> Aff (err :: AWS.RequestError | eff) UpdateIdentityProviderResponse
updateIdentityProvider = AWS.request serviceName "UpdateIdentityProvider" 


-- | <p>Updates the name and scopes of resource server. All other fields are read-only.</p>
updateResourceServer :: forall eff. UpdateResourceServerRequest -> Aff (err :: AWS.RequestError | eff) UpdateResourceServerResponse
updateResourceServer = AWS.request serviceName "UpdateResourceServer" 


-- | <p>Allows a user to update a specific attribute (one at a time).</p>
updateUserAttributes :: forall eff. UpdateUserAttributesRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserAttributesResponse
updateUserAttributes = AWS.request serviceName "UpdateUserAttributes" 


-- | <p>Updates the specified user pool with the specified attributes.</p>
updateUserPool :: forall eff. UpdateUserPoolRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserPoolResponse
updateUserPool = AWS.request serviceName "UpdateUserPool" 


-- | <p>Allows the developer to update the specified user pool client and password policy.</p>
updateUserPoolClient :: forall eff. UpdateUserPoolClientRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserPoolClientResponse
updateUserPoolClient = AWS.request serviceName "UpdateUserPoolClient" 


-- | <p>Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful,</p>
verifySoftwareToken :: forall eff. VerifySoftwareTokenRequest -> Aff (err :: AWS.RequestError | eff) VerifySoftwareTokenResponse
verifySoftwareToken = AWS.request serviceName "VerifySoftwareToken" 


-- | <p>Verifies the specified user attributes in the user pool.</p>
verifyUserAttribute :: forall eff. VerifyUserAttributeRequest -> Aff (err :: AWS.RequestError | eff) VerifyUserAttributeResponse
verifyUserAttribute = AWS.request serviceName "VerifyUserAttribute" 


newtype AWSAccountIdType = AWSAccountIdType String
derive instance newtypeAWSAccountIdType :: Newtype AWSAccountIdType _


newtype AccountTakeoverActionNotifyType = AccountTakeoverActionNotifyType Boolean
derive instance newtypeAccountTakeoverActionNotifyType :: Newtype AccountTakeoverActionNotifyType _


-- | <p>Account takeover action type.</p>
newtype AccountTakeoverActionType = AccountTakeoverActionType 
  { "Notify" :: (AccountTakeoverActionNotifyType)
  , "EventAction" :: (AccountTakeoverEventActionType)
  }
derive instance newtypeAccountTakeoverActionType :: Newtype AccountTakeoverActionType _


-- | <p>Account takeover actions type.</p>
newtype AccountTakeoverActionsType = AccountTakeoverActionsType 
  { "LowAction" :: NullOrUndefined (AccountTakeoverActionType)
  , "MediumAction" :: NullOrUndefined (AccountTakeoverActionType)
  , "HighAction" :: NullOrUndefined (AccountTakeoverActionType)
  }
derive instance newtypeAccountTakeoverActionsType :: Newtype AccountTakeoverActionsType _


newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType String
derive instance newtypeAccountTakeoverEventActionType :: Newtype AccountTakeoverEventActionType _


-- | <p>Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.</p>
newtype AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType 
  { "NotifyConfiguration" :: NullOrUndefined (NotifyConfigurationType)
  , "Actions" :: (AccountTakeoverActionsType)
  }
derive instance newtypeAccountTakeoverRiskConfigurationType :: Newtype AccountTakeoverRiskConfigurationType _


-- | <p>Represents the request to add custom attributes.</p>
newtype AddCustomAttributesRequest = AddCustomAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "CustomAttributes" :: (CustomAttributesListType)
  }
derive instance newtypeAddCustomAttributesRequest :: Newtype AddCustomAttributesRequest _


-- | <p>Represents the response from the server for the request to add custom attributes.</p>
newtype AddCustomAttributesResponse = AddCustomAttributesResponse 
  { 
  }
derive instance newtypeAddCustomAttributesResponse :: Newtype AddCustomAttributesResponse _


newtype AdminAddUserToGroupRequest = AdminAddUserToGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "GroupName" :: (GroupNameType)
  }
derive instance newtypeAdminAddUserToGroupRequest :: Newtype AdminAddUserToGroupRequest _


-- | <p>Represents the request to confirm user registration.</p>
newtype AdminConfirmSignUpRequest = AdminConfirmSignUpRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminConfirmSignUpRequest :: Newtype AdminConfirmSignUpRequest _


-- | <p>Represents the response from the server for the request to confirm registration.</p>
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse 
  { 
  }
derive instance newtypeAdminConfirmSignUpResponse :: Newtype AdminConfirmSignUpResponse _


-- | <p>The configuration for creating a new user profile.</p>
newtype AdminCreateUserConfigType = AdminCreateUserConfigType 
  { "AllowAdminCreateUserOnly" :: NullOrUndefined (BooleanType)
  , "UnusedAccountValidityDays" :: NullOrUndefined (AdminCreateUserUnusedAccountValidityDaysType)
  , "InviteMessageTemplate" :: NullOrUndefined (MessageTemplateType)
  }
derive instance newtypeAdminCreateUserConfigType :: Newtype AdminCreateUserConfigType _


-- | <p>Represents the request to create a user in the specified user pool.</p>
newtype AdminCreateUserRequest = AdminCreateUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributes" :: NullOrUndefined (AttributeListType)
  , "ValidationData" :: NullOrUndefined (AttributeListType)
  , "TemporaryPassword" :: NullOrUndefined (PasswordType)
  , "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation)
  , "MessageAction" :: NullOrUndefined (MessageActionType)
  , "DesiredDeliveryMediums" :: NullOrUndefined (DeliveryMediumListType)
  }
derive instance newtypeAdminCreateUserRequest :: Newtype AdminCreateUserRequest _


-- | <p>Represents the response from the server to the request to create the user.</p>
newtype AdminCreateUserResponse = AdminCreateUserResponse 
  { "User" :: NullOrUndefined (UserType)
  }
derive instance newtypeAdminCreateUserResponse :: Newtype AdminCreateUserResponse _


newtype AdminCreateUserUnusedAccountValidityDaysType = AdminCreateUserUnusedAccountValidityDaysType Int
derive instance newtypeAdminCreateUserUnusedAccountValidityDaysType :: Newtype AdminCreateUserUnusedAccountValidityDaysType _


-- | <p>Represents the request to delete user attributes as an administrator.</p>
newtype AdminDeleteUserAttributesRequest = AdminDeleteUserAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributeNames" :: (AttributeNameListType)
  }
derive instance newtypeAdminDeleteUserAttributesRequest :: Newtype AdminDeleteUserAttributesRequest _


-- | <p>Represents the response received from the server for a request to delete user attributes.</p>
newtype AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse 
  { 
  }
derive instance newtypeAdminDeleteUserAttributesResponse :: Newtype AdminDeleteUserAttributesResponse _


-- | <p>Represents the request to delete a user as an administrator.</p>
newtype AdminDeleteUserRequest = AdminDeleteUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminDeleteUserRequest :: Newtype AdminDeleteUserRequest _


newtype AdminDisableProviderForUserRequest = AdminDisableProviderForUserRequest 
  { "UserPoolId" :: (StringType)
  , "User" :: (ProviderUserIdentifierType)
  }
derive instance newtypeAdminDisableProviderForUserRequest :: Newtype AdminDisableProviderForUserRequest _


newtype AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse 
  { 
  }
derive instance newtypeAdminDisableProviderForUserResponse :: Newtype AdminDisableProviderForUserResponse _


-- | <p>Represents the request to disable any user as an administrator.</p>
newtype AdminDisableUserRequest = AdminDisableUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminDisableUserRequest :: Newtype AdminDisableUserRequest _


-- | <p>Represents the response received from the server to disable the user as an administrator.</p>
newtype AdminDisableUserResponse = AdminDisableUserResponse 
  { 
  }
derive instance newtypeAdminDisableUserResponse :: Newtype AdminDisableUserResponse _


-- | <p>Represents the request that enables the user as an administrator.</p>
newtype AdminEnableUserRequest = AdminEnableUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminEnableUserRequest :: Newtype AdminEnableUserRequest _


-- | <p>Represents the response from the server for the request to enable a user as an administrator.</p>
newtype AdminEnableUserResponse = AdminEnableUserResponse 
  { 
  }
derive instance newtypeAdminEnableUserResponse :: Newtype AdminEnableUserResponse _


-- | <p>Sends the forgot device request, as an administrator.</p>
newtype AdminForgetDeviceRequest = AdminForgetDeviceRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "DeviceKey" :: (DeviceKeyType)
  }
derive instance newtypeAdminForgetDeviceRequest :: Newtype AdminForgetDeviceRequest _


-- | <p>Represents the request to get the device, as an administrator.</p>
newtype AdminGetDeviceRequest = AdminGetDeviceRequest 
  { "DeviceKey" :: (DeviceKeyType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminGetDeviceRequest :: Newtype AdminGetDeviceRequest _


-- | <p>Gets the device response, as an administrator.</p>
newtype AdminGetDeviceResponse = AdminGetDeviceResponse 
  { "Device" :: (DeviceType)
  }
derive instance newtypeAdminGetDeviceResponse :: Newtype AdminGetDeviceResponse _


-- | <p>Represents the request to get the specified user as an administrator.</p>
newtype AdminGetUserRequest = AdminGetUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminGetUserRequest :: Newtype AdminGetUserRequest _


-- | <p>Represents the response from the server from the request to get the specified user as an administrator.</p>
newtype AdminGetUserResponse = AdminGetUserResponse 
  { "Username" :: (UsernameType)
  , "UserAttributes" :: NullOrUndefined (AttributeListType)
  , "UserCreateDate" :: NullOrUndefined (DateType)
  , "UserLastModifiedDate" :: NullOrUndefined (DateType)
  , "Enabled" :: NullOrUndefined (BooleanType)
  , "UserStatus" :: NullOrUndefined (UserStatusType)
  , "MFAOptions" :: NullOrUndefined (MFAOptionListType)
  , "PreferredMfaSetting" :: NullOrUndefined (StringType)
  , "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType)
  }
derive instance newtypeAdminGetUserResponse :: Newtype AdminGetUserResponse _


-- | <p>Initiates the authorization request, as an administrator.</p>
newtype AdminInitiateAuthRequest = AdminInitiateAuthRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "AuthFlow" :: (AuthFlowType)
  , "AuthParameters" :: NullOrUndefined (AuthParametersType)
  , "ClientMetadata" :: NullOrUndefined (ClientMetadataType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "ContextData" :: NullOrUndefined (ContextDataType)
  }
derive instance newtypeAdminInitiateAuthRequest :: Newtype AdminInitiateAuthRequest _


-- | <p>Initiates the authentication response, as an administrator.</p>
newtype AdminInitiateAuthResponse = AdminInitiateAuthResponse 
  { "ChallengeName" :: NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeAdminInitiateAuthResponse :: Newtype AdminInitiateAuthResponse _


newtype AdminLinkProviderForUserRequest = AdminLinkProviderForUserRequest 
  { "UserPoolId" :: (StringType)
  , "DestinationUser" :: (ProviderUserIdentifierType)
  , "SourceUser" :: (ProviderUserIdentifierType)
  }
derive instance newtypeAdminLinkProviderForUserRequest :: Newtype AdminLinkProviderForUserRequest _


newtype AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse 
  { 
  }
derive instance newtypeAdminLinkProviderForUserResponse :: Newtype AdminLinkProviderForUserResponse _


-- | <p>Represents the request to list devices, as an administrator.</p>
newtype AdminListDevicesRequest = AdminListDevicesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeAdminListDevicesRequest :: Newtype AdminListDevicesRequest _


-- | <p>Lists the device's response, as an administrator.</p>
newtype AdminListDevicesResponse = AdminListDevicesResponse 
  { "Devices" :: NullOrUndefined (DeviceListType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeAdminListDevicesResponse :: Newtype AdminListDevicesResponse _


newtype AdminListGroupsForUserRequest = AdminListGroupsForUserRequest 
  { "Username" :: (UsernameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListGroupsForUserRequest :: Newtype AdminListGroupsForUserRequest _


newtype AdminListGroupsForUserResponse = AdminListGroupsForUserResponse 
  { "Groups" :: NullOrUndefined (GroupListType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListGroupsForUserResponse :: Newtype AdminListGroupsForUserResponse _


newtype AdminListUserAuthEventsRequest = AdminListUserAuthEventsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "MaxResults" :: NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListUserAuthEventsRequest :: Newtype AdminListUserAuthEventsRequest _


newtype AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse 
  { "AuthEvents" :: NullOrUndefined (AuthEventsType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListUserAuthEventsResponse :: Newtype AdminListUserAuthEventsResponse _


newtype AdminRemoveUserFromGroupRequest = AdminRemoveUserFromGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "GroupName" :: (GroupNameType)
  }
derive instance newtypeAdminRemoveUserFromGroupRequest :: Newtype AdminRemoveUserFromGroupRequest _


-- | <p>Represents the request to reset a user's password as an administrator.</p>
newtype AdminResetUserPasswordRequest = AdminResetUserPasswordRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminResetUserPasswordRequest :: Newtype AdminResetUserPasswordRequest _


-- | <p>Represents the response from the server to reset a user password as an administrator.</p>
newtype AdminResetUserPasswordResponse = AdminResetUserPasswordResponse 
  { 
  }
derive instance newtypeAdminResetUserPasswordResponse :: Newtype AdminResetUserPasswordResponse _


-- | <p>The request to respond to the authentication challenge, as an administrator.</p>
newtype AdminRespondToAuthChallengeRequest = AdminRespondToAuthChallengeRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "ChallengeName" :: (ChallengeNameType)
  , "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType)
  , "Session" :: NullOrUndefined (SessionType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "ContextData" :: NullOrUndefined (ContextDataType)
  }
derive instance newtypeAdminRespondToAuthChallengeRequest :: Newtype AdminRespondToAuthChallengeRequest _


-- | <p>Responds to the authentication challenge, as an administrator.</p>
newtype AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse 
  { "ChallengeName" :: NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeAdminRespondToAuthChallengeResponse :: Newtype AdminRespondToAuthChallengeResponse _


newtype AdminSetUserMFAPreferenceRequest = AdminSetUserMFAPreferenceRequest 
  { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType)
  , "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType)
  , "Username" :: (UsernameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeAdminSetUserMFAPreferenceRequest :: Newtype AdminSetUserMFAPreferenceRequest _


newtype AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse 
  { 
  }
derive instance newtypeAdminSetUserMFAPreferenceResponse :: Newtype AdminSetUserMFAPreferenceResponse _


-- | <p>Represents the request to set user settings as an administrator.</p>
newtype AdminSetUserSettingsRequest = AdminSetUserSettingsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "MFAOptions" :: (MFAOptionListType)
  }
derive instance newtypeAdminSetUserSettingsRequest :: Newtype AdminSetUserSettingsRequest _


-- | <p>Represents the response from the server to set user settings as an administrator.</p>
newtype AdminSetUserSettingsResponse = AdminSetUserSettingsResponse 
  { 
  }
derive instance newtypeAdminSetUserSettingsResponse :: Newtype AdminSetUserSettingsResponse _


newtype AdminUpdateAuthEventFeedbackRequest = AdminUpdateAuthEventFeedbackRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "EventId" :: (EventIdType)
  , "FeedbackValue" :: (FeedbackValueType)
  }
derive instance newtypeAdminUpdateAuthEventFeedbackRequest :: Newtype AdminUpdateAuthEventFeedbackRequest _


newtype AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse 
  { 
  }
derive instance newtypeAdminUpdateAuthEventFeedbackResponse :: Newtype AdminUpdateAuthEventFeedbackResponse _


-- | <p>The request to update the device status, as an administrator.</p>
newtype AdminUpdateDeviceStatusRequest = AdminUpdateDeviceStatusRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType)
  }
derive instance newtypeAdminUpdateDeviceStatusRequest :: Newtype AdminUpdateDeviceStatusRequest _


-- | <p>The status response from the request to update the device, as an administrator.</p>
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse 
  { 
  }
derive instance newtypeAdminUpdateDeviceStatusResponse :: Newtype AdminUpdateDeviceStatusResponse _


-- | <p>Represents the request to update the user's attributes as an administrator.</p>
newtype AdminUpdateUserAttributesRequest = AdminUpdateUserAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributes" :: (AttributeListType)
  }
derive instance newtypeAdminUpdateUserAttributesRequest :: Newtype AdminUpdateUserAttributesRequest _


-- | <p>Represents the response from the server for the request to update user attributes as an administrator.</p>
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse 
  { 
  }
derive instance newtypeAdminUpdateUserAttributesResponse :: Newtype AdminUpdateUserAttributesResponse _


-- | <p>The request to sign out of all devices, as an administrator.</p>
newtype AdminUserGlobalSignOutRequest = AdminUserGlobalSignOutRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminUserGlobalSignOutRequest :: Newtype AdminUserGlobalSignOutRequest _


-- | <p>The global sign-out response, as an administrator.</p>
newtype AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse 
  { 
  }
derive instance newtypeAdminUserGlobalSignOutResponse :: Newtype AdminUserGlobalSignOutResponse _


newtype AdvancedSecurityModeType = AdvancedSecurityModeType String
derive instance newtypeAdvancedSecurityModeType :: Newtype AdvancedSecurityModeType _


newtype AliasAttributeType = AliasAttributeType String
derive instance newtypeAliasAttributeType :: Newtype AliasAttributeType _


newtype AliasAttributesListType = AliasAttributesListType (Array AliasAttributeType)
derive instance newtypeAliasAttributesListType :: Newtype AliasAttributesListType _


-- | <p>This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.</p>
newtype AliasExistsException = AliasExistsException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeAliasExistsException :: Newtype AliasExistsException _


-- | <p>The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.</p>
newtype AnalyticsConfigurationType = AnalyticsConfigurationType 
  { "ApplicationId" :: (HexStringType)
  , "RoleArn" :: (ArnType)
  , "ExternalId" :: (StringType)
  , "UserDataShared" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeAnalyticsConfigurationType :: Newtype AnalyticsConfigurationType _


-- | <p>An Amazon Pinpoint analytics endpoint.</p> <p>An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.</p>
newtype AnalyticsMetadataType = AnalyticsMetadataType 
  { "AnalyticsEndpointId" :: NullOrUndefined (StringType)
  }
derive instance newtypeAnalyticsMetadataType :: Newtype AnalyticsMetadataType _


newtype ArnType = ArnType String
derive instance newtypeArnType :: Newtype ArnType _


newtype AssociateSoftwareTokenRequest = AssociateSoftwareTokenRequest 
  { "AccessToken" :: NullOrUndefined (TokenModelType)
  , "Session" :: NullOrUndefined (SessionType)
  }
derive instance newtypeAssociateSoftwareTokenRequest :: Newtype AssociateSoftwareTokenRequest _


newtype AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse 
  { "SecretCode" :: NullOrUndefined (SecretCodeType)
  , "Session" :: NullOrUndefined (SessionType)
  }
derive instance newtypeAssociateSoftwareTokenResponse :: Newtype AssociateSoftwareTokenResponse _


newtype AttributeDataType = AttributeDataType String
derive instance newtypeAttributeDataType :: Newtype AttributeDataType _


newtype AttributeListType = AttributeListType (Array AttributeType)
derive instance newtypeAttributeListType :: Newtype AttributeListType _


newtype AttributeMappingKeyType = AttributeMappingKeyType String
derive instance newtypeAttributeMappingKeyType :: Newtype AttributeMappingKeyType _


newtype AttributeMappingType = AttributeMappingType (Map AttributeMappingKeyType StringType)
derive instance newtypeAttributeMappingType :: Newtype AttributeMappingType _


newtype AttributeNameListType = AttributeNameListType (Array AttributeNameType)
derive instance newtypeAttributeNameListType :: Newtype AttributeNameListType _


newtype AttributeNameType = AttributeNameType String
derive instance newtypeAttributeNameType :: Newtype AttributeNameType _


-- | <p>Specifies whether the attribute is standard or custom.</p>
newtype AttributeType = AttributeType 
  { "Name" :: (AttributeNameType)
  , "Value" :: NullOrUndefined (AttributeValueType)
  }
derive instance newtypeAttributeType :: Newtype AttributeType _


newtype AttributeValueType = AttributeValueType String
derive instance newtypeAttributeValueType :: Newtype AttributeValueType _


-- | <p>The authentication event type.</p>
newtype AuthEventType = AuthEventType 
  { "EventId" :: NullOrUndefined (StringType)
  , "EventType" :: NullOrUndefined (EventType)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "EventResponse" :: NullOrUndefined (EventResponseType)
  , "EventRisk" :: NullOrUndefined (EventRiskType)
  , "ChallengeResponses" :: NullOrUndefined (ChallengeResponseListType)
  , "EventContextData" :: NullOrUndefined (EventContextDataType)
  , "EventFeedback" :: NullOrUndefined (EventFeedbackType)
  }
derive instance newtypeAuthEventType :: Newtype AuthEventType _


newtype AuthEventsType = AuthEventsType (Array AuthEventType)
derive instance newtypeAuthEventsType :: Newtype AuthEventsType _


newtype AuthFlowType = AuthFlowType String
derive instance newtypeAuthFlowType :: Newtype AuthFlowType _


newtype AuthParametersType = AuthParametersType (Map StringType StringType)
derive instance newtypeAuthParametersType :: Newtype AuthParametersType _


-- | <p>The authentication result.</p>
newtype AuthenticationResultType = AuthenticationResultType 
  { "AccessToken" :: NullOrUndefined (TokenModelType)
  , "ExpiresIn" :: NullOrUndefined (IntegerType)
  , "TokenType" :: NullOrUndefined (StringType)
  , "RefreshToken" :: NullOrUndefined (TokenModelType)
  , "IdToken" :: NullOrUndefined (TokenModelType)
  , "NewDeviceMetadata" :: NullOrUndefined (NewDeviceMetadataType)
  }
derive instance newtypeAuthenticationResultType :: Newtype AuthenticationResultType _


newtype BlockedIPRangeListType = BlockedIPRangeListType (Array StringType)
derive instance newtypeBlockedIPRangeListType :: Newtype BlockedIPRangeListType _


newtype BooleanType = BooleanType Boolean
derive instance newtypeBooleanType :: Newtype BooleanType _


newtype CSSType = CSSType String
derive instance newtypeCSSType :: Newtype CSSType _


newtype CSSVersionType = CSSVersionType String
derive instance newtypeCSSVersionType :: Newtype CSSVersionType _


newtype CallbackURLsListType = CallbackURLsListType (Array RedirectUrlType)
derive instance newtypeCallbackURLsListType :: Newtype CallbackURLsListType _


newtype ChallengeName = ChallengeName String
derive instance newtypeChallengeName :: Newtype ChallengeName _


newtype ChallengeNameType = ChallengeNameType String
derive instance newtypeChallengeNameType :: Newtype ChallengeNameType _


newtype ChallengeParametersType = ChallengeParametersType (Map StringType StringType)
derive instance newtypeChallengeParametersType :: Newtype ChallengeParametersType _


newtype ChallengeResponse = ChallengeResponse String
derive instance newtypeChallengeResponse :: Newtype ChallengeResponse _


newtype ChallengeResponseListType = ChallengeResponseListType (Array ChallengeResponseType)
derive instance newtypeChallengeResponseListType :: Newtype ChallengeResponseListType _


-- | <p>The challenge response type.</p>
newtype ChallengeResponseType = ChallengeResponseType 
  { "ChallengeName" :: NullOrUndefined (ChallengeName)
  , "ChallengeResponse" :: NullOrUndefined (ChallengeResponse)
  }
derive instance newtypeChallengeResponseType :: Newtype ChallengeResponseType _


newtype ChallengeResponsesType = ChallengeResponsesType (Map StringType StringType)
derive instance newtypeChallengeResponsesType :: Newtype ChallengeResponsesType _


-- | <p>Represents the request to change a user password.</p>
newtype ChangePasswordRequest = ChangePasswordRequest 
  { "PreviousPassword" :: (PasswordType)
  , "ProposedPassword" :: (PasswordType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeChangePasswordRequest :: Newtype ChangePasswordRequest _


-- | <p>The response from the server to the change password request.</p>
newtype ChangePasswordResponse = ChangePasswordResponse 
  { 
  }
derive instance newtypeChangePasswordResponse :: Newtype ChangePasswordResponse _


newtype ClientIdType = ClientIdType String
derive instance newtypeClientIdType :: Newtype ClientIdType _


newtype ClientMetadataType = ClientMetadataType (Map StringType StringType)
derive instance newtypeClientMetadataType :: Newtype ClientMetadataType _


newtype ClientNameType = ClientNameType String
derive instance newtypeClientNameType :: Newtype ClientNameType _


newtype ClientPermissionListType = ClientPermissionListType (Array ClientPermissionType)
derive instance newtypeClientPermissionListType :: Newtype ClientPermissionListType _


newtype ClientPermissionType = ClientPermissionType String
derive instance newtypeClientPermissionType :: Newtype ClientPermissionType _


newtype ClientSecretType = ClientSecretType String
derive instance newtypeClientSecretType :: Newtype ClientSecretType _


newtype CodeDeliveryDetailsListType = CodeDeliveryDetailsListType (Array CodeDeliveryDetailsType)
derive instance newtypeCodeDeliveryDetailsListType :: Newtype CodeDeliveryDetailsListType _


-- | <p>The code delivery details being returned from the server.</p>
newtype CodeDeliveryDetailsType = CodeDeliveryDetailsType 
  { "Destination" :: NullOrUndefined (StringType)
  , "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType)
  , "AttributeName" :: NullOrUndefined (AttributeNameType)
  }
derive instance newtypeCodeDeliveryDetailsType :: Newtype CodeDeliveryDetailsType _


-- | <p>This exception is thrown when a verification code fails to deliver successfully.</p>
newtype CodeDeliveryFailureException = CodeDeliveryFailureException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeCodeDeliveryFailureException :: Newtype CodeDeliveryFailureException _


-- | <p>This exception is thrown if the provided code does not match what the server was expecting.</p>
newtype CodeMismatchException = CodeMismatchException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeCodeMismatchException :: Newtype CodeMismatchException _


newtype CompletionMessageType = CompletionMessageType String
derive instance newtypeCompletionMessageType :: Newtype CompletionMessageType _


-- | <p>The compromised credentials actions type</p>
newtype CompromisedCredentialsActionsType = CompromisedCredentialsActionsType 
  { "EventAction" :: (CompromisedCredentialsEventActionType)
  }
derive instance newtypeCompromisedCredentialsActionsType :: Newtype CompromisedCredentialsActionsType _


newtype CompromisedCredentialsEventActionType = CompromisedCredentialsEventActionType String
derive instance newtypeCompromisedCredentialsEventActionType :: Newtype CompromisedCredentialsEventActionType _


-- | <p>The compromised credentials risk configuration type.</p>
newtype CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType 
  { "EventFilter" :: NullOrUndefined (EventFiltersType)
  , "Actions" :: (CompromisedCredentialsActionsType)
  }
derive instance newtypeCompromisedCredentialsRiskConfigurationType :: Newtype CompromisedCredentialsRiskConfigurationType _


-- | <p>This exception is thrown if two or more modifications are happening concurrently.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>Confirms the device request.</p>
newtype ConfirmDeviceRequest = ConfirmDeviceRequest 
  { "AccessToken" :: (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceSecretVerifierConfig" :: NullOrUndefined (DeviceSecretVerifierConfigType)
  , "DeviceName" :: NullOrUndefined (DeviceNameType)
  }
derive instance newtypeConfirmDeviceRequest :: Newtype ConfirmDeviceRequest _


-- | <p>Confirms the device response.</p>
newtype ConfirmDeviceResponse = ConfirmDeviceResponse 
  { "UserConfirmationNecessary" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeConfirmDeviceResponse :: Newtype ConfirmDeviceResponse _


-- | <p>The request representing the confirmation for a password reset.</p>
newtype ConfirmForgotPasswordRequest = ConfirmForgotPasswordRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "ConfirmationCode" :: (ConfirmationCodeType)
  , "Password" :: (PasswordType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  }
derive instance newtypeConfirmForgotPasswordRequest :: Newtype ConfirmForgotPasswordRequest _


-- | <p>The response from the server that results from a user's request to retrieve a forgotten password.</p>
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse 
  { 
  }
derive instance newtypeConfirmForgotPasswordResponse :: Newtype ConfirmForgotPasswordResponse _


-- | <p>Represents the request to confirm registration of a user.</p>
newtype ConfirmSignUpRequest = ConfirmSignUpRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "ConfirmationCode" :: (ConfirmationCodeType)
  , "ForceAliasCreation" :: NullOrUndefined (ForceAliasCreation)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  }
derive instance newtypeConfirmSignUpRequest :: Newtype ConfirmSignUpRequest _


-- | <p>Represents the response from the server for the registration confirmation.</p>
newtype ConfirmSignUpResponse = ConfirmSignUpResponse 
  { 
  }
derive instance newtypeConfirmSignUpResponse :: Newtype ConfirmSignUpResponse _


newtype ConfirmationCodeType = ConfirmationCodeType String
derive instance newtypeConfirmationCodeType :: Newtype ConfirmationCodeType _


-- | <p>Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>
newtype ContextDataType = ContextDataType 
  { "IpAddress" :: (StringType)
  , "ServerName" :: (StringType)
  , "ServerPath" :: (StringType)
  , "HttpHeaders" :: (HttpHeaderList)
  , "EncodedData" :: NullOrUndefined (StringType)
  }
derive instance newtypeContextDataType :: Newtype ContextDataType _


newtype CreateGroupRequest = CreateGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Description" :: NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined (PrecedenceType)
  }
derive instance newtypeCreateGroupRequest :: Newtype CreateGroupRequest _


newtype CreateGroupResponse = CreateGroupResponse 
  { "Group" :: NullOrUndefined (GroupType)
  }
derive instance newtypeCreateGroupResponse :: Newtype CreateGroupResponse _


newtype CreateIdentityProviderRequest = CreateIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameTypeV1)
  , "ProviderType" :: (IdentityProviderTypeType)
  , "ProviderDetails" :: (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType)
  }
derive instance newtypeCreateIdentityProviderRequest :: Newtype CreateIdentityProviderRequest _


newtype CreateIdentityProviderResponse = CreateIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeCreateIdentityProviderResponse :: Newtype CreateIdentityProviderResponse _


newtype CreateResourceServerRequest = CreateResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  , "Name" :: (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeCreateResourceServerRequest :: Newtype CreateResourceServerRequest _


newtype CreateResourceServerResponse = CreateResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeCreateResourceServerResponse :: Newtype CreateResourceServerResponse _


-- | <p>Represents the request to create the user import job.</p>
newtype CreateUserImportJobRequest = CreateUserImportJobRequest 
  { "JobName" :: (UserImportJobNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "CloudWatchLogsRoleArn" :: (ArnType)
  }
derive instance newtypeCreateUserImportJobRequest :: Newtype CreateUserImportJobRequest _


-- | <p>Represents the response from the server to the request to create the user import job.</p>
newtype CreateUserImportJobResponse = CreateUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined (UserImportJobType)
  }
derive instance newtypeCreateUserImportJobResponse :: Newtype CreateUserImportJobResponse _


-- | <p>Represents the request to create a user pool client.</p>
newtype CreateUserPoolClientRequest = CreateUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientName" :: (ClientNameType)
  , "GenerateSecret" :: NullOrUndefined (GenerateSecret)
  , "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeCreateUserPoolClientRequest :: Newtype CreateUserPoolClientRequest _


-- | <p>Represents the response from the server to create a user pool client.</p>
newtype CreateUserPoolClientResponse = CreateUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeCreateUserPoolClientResponse :: Newtype CreateUserPoolClientResponse _


newtype CreateUserPoolDomainRequest = CreateUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeCreateUserPoolDomainRequest :: Newtype CreateUserPoolDomainRequest _


newtype CreateUserPoolDomainResponse = CreateUserPoolDomainResponse 
  { 
  }
derive instance newtypeCreateUserPoolDomainResponse :: Newtype CreateUserPoolDomainResponse _


-- | <p>Represents the request to create a user pool.</p>
newtype CreateUserPoolRequest = CreateUserPoolRequest 
  { "PoolName" :: (UserPoolNameType)
  , "Policies" :: NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined (LambdaConfigType)
  , "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType)
  , "AliasAttributes" :: NullOrUndefined (AliasAttributesListType)
  , "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType)
  , "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined (UserPoolTagsType)
  , "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType)
  , "Schema" :: NullOrUndefined (SchemaAttributesListType)
  , "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeCreateUserPoolRequest :: Newtype CreateUserPoolRequest _


-- | <p>Represents the response from the server for the request to create a user pool.</p>
newtype CreateUserPoolResponse = CreateUserPoolResponse 
  { "UserPool" :: NullOrUndefined (UserPoolType)
  }
derive instance newtypeCreateUserPoolResponse :: Newtype CreateUserPoolResponse _


newtype CustomAttributeNameType = CustomAttributeNameType String
derive instance newtypeCustomAttributeNameType :: Newtype CustomAttributeNameType _


newtype CustomAttributesListType = CustomAttributesListType (Array SchemaAttributeType)
derive instance newtypeCustomAttributesListType :: Newtype CustomAttributesListType _


newtype DateType = DateType Number
derive instance newtypeDateType :: Newtype DateType _


newtype DefaultEmailOptionType = DefaultEmailOptionType String
derive instance newtypeDefaultEmailOptionType :: Newtype DefaultEmailOptionType _


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteGroupRequest :: Newtype DeleteGroupRequest _


newtype DeleteIdentityProviderRequest = DeleteIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  }
derive instance newtypeDeleteIdentityProviderRequest :: Newtype DeleteIdentityProviderRequest _


newtype DeleteResourceServerRequest = DeleteResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  }
derive instance newtypeDeleteResourceServerRequest :: Newtype DeleteResourceServerRequest _


-- | <p>Represents the request to delete user attributes.</p>
newtype DeleteUserAttributesRequest = DeleteUserAttributesRequest 
  { "UserAttributeNames" :: (AttributeNameListType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeDeleteUserAttributesRequest :: Newtype DeleteUserAttributesRequest _


-- | <p>Represents the response from the server to delete user attributes.</p>
newtype DeleteUserAttributesResponse = DeleteUserAttributesResponse 
  { 
  }
derive instance newtypeDeleteUserAttributesResponse :: Newtype DeleteUserAttributesResponse _


-- | <p>Represents the request to delete a user pool client.</p>
newtype DeleteUserPoolClientRequest = DeleteUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  }
derive instance newtypeDeleteUserPoolClientRequest :: Newtype DeleteUserPoolClientRequest _


newtype DeleteUserPoolDomainRequest = DeleteUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteUserPoolDomainRequest :: Newtype DeleteUserPoolDomainRequest _


newtype DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse 
  { 
  }
derive instance newtypeDeleteUserPoolDomainResponse :: Newtype DeleteUserPoolDomainResponse _


-- | <p>Represents the request to delete a user pool.</p>
newtype DeleteUserPoolRequest = DeleteUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteUserPoolRequest :: Newtype DeleteUserPoolRequest _


-- | <p>Represents the request to delete a user.</p>
newtype DeleteUserRequest = DeleteUserRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _


newtype DeliveryMediumListType = DeliveryMediumListType (Array DeliveryMediumType)
derive instance newtypeDeliveryMediumListType :: Newtype DeliveryMediumListType _


newtype DeliveryMediumType = DeliveryMediumType String
derive instance newtypeDeliveryMediumType :: Newtype DeliveryMediumType _


newtype DescribeIdentityProviderRequest = DescribeIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  }
derive instance newtypeDescribeIdentityProviderRequest :: Newtype DescribeIdentityProviderRequest _


newtype DescribeIdentityProviderResponse = DescribeIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeDescribeIdentityProviderResponse :: Newtype DescribeIdentityProviderResponse _


newtype DescribeResourceServerRequest = DescribeResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  }
derive instance newtypeDescribeResourceServerRequest :: Newtype DescribeResourceServerRequest _


newtype DescribeResourceServerResponse = DescribeResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeDescribeResourceServerResponse :: Newtype DescribeResourceServerResponse _


newtype DescribeRiskConfigurationRequest = DescribeRiskConfigurationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  }
derive instance newtypeDescribeRiskConfigurationRequest :: Newtype DescribeRiskConfigurationRequest _


newtype DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse 
  { "RiskConfiguration" :: (RiskConfigurationType)
  }
derive instance newtypeDescribeRiskConfigurationResponse :: Newtype DescribeRiskConfigurationResponse _


-- | <p>Represents the request to describe the user import job.</p>
newtype DescribeUserImportJobRequest = DescribeUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeDescribeUserImportJobRequest :: Newtype DescribeUserImportJobRequest _


-- | <p>Represents the response from the server to the request to describe the user import job.</p>
newtype DescribeUserImportJobResponse = DescribeUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined (UserImportJobType)
  }
derive instance newtypeDescribeUserImportJobResponse :: Newtype DescribeUserImportJobResponse _


-- | <p>Represents the request to describe a user pool client.</p>
newtype DescribeUserPoolClientRequest = DescribeUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  }
derive instance newtypeDescribeUserPoolClientRequest :: Newtype DescribeUserPoolClientRequest _


-- | <p>Represents the response from the server from a request to describe the user pool client.</p>
newtype DescribeUserPoolClientResponse = DescribeUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeDescribeUserPoolClientResponse :: Newtype DescribeUserPoolClientResponse _


newtype DescribeUserPoolDomainRequest = DescribeUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  }
derive instance newtypeDescribeUserPoolDomainRequest :: Newtype DescribeUserPoolDomainRequest _


newtype DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse 
  { "DomainDescription" :: NullOrUndefined (DomainDescriptionType)
  }
derive instance newtypeDescribeUserPoolDomainResponse :: Newtype DescribeUserPoolDomainResponse _


-- | <p>Represents the request to describe the user pool.</p>
newtype DescribeUserPoolRequest = DescribeUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDescribeUserPoolRequest :: Newtype DescribeUserPoolRequest _


-- | <p>Represents the response to describe the user pool.</p>
newtype DescribeUserPoolResponse = DescribeUserPoolResponse 
  { "UserPool" :: NullOrUndefined (UserPoolType)
  }
derive instance newtypeDescribeUserPoolResponse :: Newtype DescribeUserPoolResponse _


newtype DescriptionType = DescriptionType String
derive instance newtypeDescriptionType :: Newtype DescriptionType _


-- | <p>The configuration for the user pool's device tracking.</p>
newtype DeviceConfigurationType = DeviceConfigurationType 
  { "ChallengeRequiredOnNewDevice" :: NullOrUndefined (BooleanType)
  , "DeviceOnlyRememberedOnUserPrompt" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeDeviceConfigurationType :: Newtype DeviceConfigurationType _


newtype DeviceKeyType = DeviceKeyType String
derive instance newtypeDeviceKeyType :: Newtype DeviceKeyType _


newtype DeviceListType = DeviceListType (Array DeviceType)
derive instance newtypeDeviceListType :: Newtype DeviceListType _


newtype DeviceNameType = DeviceNameType String
derive instance newtypeDeviceNameType :: Newtype DeviceNameType _


newtype DeviceRememberedStatusType = DeviceRememberedStatusType String
derive instance newtypeDeviceRememberedStatusType :: Newtype DeviceRememberedStatusType _


-- | <p>The device verifier against which it will be authenticated.</p>
newtype DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType 
  { "PasswordVerifier" :: NullOrUndefined (StringType)
  , "Salt" :: NullOrUndefined (StringType)
  }
derive instance newtypeDeviceSecretVerifierConfigType :: Newtype DeviceSecretVerifierConfigType _


-- | <p>The device type.</p>
newtype DeviceType = DeviceType 
  { "DeviceKey" :: NullOrUndefined (DeviceKeyType)
  , "DeviceAttributes" :: NullOrUndefined (AttributeListType)
  , "DeviceCreateDate" :: NullOrUndefined (DateType)
  , "DeviceLastModifiedDate" :: NullOrUndefined (DateType)
  , "DeviceLastAuthenticatedDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeDeviceType :: Newtype DeviceType _


-- | <p>A container for information about a domain.</p>
newtype DomainDescriptionType = DomainDescriptionType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "AWSAccountId" :: NullOrUndefined (AWSAccountIdType)
  , "Domain" :: NullOrUndefined (DomainType)
  , "S3Bucket" :: NullOrUndefined (S3BucketType)
  , "CloudFrontDistribution" :: NullOrUndefined (ArnType)
  , "Version" :: NullOrUndefined (DomainVersionType)
  , "Status" :: NullOrUndefined (DomainStatusType)
  }
derive instance newtypeDomainDescriptionType :: Newtype DomainDescriptionType _


newtype DomainStatusType = DomainStatusType String
derive instance newtypeDomainStatusType :: Newtype DomainStatusType _


newtype DomainType = DomainType String
derive instance newtypeDomainType :: Newtype DomainType _


newtype DomainVersionType = DomainVersionType String
derive instance newtypeDomainVersionType :: Newtype DomainVersionType _


-- | <p>This exception is thrown when the provider is already supported by the user pool.</p>
newtype DuplicateProviderException = DuplicateProviderException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeDuplicateProviderException :: Newtype DuplicateProviderException _


newtype EmailAddressType = EmailAddressType String
derive instance newtypeEmailAddressType :: Newtype EmailAddressType _


-- | <p>The email configuration type.</p>
newtype EmailConfigurationType = EmailConfigurationType 
  { "SourceArn" :: NullOrUndefined (ArnType)
  , "ReplyToEmailAddress" :: NullOrUndefined (EmailAddressType)
  }
derive instance newtypeEmailConfigurationType :: Newtype EmailConfigurationType _


newtype EmailNotificationBodyType = EmailNotificationBodyType String
derive instance newtypeEmailNotificationBodyType :: Newtype EmailNotificationBodyType _


newtype EmailNotificationSubjectType = EmailNotificationSubjectType String
derive instance newtypeEmailNotificationSubjectType :: Newtype EmailNotificationSubjectType _


newtype EmailVerificationMessageByLinkType = EmailVerificationMessageByLinkType String
derive instance newtypeEmailVerificationMessageByLinkType :: Newtype EmailVerificationMessageByLinkType _


newtype EmailVerificationMessageType = EmailVerificationMessageType String
derive instance newtypeEmailVerificationMessageType :: Newtype EmailVerificationMessageType _


newtype EmailVerificationSubjectByLinkType = EmailVerificationSubjectByLinkType String
derive instance newtypeEmailVerificationSubjectByLinkType :: Newtype EmailVerificationSubjectByLinkType _


newtype EmailVerificationSubjectType = EmailVerificationSubjectType String
derive instance newtypeEmailVerificationSubjectType :: Newtype EmailVerificationSubjectType _


-- | <p>This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).</p>
newtype EnableSoftwareTokenMFAException = EnableSoftwareTokenMFAException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeEnableSoftwareTokenMFAException :: Newtype EnableSoftwareTokenMFAException _


-- | <p>Specifies the user context data captured at the time of an event request.</p>
newtype EventContextDataType = EventContextDataType 
  { "IpAddress" :: NullOrUndefined (StringType)
  , "DeviceName" :: NullOrUndefined (StringType)
  , "Timezone" :: NullOrUndefined (StringType)
  , "City" :: NullOrUndefined (StringType)
  , "Country" :: NullOrUndefined (StringType)
  }
derive instance newtypeEventContextDataType :: Newtype EventContextDataType _


-- | <p>Specifies the event feedback type.</p>
newtype EventFeedbackType = EventFeedbackType 
  { "FeedbackValue" :: (FeedbackValueType)
  , "Provider" :: (StringType)
  , "FeedbackDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeEventFeedbackType :: Newtype EventFeedbackType _


newtype EventFilterType = EventFilterType String
derive instance newtypeEventFilterType :: Newtype EventFilterType _


newtype EventFiltersType = EventFiltersType (Array EventFilterType)
derive instance newtypeEventFiltersType :: Newtype EventFiltersType _


newtype EventIdType = EventIdType String
derive instance newtypeEventIdType :: Newtype EventIdType _


newtype EventResponseType = EventResponseType String
derive instance newtypeEventResponseType :: Newtype EventResponseType _


-- | <p>The event risk type.</p>
newtype EventRiskType = EventRiskType 
  { "RiskDecision" :: NullOrUndefined (RiskDecisionType)
  , "RiskLevel" :: NullOrUndefined (RiskLevelType)
  }
derive instance newtypeEventRiskType :: Newtype EventRiskType _


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _


-- | <p>This exception is thrown if a code has expired.</p>
newtype ExpiredCodeException = ExpiredCodeException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeExpiredCodeException :: Newtype ExpiredCodeException _


newtype ExplicitAuthFlowsListType = ExplicitAuthFlowsListType (Array ExplicitAuthFlowsType)
derive instance newtypeExplicitAuthFlowsListType :: Newtype ExplicitAuthFlowsListType _


newtype ExplicitAuthFlowsType = ExplicitAuthFlowsType String
derive instance newtypeExplicitAuthFlowsType :: Newtype ExplicitAuthFlowsType _


newtype FeedbackValueType = FeedbackValueType String
derive instance newtypeFeedbackValueType :: Newtype FeedbackValueType _


newtype ForceAliasCreation = ForceAliasCreation Boolean
derive instance newtypeForceAliasCreation :: Newtype ForceAliasCreation _


-- | <p>Represents the request to forget the device.</p>
newtype ForgetDeviceRequest = ForgetDeviceRequest 
  { "AccessToken" :: NullOrUndefined (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  }
derive instance newtypeForgetDeviceRequest :: Newtype ForgetDeviceRequest _


-- | <p>Represents the request to reset a user's password.</p>
newtype ForgotPasswordRequest = ForgotPasswordRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined (SecretHashType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  , "Username" :: (UsernameType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  }
derive instance newtypeForgotPasswordRequest :: Newtype ForgotPasswordRequest _


-- | <p>Respresents the response from the server regarding the request to reset a password.</p>
newtype ForgotPasswordResponse = ForgotPasswordResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeForgotPasswordResponse :: Newtype ForgotPasswordResponse _


newtype GenerateSecret = GenerateSecret Boolean
derive instance newtypeGenerateSecret :: Newtype GenerateSecret _


-- | <p>Represents the request to get the header information for the .csv file for the user import job.</p>
newtype GetCSVHeaderRequest = GetCSVHeaderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetCSVHeaderRequest :: Newtype GetCSVHeaderRequest _


-- | <p>Represents the response from the server to the request to get the header information for the .csv file for the user import job.</p>
newtype GetCSVHeaderResponse = GetCSVHeaderResponse 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "CSVHeader" :: NullOrUndefined (ListOfStringTypes)
  }
derive instance newtypeGetCSVHeaderResponse :: Newtype GetCSVHeaderResponse _


-- | <p>Represents the request to get the device.</p>
newtype GetDeviceRequest = GetDeviceRequest 
  { "DeviceKey" :: (DeviceKeyType)
  , "AccessToken" :: NullOrUndefined (TokenModelType)
  }
derive instance newtypeGetDeviceRequest :: Newtype GetDeviceRequest _


-- | <p>Gets the device response.</p>
newtype GetDeviceResponse = GetDeviceResponse 
  { "Device" :: (DeviceType)
  }
derive instance newtypeGetDeviceResponse :: Newtype GetDeviceResponse _


newtype GetGroupRequest = GetGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetGroupRequest :: Newtype GetGroupRequest _


newtype GetGroupResponse = GetGroupResponse 
  { "Group" :: NullOrUndefined (GroupType)
  }
derive instance newtypeGetGroupResponse :: Newtype GetGroupResponse _


newtype GetIdentityProviderByIdentifierRequest = GetIdentityProviderByIdentifierRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "IdpIdentifier" :: (IdpIdentifierType)
  }
derive instance newtypeGetIdentityProviderByIdentifierRequest :: Newtype GetIdentityProviderByIdentifierRequest _


newtype GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeGetIdentityProviderByIdentifierResponse :: Newtype GetIdentityProviderByIdentifierResponse _


-- | <p>Request to get a signing certificate from Cognito.</p>
newtype GetSigningCertificateRequest = GetSigningCertificateRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetSigningCertificateRequest :: Newtype GetSigningCertificateRequest _


-- | <p>Response from Cognito for a signing certificate request.</p>
newtype GetSigningCertificateResponse = GetSigningCertificateResponse 
  { "Certificate" :: NullOrUndefined (StringType)
  }
derive instance newtypeGetSigningCertificateResponse :: Newtype GetSigningCertificateResponse _


newtype GetUICustomizationRequest = GetUICustomizationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  }
derive instance newtypeGetUICustomizationRequest :: Newtype GetUICustomizationRequest _


newtype GetUICustomizationResponse = GetUICustomizationResponse 
  { "UICustomization" :: (UICustomizationType)
  }
derive instance newtypeGetUICustomizationResponse :: Newtype GetUICustomizationResponse _


-- | <p>Represents the request to get user attribute verification.</p>
newtype GetUserAttributeVerificationCodeRequest = GetUserAttributeVerificationCodeRequest 
  { "AccessToken" :: (TokenModelType)
  , "AttributeName" :: (AttributeNameType)
  }
derive instance newtypeGetUserAttributeVerificationCodeRequest :: Newtype GetUserAttributeVerificationCodeRequest _


-- | <p>The verification code response returned by the server response to get the user attribute verification code.</p>
newtype GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeGetUserAttributeVerificationCodeResponse :: Newtype GetUserAttributeVerificationCodeResponse _


newtype GetUserPoolMfaConfigRequest = GetUserPoolMfaConfigRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetUserPoolMfaConfigRequest :: Newtype GetUserPoolMfaConfigRequest _


newtype GetUserPoolMfaConfigResponse = GetUserPoolMfaConfigResponse 
  { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeGetUserPoolMfaConfigResponse :: Newtype GetUserPoolMfaConfigResponse _


-- | <p>Represents the request to get information about the user.</p>
newtype GetUserRequest = GetUserRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeGetUserRequest :: Newtype GetUserRequest _


-- | <p>Represents the response from the server from the request to get information about the user.</p>
newtype GetUserResponse = GetUserResponse 
  { "Username" :: (UsernameType)
  , "UserAttributes" :: (AttributeListType)
  , "MFAOptions" :: NullOrUndefined (MFAOptionListType)
  , "PreferredMfaSetting" :: NullOrUndefined (StringType)
  , "UserMFASettingList" :: NullOrUndefined (UserMFASettingListType)
  }
derive instance newtypeGetUserResponse :: Newtype GetUserResponse _


-- | <p>Represents the request to sign out all devices.</p>
newtype GlobalSignOutRequest = GlobalSignOutRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeGlobalSignOutRequest :: Newtype GlobalSignOutRequest _


-- | <p>The response to the request to sign out all devices.</p>
newtype GlobalSignOutResponse = GlobalSignOutResponse 
  { 
  }
derive instance newtypeGlobalSignOutResponse :: Newtype GlobalSignOutResponse _


-- | <p>This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.</p>
newtype GroupExistsException = GroupExistsException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeGroupExistsException :: Newtype GroupExistsException _


newtype GroupListType = GroupListType (Array GroupType)
derive instance newtypeGroupListType :: Newtype GroupListType _


newtype GroupNameType = GroupNameType String
derive instance newtypeGroupNameType :: Newtype GroupNameType _


-- | <p>The group type.</p>
newtype GroupType = GroupType 
  { "GroupName" :: NullOrUndefined (GroupNameType)
  , "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "Description" :: NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined (PrecedenceType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeGroupType :: Newtype GroupType _


newtype HexStringType = HexStringType String
derive instance newtypeHexStringType :: Newtype HexStringType _


-- | <p>The HTTP header.</p>
newtype HttpHeader = HttpHeader 
  { "HeaderName'" :: NullOrUndefined (StringType)
  , "HeaderValue'" :: NullOrUndefined (StringType)
  }
derive instance newtypeHttpHeader :: Newtype HttpHeader _


newtype HttpHeaderList = HttpHeaderList (Array HttpHeader)
derive instance newtypeHttpHeaderList :: Newtype HttpHeaderList _


-- | <p>A container for information about an identity provider.</p>
newtype IdentityProviderType = IdentityProviderType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "ProviderName" :: NullOrUndefined (ProviderNameType)
  , "ProviderType" :: NullOrUndefined (IdentityProviderTypeType)
  , "ProviderDetails" :: NullOrUndefined (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeIdentityProviderType :: Newtype IdentityProviderType _


newtype IdentityProviderTypeType = IdentityProviderTypeType String
derive instance newtypeIdentityProviderTypeType :: Newtype IdentityProviderTypeType _


newtype IdpIdentifierType = IdpIdentifierType String
derive instance newtypeIdpIdentifierType :: Newtype IdpIdentifierType _


newtype IdpIdentifiersListType = IdpIdentifiersListType (Array IdpIdentifierType)
derive instance newtypeIdpIdentifiersListType :: Newtype IdpIdentifiersListType _


newtype ImageFileType = ImageFileType String
derive instance newtypeImageFileType :: Newtype ImageFileType _


newtype ImageUrlType = ImageUrlType String
derive instance newtypeImageUrlType :: Newtype ImageUrlType _


-- | <p>Initiates the authentication request.</p>
newtype InitiateAuthRequest = InitiateAuthRequest 
  { "AuthFlow" :: (AuthFlowType)
  , "AuthParameters" :: NullOrUndefined (AuthParametersType)
  , "ClientMetadata" :: NullOrUndefined (ClientMetadataType)
  , "ClientId" :: (ClientIdType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  }
derive instance newtypeInitiateAuthRequest :: Newtype InitiateAuthRequest _


-- | <p>Initiates the authentication response.</p>
newtype InitiateAuthResponse = InitiateAuthResponse 
  { "ChallengeName" :: NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeInitiateAuthResponse :: Newtype InitiateAuthResponse _


newtype IntegerType = IntegerType Int
derive instance newtypeIntegerType :: Newtype IntegerType _


-- | <p>This exception is thrown when Amazon Cognito encounters an internal error.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _


-- | <p>This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.</p>
newtype InvalidEmailRoleAccessPolicyException = InvalidEmailRoleAccessPolicyException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidEmailRoleAccessPolicyException :: Newtype InvalidEmailRoleAccessPolicyException _


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.</p>
newtype InvalidLambdaResponseException = InvalidLambdaResponseException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidLambdaResponseException :: Newtype InvalidLambdaResponseException _


-- | <p>This exception is thrown when the specified OAuth flow is invalid.</p>
newtype InvalidOAuthFlowException = InvalidOAuthFlowException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidOAuthFlowException :: Newtype InvalidOAuthFlowException _


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid parameter.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid password.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidPasswordException :: Newtype InvalidPasswordException _


-- | <p>This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.</p>
newtype InvalidSmsRoleAccessPolicyException = InvalidSmsRoleAccessPolicyException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidSmsRoleAccessPolicyException :: Newtype InvalidSmsRoleAccessPolicyException _


-- | <p>This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust <b>cognito-idp.amazonaws.com</b> or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.</p>
newtype InvalidSmsRoleTrustRelationshipException = InvalidSmsRoleTrustRelationshipException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidSmsRoleTrustRelationshipException :: Newtype InvalidSmsRoleTrustRelationshipException _


-- | <p>This exception is thrown when the user pool configuration is invalid.</p>
newtype InvalidUserPoolConfigurationException = InvalidUserPoolConfigurationException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidUserPoolConfigurationException :: Newtype InvalidUserPoolConfigurationException _


-- | <p>Specifies the configuration for AWS Lambda triggers.</p>
newtype LambdaConfigType = LambdaConfigType 
  { "PreSignUp" :: NullOrUndefined (ArnType)
  , "CustomMessage" :: NullOrUndefined (ArnType)
  , "PostConfirmation" :: NullOrUndefined (ArnType)
  , "PreAuthentication" :: NullOrUndefined (ArnType)
  , "PostAuthentication" :: NullOrUndefined (ArnType)
  , "DefineAuthChallenge" :: NullOrUndefined (ArnType)
  , "CreateAuthChallenge" :: NullOrUndefined (ArnType)
  , "VerifyAuthChallengeResponse" :: NullOrUndefined (ArnType)
  , "PreTokenGeneration" :: NullOrUndefined (ArnType)
  , "UserMigration" :: NullOrUndefined (ArnType)
  }
derive instance newtypeLambdaConfigType :: Newtype LambdaConfigType _


-- | <p>This exception is thrown when a user exceeds the limit for a requested AWS resource.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>Represents the request to list the devices.</p>
newtype ListDevicesRequest = ListDevicesRequest 
  { "AccessToken" :: (TokenModelType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListDevicesRequest :: Newtype ListDevicesRequest _


-- | <p>Represents the response to list devices.</p>
newtype ListDevicesResponse = ListDevicesResponse 
  { "Devices" :: NullOrUndefined (DeviceListType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListDevicesResponse :: Newtype ListDevicesResponse _


newtype ListGroupsRequest = ListGroupsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListGroupsRequest :: Newtype ListGroupsRequest _


newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: NullOrUndefined (GroupListType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListGroupsResponse :: Newtype ListGroupsResponse _


newtype ListIdentityProvidersRequest = ListIdentityProvidersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined (ListProvidersLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListIdentityProvidersRequest :: Newtype ListIdentityProvidersRequest _


newtype ListIdentityProvidersResponse = ListIdentityProvidersResponse 
  { "Providers" :: (ProvidersListType)
  , "NextToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListIdentityProvidersResponse :: Newtype ListIdentityProvidersResponse _


newtype ListOfStringTypes = ListOfStringTypes (Array StringType)
derive instance newtypeListOfStringTypes :: Newtype ListOfStringTypes _


newtype ListProvidersLimitType = ListProvidersLimitType Int
derive instance newtypeListProvidersLimitType :: Newtype ListProvidersLimitType _


newtype ListResourceServersLimitType = ListResourceServersLimitType Int
derive instance newtypeListResourceServersLimitType :: Newtype ListResourceServersLimitType _


newtype ListResourceServersRequest = ListResourceServersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined (ListResourceServersLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListResourceServersRequest :: Newtype ListResourceServersRequest _


newtype ListResourceServersResponse = ListResourceServersResponse 
  { "ResourceServers" :: (ResourceServersListType)
  , "NextToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListResourceServersResponse :: Newtype ListResourceServersResponse _


-- | <p>Represents the request to list the user import jobs.</p>
newtype ListUserImportJobsRequest = ListUserImportJobsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: (PoolQueryLimitType)
  , "PaginationToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserImportJobsRequest :: Newtype ListUserImportJobsRequest _


-- | <p>Represents the response from the server to the request to list the user import jobs.</p>
newtype ListUserImportJobsResponse = ListUserImportJobsResponse 
  { "UserImportJobs" :: NullOrUndefined (UserImportJobsListType)
  , "PaginationToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserImportJobsResponse :: Newtype ListUserImportJobsResponse _


-- | <p>Represents the request to list the user pool clients.</p>
newtype ListUserPoolClientsRequest = ListUserPoolClientsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUserPoolClientsRequest :: Newtype ListUserPoolClientsRequest _


-- | <p>Represents the response from the server that lists user pool clients.</p>
newtype ListUserPoolClientsResponse = ListUserPoolClientsResponse 
  { "UserPoolClients" :: NullOrUndefined (UserPoolClientListType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUserPoolClientsResponse :: Newtype ListUserPoolClientsResponse _


-- | <p>Represents the request to list user pools.</p>
newtype ListUserPoolsRequest = ListUserPoolsRequest 
  { "NextToken" :: NullOrUndefined (PaginationKeyType)
  , "MaxResults" :: (PoolQueryLimitType)
  }
derive instance newtypeListUserPoolsRequest :: Newtype ListUserPoolsRequest _


-- | <p>Represents the response to list user pools.</p>
newtype ListUserPoolsResponse = ListUserPoolsResponse 
  { "UserPools" :: NullOrUndefined (UserPoolListType)
  , "NextToken" :: NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserPoolsResponse :: Newtype ListUserPoolsResponse _


newtype ListUsersInGroupRequest = ListUsersInGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "GroupName" :: (GroupNameType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUsersInGroupRequest :: Newtype ListUsersInGroupRequest _


newtype ListUsersInGroupResponse = ListUsersInGroupResponse 
  { "Users" :: NullOrUndefined (UsersListType)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUsersInGroupResponse :: Newtype ListUsersInGroupResponse _


-- | <p>Represents the request to list users.</p>
newtype ListUsersRequest = ListUsersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "AttributesToGet" :: NullOrUndefined (SearchedAttributeNamesListType)
  , "Limit" :: NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  , "Filter" :: NullOrUndefined (UserFilterType)
  }
derive instance newtypeListUsersRequest :: Newtype ListUsersRequest _


-- | <p>The response from the request to list users.</p>
newtype ListUsersResponse = ListUsersResponse 
  { "Users" :: NullOrUndefined (UsersListType)
  , "PaginationToken" :: NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListUsersResponse :: Newtype ListUsersResponse _


newtype LogoutURLsListType = LogoutURLsListType (Array RedirectUrlType)
derive instance newtypeLogoutURLsListType :: Newtype LogoutURLsListType _


newtype LongType = LongType Number
derive instance newtypeLongType :: Newtype LongType _


-- | <p>This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.</p>
newtype MFAMethodNotFoundException = MFAMethodNotFoundException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeMFAMethodNotFoundException :: Newtype MFAMethodNotFoundException _


newtype MFAOptionListType = MFAOptionListType (Array MFAOptionType)
derive instance newtypeMFAOptionListType :: Newtype MFAOptionListType _


-- | <p>Specifies the different settings for multi-factor authentication (MFA).</p>
newtype MFAOptionType = MFAOptionType 
  { "DeliveryMedium" :: NullOrUndefined (DeliveryMediumType)
  , "AttributeName" :: NullOrUndefined (AttributeNameType)
  }
derive instance newtypeMFAOptionType :: Newtype MFAOptionType _


newtype MessageActionType = MessageActionType String
derive instance newtypeMessageActionType :: Newtype MessageActionType _


-- | <p>The message template structure.</p>
newtype MessageTemplateType = MessageTemplateType 
  { "SMSMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType)
  , "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType)
  }
derive instance newtypeMessageTemplateType :: Newtype MessageTemplateType _


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _


-- | <p>The new device metadata type.</p>
newtype NewDeviceMetadataType = NewDeviceMetadataType 
  { "DeviceKey" :: NullOrUndefined (DeviceKeyType)
  , "DeviceGroupKey" :: NullOrUndefined (StringType)
  }
derive instance newtypeNewDeviceMetadataType :: Newtype NewDeviceMetadataType _


-- | <p>This exception is thrown when a user is not authorized.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _


-- | <p>The notify configuration type.</p>
newtype NotifyConfigurationType = NotifyConfigurationType 
  { "From" :: NullOrUndefined (StringType)
  , "ReplyTo" :: NullOrUndefined (StringType)
  , "SourceArn" :: (ArnType)
  , "BlockEmail" :: NullOrUndefined (NotifyEmailType)
  , "NoActionEmail" :: NullOrUndefined (NotifyEmailType)
  , "MfaEmail" :: NullOrUndefined (NotifyEmailType)
  }
derive instance newtypeNotifyConfigurationType :: Newtype NotifyConfigurationType _


-- | <p>The notify email type.</p>
newtype NotifyEmailType = NotifyEmailType 
  { "Subject" :: (EmailNotificationSubjectType)
  , "HtmlBody" :: NullOrUndefined (EmailNotificationBodyType)
  , "TextBody" :: NullOrUndefined (EmailNotificationBodyType)
  }
derive instance newtypeNotifyEmailType :: Newtype NotifyEmailType _


-- | <p>The minimum and maximum value of an attribute that is of the number data type.</p>
newtype NumberAttributeConstraintsType = NumberAttributeConstraintsType 
  { "MinValue" :: NullOrUndefined (StringType)
  , "MaxValue" :: NullOrUndefined (StringType)
  }
derive instance newtypeNumberAttributeConstraintsType :: Newtype NumberAttributeConstraintsType _


newtype OAuthFlowType = OAuthFlowType String
derive instance newtypeOAuthFlowType :: Newtype OAuthFlowType _


newtype OAuthFlowsType = OAuthFlowsType (Array OAuthFlowType)
derive instance newtypeOAuthFlowsType :: Newtype OAuthFlowsType _


newtype PaginationKey = PaginationKey String
derive instance newtypePaginationKey :: Newtype PaginationKey _


newtype PaginationKeyType = PaginationKeyType String
derive instance newtypePaginationKeyType :: Newtype PaginationKeyType _


newtype PasswordPolicyMinLengthType = PasswordPolicyMinLengthType Int
derive instance newtypePasswordPolicyMinLengthType :: Newtype PasswordPolicyMinLengthType _


-- | <p>The password policy type.</p>
newtype PasswordPolicyType = PasswordPolicyType 
  { "MinimumLength" :: NullOrUndefined (PasswordPolicyMinLengthType)
  , "RequireUppercase" :: NullOrUndefined (BooleanType)
  , "RequireLowercase" :: NullOrUndefined (BooleanType)
  , "RequireNumbers" :: NullOrUndefined (BooleanType)
  , "RequireSymbols" :: NullOrUndefined (BooleanType)
  }
derive instance newtypePasswordPolicyType :: Newtype PasswordPolicyType _


-- | <p>This exception is thrown when a password reset is required.</p>
newtype PasswordResetRequiredException = PasswordResetRequiredException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypePasswordResetRequiredException :: Newtype PasswordResetRequiredException _


newtype PasswordType = PasswordType String
derive instance newtypePasswordType :: Newtype PasswordType _


newtype PoolQueryLimitType = PoolQueryLimitType Int
derive instance newtypePoolQueryLimitType :: Newtype PoolQueryLimitType _


newtype PreSignedUrlType = PreSignedUrlType String
derive instance newtypePreSignedUrlType :: Newtype PreSignedUrlType _


newtype PrecedenceType = PrecedenceType Int
derive instance newtypePrecedenceType :: Newtype PrecedenceType _


-- | <p>This exception is thrown when a precondition is not met.</p>
newtype PreconditionNotMetException = PreconditionNotMetException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypePreconditionNotMetException :: Newtype PreconditionNotMetException _


-- | <p>A container for identity provider details.</p>
newtype ProviderDescription = ProviderDescription 
  { "ProviderName" :: NullOrUndefined (ProviderNameType)
  , "ProviderType" :: NullOrUndefined (IdentityProviderTypeType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeProviderDescription :: Newtype ProviderDescription _


newtype ProviderDetailsType = ProviderDetailsType (Map StringType StringType)
derive instance newtypeProviderDetailsType :: Newtype ProviderDetailsType _


newtype ProviderNameType = ProviderNameType String
derive instance newtypeProviderNameType :: Newtype ProviderNameType _


newtype ProviderNameTypeV1 = ProviderNameTypeV1 String
derive instance newtypeProviderNameTypeV1 :: Newtype ProviderNameTypeV1 _


-- | <p>A container for information about an identity provider for a user pool.</p>
newtype ProviderUserIdentifierType = ProviderUserIdentifierType 
  { "ProviderName" :: NullOrUndefined (ProviderNameType)
  , "ProviderAttributeName" :: NullOrUndefined (StringType)
  , "ProviderAttributeValue" :: NullOrUndefined (StringType)
  }
derive instance newtypeProviderUserIdentifierType :: Newtype ProviderUserIdentifierType _


newtype ProvidersListType = ProvidersListType (Array ProviderDescription)
derive instance newtypeProvidersListType :: Newtype ProvidersListType _


newtype QueryLimit = QueryLimit Int
derive instance newtypeQueryLimit :: Newtype QueryLimit _


newtype QueryLimitType = QueryLimitType Int
derive instance newtypeQueryLimitType :: Newtype QueryLimitType _


newtype RedirectUrlType = RedirectUrlType String
derive instance newtypeRedirectUrlType :: Newtype RedirectUrlType _


newtype RefreshTokenValidityType = RefreshTokenValidityType Int
derive instance newtypeRefreshTokenValidityType :: Newtype RefreshTokenValidityType _


-- | <p>Represents the request to resend the confirmation code.</p>
newtype ResendConfirmationCodeRequest = ResendConfirmationCodeRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined (SecretHashType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  , "Username" :: (UsernameType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  }
derive instance newtypeResendConfirmationCodeRequest :: Newtype ResendConfirmationCodeRequest _


-- | <p>The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.</p>
newtype ResendConfirmationCodeResponse = ResendConfirmationCodeResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeResendConfirmationCodeResponse :: Newtype ResendConfirmationCodeResponse _


-- | <p>This exception is thrown when the Amazon Cognito service cannot find the requested resource.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype ResourceServerIdentifierType = ResourceServerIdentifierType String
derive instance newtypeResourceServerIdentifierType :: Newtype ResourceServerIdentifierType _


newtype ResourceServerNameType = ResourceServerNameType String
derive instance newtypeResourceServerNameType :: Newtype ResourceServerNameType _


newtype ResourceServerScopeDescriptionType = ResourceServerScopeDescriptionType String
derive instance newtypeResourceServerScopeDescriptionType :: Newtype ResourceServerScopeDescriptionType _


newtype ResourceServerScopeListType = ResourceServerScopeListType (Array ResourceServerScopeType)
derive instance newtypeResourceServerScopeListType :: Newtype ResourceServerScopeListType _


newtype ResourceServerScopeNameType = ResourceServerScopeNameType String
derive instance newtypeResourceServerScopeNameType :: Newtype ResourceServerScopeNameType _


-- | <p>A resource server scope.</p>
newtype ResourceServerScopeType = ResourceServerScopeType 
  { "ScopeName" :: (ResourceServerScopeNameType)
  , "ScopeDescription" :: (ResourceServerScopeDescriptionType)
  }
derive instance newtypeResourceServerScopeType :: Newtype ResourceServerScopeType _


-- | <p>A container for information about a resource server for a user pool.</p>
newtype ResourceServerType = ResourceServerType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "Identifier" :: NullOrUndefined (ResourceServerIdentifierType)
  , "Name" :: NullOrUndefined (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeResourceServerType :: Newtype ResourceServerType _


newtype ResourceServersListType = ResourceServersListType (Array ResourceServerType)
derive instance newtypeResourceServersListType :: Newtype ResourceServersListType _


-- | <p>The request to respond to an authentication challenge.</p>
newtype RespondToAuthChallengeRequest = RespondToAuthChallengeRequest 
  { "ClientId" :: (ClientIdType)
  , "ChallengeName" :: (ChallengeNameType)
  , "Session" :: NullOrUndefined (SessionType)
  , "ChallengeResponses" :: NullOrUndefined (ChallengeResponsesType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  }
derive instance newtypeRespondToAuthChallengeRequest :: Newtype RespondToAuthChallengeRequest _


-- | <p>The response to respond to the authentication challenge.</p>
newtype RespondToAuthChallengeResponse = RespondToAuthChallengeResponse 
  { "ChallengeName" :: NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeRespondToAuthChallengeResponse :: Newtype RespondToAuthChallengeResponse _


-- | <p>The risk configuration type.</p>
newtype RiskConfigurationType = RiskConfigurationType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  , "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType)
  , "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType)
  , "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeRiskConfigurationType :: Newtype RiskConfigurationType _


newtype RiskDecisionType = RiskDecisionType String
derive instance newtypeRiskDecisionType :: Newtype RiskDecisionType _


-- | <p>The type of the configuration to override the risk decision.</p>
newtype RiskExceptionConfigurationType = RiskExceptionConfigurationType 
  { "BlockedIPRangeList" :: NullOrUndefined (BlockedIPRangeListType)
  , "SkippedIPRangeList" :: NullOrUndefined (SkippedIPRangeListType)
  }
derive instance newtypeRiskExceptionConfigurationType :: Newtype RiskExceptionConfigurationType _


newtype RiskLevelType = RiskLevelType String
derive instance newtypeRiskLevelType :: Newtype RiskLevelType _


newtype S3BucketType = S3BucketType String
derive instance newtypeS3BucketType :: Newtype S3BucketType _


-- | <p>The SMS multi-factor authentication (MFA) settings type.</p>
newtype SMSMfaSettingsType = SMSMfaSettingsType 
  { "Enabled" :: NullOrUndefined (BooleanType)
  , "PreferredMfa" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeSMSMfaSettingsType :: Newtype SMSMfaSettingsType _


-- | <p>Contains information about the schema attribute.</p>
newtype SchemaAttributeType = SchemaAttributeType 
  { "Name" :: NullOrUndefined (CustomAttributeNameType)
  , "AttributeDataType" :: NullOrUndefined (AttributeDataType)
  , "DeveloperOnlyAttribute" :: NullOrUndefined (BooleanType)
  , "Mutable" :: NullOrUndefined (BooleanType)
  , "Required" :: NullOrUndefined (BooleanType)
  , "NumberAttributeConstraints" :: NullOrUndefined (NumberAttributeConstraintsType)
  , "StringAttributeConstraints" :: NullOrUndefined (StringAttributeConstraintsType)
  }
derive instance newtypeSchemaAttributeType :: Newtype SchemaAttributeType _


newtype SchemaAttributesListType = SchemaAttributesListType (Array SchemaAttributeType)
derive instance newtypeSchemaAttributesListType :: Newtype SchemaAttributesListType _


-- | <p>This exception is thrown when the specified scope does not exist.</p>
newtype ScopeDoesNotExistException = ScopeDoesNotExistException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeScopeDoesNotExistException :: Newtype ScopeDoesNotExistException _


newtype ScopeListType = ScopeListType (Array ScopeType)
derive instance newtypeScopeListType :: Newtype ScopeListType _


newtype ScopeType = ScopeType String
derive instance newtypeScopeType :: Newtype ScopeType _


newtype SearchPaginationTokenType = SearchPaginationTokenType String
derive instance newtypeSearchPaginationTokenType :: Newtype SearchPaginationTokenType _


newtype SearchedAttributeNamesListType = SearchedAttributeNamesListType (Array AttributeNameType)
derive instance newtypeSearchedAttributeNamesListType :: Newtype SearchedAttributeNamesListType _


newtype SecretCodeType = SecretCodeType String
derive instance newtypeSecretCodeType :: Newtype SecretCodeType _


newtype SecretHashType = SecretHashType String
derive instance newtypeSecretHashType :: Newtype SecretHashType _


newtype SessionType = SessionType String
derive instance newtypeSessionType :: Newtype SessionType _


newtype SetRiskConfigurationRequest = SetRiskConfigurationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  , "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined (CompromisedCredentialsRiskConfigurationType)
  , "AccountTakeoverRiskConfiguration" :: NullOrUndefined (AccountTakeoverRiskConfigurationType)
  , "RiskExceptionConfiguration" :: NullOrUndefined (RiskExceptionConfigurationType)
  }
derive instance newtypeSetRiskConfigurationRequest :: Newtype SetRiskConfigurationRequest _


newtype SetRiskConfigurationResponse = SetRiskConfigurationResponse 
  { "RiskConfiguration" :: (RiskConfigurationType)
  }
derive instance newtypeSetRiskConfigurationResponse :: Newtype SetRiskConfigurationResponse _


newtype SetUICustomizationRequest = SetUICustomizationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  , "CSS" :: NullOrUndefined (CSSType)
  , "ImageFile" :: NullOrUndefined (ImageFileType)
  }
derive instance newtypeSetUICustomizationRequest :: Newtype SetUICustomizationRequest _


newtype SetUICustomizationResponse = SetUICustomizationResponse 
  { "UICustomization" :: (UICustomizationType)
  }
derive instance newtypeSetUICustomizationResponse :: Newtype SetUICustomizationResponse _


newtype SetUserMFAPreferenceRequest = SetUserMFAPreferenceRequest 
  { "SMSMfaSettings" :: NullOrUndefined (SMSMfaSettingsType)
  , "SoftwareTokenMfaSettings" :: NullOrUndefined (SoftwareTokenMfaSettingsType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeSetUserMFAPreferenceRequest :: Newtype SetUserMFAPreferenceRequest _


newtype SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse 
  { 
  }
derive instance newtypeSetUserMFAPreferenceResponse :: Newtype SetUserMFAPreferenceResponse _


newtype SetUserPoolMfaConfigRequest = SetUserPoolMfaConfigRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeSetUserPoolMfaConfigRequest :: Newtype SetUserPoolMfaConfigRequest _


newtype SetUserPoolMfaConfigResponse = SetUserPoolMfaConfigResponse 
  { "SmsMfaConfiguration" :: NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeSetUserPoolMfaConfigResponse :: Newtype SetUserPoolMfaConfigResponse _


-- | <p>Represents the request to set user settings.</p>
newtype SetUserSettingsRequest = SetUserSettingsRequest 
  { "AccessToken" :: (TokenModelType)
  , "MFAOptions" :: (MFAOptionListType)
  }
derive instance newtypeSetUserSettingsRequest :: Newtype SetUserSettingsRequest _


-- | <p>The response from the server for a set user settings request.</p>
newtype SetUserSettingsResponse = SetUserSettingsResponse 
  { 
  }
derive instance newtypeSetUserSettingsResponse :: Newtype SetUserSettingsResponse _


-- | <p>Represents the request to register a user.</p>
newtype SignUpRequest = SignUpRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "Password" :: (PasswordType)
  , "UserAttributes" :: NullOrUndefined (AttributeListType)
  , "ValidationData" :: NullOrUndefined (AttributeListType)
  , "AnalyticsMetadata" :: NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined (UserContextDataType)
  }
derive instance newtypeSignUpRequest :: Newtype SignUpRequest _


-- | <p>The response from the server for a registration request.</p>
newtype SignUpResponse = SignUpResponse 
  { "UserConfirmed" :: (BooleanType)
  , "CodeDeliveryDetails" :: NullOrUndefined (CodeDeliveryDetailsType)
  , "UserSub" :: (StringType)
  }
derive instance newtypeSignUpResponse :: Newtype SignUpResponse _


newtype SkippedIPRangeListType = SkippedIPRangeListType (Array StringType)
derive instance newtypeSkippedIPRangeListType :: Newtype SkippedIPRangeListType _


-- | <p>The SMS configuration type.</p>
newtype SmsConfigurationType = SmsConfigurationType 
  { "SnsCallerArn" :: (ArnType)
  , "ExternalId" :: NullOrUndefined (StringType)
  }
derive instance newtypeSmsConfigurationType :: Newtype SmsConfigurationType _


-- | <p>The SMS text message multi-factor authentication (MFA) configuration type.</p>
newtype SmsMfaConfigType = SmsMfaConfigType 
  { "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType)
  }
derive instance newtypeSmsMfaConfigType :: Newtype SmsMfaConfigType _


newtype SmsVerificationMessageType = SmsVerificationMessageType String
derive instance newtypeSmsVerificationMessageType :: Newtype SmsVerificationMessageType _


-- | <p>This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.</p>
newtype SoftwareTokenMFANotFoundException = SoftwareTokenMFANotFoundException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeSoftwareTokenMFANotFoundException :: Newtype SoftwareTokenMFANotFoundException _


newtype SoftwareTokenMFAUserCodeType = SoftwareTokenMFAUserCodeType String
derive instance newtypeSoftwareTokenMFAUserCodeType :: Newtype SoftwareTokenMFAUserCodeType _


-- | <p>The type used for enabling software token MFA at the user pool level.</p>
newtype SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType 
  { "Enabled" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeSoftwareTokenMfaConfigType :: Newtype SoftwareTokenMfaConfigType _


-- | <p>The type used for enabling software token MFA at the user level.</p>
newtype SoftwareTokenMfaSettingsType = SoftwareTokenMfaSettingsType 
  { "Enabled" :: NullOrUndefined (BooleanType)
  , "PreferredMfa" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeSoftwareTokenMfaSettingsType :: Newtype SoftwareTokenMfaSettingsType _


-- | <p>Represents the request to start the user import job.</p>
newtype StartUserImportJobRequest = StartUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeStartUserImportJobRequest :: Newtype StartUserImportJobRequest _


-- | <p>Represents the response from the server to the request to start the user import job.</p>
newtype StartUserImportJobResponse = StartUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined (UserImportJobType)
  }
derive instance newtypeStartUserImportJobResponse :: Newtype StartUserImportJobResponse _


newtype StatusType = StatusType String
derive instance newtypeStatusType :: Newtype StatusType _


-- | <p>Represents the request to stop the user import job.</p>
newtype StopUserImportJobRequest = StopUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeStopUserImportJobRequest :: Newtype StopUserImportJobRequest _


-- | <p>Represents the response from the server to the request to stop the user import job.</p>
newtype StopUserImportJobResponse = StopUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined (UserImportJobType)
  }
derive instance newtypeStopUserImportJobResponse :: Newtype StopUserImportJobResponse _


-- | <p>The constraints associated with a string attribute.</p>
newtype StringAttributeConstraintsType = StringAttributeConstraintsType 
  { "MinLength" :: NullOrUndefined (StringType)
  , "MaxLength" :: NullOrUndefined (StringType)
  }
derive instance newtypeStringAttributeConstraintsType :: Newtype StringAttributeConstraintsType _


newtype StringType = StringType String
derive instance newtypeStringType :: Newtype StringType _


newtype SupportedIdentityProvidersListType = SupportedIdentityProvidersListType (Array ProviderNameType)
derive instance newtypeSupportedIdentityProvidersListType :: Newtype SupportedIdentityProvidersListType _


newtype TokenModelType = TokenModelType String
derive instance newtypeTokenModelType :: Newtype TokenModelType _


-- | <p>This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).</p>
newtype TooManyFailedAttemptsException = TooManyFailedAttemptsException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeTooManyFailedAttemptsException :: Newtype TooManyFailedAttemptsException _


-- | <p>This exception is thrown when the user has made too many requests for a given operation.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | <p>A container for the UI customization information for a user pool's built-in app UI.</p>
newtype UICustomizationType = UICustomizationType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  , "ImageUrl" :: NullOrUndefined (ImageUrlType)
  , "CSS" :: NullOrUndefined (CSSType)
  , "CSSVersion" :: NullOrUndefined (CSSVersionType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeUICustomizationType :: Newtype UICustomizationType _


-- | <p>This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.</p>
newtype UnexpectedLambdaException = UnexpectedLambdaException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUnexpectedLambdaException :: Newtype UnexpectedLambdaException _


-- | <p>This exception is thrown when the specified identifier is not supported.</p>
newtype UnsupportedIdentityProviderException = UnsupportedIdentityProviderException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUnsupportedIdentityProviderException :: Newtype UnsupportedIdentityProviderException _


-- | <p>The request failed because the user is in an unsupported state.</p>
newtype UnsupportedUserStateException = UnsupportedUserStateException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUnsupportedUserStateException :: Newtype UnsupportedUserStateException _


newtype UpdateAuthEventFeedbackRequest = UpdateAuthEventFeedbackRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "EventId" :: (EventIdType)
  , "FeedbackToken" :: (TokenModelType)
  , "FeedbackValue" :: (FeedbackValueType)
  }
derive instance newtypeUpdateAuthEventFeedbackRequest :: Newtype UpdateAuthEventFeedbackRequest _


newtype UpdateAuthEventFeedbackResponse = UpdateAuthEventFeedbackResponse 
  { 
  }
derive instance newtypeUpdateAuthEventFeedbackResponse :: Newtype UpdateAuthEventFeedbackResponse _


-- | <p>Represents the request to update the device status.</p>
newtype UpdateDeviceStatusRequest = UpdateDeviceStatusRequest 
  { "AccessToken" :: (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceRememberedStatus" :: NullOrUndefined (DeviceRememberedStatusType)
  }
derive instance newtypeUpdateDeviceStatusRequest :: Newtype UpdateDeviceStatusRequest _


-- | <p>The response to the request to update the device status.</p>
newtype UpdateDeviceStatusResponse = UpdateDeviceStatusResponse 
  { 
  }
derive instance newtypeUpdateDeviceStatusResponse :: Newtype UpdateDeviceStatusResponse _


newtype UpdateGroupRequest = UpdateGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Description" :: NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined (PrecedenceType)
  }
derive instance newtypeUpdateGroupRequest :: Newtype UpdateGroupRequest _


newtype UpdateGroupResponse = UpdateGroupResponse 
  { "Group" :: NullOrUndefined (GroupType)
  }
derive instance newtypeUpdateGroupResponse :: Newtype UpdateGroupResponse _


newtype UpdateIdentityProviderRequest = UpdateIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  , "ProviderDetails" :: NullOrUndefined (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined (IdpIdentifiersListType)
  }
derive instance newtypeUpdateIdentityProviderRequest :: Newtype UpdateIdentityProviderRequest _


newtype UpdateIdentityProviderResponse = UpdateIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeUpdateIdentityProviderResponse :: Newtype UpdateIdentityProviderResponse _


newtype UpdateResourceServerRequest = UpdateResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  , "Name" :: (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeUpdateResourceServerRequest :: Newtype UpdateResourceServerRequest _


newtype UpdateResourceServerResponse = UpdateResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeUpdateResourceServerResponse :: Newtype UpdateResourceServerResponse _


-- | <p>Represents the request to update user attributes.</p>
newtype UpdateUserAttributesRequest = UpdateUserAttributesRequest 
  { "UserAttributes" :: (AttributeListType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeUpdateUserAttributesRequest :: Newtype UpdateUserAttributesRequest _


-- | <p>Represents the response from the server for the request to update user attributes.</p>
newtype UpdateUserAttributesResponse = UpdateUserAttributesResponse 
  { "CodeDeliveryDetailsList" :: NullOrUndefined (CodeDeliveryDetailsListType)
  }
derive instance newtypeUpdateUserAttributesResponse :: Newtype UpdateUserAttributesResponse _


-- | <p>Represents the request to update the user pool client.</p>
newtype UpdateUserPoolClientRequest = UpdateUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "ClientName" :: NullOrUndefined (ClientNameType)
  , "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeUpdateUserPoolClientRequest :: Newtype UpdateUserPoolClientRequest _


-- | <p>Represents the response from the server to the request to update the user pool client.</p>
newtype UpdateUserPoolClientResponse = UpdateUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeUpdateUserPoolClientResponse :: Newtype UpdateUserPoolClientResponse _


-- | <p>Represents the request to update the user pool.</p>
newtype UpdateUserPoolRequest = UpdateUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Policies" :: NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined (LambdaConfigType)
  , "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType)
  , "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined (UserPoolTagsType)
  , "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType)
  , "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeUpdateUserPoolRequest :: Newtype UpdateUserPoolRequest _


-- | <p>Represents the response from the server when you make a request to update the user pool.</p>
newtype UpdateUserPoolResponse = UpdateUserPoolResponse 
  { 
  }
derive instance newtypeUpdateUserPoolResponse :: Newtype UpdateUserPoolResponse _


-- | <p>Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>
newtype UserContextDataType = UserContextDataType 
  { "EncodedData" :: NullOrUndefined (StringType)
  }
derive instance newtypeUserContextDataType :: Newtype UserContextDataType _


newtype UserFilterType = UserFilterType String
derive instance newtypeUserFilterType :: Newtype UserFilterType _


-- | <p>This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.</p>
newtype UserImportInProgressException = UserImportInProgressException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserImportInProgressException :: Newtype UserImportInProgressException _


newtype UserImportJobIdType = UserImportJobIdType String
derive instance newtypeUserImportJobIdType :: Newtype UserImportJobIdType _


newtype UserImportJobNameType = UserImportJobNameType String
derive instance newtypeUserImportJobNameType :: Newtype UserImportJobNameType _


newtype UserImportJobStatusType = UserImportJobStatusType String
derive instance newtypeUserImportJobStatusType :: Newtype UserImportJobStatusType _


-- | <p>The user import job type.</p>
newtype UserImportJobType = UserImportJobType 
  { "JobName" :: NullOrUndefined (UserImportJobNameType)
  , "JobId" :: NullOrUndefined (UserImportJobIdType)
  , "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "PreSignedUrl" :: NullOrUndefined (PreSignedUrlType)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "StartDate" :: NullOrUndefined (DateType)
  , "CompletionDate" :: NullOrUndefined (DateType)
  , "Status" :: NullOrUndefined (UserImportJobStatusType)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (ArnType)
  , "ImportedUsers" :: NullOrUndefined (LongType)
  , "SkippedUsers" :: NullOrUndefined (LongType)
  , "FailedUsers" :: NullOrUndefined (LongType)
  , "CompletionMessage" :: NullOrUndefined (CompletionMessageType)
  }
derive instance newtypeUserImportJobType :: Newtype UserImportJobType _


newtype UserImportJobsListType = UserImportJobsListType (Array UserImportJobType)
derive instance newtypeUserImportJobsListType :: Newtype UserImportJobsListType _


-- | <p>This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.</p>
newtype UserLambdaValidationException = UserLambdaValidationException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserLambdaValidationException :: Newtype UserLambdaValidationException _


newtype UserMFASettingListType = UserMFASettingListType (Array StringType)
derive instance newtypeUserMFASettingListType :: Newtype UserMFASettingListType _


-- | <p>This exception is thrown when a user is not confirmed successfully.</p>
newtype UserNotConfirmedException = UserNotConfirmedException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserNotConfirmedException :: Newtype UserNotConfirmedException _


-- | <p>This exception is thrown when a user is not found.</p>
newtype UserNotFoundException = UserNotFoundException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserNotFoundException :: Newtype UserNotFoundException _


-- | <p>This exception is thrown when user pool add-ons are not enabled.</p>
newtype UserPoolAddOnNotEnabledException = UserPoolAddOnNotEnabledException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserPoolAddOnNotEnabledException :: Newtype UserPoolAddOnNotEnabledException _


-- | <p>The user pool add-ons type.</p>
newtype UserPoolAddOnsType = UserPoolAddOnsType 
  { "AdvancedSecurityMode" :: (AdvancedSecurityModeType)
  }
derive instance newtypeUserPoolAddOnsType :: Newtype UserPoolAddOnsType _


-- | <p>The description of the user pool client.</p>
newtype UserPoolClientDescription = UserPoolClientDescription 
  { "ClientId" :: NullOrUndefined (ClientIdType)
  , "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "ClientName" :: NullOrUndefined (ClientNameType)
  }
derive instance newtypeUserPoolClientDescription :: Newtype UserPoolClientDescription _


newtype UserPoolClientListType = UserPoolClientListType (Array UserPoolClientDescription)
derive instance newtypeUserPoolClientListType :: Newtype UserPoolClientListType _


-- | <p>Contains information about a user pool client.</p>
newtype UserPoolClientType = UserPoolClientType 
  { "UserPoolId" :: NullOrUndefined (UserPoolIdType)
  , "ClientName" :: NullOrUndefined (ClientNameType)
  , "ClientId" :: NullOrUndefined (ClientIdType)
  , "ClientSecret" :: NullOrUndefined (ClientSecretType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "RefreshTokenValidity" :: NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeUserPoolClientType :: Newtype UserPoolClientType _


-- | <p>A user pool description.</p>
newtype UserPoolDescriptionType = UserPoolDescriptionType 
  { "Id" :: NullOrUndefined (UserPoolIdType)
  , "Name" :: NullOrUndefined (UserPoolNameType)
  , "LambdaConfig" :: NullOrUndefined (LambdaConfigType)
  , "Status" :: NullOrUndefined (StatusType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeUserPoolDescriptionType :: Newtype UserPoolDescriptionType _


newtype UserPoolIdType = UserPoolIdType String
derive instance newtypeUserPoolIdType :: Newtype UserPoolIdType _


newtype UserPoolListType = UserPoolListType (Array UserPoolDescriptionType)
derive instance newtypeUserPoolListType :: Newtype UserPoolListType _


newtype UserPoolMfaType = UserPoolMfaType String
derive instance newtypeUserPoolMfaType :: Newtype UserPoolMfaType _


newtype UserPoolNameType = UserPoolNameType String
derive instance newtypeUserPoolNameType :: Newtype UserPoolNameType _


-- | <p>The policy associated with a user pool.</p>
newtype UserPoolPolicyType = UserPoolPolicyType 
  { "PasswordPolicy" :: NullOrUndefined (PasswordPolicyType)
  }
derive instance newtypeUserPoolPolicyType :: Newtype UserPoolPolicyType _


-- | <p>This exception is thrown when a user pool tag cannot be set or updated.</p>
newtype UserPoolTaggingException = UserPoolTaggingException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUserPoolTaggingException :: Newtype UserPoolTaggingException _


newtype UserPoolTagsType = UserPoolTagsType (Map StringType StringType)
derive instance newtypeUserPoolTagsType :: Newtype UserPoolTagsType _


-- | <p>A container for information about the user pool.</p>
newtype UserPoolType = UserPoolType 
  { "Id" :: NullOrUndefined (UserPoolIdType)
  , "Name" :: NullOrUndefined (UserPoolNameType)
  , "Policies" :: NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined (LambdaConfigType)
  , "Status" :: NullOrUndefined (StatusType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "SchemaAttributes" :: NullOrUndefined (SchemaAttributesListType)
  , "AutoVerifiedAttributes" :: NullOrUndefined (VerifiedAttributesListType)
  , "AliasAttributes" :: NullOrUndefined (AliasAttributesListType)
  , "UsernameAttributes" :: NullOrUndefined (UsernameAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined (DeviceConfigurationType)
  , "EstimatedNumberOfUsers" :: NullOrUndefined (IntegerType)
  , "EmailConfiguration" :: NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined (UserPoolTagsType)
  , "SmsConfigurationFailure" :: NullOrUndefined (StringType)
  , "EmailConfigurationFailure" :: NullOrUndefined (StringType)
  , "Domain" :: NullOrUndefined (DomainType)
  , "AdminCreateUserConfig" :: NullOrUndefined (AdminCreateUserConfigType)
  , "UserPoolAddOns" :: NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeUserPoolType :: Newtype UserPoolType _


newtype UserStatusType = UserStatusType String
derive instance newtypeUserStatusType :: Newtype UserStatusType _


-- | <p>The user type.</p>
newtype UserType = UserType 
  { "Username" :: NullOrUndefined (UsernameType)
  , "Attributes" :: NullOrUndefined (AttributeListType)
  , "UserCreateDate" :: NullOrUndefined (DateType)
  , "UserLastModifiedDate" :: NullOrUndefined (DateType)
  , "Enabled" :: NullOrUndefined (BooleanType)
  , "UserStatus" :: NullOrUndefined (UserStatusType)
  , "MFAOptions" :: NullOrUndefined (MFAOptionListType)
  }
derive instance newtypeUserType :: Newtype UserType _


newtype UsernameAttributeType = UsernameAttributeType String
derive instance newtypeUsernameAttributeType :: Newtype UsernameAttributeType _


newtype UsernameAttributesListType = UsernameAttributesListType (Array UsernameAttributeType)
derive instance newtypeUsernameAttributesListType :: Newtype UsernameAttributesListType _


-- | <p>This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.</p>
newtype UsernameExistsException = UsernameExistsException 
  { "Message'" :: NullOrUndefined (MessageType)
  }
derive instance newtypeUsernameExistsException :: Newtype UsernameExistsException _


newtype UsernameType = UsernameType String
derive instance newtypeUsernameType :: Newtype UsernameType _


newtype UsersListType = UsersListType (Array UserType)
derive instance newtypeUsersListType :: Newtype UsersListType _


-- | <p>The template for verification messages.</p>
newtype VerificationMessageTemplateType = VerificationMessageTemplateType 
  { "SmsMessage" :: NullOrUndefined (SmsVerificationMessageType)
  , "EmailMessage" :: NullOrUndefined (EmailVerificationMessageType)
  , "EmailSubject" :: NullOrUndefined (EmailVerificationSubjectType)
  , "EmailMessageByLink" :: NullOrUndefined (EmailVerificationMessageByLinkType)
  , "EmailSubjectByLink" :: NullOrUndefined (EmailVerificationSubjectByLinkType)
  , "DefaultEmailOption" :: NullOrUndefined (DefaultEmailOptionType)
  }
derive instance newtypeVerificationMessageTemplateType :: Newtype VerificationMessageTemplateType _


newtype VerifiedAttributeType = VerifiedAttributeType String
derive instance newtypeVerifiedAttributeType :: Newtype VerifiedAttributeType _


newtype VerifiedAttributesListType = VerifiedAttributesListType (Array VerifiedAttributeType)
derive instance newtypeVerifiedAttributesListType :: Newtype VerifiedAttributesListType _


newtype VerifySoftwareTokenRequest = VerifySoftwareTokenRequest 
  { "AccessToken" :: NullOrUndefined (TokenModelType)
  , "Session" :: NullOrUndefined (SessionType)
  , "UserCode" :: (SoftwareTokenMFAUserCodeType)
  , "FriendlyDeviceName" :: NullOrUndefined (StringType)
  }
derive instance newtypeVerifySoftwareTokenRequest :: Newtype VerifySoftwareTokenRequest _


newtype VerifySoftwareTokenResponse = VerifySoftwareTokenResponse 
  { "Status" :: NullOrUndefined (VerifySoftwareTokenResponseType)
  , "Session" :: NullOrUndefined (SessionType)
  }
derive instance newtypeVerifySoftwareTokenResponse :: Newtype VerifySoftwareTokenResponse _


newtype VerifySoftwareTokenResponseType = VerifySoftwareTokenResponseType String
derive instance newtypeVerifySoftwareTokenResponseType :: Newtype VerifySoftwareTokenResponseType _


-- | <p>Represents the request to verify user attributes.</p>
newtype VerifyUserAttributeRequest = VerifyUserAttributeRequest 
  { "AccessToken" :: (TokenModelType)
  , "AttributeName" :: (AttributeNameType)
  , "Code" :: (ConfirmationCodeType)
  }
derive instance newtypeVerifyUserAttributeRequest :: Newtype VerifyUserAttributeRequest _


-- | <p>A container representing the response from the server from the request to verify user attributes.</p>
newtype VerifyUserAttributeResponse = VerifyUserAttributeResponse 
  { 
  }
derive instance newtypeVerifyUserAttributeResponse :: Newtype VerifyUserAttributeResponse _
