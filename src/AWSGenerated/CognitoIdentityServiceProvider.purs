

-- | <p>Using the Amazon Cognito User Pools API, you can create a user pool to manage directories and users. You can authenticate a user to obtain tokens related to user identity and access policies.</p> <p>This API reference provides information about user pools in Amazon Cognito User Pools.</p> <p>For more information, see the Amazon Cognito Documentation.</p>
module AWS.CognitoIdentityServiceProvider where

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

serviceName = "CognitoIdentityServiceProvider" :: String


-- | <p>Adds additional user attributes to the user pool schema.</p>
addCustomAttributes :: forall eff. AddCustomAttributesRequest -> Aff (exception :: EXCEPTION | eff) AddCustomAttributesResponse
addCustomAttributes = Request.request serviceName "addCustomAttributes" 


-- | <p>Adds the specified user to the specified group.</p> <p>Requires developer credentials.</p>
adminAddUserToGroup :: forall eff. AdminAddUserToGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
adminAddUserToGroup = Request.request serviceName "adminAddUserToGroup" 


-- | <p>Confirms user registration as an admin without using a confirmation code. Works on any user.</p> <p>Requires developer credentials.</p>
adminConfirmSignUp :: forall eff. AdminConfirmSignUpRequest -> Aff (exception :: EXCEPTION | eff) AdminConfirmSignUpResponse
adminConfirmSignUp = Request.request serviceName "adminConfirmSignUp" 


-- | <p>Creates a new user in the specified user pool.</p> <p>If <code>MessageAction</code> is not set, the default is to send a welcome message via email or phone (SMS).</p> <note> <p>This message is based on a template that you configured in your call to or . This template includes your custom sign-up instructions and placeholders for user name and temporary password.</p> </note> <p>Alternatively, you can call AdminCreateUser with “SUPPRESS” for the <code>MessageAction</code> parameter, and Amazon Cognito will not send any email. </p> <p>In either case, the user will be in the <code>FORCE_CHANGE_PASSWORD</code> state until they sign in and change their password.</p> <p>AdminCreateUser requires developer credentials.</p>
adminCreateUser :: forall eff. AdminCreateUserRequest -> Aff (exception :: EXCEPTION | eff) AdminCreateUserResponse
adminCreateUser = Request.request serviceName "adminCreateUser" 


-- | <p>Deletes a user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDeleteUser :: forall eff. AdminDeleteUserRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
adminDeleteUser = Request.request serviceName "adminDeleteUser" 


-- | <p>Deletes the user attributes in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDeleteUserAttributes :: forall eff. AdminDeleteUserAttributesRequest -> Aff (exception :: EXCEPTION | eff) AdminDeleteUserAttributesResponse
adminDeleteUserAttributes = Request.request serviceName "adminDeleteUserAttributes" 


-- | <p>Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked <code>DestinationUser</code>) signs in, they must create a new user account. See .</p> <p>This action is enabled only for admin access and requires developer credentials.</p> <p>The <code>ProviderName</code> must match the value specified when creating an IdP for the pool. </p> <p>To disable a native username + password user, the <code>ProviderName</code> value must be <code>Cognito</code> and the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code>, with the <code>ProviderAttributeValue</code> being the name that is used in the user pool for the user.</p> <p>The <code>ProviderAttributeName</code> must always be <code>Cognito_Subject</code> for social identity providers. The <code>ProviderAttributeValue</code> must always be the exact subject that was used when the user was originally linked as a source user.</p> <p>For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the <code>ProviderAttributeName</code> and <code>ProviderAttributeValue</code> must be the same values that were used for the <code>SourceUser</code> when the identities were originally linked in the call. (If the linking was done with <code>ProviderAttributeName</code> set to <code>Cognito_Subject</code>, the same applies here). However, if the user has already signed in, the <code>ProviderAttributeName</code> must be <code>Cognito_Subject</code> and <code>ProviderAttributeValue</code> must be the subject of the SAML assertion.</p>
adminDisableProviderForUser :: forall eff. AdminDisableProviderForUserRequest -> Aff (exception :: EXCEPTION | eff) AdminDisableProviderForUserResponse
adminDisableProviderForUser = Request.request serviceName "adminDisableProviderForUser" 


-- | <p>Disables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminDisableUser :: forall eff. AdminDisableUserRequest -> Aff (exception :: EXCEPTION | eff) AdminDisableUserResponse
adminDisableUser = Request.request serviceName "adminDisableUser" 


-- | <p>Enables the specified user as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminEnableUser :: forall eff. AdminEnableUserRequest -> Aff (exception :: EXCEPTION | eff) AdminEnableUserResponse
adminEnableUser = Request.request serviceName "adminEnableUser" 


-- | <p>Forgets the device, as an administrator.</p> <p>Requires developer credentials.</p>
adminForgetDevice :: forall eff. AdminForgetDeviceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
adminForgetDevice = Request.request serviceName "adminForgetDevice" 


-- | <p>Gets the device, as an administrator.</p> <p>Requires developer credentials.</p>
adminGetDevice :: forall eff. AdminGetDeviceRequest -> Aff (exception :: EXCEPTION | eff) AdminGetDeviceResponse
adminGetDevice = Request.request serviceName "adminGetDevice" 


-- | <p>Gets the specified user by user name in a user pool as an administrator. Works on any user.</p> <p>Requires developer credentials.</p>
adminGetUser :: forall eff. AdminGetUserRequest -> Aff (exception :: EXCEPTION | eff) AdminGetUserResponse
adminGetUser = Request.request serviceName "adminGetUser" 


-- | <p>Initiates the authentication flow, as an administrator.</p> <p>Requires developer credentials.</p>
adminInitiateAuth :: forall eff. AdminInitiateAuthRequest -> Aff (exception :: EXCEPTION | eff) AdminInitiateAuthResponse
adminInitiateAuth = Request.request serviceName "adminInitiateAuth" 


-- | <p>Links an existing user account in a user pool (<code>DestinationUser</code>) to an identity from an external identity provider (<code>SourceUser</code>) based on a specified attribute name and value from the external identity provider. This allows you to create a link from the existing user account to an external federated user identity that has not yet been used to sign in, so that the federated user identity can be used to sign in as the existing user account. </p> <p> For example, if there is an existing user with a username and password, this API links that user to a federated user identity, so that when the federated user identity is used, the user signs in as the existing user account. </p> <important> <p>Because this API allows a user with an external federated identity to sign in as an existing user in the user pool, it is critical that it only be used with external identity providers and provider attributes that have been trusted by the application owner.</p> </important> <p>See also .</p> <p>This action is enabled only for admin access and requires developer credentials.</p>
adminLinkProviderForUser :: forall eff. AdminLinkProviderForUserRequest -> Aff (exception :: EXCEPTION | eff) AdminLinkProviderForUserResponse
adminLinkProviderForUser = Request.request serviceName "adminLinkProviderForUser" 


-- | <p>Lists devices, as an administrator.</p> <p>Requires developer credentials.</p>
adminListDevices :: forall eff. AdminListDevicesRequest -> Aff (exception :: EXCEPTION | eff) AdminListDevicesResponse
adminListDevices = Request.request serviceName "adminListDevices" 


-- | <p>Lists the groups that the user belongs to.</p> <p>Requires developer credentials.</p>
adminListGroupsForUser :: forall eff. AdminListGroupsForUserRequest -> Aff (exception :: EXCEPTION | eff) AdminListGroupsForUserResponse
adminListGroupsForUser = Request.request serviceName "adminListGroupsForUser" 


-- | <p>Lists a history of user activity and any risks detected as part of Amazon Cognito advanced security.</p>
adminListUserAuthEvents :: forall eff. AdminListUserAuthEventsRequest -> Aff (exception :: EXCEPTION | eff) AdminListUserAuthEventsResponse
adminListUserAuthEvents = Request.request serviceName "adminListUserAuthEvents" 


-- | <p>Removes the specified user from the specified group.</p> <p>Requires developer credentials.</p>
adminRemoveUserFromGroup :: forall eff. AdminRemoveUserFromGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
adminRemoveUserFromGroup = Request.request serviceName "adminRemoveUserFromGroup" 


-- | <p>Resets the specified user's password in a user pool as an administrator. Works on any user.</p> <p>When a developer calls this API, the current password is invalidated, so it must be changed. If a user tries to sign in after the API is called, the app will get a PasswordResetRequiredException exception back and should direct the user down the flow to reset the password, which is the same as the forgot password flow. In addition, if the user pool has phone verification selected and a verified phone number exists for the user, or if email verification is selected and a verified email exists for the user, calling this API will also result in sending a message to the end user with the code to change their password.</p> <p>Requires developer credentials.</p>
adminResetUserPassword :: forall eff. AdminResetUserPasswordRequest -> Aff (exception :: EXCEPTION | eff) AdminResetUserPasswordResponse
adminResetUserPassword = Request.request serviceName "adminResetUserPassword" 


-- | <p>Responds to an authentication challenge, as an administrator.</p> <p>Requires developer credentials.</p>
adminRespondToAuthChallenge :: forall eff. AdminRespondToAuthChallengeRequest -> Aff (exception :: EXCEPTION | eff) AdminRespondToAuthChallengeResponse
adminRespondToAuthChallenge = Request.request serviceName "adminRespondToAuthChallenge" 


-- | <p>Sets the user's multi-factor authentication (MFA) preference.</p>
adminSetUserMFAPreference :: forall eff. AdminSetUserMFAPreferenceRequest -> Aff (exception :: EXCEPTION | eff) AdminSetUserMFAPreferenceResponse
adminSetUserMFAPreference = Request.request serviceName "adminSetUserMFAPreference" 


-- | <p>Sets all the user settings for a specified user name. Works on any user.</p> <p>Requires developer credentials.</p>
adminSetUserSettings :: forall eff. AdminSetUserSettingsRequest -> Aff (exception :: EXCEPTION | eff) AdminSetUserSettingsResponse
adminSetUserSettings = Request.request serviceName "adminSetUserSettings" 


-- | <p>Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>
adminUpdateAuthEventFeedback :: forall eff. AdminUpdateAuthEventFeedbackRequest -> Aff (exception :: EXCEPTION | eff) AdminUpdateAuthEventFeedbackResponse
adminUpdateAuthEventFeedback = Request.request serviceName "adminUpdateAuthEventFeedback" 


-- | <p>Updates the device status as an administrator.</p> <p>Requires developer credentials.</p>
adminUpdateDeviceStatus :: forall eff. AdminUpdateDeviceStatusRequest -> Aff (exception :: EXCEPTION | eff) AdminUpdateDeviceStatusResponse
adminUpdateDeviceStatus = Request.request serviceName "adminUpdateDeviceStatus" 


-- | <p>Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.</p> <p>For custom attributes, you must prepend the <code>custom:</code> prefix to the attribute name.</p> <p>In addition to updating user attributes, this API can also be used to mark phone and email as verified.</p> <p>Requires developer credentials.</p>
adminUpdateUserAttributes :: forall eff. AdminUpdateUserAttributesRequest -> Aff (exception :: EXCEPTION | eff) AdminUpdateUserAttributesResponse
adminUpdateUserAttributes = Request.request serviceName "adminUpdateUserAttributes" 


-- | <p>Signs out users from all devices, as an administrator.</p> <p>Requires developer credentials.</p>
adminUserGlobalSignOut :: forall eff. AdminUserGlobalSignOutRequest -> Aff (exception :: EXCEPTION | eff) AdminUserGlobalSignOutResponse
adminUserGlobalSignOut = Request.request serviceName "adminUserGlobalSignOut" 


-- | <p>Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.</p>
associateSoftwareToken :: forall eff. AssociateSoftwareTokenRequest -> Aff (exception :: EXCEPTION | eff) AssociateSoftwareTokenResponse
associateSoftwareToken = Request.request serviceName "associateSoftwareToken" 


-- | <p>Changes the password for a specified user in a user pool.</p>
changePassword :: forall eff. ChangePasswordRequest -> Aff (exception :: EXCEPTION | eff) ChangePasswordResponse
changePassword = Request.request serviceName "changePassword" 


-- | <p>Confirms tracking of the device. This API call is the call that begins device tracking.</p>
confirmDevice :: forall eff. ConfirmDeviceRequest -> Aff (exception :: EXCEPTION | eff) ConfirmDeviceResponse
confirmDevice = Request.request serviceName "confirmDevice" 


-- | <p>Allows a user to enter a confirmation code to reset a forgotten password.</p>
confirmForgotPassword :: forall eff. ConfirmForgotPasswordRequest -> Aff (exception :: EXCEPTION | eff) ConfirmForgotPasswordResponse
confirmForgotPassword = Request.request serviceName "confirmForgotPassword" 


-- | <p>Confirms registration of a user and handles the existing alias from a previous user.</p>
confirmSignUp :: forall eff. ConfirmSignUpRequest -> Aff (exception :: EXCEPTION | eff) ConfirmSignUpResponse
confirmSignUp = Request.request serviceName "confirmSignUp" 


-- | <p>Creates a new group in the specified user pool.</p> <p>Requires developer credentials.</p>
createGroup :: forall eff. CreateGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateGroupResponse
createGroup = Request.request serviceName "createGroup" 


-- | <p>Creates an identity provider for a user pool.</p>
createIdentityProvider :: forall eff. CreateIdentityProviderRequest -> Aff (exception :: EXCEPTION | eff) CreateIdentityProviderResponse
createIdentityProvider = Request.request serviceName "createIdentityProvider" 


-- | <p>Creates a new OAuth2.0 resource server and defines custom scopes in it.</p>
createResourceServer :: forall eff. CreateResourceServerRequest -> Aff (exception :: EXCEPTION | eff) CreateResourceServerResponse
createResourceServer = Request.request serviceName "createResourceServer" 


-- | <p>Creates the user import job.</p>
createUserImportJob :: forall eff. CreateUserImportJobRequest -> Aff (exception :: EXCEPTION | eff) CreateUserImportJobResponse
createUserImportJob = Request.request serviceName "createUserImportJob" 


-- | <p>Creates a new Amazon Cognito user pool and sets the password policy for the pool.</p>
createUserPool :: forall eff. CreateUserPoolRequest -> Aff (exception :: EXCEPTION | eff) CreateUserPoolResponse
createUserPool = Request.request serviceName "createUserPool" 


-- | <p>Creates the user pool client.</p>
createUserPoolClient :: forall eff. CreateUserPoolClientRequest -> Aff (exception :: EXCEPTION | eff) CreateUserPoolClientResponse
createUserPoolClient = Request.request serviceName "createUserPoolClient" 


-- | <p>Creates a new domain for a user pool.</p>
createUserPoolDomain :: forall eff. CreateUserPoolDomainRequest -> Aff (exception :: EXCEPTION | eff) CreateUserPoolDomainResponse
createUserPoolDomain = Request.request serviceName "createUserPoolDomain" 


-- | <p>Deletes a group. Currently only groups with no members can be deleted.</p> <p>Requires developer credentials.</p>
deleteGroup :: forall eff. DeleteGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteGroup = Request.request serviceName "deleteGroup" 


-- | <p>Deletes an identity provider for a user pool.</p>
deleteIdentityProvider :: forall eff. DeleteIdentityProviderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIdentityProvider = Request.request serviceName "deleteIdentityProvider" 


-- | <p>Deletes a resource server.</p>
deleteResourceServer :: forall eff. DeleteResourceServerRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteResourceServer = Request.request serviceName "deleteResourceServer" 


-- | <p>Allows a user to delete himself or herself.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUser = Request.request serviceName "deleteUser" 


-- | <p>Deletes the attributes for a user.</p>
deleteUserAttributes :: forall eff. DeleteUserAttributesRequest -> Aff (exception :: EXCEPTION | eff) DeleteUserAttributesResponse
deleteUserAttributes = Request.request serviceName "deleteUserAttributes" 


-- | <p>Deletes the specified Amazon Cognito user pool.</p>
deleteUserPool :: forall eff. DeleteUserPoolRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUserPool = Request.request serviceName "deleteUserPool" 


-- | <p>Allows the developer to delete the user pool client.</p>
deleteUserPoolClient :: forall eff. DeleteUserPoolClientRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUserPoolClient = Request.request serviceName "deleteUserPoolClient" 


-- | <p>Deletes a domain for a user pool.</p>
deleteUserPoolDomain :: forall eff. DeleteUserPoolDomainRequest -> Aff (exception :: EXCEPTION | eff) DeleteUserPoolDomainResponse
deleteUserPoolDomain = Request.request serviceName "deleteUserPoolDomain" 


-- | <p>Gets information about a specific identity provider.</p>
describeIdentityProvider :: forall eff. DescribeIdentityProviderRequest -> Aff (exception :: EXCEPTION | eff) DescribeIdentityProviderResponse
describeIdentityProvider = Request.request serviceName "describeIdentityProvider" 


-- | <p>Describes a resource server.</p>
describeResourceServer :: forall eff. DescribeResourceServerRequest -> Aff (exception :: EXCEPTION | eff) DescribeResourceServerResponse
describeResourceServer = Request.request serviceName "describeResourceServer" 


-- | <p>Describes the risk configuration.</p>
describeRiskConfiguration :: forall eff. DescribeRiskConfigurationRequest -> Aff (exception :: EXCEPTION | eff) DescribeRiskConfigurationResponse
describeRiskConfiguration = Request.request serviceName "describeRiskConfiguration" 


-- | <p>Describes the user import job.</p>
describeUserImportJob :: forall eff. DescribeUserImportJobRequest -> Aff (exception :: EXCEPTION | eff) DescribeUserImportJobResponse
describeUserImportJob = Request.request serviceName "describeUserImportJob" 


-- | <p>Returns the configuration information and metadata of the specified user pool.</p>
describeUserPool :: forall eff. DescribeUserPoolRequest -> Aff (exception :: EXCEPTION | eff) DescribeUserPoolResponse
describeUserPool = Request.request serviceName "describeUserPool" 


-- | <p>Client method for returning the configuration information and metadata of the specified user pool client.</p>
describeUserPoolClient :: forall eff. DescribeUserPoolClientRequest -> Aff (exception :: EXCEPTION | eff) DescribeUserPoolClientResponse
describeUserPoolClient = Request.request serviceName "describeUserPoolClient" 


-- | <p>Gets information about a domain.</p>
describeUserPoolDomain :: forall eff. DescribeUserPoolDomainRequest -> Aff (exception :: EXCEPTION | eff) DescribeUserPoolDomainResponse
describeUserPoolDomain = Request.request serviceName "describeUserPoolDomain" 


-- | <p>Forgets the specified device.</p>
forgetDevice :: forall eff. ForgetDeviceRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
forgetDevice = Request.request serviceName "forgetDevice" 


-- | <p>Calling this API causes a message to be sent to the end user with a confirmation code that is required to change the user's password. For the <code>Username</code> parameter, you can use the username or user alias. If a verified phone number exists for the user, the confirmation code is sent to the phone number. Otherwise, if a verified email exists, the confirmation code is sent to the email. If neither a verified phone number nor a verified email exists, <code>InvalidParameterException</code> is thrown. To use the confirmation code for resetting the password, call .</p>
forgotPassword :: forall eff. ForgotPasswordRequest -> Aff (exception :: EXCEPTION | eff) ForgotPasswordResponse
forgotPassword = Request.request serviceName "forgotPassword" 


-- | <p>Gets the header information for the .csv file to be used as input for the user import job.</p>
getCSVHeader :: forall eff. GetCSVHeaderRequest -> Aff (exception :: EXCEPTION | eff) GetCSVHeaderResponse
getCSVHeader = Request.request serviceName "getCSVHeader" 


-- | <p>Gets the device.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (exception :: EXCEPTION | eff) GetDeviceResponse
getDevice = Request.request serviceName "getDevice" 


-- | <p>Gets a group.</p> <p>Requires developer credentials.</p>
getGroup :: forall eff. GetGroupRequest -> Aff (exception :: EXCEPTION | eff) GetGroupResponse
getGroup = Request.request serviceName "getGroup" 


-- | <p>Gets the specified identity provider.</p>
getIdentityProviderByIdentifier :: forall eff. GetIdentityProviderByIdentifierRequest -> Aff (exception :: EXCEPTION | eff) GetIdentityProviderByIdentifierResponse
getIdentityProviderByIdentifier = Request.request serviceName "getIdentityProviderByIdentifier" 


-- | <p>This method takes a user pool ID, and returns the signing certificate.</p>
getSigningCertificate :: forall eff. GetSigningCertificateRequest -> Aff (exception :: EXCEPTION | eff) GetSigningCertificateResponse
getSigningCertificate = Request.request serviceName "getSigningCertificate" 


-- | <p>Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app <code>clientId</code> will be <code>ALL</code>), then that is returned. If nothing is present, then an empty shape is returned.</p>
getUICustomization :: forall eff. GetUICustomizationRequest -> Aff (exception :: EXCEPTION | eff) GetUICustomizationResponse
getUICustomization = Request.request serviceName "getUICustomization" 


-- | <p>Gets the user attributes and metadata for a user.</p>
getUser :: forall eff. GetUserRequest -> Aff (exception :: EXCEPTION | eff) GetUserResponse
getUser = Request.request serviceName "getUser" 


-- | <p>Gets the user attribute verification code for the specified attribute name.</p>
getUserAttributeVerificationCode :: forall eff. GetUserAttributeVerificationCodeRequest -> Aff (exception :: EXCEPTION | eff) GetUserAttributeVerificationCodeResponse
getUserAttributeVerificationCode = Request.request serviceName "getUserAttributeVerificationCode" 


-- | <p>Gets the user pool multi-factor authentication (MFA) configuration.</p>
getUserPoolMfaConfig :: forall eff. GetUserPoolMfaConfigRequest -> Aff (exception :: EXCEPTION | eff) GetUserPoolMfaConfigResponse
getUserPoolMfaConfig = Request.request serviceName "getUserPoolMfaConfig" 


-- | <p>Signs out users from all devices.</p>
globalSignOut :: forall eff. GlobalSignOutRequest -> Aff (exception :: EXCEPTION | eff) GlobalSignOutResponse
globalSignOut = Request.request serviceName "globalSignOut" 


-- | <p>Initiates the authentication flow.</p>
initiateAuth :: forall eff. InitiateAuthRequest -> Aff (exception :: EXCEPTION | eff) InitiateAuthResponse
initiateAuth = Request.request serviceName "initiateAuth" 


-- | <p>Lists the devices.</p>
listDevices :: forall eff. ListDevicesRequest -> Aff (exception :: EXCEPTION | eff) ListDevicesResponse
listDevices = Request.request serviceName "listDevices" 


-- | <p>Lists the groups associated with a user pool.</p> <p>Requires developer credentials.</p>
listGroups :: forall eff. ListGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListGroupsResponse
listGroups = Request.request serviceName "listGroups" 


-- | <p>Lists information about all identity providers for a user pool.</p>
listIdentityProviders :: forall eff. ListIdentityProvidersRequest -> Aff (exception :: EXCEPTION | eff) ListIdentityProvidersResponse
listIdentityProviders = Request.request serviceName "listIdentityProviders" 


-- | <p>Lists the resource servers for a user pool.</p>
listResourceServers :: forall eff. ListResourceServersRequest -> Aff (exception :: EXCEPTION | eff) ListResourceServersResponse
listResourceServers = Request.request serviceName "listResourceServers" 


-- | <p>Lists the user import jobs.</p>
listUserImportJobs :: forall eff. ListUserImportJobsRequest -> Aff (exception :: EXCEPTION | eff) ListUserImportJobsResponse
listUserImportJobs = Request.request serviceName "listUserImportJobs" 


-- | <p>Lists the clients that have been created for the specified user pool.</p>
listUserPoolClients :: forall eff. ListUserPoolClientsRequest -> Aff (exception :: EXCEPTION | eff) ListUserPoolClientsResponse
listUserPoolClients = Request.request serviceName "listUserPoolClients" 


-- | <p>Lists the user pools associated with an AWS account.</p>
listUserPools :: forall eff. ListUserPoolsRequest -> Aff (exception :: EXCEPTION | eff) ListUserPoolsResponse
listUserPools = Request.request serviceName "listUserPools" 


-- | <p>Lists the users in the Amazon Cognito user pool.</p>
listUsers :: forall eff. ListUsersRequest -> Aff (exception :: EXCEPTION | eff) ListUsersResponse
listUsers = Request.request serviceName "listUsers" 


-- | <p>Lists the users in the specified group.</p> <p>Requires developer credentials.</p>
listUsersInGroup :: forall eff. ListUsersInGroupRequest -> Aff (exception :: EXCEPTION | eff) ListUsersInGroupResponse
listUsersInGroup = Request.request serviceName "listUsersInGroup" 


-- | <p>Resends the confirmation (for confirmation of registration) to a specific user in the user pool.</p>
resendConfirmationCode :: forall eff. ResendConfirmationCodeRequest -> Aff (exception :: EXCEPTION | eff) ResendConfirmationCodeResponse
resendConfirmationCode = Request.request serviceName "resendConfirmationCode" 


-- | <p>Responds to the authentication challenge.</p>
respondToAuthChallenge :: forall eff. RespondToAuthChallengeRequest -> Aff (exception :: EXCEPTION | eff) RespondToAuthChallengeResponse
respondToAuthChallenge = Request.request serviceName "respondToAuthChallenge" 


-- | <p>Configures actions on detected risks. To delete the risk configuration for <code>UserPoolId</code> or <code>ClientId</code>, pass null values for all four configuration types.</p> <p>To enable Amazon Cognito advanced security features, update the user pool to include the <code>UserPoolAddOns</code> key<code>AdvancedSecurityMode</code>.</p> <p>See .</p>
setRiskConfiguration :: forall eff. SetRiskConfigurationRequest -> Aff (exception :: EXCEPTION | eff) SetRiskConfigurationResponse
setRiskConfiguration = Request.request serviceName "setRiskConfiguration" 


-- | <p>Sets the UI customization information for a user pool's built-in app UI.</p> <p>You can specify app UI customization settings for a single client (with a specific <code>clientId</code>) or for all clients (by setting the <code>clientId</code> to <code>ALL</code>). If you specify <code>ALL</code>, the default configuration will be used for every client that has no UI customization set previously. If you specify UI customization settings for a particular client, it will no longer fall back to the <code>ALL</code> configuration. </p> <note> <p>To use this API, your user pool must have a domain associated with it. Otherwise, there is no place to host the app's pages, and the service will throw an error.</p> </note>
setUICustomization :: forall eff. SetUICustomizationRequest -> Aff (exception :: EXCEPTION | eff) SetUICustomizationResponse
setUICustomization = Request.request serviceName "setUICustomization" 


-- | <p>Set the user's multi-factor authentication (MFA) method preference.</p>
setUserMFAPreference :: forall eff. SetUserMFAPreferenceRequest -> Aff (exception :: EXCEPTION | eff) SetUserMFAPreferenceResponse
setUserMFAPreference = Request.request serviceName "setUserMFAPreference" 


-- | <p>Set the user pool MFA configuration.</p>
setUserPoolMfaConfig :: forall eff. SetUserPoolMfaConfigRequest -> Aff (exception :: EXCEPTION | eff) SetUserPoolMfaConfigResponse
setUserPoolMfaConfig = Request.request serviceName "setUserPoolMfaConfig" 


-- | <p>Sets the user settings like multi-factor authentication (MFA). If MFA is to be removed for a particular attribute pass the attribute with code delivery as null. If null list is passed, all MFA options are removed.</p>
setUserSettings :: forall eff. SetUserSettingsRequest -> Aff (exception :: EXCEPTION | eff) SetUserSettingsResponse
setUserSettings = Request.request serviceName "setUserSettings" 


-- | <p>Registers the user in the specified user pool and creates a user name, password, and user attributes.</p>
signUp :: forall eff. SignUpRequest -> Aff (exception :: EXCEPTION | eff) SignUpResponse
signUp = Request.request serviceName "signUp" 


-- | <p>Starts the user import.</p>
startUserImportJob :: forall eff. StartUserImportJobRequest -> Aff (exception :: EXCEPTION | eff) StartUserImportJobResponse
startUserImportJob = Request.request serviceName "startUserImportJob" 


-- | <p>Stops the user import job.</p>
stopUserImportJob :: forall eff. StopUserImportJobRequest -> Aff (exception :: EXCEPTION | eff) StopUserImportJobResponse
stopUserImportJob = Request.request serviceName "stopUserImportJob" 


-- | <p>Provides the feedback for an authentication event whether it was from a valid user or not. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.</p>
updateAuthEventFeedback :: forall eff. UpdateAuthEventFeedbackRequest -> Aff (exception :: EXCEPTION | eff) UpdateAuthEventFeedbackResponse
updateAuthEventFeedback = Request.request serviceName "updateAuthEventFeedback" 


-- | <p>Updates the device status.</p>
updateDeviceStatus :: forall eff. UpdateDeviceStatusRequest -> Aff (exception :: EXCEPTION | eff) UpdateDeviceStatusResponse
updateDeviceStatus = Request.request serviceName "updateDeviceStatus" 


-- | <p>Updates the specified group with the specified attributes.</p> <p>Requires developer credentials.</p>
updateGroup :: forall eff. UpdateGroupRequest -> Aff (exception :: EXCEPTION | eff) UpdateGroupResponse
updateGroup = Request.request serviceName "updateGroup" 


-- | <p>Updates identity provider information for a user pool.</p>
updateIdentityProvider :: forall eff. UpdateIdentityProviderRequest -> Aff (exception :: EXCEPTION | eff) UpdateIdentityProviderResponse
updateIdentityProvider = Request.request serviceName "updateIdentityProvider" 


-- | <p>Updates the name and scopes of resource server. All other fields are read-only.</p>
updateResourceServer :: forall eff. UpdateResourceServerRequest -> Aff (exception :: EXCEPTION | eff) UpdateResourceServerResponse
updateResourceServer = Request.request serviceName "updateResourceServer" 


-- | <p>Allows a user to update a specific attribute (one at a time).</p>
updateUserAttributes :: forall eff. UpdateUserAttributesRequest -> Aff (exception :: EXCEPTION | eff) UpdateUserAttributesResponse
updateUserAttributes = Request.request serviceName "updateUserAttributes" 


-- | <p>Updates the specified user pool with the specified attributes.</p>
updateUserPool :: forall eff. UpdateUserPoolRequest -> Aff (exception :: EXCEPTION | eff) UpdateUserPoolResponse
updateUserPool = Request.request serviceName "updateUserPool" 


-- | <p>Allows the developer to update the specified user pool client and password policy.</p>
updateUserPoolClient :: forall eff. UpdateUserPoolClientRequest -> Aff (exception :: EXCEPTION | eff) UpdateUserPoolClientResponse
updateUserPoolClient = Request.request serviceName "updateUserPoolClient" 


-- | <p>Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful,</p>
verifySoftwareToken :: forall eff. VerifySoftwareTokenRequest -> Aff (exception :: EXCEPTION | eff) VerifySoftwareTokenResponse
verifySoftwareToken = Request.request serviceName "verifySoftwareToken" 


-- | <p>Verifies the specified user attributes in the user pool.</p>
verifyUserAttribute :: forall eff. VerifyUserAttributeRequest -> Aff (exception :: EXCEPTION | eff) VerifyUserAttributeResponse
verifyUserAttribute = Request.request serviceName "verifyUserAttribute" 


newtype AWSAccountIdType = AWSAccountIdType String
derive instance newtypeAWSAccountIdType :: Newtype AWSAccountIdType _
derive instance repGenericAWSAccountIdType :: Generic AWSAccountIdType _
instance showAWSAccountIdType :: Show AWSAccountIdType where
  show = genericShow
instance decodeAWSAccountIdType :: Decode AWSAccountIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSAccountIdType :: Encode AWSAccountIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountTakeoverActionNotifyType = AccountTakeoverActionNotifyType Boolean
derive instance newtypeAccountTakeoverActionNotifyType :: Newtype AccountTakeoverActionNotifyType _
derive instance repGenericAccountTakeoverActionNotifyType :: Generic AccountTakeoverActionNotifyType _
instance showAccountTakeoverActionNotifyType :: Show AccountTakeoverActionNotifyType where
  show = genericShow
instance decodeAccountTakeoverActionNotifyType :: Decode AccountTakeoverActionNotifyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountTakeoverActionNotifyType :: Encode AccountTakeoverActionNotifyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Account takeover action type.</p>
newtype AccountTakeoverActionType = AccountTakeoverActionType 
  { "Notify" :: (AccountTakeoverActionNotifyType)
  , "EventAction" :: (AccountTakeoverEventActionType)
  }
derive instance newtypeAccountTakeoverActionType :: Newtype AccountTakeoverActionType _
derive instance repGenericAccountTakeoverActionType :: Generic AccountTakeoverActionType _
instance showAccountTakeoverActionType :: Show AccountTakeoverActionType where
  show = genericShow
instance decodeAccountTakeoverActionType :: Decode AccountTakeoverActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountTakeoverActionType :: Encode AccountTakeoverActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Account takeover actions type.</p>
newtype AccountTakeoverActionsType = AccountTakeoverActionsType 
  { "LowAction" :: NullOrUndefined.NullOrUndefined (AccountTakeoverActionType)
  , "MediumAction" :: NullOrUndefined.NullOrUndefined (AccountTakeoverActionType)
  , "HighAction" :: NullOrUndefined.NullOrUndefined (AccountTakeoverActionType)
  }
derive instance newtypeAccountTakeoverActionsType :: Newtype AccountTakeoverActionsType _
derive instance repGenericAccountTakeoverActionsType :: Generic AccountTakeoverActionsType _
instance showAccountTakeoverActionsType :: Show AccountTakeoverActionsType where
  show = genericShow
instance decodeAccountTakeoverActionsType :: Decode AccountTakeoverActionsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountTakeoverActionsType :: Encode AccountTakeoverActionsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType String
derive instance newtypeAccountTakeoverEventActionType :: Newtype AccountTakeoverEventActionType _
derive instance repGenericAccountTakeoverEventActionType :: Generic AccountTakeoverEventActionType _
instance showAccountTakeoverEventActionType :: Show AccountTakeoverEventActionType where
  show = genericShow
instance decodeAccountTakeoverEventActionType :: Decode AccountTakeoverEventActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountTakeoverEventActionType :: Encode AccountTakeoverEventActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.</p>
newtype AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType 
  { "NotifyConfiguration" :: NullOrUndefined.NullOrUndefined (NotifyConfigurationType)
  , "Actions" :: (AccountTakeoverActionsType)
  }
derive instance newtypeAccountTakeoverRiskConfigurationType :: Newtype AccountTakeoverRiskConfigurationType _
derive instance repGenericAccountTakeoverRiskConfigurationType :: Generic AccountTakeoverRiskConfigurationType _
instance showAccountTakeoverRiskConfigurationType :: Show AccountTakeoverRiskConfigurationType where
  show = genericShow
instance decodeAccountTakeoverRiskConfigurationType :: Decode AccountTakeoverRiskConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountTakeoverRiskConfigurationType :: Encode AccountTakeoverRiskConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to add custom attributes.</p>
newtype AddCustomAttributesRequest = AddCustomAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "CustomAttributes" :: (CustomAttributesListType)
  }
derive instance newtypeAddCustomAttributesRequest :: Newtype AddCustomAttributesRequest _
derive instance repGenericAddCustomAttributesRequest :: Generic AddCustomAttributesRequest _
instance showAddCustomAttributesRequest :: Show AddCustomAttributesRequest where
  show = genericShow
instance decodeAddCustomAttributesRequest :: Decode AddCustomAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddCustomAttributesRequest :: Encode AddCustomAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to add custom attributes.</p>
newtype AddCustomAttributesResponse = AddCustomAttributesResponse Types.NoArguments
derive instance newtypeAddCustomAttributesResponse :: Newtype AddCustomAttributesResponse _
derive instance repGenericAddCustomAttributesResponse :: Generic AddCustomAttributesResponse _
instance showAddCustomAttributesResponse :: Show AddCustomAttributesResponse where
  show = genericShow
instance decodeAddCustomAttributesResponse :: Decode AddCustomAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddCustomAttributesResponse :: Encode AddCustomAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminAddUserToGroupRequest = AdminAddUserToGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "GroupName" :: (GroupNameType)
  }
derive instance newtypeAdminAddUserToGroupRequest :: Newtype AdminAddUserToGroupRequest _
derive instance repGenericAdminAddUserToGroupRequest :: Generic AdminAddUserToGroupRequest _
instance showAdminAddUserToGroupRequest :: Show AdminAddUserToGroupRequest where
  show = genericShow
instance decodeAdminAddUserToGroupRequest :: Decode AdminAddUserToGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminAddUserToGroupRequest :: Encode AdminAddUserToGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to confirm user registration.</p>
newtype AdminConfirmSignUpRequest = AdminConfirmSignUpRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminConfirmSignUpRequest :: Newtype AdminConfirmSignUpRequest _
derive instance repGenericAdminConfirmSignUpRequest :: Generic AdminConfirmSignUpRequest _
instance showAdminConfirmSignUpRequest :: Show AdminConfirmSignUpRequest where
  show = genericShow
instance decodeAdminConfirmSignUpRequest :: Decode AdminConfirmSignUpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminConfirmSignUpRequest :: Encode AdminConfirmSignUpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to confirm registration.</p>
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse Types.NoArguments
derive instance newtypeAdminConfirmSignUpResponse :: Newtype AdminConfirmSignUpResponse _
derive instance repGenericAdminConfirmSignUpResponse :: Generic AdminConfirmSignUpResponse _
instance showAdminConfirmSignUpResponse :: Show AdminConfirmSignUpResponse where
  show = genericShow
instance decodeAdminConfirmSignUpResponse :: Decode AdminConfirmSignUpResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminConfirmSignUpResponse :: Encode AdminConfirmSignUpResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration for creating a new user profile.</p>
newtype AdminCreateUserConfigType = AdminCreateUserConfigType 
  { "AllowAdminCreateUserOnly" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "UnusedAccountValidityDays" :: NullOrUndefined.NullOrUndefined (AdminCreateUserUnusedAccountValidityDaysType)
  , "InviteMessageTemplate" :: NullOrUndefined.NullOrUndefined (MessageTemplateType)
  }
derive instance newtypeAdminCreateUserConfigType :: Newtype AdminCreateUserConfigType _
derive instance repGenericAdminCreateUserConfigType :: Generic AdminCreateUserConfigType _
instance showAdminCreateUserConfigType :: Show AdminCreateUserConfigType where
  show = genericShow
instance decodeAdminCreateUserConfigType :: Decode AdminCreateUserConfigType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminCreateUserConfigType :: Encode AdminCreateUserConfigType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to create a user in the specified user pool.</p>
newtype AdminCreateUserRequest = AdminCreateUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributes" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "ValidationData" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "TemporaryPassword" :: NullOrUndefined.NullOrUndefined (PasswordType)
  , "ForceAliasCreation" :: NullOrUndefined.NullOrUndefined (ForceAliasCreation)
  , "MessageAction" :: NullOrUndefined.NullOrUndefined (MessageActionType)
  , "DesiredDeliveryMediums" :: NullOrUndefined.NullOrUndefined (DeliveryMediumListType)
  }
derive instance newtypeAdminCreateUserRequest :: Newtype AdminCreateUserRequest _
derive instance repGenericAdminCreateUserRequest :: Generic AdminCreateUserRequest _
instance showAdminCreateUserRequest :: Show AdminCreateUserRequest where
  show = genericShow
instance decodeAdminCreateUserRequest :: Decode AdminCreateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminCreateUserRequest :: Encode AdminCreateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to create the user.</p>
newtype AdminCreateUserResponse = AdminCreateUserResponse 
  { "User" :: NullOrUndefined.NullOrUndefined (UserType)
  }
derive instance newtypeAdminCreateUserResponse :: Newtype AdminCreateUserResponse _
derive instance repGenericAdminCreateUserResponse :: Generic AdminCreateUserResponse _
instance showAdminCreateUserResponse :: Show AdminCreateUserResponse where
  show = genericShow
instance decodeAdminCreateUserResponse :: Decode AdminCreateUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminCreateUserResponse :: Encode AdminCreateUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminCreateUserUnusedAccountValidityDaysType = AdminCreateUserUnusedAccountValidityDaysType Int
derive instance newtypeAdminCreateUserUnusedAccountValidityDaysType :: Newtype AdminCreateUserUnusedAccountValidityDaysType _
derive instance repGenericAdminCreateUserUnusedAccountValidityDaysType :: Generic AdminCreateUserUnusedAccountValidityDaysType _
instance showAdminCreateUserUnusedAccountValidityDaysType :: Show AdminCreateUserUnusedAccountValidityDaysType where
  show = genericShow
instance decodeAdminCreateUserUnusedAccountValidityDaysType :: Decode AdminCreateUserUnusedAccountValidityDaysType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminCreateUserUnusedAccountValidityDaysType :: Encode AdminCreateUserUnusedAccountValidityDaysType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete user attributes as an administrator.</p>
newtype AdminDeleteUserAttributesRequest = AdminDeleteUserAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributeNames" :: (AttributeNameListType)
  }
derive instance newtypeAdminDeleteUserAttributesRequest :: Newtype AdminDeleteUserAttributesRequest _
derive instance repGenericAdminDeleteUserAttributesRequest :: Generic AdminDeleteUserAttributesRequest _
instance showAdminDeleteUserAttributesRequest :: Show AdminDeleteUserAttributesRequest where
  show = genericShow
instance decodeAdminDeleteUserAttributesRequest :: Decode AdminDeleteUserAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDeleteUserAttributesRequest :: Encode AdminDeleteUserAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response received from the server for a request to delete user attributes.</p>
newtype AdminDeleteUserAttributesResponse = AdminDeleteUserAttributesResponse Types.NoArguments
derive instance newtypeAdminDeleteUserAttributesResponse :: Newtype AdminDeleteUserAttributesResponse _
derive instance repGenericAdminDeleteUserAttributesResponse :: Generic AdminDeleteUserAttributesResponse _
instance showAdminDeleteUserAttributesResponse :: Show AdminDeleteUserAttributesResponse where
  show = genericShow
instance decodeAdminDeleteUserAttributesResponse :: Decode AdminDeleteUserAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDeleteUserAttributesResponse :: Encode AdminDeleteUserAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete a user as an administrator.</p>
newtype AdminDeleteUserRequest = AdminDeleteUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminDeleteUserRequest :: Newtype AdminDeleteUserRequest _
derive instance repGenericAdminDeleteUserRequest :: Generic AdminDeleteUserRequest _
instance showAdminDeleteUserRequest :: Show AdminDeleteUserRequest where
  show = genericShow
instance decodeAdminDeleteUserRequest :: Decode AdminDeleteUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDeleteUserRequest :: Encode AdminDeleteUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminDisableProviderForUserRequest = AdminDisableProviderForUserRequest 
  { "UserPoolId" :: (StringType)
  , "User" :: (ProviderUserIdentifierType)
  }
derive instance newtypeAdminDisableProviderForUserRequest :: Newtype AdminDisableProviderForUserRequest _
derive instance repGenericAdminDisableProviderForUserRequest :: Generic AdminDisableProviderForUserRequest _
instance showAdminDisableProviderForUserRequest :: Show AdminDisableProviderForUserRequest where
  show = genericShow
instance decodeAdminDisableProviderForUserRequest :: Decode AdminDisableProviderForUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDisableProviderForUserRequest :: Encode AdminDisableProviderForUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse Types.NoArguments
derive instance newtypeAdminDisableProviderForUserResponse :: Newtype AdminDisableProviderForUserResponse _
derive instance repGenericAdminDisableProviderForUserResponse :: Generic AdminDisableProviderForUserResponse _
instance showAdminDisableProviderForUserResponse :: Show AdminDisableProviderForUserResponse where
  show = genericShow
instance decodeAdminDisableProviderForUserResponse :: Decode AdminDisableProviderForUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDisableProviderForUserResponse :: Encode AdminDisableProviderForUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to disable any user as an administrator.</p>
newtype AdminDisableUserRequest = AdminDisableUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminDisableUserRequest :: Newtype AdminDisableUserRequest _
derive instance repGenericAdminDisableUserRequest :: Generic AdminDisableUserRequest _
instance showAdminDisableUserRequest :: Show AdminDisableUserRequest where
  show = genericShow
instance decodeAdminDisableUserRequest :: Decode AdminDisableUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDisableUserRequest :: Encode AdminDisableUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response received from the server to disable the user as an administrator.</p>
newtype AdminDisableUserResponse = AdminDisableUserResponse Types.NoArguments
derive instance newtypeAdminDisableUserResponse :: Newtype AdminDisableUserResponse _
derive instance repGenericAdminDisableUserResponse :: Generic AdminDisableUserResponse _
instance showAdminDisableUserResponse :: Show AdminDisableUserResponse where
  show = genericShow
instance decodeAdminDisableUserResponse :: Decode AdminDisableUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminDisableUserResponse :: Encode AdminDisableUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request that enables the user as an administrator.</p>
newtype AdminEnableUserRequest = AdminEnableUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminEnableUserRequest :: Newtype AdminEnableUserRequest _
derive instance repGenericAdminEnableUserRequest :: Generic AdminEnableUserRequest _
instance showAdminEnableUserRequest :: Show AdminEnableUserRequest where
  show = genericShow
instance decodeAdminEnableUserRequest :: Decode AdminEnableUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminEnableUserRequest :: Encode AdminEnableUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to enable a user as an administrator.</p>
newtype AdminEnableUserResponse = AdminEnableUserResponse Types.NoArguments
derive instance newtypeAdminEnableUserResponse :: Newtype AdminEnableUserResponse _
derive instance repGenericAdminEnableUserResponse :: Generic AdminEnableUserResponse _
instance showAdminEnableUserResponse :: Show AdminEnableUserResponse where
  show = genericShow
instance decodeAdminEnableUserResponse :: Decode AdminEnableUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminEnableUserResponse :: Encode AdminEnableUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Sends the forgot device request, as an administrator.</p>
newtype AdminForgetDeviceRequest = AdminForgetDeviceRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "DeviceKey" :: (DeviceKeyType)
  }
derive instance newtypeAdminForgetDeviceRequest :: Newtype AdminForgetDeviceRequest _
derive instance repGenericAdminForgetDeviceRequest :: Generic AdminForgetDeviceRequest _
instance showAdminForgetDeviceRequest :: Show AdminForgetDeviceRequest where
  show = genericShow
instance decodeAdminForgetDeviceRequest :: Decode AdminForgetDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminForgetDeviceRequest :: Encode AdminForgetDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get the device, as an administrator.</p>
newtype AdminGetDeviceRequest = AdminGetDeviceRequest 
  { "DeviceKey" :: (DeviceKeyType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminGetDeviceRequest :: Newtype AdminGetDeviceRequest _
derive instance repGenericAdminGetDeviceRequest :: Generic AdminGetDeviceRequest _
instance showAdminGetDeviceRequest :: Show AdminGetDeviceRequest where
  show = genericShow
instance decodeAdminGetDeviceRequest :: Decode AdminGetDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminGetDeviceRequest :: Encode AdminGetDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the device response, as an administrator.</p>
newtype AdminGetDeviceResponse = AdminGetDeviceResponse 
  { "Device" :: (DeviceType)
  }
derive instance newtypeAdminGetDeviceResponse :: Newtype AdminGetDeviceResponse _
derive instance repGenericAdminGetDeviceResponse :: Generic AdminGetDeviceResponse _
instance showAdminGetDeviceResponse :: Show AdminGetDeviceResponse where
  show = genericShow
instance decodeAdminGetDeviceResponse :: Decode AdminGetDeviceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminGetDeviceResponse :: Encode AdminGetDeviceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get the specified user as an administrator.</p>
newtype AdminGetUserRequest = AdminGetUserRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminGetUserRequest :: Newtype AdminGetUserRequest _
derive instance repGenericAdminGetUserRequest :: Generic AdminGetUserRequest _
instance showAdminGetUserRequest :: Show AdminGetUserRequest where
  show = genericShow
instance decodeAdminGetUserRequest :: Decode AdminGetUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminGetUserRequest :: Encode AdminGetUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server from the request to get the specified user as an administrator.</p>
newtype AdminGetUserResponse = AdminGetUserResponse 
  { "Username" :: (UsernameType)
  , "UserAttributes" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "UserCreateDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "UserLastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "UserStatus" :: NullOrUndefined.NullOrUndefined (UserStatusType)
  , "MFAOptions" :: NullOrUndefined.NullOrUndefined (MFAOptionListType)
  , "PreferredMfaSetting" :: NullOrUndefined.NullOrUndefined (StringType)
  , "UserMFASettingList" :: NullOrUndefined.NullOrUndefined (UserMFASettingListType)
  }
derive instance newtypeAdminGetUserResponse :: Newtype AdminGetUserResponse _
derive instance repGenericAdminGetUserResponse :: Generic AdminGetUserResponse _
instance showAdminGetUserResponse :: Show AdminGetUserResponse where
  show = genericShow
instance decodeAdminGetUserResponse :: Decode AdminGetUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminGetUserResponse :: Encode AdminGetUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the authorization request, as an administrator.</p>
newtype AdminInitiateAuthRequest = AdminInitiateAuthRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "AuthFlow" :: (AuthFlowType)
  , "AuthParameters" :: NullOrUndefined.NullOrUndefined (AuthParametersType)
  , "ClientMetadata" :: NullOrUndefined.NullOrUndefined (ClientMetadataType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "ContextData" :: NullOrUndefined.NullOrUndefined (ContextDataType)
  }
derive instance newtypeAdminInitiateAuthRequest :: Newtype AdminInitiateAuthRequest _
derive instance repGenericAdminInitiateAuthRequest :: Generic AdminInitiateAuthRequest _
instance showAdminInitiateAuthRequest :: Show AdminInitiateAuthRequest where
  show = genericShow
instance decodeAdminInitiateAuthRequest :: Decode AdminInitiateAuthRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminInitiateAuthRequest :: Encode AdminInitiateAuthRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the authentication response, as an administrator.</p>
newtype AdminInitiateAuthResponse = AdminInitiateAuthResponse 
  { "ChallengeName" :: NullOrUndefined.NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined.NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined.NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeAdminInitiateAuthResponse :: Newtype AdminInitiateAuthResponse _
derive instance repGenericAdminInitiateAuthResponse :: Generic AdminInitiateAuthResponse _
instance showAdminInitiateAuthResponse :: Show AdminInitiateAuthResponse where
  show = genericShow
instance decodeAdminInitiateAuthResponse :: Decode AdminInitiateAuthResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminInitiateAuthResponse :: Encode AdminInitiateAuthResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminLinkProviderForUserRequest = AdminLinkProviderForUserRequest 
  { "UserPoolId" :: (StringType)
  , "DestinationUser" :: (ProviderUserIdentifierType)
  , "SourceUser" :: (ProviderUserIdentifierType)
  }
derive instance newtypeAdminLinkProviderForUserRequest :: Newtype AdminLinkProviderForUserRequest _
derive instance repGenericAdminLinkProviderForUserRequest :: Generic AdminLinkProviderForUserRequest _
instance showAdminLinkProviderForUserRequest :: Show AdminLinkProviderForUserRequest where
  show = genericShow
instance decodeAdminLinkProviderForUserRequest :: Decode AdminLinkProviderForUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminLinkProviderForUserRequest :: Encode AdminLinkProviderForUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminLinkProviderForUserResponse = AdminLinkProviderForUserResponse Types.NoArguments
derive instance newtypeAdminLinkProviderForUserResponse :: Newtype AdminLinkProviderForUserResponse _
derive instance repGenericAdminLinkProviderForUserResponse :: Generic AdminLinkProviderForUserResponse _
instance showAdminLinkProviderForUserResponse :: Show AdminLinkProviderForUserResponse where
  show = genericShow
instance decodeAdminLinkProviderForUserResponse :: Decode AdminLinkProviderForUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminLinkProviderForUserResponse :: Encode AdminLinkProviderForUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list devices, as an administrator.</p>
newtype AdminListDevicesRequest = AdminListDevicesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeAdminListDevicesRequest :: Newtype AdminListDevicesRequest _
derive instance repGenericAdminListDevicesRequest :: Generic AdminListDevicesRequest _
instance showAdminListDevicesRequest :: Show AdminListDevicesRequest where
  show = genericShow
instance decodeAdminListDevicesRequest :: Decode AdminListDevicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListDevicesRequest :: Encode AdminListDevicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Lists the device's response, as an administrator.</p>
newtype AdminListDevicesResponse = AdminListDevicesResponse 
  { "Devices" :: NullOrUndefined.NullOrUndefined (DeviceListType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeAdminListDevicesResponse :: Newtype AdminListDevicesResponse _
derive instance repGenericAdminListDevicesResponse :: Generic AdminListDevicesResponse _
instance showAdminListDevicesResponse :: Show AdminListDevicesResponse where
  show = genericShow
instance decodeAdminListDevicesResponse :: Decode AdminListDevicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListDevicesResponse :: Encode AdminListDevicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminListGroupsForUserRequest = AdminListGroupsForUserRequest 
  { "Username" :: (UsernameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListGroupsForUserRequest :: Newtype AdminListGroupsForUserRequest _
derive instance repGenericAdminListGroupsForUserRequest :: Generic AdminListGroupsForUserRequest _
instance showAdminListGroupsForUserRequest :: Show AdminListGroupsForUserRequest where
  show = genericShow
instance decodeAdminListGroupsForUserRequest :: Decode AdminListGroupsForUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListGroupsForUserRequest :: Encode AdminListGroupsForUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminListGroupsForUserResponse = AdminListGroupsForUserResponse 
  { "Groups" :: NullOrUndefined.NullOrUndefined (GroupListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListGroupsForUserResponse :: Newtype AdminListGroupsForUserResponse _
derive instance repGenericAdminListGroupsForUserResponse :: Generic AdminListGroupsForUserResponse _
instance showAdminListGroupsForUserResponse :: Show AdminListGroupsForUserResponse where
  show = genericShow
instance decodeAdminListGroupsForUserResponse :: Decode AdminListGroupsForUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListGroupsForUserResponse :: Encode AdminListGroupsForUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminListUserAuthEventsRequest = AdminListUserAuthEventsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListUserAuthEventsRequest :: Newtype AdminListUserAuthEventsRequest _
derive instance repGenericAdminListUserAuthEventsRequest :: Generic AdminListUserAuthEventsRequest _
instance showAdminListUserAuthEventsRequest :: Show AdminListUserAuthEventsRequest where
  show = genericShow
instance decodeAdminListUserAuthEventsRequest :: Decode AdminListUserAuthEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListUserAuthEventsRequest :: Encode AdminListUserAuthEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse 
  { "AuthEvents" :: NullOrUndefined.NullOrUndefined (AuthEventsType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeAdminListUserAuthEventsResponse :: Newtype AdminListUserAuthEventsResponse _
derive instance repGenericAdminListUserAuthEventsResponse :: Generic AdminListUserAuthEventsResponse _
instance showAdminListUserAuthEventsResponse :: Show AdminListUserAuthEventsResponse where
  show = genericShow
instance decodeAdminListUserAuthEventsResponse :: Decode AdminListUserAuthEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminListUserAuthEventsResponse :: Encode AdminListUserAuthEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminRemoveUserFromGroupRequest = AdminRemoveUserFromGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "GroupName" :: (GroupNameType)
  }
derive instance newtypeAdminRemoveUserFromGroupRequest :: Newtype AdminRemoveUserFromGroupRequest _
derive instance repGenericAdminRemoveUserFromGroupRequest :: Generic AdminRemoveUserFromGroupRequest _
instance showAdminRemoveUserFromGroupRequest :: Show AdminRemoveUserFromGroupRequest where
  show = genericShow
instance decodeAdminRemoveUserFromGroupRequest :: Decode AdminRemoveUserFromGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminRemoveUserFromGroupRequest :: Encode AdminRemoveUserFromGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to reset a user's password as an administrator.</p>
newtype AdminResetUserPasswordRequest = AdminResetUserPasswordRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminResetUserPasswordRequest :: Newtype AdminResetUserPasswordRequest _
derive instance repGenericAdminResetUserPasswordRequest :: Generic AdminResetUserPasswordRequest _
instance showAdminResetUserPasswordRequest :: Show AdminResetUserPasswordRequest where
  show = genericShow
instance decodeAdminResetUserPasswordRequest :: Decode AdminResetUserPasswordRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminResetUserPasswordRequest :: Encode AdminResetUserPasswordRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to reset a user password as an administrator.</p>
newtype AdminResetUserPasswordResponse = AdminResetUserPasswordResponse Types.NoArguments
derive instance newtypeAdminResetUserPasswordResponse :: Newtype AdminResetUserPasswordResponse _
derive instance repGenericAdminResetUserPasswordResponse :: Generic AdminResetUserPasswordResponse _
instance showAdminResetUserPasswordResponse :: Show AdminResetUserPasswordResponse where
  show = genericShow
instance decodeAdminResetUserPasswordResponse :: Decode AdminResetUserPasswordResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminResetUserPasswordResponse :: Encode AdminResetUserPasswordResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to respond to the authentication challenge, as an administrator.</p>
newtype AdminRespondToAuthChallengeRequest = AdminRespondToAuthChallengeRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "ChallengeName" :: (ChallengeNameType)
  , "ChallengeResponses" :: NullOrUndefined.NullOrUndefined (ChallengeResponsesType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "ContextData" :: NullOrUndefined.NullOrUndefined (ContextDataType)
  }
derive instance newtypeAdminRespondToAuthChallengeRequest :: Newtype AdminRespondToAuthChallengeRequest _
derive instance repGenericAdminRespondToAuthChallengeRequest :: Generic AdminRespondToAuthChallengeRequest _
instance showAdminRespondToAuthChallengeRequest :: Show AdminRespondToAuthChallengeRequest where
  show = genericShow
instance decodeAdminRespondToAuthChallengeRequest :: Decode AdminRespondToAuthChallengeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminRespondToAuthChallengeRequest :: Encode AdminRespondToAuthChallengeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Responds to the authentication challenge, as an administrator.</p>
newtype AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse 
  { "ChallengeName" :: NullOrUndefined.NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined.NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined.NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeAdminRespondToAuthChallengeResponse :: Newtype AdminRespondToAuthChallengeResponse _
derive instance repGenericAdminRespondToAuthChallengeResponse :: Generic AdminRespondToAuthChallengeResponse _
instance showAdminRespondToAuthChallengeResponse :: Show AdminRespondToAuthChallengeResponse where
  show = genericShow
instance decodeAdminRespondToAuthChallengeResponse :: Decode AdminRespondToAuthChallengeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminRespondToAuthChallengeResponse :: Encode AdminRespondToAuthChallengeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminSetUserMFAPreferenceRequest = AdminSetUserMFAPreferenceRequest 
  { "SMSMfaSettings" :: NullOrUndefined.NullOrUndefined (SMSMfaSettingsType)
  , "SoftwareTokenMfaSettings" :: NullOrUndefined.NullOrUndefined (SoftwareTokenMfaSettingsType)
  , "Username" :: (UsernameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeAdminSetUserMFAPreferenceRequest :: Newtype AdminSetUserMFAPreferenceRequest _
derive instance repGenericAdminSetUserMFAPreferenceRequest :: Generic AdminSetUserMFAPreferenceRequest _
instance showAdminSetUserMFAPreferenceRequest :: Show AdminSetUserMFAPreferenceRequest where
  show = genericShow
instance decodeAdminSetUserMFAPreferenceRequest :: Decode AdminSetUserMFAPreferenceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminSetUserMFAPreferenceRequest :: Encode AdminSetUserMFAPreferenceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse Types.NoArguments
derive instance newtypeAdminSetUserMFAPreferenceResponse :: Newtype AdminSetUserMFAPreferenceResponse _
derive instance repGenericAdminSetUserMFAPreferenceResponse :: Generic AdminSetUserMFAPreferenceResponse _
instance showAdminSetUserMFAPreferenceResponse :: Show AdminSetUserMFAPreferenceResponse where
  show = genericShow
instance decodeAdminSetUserMFAPreferenceResponse :: Decode AdminSetUserMFAPreferenceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminSetUserMFAPreferenceResponse :: Encode AdminSetUserMFAPreferenceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to set user settings as an administrator.</p>
newtype AdminSetUserSettingsRequest = AdminSetUserSettingsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "MFAOptions" :: (MFAOptionListType)
  }
derive instance newtypeAdminSetUserSettingsRequest :: Newtype AdminSetUserSettingsRequest _
derive instance repGenericAdminSetUserSettingsRequest :: Generic AdminSetUserSettingsRequest _
instance showAdminSetUserSettingsRequest :: Show AdminSetUserSettingsRequest where
  show = genericShow
instance decodeAdminSetUserSettingsRequest :: Decode AdminSetUserSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminSetUserSettingsRequest :: Encode AdminSetUserSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to set user settings as an administrator.</p>
newtype AdminSetUserSettingsResponse = AdminSetUserSettingsResponse Types.NoArguments
derive instance newtypeAdminSetUserSettingsResponse :: Newtype AdminSetUserSettingsResponse _
derive instance repGenericAdminSetUserSettingsResponse :: Generic AdminSetUserSettingsResponse _
instance showAdminSetUserSettingsResponse :: Show AdminSetUserSettingsResponse where
  show = genericShow
instance decodeAdminSetUserSettingsResponse :: Decode AdminSetUserSettingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminSetUserSettingsResponse :: Encode AdminSetUserSettingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminUpdateAuthEventFeedbackRequest = AdminUpdateAuthEventFeedbackRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "EventId" :: (EventIdType)
  , "FeedbackValue" :: (FeedbackValueType)
  }
derive instance newtypeAdminUpdateAuthEventFeedbackRequest :: Newtype AdminUpdateAuthEventFeedbackRequest _
derive instance repGenericAdminUpdateAuthEventFeedbackRequest :: Generic AdminUpdateAuthEventFeedbackRequest _
instance showAdminUpdateAuthEventFeedbackRequest :: Show AdminUpdateAuthEventFeedbackRequest where
  show = genericShow
instance decodeAdminUpdateAuthEventFeedbackRequest :: Decode AdminUpdateAuthEventFeedbackRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateAuthEventFeedbackRequest :: Encode AdminUpdateAuthEventFeedbackRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse Types.NoArguments
derive instance newtypeAdminUpdateAuthEventFeedbackResponse :: Newtype AdminUpdateAuthEventFeedbackResponse _
derive instance repGenericAdminUpdateAuthEventFeedbackResponse :: Generic AdminUpdateAuthEventFeedbackResponse _
instance showAdminUpdateAuthEventFeedbackResponse :: Show AdminUpdateAuthEventFeedbackResponse where
  show = genericShow
instance decodeAdminUpdateAuthEventFeedbackResponse :: Decode AdminUpdateAuthEventFeedbackResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateAuthEventFeedbackResponse :: Encode AdminUpdateAuthEventFeedbackResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to update the device status, as an administrator.</p>
newtype AdminUpdateDeviceStatusRequest = AdminUpdateDeviceStatusRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceRememberedStatus" :: NullOrUndefined.NullOrUndefined (DeviceRememberedStatusType)
  }
derive instance newtypeAdminUpdateDeviceStatusRequest :: Newtype AdminUpdateDeviceStatusRequest _
derive instance repGenericAdminUpdateDeviceStatusRequest :: Generic AdminUpdateDeviceStatusRequest _
instance showAdminUpdateDeviceStatusRequest :: Show AdminUpdateDeviceStatusRequest where
  show = genericShow
instance decodeAdminUpdateDeviceStatusRequest :: Decode AdminUpdateDeviceStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateDeviceStatusRequest :: Encode AdminUpdateDeviceStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status response from the request to update the device, as an administrator.</p>
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse Types.NoArguments
derive instance newtypeAdminUpdateDeviceStatusResponse :: Newtype AdminUpdateDeviceStatusResponse _
derive instance repGenericAdminUpdateDeviceStatusResponse :: Generic AdminUpdateDeviceStatusResponse _
instance showAdminUpdateDeviceStatusResponse :: Show AdminUpdateDeviceStatusResponse where
  show = genericShow
instance decodeAdminUpdateDeviceStatusResponse :: Decode AdminUpdateDeviceStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateDeviceStatusResponse :: Encode AdminUpdateDeviceStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to update the user's attributes as an administrator.</p>
newtype AdminUpdateUserAttributesRequest = AdminUpdateUserAttributesRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "UserAttributes" :: (AttributeListType)
  }
derive instance newtypeAdminUpdateUserAttributesRequest :: Newtype AdminUpdateUserAttributesRequest _
derive instance repGenericAdminUpdateUserAttributesRequest :: Generic AdminUpdateUserAttributesRequest _
instance showAdminUpdateUserAttributesRequest :: Show AdminUpdateUserAttributesRequest where
  show = genericShow
instance decodeAdminUpdateUserAttributesRequest :: Decode AdminUpdateUserAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateUserAttributesRequest :: Encode AdminUpdateUserAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to update user attributes as an administrator.</p>
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse Types.NoArguments
derive instance newtypeAdminUpdateUserAttributesResponse :: Newtype AdminUpdateUserAttributesResponse _
derive instance repGenericAdminUpdateUserAttributesResponse :: Generic AdminUpdateUserAttributesResponse _
instance showAdminUpdateUserAttributesResponse :: Show AdminUpdateUserAttributesResponse where
  show = genericShow
instance decodeAdminUpdateUserAttributesResponse :: Decode AdminUpdateUserAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUpdateUserAttributesResponse :: Encode AdminUpdateUserAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to sign out of all devices, as an administrator.</p>
newtype AdminUserGlobalSignOutRequest = AdminUserGlobalSignOutRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  }
derive instance newtypeAdminUserGlobalSignOutRequest :: Newtype AdminUserGlobalSignOutRequest _
derive instance repGenericAdminUserGlobalSignOutRequest :: Generic AdminUserGlobalSignOutRequest _
instance showAdminUserGlobalSignOutRequest :: Show AdminUserGlobalSignOutRequest where
  show = genericShow
instance decodeAdminUserGlobalSignOutRequest :: Decode AdminUserGlobalSignOutRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUserGlobalSignOutRequest :: Encode AdminUserGlobalSignOutRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The global sign-out response, as an administrator.</p>
newtype AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse Types.NoArguments
derive instance newtypeAdminUserGlobalSignOutResponse :: Newtype AdminUserGlobalSignOutResponse _
derive instance repGenericAdminUserGlobalSignOutResponse :: Generic AdminUserGlobalSignOutResponse _
instance showAdminUserGlobalSignOutResponse :: Show AdminUserGlobalSignOutResponse where
  show = genericShow
instance decodeAdminUserGlobalSignOutResponse :: Decode AdminUserGlobalSignOutResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdminUserGlobalSignOutResponse :: Encode AdminUserGlobalSignOutResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdvancedSecurityModeType = AdvancedSecurityModeType String
derive instance newtypeAdvancedSecurityModeType :: Newtype AdvancedSecurityModeType _
derive instance repGenericAdvancedSecurityModeType :: Generic AdvancedSecurityModeType _
instance showAdvancedSecurityModeType :: Show AdvancedSecurityModeType where
  show = genericShow
instance decodeAdvancedSecurityModeType :: Decode AdvancedSecurityModeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdvancedSecurityModeType :: Encode AdvancedSecurityModeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasAttributeType = AliasAttributeType String
derive instance newtypeAliasAttributeType :: Newtype AliasAttributeType _
derive instance repGenericAliasAttributeType :: Generic AliasAttributeType _
instance showAliasAttributeType :: Show AliasAttributeType where
  show = genericShow
instance decodeAliasAttributeType :: Decode AliasAttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasAttributeType :: Encode AliasAttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasAttributesListType = AliasAttributesListType (Array AliasAttributeType)
derive instance newtypeAliasAttributesListType :: Newtype AliasAttributesListType _
derive instance repGenericAliasAttributesListType :: Generic AliasAttributesListType _
instance showAliasAttributesListType :: Show AliasAttributesListType where
  show = genericShow
instance decodeAliasAttributesListType :: Decode AliasAttributesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasAttributesListType :: Encode AliasAttributesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.</p>
newtype AliasExistsException = AliasExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeAliasExistsException :: Newtype AliasExistsException _
derive instance repGenericAliasExistsException :: Generic AliasExistsException _
instance showAliasExistsException :: Show AliasExistsException where
  show = genericShow
instance decodeAliasExistsException :: Decode AliasExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasExistsException :: Encode AliasExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.</p>
newtype AnalyticsConfigurationType = AnalyticsConfigurationType 
  { "ApplicationId" :: (HexStringType)
  , "RoleArn" :: (ArnType)
  , "ExternalId" :: (StringType)
  , "UserDataShared" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeAnalyticsConfigurationType :: Newtype AnalyticsConfigurationType _
derive instance repGenericAnalyticsConfigurationType :: Generic AnalyticsConfigurationType _
instance showAnalyticsConfigurationType :: Show AnalyticsConfigurationType where
  show = genericShow
instance decodeAnalyticsConfigurationType :: Decode AnalyticsConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsConfigurationType :: Encode AnalyticsConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon Pinpoint analytics endpoint.</p> <p>An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.</p>
newtype AnalyticsMetadataType = AnalyticsMetadataType 
  { "AnalyticsEndpointId" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeAnalyticsMetadataType :: Newtype AnalyticsMetadataType _
derive instance repGenericAnalyticsMetadataType :: Generic AnalyticsMetadataType _
instance showAnalyticsMetadataType :: Show AnalyticsMetadataType where
  show = genericShow
instance decodeAnalyticsMetadataType :: Decode AnalyticsMetadataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnalyticsMetadataType :: Encode AnalyticsMetadataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArnType = ArnType String
derive instance newtypeArnType :: Newtype ArnType _
derive instance repGenericArnType :: Generic ArnType _
instance showArnType :: Show ArnType where
  show = genericShow
instance decodeArnType :: Decode ArnType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArnType :: Encode ArnType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateSoftwareTokenRequest = AssociateSoftwareTokenRequest 
  { "AccessToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  }
derive instance newtypeAssociateSoftwareTokenRequest :: Newtype AssociateSoftwareTokenRequest _
derive instance repGenericAssociateSoftwareTokenRequest :: Generic AssociateSoftwareTokenRequest _
instance showAssociateSoftwareTokenRequest :: Show AssociateSoftwareTokenRequest where
  show = genericShow
instance decodeAssociateSoftwareTokenRequest :: Decode AssociateSoftwareTokenRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateSoftwareTokenRequest :: Encode AssociateSoftwareTokenRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse 
  { "SecretCode" :: NullOrUndefined.NullOrUndefined (SecretCodeType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  }
derive instance newtypeAssociateSoftwareTokenResponse :: Newtype AssociateSoftwareTokenResponse _
derive instance repGenericAssociateSoftwareTokenResponse :: Generic AssociateSoftwareTokenResponse _
instance showAssociateSoftwareTokenResponse :: Show AssociateSoftwareTokenResponse where
  show = genericShow
instance decodeAssociateSoftwareTokenResponse :: Decode AssociateSoftwareTokenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateSoftwareTokenResponse :: Encode AssociateSoftwareTokenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeDataType = AttributeDataType String
derive instance newtypeAttributeDataType :: Newtype AttributeDataType _
derive instance repGenericAttributeDataType :: Generic AttributeDataType _
instance showAttributeDataType :: Show AttributeDataType where
  show = genericShow
instance decodeAttributeDataType :: Decode AttributeDataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeDataType :: Encode AttributeDataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeListType = AttributeListType (Array AttributeType)
derive instance newtypeAttributeListType :: Newtype AttributeListType _
derive instance repGenericAttributeListType :: Generic AttributeListType _
instance showAttributeListType :: Show AttributeListType where
  show = genericShow
instance decodeAttributeListType :: Decode AttributeListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeListType :: Encode AttributeListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeMappingKeyType = AttributeMappingKeyType String
derive instance newtypeAttributeMappingKeyType :: Newtype AttributeMappingKeyType _
derive instance repGenericAttributeMappingKeyType :: Generic AttributeMappingKeyType _
instance showAttributeMappingKeyType :: Show AttributeMappingKeyType where
  show = genericShow
instance decodeAttributeMappingKeyType :: Decode AttributeMappingKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeMappingKeyType :: Encode AttributeMappingKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeMappingType = AttributeMappingType (StrMap.StrMap StringType)
derive instance newtypeAttributeMappingType :: Newtype AttributeMappingType _
derive instance repGenericAttributeMappingType :: Generic AttributeMappingType _
instance showAttributeMappingType :: Show AttributeMappingType where
  show = genericShow
instance decodeAttributeMappingType :: Decode AttributeMappingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeMappingType :: Encode AttributeMappingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeNameListType = AttributeNameListType (Array AttributeNameType)
derive instance newtypeAttributeNameListType :: Newtype AttributeNameListType _
derive instance repGenericAttributeNameListType :: Generic AttributeNameListType _
instance showAttributeNameListType :: Show AttributeNameListType where
  show = genericShow
instance decodeAttributeNameListType :: Decode AttributeNameListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameListType :: Encode AttributeNameListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeNameType = AttributeNameType String
derive instance newtypeAttributeNameType :: Newtype AttributeNameType _
derive instance repGenericAttributeNameType :: Generic AttributeNameType _
instance showAttributeNameType :: Show AttributeNameType where
  show = genericShow
instance decodeAttributeNameType :: Decode AttributeNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameType :: Encode AttributeNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies whether the attribute is standard or custom.</p>
newtype AttributeType = AttributeType 
  { "Name" :: (AttributeNameType)
  , "Value" :: NullOrUndefined.NullOrUndefined (AttributeValueType)
  }
derive instance newtypeAttributeType :: Newtype AttributeType _
derive instance repGenericAttributeType :: Generic AttributeType _
instance showAttributeType :: Show AttributeType where
  show = genericShow
instance decodeAttributeType :: Decode AttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeType :: Encode AttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValueType = AttributeValueType String
derive instance newtypeAttributeValueType :: Newtype AttributeValueType _
derive instance repGenericAttributeValueType :: Generic AttributeValueType _
instance showAttributeValueType :: Show AttributeValueType where
  show = genericShow
instance decodeAttributeValueType :: Decode AttributeValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValueType :: Encode AttributeValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authentication event type.</p>
newtype AuthEventType = AuthEventType 
  { "EventId" :: NullOrUndefined.NullOrUndefined (StringType)
  , "EventType" :: NullOrUndefined.NullOrUndefined (EventType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "EventResponse" :: NullOrUndefined.NullOrUndefined (EventResponseType)
  , "EventRisk" :: NullOrUndefined.NullOrUndefined (EventRiskType)
  , "ChallengeResponses" :: NullOrUndefined.NullOrUndefined (ChallengeResponseListType)
  , "EventContextData" :: NullOrUndefined.NullOrUndefined (EventContextDataType)
  , "EventFeedback" :: NullOrUndefined.NullOrUndefined (EventFeedbackType)
  }
derive instance newtypeAuthEventType :: Newtype AuthEventType _
derive instance repGenericAuthEventType :: Generic AuthEventType _
instance showAuthEventType :: Show AuthEventType where
  show = genericShow
instance decodeAuthEventType :: Decode AuthEventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthEventType :: Encode AuthEventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthEventsType = AuthEventsType (Array AuthEventType)
derive instance newtypeAuthEventsType :: Newtype AuthEventsType _
derive instance repGenericAuthEventsType :: Generic AuthEventsType _
instance showAuthEventsType :: Show AuthEventsType where
  show = genericShow
instance decodeAuthEventsType :: Decode AuthEventsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthEventsType :: Encode AuthEventsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthFlowType = AuthFlowType String
derive instance newtypeAuthFlowType :: Newtype AuthFlowType _
derive instance repGenericAuthFlowType :: Generic AuthFlowType _
instance showAuthFlowType :: Show AuthFlowType where
  show = genericShow
instance decodeAuthFlowType :: Decode AuthFlowType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthFlowType :: Encode AuthFlowType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthParametersType = AuthParametersType (StrMap.StrMap StringType)
derive instance newtypeAuthParametersType :: Newtype AuthParametersType _
derive instance repGenericAuthParametersType :: Generic AuthParametersType _
instance showAuthParametersType :: Show AuthParametersType where
  show = genericShow
instance decodeAuthParametersType :: Decode AuthParametersType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthParametersType :: Encode AuthParametersType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The authentication result.</p>
newtype AuthenticationResultType = AuthenticationResultType 
  { "AccessToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "ExpiresIn" :: NullOrUndefined.NullOrUndefined (IntegerType)
  , "TokenType" :: NullOrUndefined.NullOrUndefined (StringType)
  , "RefreshToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "IdToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "NewDeviceMetadata" :: NullOrUndefined.NullOrUndefined (NewDeviceMetadataType)
  }
derive instance newtypeAuthenticationResultType :: Newtype AuthenticationResultType _
derive instance repGenericAuthenticationResultType :: Generic AuthenticationResultType _
instance showAuthenticationResultType :: Show AuthenticationResultType where
  show = genericShow
instance decodeAuthenticationResultType :: Decode AuthenticationResultType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthenticationResultType :: Encode AuthenticationResultType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlockedIPRangeListType = BlockedIPRangeListType (Array StringType)
derive instance newtypeBlockedIPRangeListType :: Newtype BlockedIPRangeListType _
derive instance repGenericBlockedIPRangeListType :: Generic BlockedIPRangeListType _
instance showBlockedIPRangeListType :: Show BlockedIPRangeListType where
  show = genericShow
instance decodeBlockedIPRangeListType :: Decode BlockedIPRangeListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlockedIPRangeListType :: Encode BlockedIPRangeListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanType = BooleanType Boolean
derive instance newtypeBooleanType :: Newtype BooleanType _
derive instance repGenericBooleanType :: Generic BooleanType _
instance showBooleanType :: Show BooleanType where
  show = genericShow
instance decodeBooleanType :: Decode BooleanType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanType :: Encode BooleanType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CSSType = CSSType String
derive instance newtypeCSSType :: Newtype CSSType _
derive instance repGenericCSSType :: Generic CSSType _
instance showCSSType :: Show CSSType where
  show = genericShow
instance decodeCSSType :: Decode CSSType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSSType :: Encode CSSType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CSSVersionType = CSSVersionType String
derive instance newtypeCSSVersionType :: Newtype CSSVersionType _
derive instance repGenericCSSVersionType :: Generic CSSVersionType _
instance showCSSVersionType :: Show CSSVersionType where
  show = genericShow
instance decodeCSSVersionType :: Decode CSSVersionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSSVersionType :: Encode CSSVersionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CallbackURLsListType = CallbackURLsListType (Array RedirectUrlType)
derive instance newtypeCallbackURLsListType :: Newtype CallbackURLsListType _
derive instance repGenericCallbackURLsListType :: Generic CallbackURLsListType _
instance showCallbackURLsListType :: Show CallbackURLsListType where
  show = genericShow
instance decodeCallbackURLsListType :: Decode CallbackURLsListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCallbackURLsListType :: Encode CallbackURLsListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeName = ChallengeName String
derive instance newtypeChallengeName :: Newtype ChallengeName _
derive instance repGenericChallengeName :: Generic ChallengeName _
instance showChallengeName :: Show ChallengeName where
  show = genericShow
instance decodeChallengeName :: Decode ChallengeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeName :: Encode ChallengeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeNameType = ChallengeNameType String
derive instance newtypeChallengeNameType :: Newtype ChallengeNameType _
derive instance repGenericChallengeNameType :: Generic ChallengeNameType _
instance showChallengeNameType :: Show ChallengeNameType where
  show = genericShow
instance decodeChallengeNameType :: Decode ChallengeNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeNameType :: Encode ChallengeNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeParametersType = ChallengeParametersType (StrMap.StrMap StringType)
derive instance newtypeChallengeParametersType :: Newtype ChallengeParametersType _
derive instance repGenericChallengeParametersType :: Generic ChallengeParametersType _
instance showChallengeParametersType :: Show ChallengeParametersType where
  show = genericShow
instance decodeChallengeParametersType :: Decode ChallengeParametersType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeParametersType :: Encode ChallengeParametersType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeResponse = ChallengeResponse String
derive instance newtypeChallengeResponse :: Newtype ChallengeResponse _
derive instance repGenericChallengeResponse :: Generic ChallengeResponse _
instance showChallengeResponse :: Show ChallengeResponse where
  show = genericShow
instance decodeChallengeResponse :: Decode ChallengeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeResponse :: Encode ChallengeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeResponseListType = ChallengeResponseListType (Array ChallengeResponseType)
derive instance newtypeChallengeResponseListType :: Newtype ChallengeResponseListType _
derive instance repGenericChallengeResponseListType :: Generic ChallengeResponseListType _
instance showChallengeResponseListType :: Show ChallengeResponseListType where
  show = genericShow
instance decodeChallengeResponseListType :: Decode ChallengeResponseListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeResponseListType :: Encode ChallengeResponseListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The challenge response type.</p>
newtype ChallengeResponseType = ChallengeResponseType 
  { "ChallengeName" :: NullOrUndefined.NullOrUndefined (ChallengeName)
  , "ChallengeResponse" :: NullOrUndefined.NullOrUndefined (ChallengeResponse)
  }
derive instance newtypeChallengeResponseType :: Newtype ChallengeResponseType _
derive instance repGenericChallengeResponseType :: Generic ChallengeResponseType _
instance showChallengeResponseType :: Show ChallengeResponseType where
  show = genericShow
instance decodeChallengeResponseType :: Decode ChallengeResponseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeResponseType :: Encode ChallengeResponseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChallengeResponsesType = ChallengeResponsesType (StrMap.StrMap StringType)
derive instance newtypeChallengeResponsesType :: Newtype ChallengeResponsesType _
derive instance repGenericChallengeResponsesType :: Generic ChallengeResponsesType _
instance showChallengeResponsesType :: Show ChallengeResponsesType where
  show = genericShow
instance decodeChallengeResponsesType :: Decode ChallengeResponsesType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChallengeResponsesType :: Encode ChallengeResponsesType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to change a user password.</p>
newtype ChangePasswordRequest = ChangePasswordRequest 
  { "PreviousPassword" :: (PasswordType)
  , "ProposedPassword" :: (PasswordType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeChangePasswordRequest :: Newtype ChangePasswordRequest _
derive instance repGenericChangePasswordRequest :: Generic ChangePasswordRequest _
instance showChangePasswordRequest :: Show ChangePasswordRequest where
  show = genericShow
instance decodeChangePasswordRequest :: Decode ChangePasswordRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangePasswordRequest :: Encode ChangePasswordRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server to the change password request.</p>
newtype ChangePasswordResponse = ChangePasswordResponse Types.NoArguments
derive instance newtypeChangePasswordResponse :: Newtype ChangePasswordResponse _
derive instance repGenericChangePasswordResponse :: Generic ChangePasswordResponse _
instance showChangePasswordResponse :: Show ChangePasswordResponse where
  show = genericShow
instance decodeChangePasswordResponse :: Decode ChangePasswordResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangePasswordResponse :: Encode ChangePasswordResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientIdType = ClientIdType String
derive instance newtypeClientIdType :: Newtype ClientIdType _
derive instance repGenericClientIdType :: Generic ClientIdType _
instance showClientIdType :: Show ClientIdType where
  show = genericShow
instance decodeClientIdType :: Decode ClientIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientIdType :: Encode ClientIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientMetadataType = ClientMetadataType (StrMap.StrMap StringType)
derive instance newtypeClientMetadataType :: Newtype ClientMetadataType _
derive instance repGenericClientMetadataType :: Generic ClientMetadataType _
instance showClientMetadataType :: Show ClientMetadataType where
  show = genericShow
instance decodeClientMetadataType :: Decode ClientMetadataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientMetadataType :: Encode ClientMetadataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientNameType = ClientNameType String
derive instance newtypeClientNameType :: Newtype ClientNameType _
derive instance repGenericClientNameType :: Generic ClientNameType _
instance showClientNameType :: Show ClientNameType where
  show = genericShow
instance decodeClientNameType :: Decode ClientNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientNameType :: Encode ClientNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientPermissionListType = ClientPermissionListType (Array ClientPermissionType)
derive instance newtypeClientPermissionListType :: Newtype ClientPermissionListType _
derive instance repGenericClientPermissionListType :: Generic ClientPermissionListType _
instance showClientPermissionListType :: Show ClientPermissionListType where
  show = genericShow
instance decodeClientPermissionListType :: Decode ClientPermissionListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientPermissionListType :: Encode ClientPermissionListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientPermissionType = ClientPermissionType String
derive instance newtypeClientPermissionType :: Newtype ClientPermissionType _
derive instance repGenericClientPermissionType :: Generic ClientPermissionType _
instance showClientPermissionType :: Show ClientPermissionType where
  show = genericShow
instance decodeClientPermissionType :: Decode ClientPermissionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientPermissionType :: Encode ClientPermissionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientSecretType = ClientSecretType String
derive instance newtypeClientSecretType :: Newtype ClientSecretType _
derive instance repGenericClientSecretType :: Generic ClientSecretType _
instance showClientSecretType :: Show ClientSecretType where
  show = genericShow
instance decodeClientSecretType :: Decode ClientSecretType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientSecretType :: Encode ClientSecretType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeDeliveryDetailsListType = CodeDeliveryDetailsListType (Array CodeDeliveryDetailsType)
derive instance newtypeCodeDeliveryDetailsListType :: Newtype CodeDeliveryDetailsListType _
derive instance repGenericCodeDeliveryDetailsListType :: Generic CodeDeliveryDetailsListType _
instance showCodeDeliveryDetailsListType :: Show CodeDeliveryDetailsListType where
  show = genericShow
instance decodeCodeDeliveryDetailsListType :: Decode CodeDeliveryDetailsListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeDeliveryDetailsListType :: Encode CodeDeliveryDetailsListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The code delivery details being returned from the server.</p>
newtype CodeDeliveryDetailsType = CodeDeliveryDetailsType 
  { "Destination" :: NullOrUndefined.NullOrUndefined (StringType)
  , "DeliveryMedium" :: NullOrUndefined.NullOrUndefined (DeliveryMediumType)
  , "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeNameType)
  }
derive instance newtypeCodeDeliveryDetailsType :: Newtype CodeDeliveryDetailsType _
derive instance repGenericCodeDeliveryDetailsType :: Generic CodeDeliveryDetailsType _
instance showCodeDeliveryDetailsType :: Show CodeDeliveryDetailsType where
  show = genericShow
instance decodeCodeDeliveryDetailsType :: Decode CodeDeliveryDetailsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeDeliveryDetailsType :: Encode CodeDeliveryDetailsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a verification code fails to deliver successfully.</p>
newtype CodeDeliveryFailureException = CodeDeliveryFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeCodeDeliveryFailureException :: Newtype CodeDeliveryFailureException _
derive instance repGenericCodeDeliveryFailureException :: Generic CodeDeliveryFailureException _
instance showCodeDeliveryFailureException :: Show CodeDeliveryFailureException where
  show = genericShow
instance decodeCodeDeliveryFailureException :: Decode CodeDeliveryFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeDeliveryFailureException :: Encode CodeDeliveryFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown if the provided code does not match what the server was expecting.</p>
newtype CodeMismatchException = CodeMismatchException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeCodeMismatchException :: Newtype CodeMismatchException _
derive instance repGenericCodeMismatchException :: Generic CodeMismatchException _
instance showCodeMismatchException :: Show CodeMismatchException where
  show = genericShow
instance decodeCodeMismatchException :: Decode CodeMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeMismatchException :: Encode CodeMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompletionMessageType = CompletionMessageType String
derive instance newtypeCompletionMessageType :: Newtype CompletionMessageType _
derive instance repGenericCompletionMessageType :: Generic CompletionMessageType _
instance showCompletionMessageType :: Show CompletionMessageType where
  show = genericShow
instance decodeCompletionMessageType :: Decode CompletionMessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompletionMessageType :: Encode CompletionMessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The compromised credentials actions type</p>
newtype CompromisedCredentialsActionsType = CompromisedCredentialsActionsType 
  { "EventAction" :: (CompromisedCredentialsEventActionType)
  }
derive instance newtypeCompromisedCredentialsActionsType :: Newtype CompromisedCredentialsActionsType _
derive instance repGenericCompromisedCredentialsActionsType :: Generic CompromisedCredentialsActionsType _
instance showCompromisedCredentialsActionsType :: Show CompromisedCredentialsActionsType where
  show = genericShow
instance decodeCompromisedCredentialsActionsType :: Decode CompromisedCredentialsActionsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompromisedCredentialsActionsType :: Encode CompromisedCredentialsActionsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompromisedCredentialsEventActionType = CompromisedCredentialsEventActionType String
derive instance newtypeCompromisedCredentialsEventActionType :: Newtype CompromisedCredentialsEventActionType _
derive instance repGenericCompromisedCredentialsEventActionType :: Generic CompromisedCredentialsEventActionType _
instance showCompromisedCredentialsEventActionType :: Show CompromisedCredentialsEventActionType where
  show = genericShow
instance decodeCompromisedCredentialsEventActionType :: Decode CompromisedCredentialsEventActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompromisedCredentialsEventActionType :: Encode CompromisedCredentialsEventActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The compromised credentials risk configuration type.</p>
newtype CompromisedCredentialsRiskConfigurationType = CompromisedCredentialsRiskConfigurationType 
  { "EventFilter" :: NullOrUndefined.NullOrUndefined (EventFiltersType)
  , "Actions" :: (CompromisedCredentialsActionsType)
  }
derive instance newtypeCompromisedCredentialsRiskConfigurationType :: Newtype CompromisedCredentialsRiskConfigurationType _
derive instance repGenericCompromisedCredentialsRiskConfigurationType :: Generic CompromisedCredentialsRiskConfigurationType _
instance showCompromisedCredentialsRiskConfigurationType :: Show CompromisedCredentialsRiskConfigurationType where
  show = genericShow
instance decodeCompromisedCredentialsRiskConfigurationType :: Decode CompromisedCredentialsRiskConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompromisedCredentialsRiskConfigurationType :: Encode CompromisedCredentialsRiskConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown if two or more modifications are happening concurrently.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Confirms the device request.</p>
newtype ConfirmDeviceRequest = ConfirmDeviceRequest 
  { "AccessToken" :: (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceSecretVerifierConfig" :: NullOrUndefined.NullOrUndefined (DeviceSecretVerifierConfigType)
  , "DeviceName" :: NullOrUndefined.NullOrUndefined (DeviceNameType)
  }
derive instance newtypeConfirmDeviceRequest :: Newtype ConfirmDeviceRequest _
derive instance repGenericConfirmDeviceRequest :: Generic ConfirmDeviceRequest _
instance showConfirmDeviceRequest :: Show ConfirmDeviceRequest where
  show = genericShow
instance decodeConfirmDeviceRequest :: Decode ConfirmDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmDeviceRequest :: Encode ConfirmDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Confirms the device response.</p>
newtype ConfirmDeviceResponse = ConfirmDeviceResponse 
  { "UserConfirmationNecessary" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeConfirmDeviceResponse :: Newtype ConfirmDeviceResponse _
derive instance repGenericConfirmDeviceResponse :: Generic ConfirmDeviceResponse _
instance showConfirmDeviceResponse :: Show ConfirmDeviceResponse where
  show = genericShow
instance decodeConfirmDeviceResponse :: Decode ConfirmDeviceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmDeviceResponse :: Encode ConfirmDeviceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request representing the confirmation for a password reset.</p>
newtype ConfirmForgotPasswordRequest = ConfirmForgotPasswordRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined.NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "ConfirmationCode" :: (ConfirmationCodeType)
  , "Password" :: (PasswordType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  }
derive instance newtypeConfirmForgotPasswordRequest :: Newtype ConfirmForgotPasswordRequest _
derive instance repGenericConfirmForgotPasswordRequest :: Generic ConfirmForgotPasswordRequest _
instance showConfirmForgotPasswordRequest :: Show ConfirmForgotPasswordRequest where
  show = genericShow
instance decodeConfirmForgotPasswordRequest :: Decode ConfirmForgotPasswordRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmForgotPasswordRequest :: Encode ConfirmForgotPasswordRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server that results from a user's request to retrieve a forgotten password.</p>
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse Types.NoArguments
derive instance newtypeConfirmForgotPasswordResponse :: Newtype ConfirmForgotPasswordResponse _
derive instance repGenericConfirmForgotPasswordResponse :: Generic ConfirmForgotPasswordResponse _
instance showConfirmForgotPasswordResponse :: Show ConfirmForgotPasswordResponse where
  show = genericShow
instance decodeConfirmForgotPasswordResponse :: Decode ConfirmForgotPasswordResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmForgotPasswordResponse :: Encode ConfirmForgotPasswordResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to confirm registration of a user.</p>
newtype ConfirmSignUpRequest = ConfirmSignUpRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined.NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "ConfirmationCode" :: (ConfirmationCodeType)
  , "ForceAliasCreation" :: NullOrUndefined.NullOrUndefined (ForceAliasCreation)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  }
derive instance newtypeConfirmSignUpRequest :: Newtype ConfirmSignUpRequest _
derive instance repGenericConfirmSignUpRequest :: Generic ConfirmSignUpRequest _
instance showConfirmSignUpRequest :: Show ConfirmSignUpRequest where
  show = genericShow
instance decodeConfirmSignUpRequest :: Decode ConfirmSignUpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmSignUpRequest :: Encode ConfirmSignUpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the registration confirmation.</p>
newtype ConfirmSignUpResponse = ConfirmSignUpResponse Types.NoArguments
derive instance newtypeConfirmSignUpResponse :: Newtype ConfirmSignUpResponse _
derive instance repGenericConfirmSignUpResponse :: Generic ConfirmSignUpResponse _
instance showConfirmSignUpResponse :: Show ConfirmSignUpResponse where
  show = genericShow
instance decodeConfirmSignUpResponse :: Decode ConfirmSignUpResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmSignUpResponse :: Encode ConfirmSignUpResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfirmationCodeType = ConfirmationCodeType String
derive instance newtypeConfirmationCodeType :: Newtype ConfirmationCodeType _
derive instance repGenericConfirmationCodeType :: Generic ConfirmationCodeType _
instance showConfirmationCodeType :: Show ConfirmationCodeType where
  show = genericShow
instance decodeConfirmationCodeType :: Decode ConfirmationCodeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfirmationCodeType :: Encode ConfirmationCodeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>
newtype ContextDataType = ContextDataType 
  { "IpAddress" :: (StringType)
  , "ServerName" :: (StringType)
  , "ServerPath" :: (StringType)
  , "HttpHeaders" :: (HttpHeaderList)
  , "EncodedData" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeContextDataType :: Newtype ContextDataType _
derive instance repGenericContextDataType :: Generic ContextDataType _
instance showContextDataType :: Show ContextDataType where
  show = genericShow
instance decodeContextDataType :: Decode ContextDataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContextDataType :: Encode ContextDataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupRequest = CreateGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined.NullOrUndefined (PrecedenceType)
  }
derive instance newtypeCreateGroupRequest :: Newtype CreateGroupRequest _
derive instance repGenericCreateGroupRequest :: Generic CreateGroupRequest _
instance showCreateGroupRequest :: Show CreateGroupRequest where
  show = genericShow
instance decodeCreateGroupRequest :: Decode CreateGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupRequest :: Encode CreateGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateGroupResponse = CreateGroupResponse 
  { "Group" :: NullOrUndefined.NullOrUndefined (GroupType)
  }
derive instance newtypeCreateGroupResponse :: Newtype CreateGroupResponse _
derive instance repGenericCreateGroupResponse :: Generic CreateGroupResponse _
instance showCreateGroupResponse :: Show CreateGroupResponse where
  show = genericShow
instance decodeCreateGroupResponse :: Decode CreateGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGroupResponse :: Encode CreateGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIdentityProviderRequest = CreateIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameTypeV1)
  , "ProviderType" :: (IdentityProviderTypeType)
  , "ProviderDetails" :: (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined.NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined.NullOrUndefined (IdpIdentifiersListType)
  }
derive instance newtypeCreateIdentityProviderRequest :: Newtype CreateIdentityProviderRequest _
derive instance repGenericCreateIdentityProviderRequest :: Generic CreateIdentityProviderRequest _
instance showCreateIdentityProviderRequest :: Show CreateIdentityProviderRequest where
  show = genericShow
instance decodeCreateIdentityProviderRequest :: Decode CreateIdentityProviderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIdentityProviderRequest :: Encode CreateIdentityProviderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIdentityProviderResponse = CreateIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeCreateIdentityProviderResponse :: Newtype CreateIdentityProviderResponse _
derive instance repGenericCreateIdentityProviderResponse :: Generic CreateIdentityProviderResponse _
instance showCreateIdentityProviderResponse :: Show CreateIdentityProviderResponse where
  show = genericShow
instance decodeCreateIdentityProviderResponse :: Decode CreateIdentityProviderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIdentityProviderResponse :: Encode CreateIdentityProviderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceServerRequest = CreateResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  , "Name" :: (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined.NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeCreateResourceServerRequest :: Newtype CreateResourceServerRequest _
derive instance repGenericCreateResourceServerRequest :: Generic CreateResourceServerRequest _
instance showCreateResourceServerRequest :: Show CreateResourceServerRequest where
  show = genericShow
instance decodeCreateResourceServerRequest :: Decode CreateResourceServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceServerRequest :: Encode CreateResourceServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceServerResponse = CreateResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeCreateResourceServerResponse :: Newtype CreateResourceServerResponse _
derive instance repGenericCreateResourceServerResponse :: Generic CreateResourceServerResponse _
instance showCreateResourceServerResponse :: Show CreateResourceServerResponse where
  show = genericShow
instance decodeCreateResourceServerResponse :: Decode CreateResourceServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceServerResponse :: Encode CreateResourceServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to create the user import job.</p>
newtype CreateUserImportJobRequest = CreateUserImportJobRequest 
  { "JobName" :: (UserImportJobNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "CloudWatchLogsRoleArn" :: (ArnType)
  }
derive instance newtypeCreateUserImportJobRequest :: Newtype CreateUserImportJobRequest _
derive instance repGenericCreateUserImportJobRequest :: Generic CreateUserImportJobRequest _
instance showCreateUserImportJobRequest :: Show CreateUserImportJobRequest where
  show = genericShow
instance decodeCreateUserImportJobRequest :: Decode CreateUserImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserImportJobRequest :: Encode CreateUserImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to create the user import job.</p>
newtype CreateUserImportJobResponse = CreateUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined.NullOrUndefined (UserImportJobType)
  }
derive instance newtypeCreateUserImportJobResponse :: Newtype CreateUserImportJobResponse _
derive instance repGenericCreateUserImportJobResponse :: Generic CreateUserImportJobResponse _
instance showCreateUserImportJobResponse :: Show CreateUserImportJobResponse where
  show = genericShow
instance decodeCreateUserImportJobResponse :: Decode CreateUserImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserImportJobResponse :: Encode CreateUserImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to create a user pool client.</p>
newtype CreateUserPoolClientRequest = CreateUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientName" :: (ClientNameType)
  , "GenerateSecret" :: NullOrUndefined.NullOrUndefined (GenerateSecret)
  , "RefreshTokenValidity" :: NullOrUndefined.NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined.NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined.NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined.NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined.NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined.NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined.NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined.NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined.NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeCreateUserPoolClientRequest :: Newtype CreateUserPoolClientRequest _
derive instance repGenericCreateUserPoolClientRequest :: Generic CreateUserPoolClientRequest _
instance showCreateUserPoolClientRequest :: Show CreateUserPoolClientRequest where
  show = genericShow
instance decodeCreateUserPoolClientRequest :: Decode CreateUserPoolClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolClientRequest :: Encode CreateUserPoolClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to create a user pool client.</p>
newtype CreateUserPoolClientResponse = CreateUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined.NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeCreateUserPoolClientResponse :: Newtype CreateUserPoolClientResponse _
derive instance repGenericCreateUserPoolClientResponse :: Generic CreateUserPoolClientResponse _
instance showCreateUserPoolClientResponse :: Show CreateUserPoolClientResponse where
  show = genericShow
instance decodeCreateUserPoolClientResponse :: Decode CreateUserPoolClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolClientResponse :: Encode CreateUserPoolClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserPoolDomainRequest = CreateUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeCreateUserPoolDomainRequest :: Newtype CreateUserPoolDomainRequest _
derive instance repGenericCreateUserPoolDomainRequest :: Generic CreateUserPoolDomainRequest _
instance showCreateUserPoolDomainRequest :: Show CreateUserPoolDomainRequest where
  show = genericShow
instance decodeCreateUserPoolDomainRequest :: Decode CreateUserPoolDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolDomainRequest :: Encode CreateUserPoolDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserPoolDomainResponse = CreateUserPoolDomainResponse Types.NoArguments
derive instance newtypeCreateUserPoolDomainResponse :: Newtype CreateUserPoolDomainResponse _
derive instance repGenericCreateUserPoolDomainResponse :: Generic CreateUserPoolDomainResponse _
instance showCreateUserPoolDomainResponse :: Show CreateUserPoolDomainResponse where
  show = genericShow
instance decodeCreateUserPoolDomainResponse :: Decode CreateUserPoolDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolDomainResponse :: Encode CreateUserPoolDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to create a user pool.</p>
newtype CreateUserPoolRequest = CreateUserPoolRequest 
  { "PoolName" :: (UserPoolNameType)
  , "Policies" :: NullOrUndefined.NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined.NullOrUndefined (LambdaConfigType)
  , "AutoVerifiedAttributes" :: NullOrUndefined.NullOrUndefined (VerifiedAttributesListType)
  , "AliasAttributes" :: NullOrUndefined.NullOrUndefined (AliasAttributesListType)
  , "UsernameAttributes" :: NullOrUndefined.NullOrUndefined (UsernameAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined.NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined.NullOrUndefined (DeviceConfigurationType)
  , "EmailConfiguration" :: NullOrUndefined.NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined.NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined.NullOrUndefined (UserPoolTagsType)
  , "AdminCreateUserConfig" :: NullOrUndefined.NullOrUndefined (AdminCreateUserConfigType)
  , "Schema" :: NullOrUndefined.NullOrUndefined (SchemaAttributesListType)
  , "UserPoolAddOns" :: NullOrUndefined.NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeCreateUserPoolRequest :: Newtype CreateUserPoolRequest _
derive instance repGenericCreateUserPoolRequest :: Generic CreateUserPoolRequest _
instance showCreateUserPoolRequest :: Show CreateUserPoolRequest where
  show = genericShow
instance decodeCreateUserPoolRequest :: Decode CreateUserPoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolRequest :: Encode CreateUserPoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to create a user pool.</p>
newtype CreateUserPoolResponse = CreateUserPoolResponse 
  { "UserPool" :: NullOrUndefined.NullOrUndefined (UserPoolType)
  }
derive instance newtypeCreateUserPoolResponse :: Newtype CreateUserPoolResponse _
derive instance repGenericCreateUserPoolResponse :: Generic CreateUserPoolResponse _
instance showCreateUserPoolResponse :: Show CreateUserPoolResponse where
  show = genericShow
instance decodeCreateUserPoolResponse :: Decode CreateUserPoolResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserPoolResponse :: Encode CreateUserPoolResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomAttributeNameType = CustomAttributeNameType String
derive instance newtypeCustomAttributeNameType :: Newtype CustomAttributeNameType _
derive instance repGenericCustomAttributeNameType :: Generic CustomAttributeNameType _
instance showCustomAttributeNameType :: Show CustomAttributeNameType where
  show = genericShow
instance decodeCustomAttributeNameType :: Decode CustomAttributeNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomAttributeNameType :: Encode CustomAttributeNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomAttributesListType = CustomAttributesListType (Array SchemaAttributeType)
derive instance newtypeCustomAttributesListType :: Newtype CustomAttributesListType _
derive instance repGenericCustomAttributesListType :: Generic CustomAttributesListType _
instance showCustomAttributesListType :: Show CustomAttributesListType where
  show = genericShow
instance decodeCustomAttributesListType :: Decode CustomAttributesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomAttributesListType :: Encode CustomAttributesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateType = DateType Number
derive instance newtypeDateType :: Newtype DateType _
derive instance repGenericDateType :: Generic DateType _
instance showDateType :: Show DateType where
  show = genericShow
instance decodeDateType :: Decode DateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateType :: Encode DateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultEmailOptionType = DefaultEmailOptionType String
derive instance newtypeDefaultEmailOptionType :: Newtype DefaultEmailOptionType _
derive instance repGenericDefaultEmailOptionType :: Generic DefaultEmailOptionType _
instance showDefaultEmailOptionType :: Show DefaultEmailOptionType where
  show = genericShow
instance decodeDefaultEmailOptionType :: Decode DefaultEmailOptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultEmailOptionType :: Encode DefaultEmailOptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteGroupRequest = DeleteGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteGroupRequest :: Newtype DeleteGroupRequest _
derive instance repGenericDeleteGroupRequest :: Generic DeleteGroupRequest _
instance showDeleteGroupRequest :: Show DeleteGroupRequest where
  show = genericShow
instance decodeDeleteGroupRequest :: Decode DeleteGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteGroupRequest :: Encode DeleteGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIdentityProviderRequest = DeleteIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  }
derive instance newtypeDeleteIdentityProviderRequest :: Newtype DeleteIdentityProviderRequest _
derive instance repGenericDeleteIdentityProviderRequest :: Generic DeleteIdentityProviderRequest _
instance showDeleteIdentityProviderRequest :: Show DeleteIdentityProviderRequest where
  show = genericShow
instance decodeDeleteIdentityProviderRequest :: Decode DeleteIdentityProviderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityProviderRequest :: Encode DeleteIdentityProviderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceServerRequest = DeleteResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  }
derive instance newtypeDeleteResourceServerRequest :: Newtype DeleteResourceServerRequest _
derive instance repGenericDeleteResourceServerRequest :: Generic DeleteResourceServerRequest _
instance showDeleteResourceServerRequest :: Show DeleteResourceServerRequest where
  show = genericShow
instance decodeDeleteResourceServerRequest :: Decode DeleteResourceServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceServerRequest :: Encode DeleteResourceServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete user attributes.</p>
newtype DeleteUserAttributesRequest = DeleteUserAttributesRequest 
  { "UserAttributeNames" :: (AttributeNameListType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeDeleteUserAttributesRequest :: Newtype DeleteUserAttributesRequest _
derive instance repGenericDeleteUserAttributesRequest :: Generic DeleteUserAttributesRequest _
instance showDeleteUserAttributesRequest :: Show DeleteUserAttributesRequest where
  show = genericShow
instance decodeDeleteUserAttributesRequest :: Decode DeleteUserAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserAttributesRequest :: Encode DeleteUserAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to delete user attributes.</p>
newtype DeleteUserAttributesResponse = DeleteUserAttributesResponse Types.NoArguments
derive instance newtypeDeleteUserAttributesResponse :: Newtype DeleteUserAttributesResponse _
derive instance repGenericDeleteUserAttributesResponse :: Generic DeleteUserAttributesResponse _
instance showDeleteUserAttributesResponse :: Show DeleteUserAttributesResponse where
  show = genericShow
instance decodeDeleteUserAttributesResponse :: Decode DeleteUserAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserAttributesResponse :: Encode DeleteUserAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete a user pool client.</p>
newtype DeleteUserPoolClientRequest = DeleteUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  }
derive instance newtypeDeleteUserPoolClientRequest :: Newtype DeleteUserPoolClientRequest _
derive instance repGenericDeleteUserPoolClientRequest :: Generic DeleteUserPoolClientRequest _
instance showDeleteUserPoolClientRequest :: Show DeleteUserPoolClientRequest where
  show = genericShow
instance decodeDeleteUserPoolClientRequest :: Decode DeleteUserPoolClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserPoolClientRequest :: Encode DeleteUserPoolClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserPoolDomainRequest = DeleteUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteUserPoolDomainRequest :: Newtype DeleteUserPoolDomainRequest _
derive instance repGenericDeleteUserPoolDomainRequest :: Generic DeleteUserPoolDomainRequest _
instance showDeleteUserPoolDomainRequest :: Show DeleteUserPoolDomainRequest where
  show = genericShow
instance decodeDeleteUserPoolDomainRequest :: Decode DeleteUserPoolDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserPoolDomainRequest :: Encode DeleteUserPoolDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserPoolDomainResponse = DeleteUserPoolDomainResponse Types.NoArguments
derive instance newtypeDeleteUserPoolDomainResponse :: Newtype DeleteUserPoolDomainResponse _
derive instance repGenericDeleteUserPoolDomainResponse :: Generic DeleteUserPoolDomainResponse _
instance showDeleteUserPoolDomainResponse :: Show DeleteUserPoolDomainResponse where
  show = genericShow
instance decodeDeleteUserPoolDomainResponse :: Decode DeleteUserPoolDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserPoolDomainResponse :: Encode DeleteUserPoolDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete a user pool.</p>
newtype DeleteUserPoolRequest = DeleteUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDeleteUserPoolRequest :: Newtype DeleteUserPoolRequest _
derive instance repGenericDeleteUserPoolRequest :: Generic DeleteUserPoolRequest _
instance showDeleteUserPoolRequest :: Show DeleteUserPoolRequest where
  show = genericShow
instance decodeDeleteUserPoolRequest :: Decode DeleteUserPoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserPoolRequest :: Encode DeleteUserPoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to delete a user.</p>
newtype DeleteUserRequest = DeleteUserRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _
derive instance repGenericDeleteUserRequest :: Generic DeleteUserRequest _
instance showDeleteUserRequest :: Show DeleteUserRequest where
  show = genericShow
instance decodeDeleteUserRequest :: Decode DeleteUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserRequest :: Encode DeleteUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryMediumListType = DeliveryMediumListType (Array DeliveryMediumType)
derive instance newtypeDeliveryMediumListType :: Newtype DeliveryMediumListType _
derive instance repGenericDeliveryMediumListType :: Generic DeliveryMediumListType _
instance showDeliveryMediumListType :: Show DeliveryMediumListType where
  show = genericShow
instance decodeDeliveryMediumListType :: Decode DeliveryMediumListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryMediumListType :: Encode DeliveryMediumListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryMediumType = DeliveryMediumType String
derive instance newtypeDeliveryMediumType :: Newtype DeliveryMediumType _
derive instance repGenericDeliveryMediumType :: Generic DeliveryMediumType _
instance showDeliveryMediumType :: Show DeliveryMediumType where
  show = genericShow
instance decodeDeliveryMediumType :: Decode DeliveryMediumType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryMediumType :: Encode DeliveryMediumType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeIdentityProviderRequest = DescribeIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  }
derive instance newtypeDescribeIdentityProviderRequest :: Newtype DescribeIdentityProviderRequest _
derive instance repGenericDescribeIdentityProviderRequest :: Generic DescribeIdentityProviderRequest _
instance showDescribeIdentityProviderRequest :: Show DescribeIdentityProviderRequest where
  show = genericShow
instance decodeDescribeIdentityProviderRequest :: Decode DescribeIdentityProviderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityProviderRequest :: Encode DescribeIdentityProviderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeIdentityProviderResponse = DescribeIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeDescribeIdentityProviderResponse :: Newtype DescribeIdentityProviderResponse _
derive instance repGenericDescribeIdentityProviderResponse :: Generic DescribeIdentityProviderResponse _
instance showDescribeIdentityProviderResponse :: Show DescribeIdentityProviderResponse where
  show = genericShow
instance decodeDescribeIdentityProviderResponse :: Decode DescribeIdentityProviderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityProviderResponse :: Encode DescribeIdentityProviderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourceServerRequest = DescribeResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  }
derive instance newtypeDescribeResourceServerRequest :: Newtype DescribeResourceServerRequest _
derive instance repGenericDescribeResourceServerRequest :: Generic DescribeResourceServerRequest _
instance showDescribeResourceServerRequest :: Show DescribeResourceServerRequest where
  show = genericShow
instance decodeDescribeResourceServerRequest :: Decode DescribeResourceServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourceServerRequest :: Encode DescribeResourceServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourceServerResponse = DescribeResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeDescribeResourceServerResponse :: Newtype DescribeResourceServerResponse _
derive instance repGenericDescribeResourceServerResponse :: Generic DescribeResourceServerResponse _
instance showDescribeResourceServerResponse :: Show DescribeResourceServerResponse where
  show = genericShow
instance decodeDescribeResourceServerResponse :: Decode DescribeResourceServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourceServerResponse :: Encode DescribeResourceServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRiskConfigurationRequest = DescribeRiskConfigurationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  }
derive instance newtypeDescribeRiskConfigurationRequest :: Newtype DescribeRiskConfigurationRequest _
derive instance repGenericDescribeRiskConfigurationRequest :: Generic DescribeRiskConfigurationRequest _
instance showDescribeRiskConfigurationRequest :: Show DescribeRiskConfigurationRequest where
  show = genericShow
instance decodeDescribeRiskConfigurationRequest :: Decode DescribeRiskConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRiskConfigurationRequest :: Encode DescribeRiskConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse 
  { "RiskConfiguration" :: (RiskConfigurationType)
  }
derive instance newtypeDescribeRiskConfigurationResponse :: Newtype DescribeRiskConfigurationResponse _
derive instance repGenericDescribeRiskConfigurationResponse :: Generic DescribeRiskConfigurationResponse _
instance showDescribeRiskConfigurationResponse :: Show DescribeRiskConfigurationResponse where
  show = genericShow
instance decodeDescribeRiskConfigurationResponse :: Decode DescribeRiskConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRiskConfigurationResponse :: Encode DescribeRiskConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to describe the user import job.</p>
newtype DescribeUserImportJobRequest = DescribeUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeDescribeUserImportJobRequest :: Newtype DescribeUserImportJobRequest _
derive instance repGenericDescribeUserImportJobRequest :: Generic DescribeUserImportJobRequest _
instance showDescribeUserImportJobRequest :: Show DescribeUserImportJobRequest where
  show = genericShow
instance decodeDescribeUserImportJobRequest :: Decode DescribeUserImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserImportJobRequest :: Encode DescribeUserImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to describe the user import job.</p>
newtype DescribeUserImportJobResponse = DescribeUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined.NullOrUndefined (UserImportJobType)
  }
derive instance newtypeDescribeUserImportJobResponse :: Newtype DescribeUserImportJobResponse _
derive instance repGenericDescribeUserImportJobResponse :: Generic DescribeUserImportJobResponse _
instance showDescribeUserImportJobResponse :: Show DescribeUserImportJobResponse where
  show = genericShow
instance decodeDescribeUserImportJobResponse :: Decode DescribeUserImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserImportJobResponse :: Encode DescribeUserImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to describe a user pool client.</p>
newtype DescribeUserPoolClientRequest = DescribeUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  }
derive instance newtypeDescribeUserPoolClientRequest :: Newtype DescribeUserPoolClientRequest _
derive instance repGenericDescribeUserPoolClientRequest :: Generic DescribeUserPoolClientRequest _
instance showDescribeUserPoolClientRequest :: Show DescribeUserPoolClientRequest where
  show = genericShow
instance decodeDescribeUserPoolClientRequest :: Decode DescribeUserPoolClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolClientRequest :: Encode DescribeUserPoolClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server from a request to describe the user pool client.</p>
newtype DescribeUserPoolClientResponse = DescribeUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined.NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeDescribeUserPoolClientResponse :: Newtype DescribeUserPoolClientResponse _
derive instance repGenericDescribeUserPoolClientResponse :: Generic DescribeUserPoolClientResponse _
instance showDescribeUserPoolClientResponse :: Show DescribeUserPoolClientResponse where
  show = genericShow
instance decodeDescribeUserPoolClientResponse :: Decode DescribeUserPoolClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolClientResponse :: Encode DescribeUserPoolClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUserPoolDomainRequest = DescribeUserPoolDomainRequest 
  { "Domain" :: (DomainType)
  }
derive instance newtypeDescribeUserPoolDomainRequest :: Newtype DescribeUserPoolDomainRequest _
derive instance repGenericDescribeUserPoolDomainRequest :: Generic DescribeUserPoolDomainRequest _
instance showDescribeUserPoolDomainRequest :: Show DescribeUserPoolDomainRequest where
  show = genericShow
instance decodeDescribeUserPoolDomainRequest :: Decode DescribeUserPoolDomainRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolDomainRequest :: Encode DescribeUserPoolDomainRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUserPoolDomainResponse = DescribeUserPoolDomainResponse 
  { "DomainDescription" :: NullOrUndefined.NullOrUndefined (DomainDescriptionType)
  }
derive instance newtypeDescribeUserPoolDomainResponse :: Newtype DescribeUserPoolDomainResponse _
derive instance repGenericDescribeUserPoolDomainResponse :: Generic DescribeUserPoolDomainResponse _
instance showDescribeUserPoolDomainResponse :: Show DescribeUserPoolDomainResponse where
  show = genericShow
instance decodeDescribeUserPoolDomainResponse :: Decode DescribeUserPoolDomainResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolDomainResponse :: Encode DescribeUserPoolDomainResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to describe the user pool.</p>
newtype DescribeUserPoolRequest = DescribeUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeDescribeUserPoolRequest :: Newtype DescribeUserPoolRequest _
derive instance repGenericDescribeUserPoolRequest :: Generic DescribeUserPoolRequest _
instance showDescribeUserPoolRequest :: Show DescribeUserPoolRequest where
  show = genericShow
instance decodeDescribeUserPoolRequest :: Decode DescribeUserPoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolRequest :: Encode DescribeUserPoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response to describe the user pool.</p>
newtype DescribeUserPoolResponse = DescribeUserPoolResponse 
  { "UserPool" :: NullOrUndefined.NullOrUndefined (UserPoolType)
  }
derive instance newtypeDescribeUserPoolResponse :: Newtype DescribeUserPoolResponse _
derive instance repGenericDescribeUserPoolResponse :: Generic DescribeUserPoolResponse _
instance showDescribeUserPoolResponse :: Show DescribeUserPoolResponse where
  show = genericShow
instance decodeDescribeUserPoolResponse :: Decode DescribeUserPoolResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUserPoolResponse :: Encode DescribeUserPoolResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescriptionType = DescriptionType String
derive instance newtypeDescriptionType :: Newtype DescriptionType _
derive instance repGenericDescriptionType :: Generic DescriptionType _
instance showDescriptionType :: Show DescriptionType where
  show = genericShow
instance decodeDescriptionType :: Decode DescriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescriptionType :: Encode DescriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration for the user pool's device tracking.</p>
newtype DeviceConfigurationType = DeviceConfigurationType 
  { "ChallengeRequiredOnNewDevice" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "DeviceOnlyRememberedOnUserPrompt" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeDeviceConfigurationType :: Newtype DeviceConfigurationType _
derive instance repGenericDeviceConfigurationType :: Generic DeviceConfigurationType _
instance showDeviceConfigurationType :: Show DeviceConfigurationType where
  show = genericShow
instance decodeDeviceConfigurationType :: Decode DeviceConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceConfigurationType :: Encode DeviceConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceKeyType = DeviceKeyType String
derive instance newtypeDeviceKeyType :: Newtype DeviceKeyType _
derive instance repGenericDeviceKeyType :: Generic DeviceKeyType _
instance showDeviceKeyType :: Show DeviceKeyType where
  show = genericShow
instance decodeDeviceKeyType :: Decode DeviceKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceKeyType :: Encode DeviceKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceListType = DeviceListType (Array DeviceType)
derive instance newtypeDeviceListType :: Newtype DeviceListType _
derive instance repGenericDeviceListType :: Generic DeviceListType _
instance showDeviceListType :: Show DeviceListType where
  show = genericShow
instance decodeDeviceListType :: Decode DeviceListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceListType :: Encode DeviceListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceNameType = DeviceNameType String
derive instance newtypeDeviceNameType :: Newtype DeviceNameType _
derive instance repGenericDeviceNameType :: Generic DeviceNameType _
instance showDeviceNameType :: Show DeviceNameType where
  show = genericShow
instance decodeDeviceNameType :: Decode DeviceNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceNameType :: Encode DeviceNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceRememberedStatusType = DeviceRememberedStatusType String
derive instance newtypeDeviceRememberedStatusType :: Newtype DeviceRememberedStatusType _
derive instance repGenericDeviceRememberedStatusType :: Generic DeviceRememberedStatusType _
instance showDeviceRememberedStatusType :: Show DeviceRememberedStatusType where
  show = genericShow
instance decodeDeviceRememberedStatusType :: Decode DeviceRememberedStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceRememberedStatusType :: Encode DeviceRememberedStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The device verifier against which it will be authenticated.</p>
newtype DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType 
  { "PasswordVerifier" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Salt" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeDeviceSecretVerifierConfigType :: Newtype DeviceSecretVerifierConfigType _
derive instance repGenericDeviceSecretVerifierConfigType :: Generic DeviceSecretVerifierConfigType _
instance showDeviceSecretVerifierConfigType :: Show DeviceSecretVerifierConfigType where
  show = genericShow
instance decodeDeviceSecretVerifierConfigType :: Decode DeviceSecretVerifierConfigType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceSecretVerifierConfigType :: Encode DeviceSecretVerifierConfigType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The device type.</p>
newtype DeviceType = DeviceType 
  { "DeviceKey" :: NullOrUndefined.NullOrUndefined (DeviceKeyType)
  , "DeviceAttributes" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "DeviceCreateDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "DeviceLastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "DeviceLastAuthenticatedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeDeviceType :: Newtype DeviceType _
derive instance repGenericDeviceType :: Generic DeviceType _
instance showDeviceType :: Show DeviceType where
  show = genericShow
instance decodeDeviceType :: Decode DeviceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceType :: Encode DeviceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for information about a domain.</p>
newtype DomainDescriptionType = DomainDescriptionType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "AWSAccountId" :: NullOrUndefined.NullOrUndefined (AWSAccountIdType)
  , "Domain" :: NullOrUndefined.NullOrUndefined (DomainType)
  , "S3Bucket" :: NullOrUndefined.NullOrUndefined (S3BucketType)
  , "CloudFrontDistribution" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "Version" :: NullOrUndefined.NullOrUndefined (DomainVersionType)
  , "Status" :: NullOrUndefined.NullOrUndefined (DomainStatusType)
  }
derive instance newtypeDomainDescriptionType :: Newtype DomainDescriptionType _
derive instance repGenericDomainDescriptionType :: Generic DomainDescriptionType _
instance showDomainDescriptionType :: Show DomainDescriptionType where
  show = genericShow
instance decodeDomainDescriptionType :: Decode DomainDescriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainDescriptionType :: Encode DomainDescriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainStatusType = DomainStatusType String
derive instance newtypeDomainStatusType :: Newtype DomainStatusType _
derive instance repGenericDomainStatusType :: Generic DomainStatusType _
instance showDomainStatusType :: Show DomainStatusType where
  show = genericShow
instance decodeDomainStatusType :: Decode DomainStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainStatusType :: Encode DomainStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainType = DomainType String
derive instance newtypeDomainType :: Newtype DomainType _
derive instance repGenericDomainType :: Generic DomainType _
instance showDomainType :: Show DomainType where
  show = genericShow
instance decodeDomainType :: Decode DomainType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainType :: Encode DomainType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainVersionType = DomainVersionType String
derive instance newtypeDomainVersionType :: Newtype DomainVersionType _
derive instance repGenericDomainVersionType :: Generic DomainVersionType _
instance showDomainVersionType :: Show DomainVersionType where
  show = genericShow
instance decodeDomainVersionType :: Decode DomainVersionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainVersionType :: Encode DomainVersionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provider is already supported by the user pool.</p>
newtype DuplicateProviderException = DuplicateProviderException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeDuplicateProviderException :: Newtype DuplicateProviderException _
derive instance repGenericDuplicateProviderException :: Generic DuplicateProviderException _
instance showDuplicateProviderException :: Show DuplicateProviderException where
  show = genericShow
instance decodeDuplicateProviderException :: Decode DuplicateProviderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateProviderException :: Encode DuplicateProviderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailAddressType = EmailAddressType String
derive instance newtypeEmailAddressType :: Newtype EmailAddressType _
derive instance repGenericEmailAddressType :: Generic EmailAddressType _
instance showEmailAddressType :: Show EmailAddressType where
  show = genericShow
instance decodeEmailAddressType :: Decode EmailAddressType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailAddressType :: Encode EmailAddressType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The email configuration type.</p>
newtype EmailConfigurationType = EmailConfigurationType 
  { "SourceArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "ReplyToEmailAddress" :: NullOrUndefined.NullOrUndefined (EmailAddressType)
  }
derive instance newtypeEmailConfigurationType :: Newtype EmailConfigurationType _
derive instance repGenericEmailConfigurationType :: Generic EmailConfigurationType _
instance showEmailConfigurationType :: Show EmailConfigurationType where
  show = genericShow
instance decodeEmailConfigurationType :: Decode EmailConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailConfigurationType :: Encode EmailConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailNotificationBodyType = EmailNotificationBodyType String
derive instance newtypeEmailNotificationBodyType :: Newtype EmailNotificationBodyType _
derive instance repGenericEmailNotificationBodyType :: Generic EmailNotificationBodyType _
instance showEmailNotificationBodyType :: Show EmailNotificationBodyType where
  show = genericShow
instance decodeEmailNotificationBodyType :: Decode EmailNotificationBodyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailNotificationBodyType :: Encode EmailNotificationBodyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailNotificationSubjectType = EmailNotificationSubjectType String
derive instance newtypeEmailNotificationSubjectType :: Newtype EmailNotificationSubjectType _
derive instance repGenericEmailNotificationSubjectType :: Generic EmailNotificationSubjectType _
instance showEmailNotificationSubjectType :: Show EmailNotificationSubjectType where
  show = genericShow
instance decodeEmailNotificationSubjectType :: Decode EmailNotificationSubjectType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailNotificationSubjectType :: Encode EmailNotificationSubjectType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailVerificationMessageByLinkType = EmailVerificationMessageByLinkType String
derive instance newtypeEmailVerificationMessageByLinkType :: Newtype EmailVerificationMessageByLinkType _
derive instance repGenericEmailVerificationMessageByLinkType :: Generic EmailVerificationMessageByLinkType _
instance showEmailVerificationMessageByLinkType :: Show EmailVerificationMessageByLinkType where
  show = genericShow
instance decodeEmailVerificationMessageByLinkType :: Decode EmailVerificationMessageByLinkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailVerificationMessageByLinkType :: Encode EmailVerificationMessageByLinkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailVerificationMessageType = EmailVerificationMessageType String
derive instance newtypeEmailVerificationMessageType :: Newtype EmailVerificationMessageType _
derive instance repGenericEmailVerificationMessageType :: Generic EmailVerificationMessageType _
instance showEmailVerificationMessageType :: Show EmailVerificationMessageType where
  show = genericShow
instance decodeEmailVerificationMessageType :: Decode EmailVerificationMessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailVerificationMessageType :: Encode EmailVerificationMessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailVerificationSubjectByLinkType = EmailVerificationSubjectByLinkType String
derive instance newtypeEmailVerificationSubjectByLinkType :: Newtype EmailVerificationSubjectByLinkType _
derive instance repGenericEmailVerificationSubjectByLinkType :: Generic EmailVerificationSubjectByLinkType _
instance showEmailVerificationSubjectByLinkType :: Show EmailVerificationSubjectByLinkType where
  show = genericShow
instance decodeEmailVerificationSubjectByLinkType :: Decode EmailVerificationSubjectByLinkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailVerificationSubjectByLinkType :: Encode EmailVerificationSubjectByLinkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailVerificationSubjectType = EmailVerificationSubjectType String
derive instance newtypeEmailVerificationSubjectType :: Newtype EmailVerificationSubjectType _
derive instance repGenericEmailVerificationSubjectType :: Generic EmailVerificationSubjectType _
instance showEmailVerificationSubjectType :: Show EmailVerificationSubjectType where
  show = genericShow
instance decodeEmailVerificationSubjectType :: Decode EmailVerificationSubjectType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailVerificationSubjectType :: Encode EmailVerificationSubjectType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).</p>
newtype EnableSoftwareTokenMFAException = EnableSoftwareTokenMFAException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeEnableSoftwareTokenMFAException :: Newtype EnableSoftwareTokenMFAException _
derive instance repGenericEnableSoftwareTokenMFAException :: Generic EnableSoftwareTokenMFAException _
instance showEnableSoftwareTokenMFAException :: Show EnableSoftwareTokenMFAException where
  show = genericShow
instance decodeEnableSoftwareTokenMFAException :: Decode EnableSoftwareTokenMFAException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSoftwareTokenMFAException :: Encode EnableSoftwareTokenMFAException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the user context data captured at the time of an event request.</p>
newtype EventContextDataType = EventContextDataType 
  { "IpAddress" :: NullOrUndefined.NullOrUndefined (StringType)
  , "DeviceName" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Timezone" :: NullOrUndefined.NullOrUndefined (StringType)
  , "City" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Country" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeEventContextDataType :: Newtype EventContextDataType _
derive instance repGenericEventContextDataType :: Generic EventContextDataType _
instance showEventContextDataType :: Show EventContextDataType where
  show = genericShow
instance decodeEventContextDataType :: Decode EventContextDataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventContextDataType :: Encode EventContextDataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the event feedback type.</p>
newtype EventFeedbackType = EventFeedbackType 
  { "FeedbackValue" :: (FeedbackValueType)
  , "Provider" :: (StringType)
  , "FeedbackDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeEventFeedbackType :: Newtype EventFeedbackType _
derive instance repGenericEventFeedbackType :: Generic EventFeedbackType _
instance showEventFeedbackType :: Show EventFeedbackType where
  show = genericShow
instance decodeEventFeedbackType :: Decode EventFeedbackType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventFeedbackType :: Encode EventFeedbackType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventFilterType = EventFilterType String
derive instance newtypeEventFilterType :: Newtype EventFilterType _
derive instance repGenericEventFilterType :: Generic EventFilterType _
instance showEventFilterType :: Show EventFilterType where
  show = genericShow
instance decodeEventFilterType :: Decode EventFilterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventFilterType :: Encode EventFilterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventFiltersType = EventFiltersType (Array EventFilterType)
derive instance newtypeEventFiltersType :: Newtype EventFiltersType _
derive instance repGenericEventFiltersType :: Generic EventFiltersType _
instance showEventFiltersType :: Show EventFiltersType where
  show = genericShow
instance decodeEventFiltersType :: Decode EventFiltersType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventFiltersType :: Encode EventFiltersType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventIdType = EventIdType String
derive instance newtypeEventIdType :: Newtype EventIdType _
derive instance repGenericEventIdType :: Generic EventIdType _
instance showEventIdType :: Show EventIdType where
  show = genericShow
instance decodeEventIdType :: Decode EventIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventIdType :: Encode EventIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventResponseType = EventResponseType String
derive instance newtypeEventResponseType :: Newtype EventResponseType _
derive instance repGenericEventResponseType :: Generic EventResponseType _
instance showEventResponseType :: Show EventResponseType where
  show = genericShow
instance decodeEventResponseType :: Decode EventResponseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventResponseType :: Encode EventResponseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The event risk type.</p>
newtype EventRiskType = EventRiskType 
  { "RiskDecision" :: NullOrUndefined.NullOrUndefined (RiskDecisionType)
  , "RiskLevel" :: NullOrUndefined.NullOrUndefined (RiskLevelType)
  }
derive instance newtypeEventRiskType :: Newtype EventRiskType _
derive instance repGenericEventRiskType :: Generic EventRiskType _
instance showEventRiskType :: Show EventRiskType where
  show = genericShow
instance decodeEventRiskType :: Decode EventRiskType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventRiskType :: Encode EventRiskType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _
derive instance repGenericEventType :: Generic EventType _
instance showEventType :: Show EventType where
  show = genericShow
instance decodeEventType :: Decode EventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType :: Encode EventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown if a code has expired.</p>
newtype ExpiredCodeException = ExpiredCodeException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeExpiredCodeException :: Newtype ExpiredCodeException _
derive instance repGenericExpiredCodeException :: Generic ExpiredCodeException _
instance showExpiredCodeException :: Show ExpiredCodeException where
  show = genericShow
instance decodeExpiredCodeException :: Decode ExpiredCodeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredCodeException :: Encode ExpiredCodeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExplicitAuthFlowsListType = ExplicitAuthFlowsListType (Array ExplicitAuthFlowsType)
derive instance newtypeExplicitAuthFlowsListType :: Newtype ExplicitAuthFlowsListType _
derive instance repGenericExplicitAuthFlowsListType :: Generic ExplicitAuthFlowsListType _
instance showExplicitAuthFlowsListType :: Show ExplicitAuthFlowsListType where
  show = genericShow
instance decodeExplicitAuthFlowsListType :: Decode ExplicitAuthFlowsListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExplicitAuthFlowsListType :: Encode ExplicitAuthFlowsListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExplicitAuthFlowsType = ExplicitAuthFlowsType String
derive instance newtypeExplicitAuthFlowsType :: Newtype ExplicitAuthFlowsType _
derive instance repGenericExplicitAuthFlowsType :: Generic ExplicitAuthFlowsType _
instance showExplicitAuthFlowsType :: Show ExplicitAuthFlowsType where
  show = genericShow
instance decodeExplicitAuthFlowsType :: Decode ExplicitAuthFlowsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExplicitAuthFlowsType :: Encode ExplicitAuthFlowsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FeedbackValueType = FeedbackValueType String
derive instance newtypeFeedbackValueType :: Newtype FeedbackValueType _
derive instance repGenericFeedbackValueType :: Generic FeedbackValueType _
instance showFeedbackValueType :: Show FeedbackValueType where
  show = genericShow
instance decodeFeedbackValueType :: Decode FeedbackValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFeedbackValueType :: Encode FeedbackValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ForceAliasCreation = ForceAliasCreation Boolean
derive instance newtypeForceAliasCreation :: Newtype ForceAliasCreation _
derive instance repGenericForceAliasCreation :: Generic ForceAliasCreation _
instance showForceAliasCreation :: Show ForceAliasCreation where
  show = genericShow
instance decodeForceAliasCreation :: Decode ForceAliasCreation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForceAliasCreation :: Encode ForceAliasCreation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to forget the device.</p>
newtype ForgetDeviceRequest = ForgetDeviceRequest 
  { "AccessToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  }
derive instance newtypeForgetDeviceRequest :: Newtype ForgetDeviceRequest _
derive instance repGenericForgetDeviceRequest :: Generic ForgetDeviceRequest _
instance showForgetDeviceRequest :: Show ForgetDeviceRequest where
  show = genericShow
instance decodeForgetDeviceRequest :: Decode ForgetDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForgetDeviceRequest :: Encode ForgetDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to reset a user's password.</p>
newtype ForgotPasswordRequest = ForgotPasswordRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined.NullOrUndefined (SecretHashType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  , "Username" :: (UsernameType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  }
derive instance newtypeForgotPasswordRequest :: Newtype ForgotPasswordRequest _
derive instance repGenericForgotPasswordRequest :: Generic ForgotPasswordRequest _
instance showForgotPasswordRequest :: Show ForgotPasswordRequest where
  show = genericShow
instance decodeForgotPasswordRequest :: Decode ForgotPasswordRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForgotPasswordRequest :: Encode ForgotPasswordRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Respresents the response from the server regarding the request to reset a password.</p>
newtype ForgotPasswordResponse = ForgotPasswordResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined.NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeForgotPasswordResponse :: Newtype ForgotPasswordResponse _
derive instance repGenericForgotPasswordResponse :: Generic ForgotPasswordResponse _
instance showForgotPasswordResponse :: Show ForgotPasswordResponse where
  show = genericShow
instance decodeForgotPasswordResponse :: Decode ForgotPasswordResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForgotPasswordResponse :: Encode ForgotPasswordResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenerateSecret = GenerateSecret Boolean
derive instance newtypeGenerateSecret :: Newtype GenerateSecret _
derive instance repGenericGenerateSecret :: Generic GenerateSecret _
instance showGenerateSecret :: Show GenerateSecret where
  show = genericShow
instance decodeGenerateSecret :: Decode GenerateSecret where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenerateSecret :: Encode GenerateSecret where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get the header information for the .csv file for the user import job.</p>
newtype GetCSVHeaderRequest = GetCSVHeaderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetCSVHeaderRequest :: Newtype GetCSVHeaderRequest _
derive instance repGenericGetCSVHeaderRequest :: Generic GetCSVHeaderRequest _
instance showGetCSVHeaderRequest :: Show GetCSVHeaderRequest where
  show = genericShow
instance decodeGetCSVHeaderRequest :: Decode GetCSVHeaderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCSVHeaderRequest :: Encode GetCSVHeaderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to get the header information for the .csv file for the user import job.</p>
newtype GetCSVHeaderResponse = GetCSVHeaderResponse 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "CSVHeader" :: NullOrUndefined.NullOrUndefined (ListOfStringTypes)
  }
derive instance newtypeGetCSVHeaderResponse :: Newtype GetCSVHeaderResponse _
derive instance repGenericGetCSVHeaderResponse :: Generic GetCSVHeaderResponse _
instance showGetCSVHeaderResponse :: Show GetCSVHeaderResponse where
  show = genericShow
instance decodeGetCSVHeaderResponse :: Decode GetCSVHeaderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCSVHeaderResponse :: Encode GetCSVHeaderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get the device.</p>
newtype GetDeviceRequest = GetDeviceRequest 
  { "DeviceKey" :: (DeviceKeyType)
  , "AccessToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  }
derive instance newtypeGetDeviceRequest :: Newtype GetDeviceRequest _
derive instance repGenericGetDeviceRequest :: Generic GetDeviceRequest _
instance showGetDeviceRequest :: Show GetDeviceRequest where
  show = genericShow
instance decodeGetDeviceRequest :: Decode GetDeviceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceRequest :: Encode GetDeviceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets the device response.</p>
newtype GetDeviceResponse = GetDeviceResponse 
  { "Device" :: (DeviceType)
  }
derive instance newtypeGetDeviceResponse :: Newtype GetDeviceResponse _
derive instance repGenericGetDeviceResponse :: Generic GetDeviceResponse _
instance showGetDeviceResponse :: Show GetDeviceResponse where
  show = genericShow
instance decodeGetDeviceResponse :: Decode GetDeviceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeviceResponse :: Encode GetDeviceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupRequest = GetGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetGroupRequest :: Newtype GetGroupRequest _
derive instance repGenericGetGroupRequest :: Generic GetGroupRequest _
instance showGetGroupRequest :: Show GetGroupRequest where
  show = genericShow
instance decodeGetGroupRequest :: Decode GetGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupRequest :: Encode GetGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetGroupResponse = GetGroupResponse 
  { "Group" :: NullOrUndefined.NullOrUndefined (GroupType)
  }
derive instance newtypeGetGroupResponse :: Newtype GetGroupResponse _
derive instance repGenericGetGroupResponse :: Generic GetGroupResponse _
instance showGetGroupResponse :: Show GetGroupResponse where
  show = genericShow
instance decodeGetGroupResponse :: Decode GetGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGroupResponse :: Encode GetGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIdentityProviderByIdentifierRequest = GetIdentityProviderByIdentifierRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "IdpIdentifier" :: (IdpIdentifierType)
  }
derive instance newtypeGetIdentityProviderByIdentifierRequest :: Newtype GetIdentityProviderByIdentifierRequest _
derive instance repGenericGetIdentityProviderByIdentifierRequest :: Generic GetIdentityProviderByIdentifierRequest _
instance showGetIdentityProviderByIdentifierRequest :: Show GetIdentityProviderByIdentifierRequest where
  show = genericShow
instance decodeGetIdentityProviderByIdentifierRequest :: Decode GetIdentityProviderByIdentifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityProviderByIdentifierRequest :: Encode GetIdentityProviderByIdentifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIdentityProviderByIdentifierResponse = GetIdentityProviderByIdentifierResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeGetIdentityProviderByIdentifierResponse :: Newtype GetIdentityProviderByIdentifierResponse _
derive instance repGenericGetIdentityProviderByIdentifierResponse :: Generic GetIdentityProviderByIdentifierResponse _
instance showGetIdentityProviderByIdentifierResponse :: Show GetIdentityProviderByIdentifierResponse where
  show = genericShow
instance decodeGetIdentityProviderByIdentifierResponse :: Decode GetIdentityProviderByIdentifierResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityProviderByIdentifierResponse :: Encode GetIdentityProviderByIdentifierResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to get a signing certificate from Cognito.</p>
newtype GetSigningCertificateRequest = GetSigningCertificateRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetSigningCertificateRequest :: Newtype GetSigningCertificateRequest _
derive instance repGenericGetSigningCertificateRequest :: Generic GetSigningCertificateRequest _
instance showGetSigningCertificateRequest :: Show GetSigningCertificateRequest where
  show = genericShow
instance decodeGetSigningCertificateRequest :: Decode GetSigningCertificateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSigningCertificateRequest :: Encode GetSigningCertificateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response from Cognito for a signing certificate request.</p>
newtype GetSigningCertificateResponse = GetSigningCertificateResponse 
  { "Certificate" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeGetSigningCertificateResponse :: Newtype GetSigningCertificateResponse _
derive instance repGenericGetSigningCertificateResponse :: Generic GetSigningCertificateResponse _
instance showGetSigningCertificateResponse :: Show GetSigningCertificateResponse where
  show = genericShow
instance decodeGetSigningCertificateResponse :: Decode GetSigningCertificateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSigningCertificateResponse :: Encode GetSigningCertificateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUICustomizationRequest = GetUICustomizationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  }
derive instance newtypeGetUICustomizationRequest :: Newtype GetUICustomizationRequest _
derive instance repGenericGetUICustomizationRequest :: Generic GetUICustomizationRequest _
instance showGetUICustomizationRequest :: Show GetUICustomizationRequest where
  show = genericShow
instance decodeGetUICustomizationRequest :: Decode GetUICustomizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUICustomizationRequest :: Encode GetUICustomizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUICustomizationResponse = GetUICustomizationResponse 
  { "UICustomization" :: (UICustomizationType)
  }
derive instance newtypeGetUICustomizationResponse :: Newtype GetUICustomizationResponse _
derive instance repGenericGetUICustomizationResponse :: Generic GetUICustomizationResponse _
instance showGetUICustomizationResponse :: Show GetUICustomizationResponse where
  show = genericShow
instance decodeGetUICustomizationResponse :: Decode GetUICustomizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUICustomizationResponse :: Encode GetUICustomizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get user attribute verification.</p>
newtype GetUserAttributeVerificationCodeRequest = GetUserAttributeVerificationCodeRequest 
  { "AccessToken" :: (TokenModelType)
  , "AttributeName" :: (AttributeNameType)
  }
derive instance newtypeGetUserAttributeVerificationCodeRequest :: Newtype GetUserAttributeVerificationCodeRequest _
derive instance repGenericGetUserAttributeVerificationCodeRequest :: Generic GetUserAttributeVerificationCodeRequest _
instance showGetUserAttributeVerificationCodeRequest :: Show GetUserAttributeVerificationCodeRequest where
  show = genericShow
instance decodeGetUserAttributeVerificationCodeRequest :: Decode GetUserAttributeVerificationCodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserAttributeVerificationCodeRequest :: Encode GetUserAttributeVerificationCodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The verification code response returned by the server response to get the user attribute verification code.</p>
newtype GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined.NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeGetUserAttributeVerificationCodeResponse :: Newtype GetUserAttributeVerificationCodeResponse _
derive instance repGenericGetUserAttributeVerificationCodeResponse :: Generic GetUserAttributeVerificationCodeResponse _
instance showGetUserAttributeVerificationCodeResponse :: Show GetUserAttributeVerificationCodeResponse where
  show = genericShow
instance decodeGetUserAttributeVerificationCodeResponse :: Decode GetUserAttributeVerificationCodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserAttributeVerificationCodeResponse :: Encode GetUserAttributeVerificationCodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserPoolMfaConfigRequest = GetUserPoolMfaConfigRequest 
  { "UserPoolId" :: (UserPoolIdType)
  }
derive instance newtypeGetUserPoolMfaConfigRequest :: Newtype GetUserPoolMfaConfigRequest _
derive instance repGenericGetUserPoolMfaConfigRequest :: Generic GetUserPoolMfaConfigRequest _
instance showGetUserPoolMfaConfigRequest :: Show GetUserPoolMfaConfigRequest where
  show = genericShow
instance decodeGetUserPoolMfaConfigRequest :: Decode GetUserPoolMfaConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserPoolMfaConfigRequest :: Encode GetUserPoolMfaConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserPoolMfaConfigResponse = GetUserPoolMfaConfigResponse 
  { "SmsMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeGetUserPoolMfaConfigResponse :: Newtype GetUserPoolMfaConfigResponse _
derive instance repGenericGetUserPoolMfaConfigResponse :: Generic GetUserPoolMfaConfigResponse _
instance showGetUserPoolMfaConfigResponse :: Show GetUserPoolMfaConfigResponse where
  show = genericShow
instance decodeGetUserPoolMfaConfigResponse :: Decode GetUserPoolMfaConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserPoolMfaConfigResponse :: Encode GetUserPoolMfaConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to get information about the user.</p>
newtype GetUserRequest = GetUserRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeGetUserRequest :: Newtype GetUserRequest _
derive instance repGenericGetUserRequest :: Generic GetUserRequest _
instance showGetUserRequest :: Show GetUserRequest where
  show = genericShow
instance decodeGetUserRequest :: Decode GetUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserRequest :: Encode GetUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server from the request to get information about the user.</p>
newtype GetUserResponse = GetUserResponse 
  { "Username" :: (UsernameType)
  , "UserAttributes" :: (AttributeListType)
  , "MFAOptions" :: NullOrUndefined.NullOrUndefined (MFAOptionListType)
  , "PreferredMfaSetting" :: NullOrUndefined.NullOrUndefined (StringType)
  , "UserMFASettingList" :: NullOrUndefined.NullOrUndefined (UserMFASettingListType)
  }
derive instance newtypeGetUserResponse :: Newtype GetUserResponse _
derive instance repGenericGetUserResponse :: Generic GetUserResponse _
instance showGetUserResponse :: Show GetUserResponse where
  show = genericShow
instance decodeGetUserResponse :: Decode GetUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserResponse :: Encode GetUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to sign out all devices.</p>
newtype GlobalSignOutRequest = GlobalSignOutRequest 
  { "AccessToken" :: (TokenModelType)
  }
derive instance newtypeGlobalSignOutRequest :: Newtype GlobalSignOutRequest _
derive instance repGenericGlobalSignOutRequest :: Generic GlobalSignOutRequest _
instance showGlobalSignOutRequest :: Show GlobalSignOutRequest where
  show = genericShow
instance decodeGlobalSignOutRequest :: Decode GlobalSignOutRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalSignOutRequest :: Encode GlobalSignOutRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to the request to sign out all devices.</p>
newtype GlobalSignOutResponse = GlobalSignOutResponse Types.NoArguments
derive instance newtypeGlobalSignOutResponse :: Newtype GlobalSignOutResponse _
derive instance repGenericGlobalSignOutResponse :: Generic GlobalSignOutResponse _
instance showGlobalSignOutResponse :: Show GlobalSignOutResponse where
  show = genericShow
instance decodeGlobalSignOutResponse :: Decode GlobalSignOutResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalSignOutResponse :: Encode GlobalSignOutResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.</p>
newtype GroupExistsException = GroupExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeGroupExistsException :: Newtype GroupExistsException _
derive instance repGenericGroupExistsException :: Generic GroupExistsException _
instance showGroupExistsException :: Show GroupExistsException where
  show = genericShow
instance decodeGroupExistsException :: Decode GroupExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupExistsException :: Encode GroupExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupListType = GroupListType (Array GroupType)
derive instance newtypeGroupListType :: Newtype GroupListType _
derive instance repGenericGroupListType :: Generic GroupListType _
instance showGroupListType :: Show GroupListType where
  show = genericShow
instance decodeGroupListType :: Decode GroupListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupListType :: Encode GroupListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupNameType = GroupNameType String
derive instance newtypeGroupNameType :: Newtype GroupNameType _
derive instance repGenericGroupNameType :: Generic GroupNameType _
instance showGroupNameType :: Show GroupNameType where
  show = genericShow
instance decodeGroupNameType :: Decode GroupNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupNameType :: Encode GroupNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The group type.</p>
newtype GroupType = GroupType 
  { "GroupName" :: NullOrUndefined.NullOrUndefined (GroupNameType)
  , "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined.NullOrUndefined (PrecedenceType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeGroupType :: Newtype GroupType _
derive instance repGenericGroupType :: Generic GroupType _
instance showGroupType :: Show GroupType where
  show = genericShow
instance decodeGroupType :: Decode GroupType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupType :: Encode GroupType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HexStringType = HexStringType String
derive instance newtypeHexStringType :: Newtype HexStringType _
derive instance repGenericHexStringType :: Generic HexStringType _
instance showHexStringType :: Show HexStringType where
  show = genericShow
instance decodeHexStringType :: Decode HexStringType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHexStringType :: Encode HexStringType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The HTTP header.</p>
newtype HttpHeader = HttpHeader 
  { "HeaderName'" :: NullOrUndefined.NullOrUndefined (StringType)
  , "HeaderValue'" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeHttpHeader :: Newtype HttpHeader _
derive instance repGenericHttpHeader :: Generic HttpHeader _
instance showHttpHeader :: Show HttpHeader where
  show = genericShow
instance decodeHttpHeader :: Decode HttpHeader where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpHeader :: Encode HttpHeader where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpHeaderList = HttpHeaderList (Array HttpHeader)
derive instance newtypeHttpHeaderList :: Newtype HttpHeaderList _
derive instance repGenericHttpHeaderList :: Generic HttpHeaderList _
instance showHttpHeaderList :: Show HttpHeaderList where
  show = genericShow
instance decodeHttpHeaderList :: Decode HttpHeaderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpHeaderList :: Encode HttpHeaderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for information about an identity provider.</p>
newtype IdentityProviderType = IdentityProviderType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "ProviderName" :: NullOrUndefined.NullOrUndefined (ProviderNameType)
  , "ProviderType" :: NullOrUndefined.NullOrUndefined (IdentityProviderTypeType)
  , "ProviderDetails" :: NullOrUndefined.NullOrUndefined (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined.NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined.NullOrUndefined (IdpIdentifiersListType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeIdentityProviderType :: Newtype IdentityProviderType _
derive instance repGenericIdentityProviderType :: Generic IdentityProviderType _
instance showIdentityProviderType :: Show IdentityProviderType where
  show = genericShow
instance decodeIdentityProviderType :: Decode IdentityProviderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviderType :: Encode IdentityProviderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityProviderTypeType = IdentityProviderTypeType String
derive instance newtypeIdentityProviderTypeType :: Newtype IdentityProviderTypeType _
derive instance repGenericIdentityProviderTypeType :: Generic IdentityProviderTypeType _
instance showIdentityProviderTypeType :: Show IdentityProviderTypeType where
  show = genericShow
instance decodeIdentityProviderTypeType :: Decode IdentityProviderTypeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviderTypeType :: Encode IdentityProviderTypeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdpIdentifierType = IdpIdentifierType String
derive instance newtypeIdpIdentifierType :: Newtype IdpIdentifierType _
derive instance repGenericIdpIdentifierType :: Generic IdpIdentifierType _
instance showIdpIdentifierType :: Show IdpIdentifierType where
  show = genericShow
instance decodeIdpIdentifierType :: Decode IdpIdentifierType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdpIdentifierType :: Encode IdpIdentifierType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdpIdentifiersListType = IdpIdentifiersListType (Array IdpIdentifierType)
derive instance newtypeIdpIdentifiersListType :: Newtype IdpIdentifiersListType _
derive instance repGenericIdpIdentifiersListType :: Generic IdpIdentifiersListType _
instance showIdpIdentifiersListType :: Show IdpIdentifiersListType where
  show = genericShow
instance decodeIdpIdentifiersListType :: Decode IdpIdentifiersListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdpIdentifiersListType :: Encode IdpIdentifiersListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageFileType = ImageFileType String
derive instance newtypeImageFileType :: Newtype ImageFileType _
derive instance repGenericImageFileType :: Generic ImageFileType _
instance showImageFileType :: Show ImageFileType where
  show = genericShow
instance decodeImageFileType :: Decode ImageFileType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageFileType :: Encode ImageFileType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageUrlType = ImageUrlType String
derive instance newtypeImageUrlType :: Newtype ImageUrlType _
derive instance repGenericImageUrlType :: Generic ImageUrlType _
instance showImageUrlType :: Show ImageUrlType where
  show = genericShow
instance decodeImageUrlType :: Decode ImageUrlType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageUrlType :: Encode ImageUrlType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the authentication request.</p>
newtype InitiateAuthRequest = InitiateAuthRequest 
  { "AuthFlow" :: (AuthFlowType)
  , "AuthParameters" :: NullOrUndefined.NullOrUndefined (AuthParametersType)
  , "ClientMetadata" :: NullOrUndefined.NullOrUndefined (ClientMetadataType)
  , "ClientId" :: (ClientIdType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  }
derive instance newtypeInitiateAuthRequest :: Newtype InitiateAuthRequest _
derive instance repGenericInitiateAuthRequest :: Generic InitiateAuthRequest _
instance showInitiateAuthRequest :: Show InitiateAuthRequest where
  show = genericShow
instance decodeInitiateAuthRequest :: Decode InitiateAuthRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateAuthRequest :: Encode InitiateAuthRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the authentication response.</p>
newtype InitiateAuthResponse = InitiateAuthResponse 
  { "ChallengeName" :: NullOrUndefined.NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined.NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined.NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeInitiateAuthResponse :: Newtype InitiateAuthResponse _
derive instance repGenericInitiateAuthResponse :: Generic InitiateAuthResponse _
instance showInitiateAuthResponse :: Show InitiateAuthResponse where
  show = genericShow
instance decodeInitiateAuthResponse :: Decode InitiateAuthResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateAuthResponse :: Encode InitiateAuthResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerType = IntegerType Int
derive instance newtypeIntegerType :: Newtype IntegerType _
derive instance repGenericIntegerType :: Generic IntegerType _
instance showIntegerType :: Show IntegerType where
  show = genericShow
instance decodeIntegerType :: Decode IntegerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerType :: Encode IntegerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when Amazon Cognito encounters an internal error.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.</p>
newtype InvalidEmailRoleAccessPolicyException = InvalidEmailRoleAccessPolicyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidEmailRoleAccessPolicyException :: Newtype InvalidEmailRoleAccessPolicyException _
derive instance repGenericInvalidEmailRoleAccessPolicyException :: Generic InvalidEmailRoleAccessPolicyException _
instance showInvalidEmailRoleAccessPolicyException :: Show InvalidEmailRoleAccessPolicyException where
  show = genericShow
instance decodeInvalidEmailRoleAccessPolicyException :: Decode InvalidEmailRoleAccessPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEmailRoleAccessPolicyException :: Encode InvalidEmailRoleAccessPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.</p>
newtype InvalidLambdaResponseException = InvalidLambdaResponseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidLambdaResponseException :: Newtype InvalidLambdaResponseException _
derive instance repGenericInvalidLambdaResponseException :: Generic InvalidLambdaResponseException _
instance showInvalidLambdaResponseException :: Show InvalidLambdaResponseException where
  show = genericShow
instance decodeInvalidLambdaResponseException :: Decode InvalidLambdaResponseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLambdaResponseException :: Encode InvalidLambdaResponseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified OAuth flow is invalid.</p>
newtype InvalidOAuthFlowException = InvalidOAuthFlowException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidOAuthFlowException :: Newtype InvalidOAuthFlowException _
derive instance repGenericInvalidOAuthFlowException :: Generic InvalidOAuthFlowException _
instance showInvalidOAuthFlowException :: Show InvalidOAuthFlowException where
  show = genericShow
instance decodeInvalidOAuthFlowException :: Decode InvalidOAuthFlowException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOAuthFlowException :: Encode InvalidOAuthFlowException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid parameter.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service encounters an invalid password.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidPasswordException :: Newtype InvalidPasswordException _
derive instance repGenericInvalidPasswordException :: Generic InvalidPasswordException _
instance showInvalidPasswordException :: Show InvalidPasswordException where
  show = genericShow
instance decodeInvalidPasswordException :: Decode InvalidPasswordException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPasswordException :: Encode InvalidPasswordException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.</p>
newtype InvalidSmsRoleAccessPolicyException = InvalidSmsRoleAccessPolicyException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidSmsRoleAccessPolicyException :: Newtype InvalidSmsRoleAccessPolicyException _
derive instance repGenericInvalidSmsRoleAccessPolicyException :: Generic InvalidSmsRoleAccessPolicyException _
instance showInvalidSmsRoleAccessPolicyException :: Show InvalidSmsRoleAccessPolicyException where
  show = genericShow
instance decodeInvalidSmsRoleAccessPolicyException :: Decode InvalidSmsRoleAccessPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSmsRoleAccessPolicyException :: Encode InvalidSmsRoleAccessPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust <b>cognito-idp.amazonaws.com</b> or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.</p>
newtype InvalidSmsRoleTrustRelationshipException = InvalidSmsRoleTrustRelationshipException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidSmsRoleTrustRelationshipException :: Newtype InvalidSmsRoleTrustRelationshipException _
derive instance repGenericInvalidSmsRoleTrustRelationshipException :: Generic InvalidSmsRoleTrustRelationshipException _
instance showInvalidSmsRoleTrustRelationshipException :: Show InvalidSmsRoleTrustRelationshipException where
  show = genericShow
instance decodeInvalidSmsRoleTrustRelationshipException :: Decode InvalidSmsRoleTrustRelationshipException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSmsRoleTrustRelationshipException :: Encode InvalidSmsRoleTrustRelationshipException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the user pool configuration is invalid.</p>
newtype InvalidUserPoolConfigurationException = InvalidUserPoolConfigurationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeInvalidUserPoolConfigurationException :: Newtype InvalidUserPoolConfigurationException _
derive instance repGenericInvalidUserPoolConfigurationException :: Generic InvalidUserPoolConfigurationException _
instance showInvalidUserPoolConfigurationException :: Show InvalidUserPoolConfigurationException where
  show = genericShow
instance decodeInvalidUserPoolConfigurationException :: Decode InvalidUserPoolConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidUserPoolConfigurationException :: Encode InvalidUserPoolConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the configuration for AWS Lambda triggers.</p>
newtype LambdaConfigType = LambdaConfigType 
  { "PreSignUp" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "CustomMessage" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "PostConfirmation" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "PreAuthentication" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "PostAuthentication" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "DefineAuthChallenge" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "CreateAuthChallenge" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "VerifyAuthChallengeResponse" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "PreTokenGeneration" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "UserMigration" :: NullOrUndefined.NullOrUndefined (ArnType)
  }
derive instance newtypeLambdaConfigType :: Newtype LambdaConfigType _
derive instance repGenericLambdaConfigType :: Generic LambdaConfigType _
instance showLambdaConfigType :: Show LambdaConfigType where
  show = genericShow
instance decodeLambdaConfigType :: Decode LambdaConfigType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaConfigType :: Encode LambdaConfigType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user exceeds the limit for a requested AWS resource.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list the devices.</p>
newtype ListDevicesRequest = ListDevicesRequest 
  { "AccessToken" :: (TokenModelType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListDevicesRequest :: Newtype ListDevicesRequest _
derive instance repGenericListDevicesRequest :: Generic ListDevicesRequest _
instance showListDevicesRequest :: Show ListDevicesRequest where
  show = genericShow
instance decodeListDevicesRequest :: Decode ListDevicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicesRequest :: Encode ListDevicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response to list devices.</p>
newtype ListDevicesResponse = ListDevicesResponse 
  { "Devices" :: NullOrUndefined.NullOrUndefined (DeviceListType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListDevicesResponse :: Newtype ListDevicesResponse _
derive instance repGenericListDevicesResponse :: Generic ListDevicesResponse _
instance showListDevicesResponse :: Show ListDevicesResponse where
  show = genericShow
instance decodeListDevicesResponse :: Decode ListDevicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDevicesResponse :: Encode ListDevicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsRequest = ListGroupsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListGroupsRequest :: Newtype ListGroupsRequest _
derive instance repGenericListGroupsRequest :: Generic ListGroupsRequest _
instance showListGroupsRequest :: Show ListGroupsRequest where
  show = genericShow
instance decodeListGroupsRequest :: Decode ListGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsRequest :: Encode ListGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListGroupsResponse = ListGroupsResponse 
  { "Groups" :: NullOrUndefined.NullOrUndefined (GroupListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListGroupsResponse :: Newtype ListGroupsResponse _
derive instance repGenericListGroupsResponse :: Generic ListGroupsResponse _
instance showListGroupsResponse :: Show ListGroupsResponse where
  show = genericShow
instance decodeListGroupsResponse :: Decode ListGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGroupsResponse :: Encode ListGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIdentityProvidersRequest = ListIdentityProvidersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (ListProvidersLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListIdentityProvidersRequest :: Newtype ListIdentityProvidersRequest _
derive instance repGenericListIdentityProvidersRequest :: Generic ListIdentityProvidersRequest _
instance showListIdentityProvidersRequest :: Show ListIdentityProvidersRequest where
  show = genericShow
instance decodeListIdentityProvidersRequest :: Decode ListIdentityProvidersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityProvidersRequest :: Encode ListIdentityProvidersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIdentityProvidersResponse = ListIdentityProvidersResponse 
  { "Providers" :: (ProvidersListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListIdentityProvidersResponse :: Newtype ListIdentityProvidersResponse _
derive instance repGenericListIdentityProvidersResponse :: Generic ListIdentityProvidersResponse _
instance showListIdentityProvidersResponse :: Show ListIdentityProvidersResponse where
  show = genericShow
instance decodeListIdentityProvidersResponse :: Decode ListIdentityProvidersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityProvidersResponse :: Encode ListIdentityProvidersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfStringTypes = ListOfStringTypes (Array StringType)
derive instance newtypeListOfStringTypes :: Newtype ListOfStringTypes _
derive instance repGenericListOfStringTypes :: Generic ListOfStringTypes _
instance showListOfStringTypes :: Show ListOfStringTypes where
  show = genericShow
instance decodeListOfStringTypes :: Decode ListOfStringTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfStringTypes :: Encode ListOfStringTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListProvidersLimitType = ListProvidersLimitType Int
derive instance newtypeListProvidersLimitType :: Newtype ListProvidersLimitType _
derive instance repGenericListProvidersLimitType :: Generic ListProvidersLimitType _
instance showListProvidersLimitType :: Show ListProvidersLimitType where
  show = genericShow
instance decodeListProvidersLimitType :: Decode ListProvidersLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProvidersLimitType :: Encode ListProvidersLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceServersLimitType = ListResourceServersLimitType Int
derive instance newtypeListResourceServersLimitType :: Newtype ListResourceServersLimitType _
derive instance repGenericListResourceServersLimitType :: Generic ListResourceServersLimitType _
instance showListResourceServersLimitType :: Show ListResourceServersLimitType where
  show = genericShow
instance decodeListResourceServersLimitType :: Decode ListResourceServersLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceServersLimitType :: Encode ListResourceServersLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceServersRequest = ListResourceServersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (ListResourceServersLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListResourceServersRequest :: Newtype ListResourceServersRequest _
derive instance repGenericListResourceServersRequest :: Generic ListResourceServersRequest _
instance showListResourceServersRequest :: Show ListResourceServersRequest where
  show = genericShow
instance decodeListResourceServersRequest :: Decode ListResourceServersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceServersRequest :: Encode ListResourceServersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceServersResponse = ListResourceServersResponse 
  { "ResourceServers" :: (ResourceServersListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListResourceServersResponse :: Newtype ListResourceServersResponse _
derive instance repGenericListResourceServersResponse :: Generic ListResourceServersResponse _
instance showListResourceServersResponse :: Show ListResourceServersResponse where
  show = genericShow
instance decodeListResourceServersResponse :: Decode ListResourceServersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceServersResponse :: Encode ListResourceServersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list the user import jobs.</p>
newtype ListUserImportJobsRequest = ListUserImportJobsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: (PoolQueryLimitType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserImportJobsRequest :: Newtype ListUserImportJobsRequest _
derive instance repGenericListUserImportJobsRequest :: Generic ListUserImportJobsRequest _
instance showListUserImportJobsRequest :: Show ListUserImportJobsRequest where
  show = genericShow
instance decodeListUserImportJobsRequest :: Decode ListUserImportJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserImportJobsRequest :: Encode ListUserImportJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to list the user import jobs.</p>
newtype ListUserImportJobsResponse = ListUserImportJobsResponse 
  { "UserImportJobs" :: NullOrUndefined.NullOrUndefined (UserImportJobsListType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserImportJobsResponse :: Newtype ListUserImportJobsResponse _
derive instance repGenericListUserImportJobsResponse :: Generic ListUserImportJobsResponse _
instance showListUserImportJobsResponse :: Show ListUserImportJobsResponse where
  show = genericShow
instance decodeListUserImportJobsResponse :: Decode ListUserImportJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserImportJobsResponse :: Encode ListUserImportJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list the user pool clients.</p>
newtype ListUserPoolClientsRequest = ListUserPoolClientsRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (QueryLimit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUserPoolClientsRequest :: Newtype ListUserPoolClientsRequest _
derive instance repGenericListUserPoolClientsRequest :: Generic ListUserPoolClientsRequest _
instance showListUserPoolClientsRequest :: Show ListUserPoolClientsRequest where
  show = genericShow
instance decodeListUserPoolClientsRequest :: Decode ListUserPoolClientsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserPoolClientsRequest :: Encode ListUserPoolClientsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server that lists user pool clients.</p>
newtype ListUserPoolClientsResponse = ListUserPoolClientsResponse 
  { "UserPoolClients" :: NullOrUndefined.NullOrUndefined (UserPoolClientListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUserPoolClientsResponse :: Newtype ListUserPoolClientsResponse _
derive instance repGenericListUserPoolClientsResponse :: Generic ListUserPoolClientsResponse _
instance showListUserPoolClientsResponse :: Show ListUserPoolClientsResponse where
  show = genericShow
instance decodeListUserPoolClientsResponse :: Decode ListUserPoolClientsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserPoolClientsResponse :: Encode ListUserPoolClientsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list user pools.</p>
newtype ListUserPoolsRequest = ListUserPoolsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  , "MaxResults" :: (PoolQueryLimitType)
  }
derive instance newtypeListUserPoolsRequest :: Newtype ListUserPoolsRequest _
derive instance repGenericListUserPoolsRequest :: Generic ListUserPoolsRequest _
instance showListUserPoolsRequest :: Show ListUserPoolsRequest where
  show = genericShow
instance decodeListUserPoolsRequest :: Decode ListUserPoolsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserPoolsRequest :: Encode ListUserPoolsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response to list user pools.</p>
newtype ListUserPoolsResponse = ListUserPoolsResponse 
  { "UserPools" :: NullOrUndefined.NullOrUndefined (UserPoolListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKeyType)
  }
derive instance newtypeListUserPoolsResponse :: Newtype ListUserPoolsResponse _
derive instance repGenericListUserPoolsResponse :: Generic ListUserPoolsResponse _
instance showListUserPoolsResponse :: Show ListUserPoolsResponse where
  show = genericShow
instance decodeListUserPoolsResponse :: Decode ListUserPoolsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUserPoolsResponse :: Encode ListUserPoolsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListUsersInGroupRequest = ListUsersInGroupRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "GroupName" :: (GroupNameType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUsersInGroupRequest :: Newtype ListUsersInGroupRequest _
derive instance repGenericListUsersInGroupRequest :: Generic ListUsersInGroupRequest _
instance showListUsersInGroupRequest :: Show ListUsersInGroupRequest where
  show = genericShow
instance decodeListUsersInGroupRequest :: Decode ListUsersInGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersInGroupRequest :: Encode ListUsersInGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListUsersInGroupResponse = ListUsersInGroupResponse 
  { "Users" :: NullOrUndefined.NullOrUndefined (UsersListType)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListUsersInGroupResponse :: Newtype ListUsersInGroupResponse _
derive instance repGenericListUsersInGroupResponse :: Generic ListUsersInGroupResponse _
instance showListUsersInGroupResponse :: Show ListUsersInGroupResponse where
  show = genericShow
instance decodeListUsersInGroupResponse :: Decode ListUsersInGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersInGroupResponse :: Encode ListUsersInGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to list users.</p>
newtype ListUsersRequest = ListUsersRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "AttributesToGet" :: NullOrUndefined.NullOrUndefined (SearchedAttributeNamesListType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (QueryLimitType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  , "Filter" :: NullOrUndefined.NullOrUndefined (UserFilterType)
  }
derive instance newtypeListUsersRequest :: Newtype ListUsersRequest _
derive instance repGenericListUsersRequest :: Generic ListUsersRequest _
instance showListUsersRequest :: Show ListUsersRequest where
  show = genericShow
instance decodeListUsersRequest :: Decode ListUsersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersRequest :: Encode ListUsersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the request to list users.</p>
newtype ListUsersResponse = ListUsersResponse 
  { "Users" :: NullOrUndefined.NullOrUndefined (UsersListType)
  , "PaginationToken" :: NullOrUndefined.NullOrUndefined (SearchPaginationTokenType)
  }
derive instance newtypeListUsersResponse :: Newtype ListUsersResponse _
derive instance repGenericListUsersResponse :: Generic ListUsersResponse _
instance showListUsersResponse :: Show ListUsersResponse where
  show = genericShow
instance decodeListUsersResponse :: Decode ListUsersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListUsersResponse :: Encode ListUsersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogoutURLsListType = LogoutURLsListType (Array RedirectUrlType)
derive instance newtypeLogoutURLsListType :: Newtype LogoutURLsListType _
derive instance repGenericLogoutURLsListType :: Generic LogoutURLsListType _
instance showLogoutURLsListType :: Show LogoutURLsListType where
  show = genericShow
instance decodeLogoutURLsListType :: Decode LogoutURLsListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogoutURLsListType :: Encode LogoutURLsListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LongType = LongType Number
derive instance newtypeLongType :: Newtype LongType _
derive instance repGenericLongType :: Generic LongType _
instance showLongType :: Show LongType where
  show = genericShow
instance decodeLongType :: Decode LongType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLongType :: Encode LongType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.</p>
newtype MFAMethodNotFoundException = MFAMethodNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeMFAMethodNotFoundException :: Newtype MFAMethodNotFoundException _
derive instance repGenericMFAMethodNotFoundException :: Generic MFAMethodNotFoundException _
instance showMFAMethodNotFoundException :: Show MFAMethodNotFoundException where
  show = genericShow
instance decodeMFAMethodNotFoundException :: Decode MFAMethodNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFAMethodNotFoundException :: Encode MFAMethodNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MFAOptionListType = MFAOptionListType (Array MFAOptionType)
derive instance newtypeMFAOptionListType :: Newtype MFAOptionListType _
derive instance repGenericMFAOptionListType :: Generic MFAOptionListType _
instance showMFAOptionListType :: Show MFAOptionListType where
  show = genericShow
instance decodeMFAOptionListType :: Decode MFAOptionListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFAOptionListType :: Encode MFAOptionListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the different settings for multi-factor authentication (MFA).</p>
newtype MFAOptionType = MFAOptionType 
  { "DeliveryMedium" :: NullOrUndefined.NullOrUndefined (DeliveryMediumType)
  , "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeNameType)
  }
derive instance newtypeMFAOptionType :: Newtype MFAOptionType _
derive instance repGenericMFAOptionType :: Generic MFAOptionType _
instance showMFAOptionType :: Show MFAOptionType where
  show = genericShow
instance decodeMFAOptionType :: Decode MFAOptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMFAOptionType :: Encode MFAOptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageActionType = MessageActionType String
derive instance newtypeMessageActionType :: Newtype MessageActionType _
derive instance repGenericMessageActionType :: Generic MessageActionType _
instance showMessageActionType :: Show MessageActionType where
  show = genericShow
instance decodeMessageActionType :: Decode MessageActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageActionType :: Encode MessageActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The message template structure.</p>
newtype MessageTemplateType = MessageTemplateType 
  { "SMSMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "EmailMessage" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageType)
  , "EmailSubject" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectType)
  }
derive instance newtypeMessageTemplateType :: Newtype MessageTemplateType _
derive instance repGenericMessageTemplateType :: Generic MessageTemplateType _
instance showMessageTemplateType :: Show MessageTemplateType where
  show = genericShow
instance decodeMessageTemplateType :: Decode MessageTemplateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageTemplateType :: Encode MessageTemplateType where
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


-- | <p>The new device metadata type.</p>
newtype NewDeviceMetadataType = NewDeviceMetadataType 
  { "DeviceKey" :: NullOrUndefined.NullOrUndefined (DeviceKeyType)
  , "DeviceGroupKey" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeNewDeviceMetadataType :: Newtype NewDeviceMetadataType _
derive instance repGenericNewDeviceMetadataType :: Generic NewDeviceMetadataType _
instance showNewDeviceMetadataType :: Show NewDeviceMetadataType where
  show = genericShow
instance decodeNewDeviceMetadataType :: Decode NewDeviceMetadataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewDeviceMetadataType :: Encode NewDeviceMetadataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user is not authorized.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _
derive instance repGenericNotAuthorizedException :: Generic NotAuthorizedException _
instance showNotAuthorizedException :: Show NotAuthorizedException where
  show = genericShow
instance decodeNotAuthorizedException :: Decode NotAuthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAuthorizedException :: Encode NotAuthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The notify configuration type.</p>
newtype NotifyConfigurationType = NotifyConfigurationType 
  { "From" :: NullOrUndefined.NullOrUndefined (StringType)
  , "ReplyTo" :: NullOrUndefined.NullOrUndefined (StringType)
  , "SourceArn" :: (ArnType)
  , "BlockEmail" :: NullOrUndefined.NullOrUndefined (NotifyEmailType)
  , "NoActionEmail" :: NullOrUndefined.NullOrUndefined (NotifyEmailType)
  , "MfaEmail" :: NullOrUndefined.NullOrUndefined (NotifyEmailType)
  }
derive instance newtypeNotifyConfigurationType :: Newtype NotifyConfigurationType _
derive instance repGenericNotifyConfigurationType :: Generic NotifyConfigurationType _
instance showNotifyConfigurationType :: Show NotifyConfigurationType where
  show = genericShow
instance decodeNotifyConfigurationType :: Decode NotifyConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifyConfigurationType :: Encode NotifyConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The notify email type.</p>
newtype NotifyEmailType = NotifyEmailType 
  { "Subject" :: (EmailNotificationSubjectType)
  , "HtmlBody" :: NullOrUndefined.NullOrUndefined (EmailNotificationBodyType)
  , "TextBody" :: NullOrUndefined.NullOrUndefined (EmailNotificationBodyType)
  }
derive instance newtypeNotifyEmailType :: Newtype NotifyEmailType _
derive instance repGenericNotifyEmailType :: Generic NotifyEmailType _
instance showNotifyEmailType :: Show NotifyEmailType where
  show = genericShow
instance decodeNotifyEmailType :: Decode NotifyEmailType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifyEmailType :: Encode NotifyEmailType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The minimum and maximum value of an attribute that is of the number data type.</p>
newtype NumberAttributeConstraintsType = NumberAttributeConstraintsType 
  { "MinValue" :: NullOrUndefined.NullOrUndefined (StringType)
  , "MaxValue" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeNumberAttributeConstraintsType :: Newtype NumberAttributeConstraintsType _
derive instance repGenericNumberAttributeConstraintsType :: Generic NumberAttributeConstraintsType _
instance showNumberAttributeConstraintsType :: Show NumberAttributeConstraintsType where
  show = genericShow
instance decodeNumberAttributeConstraintsType :: Decode NumberAttributeConstraintsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberAttributeConstraintsType :: Encode NumberAttributeConstraintsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OAuthFlowType = OAuthFlowType String
derive instance newtypeOAuthFlowType :: Newtype OAuthFlowType _
derive instance repGenericOAuthFlowType :: Generic OAuthFlowType _
instance showOAuthFlowType :: Show OAuthFlowType where
  show = genericShow
instance decodeOAuthFlowType :: Decode OAuthFlowType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOAuthFlowType :: Encode OAuthFlowType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OAuthFlowsType = OAuthFlowsType (Array OAuthFlowType)
derive instance newtypeOAuthFlowsType :: Newtype OAuthFlowsType _
derive instance repGenericOAuthFlowsType :: Generic OAuthFlowsType _
instance showOAuthFlowsType :: Show OAuthFlowsType where
  show = genericShow
instance decodeOAuthFlowsType :: Decode OAuthFlowsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOAuthFlowsType :: Encode OAuthFlowsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationKey = PaginationKey String
derive instance newtypePaginationKey :: Newtype PaginationKey _
derive instance repGenericPaginationKey :: Generic PaginationKey _
instance showPaginationKey :: Show PaginationKey where
  show = genericShow
instance decodePaginationKey :: Decode PaginationKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationKey :: Encode PaginationKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationKeyType = PaginationKeyType String
derive instance newtypePaginationKeyType :: Newtype PaginationKeyType _
derive instance repGenericPaginationKeyType :: Generic PaginationKeyType _
instance showPaginationKeyType :: Show PaginationKeyType where
  show = genericShow
instance decodePaginationKeyType :: Decode PaginationKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationKeyType :: Encode PaginationKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PasswordPolicyMinLengthType = PasswordPolicyMinLengthType Int
derive instance newtypePasswordPolicyMinLengthType :: Newtype PasswordPolicyMinLengthType _
derive instance repGenericPasswordPolicyMinLengthType :: Generic PasswordPolicyMinLengthType _
instance showPasswordPolicyMinLengthType :: Show PasswordPolicyMinLengthType where
  show = genericShow
instance decodePasswordPolicyMinLengthType :: Decode PasswordPolicyMinLengthType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordPolicyMinLengthType :: Encode PasswordPolicyMinLengthType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The password policy type.</p>
newtype PasswordPolicyType = PasswordPolicyType 
  { "MinimumLength" :: NullOrUndefined.NullOrUndefined (PasswordPolicyMinLengthType)
  , "RequireUppercase" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "RequireLowercase" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "RequireNumbers" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "RequireSymbols" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypePasswordPolicyType :: Newtype PasswordPolicyType _
derive instance repGenericPasswordPolicyType :: Generic PasswordPolicyType _
instance showPasswordPolicyType :: Show PasswordPolicyType where
  show = genericShow
instance decodePasswordPolicyType :: Decode PasswordPolicyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordPolicyType :: Encode PasswordPolicyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a password reset is required.</p>
newtype PasswordResetRequiredException = PasswordResetRequiredException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypePasswordResetRequiredException :: Newtype PasswordResetRequiredException _
derive instance repGenericPasswordResetRequiredException :: Generic PasswordResetRequiredException _
instance showPasswordResetRequiredException :: Show PasswordResetRequiredException where
  show = genericShow
instance decodePasswordResetRequiredException :: Decode PasswordResetRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordResetRequiredException :: Encode PasswordResetRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PasswordType = PasswordType String
derive instance newtypePasswordType :: Newtype PasswordType _
derive instance repGenericPasswordType :: Generic PasswordType _
instance showPasswordType :: Show PasswordType where
  show = genericShow
instance decodePasswordType :: Decode PasswordType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordType :: Encode PasswordType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PoolQueryLimitType = PoolQueryLimitType Int
derive instance newtypePoolQueryLimitType :: Newtype PoolQueryLimitType _
derive instance repGenericPoolQueryLimitType :: Generic PoolQueryLimitType _
instance showPoolQueryLimitType :: Show PoolQueryLimitType where
  show = genericShow
instance decodePoolQueryLimitType :: Decode PoolQueryLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePoolQueryLimitType :: Encode PoolQueryLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PreSignedUrlType = PreSignedUrlType String
derive instance newtypePreSignedUrlType :: Newtype PreSignedUrlType _
derive instance repGenericPreSignedUrlType :: Generic PreSignedUrlType _
instance showPreSignedUrlType :: Show PreSignedUrlType where
  show = genericShow
instance decodePreSignedUrlType :: Decode PreSignedUrlType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreSignedUrlType :: Encode PreSignedUrlType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrecedenceType = PrecedenceType Int
derive instance newtypePrecedenceType :: Newtype PrecedenceType _
derive instance repGenericPrecedenceType :: Generic PrecedenceType _
instance showPrecedenceType :: Show PrecedenceType where
  show = genericShow
instance decodePrecedenceType :: Decode PrecedenceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrecedenceType :: Encode PrecedenceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a precondition is not met.</p>
newtype PreconditionNotMetException = PreconditionNotMetException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypePreconditionNotMetException :: Newtype PreconditionNotMetException _
derive instance repGenericPreconditionNotMetException :: Generic PreconditionNotMetException _
instance showPreconditionNotMetException :: Show PreconditionNotMetException where
  show = genericShow
instance decodePreconditionNotMetException :: Decode PreconditionNotMetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreconditionNotMetException :: Encode PreconditionNotMetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for identity provider details.</p>
newtype ProviderDescription = ProviderDescription 
  { "ProviderName" :: NullOrUndefined.NullOrUndefined (ProviderNameType)
  , "ProviderType" :: NullOrUndefined.NullOrUndefined (IdentityProviderTypeType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeProviderDescription :: Newtype ProviderDescription _
derive instance repGenericProviderDescription :: Generic ProviderDescription _
instance showProviderDescription :: Show ProviderDescription where
  show = genericShow
instance decodeProviderDescription :: Decode ProviderDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderDescription :: Encode ProviderDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProviderDetailsType = ProviderDetailsType (StrMap.StrMap StringType)
derive instance newtypeProviderDetailsType :: Newtype ProviderDetailsType _
derive instance repGenericProviderDetailsType :: Generic ProviderDetailsType _
instance showProviderDetailsType :: Show ProviderDetailsType where
  show = genericShow
instance decodeProviderDetailsType :: Decode ProviderDetailsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderDetailsType :: Encode ProviderDetailsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProviderNameType = ProviderNameType String
derive instance newtypeProviderNameType :: Newtype ProviderNameType _
derive instance repGenericProviderNameType :: Generic ProviderNameType _
instance showProviderNameType :: Show ProviderNameType where
  show = genericShow
instance decodeProviderNameType :: Decode ProviderNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderNameType :: Encode ProviderNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProviderNameTypeV1 = ProviderNameTypeV1 String
derive instance newtypeProviderNameTypeV1 :: Newtype ProviderNameTypeV1 _
derive instance repGenericProviderNameTypeV1 :: Generic ProviderNameTypeV1 _
instance showProviderNameTypeV1 :: Show ProviderNameTypeV1 where
  show = genericShow
instance decodeProviderNameTypeV1 :: Decode ProviderNameTypeV1 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderNameTypeV1 :: Encode ProviderNameTypeV1 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for information about an identity provider for a user pool.</p>
newtype ProviderUserIdentifierType = ProviderUserIdentifierType 
  { "ProviderName" :: NullOrUndefined.NullOrUndefined (ProviderNameType)
  , "ProviderAttributeName" :: NullOrUndefined.NullOrUndefined (StringType)
  , "ProviderAttributeValue" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeProviderUserIdentifierType :: Newtype ProviderUserIdentifierType _
derive instance repGenericProviderUserIdentifierType :: Generic ProviderUserIdentifierType _
instance showProviderUserIdentifierType :: Show ProviderUserIdentifierType where
  show = genericShow
instance decodeProviderUserIdentifierType :: Decode ProviderUserIdentifierType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProviderUserIdentifierType :: Encode ProviderUserIdentifierType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProvidersListType = ProvidersListType (Array ProviderDescription)
derive instance newtypeProvidersListType :: Newtype ProvidersListType _
derive instance repGenericProvidersListType :: Generic ProvidersListType _
instance showProvidersListType :: Show ProvidersListType where
  show = genericShow
instance decodeProvidersListType :: Decode ProvidersListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProvidersListType :: Encode ProvidersListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryLimit = QueryLimit Int
derive instance newtypeQueryLimit :: Newtype QueryLimit _
derive instance repGenericQueryLimit :: Generic QueryLimit _
instance showQueryLimit :: Show QueryLimit where
  show = genericShow
instance decodeQueryLimit :: Decode QueryLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLimit :: Encode QueryLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryLimitType = QueryLimitType Int
derive instance newtypeQueryLimitType :: Newtype QueryLimitType _
derive instance repGenericQueryLimitType :: Generic QueryLimitType _
instance showQueryLimitType :: Show QueryLimitType where
  show = genericShow
instance decodeQueryLimitType :: Decode QueryLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLimitType :: Encode QueryLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RedirectUrlType = RedirectUrlType String
derive instance newtypeRedirectUrlType :: Newtype RedirectUrlType _
derive instance repGenericRedirectUrlType :: Generic RedirectUrlType _
instance showRedirectUrlType :: Show RedirectUrlType where
  show = genericShow
instance decodeRedirectUrlType :: Decode RedirectUrlType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedirectUrlType :: Encode RedirectUrlType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RefreshTokenValidityType = RefreshTokenValidityType Int
derive instance newtypeRefreshTokenValidityType :: Newtype RefreshTokenValidityType _
derive instance repGenericRefreshTokenValidityType :: Generic RefreshTokenValidityType _
instance showRefreshTokenValidityType :: Show RefreshTokenValidityType where
  show = genericShow
instance decodeRefreshTokenValidityType :: Decode RefreshTokenValidityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRefreshTokenValidityType :: Encode RefreshTokenValidityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to resend the confirmation code.</p>
newtype ResendConfirmationCodeRequest = ResendConfirmationCodeRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined.NullOrUndefined (SecretHashType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  , "Username" :: (UsernameType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  }
derive instance newtypeResendConfirmationCodeRequest :: Newtype ResendConfirmationCodeRequest _
derive instance repGenericResendConfirmationCodeRequest :: Generic ResendConfirmationCodeRequest _
instance showResendConfirmationCodeRequest :: Show ResendConfirmationCodeRequest where
  show = genericShow
instance decodeResendConfirmationCodeRequest :: Decode ResendConfirmationCodeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResendConfirmationCodeRequest :: Encode ResendConfirmationCodeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server when the Amazon Cognito Your User Pools service makes the request to resend a confirmation code.</p>
newtype ResendConfirmationCodeResponse = ResendConfirmationCodeResponse 
  { "CodeDeliveryDetails" :: NullOrUndefined.NullOrUndefined (CodeDeliveryDetailsType)
  }
derive instance newtypeResendConfirmationCodeResponse :: Newtype ResendConfirmationCodeResponse _
derive instance repGenericResendConfirmationCodeResponse :: Generic ResendConfirmationCodeResponse _
instance showResendConfirmationCodeResponse :: Show ResendConfirmationCodeResponse where
  show = genericShow
instance decodeResendConfirmationCodeResponse :: Decode ResendConfirmationCodeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResendConfirmationCodeResponse :: Encode ResendConfirmationCodeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service cannot find the requested resource.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServerIdentifierType = ResourceServerIdentifierType String
derive instance newtypeResourceServerIdentifierType :: Newtype ResourceServerIdentifierType _
derive instance repGenericResourceServerIdentifierType :: Generic ResourceServerIdentifierType _
instance showResourceServerIdentifierType :: Show ResourceServerIdentifierType where
  show = genericShow
instance decodeResourceServerIdentifierType :: Decode ResourceServerIdentifierType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerIdentifierType :: Encode ResourceServerIdentifierType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServerNameType = ResourceServerNameType String
derive instance newtypeResourceServerNameType :: Newtype ResourceServerNameType _
derive instance repGenericResourceServerNameType :: Generic ResourceServerNameType _
instance showResourceServerNameType :: Show ResourceServerNameType where
  show = genericShow
instance decodeResourceServerNameType :: Decode ResourceServerNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerNameType :: Encode ResourceServerNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServerScopeDescriptionType = ResourceServerScopeDescriptionType String
derive instance newtypeResourceServerScopeDescriptionType :: Newtype ResourceServerScopeDescriptionType _
derive instance repGenericResourceServerScopeDescriptionType :: Generic ResourceServerScopeDescriptionType _
instance showResourceServerScopeDescriptionType :: Show ResourceServerScopeDescriptionType where
  show = genericShow
instance decodeResourceServerScopeDescriptionType :: Decode ResourceServerScopeDescriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerScopeDescriptionType :: Encode ResourceServerScopeDescriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServerScopeListType = ResourceServerScopeListType (Array ResourceServerScopeType)
derive instance newtypeResourceServerScopeListType :: Newtype ResourceServerScopeListType _
derive instance repGenericResourceServerScopeListType :: Generic ResourceServerScopeListType _
instance showResourceServerScopeListType :: Show ResourceServerScopeListType where
  show = genericShow
instance decodeResourceServerScopeListType :: Decode ResourceServerScopeListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerScopeListType :: Encode ResourceServerScopeListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServerScopeNameType = ResourceServerScopeNameType String
derive instance newtypeResourceServerScopeNameType :: Newtype ResourceServerScopeNameType _
derive instance repGenericResourceServerScopeNameType :: Generic ResourceServerScopeNameType _
instance showResourceServerScopeNameType :: Show ResourceServerScopeNameType where
  show = genericShow
instance decodeResourceServerScopeNameType :: Decode ResourceServerScopeNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerScopeNameType :: Encode ResourceServerScopeNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource server scope.</p>
newtype ResourceServerScopeType = ResourceServerScopeType 
  { "ScopeName" :: (ResourceServerScopeNameType)
  , "ScopeDescription" :: (ResourceServerScopeDescriptionType)
  }
derive instance newtypeResourceServerScopeType :: Newtype ResourceServerScopeType _
derive instance repGenericResourceServerScopeType :: Generic ResourceServerScopeType _
instance showResourceServerScopeType :: Show ResourceServerScopeType where
  show = genericShow
instance decodeResourceServerScopeType :: Decode ResourceServerScopeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerScopeType :: Encode ResourceServerScopeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for information about a resource server for a user pool.</p>
newtype ResourceServerType = ResourceServerType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "Identifier" :: NullOrUndefined.NullOrUndefined (ResourceServerIdentifierType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined.NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeResourceServerType :: Newtype ResourceServerType _
derive instance repGenericResourceServerType :: Generic ResourceServerType _
instance showResourceServerType :: Show ResourceServerType where
  show = genericShow
instance decodeResourceServerType :: Decode ResourceServerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServerType :: Encode ResourceServerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceServersListType = ResourceServersListType (Array ResourceServerType)
derive instance newtypeResourceServersListType :: Newtype ResourceServersListType _
derive instance repGenericResourceServersListType :: Generic ResourceServersListType _
instance showResourceServersListType :: Show ResourceServersListType where
  show = genericShow
instance decodeResourceServersListType :: Decode ResourceServersListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceServersListType :: Encode ResourceServersListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to respond to an authentication challenge.</p>
newtype RespondToAuthChallengeRequest = RespondToAuthChallengeRequest 
  { "ClientId" :: (ClientIdType)
  , "ChallengeName" :: (ChallengeNameType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "ChallengeResponses" :: NullOrUndefined.NullOrUndefined (ChallengeResponsesType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  }
derive instance newtypeRespondToAuthChallengeRequest :: Newtype RespondToAuthChallengeRequest _
derive instance repGenericRespondToAuthChallengeRequest :: Generic RespondToAuthChallengeRequest _
instance showRespondToAuthChallengeRequest :: Show RespondToAuthChallengeRequest where
  show = genericShow
instance decodeRespondToAuthChallengeRequest :: Decode RespondToAuthChallengeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondToAuthChallengeRequest :: Encode RespondToAuthChallengeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to respond to the authentication challenge.</p>
newtype RespondToAuthChallengeResponse = RespondToAuthChallengeResponse 
  { "ChallengeName" :: NullOrUndefined.NullOrUndefined (ChallengeNameType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "ChallengeParameters" :: NullOrUndefined.NullOrUndefined (ChallengeParametersType)
  , "AuthenticationResult" :: NullOrUndefined.NullOrUndefined (AuthenticationResultType)
  }
derive instance newtypeRespondToAuthChallengeResponse :: Newtype RespondToAuthChallengeResponse _
derive instance repGenericRespondToAuthChallengeResponse :: Generic RespondToAuthChallengeResponse _
instance showRespondToAuthChallengeResponse :: Show RespondToAuthChallengeResponse where
  show = genericShow
instance decodeRespondToAuthChallengeResponse :: Decode RespondToAuthChallengeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondToAuthChallengeResponse :: Encode RespondToAuthChallengeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The risk configuration type.</p>
newtype RiskConfigurationType = RiskConfigurationType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined.NullOrUndefined (CompromisedCredentialsRiskConfigurationType)
  , "AccountTakeoverRiskConfiguration" :: NullOrUndefined.NullOrUndefined (AccountTakeoverRiskConfigurationType)
  , "RiskExceptionConfiguration" :: NullOrUndefined.NullOrUndefined (RiskExceptionConfigurationType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeRiskConfigurationType :: Newtype RiskConfigurationType _
derive instance repGenericRiskConfigurationType :: Generic RiskConfigurationType _
instance showRiskConfigurationType :: Show RiskConfigurationType where
  show = genericShow
instance decodeRiskConfigurationType :: Decode RiskConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRiskConfigurationType :: Encode RiskConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RiskDecisionType = RiskDecisionType String
derive instance newtypeRiskDecisionType :: Newtype RiskDecisionType _
derive instance repGenericRiskDecisionType :: Generic RiskDecisionType _
instance showRiskDecisionType :: Show RiskDecisionType where
  show = genericShow
instance decodeRiskDecisionType :: Decode RiskDecisionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRiskDecisionType :: Encode RiskDecisionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of the configuration to override the risk decision.</p>
newtype RiskExceptionConfigurationType = RiskExceptionConfigurationType 
  { "BlockedIPRangeList" :: NullOrUndefined.NullOrUndefined (BlockedIPRangeListType)
  , "SkippedIPRangeList" :: NullOrUndefined.NullOrUndefined (SkippedIPRangeListType)
  }
derive instance newtypeRiskExceptionConfigurationType :: Newtype RiskExceptionConfigurationType _
derive instance repGenericRiskExceptionConfigurationType :: Generic RiskExceptionConfigurationType _
instance showRiskExceptionConfigurationType :: Show RiskExceptionConfigurationType where
  show = genericShow
instance decodeRiskExceptionConfigurationType :: Decode RiskExceptionConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRiskExceptionConfigurationType :: Encode RiskExceptionConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RiskLevelType = RiskLevelType String
derive instance newtypeRiskLevelType :: Newtype RiskLevelType _
derive instance repGenericRiskLevelType :: Generic RiskLevelType _
instance showRiskLevelType :: Show RiskLevelType where
  show = genericShow
instance decodeRiskLevelType :: Decode RiskLevelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRiskLevelType :: Encode RiskLevelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BucketType = S3BucketType String
derive instance newtypeS3BucketType :: Newtype S3BucketType _
derive instance repGenericS3BucketType :: Generic S3BucketType _
instance showS3BucketType :: Show S3BucketType where
  show = genericShow
instance decodeS3BucketType :: Decode S3BucketType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketType :: Encode S3BucketType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SMS multi-factor authentication (MFA) settings type.</p>
newtype SMSMfaSettingsType = SMSMfaSettingsType 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "PreferredMfa" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeSMSMfaSettingsType :: Newtype SMSMfaSettingsType _
derive instance repGenericSMSMfaSettingsType :: Generic SMSMfaSettingsType _
instance showSMSMfaSettingsType :: Show SMSMfaSettingsType where
  show = genericShow
instance decodeSMSMfaSettingsType :: Decode SMSMfaSettingsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSMSMfaSettingsType :: Encode SMSMfaSettingsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the schema attribute.</p>
newtype SchemaAttributeType = SchemaAttributeType 
  { "Name" :: NullOrUndefined.NullOrUndefined (CustomAttributeNameType)
  , "AttributeDataType" :: NullOrUndefined.NullOrUndefined (AttributeDataType)
  , "DeveloperOnlyAttribute" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "Mutable" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "Required" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "NumberAttributeConstraints" :: NullOrUndefined.NullOrUndefined (NumberAttributeConstraintsType)
  , "StringAttributeConstraints" :: NullOrUndefined.NullOrUndefined (StringAttributeConstraintsType)
  }
derive instance newtypeSchemaAttributeType :: Newtype SchemaAttributeType _
derive instance repGenericSchemaAttributeType :: Generic SchemaAttributeType _
instance showSchemaAttributeType :: Show SchemaAttributeType where
  show = genericShow
instance decodeSchemaAttributeType :: Decode SchemaAttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaAttributeType :: Encode SchemaAttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaAttributesListType = SchemaAttributesListType (Array SchemaAttributeType)
derive instance newtypeSchemaAttributesListType :: Newtype SchemaAttributesListType _
derive instance repGenericSchemaAttributesListType :: Generic SchemaAttributesListType _
instance showSchemaAttributesListType :: Show SchemaAttributesListType where
  show = genericShow
instance decodeSchemaAttributesListType :: Decode SchemaAttributesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaAttributesListType :: Encode SchemaAttributesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified scope does not exist.</p>
newtype ScopeDoesNotExistException = ScopeDoesNotExistException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeScopeDoesNotExistException :: Newtype ScopeDoesNotExistException _
derive instance repGenericScopeDoesNotExistException :: Generic ScopeDoesNotExistException _
instance showScopeDoesNotExistException :: Show ScopeDoesNotExistException where
  show = genericShow
instance decodeScopeDoesNotExistException :: Decode ScopeDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScopeDoesNotExistException :: Encode ScopeDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScopeListType = ScopeListType (Array ScopeType)
derive instance newtypeScopeListType :: Newtype ScopeListType _
derive instance repGenericScopeListType :: Generic ScopeListType _
instance showScopeListType :: Show ScopeListType where
  show = genericShow
instance decodeScopeListType :: Decode ScopeListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScopeListType :: Encode ScopeListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScopeType = ScopeType String
derive instance newtypeScopeType :: Newtype ScopeType _
derive instance repGenericScopeType :: Generic ScopeType _
instance showScopeType :: Show ScopeType where
  show = genericShow
instance decodeScopeType :: Decode ScopeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScopeType :: Encode ScopeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchPaginationTokenType = SearchPaginationTokenType String
derive instance newtypeSearchPaginationTokenType :: Newtype SearchPaginationTokenType _
derive instance repGenericSearchPaginationTokenType :: Generic SearchPaginationTokenType _
instance showSearchPaginationTokenType :: Show SearchPaginationTokenType where
  show = genericShow
instance decodeSearchPaginationTokenType :: Decode SearchPaginationTokenType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchPaginationTokenType :: Encode SearchPaginationTokenType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchedAttributeNamesListType = SearchedAttributeNamesListType (Array AttributeNameType)
derive instance newtypeSearchedAttributeNamesListType :: Newtype SearchedAttributeNamesListType _
derive instance repGenericSearchedAttributeNamesListType :: Generic SearchedAttributeNamesListType _
instance showSearchedAttributeNamesListType :: Show SearchedAttributeNamesListType where
  show = genericShow
instance decodeSearchedAttributeNamesListType :: Decode SearchedAttributeNamesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchedAttributeNamesListType :: Encode SearchedAttributeNamesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecretCodeType = SecretCodeType String
derive instance newtypeSecretCodeType :: Newtype SecretCodeType _
derive instance repGenericSecretCodeType :: Generic SecretCodeType _
instance showSecretCodeType :: Show SecretCodeType where
  show = genericShow
instance decodeSecretCodeType :: Decode SecretCodeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecretCodeType :: Encode SecretCodeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecretHashType = SecretHashType String
derive instance newtypeSecretHashType :: Newtype SecretHashType _
derive instance repGenericSecretHashType :: Generic SecretHashType _
instance showSecretHashType :: Show SecretHashType where
  show = genericShow
instance decodeSecretHashType :: Decode SecretHashType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecretHashType :: Encode SecretHashType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SessionType = SessionType String
derive instance newtypeSessionType :: Newtype SessionType _
derive instance repGenericSessionType :: Generic SessionType _
instance showSessionType :: Show SessionType where
  show = genericShow
instance decodeSessionType :: Decode SessionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSessionType :: Encode SessionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetRiskConfigurationRequest = SetRiskConfigurationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "CompromisedCredentialsRiskConfiguration" :: NullOrUndefined.NullOrUndefined (CompromisedCredentialsRiskConfigurationType)
  , "AccountTakeoverRiskConfiguration" :: NullOrUndefined.NullOrUndefined (AccountTakeoverRiskConfigurationType)
  , "RiskExceptionConfiguration" :: NullOrUndefined.NullOrUndefined (RiskExceptionConfigurationType)
  }
derive instance newtypeSetRiskConfigurationRequest :: Newtype SetRiskConfigurationRequest _
derive instance repGenericSetRiskConfigurationRequest :: Generic SetRiskConfigurationRequest _
instance showSetRiskConfigurationRequest :: Show SetRiskConfigurationRequest where
  show = genericShow
instance decodeSetRiskConfigurationRequest :: Decode SetRiskConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetRiskConfigurationRequest :: Encode SetRiskConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetRiskConfigurationResponse = SetRiskConfigurationResponse 
  { "RiskConfiguration" :: (RiskConfigurationType)
  }
derive instance newtypeSetRiskConfigurationResponse :: Newtype SetRiskConfigurationResponse _
derive instance repGenericSetRiskConfigurationResponse :: Generic SetRiskConfigurationResponse _
instance showSetRiskConfigurationResponse :: Show SetRiskConfigurationResponse where
  show = genericShow
instance decodeSetRiskConfigurationResponse :: Decode SetRiskConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetRiskConfigurationResponse :: Encode SetRiskConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUICustomizationRequest = SetUICustomizationRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "CSS" :: NullOrUndefined.NullOrUndefined (CSSType)
  , "ImageFile" :: NullOrUndefined.NullOrUndefined (ImageFileType)
  }
derive instance newtypeSetUICustomizationRequest :: Newtype SetUICustomizationRequest _
derive instance repGenericSetUICustomizationRequest :: Generic SetUICustomizationRequest _
instance showSetUICustomizationRequest :: Show SetUICustomizationRequest where
  show = genericShow
instance decodeSetUICustomizationRequest :: Decode SetUICustomizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUICustomizationRequest :: Encode SetUICustomizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUICustomizationResponse = SetUICustomizationResponse 
  { "UICustomization" :: (UICustomizationType)
  }
derive instance newtypeSetUICustomizationResponse :: Newtype SetUICustomizationResponse _
derive instance repGenericSetUICustomizationResponse :: Generic SetUICustomizationResponse _
instance showSetUICustomizationResponse :: Show SetUICustomizationResponse where
  show = genericShow
instance decodeSetUICustomizationResponse :: Decode SetUICustomizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUICustomizationResponse :: Encode SetUICustomizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUserMFAPreferenceRequest = SetUserMFAPreferenceRequest 
  { "SMSMfaSettings" :: NullOrUndefined.NullOrUndefined (SMSMfaSettingsType)
  , "SoftwareTokenMfaSettings" :: NullOrUndefined.NullOrUndefined (SoftwareTokenMfaSettingsType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeSetUserMFAPreferenceRequest :: Newtype SetUserMFAPreferenceRequest _
derive instance repGenericSetUserMFAPreferenceRequest :: Generic SetUserMFAPreferenceRequest _
instance showSetUserMFAPreferenceRequest :: Show SetUserMFAPreferenceRequest where
  show = genericShow
instance decodeSetUserMFAPreferenceRequest :: Decode SetUserMFAPreferenceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserMFAPreferenceRequest :: Encode SetUserMFAPreferenceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse Types.NoArguments
derive instance newtypeSetUserMFAPreferenceResponse :: Newtype SetUserMFAPreferenceResponse _
derive instance repGenericSetUserMFAPreferenceResponse :: Generic SetUserMFAPreferenceResponse _
instance showSetUserMFAPreferenceResponse :: Show SetUserMFAPreferenceResponse where
  show = genericShow
instance decodeSetUserMFAPreferenceResponse :: Decode SetUserMFAPreferenceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserMFAPreferenceResponse :: Encode SetUserMFAPreferenceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUserPoolMfaConfigRequest = SetUserPoolMfaConfigRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "SmsMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeSetUserPoolMfaConfigRequest :: Newtype SetUserPoolMfaConfigRequest _
derive instance repGenericSetUserPoolMfaConfigRequest :: Generic SetUserPoolMfaConfigRequest _
instance showSetUserPoolMfaConfigRequest :: Show SetUserPoolMfaConfigRequest where
  show = genericShow
instance decodeSetUserPoolMfaConfigRequest :: Decode SetUserPoolMfaConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserPoolMfaConfigRequest :: Encode SetUserPoolMfaConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetUserPoolMfaConfigResponse = SetUserPoolMfaConfigResponse 
  { "SmsMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SmsMfaConfigType)
  , "SoftwareTokenMfaConfiguration" :: NullOrUndefined.NullOrUndefined (SoftwareTokenMfaConfigType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  }
derive instance newtypeSetUserPoolMfaConfigResponse :: Newtype SetUserPoolMfaConfigResponse _
derive instance repGenericSetUserPoolMfaConfigResponse :: Generic SetUserPoolMfaConfigResponse _
instance showSetUserPoolMfaConfigResponse :: Show SetUserPoolMfaConfigResponse where
  show = genericShow
instance decodeSetUserPoolMfaConfigResponse :: Decode SetUserPoolMfaConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserPoolMfaConfigResponse :: Encode SetUserPoolMfaConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to set user settings.</p>
newtype SetUserSettingsRequest = SetUserSettingsRequest 
  { "AccessToken" :: (TokenModelType)
  , "MFAOptions" :: (MFAOptionListType)
  }
derive instance newtypeSetUserSettingsRequest :: Newtype SetUserSettingsRequest _
derive instance repGenericSetUserSettingsRequest :: Generic SetUserSettingsRequest _
instance showSetUserSettingsRequest :: Show SetUserSettingsRequest where
  show = genericShow
instance decodeSetUserSettingsRequest :: Decode SetUserSettingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserSettingsRequest :: Encode SetUserSettingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server for a set user settings request.</p>
newtype SetUserSettingsResponse = SetUserSettingsResponse Types.NoArguments
derive instance newtypeSetUserSettingsResponse :: Newtype SetUserSettingsResponse _
derive instance repGenericSetUserSettingsResponse :: Generic SetUserSettingsResponse _
instance showSetUserSettingsResponse :: Show SetUserSettingsResponse where
  show = genericShow
instance decodeSetUserSettingsResponse :: Decode SetUserSettingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetUserSettingsResponse :: Encode SetUserSettingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to register a user.</p>
newtype SignUpRequest = SignUpRequest 
  { "ClientId" :: (ClientIdType)
  , "SecretHash" :: NullOrUndefined.NullOrUndefined (SecretHashType)
  , "Username" :: (UsernameType)
  , "Password" :: (PasswordType)
  , "UserAttributes" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "ValidationData" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "AnalyticsMetadata" :: NullOrUndefined.NullOrUndefined (AnalyticsMetadataType)
  , "UserContextData" :: NullOrUndefined.NullOrUndefined (UserContextDataType)
  }
derive instance newtypeSignUpRequest :: Newtype SignUpRequest _
derive instance repGenericSignUpRequest :: Generic SignUpRequest _
instance showSignUpRequest :: Show SignUpRequest where
  show = genericShow
instance decodeSignUpRequest :: Decode SignUpRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignUpRequest :: Encode SignUpRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response from the server for a registration request.</p>
newtype SignUpResponse = SignUpResponse 
  { "UserConfirmed" :: (BooleanType)
  , "CodeDeliveryDetails" :: NullOrUndefined.NullOrUndefined (CodeDeliveryDetailsType)
  , "UserSub" :: (StringType)
  }
derive instance newtypeSignUpResponse :: Newtype SignUpResponse _
derive instance repGenericSignUpResponse :: Generic SignUpResponse _
instance showSignUpResponse :: Show SignUpResponse where
  show = genericShow
instance decodeSignUpResponse :: Decode SignUpResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignUpResponse :: Encode SignUpResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SkippedIPRangeListType = SkippedIPRangeListType (Array StringType)
derive instance newtypeSkippedIPRangeListType :: Newtype SkippedIPRangeListType _
derive instance repGenericSkippedIPRangeListType :: Generic SkippedIPRangeListType _
instance showSkippedIPRangeListType :: Show SkippedIPRangeListType where
  show = genericShow
instance decodeSkippedIPRangeListType :: Decode SkippedIPRangeListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSkippedIPRangeListType :: Encode SkippedIPRangeListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SMS configuration type.</p>
newtype SmsConfigurationType = SmsConfigurationType 
  { "SnsCallerArn" :: (ArnType)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeSmsConfigurationType :: Newtype SmsConfigurationType _
derive instance repGenericSmsConfigurationType :: Generic SmsConfigurationType _
instance showSmsConfigurationType :: Show SmsConfigurationType where
  show = genericShow
instance decodeSmsConfigurationType :: Decode SmsConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmsConfigurationType :: Encode SmsConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SMS text message multi-factor authentication (MFA) configuration type.</p>
newtype SmsMfaConfigType = SmsMfaConfigType 
  { "SmsAuthenticationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "SmsConfiguration" :: NullOrUndefined.NullOrUndefined (SmsConfigurationType)
  }
derive instance newtypeSmsMfaConfigType :: Newtype SmsMfaConfigType _
derive instance repGenericSmsMfaConfigType :: Generic SmsMfaConfigType _
instance showSmsMfaConfigType :: Show SmsMfaConfigType where
  show = genericShow
instance decodeSmsMfaConfigType :: Decode SmsMfaConfigType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmsMfaConfigType :: Encode SmsMfaConfigType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SmsVerificationMessageType = SmsVerificationMessageType String
derive instance newtypeSmsVerificationMessageType :: Newtype SmsVerificationMessageType _
derive instance repGenericSmsVerificationMessageType :: Generic SmsVerificationMessageType _
instance showSmsVerificationMessageType :: Show SmsVerificationMessageType where
  show = genericShow
instance decodeSmsVerificationMessageType :: Decode SmsVerificationMessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmsVerificationMessageType :: Encode SmsVerificationMessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.</p>
newtype SoftwareTokenMFANotFoundException = SoftwareTokenMFANotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeSoftwareTokenMFANotFoundException :: Newtype SoftwareTokenMFANotFoundException _
derive instance repGenericSoftwareTokenMFANotFoundException :: Generic SoftwareTokenMFANotFoundException _
instance showSoftwareTokenMFANotFoundException :: Show SoftwareTokenMFANotFoundException where
  show = genericShow
instance decodeSoftwareTokenMFANotFoundException :: Decode SoftwareTokenMFANotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSoftwareTokenMFANotFoundException :: Encode SoftwareTokenMFANotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SoftwareTokenMFAUserCodeType = SoftwareTokenMFAUserCodeType String
derive instance newtypeSoftwareTokenMFAUserCodeType :: Newtype SoftwareTokenMFAUserCodeType _
derive instance repGenericSoftwareTokenMFAUserCodeType :: Generic SoftwareTokenMFAUserCodeType _
instance showSoftwareTokenMFAUserCodeType :: Show SoftwareTokenMFAUserCodeType where
  show = genericShow
instance decodeSoftwareTokenMFAUserCodeType :: Decode SoftwareTokenMFAUserCodeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSoftwareTokenMFAUserCodeType :: Encode SoftwareTokenMFAUserCodeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type used for enabling software token MFA at the user pool level.</p>
newtype SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeSoftwareTokenMfaConfigType :: Newtype SoftwareTokenMfaConfigType _
derive instance repGenericSoftwareTokenMfaConfigType :: Generic SoftwareTokenMfaConfigType _
instance showSoftwareTokenMfaConfigType :: Show SoftwareTokenMfaConfigType where
  show = genericShow
instance decodeSoftwareTokenMfaConfigType :: Decode SoftwareTokenMfaConfigType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSoftwareTokenMfaConfigType :: Encode SoftwareTokenMfaConfigType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type used for enabling software token MFA at the user level.</p>
newtype SoftwareTokenMfaSettingsType = SoftwareTokenMfaSettingsType 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "PreferredMfa" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeSoftwareTokenMfaSettingsType :: Newtype SoftwareTokenMfaSettingsType _
derive instance repGenericSoftwareTokenMfaSettingsType :: Generic SoftwareTokenMfaSettingsType _
instance showSoftwareTokenMfaSettingsType :: Show SoftwareTokenMfaSettingsType where
  show = genericShow
instance decodeSoftwareTokenMfaSettingsType :: Decode SoftwareTokenMfaSettingsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSoftwareTokenMfaSettingsType :: Encode SoftwareTokenMfaSettingsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to start the user import job.</p>
newtype StartUserImportJobRequest = StartUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeStartUserImportJobRequest :: Newtype StartUserImportJobRequest _
derive instance repGenericStartUserImportJobRequest :: Generic StartUserImportJobRequest _
instance showStartUserImportJobRequest :: Show StartUserImportJobRequest where
  show = genericShow
instance decodeStartUserImportJobRequest :: Decode StartUserImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartUserImportJobRequest :: Encode StartUserImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to start the user import job.</p>
newtype StartUserImportJobResponse = StartUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined.NullOrUndefined (UserImportJobType)
  }
derive instance newtypeStartUserImportJobResponse :: Newtype StartUserImportJobResponse _
derive instance repGenericStartUserImportJobResponse :: Generic StartUserImportJobResponse _
instance showStartUserImportJobResponse :: Show StartUserImportJobResponse where
  show = genericShow
instance decodeStartUserImportJobResponse :: Decode StartUserImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartUserImportJobResponse :: Encode StartUserImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusType = StatusType String
derive instance newtypeStatusType :: Newtype StatusType _
derive instance repGenericStatusType :: Generic StatusType _
instance showStatusType :: Show StatusType where
  show = genericShow
instance decodeStatusType :: Decode StatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusType :: Encode StatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to stop the user import job.</p>
newtype StopUserImportJobRequest = StopUserImportJobRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "JobId" :: (UserImportJobIdType)
  }
derive instance newtypeStopUserImportJobRequest :: Newtype StopUserImportJobRequest _
derive instance repGenericStopUserImportJobRequest :: Generic StopUserImportJobRequest _
instance showStopUserImportJobRequest :: Show StopUserImportJobRequest where
  show = genericShow
instance decodeStopUserImportJobRequest :: Decode StopUserImportJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopUserImportJobRequest :: Encode StopUserImportJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to stop the user import job.</p>
newtype StopUserImportJobResponse = StopUserImportJobResponse 
  { "UserImportJob" :: NullOrUndefined.NullOrUndefined (UserImportJobType)
  }
derive instance newtypeStopUserImportJobResponse :: Newtype StopUserImportJobResponse _
derive instance repGenericStopUserImportJobResponse :: Generic StopUserImportJobResponse _
instance showStopUserImportJobResponse :: Show StopUserImportJobResponse where
  show = genericShow
instance decodeStopUserImportJobResponse :: Decode StopUserImportJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopUserImportJobResponse :: Encode StopUserImportJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The constraints associated with a string attribute.</p>
newtype StringAttributeConstraintsType = StringAttributeConstraintsType 
  { "MinLength" :: NullOrUndefined.NullOrUndefined (StringType)
  , "MaxLength" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeStringAttributeConstraintsType :: Newtype StringAttributeConstraintsType _
derive instance repGenericStringAttributeConstraintsType :: Generic StringAttributeConstraintsType _
instance showStringAttributeConstraintsType :: Show StringAttributeConstraintsType where
  show = genericShow
instance decodeStringAttributeConstraintsType :: Decode StringAttributeConstraintsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringAttributeConstraintsType :: Encode StringAttributeConstraintsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringType = StringType String
derive instance newtypeStringType :: Newtype StringType _
derive instance repGenericStringType :: Generic StringType _
instance showStringType :: Show StringType where
  show = genericShow
instance decodeStringType :: Decode StringType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringType :: Encode StringType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedIdentityProvidersListType = SupportedIdentityProvidersListType (Array ProviderNameType)
derive instance newtypeSupportedIdentityProvidersListType :: Newtype SupportedIdentityProvidersListType _
derive instance repGenericSupportedIdentityProvidersListType :: Generic SupportedIdentityProvidersListType _
instance showSupportedIdentityProvidersListType :: Show SupportedIdentityProvidersListType where
  show = genericShow
instance decodeSupportedIdentityProvidersListType :: Decode SupportedIdentityProvidersListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedIdentityProvidersListType :: Encode SupportedIdentityProvidersListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TokenModelType = TokenModelType String
derive instance newtypeTokenModelType :: Newtype TokenModelType _
derive instance repGenericTokenModelType :: Generic TokenModelType _
instance showTokenModelType :: Show TokenModelType where
  show = genericShow
instance decodeTokenModelType :: Decode TokenModelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTokenModelType :: Encode TokenModelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).</p>
newtype TooManyFailedAttemptsException = TooManyFailedAttemptsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeTooManyFailedAttemptsException :: Newtype TooManyFailedAttemptsException _
derive instance repGenericTooManyFailedAttemptsException :: Generic TooManyFailedAttemptsException _
instance showTooManyFailedAttemptsException :: Show TooManyFailedAttemptsException where
  show = genericShow
instance decodeTooManyFailedAttemptsException :: Decode TooManyFailedAttemptsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyFailedAttemptsException :: Encode TooManyFailedAttemptsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the user has made too many requests for a given operation.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for the UI customization information for a user pool's built-in app UI.</p>
newtype UICustomizationType = UICustomizationType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "ImageUrl" :: NullOrUndefined.NullOrUndefined (ImageUrlType)
  , "CSS" :: NullOrUndefined.NullOrUndefined (CSSType)
  , "CSSVersion" :: NullOrUndefined.NullOrUndefined (CSSVersionType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeUICustomizationType :: Newtype UICustomizationType _
derive instance repGenericUICustomizationType :: Generic UICustomizationType _
instance showUICustomizationType :: Show UICustomizationType where
  show = genericShow
instance decodeUICustomizationType :: Decode UICustomizationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUICustomizationType :: Encode UICustomizationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.</p>
newtype UnexpectedLambdaException = UnexpectedLambdaException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUnexpectedLambdaException :: Newtype UnexpectedLambdaException _
derive instance repGenericUnexpectedLambdaException :: Generic UnexpectedLambdaException _
instance showUnexpectedLambdaException :: Show UnexpectedLambdaException where
  show = genericShow
instance decodeUnexpectedLambdaException :: Decode UnexpectedLambdaException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnexpectedLambdaException :: Encode UnexpectedLambdaException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified identifier is not supported.</p>
newtype UnsupportedIdentityProviderException = UnsupportedIdentityProviderException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUnsupportedIdentityProviderException :: Newtype UnsupportedIdentityProviderException _
derive instance repGenericUnsupportedIdentityProviderException :: Generic UnsupportedIdentityProviderException _
instance showUnsupportedIdentityProviderException :: Show UnsupportedIdentityProviderException where
  show = genericShow
instance decodeUnsupportedIdentityProviderException :: Decode UnsupportedIdentityProviderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedIdentityProviderException :: Encode UnsupportedIdentityProviderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request failed because the user is in an unsupported state.</p>
newtype UnsupportedUserStateException = UnsupportedUserStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUnsupportedUserStateException :: Newtype UnsupportedUserStateException _
derive instance repGenericUnsupportedUserStateException :: Generic UnsupportedUserStateException _
instance showUnsupportedUserStateException :: Show UnsupportedUserStateException where
  show = genericShow
instance decodeUnsupportedUserStateException :: Decode UnsupportedUserStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedUserStateException :: Encode UnsupportedUserStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAuthEventFeedbackRequest = UpdateAuthEventFeedbackRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Username" :: (UsernameType)
  , "EventId" :: (EventIdType)
  , "FeedbackToken" :: (TokenModelType)
  , "FeedbackValue" :: (FeedbackValueType)
  }
derive instance newtypeUpdateAuthEventFeedbackRequest :: Newtype UpdateAuthEventFeedbackRequest _
derive instance repGenericUpdateAuthEventFeedbackRequest :: Generic UpdateAuthEventFeedbackRequest _
instance showUpdateAuthEventFeedbackRequest :: Show UpdateAuthEventFeedbackRequest where
  show = genericShow
instance decodeUpdateAuthEventFeedbackRequest :: Decode UpdateAuthEventFeedbackRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAuthEventFeedbackRequest :: Encode UpdateAuthEventFeedbackRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAuthEventFeedbackResponse = UpdateAuthEventFeedbackResponse Types.NoArguments
derive instance newtypeUpdateAuthEventFeedbackResponse :: Newtype UpdateAuthEventFeedbackResponse _
derive instance repGenericUpdateAuthEventFeedbackResponse :: Generic UpdateAuthEventFeedbackResponse _
instance showUpdateAuthEventFeedbackResponse :: Show UpdateAuthEventFeedbackResponse where
  show = genericShow
instance decodeUpdateAuthEventFeedbackResponse :: Decode UpdateAuthEventFeedbackResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAuthEventFeedbackResponse :: Encode UpdateAuthEventFeedbackResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to update the device status.</p>
newtype UpdateDeviceStatusRequest = UpdateDeviceStatusRequest 
  { "AccessToken" :: (TokenModelType)
  , "DeviceKey" :: (DeviceKeyType)
  , "DeviceRememberedStatus" :: NullOrUndefined.NullOrUndefined (DeviceRememberedStatusType)
  }
derive instance newtypeUpdateDeviceStatusRequest :: Newtype UpdateDeviceStatusRequest _
derive instance repGenericUpdateDeviceStatusRequest :: Generic UpdateDeviceStatusRequest _
instance showUpdateDeviceStatusRequest :: Show UpdateDeviceStatusRequest where
  show = genericShow
instance decodeUpdateDeviceStatusRequest :: Decode UpdateDeviceStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDeviceStatusRequest :: Encode UpdateDeviceStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to the request to update the device status.</p>
newtype UpdateDeviceStatusResponse = UpdateDeviceStatusResponse Types.NoArguments
derive instance newtypeUpdateDeviceStatusResponse :: Newtype UpdateDeviceStatusResponse _
derive instance repGenericUpdateDeviceStatusResponse :: Generic UpdateDeviceStatusResponse _
instance showUpdateDeviceStatusResponse :: Show UpdateDeviceStatusResponse where
  show = genericShow
instance decodeUpdateDeviceStatusResponse :: Decode UpdateDeviceStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDeviceStatusResponse :: Encode UpdateDeviceStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupRequest = UpdateGroupRequest 
  { "GroupName" :: (GroupNameType)
  , "UserPoolId" :: (UserPoolIdType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionType)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "Precedence" :: NullOrUndefined.NullOrUndefined (PrecedenceType)
  }
derive instance newtypeUpdateGroupRequest :: Newtype UpdateGroupRequest _
derive instance repGenericUpdateGroupRequest :: Generic UpdateGroupRequest _
instance showUpdateGroupRequest :: Show UpdateGroupRequest where
  show = genericShow
instance decodeUpdateGroupRequest :: Decode UpdateGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupRequest :: Encode UpdateGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateGroupResponse = UpdateGroupResponse 
  { "Group" :: NullOrUndefined.NullOrUndefined (GroupType)
  }
derive instance newtypeUpdateGroupResponse :: Newtype UpdateGroupResponse _
derive instance repGenericUpdateGroupResponse :: Generic UpdateGroupResponse _
instance showUpdateGroupResponse :: Show UpdateGroupResponse where
  show = genericShow
instance decodeUpdateGroupResponse :: Decode UpdateGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGroupResponse :: Encode UpdateGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIdentityProviderRequest = UpdateIdentityProviderRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ProviderName" :: (ProviderNameType)
  , "ProviderDetails" :: NullOrUndefined.NullOrUndefined (ProviderDetailsType)
  , "AttributeMapping" :: NullOrUndefined.NullOrUndefined (AttributeMappingType)
  , "IdpIdentifiers" :: NullOrUndefined.NullOrUndefined (IdpIdentifiersListType)
  }
derive instance newtypeUpdateIdentityProviderRequest :: Newtype UpdateIdentityProviderRequest _
derive instance repGenericUpdateIdentityProviderRequest :: Generic UpdateIdentityProviderRequest _
instance showUpdateIdentityProviderRequest :: Show UpdateIdentityProviderRequest where
  show = genericShow
instance decodeUpdateIdentityProviderRequest :: Decode UpdateIdentityProviderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIdentityProviderRequest :: Encode UpdateIdentityProviderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIdentityProviderResponse = UpdateIdentityProviderResponse 
  { "IdentityProvider" :: (IdentityProviderType)
  }
derive instance newtypeUpdateIdentityProviderResponse :: Newtype UpdateIdentityProviderResponse _
derive instance repGenericUpdateIdentityProviderResponse :: Generic UpdateIdentityProviderResponse _
instance showUpdateIdentityProviderResponse :: Show UpdateIdentityProviderResponse where
  show = genericShow
instance decodeUpdateIdentityProviderResponse :: Decode UpdateIdentityProviderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIdentityProviderResponse :: Encode UpdateIdentityProviderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceServerRequest = UpdateResourceServerRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Identifier" :: (ResourceServerIdentifierType)
  , "Name" :: (ResourceServerNameType)
  , "Scopes" :: NullOrUndefined.NullOrUndefined (ResourceServerScopeListType)
  }
derive instance newtypeUpdateResourceServerRequest :: Newtype UpdateResourceServerRequest _
derive instance repGenericUpdateResourceServerRequest :: Generic UpdateResourceServerRequest _
instance showUpdateResourceServerRequest :: Show UpdateResourceServerRequest where
  show = genericShow
instance decodeUpdateResourceServerRequest :: Decode UpdateResourceServerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceServerRequest :: Encode UpdateResourceServerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateResourceServerResponse = UpdateResourceServerResponse 
  { "ResourceServer" :: (ResourceServerType)
  }
derive instance newtypeUpdateResourceServerResponse :: Newtype UpdateResourceServerResponse _
derive instance repGenericUpdateResourceServerResponse :: Generic UpdateResourceServerResponse _
instance showUpdateResourceServerResponse :: Show UpdateResourceServerResponse where
  show = genericShow
instance decodeUpdateResourceServerResponse :: Decode UpdateResourceServerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateResourceServerResponse :: Encode UpdateResourceServerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to update user attributes.</p>
newtype UpdateUserAttributesRequest = UpdateUserAttributesRequest 
  { "UserAttributes" :: (AttributeListType)
  , "AccessToken" :: (TokenModelType)
  }
derive instance newtypeUpdateUserAttributesRequest :: Newtype UpdateUserAttributesRequest _
derive instance repGenericUpdateUserAttributesRequest :: Generic UpdateUserAttributesRequest _
instance showUpdateUserAttributesRequest :: Show UpdateUserAttributesRequest where
  show = genericShow
instance decodeUpdateUserAttributesRequest :: Decode UpdateUserAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserAttributesRequest :: Encode UpdateUserAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server for the request to update user attributes.</p>
newtype UpdateUserAttributesResponse = UpdateUserAttributesResponse 
  { "CodeDeliveryDetailsList" :: NullOrUndefined.NullOrUndefined (CodeDeliveryDetailsListType)
  }
derive instance newtypeUpdateUserAttributesResponse :: Newtype UpdateUserAttributesResponse _
derive instance repGenericUpdateUserAttributesResponse :: Generic UpdateUserAttributesResponse _
instance showUpdateUserAttributesResponse :: Show UpdateUserAttributesResponse where
  show = genericShow
instance decodeUpdateUserAttributesResponse :: Decode UpdateUserAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserAttributesResponse :: Encode UpdateUserAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to update the user pool client.</p>
newtype UpdateUserPoolClientRequest = UpdateUserPoolClientRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "ClientId" :: (ClientIdType)
  , "ClientName" :: NullOrUndefined.NullOrUndefined (ClientNameType)
  , "RefreshTokenValidity" :: NullOrUndefined.NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined.NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined.NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined.NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined.NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined.NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined.NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined.NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined.NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeUpdateUserPoolClientRequest :: Newtype UpdateUserPoolClientRequest _
derive instance repGenericUpdateUserPoolClientRequest :: Generic UpdateUserPoolClientRequest _
instance showUpdateUserPoolClientRequest :: Show UpdateUserPoolClientRequest where
  show = genericShow
instance decodeUpdateUserPoolClientRequest :: Decode UpdateUserPoolClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserPoolClientRequest :: Encode UpdateUserPoolClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server to the request to update the user pool client.</p>
newtype UpdateUserPoolClientResponse = UpdateUserPoolClientResponse 
  { "UserPoolClient" :: NullOrUndefined.NullOrUndefined (UserPoolClientType)
  }
derive instance newtypeUpdateUserPoolClientResponse :: Newtype UpdateUserPoolClientResponse _
derive instance repGenericUpdateUserPoolClientResponse :: Generic UpdateUserPoolClientResponse _
instance showUpdateUserPoolClientResponse :: Show UpdateUserPoolClientResponse where
  show = genericShow
instance decodeUpdateUserPoolClientResponse :: Decode UpdateUserPoolClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserPoolClientResponse :: Encode UpdateUserPoolClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to update the user pool.</p>
newtype UpdateUserPoolRequest = UpdateUserPoolRequest 
  { "UserPoolId" :: (UserPoolIdType)
  , "Policies" :: NullOrUndefined.NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined.NullOrUndefined (LambdaConfigType)
  , "AutoVerifiedAttributes" :: NullOrUndefined.NullOrUndefined (VerifiedAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined.NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined.NullOrUndefined (DeviceConfigurationType)
  , "EmailConfiguration" :: NullOrUndefined.NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined.NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined.NullOrUndefined (UserPoolTagsType)
  , "AdminCreateUserConfig" :: NullOrUndefined.NullOrUndefined (AdminCreateUserConfigType)
  , "UserPoolAddOns" :: NullOrUndefined.NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeUpdateUserPoolRequest :: Newtype UpdateUserPoolRequest _
derive instance repGenericUpdateUserPoolRequest :: Generic UpdateUserPoolRequest _
instance showUpdateUserPoolRequest :: Show UpdateUserPoolRequest where
  show = genericShow
instance decodeUpdateUserPoolRequest :: Decode UpdateUserPoolRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserPoolRequest :: Encode UpdateUserPoolRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the response from the server when you make a request to update the user pool.</p>
newtype UpdateUserPoolResponse = UpdateUserPoolResponse Types.NoArguments
derive instance newtypeUpdateUserPoolResponse :: Newtype UpdateUserPoolResponse _
derive instance repGenericUpdateUserPoolResponse :: Generic UpdateUserPoolResponse _
instance showUpdateUserPoolResponse :: Show UpdateUserPoolResponse where
  show = genericShow
instance decodeUpdateUserPoolResponse :: Decode UpdateUserPoolResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserPoolResponse :: Encode UpdateUserPoolResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.</p>
newtype UserContextDataType = UserContextDataType 
  { "EncodedData" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeUserContextDataType :: Newtype UserContextDataType _
derive instance repGenericUserContextDataType :: Generic UserContextDataType _
instance showUserContextDataType :: Show UserContextDataType where
  show = genericShow
instance decodeUserContextDataType :: Decode UserContextDataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserContextDataType :: Encode UserContextDataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserFilterType = UserFilterType String
derive instance newtypeUserFilterType :: Newtype UserFilterType _
derive instance repGenericUserFilterType :: Generic UserFilterType _
instance showUserFilterType :: Show UserFilterType where
  show = genericShow
instance decodeUserFilterType :: Decode UserFilterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserFilterType :: Encode UserFilterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.</p>
newtype UserImportInProgressException = UserImportInProgressException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserImportInProgressException :: Newtype UserImportInProgressException _
derive instance repGenericUserImportInProgressException :: Generic UserImportInProgressException _
instance showUserImportInProgressException :: Show UserImportInProgressException where
  show = genericShow
instance decodeUserImportInProgressException :: Decode UserImportInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportInProgressException :: Encode UserImportInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserImportJobIdType = UserImportJobIdType String
derive instance newtypeUserImportJobIdType :: Newtype UserImportJobIdType _
derive instance repGenericUserImportJobIdType :: Generic UserImportJobIdType _
instance showUserImportJobIdType :: Show UserImportJobIdType where
  show = genericShow
instance decodeUserImportJobIdType :: Decode UserImportJobIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportJobIdType :: Encode UserImportJobIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserImportJobNameType = UserImportJobNameType String
derive instance newtypeUserImportJobNameType :: Newtype UserImportJobNameType _
derive instance repGenericUserImportJobNameType :: Generic UserImportJobNameType _
instance showUserImportJobNameType :: Show UserImportJobNameType where
  show = genericShow
instance decodeUserImportJobNameType :: Decode UserImportJobNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportJobNameType :: Encode UserImportJobNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserImportJobStatusType = UserImportJobStatusType String
derive instance newtypeUserImportJobStatusType :: Newtype UserImportJobStatusType _
derive instance repGenericUserImportJobStatusType :: Generic UserImportJobStatusType _
instance showUserImportJobStatusType :: Show UserImportJobStatusType where
  show = genericShow
instance decodeUserImportJobStatusType :: Decode UserImportJobStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportJobStatusType :: Encode UserImportJobStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user import job type.</p>
newtype UserImportJobType = UserImportJobType 
  { "JobName" :: NullOrUndefined.NullOrUndefined (UserImportJobNameType)
  , "JobId" :: NullOrUndefined.NullOrUndefined (UserImportJobIdType)
  , "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "PreSignedUrl" :: NullOrUndefined.NullOrUndefined (PreSignedUrlType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "StartDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CompletionDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Status" :: NullOrUndefined.NullOrUndefined (UserImportJobStatusType)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (ArnType)
  , "ImportedUsers" :: NullOrUndefined.NullOrUndefined (LongType)
  , "SkippedUsers" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FailedUsers" :: NullOrUndefined.NullOrUndefined (LongType)
  , "CompletionMessage" :: NullOrUndefined.NullOrUndefined (CompletionMessageType)
  }
derive instance newtypeUserImportJobType :: Newtype UserImportJobType _
derive instance repGenericUserImportJobType :: Generic UserImportJobType _
instance showUserImportJobType :: Show UserImportJobType where
  show = genericShow
instance decodeUserImportJobType :: Decode UserImportJobType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportJobType :: Encode UserImportJobType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserImportJobsListType = UserImportJobsListType (Array UserImportJobType)
derive instance newtypeUserImportJobsListType :: Newtype UserImportJobsListType _
derive instance repGenericUserImportJobsListType :: Generic UserImportJobsListType _
instance showUserImportJobsListType :: Show UserImportJobsListType where
  show = genericShow
instance decodeUserImportJobsListType :: Decode UserImportJobsListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserImportJobsListType :: Encode UserImportJobsListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.</p>
newtype UserLambdaValidationException = UserLambdaValidationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserLambdaValidationException :: Newtype UserLambdaValidationException _
derive instance repGenericUserLambdaValidationException :: Generic UserLambdaValidationException _
instance showUserLambdaValidationException :: Show UserLambdaValidationException where
  show = genericShow
instance decodeUserLambdaValidationException :: Decode UserLambdaValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserLambdaValidationException :: Encode UserLambdaValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserMFASettingListType = UserMFASettingListType (Array StringType)
derive instance newtypeUserMFASettingListType :: Newtype UserMFASettingListType _
derive instance repGenericUserMFASettingListType :: Generic UserMFASettingListType _
instance showUserMFASettingListType :: Show UserMFASettingListType where
  show = genericShow
instance decodeUserMFASettingListType :: Decode UserMFASettingListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserMFASettingListType :: Encode UserMFASettingListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user is not confirmed successfully.</p>
newtype UserNotConfirmedException = UserNotConfirmedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserNotConfirmedException :: Newtype UserNotConfirmedException _
derive instance repGenericUserNotConfirmedException :: Generic UserNotConfirmedException _
instance showUserNotConfirmedException :: Show UserNotConfirmedException where
  show = genericShow
instance decodeUserNotConfirmedException :: Decode UserNotConfirmedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserNotConfirmedException :: Encode UserNotConfirmedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user is not found.</p>
newtype UserNotFoundException = UserNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserNotFoundException :: Newtype UserNotFoundException _
derive instance repGenericUserNotFoundException :: Generic UserNotFoundException _
instance showUserNotFoundException :: Show UserNotFoundException where
  show = genericShow
instance decodeUserNotFoundException :: Decode UserNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserNotFoundException :: Encode UserNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when user pool add-ons are not enabled.</p>
newtype UserPoolAddOnNotEnabledException = UserPoolAddOnNotEnabledException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserPoolAddOnNotEnabledException :: Newtype UserPoolAddOnNotEnabledException _
derive instance repGenericUserPoolAddOnNotEnabledException :: Generic UserPoolAddOnNotEnabledException _
instance showUserPoolAddOnNotEnabledException :: Show UserPoolAddOnNotEnabledException where
  show = genericShow
instance decodeUserPoolAddOnNotEnabledException :: Decode UserPoolAddOnNotEnabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolAddOnNotEnabledException :: Encode UserPoolAddOnNotEnabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user pool add-ons type.</p>
newtype UserPoolAddOnsType = UserPoolAddOnsType 
  { "AdvancedSecurityMode" :: (AdvancedSecurityModeType)
  }
derive instance newtypeUserPoolAddOnsType :: Newtype UserPoolAddOnsType _
derive instance repGenericUserPoolAddOnsType :: Generic UserPoolAddOnsType _
instance showUserPoolAddOnsType :: Show UserPoolAddOnsType where
  show = genericShow
instance decodeUserPoolAddOnsType :: Decode UserPoolAddOnsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolAddOnsType :: Encode UserPoolAddOnsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The description of the user pool client.</p>
newtype UserPoolClientDescription = UserPoolClientDescription 
  { "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "ClientName" :: NullOrUndefined.NullOrUndefined (ClientNameType)
  }
derive instance newtypeUserPoolClientDescription :: Newtype UserPoolClientDescription _
derive instance repGenericUserPoolClientDescription :: Generic UserPoolClientDescription _
instance showUserPoolClientDescription :: Show UserPoolClientDescription where
  show = genericShow
instance decodeUserPoolClientDescription :: Decode UserPoolClientDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolClientDescription :: Encode UserPoolClientDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolClientListType = UserPoolClientListType (Array UserPoolClientDescription)
derive instance newtypeUserPoolClientListType :: Newtype UserPoolClientListType _
derive instance repGenericUserPoolClientListType :: Generic UserPoolClientListType _
instance showUserPoolClientListType :: Show UserPoolClientListType where
  show = genericShow
instance decodeUserPoolClientListType :: Decode UserPoolClientListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolClientListType :: Encode UserPoolClientListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a user pool client.</p>
newtype UserPoolClientType = UserPoolClientType 
  { "UserPoolId" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "ClientName" :: NullOrUndefined.NullOrUndefined (ClientNameType)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (ClientIdType)
  , "ClientSecret" :: NullOrUndefined.NullOrUndefined (ClientSecretType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "RefreshTokenValidity" :: NullOrUndefined.NullOrUndefined (RefreshTokenValidityType)
  , "ReadAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "WriteAttributes" :: NullOrUndefined.NullOrUndefined (ClientPermissionListType)
  , "ExplicitAuthFlows" :: NullOrUndefined.NullOrUndefined (ExplicitAuthFlowsListType)
  , "SupportedIdentityProviders" :: NullOrUndefined.NullOrUndefined (SupportedIdentityProvidersListType)
  , "CallbackURLs" :: NullOrUndefined.NullOrUndefined (CallbackURLsListType)
  , "LogoutURLs" :: NullOrUndefined.NullOrUndefined (LogoutURLsListType)
  , "DefaultRedirectURI" :: NullOrUndefined.NullOrUndefined (RedirectUrlType)
  , "AllowedOAuthFlows" :: NullOrUndefined.NullOrUndefined (OAuthFlowsType)
  , "AllowedOAuthScopes" :: NullOrUndefined.NullOrUndefined (ScopeListType)
  , "AllowedOAuthFlowsUserPoolClient" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "AnalyticsConfiguration" :: NullOrUndefined.NullOrUndefined (AnalyticsConfigurationType)
  }
derive instance newtypeUserPoolClientType :: Newtype UserPoolClientType _
derive instance repGenericUserPoolClientType :: Generic UserPoolClientType _
instance showUserPoolClientType :: Show UserPoolClientType where
  show = genericShow
instance decodeUserPoolClientType :: Decode UserPoolClientType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolClientType :: Encode UserPoolClientType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A user pool description.</p>
newtype UserPoolDescriptionType = UserPoolDescriptionType 
  { "Id" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (UserPoolNameType)
  , "LambdaConfig" :: NullOrUndefined.NullOrUndefined (LambdaConfigType)
  , "Status" :: NullOrUndefined.NullOrUndefined (StatusType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeUserPoolDescriptionType :: Newtype UserPoolDescriptionType _
derive instance repGenericUserPoolDescriptionType :: Generic UserPoolDescriptionType _
instance showUserPoolDescriptionType :: Show UserPoolDescriptionType where
  show = genericShow
instance decodeUserPoolDescriptionType :: Decode UserPoolDescriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolDescriptionType :: Encode UserPoolDescriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolIdType = UserPoolIdType String
derive instance newtypeUserPoolIdType :: Newtype UserPoolIdType _
derive instance repGenericUserPoolIdType :: Generic UserPoolIdType _
instance showUserPoolIdType :: Show UserPoolIdType where
  show = genericShow
instance decodeUserPoolIdType :: Decode UserPoolIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolIdType :: Encode UserPoolIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolListType = UserPoolListType (Array UserPoolDescriptionType)
derive instance newtypeUserPoolListType :: Newtype UserPoolListType _
derive instance repGenericUserPoolListType :: Generic UserPoolListType _
instance showUserPoolListType :: Show UserPoolListType where
  show = genericShow
instance decodeUserPoolListType :: Decode UserPoolListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolListType :: Encode UserPoolListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolMfaType = UserPoolMfaType String
derive instance newtypeUserPoolMfaType :: Newtype UserPoolMfaType _
derive instance repGenericUserPoolMfaType :: Generic UserPoolMfaType _
instance showUserPoolMfaType :: Show UserPoolMfaType where
  show = genericShow
instance decodeUserPoolMfaType :: Decode UserPoolMfaType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolMfaType :: Encode UserPoolMfaType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolNameType = UserPoolNameType String
derive instance newtypeUserPoolNameType :: Newtype UserPoolNameType _
derive instance repGenericUserPoolNameType :: Generic UserPoolNameType _
instance showUserPoolNameType :: Show UserPoolNameType where
  show = genericShow
instance decodeUserPoolNameType :: Decode UserPoolNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolNameType :: Encode UserPoolNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The policy associated with a user pool.</p>
newtype UserPoolPolicyType = UserPoolPolicyType 
  { "PasswordPolicy" :: NullOrUndefined.NullOrUndefined (PasswordPolicyType)
  }
derive instance newtypeUserPoolPolicyType :: Newtype UserPoolPolicyType _
derive instance repGenericUserPoolPolicyType :: Generic UserPoolPolicyType _
instance showUserPoolPolicyType :: Show UserPoolPolicyType where
  show = genericShow
instance decodeUserPoolPolicyType :: Decode UserPoolPolicyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolPolicyType :: Encode UserPoolPolicyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a user pool tag cannot be set or updated.</p>
newtype UserPoolTaggingException = UserPoolTaggingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUserPoolTaggingException :: Newtype UserPoolTaggingException _
derive instance repGenericUserPoolTaggingException :: Generic UserPoolTaggingException _
instance showUserPoolTaggingException :: Show UserPoolTaggingException where
  show = genericShow
instance decodeUserPoolTaggingException :: Decode UserPoolTaggingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolTaggingException :: Encode UserPoolTaggingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserPoolTagsType = UserPoolTagsType (StrMap.StrMap StringType)
derive instance newtypeUserPoolTagsType :: Newtype UserPoolTagsType _
derive instance repGenericUserPoolTagsType :: Generic UserPoolTagsType _
instance showUserPoolTagsType :: Show UserPoolTagsType where
  show = genericShow
instance decodeUserPoolTagsType :: Decode UserPoolTagsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolTagsType :: Encode UserPoolTagsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for information about the user pool.</p>
newtype UserPoolType = UserPoolType 
  { "Id" :: NullOrUndefined.NullOrUndefined (UserPoolIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (UserPoolNameType)
  , "Policies" :: NullOrUndefined.NullOrUndefined (UserPoolPolicyType)
  , "LambdaConfig" :: NullOrUndefined.NullOrUndefined (LambdaConfigType)
  , "Status" :: NullOrUndefined.NullOrUndefined (StatusType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "SchemaAttributes" :: NullOrUndefined.NullOrUndefined (SchemaAttributesListType)
  , "AutoVerifiedAttributes" :: NullOrUndefined.NullOrUndefined (VerifiedAttributesListType)
  , "AliasAttributes" :: NullOrUndefined.NullOrUndefined (AliasAttributesListType)
  , "UsernameAttributes" :: NullOrUndefined.NullOrUndefined (UsernameAttributesListType)
  , "SmsVerificationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "EmailVerificationMessage" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageType)
  , "EmailVerificationSubject" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectType)
  , "VerificationMessageTemplate" :: NullOrUndefined.NullOrUndefined (VerificationMessageTemplateType)
  , "SmsAuthenticationMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "MfaConfiguration" :: NullOrUndefined.NullOrUndefined (UserPoolMfaType)
  , "DeviceConfiguration" :: NullOrUndefined.NullOrUndefined (DeviceConfigurationType)
  , "EstimatedNumberOfUsers" :: NullOrUndefined.NullOrUndefined (IntegerType)
  , "EmailConfiguration" :: NullOrUndefined.NullOrUndefined (EmailConfigurationType)
  , "SmsConfiguration" :: NullOrUndefined.NullOrUndefined (SmsConfigurationType)
  , "UserPoolTags" :: NullOrUndefined.NullOrUndefined (UserPoolTagsType)
  , "SmsConfigurationFailure" :: NullOrUndefined.NullOrUndefined (StringType)
  , "EmailConfigurationFailure" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Domain" :: NullOrUndefined.NullOrUndefined (DomainType)
  , "AdminCreateUserConfig" :: NullOrUndefined.NullOrUndefined (AdminCreateUserConfigType)
  , "UserPoolAddOns" :: NullOrUndefined.NullOrUndefined (UserPoolAddOnsType)
  }
derive instance newtypeUserPoolType :: Newtype UserPoolType _
derive instance repGenericUserPoolType :: Generic UserPoolType _
instance showUserPoolType :: Show UserPoolType where
  show = genericShow
instance decodeUserPoolType :: Decode UserPoolType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserPoolType :: Encode UserPoolType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserStatusType = UserStatusType String
derive instance newtypeUserStatusType :: Newtype UserStatusType _
derive instance repGenericUserStatusType :: Generic UserStatusType _
instance showUserStatusType :: Show UserStatusType where
  show = genericShow
instance decodeUserStatusType :: Decode UserStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserStatusType :: Encode UserStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user type.</p>
newtype UserType = UserType 
  { "Username" :: NullOrUndefined.NullOrUndefined (UsernameType)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (AttributeListType)
  , "UserCreateDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "UserLastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "UserStatus" :: NullOrUndefined.NullOrUndefined (UserStatusType)
  , "MFAOptions" :: NullOrUndefined.NullOrUndefined (MFAOptionListType)
  }
derive instance newtypeUserType :: Newtype UserType _
derive instance repGenericUserType :: Generic UserType _
instance showUserType :: Show UserType where
  show = genericShow
instance decodeUserType :: Decode UserType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserType :: Encode UserType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsernameAttributeType = UsernameAttributeType String
derive instance newtypeUsernameAttributeType :: Newtype UsernameAttributeType _
derive instance repGenericUsernameAttributeType :: Generic UsernameAttributeType _
instance showUsernameAttributeType :: Show UsernameAttributeType where
  show = genericShow
instance decodeUsernameAttributeType :: Decode UsernameAttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsernameAttributeType :: Encode UsernameAttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsernameAttributesListType = UsernameAttributesListType (Array UsernameAttributeType)
derive instance newtypeUsernameAttributesListType :: Newtype UsernameAttributesListType _
derive instance repGenericUsernameAttributesListType :: Generic UsernameAttributesListType _
instance showUsernameAttributesListType :: Show UsernameAttributesListType where
  show = genericShow
instance decodeUsernameAttributesListType :: Decode UsernameAttributesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsernameAttributesListType :: Encode UsernameAttributesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.</p>
newtype UsernameExistsException = UsernameExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeUsernameExistsException :: Newtype UsernameExistsException _
derive instance repGenericUsernameExistsException :: Generic UsernameExistsException _
instance showUsernameExistsException :: Show UsernameExistsException where
  show = genericShow
instance decodeUsernameExistsException :: Decode UsernameExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsernameExistsException :: Encode UsernameExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsernameType = UsernameType String
derive instance newtypeUsernameType :: Newtype UsernameType _
derive instance repGenericUsernameType :: Generic UsernameType _
instance showUsernameType :: Show UsernameType where
  show = genericShow
instance decodeUsernameType :: Decode UsernameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsernameType :: Encode UsernameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsersListType = UsersListType (Array UserType)
derive instance newtypeUsersListType :: Newtype UsersListType _
derive instance repGenericUsersListType :: Generic UsersListType _
instance showUsersListType :: Show UsersListType where
  show = genericShow
instance decodeUsersListType :: Decode UsersListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsersListType :: Encode UsersListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The template for verification messages.</p>
newtype VerificationMessageTemplateType = VerificationMessageTemplateType 
  { "SmsMessage" :: NullOrUndefined.NullOrUndefined (SmsVerificationMessageType)
  , "EmailMessage" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageType)
  , "EmailSubject" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectType)
  , "EmailMessageByLink" :: NullOrUndefined.NullOrUndefined (EmailVerificationMessageByLinkType)
  , "EmailSubjectByLink" :: NullOrUndefined.NullOrUndefined (EmailVerificationSubjectByLinkType)
  , "DefaultEmailOption" :: NullOrUndefined.NullOrUndefined (DefaultEmailOptionType)
  }
derive instance newtypeVerificationMessageTemplateType :: Newtype VerificationMessageTemplateType _
derive instance repGenericVerificationMessageTemplateType :: Generic VerificationMessageTemplateType _
instance showVerificationMessageTemplateType :: Show VerificationMessageTemplateType where
  show = genericShow
instance decodeVerificationMessageTemplateType :: Decode VerificationMessageTemplateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerificationMessageTemplateType :: Encode VerificationMessageTemplateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerifiedAttributeType = VerifiedAttributeType String
derive instance newtypeVerifiedAttributeType :: Newtype VerifiedAttributeType _
derive instance repGenericVerifiedAttributeType :: Generic VerifiedAttributeType _
instance showVerifiedAttributeType :: Show VerifiedAttributeType where
  show = genericShow
instance decodeVerifiedAttributeType :: Decode VerifiedAttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifiedAttributeType :: Encode VerifiedAttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerifiedAttributesListType = VerifiedAttributesListType (Array VerifiedAttributeType)
derive instance newtypeVerifiedAttributesListType :: Newtype VerifiedAttributesListType _
derive instance repGenericVerifiedAttributesListType :: Generic VerifiedAttributesListType _
instance showVerifiedAttributesListType :: Show VerifiedAttributesListType where
  show = genericShow
instance decodeVerifiedAttributesListType :: Decode VerifiedAttributesListType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifiedAttributesListType :: Encode VerifiedAttributesListType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerifySoftwareTokenRequest = VerifySoftwareTokenRequest 
  { "AccessToken" :: NullOrUndefined.NullOrUndefined (TokenModelType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  , "UserCode" :: (SoftwareTokenMFAUserCodeType)
  , "FriendlyDeviceName" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeVerifySoftwareTokenRequest :: Newtype VerifySoftwareTokenRequest _
derive instance repGenericVerifySoftwareTokenRequest :: Generic VerifySoftwareTokenRequest _
instance showVerifySoftwareTokenRequest :: Show VerifySoftwareTokenRequest where
  show = genericShow
instance decodeVerifySoftwareTokenRequest :: Decode VerifySoftwareTokenRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifySoftwareTokenRequest :: Encode VerifySoftwareTokenRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerifySoftwareTokenResponse = VerifySoftwareTokenResponse 
  { "Status" :: NullOrUndefined.NullOrUndefined (VerifySoftwareTokenResponseType)
  , "Session" :: NullOrUndefined.NullOrUndefined (SessionType)
  }
derive instance newtypeVerifySoftwareTokenResponse :: Newtype VerifySoftwareTokenResponse _
derive instance repGenericVerifySoftwareTokenResponse :: Generic VerifySoftwareTokenResponse _
instance showVerifySoftwareTokenResponse :: Show VerifySoftwareTokenResponse where
  show = genericShow
instance decodeVerifySoftwareTokenResponse :: Decode VerifySoftwareTokenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifySoftwareTokenResponse :: Encode VerifySoftwareTokenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerifySoftwareTokenResponseType = VerifySoftwareTokenResponseType String
derive instance newtypeVerifySoftwareTokenResponseType :: Newtype VerifySoftwareTokenResponseType _
derive instance repGenericVerifySoftwareTokenResponseType :: Generic VerifySoftwareTokenResponseType _
instance showVerifySoftwareTokenResponseType :: Show VerifySoftwareTokenResponseType where
  show = genericShow
instance decodeVerifySoftwareTokenResponseType :: Decode VerifySoftwareTokenResponseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifySoftwareTokenResponseType :: Encode VerifySoftwareTokenResponseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the request to verify user attributes.</p>
newtype VerifyUserAttributeRequest = VerifyUserAttributeRequest 
  { "AccessToken" :: (TokenModelType)
  , "AttributeName" :: (AttributeNameType)
  , "Code" :: (ConfirmationCodeType)
  }
derive instance newtypeVerifyUserAttributeRequest :: Newtype VerifyUserAttributeRequest _
derive instance repGenericVerifyUserAttributeRequest :: Generic VerifyUserAttributeRequest _
instance showVerifyUserAttributeRequest :: Show VerifyUserAttributeRequest where
  show = genericShow
instance decodeVerifyUserAttributeRequest :: Decode VerifyUserAttributeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyUserAttributeRequest :: Encode VerifyUserAttributeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container representing the response from the server from the request to verify user attributes.</p>
newtype VerifyUserAttributeResponse = VerifyUserAttributeResponse Types.NoArguments
derive instance newtypeVerifyUserAttributeResponse :: Newtype VerifyUserAttributeResponse _
derive instance repGenericVerifyUserAttributeResponse :: Generic VerifyUserAttributeResponse _
instance showVerifyUserAttributeResponse :: Show VerifyUserAttributeResponse where
  show = genericShow
instance decodeVerifyUserAttributeResponse :: Decode VerifyUserAttributeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyUserAttributeResponse :: Encode VerifyUserAttributeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
