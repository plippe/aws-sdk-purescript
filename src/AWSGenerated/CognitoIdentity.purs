

-- | <fullname>Amazon Cognito</fullname> <p>Amazon Cognito is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. Amazon Cognito uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.</p> <p>Using Amazon Cognito, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon), and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.</p> <p>To provide end-user credentials, first make an unsigned call to <a>GetId</a>. If the end user is authenticated with one of the supported identity providers, set the <code>Logins</code> map with the identity provider token. <code>GetId</code> returns a unique identifier for the user.</p> <p>Next, make an unsigned call to <a>GetCredentialsForIdentity</a>. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. Assuming your identity pool has been configured via the <a>SetIdentityPoolRoles</a> operation, <code>GetCredentialsForIdentity</code> will return AWS credentials for your use. If your pool has not been configured with <code>SetIdentityPoolRoles</code>, or if you want to follow legacy flow, make an unsigned call to <a>GetOpenIdToken</a>, which returns the OpenID token necessary to call STS and retrieve AWS credentials. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. The token returned by <code>GetOpenIdToken</code> can be passed to the STS operation <a href="http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html">AssumeRoleWithWebIdentity</a> to retrieve AWS credentials.</p> <p>If you want to use Amazon Cognito in an Android, iOS, or Unity application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/index.html">AWS Mobile SDK Developer Guide</a>.</p>
module AWS.CognitoIdentity where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CognitoIdentity" :: String


-- | <p>Creates a new identity pool. The identity pool is a store of user identity information that is specific to your AWS account. The limit on identity pools is 60 per account. The keys for <code>SupportedLoginProviders</code> are as follows:</p> <ul> <li> <p>Facebook: <code>graph.facebook.com</code> </p> </li> <li> <p>Google: <code>accounts.google.com</code> </p> </li> <li> <p>Amazon: <code>www.amazon.com</code> </p> </li> <li> <p>Twitter: <code>api.twitter.com</code> </p> </li> <li> <p>Digits: <code>www.digits.com</code> </p> </li> </ul> <p>You must use AWS Developer credentials to call this API.</p>
createIdentityPool :: forall eff. CreateIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) IdentityPool
createIdentityPool = AWS.request serviceName "createIdentityPool" 


-- | <p>Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentities :: forall eff. DeleteIdentitiesInput -> Aff (err :: AWS.RequestError | eff) DeleteIdentitiesResponse
deleteIdentities = AWS.request serviceName "deleteIdentities" 


-- | <p>Deletes a user pool. Once a pool is deleted, users will not be able to authenticate with the pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentityPool :: forall eff. DeleteIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteIdentityPool = AWS.request serviceName "deleteIdentityPool" 


-- | <p>Returns metadata related to the given identity, including when the identity was created and any associated linked logins.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentity :: forall eff. DescribeIdentityInput -> Aff (err :: AWS.RequestError | eff) IdentityDescription
describeIdentity = AWS.request serviceName "describeIdentity" 


-- | <p>Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentityPool :: forall eff. DescribeIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) IdentityPool
describeIdentityPool = AWS.request serviceName "describeIdentityPool" 


-- | <p>Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getCredentialsForIdentity :: forall eff. GetCredentialsForIdentityInput -> Aff (err :: AWS.RequestError | eff) GetCredentialsForIdentityResponse
getCredentialsForIdentity = AWS.request serviceName "getCredentialsForIdentity" 


-- | <p>Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getId :: forall eff. GetIdInput -> Aff (err :: AWS.RequestError | eff) GetIdResponse
getId = AWS.request serviceName "getId" 


-- | <p>Gets the roles for an identity pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
getIdentityPoolRoles :: forall eff. GetIdentityPoolRolesInput -> Aff (err :: AWS.RequestError | eff) GetIdentityPoolRolesResponse
getIdentityPoolRoles = AWS.request serviceName "getIdentityPoolRoles" 


-- | <p>Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by <a>GetId</a>. You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.</p> <p>The OpenId token is valid for 15 minutes.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getOpenIdToken :: forall eff. GetOpenIdTokenInput -> Aff (err :: AWS.RequestError | eff) GetOpenIdTokenResponse
getOpenIdToken = AWS.request serviceName "getOpenIdToken" 


-- | <p>Registers (or retrieves) a Cognito <code>IdentityId</code> and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the <code>Logins</code> map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.</p> <p>You can use <code>GetOpenIdTokenForDeveloperIdentity</code> to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the <code>IdentityId</code> should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing <code>IdentityId</code>. This API will create the identity in the specified <code>IdentityPoolId</code>.</p> <p>You must use AWS Developer credentials to call this API.</p>
getOpenIdTokenForDeveloperIdentity :: forall eff. GetOpenIdTokenForDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) GetOpenIdTokenForDeveloperIdentityResponse
getOpenIdTokenForDeveloperIdentity = AWS.request serviceName "getOpenIdTokenForDeveloperIdentity" 


-- | <p>Lists the identities in a pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentities :: forall eff. ListIdentitiesInput -> Aff (err :: AWS.RequestError | eff) ListIdentitiesResponse
listIdentities = AWS.request serviceName "listIdentities" 


-- | <p>Lists all of the Cognito identity pools registered for your account.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentityPools :: forall eff. ListIdentityPoolsInput -> Aff (err :: AWS.RequestError | eff) ListIdentityPoolsResponse
listIdentityPools = AWS.request serviceName "listIdentityPools" 


-- | <p>Retrieves the <code>IdentityID</code> associated with a <code>DeveloperUserIdentifier</code> or the list of <code>DeveloperUserIdentifier</code>s associated with an <code>IdentityId</code> for an existing identity. Either <code>IdentityID</code> or <code>DeveloperUserIdentifier</code> must not be null. If you supply only one of these values, the other value will be searched in the database and returned as a part of the response. If you supply both, <code>DeveloperUserIdentifier</code> will be matched against <code>IdentityID</code>. If the values are verified against the database, the response returns both values and is the same as the request. Otherwise a <code>ResourceConflictException</code> is thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
lookupDeveloperIdentity :: forall eff. LookupDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) LookupDeveloperIdentityResponse
lookupDeveloperIdentity = AWS.request serviceName "lookupDeveloperIdentity" 


-- | <p>Merges two users having different <code>IdentityId</code>s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (<code>SourceUserIdentifier</code>) with the <code>IdentityId</code> of the <code>DestinationUserIdentifier</code>. Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
mergeDeveloperIdentities :: forall eff. MergeDeveloperIdentitiesInput -> Aff (err :: AWS.RequestError | eff) MergeDeveloperIdentitiesResponse
mergeDeveloperIdentities = AWS.request serviceName "mergeDeveloperIdentities" 


-- | <p>Sets the roles for an identity pool. These roles are used when making calls to <a>GetCredentialsForIdentity</a> action.</p> <p>You must use AWS Developer credentials to call this API.</p>
setIdentityPoolRoles :: forall eff. SetIdentityPoolRolesInput -> Aff (err :: AWS.RequestError | eff) Unit
setIdentityPoolRoles = AWS.request serviceName "setIdentityPoolRoles" 


-- | <p>Unlinks a <code>DeveloperUserIdentifier</code> from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.</p> <p>You must use AWS Developer credentials to call this API.</p>
unlinkDeveloperIdentity :: forall eff. UnlinkDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) Unit
unlinkDeveloperIdentity = AWS.request serviceName "unlinkDeveloperIdentity" 


-- | <p>Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
unlinkIdentity :: forall eff. UnlinkIdentityInput -> Aff (err :: AWS.RequestError | eff) Unit
unlinkIdentity = AWS.request serviceName "unlinkIdentity" 


-- | <p>Updates a user pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
updateIdentityPool :: forall eff. IdentityPool -> Aff (err :: AWS.RequestError | eff) IdentityPool
updateIdentityPool = AWS.request serviceName "updateIdentityPool" 


newtype ARNString = ARNString String
derive instance newtypeARNString :: Newtype ARNString _


newtype AccessKeyString = AccessKeyString String
derive instance newtypeAccessKeyString :: Newtype AccessKeyString _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


newtype AmbiguousRoleResolutionType = AmbiguousRoleResolutionType String
derive instance newtypeAmbiguousRoleResolutionType :: Newtype AmbiguousRoleResolutionType _


newtype ClaimName = ClaimName String
derive instance newtypeClaimName :: Newtype ClaimName _


newtype ClaimValue = ClaimValue String
derive instance newtypeClaimValue :: Newtype ClaimValue _


-- | <p>A provider representing an Amazon Cognito Identity User Pool and its client ID.</p>
newtype CognitoIdentityProvider = CognitoIdentityProvider 
  { "ProviderName" :: NullOrUndefined (CognitoIdentityProviderName)
  , "ClientId" :: NullOrUndefined (CognitoIdentityProviderClientId)
  , "ServerSideTokenCheck" :: NullOrUndefined (CognitoIdentityProviderTokenCheck)
  }
derive instance newtypeCognitoIdentityProvider :: Newtype CognitoIdentityProvider _


newtype CognitoIdentityProviderClientId = CognitoIdentityProviderClientId String
derive instance newtypeCognitoIdentityProviderClientId :: Newtype CognitoIdentityProviderClientId _


newtype CognitoIdentityProviderList = CognitoIdentityProviderList (Array CognitoIdentityProvider)
derive instance newtypeCognitoIdentityProviderList :: Newtype CognitoIdentityProviderList _


newtype CognitoIdentityProviderName = CognitoIdentityProviderName String
derive instance newtypeCognitoIdentityProviderName :: Newtype CognitoIdentityProviderName _


newtype CognitoIdentityProviderTokenCheck = CognitoIdentityProviderTokenCheck Boolean
derive instance newtypeCognitoIdentityProviderTokenCheck :: Newtype CognitoIdentityProviderTokenCheck _


-- | <p>Thrown if there are parallel requests to modify a resource.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>Input to the CreateIdentityPool action.</p>
newtype CreateIdentityPoolInput = CreateIdentityPoolInput 
  { "IdentityPoolName" :: (IdentityPoolName)
  , "AllowUnauthenticatedIdentities" :: (IdentityPoolUnauthenticated)
  , "SupportedLoginProviders" :: NullOrUndefined (IdentityProviders)
  , "DeveloperProviderName" :: NullOrUndefined (DeveloperProviderName)
  , "OpenIdConnectProviderARNs" :: NullOrUndefined (OIDCProviderList)
  , "CognitoIdentityProviders" :: NullOrUndefined (CognitoIdentityProviderList)
  , "SamlProviderARNs" :: NullOrUndefined (SAMLProviderList)
  }
derive instance newtypeCreateIdentityPoolInput :: Newtype CreateIdentityPoolInput _


-- | <p>Credentials for the provided identity ID.</p>
newtype Credentials = Credentials 
  { "AccessKeyId" :: NullOrUndefined (AccessKeyString)
  , "SecretKey" :: NullOrUndefined (SecretKeyString)
  , "SessionToken" :: NullOrUndefined (SessionTokenString)
  , "Expiration" :: NullOrUndefined (DateType)
  }
derive instance newtypeCredentials :: Newtype Credentials _


newtype DateType = DateType Number
derive instance newtypeDateType :: Newtype DateType _


-- | <p>Input to the <code>DeleteIdentities</code> action.</p>
newtype DeleteIdentitiesInput = DeleteIdentitiesInput 
  { "IdentityIdsToDelete" :: (IdentityIdList)
  }
derive instance newtypeDeleteIdentitiesInput :: Newtype DeleteIdentitiesInput _


-- | <p>Returned in response to a successful <code>DeleteIdentities</code> operation.</p>
newtype DeleteIdentitiesResponse = DeleteIdentitiesResponse 
  { "UnprocessedIdentityIds" :: NullOrUndefined (UnprocessedIdentityIdList)
  }
derive instance newtypeDeleteIdentitiesResponse :: Newtype DeleteIdentitiesResponse _


-- | <p>Input to the DeleteIdentityPool action.</p>
newtype DeleteIdentityPoolInput = DeleteIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeDeleteIdentityPoolInput :: Newtype DeleteIdentityPoolInput _


-- | <p>Input to the <code>DescribeIdentity</code> action.</p>
newtype DescribeIdentityInput = DescribeIdentityInput 
  { "IdentityId" :: (IdentityId)
  }
derive instance newtypeDescribeIdentityInput :: Newtype DescribeIdentityInput _


-- | <p>Input to the DescribeIdentityPool action.</p>
newtype DescribeIdentityPoolInput = DescribeIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeDescribeIdentityPoolInput :: Newtype DescribeIdentityPoolInput _


newtype DeveloperProviderName = DeveloperProviderName String
derive instance newtypeDeveloperProviderName :: Newtype DeveloperProviderName _


-- | <p>The provided developer user identifier is already registered with Cognito under a different identity ID.</p>
newtype DeveloperUserAlreadyRegisteredException = DeveloperUserAlreadyRegisteredException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeDeveloperUserAlreadyRegisteredException :: Newtype DeveloperUserAlreadyRegisteredException _


newtype DeveloperUserIdentifier = DeveloperUserIdentifier String
derive instance newtypeDeveloperUserIdentifier :: Newtype DeveloperUserIdentifier _


newtype DeveloperUserIdentifierList = DeveloperUserIdentifierList (Array DeveloperUserIdentifier)
derive instance newtypeDeveloperUserIdentifierList :: Newtype DeveloperUserIdentifierList _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


-- | <p>An exception thrown when a dependent service such as Facebook or Twitter is not responding</p>
newtype ExternalServiceException = ExternalServiceException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeExternalServiceException :: Newtype ExternalServiceException _


-- | <p>Input to the <code>GetCredentialsForIdentity</code> action.</p>
newtype GetCredentialsForIdentityInput = GetCredentialsForIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  , "CustomRoleArn" :: NullOrUndefined (ARNString)
  }
derive instance newtypeGetCredentialsForIdentityInput :: Newtype GetCredentialsForIdentityInput _


-- | <p>Returned in response to a successful <code>GetCredentialsForIdentity</code> operation.</p>
newtype GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Credentials" :: NullOrUndefined (Credentials)
  }
derive instance newtypeGetCredentialsForIdentityResponse :: Newtype GetCredentialsForIdentityResponse _


-- | <p>Input to the GetId action.</p>
newtype GetIdInput = GetIdInput 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  }
derive instance newtypeGetIdInput :: Newtype GetIdInput _


-- | <p>Returned in response to a GetId request.</p>
newtype GetIdResponse = GetIdResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  }
derive instance newtypeGetIdResponse :: Newtype GetIdResponse _


-- | <p>Input to the <code>GetIdentityPoolRoles</code> action.</p>
newtype GetIdentityPoolRolesInput = GetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeGetIdentityPoolRolesInput :: Newtype GetIdentityPoolRolesInput _


-- | <p>Returned in response to a successful <code>GetIdentityPoolRoles</code> operation.</p>
newtype GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "Roles" :: NullOrUndefined (RolesMap)
  , "RoleMappings" :: NullOrUndefined (RoleMappingMap)
  }
derive instance newtypeGetIdentityPoolRolesResponse :: Newtype GetIdentityPoolRolesResponse _


-- | <p>Input to the <code>GetOpenIdTokenForDeveloperIdentity</code> action.</p>
newtype GetOpenIdTokenForDeveloperIdentityInput = GetOpenIdTokenForDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined (IdentityId)
  , "Logins" :: (LoginsMap)
  , "TokenDuration" :: NullOrUndefined (TokenDuration)
  }
derive instance newtypeGetOpenIdTokenForDeveloperIdentityInput :: Newtype GetOpenIdTokenForDeveloperIdentityInput _


-- | <p>Returned in response to a successful <code>GetOpenIdTokenForDeveloperIdentity</code> request.</p>
newtype GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined (OIDCToken)
  }
derive instance newtypeGetOpenIdTokenForDeveloperIdentityResponse :: Newtype GetOpenIdTokenForDeveloperIdentityResponse _


-- | <p>Input to the GetOpenIdToken action.</p>
newtype GetOpenIdTokenInput = GetOpenIdTokenInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  }
derive instance newtypeGetOpenIdTokenInput :: Newtype GetOpenIdTokenInput _


-- | <p>Returned in response to a successful GetOpenIdToken request.</p>
newtype GetOpenIdTokenResponse = GetOpenIdTokenResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined (OIDCToken)
  }
derive instance newtypeGetOpenIdTokenResponse :: Newtype GetOpenIdTokenResponse _


newtype HideDisabled = HideDisabled Boolean
derive instance newtypeHideDisabled :: Newtype HideDisabled _


newtype IdentitiesList = IdentitiesList (Array IdentityDescription)
derive instance newtypeIdentitiesList :: Newtype IdentitiesList _


-- | <p>A description of the identity.</p>
newtype IdentityDescription = IdentityDescription 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsList)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  }
derive instance newtypeIdentityDescription :: Newtype IdentityDescription _


newtype IdentityId = IdentityId String
derive instance newtypeIdentityId :: Newtype IdentityId _


newtype IdentityIdList = IdentityIdList (Array IdentityId)
derive instance newtypeIdentityIdList :: Newtype IdentityIdList _


-- | <p>An object representing an Amazon Cognito identity pool.</p>
newtype IdentityPool = IdentityPool 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityPoolName" :: (IdentityPoolName)
  , "AllowUnauthenticatedIdentities" :: (IdentityPoolUnauthenticated)
  , "SupportedLoginProviders" :: NullOrUndefined (IdentityProviders)
  , "DeveloperProviderName" :: NullOrUndefined (DeveloperProviderName)
  , "OpenIdConnectProviderARNs" :: NullOrUndefined (OIDCProviderList)
  , "CognitoIdentityProviders" :: NullOrUndefined (CognitoIdentityProviderList)
  , "SamlProviderARNs" :: NullOrUndefined (SAMLProviderList)
  }
derive instance newtypeIdentityPool :: Newtype IdentityPool _


newtype IdentityPoolId = IdentityPoolId String
derive instance newtypeIdentityPoolId :: Newtype IdentityPoolId _


newtype IdentityPoolName = IdentityPoolName String
derive instance newtypeIdentityPoolName :: Newtype IdentityPoolName _


-- | <p>A description of the identity pool.</p>
newtype IdentityPoolShortDescription = IdentityPoolShortDescription 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "IdentityPoolName" :: NullOrUndefined (IdentityPoolName)
  }
derive instance newtypeIdentityPoolShortDescription :: Newtype IdentityPoolShortDescription _


newtype IdentityPoolUnauthenticated = IdentityPoolUnauthenticated Boolean
derive instance newtypeIdentityPoolUnauthenticated :: Newtype IdentityPoolUnauthenticated _


newtype IdentityPoolsList = IdentityPoolsList (Array IdentityPoolShortDescription)
derive instance newtypeIdentityPoolsList :: Newtype IdentityPoolsList _


newtype IdentityProviderId = IdentityProviderId String
derive instance newtypeIdentityProviderId :: Newtype IdentityProviderId _


newtype IdentityProviderName = IdentityProviderName String
derive instance newtypeIdentityProviderName :: Newtype IdentityProviderName _


newtype IdentityProviderToken = IdentityProviderToken String
derive instance newtypeIdentityProviderToken :: Newtype IdentityProviderToken _


newtype IdentityProviders = IdentityProviders (Map IdentityProviderName IdentityProviderId)
derive instance newtypeIdentityProviders :: Newtype IdentityProviders _


-- | <p>Thrown when the service encounters an error during processing the request.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _


-- | <p>Thrown if the identity pool has no role associated for the given auth type (auth/unauth) or if the AssumeRole fails.</p>
newtype InvalidIdentityPoolConfigurationException = InvalidIdentityPoolConfigurationException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidIdentityPoolConfigurationException :: Newtype InvalidIdentityPoolConfigurationException _


-- | <p>Thrown for missing or bad input parameter(s).</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>Thrown when the total number of user pools has exceeded a preset limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>Input to the ListIdentities action.</p>
newtype ListIdentitiesInput = ListIdentitiesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  , "HideDisabled" :: NullOrUndefined (HideDisabled)
  }
derive instance newtypeListIdentitiesInput :: Newtype ListIdentitiesInput _


-- | <p>The response to a ListIdentities request.</p>
newtype ListIdentitiesResponse = ListIdentitiesResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "Identities" :: NullOrUndefined (IdentitiesList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentitiesResponse :: Newtype ListIdentitiesResponse _


-- | <p>Input to the ListIdentityPools action.</p>
newtype ListIdentityPoolsInput = ListIdentityPoolsInput 
  { "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentityPoolsInput :: Newtype ListIdentityPoolsInput _


-- | <p>The result of a successful ListIdentityPools action.</p>
newtype ListIdentityPoolsResponse = ListIdentityPoolsResponse 
  { "IdentityPools" :: NullOrUndefined (IdentityPoolsList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentityPoolsResponse :: Newtype ListIdentityPoolsResponse _


newtype LoginsList = LoginsList (Array IdentityProviderName)
derive instance newtypeLoginsList :: Newtype LoginsList _


newtype LoginsMap = LoginsMap (Map IdentityProviderName IdentityProviderToken)
derive instance newtypeLoginsMap :: Newtype LoginsMap _


-- | <p>Input to the <code>LookupDeveloperIdentityInput</code> action.</p>
newtype LookupDeveloperIdentityInput = LookupDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifier" :: NullOrUndefined (DeveloperUserIdentifier)
  , "MaxResults" :: NullOrUndefined (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeLookupDeveloperIdentityInput :: Newtype LookupDeveloperIdentityInput _


-- | <p>Returned in response to a successful <code>LookupDeveloperIdentity</code> action.</p>
newtype LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifierList" :: NullOrUndefined (DeveloperUserIdentifierList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }
derive instance newtypeLookupDeveloperIdentityResponse :: Newtype LookupDeveloperIdentityResponse _


-- | <p>A rule that maps a claim name, a claim value, and a match type to a role ARN.</p>
newtype MappingRule = MappingRule 
  { "Claim" :: (ClaimName)
  , "MatchType" :: (MappingRuleMatchType)
  , "Value" :: (ClaimValue)
  , "RoleARN" :: (ARNString)
  }
derive instance newtypeMappingRule :: Newtype MappingRule _


newtype MappingRuleMatchType = MappingRuleMatchType String
derive instance newtypeMappingRuleMatchType :: Newtype MappingRuleMatchType _


newtype MappingRulesList = MappingRulesList (Array MappingRule)
derive instance newtypeMappingRulesList :: Newtype MappingRulesList _


-- | <p>Input to the <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesInput = MergeDeveloperIdentitiesInput 
  { "SourceUserIdentifier" :: (DeveloperUserIdentifier)
  , "DestinationUserIdentifier" :: (DeveloperUserIdentifier)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeMergeDeveloperIdentitiesInput :: Newtype MergeDeveloperIdentitiesInput _


-- | <p>Returned in response to a successful <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  }
derive instance newtypeMergeDeveloperIdentitiesResponse :: Newtype MergeDeveloperIdentitiesResponse _


-- | <p>Thrown when a user is not authorized to access the requested resource.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _


newtype OIDCProviderList = OIDCProviderList (Array ARNString)
derive instance newtypeOIDCProviderList :: Newtype OIDCProviderList _


newtype OIDCToken = OIDCToken String
derive instance newtypeOIDCToken :: Newtype OIDCToken _


newtype PaginationKey = PaginationKey String
derive instance newtypePaginationKey :: Newtype PaginationKey _


newtype QueryLimit = QueryLimit Int
derive instance newtypeQueryLimit :: Newtype QueryLimit _


-- | <p>Thrown when a user tries to use a login which is already linked to another account.</p>
newtype ResourceConflictException = ResourceConflictException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeResourceConflictException :: Newtype ResourceConflictException _


-- | <p>Thrown when the requested resource (for example, a dataset or record) does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>A role mapping.</p>
newtype RoleMapping = RoleMapping 
  { "Type" :: (RoleMappingType)
  , "AmbiguousRoleResolution" :: NullOrUndefined (AmbiguousRoleResolutionType)
  , "RulesConfiguration" :: NullOrUndefined (RulesConfigurationType)
  }
derive instance newtypeRoleMapping :: Newtype RoleMapping _


newtype RoleMappingMap = RoleMappingMap (Map IdentityProviderName RoleMapping)
derive instance newtypeRoleMappingMap :: Newtype RoleMappingMap _


newtype RoleMappingType = RoleMappingType String
derive instance newtypeRoleMappingType :: Newtype RoleMappingType _


newtype RoleType = RoleType String
derive instance newtypeRoleType :: Newtype RoleType _


newtype RolesMap = RolesMap (Map RoleType ARNString)
derive instance newtypeRolesMap :: Newtype RolesMap _


-- | <p>A container for rules.</p>
newtype RulesConfigurationType = RulesConfigurationType 
  { "Rules" :: (MappingRulesList)
  }
derive instance newtypeRulesConfigurationType :: Newtype RulesConfigurationType _


newtype SAMLProviderList = SAMLProviderList (Array ARNString)
derive instance newtypeSAMLProviderList :: Newtype SAMLProviderList _


newtype SecretKeyString = SecretKeyString String
derive instance newtypeSecretKeyString :: Newtype SecretKeyString _


newtype SessionTokenString = SessionTokenString String
derive instance newtypeSessionTokenString :: Newtype SessionTokenString _


-- | <p>Input to the <code>SetIdentityPoolRoles</code> action.</p>
newtype SetIdentityPoolRolesInput = SetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "Roles" :: (RolesMap)
  , "RoleMappings" :: NullOrUndefined (RoleMappingMap)
  }
derive instance newtypeSetIdentityPoolRolesInput :: Newtype SetIdentityPoolRolesInput _


newtype TokenDuration = TokenDuration Number
derive instance newtypeTokenDuration :: Newtype TokenDuration _


-- | <p>Thrown when a request is throttled.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | <p>Input to the <code>UnlinkDeveloperIdentity</code> action.</p>
newtype UnlinkDeveloperIdentityInput = UnlinkDeveloperIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "DeveloperUserIdentifier" :: (DeveloperUserIdentifier)
  }
derive instance newtypeUnlinkDeveloperIdentityInput :: Newtype UnlinkDeveloperIdentityInput _


-- | <p>Input to the UnlinkIdentity action.</p>
newtype UnlinkIdentityInput = UnlinkIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: (LoginsMap)
  , "LoginsToRemove" :: (LoginsList)
  }
derive instance newtypeUnlinkIdentityInput :: Newtype UnlinkIdentityInput _


-- | <p>An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.</p>
newtype UnprocessedIdentityId = UnprocessedIdentityId 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeUnprocessedIdentityId :: Newtype UnprocessedIdentityId _


newtype UnprocessedIdentityIdList = UnprocessedIdentityIdList (Array UnprocessedIdentityId)
derive instance newtypeUnprocessedIdentityIdList :: Newtype UnprocessedIdentityIdList _
