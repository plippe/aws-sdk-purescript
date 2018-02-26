

-- | <fullname>Amazon Cognito</fullname> <p>Amazon Cognito is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. Amazon Cognito uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.</p> <p>Using Amazon Cognito, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon), and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.</p> <p>To provide end-user credentials, first make an unsigned call to <a>GetId</a>. If the end user is authenticated with one of the supported identity providers, set the <code>Logins</code> map with the identity provider token. <code>GetId</code> returns a unique identifier for the user.</p> <p>Next, make an unsigned call to <a>GetCredentialsForIdentity</a>. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. Assuming your identity pool has been configured via the <a>SetIdentityPoolRoles</a> operation, <code>GetCredentialsForIdentity</code> will return AWS credentials for your use. If your pool has not been configured with <code>SetIdentityPoolRoles</code>, or if you want to follow legacy flow, make an unsigned call to <a>GetOpenIdToken</a>, which returns the OpenID token necessary to call STS and retrieve AWS credentials. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. The token returned by <code>GetOpenIdToken</code> can be passed to the STS operation <a href="http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html">AssumeRoleWithWebIdentity</a> to retrieve AWS credentials.</p> <p>If you want to use Amazon Cognito in an Android, iOS, or Unity application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/index.html">AWS Mobile SDK Developer Guide</a>.</p>
module AWS.CognitoIdentity where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CognitoIdentity" :: String


-- | <p>Creates a new identity pool. The identity pool is a store of user identity information that is specific to your AWS account. The limit on identity pools is 60 per account. The keys for <code>SupportedLoginProviders</code> are as follows:</p> <ul> <li> <p>Facebook: <code>graph.facebook.com</code> </p> </li> <li> <p>Google: <code>accounts.google.com</code> </p> </li> <li> <p>Amazon: <code>www.amazon.com</code> </p> </li> <li> <p>Twitter: <code>api.twitter.com</code> </p> </li> <li> <p>Digits: <code>www.digits.com</code> </p> </li> </ul> <p>You must use AWS Developer credentials to call this API.</p>
createIdentityPool :: forall eff. CreateIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) IdentityPool
createIdentityPool = AWS.request serviceName "CreateIdentityPool" 


-- | <p>Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentities :: forall eff. DeleteIdentitiesInput -> Aff (err :: AWS.RequestError | eff) DeleteIdentitiesResponse
deleteIdentities = AWS.request serviceName "DeleteIdentities" 


-- | <p>Deletes a user pool. Once a pool is deleted, users will not be able to authenticate with the pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentityPool :: forall eff. DeleteIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteIdentityPool = AWS.request serviceName "DeleteIdentityPool" 


-- | <p>Returns metadata related to the given identity, including when the identity was created and any associated linked logins.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentity :: forall eff. DescribeIdentityInput -> Aff (err :: AWS.RequestError | eff) IdentityDescription
describeIdentity = AWS.request serviceName "DescribeIdentity" 


-- | <p>Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentityPool :: forall eff. DescribeIdentityPoolInput -> Aff (err :: AWS.RequestError | eff) IdentityPool
describeIdentityPool = AWS.request serviceName "DescribeIdentityPool" 


-- | <p>Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getCredentialsForIdentity :: forall eff. GetCredentialsForIdentityInput -> Aff (err :: AWS.RequestError | eff) GetCredentialsForIdentityResponse
getCredentialsForIdentity = AWS.request serviceName "GetCredentialsForIdentity" 


-- | <p>Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getId :: forall eff. GetIdInput -> Aff (err :: AWS.RequestError | eff) GetIdResponse
getId = AWS.request serviceName "GetId" 


-- | <p>Gets the roles for an identity pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
getIdentityPoolRoles :: forall eff. GetIdentityPoolRolesInput -> Aff (err :: AWS.RequestError | eff) GetIdentityPoolRolesResponse
getIdentityPoolRoles = AWS.request serviceName "GetIdentityPoolRoles" 


-- | <p>Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by <a>GetId</a>. You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.</p> <p>The OpenId token is valid for 15 minutes.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getOpenIdToken :: forall eff. GetOpenIdTokenInput -> Aff (err :: AWS.RequestError | eff) GetOpenIdTokenResponse
getOpenIdToken = AWS.request serviceName "GetOpenIdToken" 


-- | <p>Registers (or retrieves) a Cognito <code>IdentityId</code> and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the <code>Logins</code> map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.</p> <p>You can use <code>GetOpenIdTokenForDeveloperIdentity</code> to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the <code>IdentityId</code> should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing <code>IdentityId</code>. This API will create the identity in the specified <code>IdentityPoolId</code>.</p> <p>You must use AWS Developer credentials to call this API.</p>
getOpenIdTokenForDeveloperIdentity :: forall eff. GetOpenIdTokenForDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) GetOpenIdTokenForDeveloperIdentityResponse
getOpenIdTokenForDeveloperIdentity = AWS.request serviceName "GetOpenIdTokenForDeveloperIdentity" 


-- | <p>Lists the identities in a pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentities :: forall eff. ListIdentitiesInput -> Aff (err :: AWS.RequestError | eff) ListIdentitiesResponse
listIdentities = AWS.request serviceName "ListIdentities" 


-- | <p>Lists all of the Cognito identity pools registered for your account.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentityPools :: forall eff. ListIdentityPoolsInput -> Aff (err :: AWS.RequestError | eff) ListIdentityPoolsResponse
listIdentityPools = AWS.request serviceName "ListIdentityPools" 


-- | <p>Retrieves the <code>IdentityID</code> associated with a <code>DeveloperUserIdentifier</code> or the list of <code>DeveloperUserIdentifier</code>s associated with an <code>IdentityId</code> for an existing identity. Either <code>IdentityID</code> or <code>DeveloperUserIdentifier</code> must not be null. If you supply only one of these values, the other value will be searched in the database and returned as a part of the response. If you supply both, <code>DeveloperUserIdentifier</code> will be matched against <code>IdentityID</code>. If the values are verified against the database, the response returns both values and is the same as the request. Otherwise a <code>ResourceConflictException</code> is thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
lookupDeveloperIdentity :: forall eff. LookupDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) LookupDeveloperIdentityResponse
lookupDeveloperIdentity = AWS.request serviceName "LookupDeveloperIdentity" 


-- | <p>Merges two users having different <code>IdentityId</code>s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (<code>SourceUserIdentifier</code>) with the <code>IdentityId</code> of the <code>DestinationUserIdentifier</code>. Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
mergeDeveloperIdentities :: forall eff. MergeDeveloperIdentitiesInput -> Aff (err :: AWS.RequestError | eff) MergeDeveloperIdentitiesResponse
mergeDeveloperIdentities = AWS.request serviceName "MergeDeveloperIdentities" 


-- | <p>Sets the roles for an identity pool. These roles are used when making calls to <a>GetCredentialsForIdentity</a> action.</p> <p>You must use AWS Developer credentials to call this API.</p>
setIdentityPoolRoles :: forall eff. SetIdentityPoolRolesInput -> Aff (err :: AWS.RequestError | eff) Unit
setIdentityPoolRoles = AWS.request serviceName "SetIdentityPoolRoles" 


-- | <p>Unlinks a <code>DeveloperUserIdentifier</code> from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.</p> <p>You must use AWS Developer credentials to call this API.</p>
unlinkDeveloperIdentity :: forall eff. UnlinkDeveloperIdentityInput -> Aff (err :: AWS.RequestError | eff) Unit
unlinkDeveloperIdentity = AWS.request serviceName "UnlinkDeveloperIdentity" 


-- | <p>Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
unlinkIdentity :: forall eff. UnlinkIdentityInput -> Aff (err :: AWS.RequestError | eff) Unit
unlinkIdentity = AWS.request serviceName "UnlinkIdentity" 


-- | <p>Updates a user pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
updateIdentityPool :: forall eff. IdentityPool -> Aff (err :: AWS.RequestError | eff) IdentityPool
updateIdentityPool = AWS.request serviceName "UpdateIdentityPool" 


newtype ARNString = ARNString String


newtype AccessKeyString = AccessKeyString String


newtype AccountId = AccountId String


newtype AmbiguousRoleResolutionType = AmbiguousRoleResolutionType String


newtype ClaimName = ClaimName String


newtype ClaimValue = ClaimValue String


-- | <p>A provider representing an Amazon Cognito Identity User Pool and its client ID.</p>
newtype CognitoIdentityProvider = CognitoIdentityProvider 
  { "ProviderName" :: NullOrUndefined (CognitoIdentityProviderName)
  , "ClientId" :: NullOrUndefined (CognitoIdentityProviderClientId)
  , "ServerSideTokenCheck" :: NullOrUndefined (CognitoIdentityProviderTokenCheck)
  }


newtype CognitoIdentityProviderClientId = CognitoIdentityProviderClientId String


newtype CognitoIdentityProviderList = CognitoIdentityProviderList (Array CognitoIdentityProvider)


newtype CognitoIdentityProviderName = CognitoIdentityProviderName String


newtype CognitoIdentityProviderTokenCheck = CognitoIdentityProviderTokenCheck Boolean


-- | <p>Thrown if there are parallel requests to modify a resource.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (String)
  }


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


-- | <p>Credentials for the provided identity ID.</p>
newtype Credentials = Credentials 
  { "AccessKeyId" :: NullOrUndefined (AccessKeyString)
  , "SecretKey" :: NullOrUndefined (SecretKeyString)
  , "SessionToken" :: NullOrUndefined (SessionTokenString)
  , "Expiration" :: NullOrUndefined (DateType)
  }


newtype DateType = DateType Number


-- | <p>Input to the <code>DeleteIdentities</code> action.</p>
newtype DeleteIdentitiesInput = DeleteIdentitiesInput 
  { "IdentityIdsToDelete" :: (IdentityIdList)
  }


-- | <p>Returned in response to a successful <code>DeleteIdentities</code> operation.</p>
newtype DeleteIdentitiesResponse = DeleteIdentitiesResponse 
  { "UnprocessedIdentityIds" :: NullOrUndefined (UnprocessedIdentityIdList)
  }


-- | <p>Input to the DeleteIdentityPool action.</p>
newtype DeleteIdentityPoolInput = DeleteIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | <p>Input to the <code>DescribeIdentity</code> action.</p>
newtype DescribeIdentityInput = DescribeIdentityInput 
  { "IdentityId" :: (IdentityId)
  }


-- | <p>Input to the DescribeIdentityPool action.</p>
newtype DescribeIdentityPoolInput = DescribeIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


newtype DeveloperProviderName = DeveloperProviderName String


-- | <p>The provided developer user identifier is already registered with Cognito under a different identity ID.</p>
newtype DeveloperUserAlreadyRegisteredException = DeveloperUserAlreadyRegisteredException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype DeveloperUserIdentifier = DeveloperUserIdentifier String


newtype DeveloperUserIdentifierList = DeveloperUserIdentifierList (Array DeveloperUserIdentifier)


newtype ErrorCode = ErrorCode String


-- | <p>An exception thrown when a dependent service such as Facebook or Twitter is not responding</p>
newtype ExternalServiceException = ExternalServiceException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Input to the <code>GetCredentialsForIdentity</code> action.</p>
newtype GetCredentialsForIdentityInput = GetCredentialsForIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  , "CustomRoleArn" :: NullOrUndefined (ARNString)
  }


-- | <p>Returned in response to a successful <code>GetCredentialsForIdentity</code> operation.</p>
newtype GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Credentials" :: NullOrUndefined (Credentials)
  }


-- | <p>Input to the GetId action.</p>
newtype GetIdInput = GetIdInput 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  }


-- | <p>Returned in response to a GetId request.</p>
newtype GetIdResponse = GetIdResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  }


-- | <p>Input to the <code>GetIdentityPoolRoles</code> action.</p>
newtype GetIdentityPoolRolesInput = GetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }


-- | <p>Returned in response to a successful <code>GetIdentityPoolRoles</code> operation.</p>
newtype GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "Roles" :: NullOrUndefined (RolesMap)
  , "RoleMappings" :: NullOrUndefined (RoleMappingMap)
  }


-- | <p>Input to the <code>GetOpenIdTokenForDeveloperIdentity</code> action.</p>
newtype GetOpenIdTokenForDeveloperIdentityInput = GetOpenIdTokenForDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined (IdentityId)
  , "Logins" :: (LoginsMap)
  , "TokenDuration" :: NullOrUndefined (TokenDuration)
  }


-- | <p>Returned in response to a successful <code>GetOpenIdTokenForDeveloperIdentity</code> request.</p>
newtype GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined (OIDCToken)
  }


-- | <p>Input to the GetOpenIdToken action.</p>
newtype GetOpenIdTokenInput = GetOpenIdTokenInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsMap)
  }


-- | <p>Returned in response to a successful GetOpenIdToken request.</p>
newtype GetOpenIdTokenResponse = GetOpenIdTokenResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined (OIDCToken)
  }


newtype HideDisabled = HideDisabled Boolean


newtype IdentitiesList = IdentitiesList (Array IdentityDescription)


-- | <p>A description of the identity.</p>
newtype IdentityDescription = IdentityDescription 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "Logins" :: NullOrUndefined (LoginsList)
  , "CreationDate" :: NullOrUndefined (DateType)
  , "LastModifiedDate" :: NullOrUndefined (DateType)
  }


newtype IdentityId = IdentityId String


newtype IdentityIdList = IdentityIdList (Array IdentityId)


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


newtype IdentityPoolId = IdentityPoolId String


newtype IdentityPoolName = IdentityPoolName String


-- | <p>A description of the identity pool.</p>
newtype IdentityPoolShortDescription = IdentityPoolShortDescription 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "IdentityPoolName" :: NullOrUndefined (IdentityPoolName)
  }


newtype IdentityPoolUnauthenticated = IdentityPoolUnauthenticated Boolean


newtype IdentityPoolsList = IdentityPoolsList (Array IdentityPoolShortDescription)


newtype IdentityProviderId = IdentityProviderId String


newtype IdentityProviderName = IdentityProviderName String


newtype IdentityProviderToken = IdentityProviderToken String


newtype IdentityProviders = IdentityProviders (Map IdentityProviderName IdentityProviderId)


-- | <p>Thrown when the service encounters an error during processing the request.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Thrown if the identity pool has no role associated for the given auth type (auth/unauth) or if the AssumeRole fails.</p>
newtype InvalidIdentityPoolConfigurationException = InvalidIdentityPoolConfigurationException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Thrown for missing or bad input parameter(s).</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Thrown when the total number of user pools has exceeded a preset limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Input to the ListIdentities action.</p>
newtype ListIdentitiesInput = ListIdentitiesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  , "HideDisabled" :: NullOrUndefined (HideDisabled)
  }


-- | <p>The response to a ListIdentities request.</p>
newtype ListIdentitiesResponse = ListIdentitiesResponse 
  { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId)
  , "Identities" :: NullOrUndefined (IdentitiesList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }


-- | <p>Input to the ListIdentityPools action.</p>
newtype ListIdentityPoolsInput = ListIdentityPoolsInput 
  { "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }


-- | <p>The result of a successful ListIdentityPools action.</p>
newtype ListIdentityPoolsResponse = ListIdentityPoolsResponse 
  { "IdentityPools" :: NullOrUndefined (IdentityPoolsList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }


newtype LoginsList = LoginsList (Array IdentityProviderName)


newtype LoginsMap = LoginsMap (Map IdentityProviderName IdentityProviderToken)


-- | <p>Input to the <code>LookupDeveloperIdentityInput</code> action.</p>
newtype LookupDeveloperIdentityInput = LookupDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifier" :: NullOrUndefined (DeveloperUserIdentifier)
  , "MaxResults" :: NullOrUndefined (QueryLimit)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }


-- | <p>Returned in response to a successful <code>LookupDeveloperIdentity</code> action.</p>
newtype LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifierList" :: NullOrUndefined (DeveloperUserIdentifierList)
  , "NextToken" :: NullOrUndefined (PaginationKey)
  }


-- | <p>A rule that maps a claim name, a claim value, and a match type to a role ARN.</p>
newtype MappingRule = MappingRule 
  { "Claim" :: (ClaimName)
  , "MatchType" :: (MappingRuleMatchType)
  , "Value" :: (ClaimValue)
  , "RoleARN" :: (ARNString)
  }


newtype MappingRuleMatchType = MappingRuleMatchType String


newtype MappingRulesList = MappingRulesList (Array MappingRule)


-- | <p>Input to the <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesInput = MergeDeveloperIdentitiesInput 
  { "SourceUserIdentifier" :: (DeveloperUserIdentifier)
  , "DestinationUserIdentifier" :: (DeveloperUserIdentifier)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "IdentityPoolId" :: (IdentityPoolId)
  }


-- | <p>Returned in response to a successful <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  }


-- | <p>Thrown when a user is not authorized to access the requested resource.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype OIDCProviderList = OIDCProviderList (Array ARNString)


newtype OIDCToken = OIDCToken String


newtype PaginationKey = PaginationKey String


newtype QueryLimit = QueryLimit Int


-- | <p>Thrown when a user tries to use a login which is already linked to another account.</p>
newtype ResourceConflictException = ResourceConflictException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Thrown when the requested resource (for example, a dataset or record) does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>A role mapping.</p>
newtype RoleMapping = RoleMapping 
  { "Type" :: (RoleMappingType)
  , "AmbiguousRoleResolution" :: NullOrUndefined (AmbiguousRoleResolutionType)
  , "RulesConfiguration" :: NullOrUndefined (RulesConfigurationType)
  }


newtype RoleMappingMap = RoleMappingMap (Map IdentityProviderName RoleMapping)


newtype RoleMappingType = RoleMappingType String


newtype RoleType = RoleType String


newtype RolesMap = RolesMap (Map RoleType ARNString)


-- | <p>A container for rules.</p>
newtype RulesConfigurationType = RulesConfigurationType 
  { "Rules" :: (MappingRulesList)
  }


newtype SAMLProviderList = SAMLProviderList (Array ARNString)


newtype SecretKeyString = SecretKeyString String


newtype SessionTokenString = SessionTokenString String


-- | <p>Input to the <code>SetIdentityPoolRoles</code> action.</p>
newtype SetIdentityPoolRolesInput = SetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "Roles" :: (RolesMap)
  , "RoleMappings" :: NullOrUndefined (RoleMappingMap)
  }


newtype TokenDuration = TokenDuration Number


-- | <p>Thrown when a request is throttled.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Input to the <code>UnlinkDeveloperIdentity</code> action.</p>
newtype UnlinkDeveloperIdentityInput = UnlinkDeveloperIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "DeveloperUserIdentifier" :: (DeveloperUserIdentifier)
  }


-- | <p>Input to the UnlinkIdentity action.</p>
newtype UnlinkIdentityInput = UnlinkIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: (LoginsMap)
  , "LoginsToRemove" :: (LoginsList)
  }


-- | <p>An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.</p>
newtype UnprocessedIdentityId = UnprocessedIdentityId 
  { "IdentityId" :: NullOrUndefined (IdentityId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  }


newtype UnprocessedIdentityIdList = UnprocessedIdentityIdList (Array UnprocessedIdentityId)
