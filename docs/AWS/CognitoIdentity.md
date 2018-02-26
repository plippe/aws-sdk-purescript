## Module AWS.CognitoIdentity

<fullname>Amazon Cognito</fullname> <p>Amazon Cognito is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. Amazon Cognito uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.</p> <p>Using Amazon Cognito, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon), and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.</p> <p>To provide end-user credentials, first make an unsigned call to <a>GetId</a>. If the end user is authenticated with one of the supported identity providers, set the <code>Logins</code> map with the identity provider token. <code>GetId</code> returns a unique identifier for the user.</p> <p>Next, make an unsigned call to <a>GetCredentialsForIdentity</a>. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. Assuming your identity pool has been configured via the <a>SetIdentityPoolRoles</a> operation, <code>GetCredentialsForIdentity</code> will return AWS credentials for your use. If your pool has not been configured with <code>SetIdentityPoolRoles</code>, or if you want to follow legacy flow, make an unsigned call to <a>GetOpenIdToken</a>, which returns the OpenID token necessary to call STS and retrieve AWS credentials. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. The token returned by <code>GetOpenIdToken</code> can be passed to the STS operation <a href="http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html">AssumeRoleWithWebIdentity</a> to retrieve AWS credentials.</p> <p>If you want to use Amazon Cognito in an Android, iOS, or Unity application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/index.html">AWS Mobile SDK Developer Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createIdentityPool`

``` purescript
createIdentityPool :: forall eff. CreateIdentityPoolInput -> Aff (err :: RequestError | eff) IdentityPool
```

<p>Creates a new identity pool. The identity pool is a store of user identity information that is specific to your AWS account. The limit on identity pools is 60 per account. The keys for <code>SupportedLoginProviders</code> are as follows:</p> <ul> <li> <p>Facebook: <code>graph.facebook.com</code> </p> </li> <li> <p>Google: <code>accounts.google.com</code> </p> </li> <li> <p>Amazon: <code>www.amazon.com</code> </p> </li> <li> <p>Twitter: <code>api.twitter.com</code> </p> </li> <li> <p>Digits: <code>www.digits.com</code> </p> </li> </ul> <p>You must use AWS Developer credentials to call this API.</p>

#### `deleteIdentities`

``` purescript
deleteIdentities :: forall eff. DeleteIdentitiesInput -> Aff (err :: RequestError | eff) DeleteIdentitiesResponse
```

<p>Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `deleteIdentityPool`

``` purescript
deleteIdentityPool :: forall eff. DeleteIdentityPoolInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a user pool. Once a pool is deleted, users will not be able to authenticate with the pool.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `describeIdentity`

``` purescript
describeIdentity :: forall eff. DescribeIdentityInput -> Aff (err :: RequestError | eff) IdentityDescription
```

<p>Returns metadata related to the given identity, including when the identity was created and any associated linked logins.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `describeIdentityPool`

``` purescript
describeIdentityPool :: forall eff. DescribeIdentityPoolInput -> Aff (err :: RequestError | eff) IdentityPool
```

<p>Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `getCredentialsForIdentity`

``` purescript
getCredentialsForIdentity :: forall eff. GetCredentialsForIdentityInput -> Aff (err :: RequestError | eff) GetCredentialsForIdentityResponse
```

<p>Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.</p> <p>This is a public API. You do not need any credentials to call this API.</p>

#### `getId`

``` purescript
getId :: forall eff. GetIdInput -> Aff (err :: RequestError | eff) GetIdResponse
```

<p>Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.</p> <p>This is a public API. You do not need any credentials to call this API.</p>

#### `getIdentityPoolRoles`

``` purescript
getIdentityPoolRoles :: forall eff. GetIdentityPoolRolesInput -> Aff (err :: RequestError | eff) GetIdentityPoolRolesResponse
```

<p>Gets the roles for an identity pool.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `getOpenIdToken`

``` purescript
getOpenIdToken :: forall eff. GetOpenIdTokenInput -> Aff (err :: RequestError | eff) GetOpenIdTokenResponse
```

<p>Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by <a>GetId</a>. You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.</p> <p>The OpenId token is valid for 15 minutes.</p> <p>This is a public API. You do not need any credentials to call this API.</p>

#### `getOpenIdTokenForDeveloperIdentity`

``` purescript
getOpenIdTokenForDeveloperIdentity :: forall eff. GetOpenIdTokenForDeveloperIdentityInput -> Aff (err :: RequestError | eff) GetOpenIdTokenForDeveloperIdentityResponse
```

<p>Registers (or retrieves) a Cognito <code>IdentityId</code> and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the <code>Logins</code> map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.</p> <p>You can use <code>GetOpenIdTokenForDeveloperIdentity</code> to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the <code>IdentityId</code> should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing <code>IdentityId</code>. This API will create the identity in the specified <code>IdentityPoolId</code>.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `listIdentities`

``` purescript
listIdentities :: forall eff. ListIdentitiesInput -> Aff (err :: RequestError | eff) ListIdentitiesResponse
```

<p>Lists the identities in a pool.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `listIdentityPools`

``` purescript
listIdentityPools :: forall eff. ListIdentityPoolsInput -> Aff (err :: RequestError | eff) ListIdentityPoolsResponse
```

<p>Lists all of the Cognito identity pools registered for your account.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `lookupDeveloperIdentity`

``` purescript
lookupDeveloperIdentity :: forall eff. LookupDeveloperIdentityInput -> Aff (err :: RequestError | eff) LookupDeveloperIdentityResponse
```

<p>Retrieves the <code>IdentityID</code> associated with a <code>DeveloperUserIdentifier</code> or the list of <code>DeveloperUserIdentifier</code>s associated with an <code>IdentityId</code> for an existing identity. Either <code>IdentityID</code> or <code>DeveloperUserIdentifier</code> must not be null. If you supply only one of these values, the other value will be searched in the database and returned as a part of the response. If you supply both, <code>DeveloperUserIdentifier</code> will be matched against <code>IdentityID</code>. If the values are verified against the database, the response returns both values and is the same as the request. Otherwise a <code>ResourceConflictException</code> is thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `mergeDeveloperIdentities`

``` purescript
mergeDeveloperIdentities :: forall eff. MergeDeveloperIdentitiesInput -> Aff (err :: RequestError | eff) MergeDeveloperIdentitiesResponse
```

<p>Merges two users having different <code>IdentityId</code>s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (<code>SourceUserIdentifier</code>) with the <code>IdentityId</code> of the <code>DestinationUserIdentifier</code>. Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `setIdentityPoolRoles`

``` purescript
setIdentityPoolRoles :: forall eff. SetIdentityPoolRolesInput -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the roles for an identity pool. These roles are used when making calls to <a>GetCredentialsForIdentity</a> action.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `unlinkDeveloperIdentity`

``` purescript
unlinkDeveloperIdentity :: forall eff. UnlinkDeveloperIdentityInput -> Aff (err :: RequestError | eff) Unit
```

<p>Unlinks a <code>DeveloperUserIdentifier</code> from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `unlinkIdentity`

``` purescript
unlinkIdentity :: forall eff. UnlinkIdentityInput -> Aff (err :: RequestError | eff) Unit
```

<p>Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.</p> <p>This is a public API. You do not need any credentials to call this API.</p>

#### `updateIdentityPool`

``` purescript
updateIdentityPool :: forall eff. IdentityPool -> Aff (err :: RequestError | eff) IdentityPool
```

<p>Updates a user pool.</p> <p>You must use AWS Developer credentials to call this API.</p>

#### `ARNString`

``` purescript
newtype ARNString
  = ARNString String
```

#### `AccessKeyString`

``` purescript
newtype AccessKeyString
  = AccessKeyString String
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

#### `AmbiguousRoleResolutionType`

``` purescript
newtype AmbiguousRoleResolutionType
  = AmbiguousRoleResolutionType String
```

#### `ClaimName`

``` purescript
newtype ClaimName
  = ClaimName String
```

#### `ClaimValue`

``` purescript
newtype ClaimValue
  = ClaimValue String
```

#### `CognitoIdentityProvider`

``` purescript
newtype CognitoIdentityProvider
  = CognitoIdentityProvider { "ProviderName" :: NullOrUndefined (CognitoIdentityProviderName), "ClientId" :: NullOrUndefined (CognitoIdentityProviderClientId), "ServerSideTokenCheck" :: NullOrUndefined (CognitoIdentityProviderTokenCheck) }
```

<p>A provider representing an Amazon Cognito Identity User Pool and its client ID.</p>

#### `CognitoIdentityProviderClientId`

``` purescript
newtype CognitoIdentityProviderClientId
  = CognitoIdentityProviderClientId String
```

#### `CognitoIdentityProviderList`

``` purescript
newtype CognitoIdentityProviderList
  = CognitoIdentityProviderList (Array CognitoIdentityProvider)
```

#### `CognitoIdentityProviderName`

``` purescript
newtype CognitoIdentityProviderName
  = CognitoIdentityProviderName String
```

#### `CognitoIdentityProviderTokenCheck`

``` purescript
newtype CognitoIdentityProviderTokenCheck
  = CognitoIdentityProviderTokenCheck Boolean
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown if there are parallel requests to modify a resource.</p>

#### `CreateIdentityPoolInput`

``` purescript
newtype CreateIdentityPoolInput
  = CreateIdentityPoolInput { "IdentityPoolName" :: IdentityPoolName, "AllowUnauthenticatedIdentities" :: IdentityPoolUnauthenticated, "SupportedLoginProviders" :: NullOrUndefined (IdentityProviders), "DeveloperProviderName" :: NullOrUndefined (DeveloperProviderName), "OpenIdConnectProviderARNs" :: NullOrUndefined (OIDCProviderList), "CognitoIdentityProviders" :: NullOrUndefined (CognitoIdentityProviderList), "SamlProviderARNs" :: NullOrUndefined (SAMLProviderList) }
```

<p>Input to the CreateIdentityPool action.</p>

#### `Credentials`

``` purescript
newtype Credentials
  = Credentials { "AccessKeyId" :: NullOrUndefined (AccessKeyString), "SecretKey" :: NullOrUndefined (SecretKeyString), "SessionToken" :: NullOrUndefined (SessionTokenString), "Expiration" :: NullOrUndefined (DateType) }
```

<p>Credentials for the provided identity ID.</p>

#### `DateType`

``` purescript
newtype DateType
  = DateType Number
```

#### `DeleteIdentitiesInput`

``` purescript
newtype DeleteIdentitiesInput
  = DeleteIdentitiesInput { "IdentityIdsToDelete" :: IdentityIdList }
```

<p>Input to the <code>DeleteIdentities</code> action.</p>

#### `DeleteIdentitiesResponse`

``` purescript
newtype DeleteIdentitiesResponse
  = DeleteIdentitiesResponse { "UnprocessedIdentityIds" :: NullOrUndefined (UnprocessedIdentityIdList) }
```

<p>Returned in response to a successful <code>DeleteIdentities</code> operation.</p>

#### `DeleteIdentityPoolInput`

``` purescript
newtype DeleteIdentityPoolInput
  = DeleteIdentityPoolInput { "IdentityPoolId" :: IdentityPoolId }
```

<p>Input to the DeleteIdentityPool action.</p>

#### `DescribeIdentityInput`

``` purescript
newtype DescribeIdentityInput
  = DescribeIdentityInput { "IdentityId" :: IdentityId }
```

<p>Input to the <code>DescribeIdentity</code> action.</p>

#### `DescribeIdentityPoolInput`

``` purescript
newtype DescribeIdentityPoolInput
  = DescribeIdentityPoolInput { "IdentityPoolId" :: IdentityPoolId }
```

<p>Input to the DescribeIdentityPool action.</p>

#### `DeveloperProviderName`

``` purescript
newtype DeveloperProviderName
  = DeveloperProviderName String
```

#### `DeveloperUserAlreadyRegisteredException`

``` purescript
newtype DeveloperUserAlreadyRegisteredException
  = DeveloperUserAlreadyRegisteredException { "Message'" :: NullOrUndefined (String) }
```

<p>The provided developer user identifier is already registered with Cognito under a different identity ID.</p>

#### `DeveloperUserIdentifier`

``` purescript
newtype DeveloperUserIdentifier
  = DeveloperUserIdentifier String
```

#### `DeveloperUserIdentifierList`

``` purescript
newtype DeveloperUserIdentifierList
  = DeveloperUserIdentifierList (Array DeveloperUserIdentifier)
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `ExternalServiceException`

``` purescript
newtype ExternalServiceException
  = ExternalServiceException { "Message'" :: NullOrUndefined (String) }
```

<p>An exception thrown when a dependent service such as Facebook or Twitter is not responding</p>

#### `GetCredentialsForIdentityInput`

``` purescript
newtype GetCredentialsForIdentityInput
  = GetCredentialsForIdentityInput { "IdentityId" :: IdentityId, "Logins" :: NullOrUndefined (LoginsMap), "CustomRoleArn" :: NullOrUndefined (ARNString) }
```

<p>Input to the <code>GetCredentialsForIdentity</code> action.</p>

#### `GetCredentialsForIdentityResponse`

``` purescript
newtype GetCredentialsForIdentityResponse
  = GetCredentialsForIdentityResponse { "IdentityId" :: NullOrUndefined (IdentityId), "Credentials" :: NullOrUndefined (Credentials) }
```

<p>Returned in response to a successful <code>GetCredentialsForIdentity</code> operation.</p>

#### `GetIdInput`

``` purescript
newtype GetIdInput
  = GetIdInput { "AccountId" :: NullOrUndefined (AccountId), "IdentityPoolId" :: IdentityPoolId, "Logins" :: NullOrUndefined (LoginsMap) }
```

<p>Input to the GetId action.</p>

#### `GetIdResponse`

``` purescript
newtype GetIdResponse
  = GetIdResponse { "IdentityId" :: NullOrUndefined (IdentityId) }
```

<p>Returned in response to a GetId request.</p>

#### `GetIdentityPoolRolesInput`

``` purescript
newtype GetIdentityPoolRolesInput
  = GetIdentityPoolRolesInput { "IdentityPoolId" :: IdentityPoolId }
```

<p>Input to the <code>GetIdentityPoolRoles</code> action.</p>

#### `GetIdentityPoolRolesResponse`

``` purescript
newtype GetIdentityPoolRolesResponse
  = GetIdentityPoolRolesResponse { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId), "Roles" :: NullOrUndefined (RolesMap), "RoleMappings" :: NullOrUndefined (RoleMappingMap) }
```

<p>Returned in response to a successful <code>GetIdentityPoolRoles</code> operation.</p>

#### `GetOpenIdTokenForDeveloperIdentityInput`

``` purescript
newtype GetOpenIdTokenForDeveloperIdentityInput
  = GetOpenIdTokenForDeveloperIdentityInput { "IdentityPoolId" :: IdentityPoolId, "IdentityId" :: NullOrUndefined (IdentityId), "Logins" :: LoginsMap, "TokenDuration" :: NullOrUndefined (TokenDuration) }
```

<p>Input to the <code>GetOpenIdTokenForDeveloperIdentity</code> action.</p>

#### `GetOpenIdTokenForDeveloperIdentityResponse`

``` purescript
newtype GetOpenIdTokenForDeveloperIdentityResponse
  = GetOpenIdTokenForDeveloperIdentityResponse { "IdentityId" :: NullOrUndefined (IdentityId), "Token" :: NullOrUndefined (OIDCToken) }
```

<p>Returned in response to a successful <code>GetOpenIdTokenForDeveloperIdentity</code> request.</p>

#### `GetOpenIdTokenInput`

``` purescript
newtype GetOpenIdTokenInput
  = GetOpenIdTokenInput { "IdentityId" :: IdentityId, "Logins" :: NullOrUndefined (LoginsMap) }
```

<p>Input to the GetOpenIdToken action.</p>

#### `GetOpenIdTokenResponse`

``` purescript
newtype GetOpenIdTokenResponse
  = GetOpenIdTokenResponse { "IdentityId" :: NullOrUndefined (IdentityId), "Token" :: NullOrUndefined (OIDCToken) }
```

<p>Returned in response to a successful GetOpenIdToken request.</p>

#### `HideDisabled`

``` purescript
newtype HideDisabled
  = HideDisabled Boolean
```

#### `IdentitiesList`

``` purescript
newtype IdentitiesList
  = IdentitiesList (Array IdentityDescription)
```

#### `IdentityDescription`

``` purescript
newtype IdentityDescription
  = IdentityDescription { "IdentityId" :: NullOrUndefined (IdentityId), "Logins" :: NullOrUndefined (LoginsList), "CreationDate" :: NullOrUndefined (DateType), "LastModifiedDate" :: NullOrUndefined (DateType) }
```

<p>A description of the identity.</p>

#### `IdentityId`

``` purescript
newtype IdentityId
  = IdentityId String
```

#### `IdentityIdList`

``` purescript
newtype IdentityIdList
  = IdentityIdList (Array IdentityId)
```

#### `IdentityPool`

``` purescript
newtype IdentityPool
  = IdentityPool { "IdentityPoolId" :: IdentityPoolId, "IdentityPoolName" :: IdentityPoolName, "AllowUnauthenticatedIdentities" :: IdentityPoolUnauthenticated, "SupportedLoginProviders" :: NullOrUndefined (IdentityProviders), "DeveloperProviderName" :: NullOrUndefined (DeveloperProviderName), "OpenIdConnectProviderARNs" :: NullOrUndefined (OIDCProviderList), "CognitoIdentityProviders" :: NullOrUndefined (CognitoIdentityProviderList), "SamlProviderARNs" :: NullOrUndefined (SAMLProviderList) }
```

<p>An object representing an Amazon Cognito identity pool.</p>

#### `IdentityPoolId`

``` purescript
newtype IdentityPoolId
  = IdentityPoolId String
```

#### `IdentityPoolName`

``` purescript
newtype IdentityPoolName
  = IdentityPoolName String
```

#### `IdentityPoolShortDescription`

``` purescript
newtype IdentityPoolShortDescription
  = IdentityPoolShortDescription { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId), "IdentityPoolName" :: NullOrUndefined (IdentityPoolName) }
```

<p>A description of the identity pool.</p>

#### `IdentityPoolUnauthenticated`

``` purescript
newtype IdentityPoolUnauthenticated
  = IdentityPoolUnauthenticated Boolean
```

#### `IdentityPoolsList`

``` purescript
newtype IdentityPoolsList
  = IdentityPoolsList (Array IdentityPoolShortDescription)
```

#### `IdentityProviderId`

``` purescript
newtype IdentityProviderId
  = IdentityProviderId String
```

#### `IdentityProviderName`

``` purescript
newtype IdentityProviderName
  = IdentityProviderName String
```

#### `IdentityProviderToken`

``` purescript
newtype IdentityProviderToken
  = IdentityProviderToken String
```

#### `IdentityProviders`

``` purescript
newtype IdentityProviders
  = IdentityProviders (Map IdentityProviderName IdentityProviderId)
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when the service encounters an error during processing the request.</p>

#### `InvalidIdentityPoolConfigurationException`

``` purescript
newtype InvalidIdentityPoolConfigurationException
  = InvalidIdentityPoolConfigurationException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown if the identity pool has no role associated for the given auth type (auth/unauth) or if the AssumeRole fails.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown for missing or bad input parameter(s).</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when the total number of user pools has exceeded a preset limit.</p>

#### `ListIdentitiesInput`

``` purescript
newtype ListIdentitiesInput
  = ListIdentitiesInput { "IdentityPoolId" :: IdentityPoolId, "MaxResults" :: QueryLimit, "NextToken" :: NullOrUndefined (PaginationKey), "HideDisabled" :: NullOrUndefined (HideDisabled) }
```

<p>Input to the ListIdentities action.</p>

#### `ListIdentitiesResponse`

``` purescript
newtype ListIdentitiesResponse
  = ListIdentitiesResponse { "IdentityPoolId" :: NullOrUndefined (IdentityPoolId), "Identities" :: NullOrUndefined (IdentitiesList), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>The response to a ListIdentities request.</p>

#### `ListIdentityPoolsInput`

``` purescript
newtype ListIdentityPoolsInput
  = ListIdentityPoolsInput { "MaxResults" :: QueryLimit, "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Input to the ListIdentityPools action.</p>

#### `ListIdentityPoolsResponse`

``` purescript
newtype ListIdentityPoolsResponse
  = ListIdentityPoolsResponse { "IdentityPools" :: NullOrUndefined (IdentityPoolsList), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>The result of a successful ListIdentityPools action.</p>

#### `LoginsList`

``` purescript
newtype LoginsList
  = LoginsList (Array IdentityProviderName)
```

#### `LoginsMap`

``` purescript
newtype LoginsMap
  = LoginsMap (Map IdentityProviderName IdentityProviderToken)
```

#### `LookupDeveloperIdentityInput`

``` purescript
newtype LookupDeveloperIdentityInput
  = LookupDeveloperIdentityInput { "IdentityPoolId" :: IdentityPoolId, "IdentityId" :: NullOrUndefined (IdentityId), "DeveloperUserIdentifier" :: NullOrUndefined (DeveloperUserIdentifier), "MaxResults" :: NullOrUndefined (QueryLimit), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Input to the <code>LookupDeveloperIdentityInput</code> action.</p>

#### `LookupDeveloperIdentityResponse`

``` purescript
newtype LookupDeveloperIdentityResponse
  = LookupDeveloperIdentityResponse { "IdentityId" :: NullOrUndefined (IdentityId), "DeveloperUserIdentifierList" :: NullOrUndefined (DeveloperUserIdentifierList), "NextToken" :: NullOrUndefined (PaginationKey) }
```

<p>Returned in response to a successful <code>LookupDeveloperIdentity</code> action.</p>

#### `MappingRule`

``` purescript
newtype MappingRule
  = MappingRule { "Claim" :: ClaimName, "MatchType" :: MappingRuleMatchType, "Value" :: ClaimValue, "RoleARN" :: ARNString }
```

<p>A rule that maps a claim name, a claim value, and a match type to a role ARN.</p>

#### `MappingRuleMatchType`

``` purescript
newtype MappingRuleMatchType
  = MappingRuleMatchType String
```

#### `MappingRulesList`

``` purescript
newtype MappingRulesList
  = MappingRulesList (Array MappingRule)
```

#### `MergeDeveloperIdentitiesInput`

``` purescript
newtype MergeDeveloperIdentitiesInput
  = MergeDeveloperIdentitiesInput { "SourceUserIdentifier" :: DeveloperUserIdentifier, "DestinationUserIdentifier" :: DeveloperUserIdentifier, "DeveloperProviderName" :: DeveloperProviderName, "IdentityPoolId" :: IdentityPoolId }
```

<p>Input to the <code>MergeDeveloperIdentities</code> action.</p>

#### `MergeDeveloperIdentitiesResponse`

``` purescript
newtype MergeDeveloperIdentitiesResponse
  = MergeDeveloperIdentitiesResponse { "IdentityId" :: NullOrUndefined (IdentityId) }
```

<p>Returned in response to a successful <code>MergeDeveloperIdentities</code> action.</p>

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when a user is not authorized to access the requested resource.</p>

#### `OIDCProviderList`

``` purescript
newtype OIDCProviderList
  = OIDCProviderList (Array ARNString)
```

#### `OIDCToken`

``` purescript
newtype OIDCToken
  = OIDCToken String
```

#### `PaginationKey`

``` purescript
newtype PaginationKey
  = PaginationKey String
```

#### `QueryLimit`

``` purescript
newtype QueryLimit
  = QueryLimit Int
```

#### `ResourceConflictException`

``` purescript
newtype ResourceConflictException
  = ResourceConflictException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when a user tries to use a login which is already linked to another account.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when the requested resource (for example, a dataset or record) does not exist.</p>

#### `RoleMapping`

``` purescript
newtype RoleMapping
  = RoleMapping { "Type" :: RoleMappingType, "AmbiguousRoleResolution" :: NullOrUndefined (AmbiguousRoleResolutionType), "RulesConfiguration" :: NullOrUndefined (RulesConfigurationType) }
```

<p>A role mapping.</p>

#### `RoleMappingMap`

``` purescript
newtype RoleMappingMap
  = RoleMappingMap (Map IdentityProviderName RoleMapping)
```

#### `RoleMappingType`

``` purescript
newtype RoleMappingType
  = RoleMappingType String
```

#### `RoleType`

``` purescript
newtype RoleType
  = RoleType String
```

#### `RolesMap`

``` purescript
newtype RolesMap
  = RolesMap (Map RoleType ARNString)
```

#### `RulesConfigurationType`

``` purescript
newtype RulesConfigurationType
  = RulesConfigurationType { "Rules" :: MappingRulesList }
```

<p>A container for rules.</p>

#### `SAMLProviderList`

``` purescript
newtype SAMLProviderList
  = SAMLProviderList (Array ARNString)
```

#### `SecretKeyString`

``` purescript
newtype SecretKeyString
  = SecretKeyString String
```

#### `SessionTokenString`

``` purescript
newtype SessionTokenString
  = SessionTokenString String
```

#### `SetIdentityPoolRolesInput`

``` purescript
newtype SetIdentityPoolRolesInput
  = SetIdentityPoolRolesInput { "IdentityPoolId" :: IdentityPoolId, "Roles" :: RolesMap, "RoleMappings" :: NullOrUndefined (RoleMappingMap) }
```

<p>Input to the <code>SetIdentityPoolRoles</code> action.</p>

#### `TokenDuration`

``` purescript
newtype TokenDuration
  = TokenDuration Number
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message'" :: NullOrUndefined (String) }
```

<p>Thrown when a request is throttled.</p>

#### `UnlinkDeveloperIdentityInput`

``` purescript
newtype UnlinkDeveloperIdentityInput
  = UnlinkDeveloperIdentityInput { "IdentityId" :: IdentityId, "IdentityPoolId" :: IdentityPoolId, "DeveloperProviderName" :: DeveloperProviderName, "DeveloperUserIdentifier" :: DeveloperUserIdentifier }
```

<p>Input to the <code>UnlinkDeveloperIdentity</code> action.</p>

#### `UnlinkIdentityInput`

``` purescript
newtype UnlinkIdentityInput
  = UnlinkIdentityInput { "IdentityId" :: IdentityId, "Logins" :: LoginsMap, "LoginsToRemove" :: LoginsList }
```

<p>Input to the UnlinkIdentity action.</p>

#### `UnprocessedIdentityId`

``` purescript
newtype UnprocessedIdentityId
  = UnprocessedIdentityId { "IdentityId" :: NullOrUndefined (IdentityId), "ErrorCode" :: NullOrUndefined (ErrorCode) }
```

<p>An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.</p>

#### `UnprocessedIdentityIdList`

``` purescript
newtype UnprocessedIdentityIdList
  = UnprocessedIdentityIdList (Array UnprocessedIdentityId)
```


