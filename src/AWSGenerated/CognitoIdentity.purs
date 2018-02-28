

-- | <fullname>Amazon Cognito</fullname> <p>Amazon Cognito is a web service that delivers scoped temporary credentials to mobile devices and other untrusted environments. Amazon Cognito uniquely identifies a device and supplies the user with a consistent identity over the lifetime of an application.</p> <p>Using Amazon Cognito, you can enable authentication with one or more third-party identity providers (Facebook, Google, or Login with Amazon), and you can also choose to support unauthenticated access from your app. Cognito delivers a unique identifier for each user and acts as an OpenID token provider trusted by AWS Security Token Service (STS) to access temporary, limited-privilege AWS credentials.</p> <p>To provide end-user credentials, first make an unsigned call to <a>GetId</a>. If the end user is authenticated with one of the supported identity providers, set the <code>Logins</code> map with the identity provider token. <code>GetId</code> returns a unique identifier for the user.</p> <p>Next, make an unsigned call to <a>GetCredentialsForIdentity</a>. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. Assuming your identity pool has been configured via the <a>SetIdentityPoolRoles</a> operation, <code>GetCredentialsForIdentity</code> will return AWS credentials for your use. If your pool has not been configured with <code>SetIdentityPoolRoles</code>, or if you want to follow legacy flow, make an unsigned call to <a>GetOpenIdToken</a>, which returns the OpenID token necessary to call STS and retrieve AWS credentials. This call expects the same <code>Logins</code> map as the <code>GetId</code> call, as well as the <code>IdentityID</code> originally returned by <code>GetId</code>. The token returned by <code>GetOpenIdToken</code> can be passed to the STS operation <a href="http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html">AssumeRoleWithWebIdentity</a> to retrieve AWS credentials.</p> <p>If you want to use Amazon Cognito in an Android, iOS, or Unity application, you will probably want to make API calls via the AWS Mobile SDK. To learn more, see the <a href="http://docs.aws.amazon.com/mobile/index.html">AWS Mobile SDK Developer Guide</a>.</p>
module AWS.CognitoIdentity where

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

serviceName = "CognitoIdentity" :: String


-- | <p>Creates a new identity pool. The identity pool is a store of user identity information that is specific to your AWS account. The limit on identity pools is 60 per account. The keys for <code>SupportedLoginProviders</code> are as follows:</p> <ul> <li> <p>Facebook: <code>graph.facebook.com</code> </p> </li> <li> <p>Google: <code>accounts.google.com</code> </p> </li> <li> <p>Amazon: <code>www.amazon.com</code> </p> </li> <li> <p>Twitter: <code>api.twitter.com</code> </p> </li> <li> <p>Digits: <code>www.digits.com</code> </p> </li> </ul> <p>You must use AWS Developer credentials to call this API.</p>
createIdentityPool :: forall eff. CreateIdentityPoolInput -> Aff (exception :: EXCEPTION | eff) IdentityPool
createIdentityPool = Request.request serviceName "createIdentityPool" 


-- | <p>Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentities :: forall eff. DeleteIdentitiesInput -> Aff (exception :: EXCEPTION | eff) DeleteIdentitiesResponse
deleteIdentities = Request.request serviceName "deleteIdentities" 


-- | <p>Deletes a user pool. Once a pool is deleted, users will not be able to authenticate with the pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
deleteIdentityPool :: forall eff. DeleteIdentityPoolInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteIdentityPool = Request.request serviceName "deleteIdentityPool" 


-- | <p>Returns metadata related to the given identity, including when the identity was created and any associated linked logins.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentity :: forall eff. DescribeIdentityInput -> Aff (exception :: EXCEPTION | eff) IdentityDescription
describeIdentity = Request.request serviceName "describeIdentity" 


-- | <p>Gets details about a particular identity pool, including the pool name, ID description, creation date, and current number of users.</p> <p>You must use AWS Developer credentials to call this API.</p>
describeIdentityPool :: forall eff. DescribeIdentityPoolInput -> Aff (exception :: EXCEPTION | eff) IdentityPool
describeIdentityPool = Request.request serviceName "describeIdentityPool" 


-- | <p>Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getCredentialsForIdentity :: forall eff. GetCredentialsForIdentityInput -> Aff (exception :: EXCEPTION | eff) GetCredentialsForIdentityResponse
getCredentialsForIdentity = Request.request serviceName "getCredentialsForIdentity" 


-- | <p>Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getId :: forall eff. GetIdInput -> Aff (exception :: EXCEPTION | eff) GetIdResponse
getId = Request.request serviceName "getId" 


-- | <p>Gets the roles for an identity pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
getIdentityPoolRoles :: forall eff. GetIdentityPoolRolesInput -> Aff (exception :: EXCEPTION | eff) GetIdentityPoolRolesResponse
getIdentityPoolRoles = Request.request serviceName "getIdentityPoolRoles" 


-- | <p>Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by <a>GetId</a>. You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.</p> <p>The OpenId token is valid for 15 minutes.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
getOpenIdToken :: forall eff. GetOpenIdTokenInput -> Aff (exception :: EXCEPTION | eff) GetOpenIdTokenResponse
getOpenIdToken = Request.request serviceName "getOpenIdToken" 


-- | <p>Registers (or retrieves) a Cognito <code>IdentityId</code> and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the <code>Logins</code> map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.</p> <p>You can use <code>GetOpenIdTokenForDeveloperIdentity</code> to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the <code>IdentityId</code> should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing <code>IdentityId</code>. This API will create the identity in the specified <code>IdentityPoolId</code>.</p> <p>You must use AWS Developer credentials to call this API.</p>
getOpenIdTokenForDeveloperIdentity :: forall eff. GetOpenIdTokenForDeveloperIdentityInput -> Aff (exception :: EXCEPTION | eff) GetOpenIdTokenForDeveloperIdentityResponse
getOpenIdTokenForDeveloperIdentity = Request.request serviceName "getOpenIdTokenForDeveloperIdentity" 


-- | <p>Lists the identities in a pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentities :: forall eff. ListIdentitiesInput -> Aff (exception :: EXCEPTION | eff) ListIdentitiesResponse
listIdentities = Request.request serviceName "listIdentities" 


-- | <p>Lists all of the Cognito identity pools registered for your account.</p> <p>You must use AWS Developer credentials to call this API.</p>
listIdentityPools :: forall eff. ListIdentityPoolsInput -> Aff (exception :: EXCEPTION | eff) ListIdentityPoolsResponse
listIdentityPools = Request.request serviceName "listIdentityPools" 


-- | <p>Retrieves the <code>IdentityID</code> associated with a <code>DeveloperUserIdentifier</code> or the list of <code>DeveloperUserIdentifier</code>s associated with an <code>IdentityId</code> for an existing identity. Either <code>IdentityID</code> or <code>DeveloperUserIdentifier</code> must not be null. If you supply only one of these values, the other value will be searched in the database and returned as a part of the response. If you supply both, <code>DeveloperUserIdentifier</code> will be matched against <code>IdentityID</code>. If the values are verified against the database, the response returns both values and is the same as the request. Otherwise a <code>ResourceConflictException</code> is thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
lookupDeveloperIdentity :: forall eff. LookupDeveloperIdentityInput -> Aff (exception :: EXCEPTION | eff) LookupDeveloperIdentityResponse
lookupDeveloperIdentity = Request.request serviceName "lookupDeveloperIdentity" 


-- | <p>Merges two users having different <code>IdentityId</code>s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (<code>SourceUserIdentifier</code>) with the <code>IdentityId</code> of the <code>DestinationUserIdentifier</code>. Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.</p> <p>You must use AWS Developer credentials to call this API.</p>
mergeDeveloperIdentities :: forall eff. MergeDeveloperIdentitiesInput -> Aff (exception :: EXCEPTION | eff) MergeDeveloperIdentitiesResponse
mergeDeveloperIdentities = Request.request serviceName "mergeDeveloperIdentities" 


-- | <p>Sets the roles for an identity pool. These roles are used when making calls to <a>GetCredentialsForIdentity</a> action.</p> <p>You must use AWS Developer credentials to call this API.</p>
setIdentityPoolRoles :: forall eff. SetIdentityPoolRolesInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setIdentityPoolRoles = Request.request serviceName "setIdentityPoolRoles" 


-- | <p>Unlinks a <code>DeveloperUserIdentifier</code> from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.</p> <p>You must use AWS Developer credentials to call this API.</p>
unlinkDeveloperIdentity :: forall eff. UnlinkDeveloperIdentityInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
unlinkDeveloperIdentity = Request.request serviceName "unlinkDeveloperIdentity" 


-- | <p>Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.</p> <p>This is a public API. You do not need any credentials to call this API.</p>
unlinkIdentity :: forall eff. UnlinkIdentityInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
unlinkIdentity = Request.request serviceName "unlinkIdentity" 


-- | <p>Updates a user pool.</p> <p>You must use AWS Developer credentials to call this API.</p>
updateIdentityPool :: forall eff. IdentityPool -> Aff (exception :: EXCEPTION | eff) IdentityPool
updateIdentityPool = Request.request serviceName "updateIdentityPool" 


newtype ARNString = ARNString String
derive instance newtypeARNString :: Newtype ARNString _
derive instance repGenericARNString :: Generic ARNString _
instance showARNString :: Show ARNString where
  show = genericShow
instance decodeARNString :: Decode ARNString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeARNString :: Encode ARNString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessKeyString = AccessKeyString String
derive instance newtypeAccessKeyString :: Newtype AccessKeyString _
derive instance repGenericAccessKeyString :: Generic AccessKeyString _
instance showAccessKeyString :: Show AccessKeyString where
  show = genericShow
instance decodeAccessKeyString :: Decode AccessKeyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessKeyString :: Encode AccessKeyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AmbiguousRoleResolutionType = AmbiguousRoleResolutionType String
derive instance newtypeAmbiguousRoleResolutionType :: Newtype AmbiguousRoleResolutionType _
derive instance repGenericAmbiguousRoleResolutionType :: Generic AmbiguousRoleResolutionType _
instance showAmbiguousRoleResolutionType :: Show AmbiguousRoleResolutionType where
  show = genericShow
instance decodeAmbiguousRoleResolutionType :: Decode AmbiguousRoleResolutionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmbiguousRoleResolutionType :: Encode AmbiguousRoleResolutionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClaimName = ClaimName String
derive instance newtypeClaimName :: Newtype ClaimName _
derive instance repGenericClaimName :: Generic ClaimName _
instance showClaimName :: Show ClaimName where
  show = genericShow
instance decodeClaimName :: Decode ClaimName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClaimName :: Encode ClaimName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClaimValue = ClaimValue String
derive instance newtypeClaimValue :: Newtype ClaimValue _
derive instance repGenericClaimValue :: Generic ClaimValue _
instance showClaimValue :: Show ClaimValue where
  show = genericShow
instance decodeClaimValue :: Decode ClaimValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClaimValue :: Encode ClaimValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A provider representing an Amazon Cognito Identity User Pool and its client ID.</p>
newtype CognitoIdentityProvider = CognitoIdentityProvider 
  { "ProviderName" :: NullOrUndefined.NullOrUndefined (CognitoIdentityProviderName)
  , "ClientId" :: NullOrUndefined.NullOrUndefined (CognitoIdentityProviderClientId)
  , "ServerSideTokenCheck" :: NullOrUndefined.NullOrUndefined (CognitoIdentityProviderTokenCheck)
  }
derive instance newtypeCognitoIdentityProvider :: Newtype CognitoIdentityProvider _
derive instance repGenericCognitoIdentityProvider :: Generic CognitoIdentityProvider _
instance showCognitoIdentityProvider :: Show CognitoIdentityProvider where
  show = genericShow
instance decodeCognitoIdentityProvider :: Decode CognitoIdentityProvider where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityProvider :: Encode CognitoIdentityProvider where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoIdentityProviderClientId = CognitoIdentityProviderClientId String
derive instance newtypeCognitoIdentityProviderClientId :: Newtype CognitoIdentityProviderClientId _
derive instance repGenericCognitoIdentityProviderClientId :: Generic CognitoIdentityProviderClientId _
instance showCognitoIdentityProviderClientId :: Show CognitoIdentityProviderClientId where
  show = genericShow
instance decodeCognitoIdentityProviderClientId :: Decode CognitoIdentityProviderClientId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityProviderClientId :: Encode CognitoIdentityProviderClientId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoIdentityProviderList = CognitoIdentityProviderList (Array CognitoIdentityProvider)
derive instance newtypeCognitoIdentityProviderList :: Newtype CognitoIdentityProviderList _
derive instance repGenericCognitoIdentityProviderList :: Generic CognitoIdentityProviderList _
instance showCognitoIdentityProviderList :: Show CognitoIdentityProviderList where
  show = genericShow
instance decodeCognitoIdentityProviderList :: Decode CognitoIdentityProviderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityProviderList :: Encode CognitoIdentityProviderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoIdentityProviderName = CognitoIdentityProviderName String
derive instance newtypeCognitoIdentityProviderName :: Newtype CognitoIdentityProviderName _
derive instance repGenericCognitoIdentityProviderName :: Generic CognitoIdentityProviderName _
instance showCognitoIdentityProviderName :: Show CognitoIdentityProviderName where
  show = genericShow
instance decodeCognitoIdentityProviderName :: Decode CognitoIdentityProviderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityProviderName :: Encode CognitoIdentityProviderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CognitoIdentityProviderTokenCheck = CognitoIdentityProviderTokenCheck Boolean
derive instance newtypeCognitoIdentityProviderTokenCheck :: Newtype CognitoIdentityProviderTokenCheck _
derive instance repGenericCognitoIdentityProviderTokenCheck :: Generic CognitoIdentityProviderTokenCheck _
instance showCognitoIdentityProviderTokenCheck :: Show CognitoIdentityProviderTokenCheck where
  show = genericShow
instance decodeCognitoIdentityProviderTokenCheck :: Decode CognitoIdentityProviderTokenCheck where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCognitoIdentityProviderTokenCheck :: Encode CognitoIdentityProviderTokenCheck where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown if there are parallel requests to modify a resource.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the CreateIdentityPool action.</p>
newtype CreateIdentityPoolInput = CreateIdentityPoolInput 
  { "IdentityPoolName" :: (IdentityPoolName)
  , "AllowUnauthenticatedIdentities" :: (IdentityPoolUnauthenticated)
  , "SupportedLoginProviders" :: NullOrUndefined.NullOrUndefined (IdentityProviders)
  , "DeveloperProviderName" :: NullOrUndefined.NullOrUndefined (DeveloperProviderName)
  , "OpenIdConnectProviderARNs" :: NullOrUndefined.NullOrUndefined (OIDCProviderList)
  , "CognitoIdentityProviders" :: NullOrUndefined.NullOrUndefined (CognitoIdentityProviderList)
  , "SamlProviderARNs" :: NullOrUndefined.NullOrUndefined (SAMLProviderList)
  }
derive instance newtypeCreateIdentityPoolInput :: Newtype CreateIdentityPoolInput _
derive instance repGenericCreateIdentityPoolInput :: Generic CreateIdentityPoolInput _
instance showCreateIdentityPoolInput :: Show CreateIdentityPoolInput where
  show = genericShow
instance decodeCreateIdentityPoolInput :: Decode CreateIdentityPoolInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIdentityPoolInput :: Encode CreateIdentityPoolInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Credentials for the provided identity ID.</p>
newtype Credentials = Credentials 
  { "AccessKeyId" :: NullOrUndefined.NullOrUndefined (AccessKeyString)
  , "SecretKey" :: NullOrUndefined.NullOrUndefined (SecretKeyString)
  , "SessionToken" :: NullOrUndefined.NullOrUndefined (SessionTokenString)
  , "Expiration" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeCredentials :: Newtype Credentials _
derive instance repGenericCredentials :: Generic Credentials _
instance showCredentials :: Show Credentials where
  show = genericShow
instance decodeCredentials :: Decode Credentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCredentials :: Encode Credentials where
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


-- | <p>Input to the <code>DeleteIdentities</code> action.</p>
newtype DeleteIdentitiesInput = DeleteIdentitiesInput 
  { "IdentityIdsToDelete" :: (IdentityIdList)
  }
derive instance newtypeDeleteIdentitiesInput :: Newtype DeleteIdentitiesInput _
derive instance repGenericDeleteIdentitiesInput :: Generic DeleteIdentitiesInput _
instance showDeleteIdentitiesInput :: Show DeleteIdentitiesInput where
  show = genericShow
instance decodeDeleteIdentitiesInput :: Decode DeleteIdentitiesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentitiesInput :: Encode DeleteIdentitiesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>DeleteIdentities</code> operation.</p>
newtype DeleteIdentitiesResponse = DeleteIdentitiesResponse 
  { "UnprocessedIdentityIds" :: NullOrUndefined.NullOrUndefined (UnprocessedIdentityIdList)
  }
derive instance newtypeDeleteIdentitiesResponse :: Newtype DeleteIdentitiesResponse _
derive instance repGenericDeleteIdentitiesResponse :: Generic DeleteIdentitiesResponse _
instance showDeleteIdentitiesResponse :: Show DeleteIdentitiesResponse where
  show = genericShow
instance decodeDeleteIdentitiesResponse :: Decode DeleteIdentitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentitiesResponse :: Encode DeleteIdentitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the DeleteIdentityPool action.</p>
newtype DeleteIdentityPoolInput = DeleteIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeDeleteIdentityPoolInput :: Newtype DeleteIdentityPoolInput _
derive instance repGenericDeleteIdentityPoolInput :: Generic DeleteIdentityPoolInput _
instance showDeleteIdentityPoolInput :: Show DeleteIdentityPoolInput where
  show = genericShow
instance decodeDeleteIdentityPoolInput :: Decode DeleteIdentityPoolInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIdentityPoolInput :: Encode DeleteIdentityPoolInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>DescribeIdentity</code> action.</p>
newtype DescribeIdentityInput = DescribeIdentityInput 
  { "IdentityId" :: (IdentityId)
  }
derive instance newtypeDescribeIdentityInput :: Newtype DescribeIdentityInput _
derive instance repGenericDescribeIdentityInput :: Generic DescribeIdentityInput _
instance showDescribeIdentityInput :: Show DescribeIdentityInput where
  show = genericShow
instance decodeDescribeIdentityInput :: Decode DescribeIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityInput :: Encode DescribeIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the DescribeIdentityPool action.</p>
newtype DescribeIdentityPoolInput = DescribeIdentityPoolInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeDescribeIdentityPoolInput :: Newtype DescribeIdentityPoolInput _
derive instance repGenericDescribeIdentityPoolInput :: Generic DescribeIdentityPoolInput _
instance showDescribeIdentityPoolInput :: Show DescribeIdentityPoolInput where
  show = genericShow
instance decodeDescribeIdentityPoolInput :: Decode DescribeIdentityPoolInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeIdentityPoolInput :: Encode DescribeIdentityPoolInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeveloperProviderName = DeveloperProviderName String
derive instance newtypeDeveloperProviderName :: Newtype DeveloperProviderName _
derive instance repGenericDeveloperProviderName :: Generic DeveloperProviderName _
instance showDeveloperProviderName :: Show DeveloperProviderName where
  show = genericShow
instance decodeDeveloperProviderName :: Decode DeveloperProviderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeveloperProviderName :: Encode DeveloperProviderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided developer user identifier is already registered with Cognito under a different identity ID.</p>
newtype DeveloperUserAlreadyRegisteredException = DeveloperUserAlreadyRegisteredException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeveloperUserAlreadyRegisteredException :: Newtype DeveloperUserAlreadyRegisteredException _
derive instance repGenericDeveloperUserAlreadyRegisteredException :: Generic DeveloperUserAlreadyRegisteredException _
instance showDeveloperUserAlreadyRegisteredException :: Show DeveloperUserAlreadyRegisteredException where
  show = genericShow
instance decodeDeveloperUserAlreadyRegisteredException :: Decode DeveloperUserAlreadyRegisteredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeveloperUserAlreadyRegisteredException :: Encode DeveloperUserAlreadyRegisteredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeveloperUserIdentifier = DeveloperUserIdentifier String
derive instance newtypeDeveloperUserIdentifier :: Newtype DeveloperUserIdentifier _
derive instance repGenericDeveloperUserIdentifier :: Generic DeveloperUserIdentifier _
instance showDeveloperUserIdentifier :: Show DeveloperUserIdentifier where
  show = genericShow
instance decodeDeveloperUserIdentifier :: Decode DeveloperUserIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeveloperUserIdentifier :: Encode DeveloperUserIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeveloperUserIdentifierList = DeveloperUserIdentifierList (Array DeveloperUserIdentifier)
derive instance newtypeDeveloperUserIdentifierList :: Newtype DeveloperUserIdentifierList _
derive instance repGenericDeveloperUserIdentifierList :: Generic DeveloperUserIdentifierList _
instance showDeveloperUserIdentifierList :: Show DeveloperUserIdentifierList where
  show = genericShow
instance decodeDeveloperUserIdentifierList :: Decode DeveloperUserIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeveloperUserIdentifierList :: Encode DeveloperUserIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception thrown when a dependent service such as Facebook or Twitter is not responding</p>
newtype ExternalServiceException = ExternalServiceException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeExternalServiceException :: Newtype ExternalServiceException _
derive instance repGenericExternalServiceException :: Generic ExternalServiceException _
instance showExternalServiceException :: Show ExternalServiceException where
  show = genericShow
instance decodeExternalServiceException :: Decode ExternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalServiceException :: Encode ExternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>GetCredentialsForIdentity</code> action.</p>
newtype GetCredentialsForIdentityInput = GetCredentialsForIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined.NullOrUndefined (LoginsMap)
  , "CustomRoleArn" :: NullOrUndefined.NullOrUndefined (ARNString)
  }
derive instance newtypeGetCredentialsForIdentityInput :: Newtype GetCredentialsForIdentityInput _
derive instance repGenericGetCredentialsForIdentityInput :: Generic GetCredentialsForIdentityInput _
instance showGetCredentialsForIdentityInput :: Show GetCredentialsForIdentityInput where
  show = genericShow
instance decodeGetCredentialsForIdentityInput :: Decode GetCredentialsForIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCredentialsForIdentityInput :: Encode GetCredentialsForIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>GetCredentialsForIdentity</code> operation.</p>
newtype GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "Credentials" :: NullOrUndefined.NullOrUndefined (Credentials)
  }
derive instance newtypeGetCredentialsForIdentityResponse :: Newtype GetCredentialsForIdentityResponse _
derive instance repGenericGetCredentialsForIdentityResponse :: Generic GetCredentialsForIdentityResponse _
instance showGetCredentialsForIdentityResponse :: Show GetCredentialsForIdentityResponse where
  show = genericShow
instance decodeGetCredentialsForIdentityResponse :: Decode GetCredentialsForIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCredentialsForIdentityResponse :: Encode GetCredentialsForIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the GetId action.</p>
newtype GetIdInput = GetIdInput 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "Logins" :: NullOrUndefined.NullOrUndefined (LoginsMap)
  }
derive instance newtypeGetIdInput :: Newtype GetIdInput _
derive instance repGenericGetIdInput :: Generic GetIdInput _
instance showGetIdInput :: Show GetIdInput where
  show = genericShow
instance decodeGetIdInput :: Decode GetIdInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdInput :: Encode GetIdInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a GetId request.</p>
newtype GetIdResponse = GetIdResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  }
derive instance newtypeGetIdResponse :: Newtype GetIdResponse _
derive instance repGenericGetIdResponse :: Generic GetIdResponse _
instance showGetIdResponse :: Show GetIdResponse where
  show = genericShow
instance decodeGetIdResponse :: Decode GetIdResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdResponse :: Encode GetIdResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>GetIdentityPoolRoles</code> action.</p>
newtype GetIdentityPoolRolesInput = GetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeGetIdentityPoolRolesInput :: Newtype GetIdentityPoolRolesInput _
derive instance repGenericGetIdentityPoolRolesInput :: Generic GetIdentityPoolRolesInput _
instance showGetIdentityPoolRolesInput :: Show GetIdentityPoolRolesInput where
  show = genericShow
instance decodeGetIdentityPoolRolesInput :: Decode GetIdentityPoolRolesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoolRolesInput :: Encode GetIdentityPoolRolesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>GetIdentityPoolRoles</code> operation.</p>
newtype GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "Roles" :: NullOrUndefined.NullOrUndefined (RolesMap)
  , "RoleMappings" :: NullOrUndefined.NullOrUndefined (RoleMappingMap)
  }
derive instance newtypeGetIdentityPoolRolesResponse :: Newtype GetIdentityPoolRolesResponse _
derive instance repGenericGetIdentityPoolRolesResponse :: Generic GetIdentityPoolRolesResponse _
instance showGetIdentityPoolRolesResponse :: Show GetIdentityPoolRolesResponse where
  show = genericShow
instance decodeGetIdentityPoolRolesResponse :: Decode GetIdentityPoolRolesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIdentityPoolRolesResponse :: Encode GetIdentityPoolRolesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>GetOpenIdTokenForDeveloperIdentity</code> action.</p>
newtype GetOpenIdTokenForDeveloperIdentityInput = GetOpenIdTokenForDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "Logins" :: (LoginsMap)
  , "TokenDuration" :: NullOrUndefined.NullOrUndefined (TokenDuration)
  }
derive instance newtypeGetOpenIdTokenForDeveloperIdentityInput :: Newtype GetOpenIdTokenForDeveloperIdentityInput _
derive instance repGenericGetOpenIdTokenForDeveloperIdentityInput :: Generic GetOpenIdTokenForDeveloperIdentityInput _
instance showGetOpenIdTokenForDeveloperIdentityInput :: Show GetOpenIdTokenForDeveloperIdentityInput where
  show = genericShow
instance decodeGetOpenIdTokenForDeveloperIdentityInput :: Decode GetOpenIdTokenForDeveloperIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOpenIdTokenForDeveloperIdentityInput :: Encode GetOpenIdTokenForDeveloperIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>GetOpenIdTokenForDeveloperIdentity</code> request.</p>
newtype GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined.NullOrUndefined (OIDCToken)
  }
derive instance newtypeGetOpenIdTokenForDeveloperIdentityResponse :: Newtype GetOpenIdTokenForDeveloperIdentityResponse _
derive instance repGenericGetOpenIdTokenForDeveloperIdentityResponse :: Generic GetOpenIdTokenForDeveloperIdentityResponse _
instance showGetOpenIdTokenForDeveloperIdentityResponse :: Show GetOpenIdTokenForDeveloperIdentityResponse where
  show = genericShow
instance decodeGetOpenIdTokenForDeveloperIdentityResponse :: Decode GetOpenIdTokenForDeveloperIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOpenIdTokenForDeveloperIdentityResponse :: Encode GetOpenIdTokenForDeveloperIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the GetOpenIdToken action.</p>
newtype GetOpenIdTokenInput = GetOpenIdTokenInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: NullOrUndefined.NullOrUndefined (LoginsMap)
  }
derive instance newtypeGetOpenIdTokenInput :: Newtype GetOpenIdTokenInput _
derive instance repGenericGetOpenIdTokenInput :: Generic GetOpenIdTokenInput _
instance showGetOpenIdTokenInput :: Show GetOpenIdTokenInput where
  show = genericShow
instance decodeGetOpenIdTokenInput :: Decode GetOpenIdTokenInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOpenIdTokenInput :: Encode GetOpenIdTokenInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful GetOpenIdToken request.</p>
newtype GetOpenIdTokenResponse = GetOpenIdTokenResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "Token" :: NullOrUndefined.NullOrUndefined (OIDCToken)
  }
derive instance newtypeGetOpenIdTokenResponse :: Newtype GetOpenIdTokenResponse _
derive instance repGenericGetOpenIdTokenResponse :: Generic GetOpenIdTokenResponse _
instance showGetOpenIdTokenResponse :: Show GetOpenIdTokenResponse where
  show = genericShow
instance decodeGetOpenIdTokenResponse :: Decode GetOpenIdTokenResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetOpenIdTokenResponse :: Encode GetOpenIdTokenResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HideDisabled = HideDisabled Boolean
derive instance newtypeHideDisabled :: Newtype HideDisabled _
derive instance repGenericHideDisabled :: Generic HideDisabled _
instance showHideDisabled :: Show HideDisabled where
  show = genericShow
instance decodeHideDisabled :: Decode HideDisabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHideDisabled :: Encode HideDisabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentitiesList = IdentitiesList (Array IdentityDescription)
derive instance newtypeIdentitiesList :: Newtype IdentitiesList _
derive instance repGenericIdentitiesList :: Generic IdentitiesList _
instance showIdentitiesList :: Show IdentitiesList where
  show = genericShow
instance decodeIdentitiesList :: Decode IdentitiesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentitiesList :: Encode IdentitiesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of the identity.</p>
newtype IdentityDescription = IdentityDescription 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "Logins" :: NullOrUndefined.NullOrUndefined (LoginsList)
  , "CreationDate" :: NullOrUndefined.NullOrUndefined (DateType)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateType)
  }
derive instance newtypeIdentityDescription :: Newtype IdentityDescription _
derive instance repGenericIdentityDescription :: Generic IdentityDescription _
instance showIdentityDescription :: Show IdentityDescription where
  show = genericShow
instance decodeIdentityDescription :: Decode IdentityDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityDescription :: Encode IdentityDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityId = IdentityId String
derive instance newtypeIdentityId :: Newtype IdentityId _
derive instance repGenericIdentityId :: Generic IdentityId _
instance showIdentityId :: Show IdentityId where
  show = genericShow
instance decodeIdentityId :: Decode IdentityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityId :: Encode IdentityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityIdList = IdentityIdList (Array IdentityId)
derive instance newtypeIdentityIdList :: Newtype IdentityIdList _
derive instance repGenericIdentityIdList :: Generic IdentityIdList _
instance showIdentityIdList :: Show IdentityIdList where
  show = genericShow
instance decodeIdentityIdList :: Decode IdentityIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityIdList :: Encode IdentityIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an Amazon Cognito identity pool.</p>
newtype IdentityPool = IdentityPool 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityPoolName" :: (IdentityPoolName)
  , "AllowUnauthenticatedIdentities" :: (IdentityPoolUnauthenticated)
  , "SupportedLoginProviders" :: NullOrUndefined.NullOrUndefined (IdentityProviders)
  , "DeveloperProviderName" :: NullOrUndefined.NullOrUndefined (DeveloperProviderName)
  , "OpenIdConnectProviderARNs" :: NullOrUndefined.NullOrUndefined (OIDCProviderList)
  , "CognitoIdentityProviders" :: NullOrUndefined.NullOrUndefined (CognitoIdentityProviderList)
  , "SamlProviderARNs" :: NullOrUndefined.NullOrUndefined (SAMLProviderList)
  }
derive instance newtypeIdentityPool :: Newtype IdentityPool _
derive instance repGenericIdentityPool :: Generic IdentityPool _
instance showIdentityPool :: Show IdentityPool where
  show = genericShow
instance decodeIdentityPool :: Decode IdentityPool where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPool :: Encode IdentityPool where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolId = IdentityPoolId String
derive instance newtypeIdentityPoolId :: Newtype IdentityPoolId _
derive instance repGenericIdentityPoolId :: Generic IdentityPoolId _
instance showIdentityPoolId :: Show IdentityPoolId where
  show = genericShow
instance decodeIdentityPoolId :: Decode IdentityPoolId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolId :: Encode IdentityPoolId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolName = IdentityPoolName String
derive instance newtypeIdentityPoolName :: Newtype IdentityPoolName _
derive instance repGenericIdentityPoolName :: Generic IdentityPoolName _
instance showIdentityPoolName :: Show IdentityPoolName where
  show = genericShow
instance decodeIdentityPoolName :: Decode IdentityPoolName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolName :: Encode IdentityPoolName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of the identity pool.</p>
newtype IdentityPoolShortDescription = IdentityPoolShortDescription 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "IdentityPoolName" :: NullOrUndefined.NullOrUndefined (IdentityPoolName)
  }
derive instance newtypeIdentityPoolShortDescription :: Newtype IdentityPoolShortDescription _
derive instance repGenericIdentityPoolShortDescription :: Generic IdentityPoolShortDescription _
instance showIdentityPoolShortDescription :: Show IdentityPoolShortDescription where
  show = genericShow
instance decodeIdentityPoolShortDescription :: Decode IdentityPoolShortDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolShortDescription :: Encode IdentityPoolShortDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolUnauthenticated = IdentityPoolUnauthenticated Boolean
derive instance newtypeIdentityPoolUnauthenticated :: Newtype IdentityPoolUnauthenticated _
derive instance repGenericIdentityPoolUnauthenticated :: Generic IdentityPoolUnauthenticated _
instance showIdentityPoolUnauthenticated :: Show IdentityPoolUnauthenticated where
  show = genericShow
instance decodeIdentityPoolUnauthenticated :: Decode IdentityPoolUnauthenticated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolUnauthenticated :: Encode IdentityPoolUnauthenticated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityPoolsList = IdentityPoolsList (Array IdentityPoolShortDescription)
derive instance newtypeIdentityPoolsList :: Newtype IdentityPoolsList _
derive instance repGenericIdentityPoolsList :: Generic IdentityPoolsList _
instance showIdentityPoolsList :: Show IdentityPoolsList where
  show = genericShow
instance decodeIdentityPoolsList :: Decode IdentityPoolsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityPoolsList :: Encode IdentityPoolsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityProviderId = IdentityProviderId String
derive instance newtypeIdentityProviderId :: Newtype IdentityProviderId _
derive instance repGenericIdentityProviderId :: Generic IdentityProviderId _
instance showIdentityProviderId :: Show IdentityProviderId where
  show = genericShow
instance decodeIdentityProviderId :: Decode IdentityProviderId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviderId :: Encode IdentityProviderId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityProviderName = IdentityProviderName String
derive instance newtypeIdentityProviderName :: Newtype IdentityProviderName _
derive instance repGenericIdentityProviderName :: Generic IdentityProviderName _
instance showIdentityProviderName :: Show IdentityProviderName where
  show = genericShow
instance decodeIdentityProviderName :: Decode IdentityProviderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviderName :: Encode IdentityProviderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityProviderToken = IdentityProviderToken String
derive instance newtypeIdentityProviderToken :: Newtype IdentityProviderToken _
derive instance repGenericIdentityProviderToken :: Generic IdentityProviderToken _
instance showIdentityProviderToken :: Show IdentityProviderToken where
  show = genericShow
instance decodeIdentityProviderToken :: Decode IdentityProviderToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviderToken :: Encode IdentityProviderToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdentityProviders = IdentityProviders (StrMap.StrMap IdentityProviderId)
derive instance newtypeIdentityProviders :: Newtype IdentityProviders _
derive instance repGenericIdentityProviders :: Generic IdentityProviders _
instance showIdentityProviders :: Show IdentityProviders where
  show = genericShow
instance decodeIdentityProviders :: Decode IdentityProviders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentityProviders :: Encode IdentityProviders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when the service encounters an error during processing the request.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _
derive instance repGenericInternalErrorException :: Generic InternalErrorException _
instance showInternalErrorException :: Show InternalErrorException where
  show = genericShow
instance decodeInternalErrorException :: Decode InternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalErrorException :: Encode InternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown if the identity pool has no role associated for the given auth type (auth/unauth) or if the AssumeRole fails.</p>
newtype InvalidIdentityPoolConfigurationException = InvalidIdentityPoolConfigurationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidIdentityPoolConfigurationException :: Newtype InvalidIdentityPoolConfigurationException _
derive instance repGenericInvalidIdentityPoolConfigurationException :: Generic InvalidIdentityPoolConfigurationException _
instance showInvalidIdentityPoolConfigurationException :: Show InvalidIdentityPoolConfigurationException where
  show = genericShow
instance decodeInvalidIdentityPoolConfigurationException :: Decode InvalidIdentityPoolConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidIdentityPoolConfigurationException :: Encode InvalidIdentityPoolConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown for missing or bad input parameter(s).</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when the total number of user pools has exceeded a preset limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the ListIdentities action.</p>
newtype ListIdentitiesInput = ListIdentitiesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  , "HideDisabled" :: NullOrUndefined.NullOrUndefined (HideDisabled)
  }
derive instance newtypeListIdentitiesInput :: Newtype ListIdentitiesInput _
derive instance repGenericListIdentitiesInput :: Generic ListIdentitiesInput _
instance showListIdentitiesInput :: Show ListIdentitiesInput where
  show = genericShow
instance decodeListIdentitiesInput :: Decode ListIdentitiesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentitiesInput :: Encode ListIdentitiesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response to a ListIdentities request.</p>
newtype ListIdentitiesResponse = ListIdentitiesResponse 
  { "IdentityPoolId" :: NullOrUndefined.NullOrUndefined (IdentityPoolId)
  , "Identities" :: NullOrUndefined.NullOrUndefined (IdentitiesList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentitiesResponse :: Newtype ListIdentitiesResponse _
derive instance repGenericListIdentitiesResponse :: Generic ListIdentitiesResponse _
instance showListIdentitiesResponse :: Show ListIdentitiesResponse where
  show = genericShow
instance decodeListIdentitiesResponse :: Decode ListIdentitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentitiesResponse :: Encode ListIdentitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the ListIdentityPools action.</p>
newtype ListIdentityPoolsInput = ListIdentityPoolsInput 
  { "MaxResults" :: (QueryLimit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentityPoolsInput :: Newtype ListIdentityPoolsInput _
derive instance repGenericListIdentityPoolsInput :: Generic ListIdentityPoolsInput _
instance showListIdentityPoolsInput :: Show ListIdentityPoolsInput where
  show = genericShow
instance decodeListIdentityPoolsInput :: Decode ListIdentityPoolsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoolsInput :: Encode ListIdentityPoolsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a successful ListIdentityPools action.</p>
newtype ListIdentityPoolsResponse = ListIdentityPoolsResponse 
  { "IdentityPools" :: NullOrUndefined.NullOrUndefined (IdentityPoolsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeListIdentityPoolsResponse :: Newtype ListIdentityPoolsResponse _
derive instance repGenericListIdentityPoolsResponse :: Generic ListIdentityPoolsResponse _
instance showListIdentityPoolsResponse :: Show ListIdentityPoolsResponse where
  show = genericShow
instance decodeListIdentityPoolsResponse :: Decode ListIdentityPoolsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIdentityPoolsResponse :: Encode ListIdentityPoolsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoginsList = LoginsList (Array IdentityProviderName)
derive instance newtypeLoginsList :: Newtype LoginsList _
derive instance repGenericLoginsList :: Generic LoginsList _
instance showLoginsList :: Show LoginsList where
  show = genericShow
instance decodeLoginsList :: Decode LoginsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoginsList :: Encode LoginsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoginsMap = LoginsMap (StrMap.StrMap IdentityProviderToken)
derive instance newtypeLoginsMap :: Newtype LoginsMap _
derive instance repGenericLoginsMap :: Generic LoginsMap _
instance showLoginsMap :: Show LoginsMap where
  show = genericShow
instance decodeLoginsMap :: Decode LoginsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoginsMap :: Encode LoginsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>LookupDeveloperIdentityInput</code> action.</p>
newtype LookupDeveloperIdentityInput = LookupDeveloperIdentityInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifier" :: NullOrUndefined.NullOrUndefined (DeveloperUserIdentifier)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (QueryLimit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeLookupDeveloperIdentityInput :: Newtype LookupDeveloperIdentityInput _
derive instance repGenericLookupDeveloperIdentityInput :: Generic LookupDeveloperIdentityInput _
instance showLookupDeveloperIdentityInput :: Show LookupDeveloperIdentityInput where
  show = genericShow
instance decodeLookupDeveloperIdentityInput :: Decode LookupDeveloperIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupDeveloperIdentityInput :: Encode LookupDeveloperIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>LookupDeveloperIdentity</code> action.</p>
newtype LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "DeveloperUserIdentifierList" :: NullOrUndefined.NullOrUndefined (DeveloperUserIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationKey)
  }
derive instance newtypeLookupDeveloperIdentityResponse :: Newtype LookupDeveloperIdentityResponse _
derive instance repGenericLookupDeveloperIdentityResponse :: Generic LookupDeveloperIdentityResponse _
instance showLookupDeveloperIdentityResponse :: Show LookupDeveloperIdentityResponse where
  show = genericShow
instance decodeLookupDeveloperIdentityResponse :: Decode LookupDeveloperIdentityResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupDeveloperIdentityResponse :: Encode LookupDeveloperIdentityResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A rule that maps a claim name, a claim value, and a match type to a role ARN.</p>
newtype MappingRule = MappingRule 
  { "Claim" :: (ClaimName)
  , "MatchType" :: (MappingRuleMatchType)
  , "Value" :: (ClaimValue)
  , "RoleARN" :: (ARNString)
  }
derive instance newtypeMappingRule :: Newtype MappingRule _
derive instance repGenericMappingRule :: Generic MappingRule _
instance showMappingRule :: Show MappingRule where
  show = genericShow
instance decodeMappingRule :: Decode MappingRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingRule :: Encode MappingRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MappingRuleMatchType = MappingRuleMatchType String
derive instance newtypeMappingRuleMatchType :: Newtype MappingRuleMatchType _
derive instance repGenericMappingRuleMatchType :: Generic MappingRuleMatchType _
instance showMappingRuleMatchType :: Show MappingRuleMatchType where
  show = genericShow
instance decodeMappingRuleMatchType :: Decode MappingRuleMatchType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingRuleMatchType :: Encode MappingRuleMatchType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MappingRulesList = MappingRulesList (Array MappingRule)
derive instance newtypeMappingRulesList :: Newtype MappingRulesList _
derive instance repGenericMappingRulesList :: Generic MappingRulesList _
instance showMappingRulesList :: Show MappingRulesList where
  show = genericShow
instance decodeMappingRulesList :: Decode MappingRulesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingRulesList :: Encode MappingRulesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesInput = MergeDeveloperIdentitiesInput 
  { "SourceUserIdentifier" :: (DeveloperUserIdentifier)
  , "DestinationUserIdentifier" :: (DeveloperUserIdentifier)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "IdentityPoolId" :: (IdentityPoolId)
  }
derive instance newtypeMergeDeveloperIdentitiesInput :: Newtype MergeDeveloperIdentitiesInput _
derive instance repGenericMergeDeveloperIdentitiesInput :: Generic MergeDeveloperIdentitiesInput _
instance showMergeDeveloperIdentitiesInput :: Show MergeDeveloperIdentitiesInput where
  show = genericShow
instance decodeMergeDeveloperIdentitiesInput :: Decode MergeDeveloperIdentitiesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeDeveloperIdentitiesInput :: Encode MergeDeveloperIdentitiesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned in response to a successful <code>MergeDeveloperIdentities</code> action.</p>
newtype MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  }
derive instance newtypeMergeDeveloperIdentitiesResponse :: Newtype MergeDeveloperIdentitiesResponse _
derive instance repGenericMergeDeveloperIdentitiesResponse :: Generic MergeDeveloperIdentitiesResponse _
instance showMergeDeveloperIdentitiesResponse :: Show MergeDeveloperIdentitiesResponse where
  show = genericShow
instance decodeMergeDeveloperIdentitiesResponse :: Decode MergeDeveloperIdentitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergeDeveloperIdentitiesResponse :: Encode MergeDeveloperIdentitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when a user is not authorized to access the requested resource.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _
derive instance repGenericNotAuthorizedException :: Generic NotAuthorizedException _
instance showNotAuthorizedException :: Show NotAuthorizedException where
  show = genericShow
instance decodeNotAuthorizedException :: Decode NotAuthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAuthorizedException :: Encode NotAuthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OIDCProviderList = OIDCProviderList (Array ARNString)
derive instance newtypeOIDCProviderList :: Newtype OIDCProviderList _
derive instance repGenericOIDCProviderList :: Generic OIDCProviderList _
instance showOIDCProviderList :: Show OIDCProviderList where
  show = genericShow
instance decodeOIDCProviderList :: Decode OIDCProviderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOIDCProviderList :: Encode OIDCProviderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OIDCToken = OIDCToken String
derive instance newtypeOIDCToken :: Newtype OIDCToken _
derive instance repGenericOIDCToken :: Generic OIDCToken _
instance showOIDCToken :: Show OIDCToken where
  show = genericShow
instance decodeOIDCToken :: Decode OIDCToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOIDCToken :: Encode OIDCToken where
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


newtype QueryLimit = QueryLimit Int
derive instance newtypeQueryLimit :: Newtype QueryLimit _
derive instance repGenericQueryLimit :: Generic QueryLimit _
instance showQueryLimit :: Show QueryLimit where
  show = genericShow
instance decodeQueryLimit :: Decode QueryLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLimit :: Encode QueryLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when a user tries to use a login which is already linked to another account.</p>
newtype ResourceConflictException = ResourceConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceConflictException :: Newtype ResourceConflictException _
derive instance repGenericResourceConflictException :: Generic ResourceConflictException _
instance showResourceConflictException :: Show ResourceConflictException where
  show = genericShow
instance decodeResourceConflictException :: Decode ResourceConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceConflictException :: Encode ResourceConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when the requested resource (for example, a dataset or record) does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A role mapping.</p>
newtype RoleMapping = RoleMapping 
  { "Type" :: (RoleMappingType)
  , "AmbiguousRoleResolution" :: NullOrUndefined.NullOrUndefined (AmbiguousRoleResolutionType)
  , "RulesConfiguration" :: NullOrUndefined.NullOrUndefined (RulesConfigurationType)
  }
derive instance newtypeRoleMapping :: Newtype RoleMapping _
derive instance repGenericRoleMapping :: Generic RoleMapping _
instance showRoleMapping :: Show RoleMapping where
  show = genericShow
instance decodeRoleMapping :: Decode RoleMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleMapping :: Encode RoleMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleMappingMap = RoleMappingMap (StrMap.StrMap RoleMapping)
derive instance newtypeRoleMappingMap :: Newtype RoleMappingMap _
derive instance repGenericRoleMappingMap :: Generic RoleMappingMap _
instance showRoleMappingMap :: Show RoleMappingMap where
  show = genericShow
instance decodeRoleMappingMap :: Decode RoleMappingMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleMappingMap :: Encode RoleMappingMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleMappingType = RoleMappingType String
derive instance newtypeRoleMappingType :: Newtype RoleMappingType _
derive instance repGenericRoleMappingType :: Generic RoleMappingType _
instance showRoleMappingType :: Show RoleMappingType where
  show = genericShow
instance decodeRoleMappingType :: Decode RoleMappingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleMappingType :: Encode RoleMappingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleType = RoleType String
derive instance newtypeRoleType :: Newtype RoleType _
derive instance repGenericRoleType :: Generic RoleType _
instance showRoleType :: Show RoleType where
  show = genericShow
instance decodeRoleType :: Decode RoleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleType :: Encode RoleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RolesMap = RolesMap (StrMap.StrMap ARNString)
derive instance newtypeRolesMap :: Newtype RolesMap _
derive instance repGenericRolesMap :: Generic RolesMap _
instance showRolesMap :: Show RolesMap where
  show = genericShow
instance decodeRolesMap :: Decode RolesMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRolesMap :: Encode RolesMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for rules.</p>
newtype RulesConfigurationType = RulesConfigurationType 
  { "Rules" :: (MappingRulesList)
  }
derive instance newtypeRulesConfigurationType :: Newtype RulesConfigurationType _
derive instance repGenericRulesConfigurationType :: Generic RulesConfigurationType _
instance showRulesConfigurationType :: Show RulesConfigurationType where
  show = genericShow
instance decodeRulesConfigurationType :: Decode RulesConfigurationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRulesConfigurationType :: Encode RulesConfigurationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SAMLProviderList = SAMLProviderList (Array ARNString)
derive instance newtypeSAMLProviderList :: Newtype SAMLProviderList _
derive instance repGenericSAMLProviderList :: Generic SAMLProviderList _
instance showSAMLProviderList :: Show SAMLProviderList where
  show = genericShow
instance decodeSAMLProviderList :: Decode SAMLProviderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSAMLProviderList :: Encode SAMLProviderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecretKeyString = SecretKeyString String
derive instance newtypeSecretKeyString :: Newtype SecretKeyString _
derive instance repGenericSecretKeyString :: Generic SecretKeyString _
instance showSecretKeyString :: Show SecretKeyString where
  show = genericShow
instance decodeSecretKeyString :: Decode SecretKeyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecretKeyString :: Encode SecretKeyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SessionTokenString = SessionTokenString String
derive instance newtypeSessionTokenString :: Newtype SessionTokenString _
derive instance repGenericSessionTokenString :: Generic SessionTokenString _
instance showSessionTokenString :: Show SessionTokenString where
  show = genericShow
instance decodeSessionTokenString :: Decode SessionTokenString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSessionTokenString :: Encode SessionTokenString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>SetIdentityPoolRoles</code> action.</p>
newtype SetIdentityPoolRolesInput = SetIdentityPoolRolesInput 
  { "IdentityPoolId" :: (IdentityPoolId)
  , "Roles" :: (RolesMap)
  , "RoleMappings" :: NullOrUndefined.NullOrUndefined (RoleMappingMap)
  }
derive instance newtypeSetIdentityPoolRolesInput :: Newtype SetIdentityPoolRolesInput _
derive instance repGenericSetIdentityPoolRolesInput :: Generic SetIdentityPoolRolesInput _
instance showSetIdentityPoolRolesInput :: Show SetIdentityPoolRolesInput where
  show = genericShow
instance decodeSetIdentityPoolRolesInput :: Decode SetIdentityPoolRolesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIdentityPoolRolesInput :: Encode SetIdentityPoolRolesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TokenDuration = TokenDuration Number
derive instance newtypeTokenDuration :: Newtype TokenDuration _
derive instance repGenericTokenDuration :: Generic TokenDuration _
instance showTokenDuration :: Show TokenDuration where
  show = genericShow
instance decodeTokenDuration :: Decode TokenDuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTokenDuration :: Encode TokenDuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thrown when a request is throttled.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the <code>UnlinkDeveloperIdentity</code> action.</p>
newtype UnlinkDeveloperIdentityInput = UnlinkDeveloperIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "IdentityPoolId" :: (IdentityPoolId)
  , "DeveloperProviderName" :: (DeveloperProviderName)
  , "DeveloperUserIdentifier" :: (DeveloperUserIdentifier)
  }
derive instance newtypeUnlinkDeveloperIdentityInput :: Newtype UnlinkDeveloperIdentityInput _
derive instance repGenericUnlinkDeveloperIdentityInput :: Generic UnlinkDeveloperIdentityInput _
instance showUnlinkDeveloperIdentityInput :: Show UnlinkDeveloperIdentityInput where
  show = genericShow
instance decodeUnlinkDeveloperIdentityInput :: Decode UnlinkDeveloperIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnlinkDeveloperIdentityInput :: Encode UnlinkDeveloperIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to the UnlinkIdentity action.</p>
newtype UnlinkIdentityInput = UnlinkIdentityInput 
  { "IdentityId" :: (IdentityId)
  , "Logins" :: (LoginsMap)
  , "LoginsToRemove" :: (LoginsList)
  }
derive instance newtypeUnlinkIdentityInput :: Newtype UnlinkIdentityInput _
derive instance repGenericUnlinkIdentityInput :: Generic UnlinkIdentityInput _
instance showUnlinkIdentityInput :: Show UnlinkIdentityInput where
  show = genericShow
instance decodeUnlinkIdentityInput :: Decode UnlinkIdentityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnlinkIdentityInput :: Encode UnlinkIdentityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.</p>
newtype UnprocessedIdentityId = UnprocessedIdentityId 
  { "IdentityId" :: NullOrUndefined.NullOrUndefined (IdentityId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeUnprocessedIdentityId :: Newtype UnprocessedIdentityId _
derive instance repGenericUnprocessedIdentityId :: Generic UnprocessedIdentityId _
instance showUnprocessedIdentityId :: Show UnprocessedIdentityId where
  show = genericShow
instance decodeUnprocessedIdentityId :: Decode UnprocessedIdentityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedIdentityId :: Encode UnprocessedIdentityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnprocessedIdentityIdList = UnprocessedIdentityIdList (Array UnprocessedIdentityId)
derive instance newtypeUnprocessedIdentityIdList :: Newtype UnprocessedIdentityIdList _
derive instance repGenericUnprocessedIdentityIdList :: Generic UnprocessedIdentityIdList _
instance showUnprocessedIdentityIdList :: Show UnprocessedIdentityIdList where
  show = genericShow
instance decodeUnprocessedIdentityIdList :: Decode UnprocessedIdentityIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedIdentityIdList :: Encode UnprocessedIdentityIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
