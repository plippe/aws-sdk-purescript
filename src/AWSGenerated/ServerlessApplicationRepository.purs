

-- | <p>The AWS Serverless Application Repository makes it easy for developers and enterprises to quickly find
-- |  and deploy serverless applications in the AWS Cloud. For more information about serverless applications,
-- |  see Serverless Computing and Applications on the AWS website.</p><p>The AWS Serverless Application Repository is deeply integrated with the AWS Lambda console, so that developers of 
-- |  all levels can get started with serverless computing without needing to learn anything new. You can use category 
-- |  keywords to browse for applications such as web and mobile backends, data processing applications, or chatbots. 
-- |  You can also search for applications by name, publisher, or event source. To use an application, you simply choose it, 
-- |  configure any required fields, and deploy it with a few clicks. </p><p>You can also easily publish applications, sharing them publicly with the community at large, or privately
-- |  within your team or across your organization. To publish a serverless application (or app), you can use the
-- |  AWS Management Console, AWS Command Line Interface (AWS CLI), or AWS SDKs to upload the code. Along with the
-- |  code, you upload a simple manifest file, also known as the AWS Serverless Application Model (AWS SAM) template.
-- |  For more information about AWS SAM, see AWS Serverless Application Model (AWS SAM) on the AWS Labs
-- |  GitHub repository.</p><p>The AWS Serverless Application Repository Developer Guide contains more information about the two developer
-- |  experiences available:</p><ul>
-- |  <li>
-- |  <p>Consuming Applications – Browse for applications and view information about them, including
-- |  source code and readme files. Also install, configure, and deploy applications of your choosing. </p>
-- |  <p>Publishing Applications – Configure and upload applications to make them available to other
-- |  developers, and publish new versions of applications. </p>
-- |  </li>
-- |  </ul>
module AWS.ServerlessApplicationRepository where

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

serviceName = "ServerlessApplicationRepository" :: String


-- | <p>Creates an application, optionally including an AWS SAM file to create the first application version in the same call.</p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (exception :: EXCEPTION | eff) CreateApplicationResponse
createApplication = Request.request serviceName "createApplication" 


-- | <p>Creates an application version.</p>
createApplicationVersion :: forall eff. CreateApplicationVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateApplicationVersionResponse
createApplicationVersion = Request.request serviceName "createApplicationVersion" 


-- | <p>Creates an AWS CloudFormation ChangeSet for the given application.</p>
createCloudFormationChangeSet :: forall eff. CreateCloudFormationChangeSetRequest -> Aff (exception :: EXCEPTION | eff) CreateCloudFormationChangeSetResponse
createCloudFormationChangeSet = Request.request serviceName "createCloudFormationChangeSet" 


-- | <p>Deletes the specified application.</p>
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteApplication = Request.request serviceName "deleteApplication" 


-- | <p>Gets the specified application.</p>
getApplication :: forall eff. GetApplicationRequest -> Aff (exception :: EXCEPTION | eff) GetApplicationResponse
getApplication = Request.request serviceName "getApplication" 


-- | <p>Gets the policy for the specified application.</p>
getApplicationPolicy :: forall eff. GetApplicationPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetApplicationPolicyResponse
getApplicationPolicy = Request.request serviceName "getApplicationPolicy" 


-- | <p>Lists versions for the specified application.</p>
listApplicationVersions :: forall eff. ListApplicationVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListApplicationVersionsResponse
listApplicationVersions = Request.request serviceName "listApplicationVersions" 


-- | <p>Lists applications owned by the requester.</p>
listApplications :: forall eff. ListApplicationsRequest -> Aff (exception :: EXCEPTION | eff) ListApplicationsResponse
listApplications = Request.request serviceName "listApplications" 


-- | <p>Puts the policy for the specified application.</p>
putApplicationPolicy :: forall eff. PutApplicationPolicyRequest -> Aff (exception :: EXCEPTION | eff) PutApplicationPolicyResponse
putApplicationPolicy = Request.request serviceName "putApplicationPolicy" 


-- | <p>Updates the specified application.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (exception :: EXCEPTION | eff) UpdateApplicationResponse
updateApplication = Request.request serviceName "updateApplication" 


-- | <p>Details about the application.</p>
newtype Application = Application 
  { "ApplicationId" :: (String)
  , "Author" :: (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeApplication :: Newtype Application _
derive instance repGenericApplication :: Generic Application _
instance showApplication :: Show Application where
  show = genericShow
instance decodeApplication :: Decode Application where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplication :: Encode Application where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>List of application details.</p>
newtype ApplicationPage = ApplicationPage 
  { "Applications" :: (ListOfApplicationSummary')
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplicationPage :: Newtype ApplicationPage _
derive instance repGenericApplicationPage :: Generic ApplicationPage _
instance showApplicationPage :: Show ApplicationPage where
  show = genericShow
instance decodeApplicationPage :: Decode ApplicationPage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationPage :: Encode ApplicationPage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Policy statements applied to the application.</p>
newtype ApplicationPolicy = ApplicationPolicy 
  { "Statements" :: (ListOfApplicationPolicyStatement')
  }
derive instance newtypeApplicationPolicy :: Newtype ApplicationPolicy _
derive instance repGenericApplicationPolicy :: Generic ApplicationPolicy _
instance showApplicationPolicy :: Show ApplicationPolicy where
  show = genericShow
instance decodeApplicationPolicy :: Decode ApplicationPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationPolicy :: Encode ApplicationPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Policy statement applied to the application.</p>
newtype ApplicationPolicyStatement = ApplicationPolicyStatement 
  { "Actions" :: (ListOf__string')
  , "Principals" :: (ListOf__string')
  , "StatementId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplicationPolicyStatement :: Newtype ApplicationPolicyStatement _
derive instance repGenericApplicationPolicyStatement :: Generic ApplicationPolicyStatement _
instance showApplicationPolicyStatement :: Show ApplicationPolicyStatement where
  show = genericShow
instance decodeApplicationPolicyStatement :: Decode ApplicationPolicyStatement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationPolicyStatement :: Encode ApplicationPolicyStatement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Summary of details about the application.</p>
newtype ApplicationSummary = ApplicationSummary 
  { "ApplicationId" :: (String)
  , "Author" :: (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "Name" :: (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplicationSummary :: Newtype ApplicationSummary _
derive instance repGenericApplicationSummary :: Generic ApplicationSummary _
instance showApplicationSummary :: Show ApplicationSummary where
  show = genericShow
instance decodeApplicationSummary :: Decode ApplicationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationSummary :: Encode ApplicationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>List of version summaries for the application.</p>
newtype ApplicationVersionPage = ApplicationVersionPage 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: (ListOfVersionSummary')
  }
derive instance newtypeApplicationVersionPage :: Newtype ApplicationVersionPage _
derive instance repGenericApplicationVersionPage :: Generic ApplicationVersionPage _
instance showApplicationVersionPage :: Show ApplicationVersionPage where
  show = genericShow
instance decodeApplicationVersionPage :: Decode ApplicationVersionPage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionPage :: Encode ApplicationVersionPage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One of the parameters in the request is invalid.</p>
newtype BadRequestException = BadRequestException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details of the change set.</p>
newtype ChangeSetDetails = ChangeSetDetails 
  { "ApplicationId" :: (String)
  , "ChangeSetId" :: (String)
  , "SemanticVersion" :: (String)
  , "StackId" :: (String)
  }
derive instance newtypeChangeSetDetails :: Newtype ChangeSetDetails _
derive instance repGenericChangeSetDetails :: Generic ChangeSetDetails _
instance showChangeSetDetails :: Show ChangeSetDetails where
  show = genericShow
instance decodeChangeSetDetails :: Decode ChangeSetDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetDetails :: Encode ChangeSetDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource already exists.</p>
newtype ConflictException = ConflictException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Create application request.</p>
newtype CreateApplicationInput = CreateApplicationInput 
  { "Author" :: (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseBody" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: (String)
  , "ReadmeBody" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationInput :: Newtype CreateApplicationInput _
derive instance repGenericCreateApplicationInput :: Generic CreateApplicationInput _
instance showCreateApplicationInput :: Show CreateApplicationInput where
  show = genericShow
instance decodeCreateApplicationInput :: Decode CreateApplicationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationInput :: Encode CreateApplicationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseBody" :: NullOrUndefined.NullOrUndefined (String)
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeBody" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _
derive instance repGenericCreateApplicationRequest :: Generic CreateApplicationRequest _
instance showCreateApplicationRequest :: Show CreateApplicationRequest where
  show = genericShow
instance decodeCreateApplicationRequest :: Decode CreateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationRequest :: Encode CreateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeCreateApplicationResponse :: Newtype CreateApplicationResponse _
derive instance repGenericCreateApplicationResponse :: Generic CreateApplicationResponse _
instance showCreateApplicationResponse :: Show CreateApplicationResponse where
  show = genericShow
instance decodeCreateApplicationResponse :: Decode CreateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationResponse :: Encode CreateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Create version request.</p>
newtype CreateApplicationVersionInput = CreateApplicationVersionInput 
  { "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationVersionInput :: Newtype CreateApplicationVersionInput _
derive instance repGenericCreateApplicationVersionInput :: Generic CreateApplicationVersionInput _
instance showCreateApplicationVersionInput :: Show CreateApplicationVersionInput where
  show = genericShow
instance decodeCreateApplicationVersionInput :: Decode CreateApplicationVersionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationVersionInput :: Encode CreateApplicationVersionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationVersionRequest = CreateApplicationVersionRequest 
  { "ApplicationId" :: (String)
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationVersionRequest :: Newtype CreateApplicationVersionRequest _
derive instance repGenericCreateApplicationVersionRequest :: Generic CreateApplicationVersionRequest _
instance showCreateApplicationVersionRequest :: Show CreateApplicationVersionRequest where
  show = genericShow
instance decodeCreateApplicationVersionRequest :: Decode CreateApplicationVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationVersionRequest :: Encode CreateApplicationVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationVersionResponse = CreateApplicationVersionResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "ParameterDefinitions" :: NullOrUndefined.NullOrUndefined (ListOfParameterDefinition')
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationVersionResponse :: Newtype CreateApplicationVersionResponse _
derive instance repGenericCreateApplicationVersionResponse :: Generic CreateApplicationVersionResponse _
instance showCreateApplicationVersionResponse :: Show CreateApplicationVersionResponse where
  show = genericShow
instance decodeCreateApplicationVersionResponse :: Decode CreateApplicationVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationVersionResponse :: Encode CreateApplicationVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Create application ChangeSet request.</p>
newtype CreateCloudFormationChangeSetInput = CreateCloudFormationChangeSetInput 
  { "ParameterOverrides" :: NullOrUndefined.NullOrUndefined (ListOfParameterValue')
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "StackName" :: (String)
  }
derive instance newtypeCreateCloudFormationChangeSetInput :: Newtype CreateCloudFormationChangeSetInput _
derive instance repGenericCreateCloudFormationChangeSetInput :: Generic CreateCloudFormationChangeSetInput _
instance showCreateCloudFormationChangeSetInput :: Show CreateCloudFormationChangeSetInput where
  show = genericShow
instance decodeCreateCloudFormationChangeSetInput :: Decode CreateCloudFormationChangeSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCloudFormationChangeSetInput :: Encode CreateCloudFormationChangeSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCloudFormationChangeSetRequest = CreateCloudFormationChangeSetRequest 
  { "ApplicationId" :: (String)
  , "ParameterOverrides" :: NullOrUndefined.NullOrUndefined (ListOfParameterValue')
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "StackName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCloudFormationChangeSetRequest :: Newtype CreateCloudFormationChangeSetRequest _
derive instance repGenericCreateCloudFormationChangeSetRequest :: Generic CreateCloudFormationChangeSetRequest _
instance showCreateCloudFormationChangeSetRequest :: Show CreateCloudFormationChangeSetRequest where
  show = genericShow
instance decodeCreateCloudFormationChangeSetRequest :: Decode CreateCloudFormationChangeSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCloudFormationChangeSetRequest :: Encode CreateCloudFormationChangeSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "ChangeSetId" :: NullOrUndefined.NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "StackId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCloudFormationChangeSetResponse :: Newtype CreateCloudFormationChangeSetResponse _
derive instance repGenericCreateCloudFormationChangeSetResponse :: Generic CreateCloudFormationChangeSetResponse _
instance showCreateCloudFormationChangeSetResponse :: Show CreateCloudFormationChangeSetResponse where
  show = genericShow
instance decodeCreateCloudFormationChangeSetResponse :: Decode CreateCloudFormationChangeSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCloudFormationChangeSetResponse :: Encode CreateCloudFormationChangeSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationRequest = DeleteApplicationRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApplicationRequest :: Newtype DeleteApplicationRequest _
derive instance repGenericDeleteApplicationRequest :: Generic DeleteApplicationRequest _
instance showDeleteApplicationRequest :: Show DeleteApplicationRequest where
  show = genericShow
instance decodeDeleteApplicationRequest :: Decode DeleteApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationRequest :: Encode DeleteApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client is not authenticated.</p>
newtype ForbiddenException = ForbiddenException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _
derive instance repGenericForbiddenException :: Generic ForbiddenException _
instance showForbiddenException :: Show ForbiddenException where
  show = genericShow
instance decodeForbiddenException :: Decode ForbiddenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForbiddenException :: Encode ForbiddenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationPolicyRequest = GetApplicationPolicyRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApplicationPolicyRequest :: Newtype GetApplicationPolicyRequest _
derive instance repGenericGetApplicationPolicyRequest :: Generic GetApplicationPolicyRequest _
instance showGetApplicationPolicyRequest :: Show GetApplicationPolicyRequest where
  show = genericShow
instance decodeGetApplicationPolicyRequest :: Decode GetApplicationPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationPolicyRequest :: Encode GetApplicationPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationPolicyResponse = GetApplicationPolicyResponse 
  { "Statements" :: NullOrUndefined.NullOrUndefined (ListOfApplicationPolicyStatement')
  }
derive instance newtypeGetApplicationPolicyResponse :: Newtype GetApplicationPolicyResponse _
derive instance repGenericGetApplicationPolicyResponse :: Generic GetApplicationPolicyResponse _
instance showGetApplicationPolicyResponse :: Show GetApplicationPolicyResponse where
  show = genericShow
instance decodeGetApplicationPolicyResponse :: Decode GetApplicationPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationPolicyResponse :: Encode GetApplicationPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationRequest = GetApplicationRequest 
  { "ApplicationId" :: (String)
  , "SemanticVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetApplicationRequest :: Newtype GetApplicationRequest _
derive instance repGenericGetApplicationRequest :: Generic GetApplicationRequest _
instance showGetApplicationRequest :: Show GetApplicationRequest where
  show = genericShow
instance decodeGetApplicationRequest :: Decode GetApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationRequest :: Encode GetApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetApplicationResponse = GetApplicationResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeGetApplicationResponse :: Newtype GetApplicationResponse _
derive instance repGenericGetApplicationResponse :: Generic GetApplicationResponse _
instance showGetApplicationResponse :: Show GetApplicationResponse where
  show = genericShow
instance decodeGetApplicationResponse :: Decode GetApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetApplicationResponse :: Encode GetApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS Serverless Application Repository service encountered an internal error.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApplicationVersionsRequest = ListApplicationVersionsRequest 
  { "ApplicationId" :: (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListApplicationVersionsRequest :: Newtype ListApplicationVersionsRequest _
derive instance repGenericListApplicationVersionsRequest :: Generic ListApplicationVersionsRequest _
instance showListApplicationVersionsRequest :: Show ListApplicationVersionsRequest where
  show = genericShow
instance decodeListApplicationVersionsRequest :: Decode ListApplicationVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationVersionsRequest :: Encode ListApplicationVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApplicationVersionsResponse = ListApplicationVersionsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions" :: NullOrUndefined.NullOrUndefined (ListOfVersionSummary')
  }
derive instance newtypeListApplicationVersionsResponse :: Newtype ListApplicationVersionsResponse _
derive instance repGenericListApplicationVersionsResponse :: Generic ListApplicationVersionsResponse _
instance showListApplicationVersionsResponse :: Show ListApplicationVersionsResponse where
  show = genericShow
instance decodeListApplicationVersionsResponse :: Decode ListApplicationVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationVersionsResponse :: Encode ListApplicationVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApplicationsRequest = ListApplicationsRequest 
  { "MaxItems" :: NullOrUndefined.NullOrUndefined (MaxItems)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListApplicationsRequest :: Newtype ListApplicationsRequest _
derive instance repGenericListApplicationsRequest :: Generic ListApplicationsRequest _
instance showListApplicationsRequest :: Show ListApplicationsRequest where
  show = genericShow
instance decodeListApplicationsRequest :: Decode ListApplicationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationsRequest :: Encode ListApplicationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApplicationsResponse = ListApplicationsResponse 
  { "Applications" :: NullOrUndefined.NullOrUndefined (ListOfApplicationSummary')
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListApplicationsResponse :: Newtype ListApplicationsResponse _
derive instance repGenericListApplicationsResponse :: Generic ListApplicationsResponse _
instance showListApplicationsResponse :: Show ListApplicationsResponse where
  show = genericShow
instance decodeListApplicationsResponse :: Decode ListApplicationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationsResponse :: Encode ListApplicationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxItems = MaxItems Int
derive instance newtypeMaxItems :: Newtype MaxItems _
derive instance repGenericMaxItems :: Generic MaxItems _
instance showMaxItems :: Show MaxItems where
  show = genericShow
instance decodeMaxItems :: Decode MaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxItems :: Encode MaxItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource (for example, an access policy statement) specified in the request does not exist.</p>
newtype NotFoundException = NotFoundException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameters supported by the application.</p>
newtype ParameterDefinition = ParameterDefinition 
  { "AllowedPattern" :: NullOrUndefined.NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "ConstraintDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxValue" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinValue" :: NullOrUndefined.NullOrUndefined (Int)
  , "Name" :: (String)
  , "NoEcho" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ReferencedByResources" :: (ListOf__string')
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterDefinition :: Newtype ParameterDefinition _
derive instance repGenericParameterDefinition :: Generic ParameterDefinition _
instance showParameterDefinition :: Show ParameterDefinition where
  show = genericShow
instance decodeParameterDefinition :: Decode ParameterDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterDefinition :: Encode ParameterDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameter value of the application.</p>
newtype ParameterValue = ParameterValue 
  { "Name" :: (String)
  , "Value" :: (String)
  }
derive instance newtypeParameterValue :: Newtype ParameterValue _
derive instance repGenericParameterValue :: Generic ParameterValue _
instance showParameterValue :: Show ParameterValue where
  show = genericShow
instance decodeParameterValue :: Decode ParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterValue :: Encode ParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutApplicationPolicyRequest = PutApplicationPolicyRequest 
  { "ApplicationId" :: (String)
  , "Statements" :: NullOrUndefined.NullOrUndefined (ListOfApplicationPolicyStatement')
  }
derive instance newtypePutApplicationPolicyRequest :: Newtype PutApplicationPolicyRequest _
derive instance repGenericPutApplicationPolicyRequest :: Generic PutApplicationPolicyRequest _
instance showPutApplicationPolicyRequest :: Show PutApplicationPolicyRequest where
  show = genericShow
instance decodePutApplicationPolicyRequest :: Decode PutApplicationPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutApplicationPolicyRequest :: Encode PutApplicationPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutApplicationPolicyResponse = PutApplicationPolicyResponse 
  { "Statements" :: NullOrUndefined.NullOrUndefined (ListOfApplicationPolicyStatement')
  }
derive instance newtypePutApplicationPolicyResponse :: Newtype PutApplicationPolicyResponse _
derive instance repGenericPutApplicationPolicyResponse :: Generic PutApplicationPolicyResponse _
instance showPutApplicationPolicyResponse :: Show PutApplicationPolicyResponse where
  show = genericShow
instance decodePutApplicationPolicyResponse :: Decode PutApplicationPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutApplicationPolicyResponse :: Encode PutApplicationPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client is sending more than the allowed number of requests per unit time.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Update application request.</p>
newtype UpdateApplicationInput = UpdateApplicationInput 
  { "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "ReadmeBody" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateApplicationInput :: Newtype UpdateApplicationInput _
derive instance repGenericUpdateApplicationInput :: Generic UpdateApplicationInput _
instance showUpdateApplicationInput :: Show UpdateApplicationInput where
  show = genericShow
instance decodeUpdateApplicationInput :: Decode UpdateApplicationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationInput :: Encode UpdateApplicationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ApplicationId" :: (String)
  , "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "ReadmeBody" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateApplicationRequest :: Newtype UpdateApplicationRequest _
derive instance repGenericUpdateApplicationRequest :: Generic UpdateApplicationRequest _
instance showUpdateApplicationRequest :: Show UpdateApplicationRequest where
  show = genericShow
instance decodeUpdateApplicationRequest :: Decode UpdateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationRequest :: Encode UpdateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationResponse = UpdateApplicationResponse 
  { "ApplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "Author" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Labels" :: NullOrUndefined.NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (Version)
  }
derive instance newtypeUpdateApplicationResponse :: Newtype UpdateApplicationResponse _
derive instance repGenericUpdateApplicationResponse :: Generic UpdateApplicationResponse _
instance showUpdateApplicationResponse :: Show UpdateApplicationResponse where
  show = genericShow
instance decodeUpdateApplicationResponse :: Decode UpdateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationResponse :: Encode UpdateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Application version details.</p>
newtype Version = Version 
  { "ApplicationId" :: (String)
  , "CreationTime" :: (String)
  , "ParameterDefinitions" :: (ListOfParameterDefinition')
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "TemplateUrl" :: (String)
  }
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Application version summary.</p>
newtype VersionSummary = VersionSummary 
  { "ApplicationId" :: (String)
  , "CreationTime" :: (String)
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVersionSummary :: Newtype VersionSummary _
derive instance repGenericVersionSummary :: Generic VersionSummary _
instance showVersionSummary :: Show VersionSummary where
  show = genericShow
instance decodeVersionSummary :: Decode VersionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionSummary :: Encode VersionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfApplicationPolicyStatement' = ListOfApplicationPolicyStatement' (Array ApplicationPolicyStatement)
derive instance newtypeListOfApplicationPolicyStatement' :: Newtype ListOfApplicationPolicyStatement' _
derive instance repGenericListOfApplicationPolicyStatement' :: Generic ListOfApplicationPolicyStatement' _
instance showListOfApplicationPolicyStatement' :: Show ListOfApplicationPolicyStatement' where
  show = genericShow
instance decodeListOfApplicationPolicyStatement' :: Decode ListOfApplicationPolicyStatement' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfApplicationPolicyStatement' :: Encode ListOfApplicationPolicyStatement' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfApplicationSummary' = ListOfApplicationSummary' (Array ApplicationSummary)
derive instance newtypeListOfApplicationSummary' :: Newtype ListOfApplicationSummary' _
derive instance repGenericListOfApplicationSummary' :: Generic ListOfApplicationSummary' _
instance showListOfApplicationSummary' :: Show ListOfApplicationSummary' where
  show = genericShow
instance decodeListOfApplicationSummary' :: Decode ListOfApplicationSummary' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfApplicationSummary' :: Encode ListOfApplicationSummary' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfParameterDefinition' = ListOfParameterDefinition' (Array ParameterDefinition)
derive instance newtypeListOfParameterDefinition' :: Newtype ListOfParameterDefinition' _
derive instance repGenericListOfParameterDefinition' :: Generic ListOfParameterDefinition' _
instance showListOfParameterDefinition' :: Show ListOfParameterDefinition' where
  show = genericShow
instance decodeListOfParameterDefinition' :: Decode ListOfParameterDefinition' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfParameterDefinition' :: Encode ListOfParameterDefinition' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfParameterValue' = ListOfParameterValue' (Array ParameterValue)
derive instance newtypeListOfParameterValue' :: Newtype ListOfParameterValue' _
derive instance repGenericListOfParameterValue' :: Generic ListOfParameterValue' _
instance showListOfParameterValue' :: Show ListOfParameterValue' where
  show = genericShow
instance decodeListOfParameterValue' :: Decode ListOfParameterValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfParameterValue' :: Encode ListOfParameterValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfVersionSummary' = ListOfVersionSummary' (Array VersionSummary)
derive instance newtypeListOfVersionSummary' :: Newtype ListOfVersionSummary' _
derive instance repGenericListOfVersionSummary' :: Generic ListOfVersionSummary' _
instance showListOfVersionSummary' :: Show ListOfVersionSummary' where
  show = genericShow
instance decodeListOfVersionSummary' :: Decode ListOfVersionSummary' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfVersionSummary' :: Encode ListOfVersionSummary' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOf__string' = ListOf__string' (Array String)
derive instance newtypeListOf__string' :: Newtype ListOf__string' _
derive instance repGenericListOf__string' :: Generic ListOf__string' _
instance showListOf__string' :: Show ListOf__string' where
  show = genericShow
instance decodeListOf__string' :: Decode ListOf__string' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOf__string' :: Encode ListOf__string' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
