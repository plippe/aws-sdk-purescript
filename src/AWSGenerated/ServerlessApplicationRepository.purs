

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

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ServerlessApplicationRepository" :: String


-- | <p>Creates an application, optionally including an AWS SAM file to create the first application version in the same call.</p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: AWS.RequestError | eff) CreateApplicationResponse
createApplication = AWS.request serviceName "CreateApplication" 


-- | <p>Creates an application version.</p>
createApplicationVersion :: forall eff. CreateApplicationVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateApplicationVersionResponse
createApplicationVersion = AWS.request serviceName "CreateApplicationVersion" 


-- | <p>Creates an AWS CloudFormation ChangeSet for the given application.</p>
createCloudFormationChangeSet :: forall eff. CreateCloudFormationChangeSetRequest -> Aff (err :: AWS.RequestError | eff) CreateCloudFormationChangeSetResponse
createCloudFormationChangeSet = AWS.request serviceName "CreateCloudFormationChangeSet" 


-- | <p>Deletes the specified application.</p>
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteApplication = AWS.request serviceName "DeleteApplication" 


-- | <p>Gets the specified application.</p>
getApplication :: forall eff. GetApplicationRequest -> Aff (err :: AWS.RequestError | eff) GetApplicationResponse
getApplication = AWS.request serviceName "GetApplication" 


-- | <p>Gets the policy for the specified application.</p>
getApplicationPolicy :: forall eff. GetApplicationPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetApplicationPolicyResponse
getApplicationPolicy = AWS.request serviceName "GetApplicationPolicy" 


-- | <p>Lists versions for the specified application.</p>
listApplicationVersions :: forall eff. ListApplicationVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListApplicationVersionsResponse
listApplicationVersions = AWS.request serviceName "ListApplicationVersions" 


-- | <p>Lists applications owned by the requester.</p>
listApplications :: forall eff. ListApplicationsRequest -> Aff (err :: AWS.RequestError | eff) ListApplicationsResponse
listApplications = AWS.request serviceName "ListApplications" 


-- | <p>Puts the policy for the specified application.</p>
putApplicationPolicy :: forall eff. PutApplicationPolicyRequest -> Aff (err :: AWS.RequestError | eff) PutApplicationPolicyResponse
putApplicationPolicy = AWS.request serviceName "PutApplicationPolicy" 


-- | <p>Updates the specified application.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: AWS.RequestError | eff) UpdateApplicationResponse
updateApplication = AWS.request serviceName "UpdateApplication" 


-- | <p>Details about the application.</p>
newtype Application = Application 
  { "ApplicationId" :: (String)
  , "Author" :: (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Version)
  }


-- | <p>List of application details.</p>
newtype ApplicationPage = ApplicationPage 
  { "Applications" :: (ListOfApplicationSummary')
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Policy statements applied to the application.</p>
newtype ApplicationPolicy = ApplicationPolicy 
  { "Statements" :: (ListOfApplicationPolicyStatement')
  }


-- | <p>Policy statement applied to the application.</p>
newtype ApplicationPolicyStatement = ApplicationPolicyStatement 
  { "Actions" :: (ListOf__string')
  , "Principals" :: (ListOf__string')
  , "StatementId" :: NullOrUndefined (String)
  }


-- | <p>Summary of details about the application.</p>
newtype ApplicationSummary = ApplicationSummary 
  { "ApplicationId" :: (String)
  , "Author" :: (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "Name" :: (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  }


-- | <p>List of version summaries for the application.</p>
newtype ApplicationVersionPage = ApplicationVersionPage 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: (ListOfVersionSummary')
  }


-- | <p>One of the parameters in the request is invalid.</p>
newtype BadRequestException = BadRequestException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>Details of the change set.</p>
newtype ChangeSetDetails = ChangeSetDetails 
  { "ApplicationId" :: (String)
  , "ChangeSetId" :: (String)
  , "SemanticVersion" :: (String)
  , "StackId" :: (String)
  }


-- | <p>The resource already exists.</p>
newtype ConflictException = ConflictException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>Create application request.</p>
newtype CreateApplicationInput = CreateApplicationInput 
  { "Author" :: (String)
  , "Description" :: (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseBody" :: NullOrUndefined (String)
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "ReadmeBody" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined (String)
  }


newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Author" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseBody" :: NullOrUndefined (String)
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "ReadmeBody" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined (String)
  }


newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "Author" :: NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Version)
  }


-- | <p>Create version request.</p>
newtype CreateApplicationVersionInput = CreateApplicationVersionInput 
  { "SourceCodeUrl" :: NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined (String)
  }


newtype CreateApplicationVersionRequest = CreateApplicationVersionRequest 
  { "ApplicationId" :: (String)
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  , "TemplateBody" :: NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined (String)
  }


newtype CreateApplicationVersionResponse = CreateApplicationVersionResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "ParameterDefinitions" :: NullOrUndefined (ListOfParameterDefinition')
  , "SemanticVersion" :: NullOrUndefined (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  , "TemplateUrl" :: NullOrUndefined (String)
  }


-- | <p>Create application ChangeSet request.</p>
newtype CreateCloudFormationChangeSetInput = CreateCloudFormationChangeSetInput 
  { "ParameterOverrides" :: NullOrUndefined (ListOfParameterValue')
  , "SemanticVersion" :: NullOrUndefined (String)
  , "StackName" :: (String)
  }


newtype CreateCloudFormationChangeSetRequest = CreateCloudFormationChangeSetRequest 
  { "ApplicationId" :: (String)
  , "ParameterOverrides" :: NullOrUndefined (ListOfParameterValue')
  , "SemanticVersion" :: NullOrUndefined (String)
  , "StackName" :: NullOrUndefined (String)
  }


newtype CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "ChangeSetId" :: NullOrUndefined (String)
  , "SemanticVersion" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  }


newtype DeleteApplicationRequest = DeleteApplicationRequest 
  { "ApplicationId" :: (String)
  }


-- | <p>The client is not authenticated.</p>
newtype ForbiddenException = ForbiddenException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


newtype GetApplicationPolicyRequest = GetApplicationPolicyRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApplicationPolicyResponse = GetApplicationPolicyResponse 
  { "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement')
  }


newtype GetApplicationRequest = GetApplicationRequest 
  { "ApplicationId" :: (String)
  , "SemanticVersion" :: NullOrUndefined (String)
  }


newtype GetApplicationResponse = GetApplicationResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "Author" :: NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Version)
  }


-- | <p>The AWS Serverless Application Repository service encountered an internal error.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


newtype ListApplicationVersionsRequest = ListApplicationVersionsRequest 
  { "ApplicationId" :: (String)
  , "MaxItems" :: NullOrUndefined (MaxItems)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListApplicationVersionsResponse = ListApplicationVersionsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Versions" :: NullOrUndefined (ListOfVersionSummary')
  }


newtype ListApplicationsRequest = ListApplicationsRequest 
  { "MaxItems" :: NullOrUndefined (MaxItems)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListApplicationsResponse = ListApplicationsResponse 
  { "Applications" :: NullOrUndefined (ListOfApplicationSummary')
  , "NextToken" :: NullOrUndefined (String)
  }


newtype MaxItems = MaxItems Int


-- | <p>The resource (for example, an access policy statement) specified in the request does not exist.</p>
newtype NotFoundException = NotFoundException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>Parameters supported by the application.</p>
newtype ParameterDefinition = ParameterDefinition 
  { "AllowedPattern" :: NullOrUndefined (String)
  , "AllowedValues" :: NullOrUndefined (ListOf__string')
  , "ConstraintDescription" :: NullOrUndefined (String)
  , "DefaultValue" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "MaxLength" :: NullOrUndefined (Int)
  , "MaxValue" :: NullOrUndefined (Int)
  , "MinLength" :: NullOrUndefined (Int)
  , "MinValue" :: NullOrUndefined (Int)
  , "Name" :: (String)
  , "NoEcho" :: NullOrUndefined (Boolean)
  , "ReferencedByResources" :: (ListOf__string')
  , "Type" :: NullOrUndefined (String)
  }


-- | <p>Parameter value of the application.</p>
newtype ParameterValue = ParameterValue 
  { "Name" :: (String)
  , "Value" :: (String)
  }


newtype PutApplicationPolicyRequest = PutApplicationPolicyRequest 
  { "ApplicationId" :: (String)
  , "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement')
  }


newtype PutApplicationPolicyResponse = PutApplicationPolicyResponse 
  { "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement')
  }


-- | <p>The client is sending more than the allowed number of requests per unit time.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>Update application request.</p>
newtype UpdateApplicationInput = UpdateApplicationInput 
  { "Author" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "ReadmeBody" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  }


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ApplicationId" :: (String)
  , "Author" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "ReadmeBody" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  }


newtype UpdateApplicationResponse = UpdateApplicationResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "Author" :: NullOrUndefined (String)
  , "CreationTime" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HomePageUrl" :: NullOrUndefined (String)
  , "Labels" :: NullOrUndefined (ListOf__string')
  , "LicenseUrl" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "ReadmeUrl" :: NullOrUndefined (String)
  , "SpdxLicenseId" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Version)
  }


-- | <p>Application version details.</p>
newtype Version = Version 
  { "ApplicationId" :: (String)
  , "CreationTime" :: (String)
  , "ParameterDefinitions" :: (ListOfParameterDefinition')
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  , "TemplateUrl" :: (String)
  }


-- | <p>Application version summary.</p>
newtype VersionSummary = VersionSummary 
  { "ApplicationId" :: (String)
  , "CreationTime" :: (String)
  , "SemanticVersion" :: (String)
  , "SourceCodeUrl" :: NullOrUndefined (String)
  }


newtype ListOfApplicationPolicyStatement' = ListOfApplicationPolicyStatement' (Array ApplicationPolicyStatement)


newtype ListOfApplicationSummary' = ListOfApplicationSummary' (Array ApplicationSummary)


newtype ListOfParameterDefinition' = ListOfParameterDefinition' (Array ParameterDefinition)


newtype ListOfParameterValue' = ListOfParameterValue' (Array ParameterValue)


newtype ListOfVersionSummary' = ListOfVersionSummary' (Array VersionSummary)


newtype ListOf__string' = ListOf__string' (Array String)
