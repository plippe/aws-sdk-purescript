## Module AWS.ServerlessApplicationRepository

<p>The AWS Serverless Application Repository makes it easy for developers and enterprises to quickly find
 and deploy serverless applications in the AWS Cloud. For more information about serverless applications,
 see Serverless Computing and Applications on the AWS website.</p><p>The AWS Serverless Application Repository is deeply integrated with the AWS Lambda console, so that developers of 
 all levels can get started with serverless computing without needing to learn anything new. You can use category 
 keywords to browse for applications such as web and mobile backends, data processing applications, or chatbots. 
 You can also search for applications by name, publisher, or event source. To use an application, you simply choose it, 
 configure any required fields, and deploy it with a few clicks. </p><p>You can also easily publish applications, sharing them publicly with the community at large, or privately
 within your team or across your organization. To publish a serverless application (or app), you can use the
 AWS Management Console, AWS Command Line Interface (AWS CLI), or AWS SDKs to upload the code. Along with the
 code, you upload a simple manifest file, also known as the AWS Serverless Application Model (AWS SAM) template.
 For more information about AWS SAM, see AWS Serverless Application Model (AWS SAM) on the AWS Labs
 GitHub repository.</p><p>The AWS Serverless Application Repository Developer Guide contains more information about the two developer
 experiences available:</p><ul>
 <li>
 <p>Consuming Applications – Browse for applications and view information about them, including
 source code and readme files. Also install, configure, and deploy applications of your choosing. </p>
 <p>Publishing Applications – Configure and upload applications to make them available to other
 developers, and publish new versions of applications. </p>
 </li>
 </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createApplication`

``` purescript
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: RequestError | eff) CreateApplicationResponse
```

<p>Creates an application, optionally including an AWS SAM file to create the first application version in the same call.</p>

#### `createApplicationVersion`

``` purescript
createApplicationVersion :: forall eff. CreateApplicationVersionRequest -> Aff (err :: RequestError | eff) CreateApplicationVersionResponse
```

<p>Creates an application version.</p>

#### `createCloudFormationChangeSet`

``` purescript
createCloudFormationChangeSet :: forall eff. CreateCloudFormationChangeSetRequest -> Aff (err :: RequestError | eff) CreateCloudFormationChangeSetResponse
```

<p>Creates an AWS CloudFormation ChangeSet for the given application.</p>

#### `deleteApplication`

``` purescript
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified application.</p>

#### `getApplication`

``` purescript
getApplication :: forall eff. GetApplicationRequest -> Aff (err :: RequestError | eff) GetApplicationResponse
```

<p>Gets the specified application.</p>

#### `getApplicationPolicy`

``` purescript
getApplicationPolicy :: forall eff. GetApplicationPolicyRequest -> Aff (err :: RequestError | eff) GetApplicationPolicyResponse
```

<p>Gets the policy for the specified application.</p>

#### `listApplicationVersions`

``` purescript
listApplicationVersions :: forall eff. ListApplicationVersionsRequest -> Aff (err :: RequestError | eff) ListApplicationVersionsResponse
```

<p>Lists versions for the specified application.</p>

#### `listApplications`

``` purescript
listApplications :: forall eff. ListApplicationsRequest -> Aff (err :: RequestError | eff) ListApplicationsResponse
```

<p>Lists applications owned by the requester.</p>

#### `putApplicationPolicy`

``` purescript
putApplicationPolicy :: forall eff. PutApplicationPolicyRequest -> Aff (err :: RequestError | eff) PutApplicationPolicyResponse
```

<p>Puts the policy for the specified application.</p>

#### `updateApplication`

``` purescript
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: RequestError | eff) UpdateApplicationResponse
```

<p>Updates the specified application.</p>

#### `Application`

``` purescript
newtype Application
  = Application { "ApplicationId" :: String, "Author" :: String, "CreationTime" :: NullOrUndefined (String), "Description" :: String, "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseUrl" :: NullOrUndefined (String), "Name" :: String, "ReadmeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Version) }
```

<p>Details about the application.</p>

##### Instances
``` purescript
Newtype Application _
```

#### `ApplicationPage`

``` purescript
newtype ApplicationPage
  = ApplicationPage { "Applications" :: ListOfApplicationSummary', "NextToken" :: NullOrUndefined (String) }
```

<p>List of application details.</p>

##### Instances
``` purescript
Newtype ApplicationPage _
```

#### `ApplicationPolicy`

``` purescript
newtype ApplicationPolicy
  = ApplicationPolicy { "Statements" :: ListOfApplicationPolicyStatement' }
```

<p>Policy statements applied to the application.</p>

##### Instances
``` purescript
Newtype ApplicationPolicy _
```

#### `ApplicationPolicyStatement`

``` purescript
newtype ApplicationPolicyStatement
  = ApplicationPolicyStatement { "Actions" :: ListOf__string', "Principals" :: ListOf__string', "StatementId" :: NullOrUndefined (String) }
```

<p>Policy statement applied to the application.</p>

##### Instances
``` purescript
Newtype ApplicationPolicyStatement _
```

#### `ApplicationSummary`

``` purescript
newtype ApplicationSummary
  = ApplicationSummary { "ApplicationId" :: String, "Author" :: String, "CreationTime" :: NullOrUndefined (String), "Description" :: String, "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "Name" :: String, "SpdxLicenseId" :: NullOrUndefined (String) }
```

<p>Summary of details about the application.</p>

##### Instances
``` purescript
Newtype ApplicationSummary _
```

#### `ApplicationVersionPage`

``` purescript
newtype ApplicationVersionPage
  = ApplicationVersionPage { "NextToken" :: NullOrUndefined (String), "Versions" :: ListOfVersionSummary' }
```

<p>List of version summaries for the application.</p>

##### Instances
``` purescript
Newtype ApplicationVersionPage _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>One of the parameters in the request is invalid.</p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `ChangeSetDetails`

``` purescript
newtype ChangeSetDetails
  = ChangeSetDetails { "ApplicationId" :: String, "ChangeSetId" :: String, "SemanticVersion" :: String, "StackId" :: String }
```

<p>Details of the change set.</p>

##### Instances
``` purescript
Newtype ChangeSetDetails _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>The resource already exists.</p>

##### Instances
``` purescript
Newtype ConflictException _
```

#### `CreateApplicationInput`

``` purescript
newtype CreateApplicationInput
  = CreateApplicationInput { "Author" :: String, "Description" :: String, "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseBody" :: NullOrUndefined (String), "LicenseUrl" :: NullOrUndefined (String), "Name" :: String, "ReadmeBody" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String), "SemanticVersion" :: NullOrUndefined (String), "SourceCodeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "TemplateBody" :: NullOrUndefined (String), "TemplateUrl" :: NullOrUndefined (String) }
```

<p>Create application request.</p>

##### Instances
``` purescript
Newtype CreateApplicationInput _
```

#### `CreateApplicationRequest`

``` purescript
newtype CreateApplicationRequest
  = CreateApplicationRequest { "Author" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseBody" :: NullOrUndefined (String), "LicenseUrl" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ReadmeBody" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String), "SemanticVersion" :: NullOrUndefined (String), "SourceCodeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "TemplateBody" :: NullOrUndefined (String), "TemplateUrl" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateApplicationRequest _
```

#### `CreateApplicationResponse`

``` purescript
newtype CreateApplicationResponse
  = CreateApplicationResponse { "ApplicationId" :: NullOrUndefined (String), "Author" :: NullOrUndefined (String), "CreationTime" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseUrl" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Version) }
```

##### Instances
``` purescript
Newtype CreateApplicationResponse _
```

#### `CreateApplicationVersionInput`

``` purescript
newtype CreateApplicationVersionInput
  = CreateApplicationVersionInput { "SourceCodeUrl" :: NullOrUndefined (String), "TemplateBody" :: NullOrUndefined (String), "TemplateUrl" :: NullOrUndefined (String) }
```

<p>Create version request.</p>

##### Instances
``` purescript
Newtype CreateApplicationVersionInput _
```

#### `CreateApplicationVersionRequest`

``` purescript
newtype CreateApplicationVersionRequest
  = CreateApplicationVersionRequest { "ApplicationId" :: String, "SemanticVersion" :: String, "SourceCodeUrl" :: NullOrUndefined (String), "TemplateBody" :: NullOrUndefined (String), "TemplateUrl" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateApplicationVersionRequest _
```

#### `CreateApplicationVersionResponse`

``` purescript
newtype CreateApplicationVersionResponse
  = CreateApplicationVersionResponse { "ApplicationId" :: NullOrUndefined (String), "CreationTime" :: NullOrUndefined (String), "ParameterDefinitions" :: NullOrUndefined (ListOfParameterDefinition'), "SemanticVersion" :: NullOrUndefined (String), "SourceCodeUrl" :: NullOrUndefined (String), "TemplateUrl" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateApplicationVersionResponse _
```

#### `CreateCloudFormationChangeSetInput`

``` purescript
newtype CreateCloudFormationChangeSetInput
  = CreateCloudFormationChangeSetInput { "ParameterOverrides" :: NullOrUndefined (ListOfParameterValue'), "SemanticVersion" :: NullOrUndefined (String), "StackName" :: String }
```

<p>Create application ChangeSet request.</p>

##### Instances
``` purescript
Newtype CreateCloudFormationChangeSetInput _
```

#### `CreateCloudFormationChangeSetRequest`

``` purescript
newtype CreateCloudFormationChangeSetRequest
  = CreateCloudFormationChangeSetRequest { "ApplicationId" :: String, "ParameterOverrides" :: NullOrUndefined (ListOfParameterValue'), "SemanticVersion" :: NullOrUndefined (String), "StackName" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateCloudFormationChangeSetRequest _
```

#### `CreateCloudFormationChangeSetResponse`

``` purescript
newtype CreateCloudFormationChangeSetResponse
  = CreateCloudFormationChangeSetResponse { "ApplicationId" :: NullOrUndefined (String), "ChangeSetId" :: NullOrUndefined (String), "SemanticVersion" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateCloudFormationChangeSetResponse _
```

#### `DeleteApplicationRequest`

``` purescript
newtype DeleteApplicationRequest
  = DeleteApplicationRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteApplicationRequest _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>The client is not authenticated.</p>

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `GetApplicationPolicyRequest`

``` purescript
newtype GetApplicationPolicyRequest
  = GetApplicationPolicyRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApplicationPolicyRequest _
```

#### `GetApplicationPolicyResponse`

``` purescript
newtype GetApplicationPolicyResponse
  = GetApplicationPolicyResponse { "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement') }
```

##### Instances
``` purescript
Newtype GetApplicationPolicyResponse _
```

#### `GetApplicationRequest`

``` purescript
newtype GetApplicationRequest
  = GetApplicationRequest { "ApplicationId" :: String, "SemanticVersion" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetApplicationRequest _
```

#### `GetApplicationResponse`

``` purescript
newtype GetApplicationResponse
  = GetApplicationResponse { "ApplicationId" :: NullOrUndefined (String), "Author" :: NullOrUndefined (String), "CreationTime" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseUrl" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Version) }
```

##### Instances
``` purescript
Newtype GetApplicationResponse _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>The AWS Serverless Application Repository service encountered an internal error.</p>

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `ListApplicationVersionsRequest`

``` purescript
newtype ListApplicationVersionsRequest
  = ListApplicationVersionsRequest { "ApplicationId" :: String, "MaxItems" :: NullOrUndefined (MaxItems), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListApplicationVersionsRequest _
```

#### `ListApplicationVersionsResponse`

``` purescript
newtype ListApplicationVersionsResponse
  = ListApplicationVersionsResponse { "NextToken" :: NullOrUndefined (String), "Versions" :: NullOrUndefined (ListOfVersionSummary') }
```

##### Instances
``` purescript
Newtype ListApplicationVersionsResponse _
```

#### `ListApplicationsRequest`

``` purescript
newtype ListApplicationsRequest
  = ListApplicationsRequest { "MaxItems" :: NullOrUndefined (MaxItems), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListApplicationsRequest _
```

#### `ListApplicationsResponse`

``` purescript
newtype ListApplicationsResponse
  = ListApplicationsResponse { "Applications" :: NullOrUndefined (ListOfApplicationSummary'), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListApplicationsResponse _
```

#### `MaxItems`

``` purescript
newtype MaxItems
  = MaxItems Int
```

##### Instances
``` purescript
Newtype MaxItems _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>The resource (for example, an access policy statement) specified in the request does not exist.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `ParameterDefinition`

``` purescript
newtype ParameterDefinition
  = ParameterDefinition { "AllowedPattern" :: NullOrUndefined (String), "AllowedValues" :: NullOrUndefined (ListOf__string'), "ConstraintDescription" :: NullOrUndefined (String), "DefaultValue" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "MaxLength" :: NullOrUndefined (Int), "MaxValue" :: NullOrUndefined (Int), "MinLength" :: NullOrUndefined (Int), "MinValue" :: NullOrUndefined (Int), "Name" :: String, "NoEcho" :: NullOrUndefined (Boolean), "ReferencedByResources" :: ListOf__string', "Type" :: NullOrUndefined (String) }
```

<p>Parameters supported by the application.</p>

##### Instances
``` purescript
Newtype ParameterDefinition _
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue { "Name" :: String, "Value" :: String }
```

<p>Parameter value of the application.</p>

##### Instances
``` purescript
Newtype ParameterValue _
```

#### `PutApplicationPolicyRequest`

``` purescript
newtype PutApplicationPolicyRequest
  = PutApplicationPolicyRequest { "ApplicationId" :: String, "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement') }
```

##### Instances
``` purescript
Newtype PutApplicationPolicyRequest _
```

#### `PutApplicationPolicyResponse`

``` purescript
newtype PutApplicationPolicyResponse
  = PutApplicationPolicyResponse { "Statements" :: NullOrUndefined (ListOfApplicationPolicyStatement') }
```

##### Instances
``` purescript
Newtype PutApplicationPolicyResponse _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>The client is sending more than the allowed number of requests per unit time.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UpdateApplicationInput`

``` purescript
newtype UpdateApplicationInput
  = UpdateApplicationInput { "Author" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "ReadmeBody" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String) }
```

<p>Update application request.</p>

##### Instances
``` purescript
Newtype UpdateApplicationInput _
```

#### `UpdateApplicationRequest`

``` purescript
newtype UpdateApplicationRequest
  = UpdateApplicationRequest { "ApplicationId" :: String, "Author" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "ReadmeBody" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateApplicationRequest _
```

#### `UpdateApplicationResponse`

``` purescript
newtype UpdateApplicationResponse
  = UpdateApplicationResponse { "ApplicationId" :: NullOrUndefined (String), "Author" :: NullOrUndefined (String), "CreationTime" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HomePageUrl" :: NullOrUndefined (String), "Labels" :: NullOrUndefined (ListOf__string'), "LicenseUrl" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "ReadmeUrl" :: NullOrUndefined (String), "SpdxLicenseId" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Version) }
```

##### Instances
``` purescript
Newtype UpdateApplicationResponse _
```

#### `Version`

``` purescript
newtype Version
  = Version { "ApplicationId" :: String, "CreationTime" :: String, "ParameterDefinitions" :: ListOfParameterDefinition', "SemanticVersion" :: String, "SourceCodeUrl" :: NullOrUndefined (String), "TemplateUrl" :: String }
```

<p>Application version details.</p>

##### Instances
``` purescript
Newtype Version _
```

#### `VersionSummary`

``` purescript
newtype VersionSummary
  = VersionSummary { "ApplicationId" :: String, "CreationTime" :: String, "SemanticVersion" :: String, "SourceCodeUrl" :: NullOrUndefined (String) }
```

<p>Application version summary.</p>

##### Instances
``` purescript
Newtype VersionSummary _
```

#### `ListOfApplicationPolicyStatement'`

``` purescript
newtype ListOfApplicationPolicyStatement'
  = ListOfApplicationPolicyStatement' (Array ApplicationPolicyStatement)
```

##### Instances
``` purescript
Newtype ListOfApplicationPolicyStatement' _
```

#### `ListOfApplicationSummary'`

``` purescript
newtype ListOfApplicationSummary'
  = ListOfApplicationSummary' (Array ApplicationSummary)
```

##### Instances
``` purescript
Newtype ListOfApplicationSummary' _
```

#### `ListOfParameterDefinition'`

``` purescript
newtype ListOfParameterDefinition'
  = ListOfParameterDefinition' (Array ParameterDefinition)
```

##### Instances
``` purescript
Newtype ListOfParameterDefinition' _
```

#### `ListOfParameterValue'`

``` purescript
newtype ListOfParameterValue'
  = ListOfParameterValue' (Array ParameterValue)
```

##### Instances
``` purescript
Newtype ListOfParameterValue' _
```

#### `ListOfVersionSummary'`

``` purescript
newtype ListOfVersionSummary'
  = ListOfVersionSummary' (Array VersionSummary)
```

##### Instances
``` purescript
Newtype ListOfVersionSummary' _
```

#### `ListOf__string'`

``` purescript
newtype ListOf__string'
  = ListOf__string' (Array String)
```

##### Instances
``` purescript
Newtype ListOf__string' _
```


