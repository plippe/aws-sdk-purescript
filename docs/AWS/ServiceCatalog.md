## Module AWS.ServiceCatalog

<fullname>AWS Service Catalog</fullname> <p> <a href="https://aws.amazon.com/servicecatalog/">AWS Service Catalog</a> enables organizations to create and manage catalogs of IT services that are approved for use on AWS. To get the most out of this documentation, you should be familiar with the terminology discussed in <a href="http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html">AWS Service Catalog Concepts</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptPortfolioShare`

``` purescript
acceptPortfolioShare :: forall eff. AcceptPortfolioShareInput -> Aff (err :: RequestError | eff) AcceptPortfolioShareOutput
```

<p>Accepts an offer to share the specified portfolio.</p>

#### `associatePrincipalWithPortfolio`

``` purescript
associatePrincipalWithPortfolio :: forall eff. AssociatePrincipalWithPortfolioInput -> Aff (err :: RequestError | eff) AssociatePrincipalWithPortfolioOutput
```

<p>Associates the specified principal ARN with the specified portfolio.</p>

#### `associateProductWithPortfolio`

``` purescript
associateProductWithPortfolio :: forall eff. AssociateProductWithPortfolioInput -> Aff (err :: RequestError | eff) AssociateProductWithPortfolioOutput
```

<p>Associates the specified product with the specified portfolio.</p>

#### `associateTagOptionWithResource`

``` purescript
associateTagOptionWithResource :: forall eff. AssociateTagOptionWithResourceInput -> Aff (err :: RequestError | eff) AssociateTagOptionWithResourceOutput
```

<p>Associate the specified TagOption with the specified portfolio or product.</p>

#### `copyProduct`

``` purescript
copyProduct :: forall eff. CopyProductInput -> Aff (err :: RequestError | eff) CopyProductOutput
```

<p>Copies the specified source product to the specified target product or a new product.</p> <p>You can copy a product to the same account or another account. You can copy a product to the same region or another region.</p> <p>This operation is performed asynchronously. To track the progress of the operation, use <a>DescribeCopyProductStatus</a>.</p>

#### `createConstraint`

``` purescript
createConstraint :: forall eff. CreateConstraintInput -> Aff (err :: RequestError | eff) CreateConstraintOutput
```

<p>Creates a constraint.</p>

#### `createPortfolio`

``` purescript
createPortfolio :: forall eff. CreatePortfolioInput -> Aff (err :: RequestError | eff) CreatePortfolioOutput
```

<p>Creates a portfolio.</p>

#### `createPortfolioShare`

``` purescript
createPortfolioShare :: forall eff. CreatePortfolioShareInput -> Aff (err :: RequestError | eff) CreatePortfolioShareOutput
```

<p>Shares the specified portfolio with the specified account.</p>

#### `createProduct`

``` purescript
createProduct :: forall eff. CreateProductInput -> Aff (err :: RequestError | eff) CreateProductOutput
```

<p>Creates a product.</p>

#### `createProvisionedProductPlan`

``` purescript
createProvisionedProductPlan :: forall eff. CreateProvisionedProductPlanInput -> Aff (err :: RequestError | eff) CreateProvisionedProductPlanOutput
```

<p>Creates a plan. A plan includes the list of resources that will be created (when provisioning a new product) or modified (when updating a provisioned product) when the plan is executed.</p> <p>You can create one plan per provisioned product. To create a plan for an existing provisioned product, it's status must be AVAILBLE or TAINTED.</p> <p>To view the resource changes in the change set, use <a>DescribeProvisionedProductPlan</a>. To create or modify the provisioned product, use <a>ExecuteProvisionedProductPlan</a>.</p>

#### `createProvisioningArtifact`

``` purescript
createProvisioningArtifact :: forall eff. CreateProvisioningArtifactInput -> Aff (err :: RequestError | eff) CreateProvisioningArtifactOutput
```

<p>Creates a provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot create a provisioning artifact for a product that was shared with you.</p>

#### `createTagOption`

``` purescript
createTagOption :: forall eff. CreateTagOptionInput -> Aff (err :: RequestError | eff) CreateTagOptionOutput
```

<p>Creates a TagOption.</p>

#### `deleteConstraint`

``` purescript
deleteConstraint :: forall eff. DeleteConstraintInput -> Aff (err :: RequestError | eff) DeleteConstraintOutput
```

<p>Deletes the specified constraint.</p>

#### `deletePortfolio`

``` purescript
deletePortfolio :: forall eff. DeletePortfolioInput -> Aff (err :: RequestError | eff) DeletePortfolioOutput
```

<p>Deletes the specified portfolio.</p> <p>You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.</p>

#### `deletePortfolioShare`

``` purescript
deletePortfolioShare :: forall eff. DeletePortfolioShareInput -> Aff (err :: RequestError | eff) DeletePortfolioShareOutput
```

<p>Stops sharing the specified portfolio with the specified account.</p>

#### `deleteProduct`

``` purescript
deleteProduct :: forall eff. DeleteProductInput -> Aff (err :: RequestError | eff) DeleteProductOutput
```

<p>Deletes the specified product.</p> <p>You cannot delete a product if it was shared with you or is associated with a portfolio.</p>

#### `deleteProvisionedProductPlan`

``` purescript
deleteProvisionedProductPlan :: forall eff. DeleteProvisionedProductPlanInput -> Aff (err :: RequestError | eff) DeleteProvisionedProductPlanOutput
```

<p>Deletes the specified plan.</p>

#### `deleteProvisioningArtifact`

``` purescript
deleteProvisioningArtifact :: forall eff. DeleteProvisioningArtifactInput -> Aff (err :: RequestError | eff) DeleteProvisioningArtifactOutput
```

<p>Deletes the specified provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot delete a provisioning artifact associated with a product that was shared with you. You cannot delete the last provisioning artifact for a product, because a product must have at least one provisioning artifact.</p>

#### `describeConstraint`

``` purescript
describeConstraint :: forall eff. DescribeConstraintInput -> Aff (err :: RequestError | eff) DescribeConstraintOutput
```

<p>Gets information about the specified constraint.</p>

#### `describeCopyProductStatus`

``` purescript
describeCopyProductStatus :: forall eff. DescribeCopyProductStatusInput -> Aff (err :: RequestError | eff) DescribeCopyProductStatusOutput
```

<p>Gets the status of the specified copy product operation.</p>

#### `describePortfolio`

``` purescript
describePortfolio :: forall eff. DescribePortfolioInput -> Aff (err :: RequestError | eff) DescribePortfolioOutput
```

<p>Gets information about the specified portfolio.</p>

#### `describeProduct`

``` purescript
describeProduct :: forall eff. DescribeProductInput -> Aff (err :: RequestError | eff) DescribeProductOutput
```

<p>Gets information about the specified product.</p>

#### `describeProductAsAdmin`

``` purescript
describeProductAsAdmin :: forall eff. DescribeProductAsAdminInput -> Aff (err :: RequestError | eff) DescribeProductAsAdminOutput
```

<p>Gets information about the specified product. This operation is run with administrator access.</p>

#### `describeProductView`

``` purescript
describeProductView :: forall eff. DescribeProductViewInput -> Aff (err :: RequestError | eff) DescribeProductViewOutput
```

<p>Gets information about the specified product.</p>

#### `describeProvisionedProduct`

``` purescript
describeProvisionedProduct :: forall eff. DescribeProvisionedProductInput -> Aff (err :: RequestError | eff) DescribeProvisionedProductOutput
```

<p>Gets information about the specified provisioned product.</p>

#### `describeProvisionedProductPlan`

``` purescript
describeProvisionedProductPlan :: forall eff. DescribeProvisionedProductPlanInput -> Aff (err :: RequestError | eff) DescribeProvisionedProductPlanOutput
```

<p>Gets information about the resource changes for the specified plan.</p>

#### `describeProvisioningArtifact`

``` purescript
describeProvisioningArtifact :: forall eff. DescribeProvisioningArtifactInput -> Aff (err :: RequestError | eff) DescribeProvisioningArtifactOutput
```

<p>Gets information about the specified provisioning artifact (also known as a version) for the specified product.</p>

#### `describeProvisioningParameters`

``` purescript
describeProvisioningParameters :: forall eff. DescribeProvisioningParametersInput -> Aff (err :: RequestError | eff) DescribeProvisioningParametersOutput
```

<p>Gets information about the configuration required to provision the specified product using the specified provisioning artifact.</p> <p>If the output contains a TagOption key with an empty list of values, there is a TagOption conflict for that key. The end user cannot take action to fix the conflict, and launch is not blocked. In subsequent calls to <a>ProvisionProduct</a>, do not include conflicted TagOption keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[<i>N</i>]:<i>Value</i>". Tag the provisioned product with the value <code>sc-tagoption-conflict-portfolioId-productId</code>.</p>

#### `describeRecord`

``` purescript
describeRecord :: forall eff. DescribeRecordInput -> Aff (err :: RequestError | eff) DescribeRecordOutput
```

<p>Gets information about the specified request operation.</p> <p>Use this operation after calling a request operation (for example, <a>ProvisionProduct</a>, <a>TerminateProvisionedProduct</a>, or <a>UpdateProvisionedProduct</a>). </p>

#### `describeTagOption`

``` purescript
describeTagOption :: forall eff. DescribeTagOptionInput -> Aff (err :: RequestError | eff) DescribeTagOptionOutput
```

<p>Gets information about the specified TagOption.</p>

#### `disassociatePrincipalFromPortfolio`

``` purescript
disassociatePrincipalFromPortfolio :: forall eff. DisassociatePrincipalFromPortfolioInput -> Aff (err :: RequestError | eff) DisassociatePrincipalFromPortfolioOutput
```

<p>Disassociates a previously associated principal ARN from a specified portfolio.</p>

#### `disassociateProductFromPortfolio`

``` purescript
disassociateProductFromPortfolio :: forall eff. DisassociateProductFromPortfolioInput -> Aff (err :: RequestError | eff) DisassociateProductFromPortfolioOutput
```

<p>Disassociates the specified product from the specified portfolio. </p>

#### `disassociateTagOptionFromResource`

``` purescript
disassociateTagOptionFromResource :: forall eff. DisassociateTagOptionFromResourceInput -> Aff (err :: RequestError | eff) DisassociateTagOptionFromResourceOutput
```

<p>Disassociates the specified TagOption from the specified resource.</p>

#### `executeProvisionedProductPlan`

``` purescript
executeProvisionedProductPlan :: forall eff. ExecuteProvisionedProductPlanInput -> Aff (err :: RequestError | eff) ExecuteProvisionedProductPlanOutput
```

<p>Provisions or modifies a product based on the resource changes for the specified plan.</p>

#### `listAcceptedPortfolioShares`

``` purescript
listAcceptedPortfolioShares :: forall eff. ListAcceptedPortfolioSharesInput -> Aff (err :: RequestError | eff) ListAcceptedPortfolioSharesOutput
```

<p>Lists all portfolios for which sharing was accepted by this account.</p>

#### `listConstraintsForPortfolio`

``` purescript
listConstraintsForPortfolio :: forall eff. ListConstraintsForPortfolioInput -> Aff (err :: RequestError | eff) ListConstraintsForPortfolioOutput
```

<p>Lists the constraints for the specified portfolio and product.</p>

#### `listLaunchPaths`

``` purescript
listLaunchPaths :: forall eff. ListLaunchPathsInput -> Aff (err :: RequestError | eff) ListLaunchPathsOutput
```

<p>Lists the paths to the specified product. A path is how the user has access to a specified product, and is necessary when provisioning a product. A path also determines the constraints put on the product.</p>

#### `listPortfolioAccess`

``` purescript
listPortfolioAccess :: forall eff. ListPortfolioAccessInput -> Aff (err :: RequestError | eff) ListPortfolioAccessOutput
```

<p>Lists the account IDs that have access to the specified portfolio.</p>

#### `listPortfolios`

``` purescript
listPortfolios :: forall eff. ListPortfoliosInput -> Aff (err :: RequestError | eff) ListPortfoliosOutput
```

<p>Lists all portfolios in the catalog.</p>

#### `listPortfoliosForProduct`

``` purescript
listPortfoliosForProduct :: forall eff. ListPortfoliosForProductInput -> Aff (err :: RequestError | eff) ListPortfoliosForProductOutput
```

<p>Lists all portfolios that the specified product is associated with.</p>

#### `listPrincipalsForPortfolio`

``` purescript
listPrincipalsForPortfolio :: forall eff. ListPrincipalsForPortfolioInput -> Aff (err :: RequestError | eff) ListPrincipalsForPortfolioOutput
```

<p>Lists all principal ARNs associated with the specified portfolio.</p>

#### `listProvisionedProductPlans`

``` purescript
listProvisionedProductPlans :: forall eff. ListProvisionedProductPlansInput -> Aff (err :: RequestError | eff) ListProvisionedProductPlansOutput
```

<p>Lists the plans for the specified provisioned product or all plans the user has access to.</p>

#### `listProvisioningArtifacts`

``` purescript
listProvisioningArtifacts :: forall eff. ListProvisioningArtifactsInput -> Aff (err :: RequestError | eff) ListProvisioningArtifactsOutput
```

<p>Lists all provisioning artifacts (also known as versions) for the specified product.</p>

#### `listRecordHistory`

``` purescript
listRecordHistory :: forall eff. ListRecordHistoryInput -> Aff (err :: RequestError | eff) ListRecordHistoryOutput
```

<p>Lists the specified requests or all performed requests.</p>

#### `listResourcesForTagOption`

``` purescript
listResourcesForTagOption :: forall eff. ListResourcesForTagOptionInput -> Aff (err :: RequestError | eff) ListResourcesForTagOptionOutput
```

<p>Lists the resources associated with the specified TagOption.</p>

#### `listTagOptions`

``` purescript
listTagOptions :: forall eff. ListTagOptionsInput -> Aff (err :: RequestError | eff) ListTagOptionsOutput
```

<p>Lists the specified TagOptions or all TagOptions.</p>

#### `provisionProduct`

``` purescript
provisionProduct :: forall eff. ProvisionProductInput -> Aff (err :: RequestError | eff) ProvisionProductOutput
```

<p>Provisions the specified product.</p> <p>A provisioned product is a resourced instance of a product. For example, provisioning a product based on a CloudFormation template launches a CloudFormation stack and its underlying resources. You can check the status of this request using <a>DescribeRecord</a>.</p> <p>If the request contains a tag key with an empty list of values, there is a tag conflict for that key. Do not include conflicted keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[<i>N</i>]:<i>Value</i>".</p>

#### `rejectPortfolioShare`

``` purescript
rejectPortfolioShare :: forall eff. RejectPortfolioShareInput -> Aff (err :: RequestError | eff) RejectPortfolioShareOutput
```

<p>Rejects an offer to share the specified portfolio.</p>

#### `scanProvisionedProducts`

``` purescript
scanProvisionedProducts :: forall eff. ScanProvisionedProductsInput -> Aff (err :: RequestError | eff) ScanProvisionedProductsOutput
```

<p>Lists the provisioned products that are available (not terminated).</p> <p>To use additional filtering, see <a>SearchProvisionedProducts</a>.</p>

#### `searchProducts`

``` purescript
searchProducts :: forall eff. SearchProductsInput -> Aff (err :: RequestError | eff) SearchProductsOutput
```

<p>Gets information about the products to which the caller has access.</p>

#### `searchProductsAsAdmin`

``` purescript
searchProductsAsAdmin :: forall eff. SearchProductsAsAdminInput -> Aff (err :: RequestError | eff) SearchProductsAsAdminOutput
```

<p>Gets information about the products for the specified portfolio or all products.</p>

#### `searchProvisionedProducts`

``` purescript
searchProvisionedProducts :: forall eff. SearchProvisionedProductsInput -> Aff (err :: RequestError | eff) SearchProvisionedProductsOutput
```

<p>Gets information about the provisioned products that meet the specified criteria.</p>

#### `terminateProvisionedProduct`

``` purescript
terminateProvisionedProduct :: forall eff. TerminateProvisionedProductInput -> Aff (err :: RequestError | eff) TerminateProvisionedProductOutput
```

<p>Terminates the specified provisioned product.</p> <p>This operation does not delete any records associated with the provisioned product.</p> <p>You can check the status of this request using <a>DescribeRecord</a>.</p>

#### `updateConstraint`

``` purescript
updateConstraint :: forall eff. UpdateConstraintInput -> Aff (err :: RequestError | eff) UpdateConstraintOutput
```

<p>Updates the specified constraint.</p>

#### `updatePortfolio`

``` purescript
updatePortfolio :: forall eff. UpdatePortfolioInput -> Aff (err :: RequestError | eff) UpdatePortfolioOutput
```

<p>Updates the specified portfolio.</p> <p>You cannot update a product that was shared with you.</p>

#### `updateProduct`

``` purescript
updateProduct :: forall eff. UpdateProductInput -> Aff (err :: RequestError | eff) UpdateProductOutput
```

<p>Updates the specified product.</p>

#### `updateProvisionedProduct`

``` purescript
updateProvisionedProduct :: forall eff. UpdateProvisionedProductInput -> Aff (err :: RequestError | eff) UpdateProvisionedProductOutput
```

<p>Requests updates to the configuration of the specified provisioned product.</p> <p>If there are tags associated with the object, they cannot be updated or added. Depending on the specific updates requested, this operation can update with no interruption, with some interruption, or replace the provisioned product entirely.</p> <p>You can check the status of this request using <a>DescribeRecord</a>.</p>

#### `updateProvisioningArtifact`

``` purescript
updateProvisioningArtifact :: forall eff. UpdateProvisioningArtifactInput -> Aff (err :: RequestError | eff) UpdateProvisioningArtifactOutput
```

<p>Updates the specified provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot update a provisioning artifact for a product that was shared with you.</p>

#### `updateTagOption`

``` purescript
updateTagOption :: forall eff. UpdateTagOptionInput -> Aff (err :: RequestError | eff) UpdateTagOptionOutput
```

<p>Updates the specified TagOption.</p>

#### `AcceptLanguage`

``` purescript
newtype AcceptLanguage
  = AcceptLanguage String
```

##### Instances
``` purescript
Newtype AcceptLanguage _
```

#### `AcceptPortfolioShareInput`

``` purescript
newtype AcceptPortfolioShareInput
  = AcceptPortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

##### Instances
``` purescript
Newtype AcceptPortfolioShareInput _
```

#### `AcceptPortfolioShareOutput`

``` purescript
newtype AcceptPortfolioShareOutput
  = AcceptPortfolioShareOutput {  }
```

##### Instances
``` purescript
Newtype AcceptPortfolioShareOutput _
```

#### `AccessLevelFilter`

``` purescript
newtype AccessLevelFilter
  = AccessLevelFilter { "Key" :: NullOrUndefined (AccessLevelFilterKey), "Value" :: NullOrUndefined (AccessLevelFilterValue) }
```

<p>The access level to use to filter results.</p>

##### Instances
``` purescript
Newtype AccessLevelFilter _
```

#### `AccessLevelFilterKey`

``` purescript
newtype AccessLevelFilterKey
  = AccessLevelFilterKey String
```

##### Instances
``` purescript
Newtype AccessLevelFilterKey _
```

#### `AccessLevelFilterValue`

``` purescript
newtype AccessLevelFilterValue
  = AccessLevelFilterValue String
```

##### Instances
``` purescript
Newtype AccessLevelFilterValue _
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

##### Instances
``` purescript
Newtype AccountId _
```

#### `AccountIds`

``` purescript
newtype AccountIds
  = AccountIds (Array AccountId)
```

##### Instances
``` purescript
Newtype AccountIds _
```

#### `AddTags`

``` purescript
newtype AddTags
  = AddTags (Array Tag)
```

##### Instances
``` purescript
Newtype AddTags _
```

#### `AllowedValue`

``` purescript
newtype AllowedValue
  = AllowedValue String
```

##### Instances
``` purescript
Newtype AllowedValue _
```

#### `AllowedValues`

``` purescript
newtype AllowedValues
  = AllowedValues (Array AllowedValue)
```

##### Instances
``` purescript
Newtype AllowedValues _
```

#### `ApproximateCount`

``` purescript
newtype ApproximateCount
  = ApproximateCount Int
```

##### Instances
``` purescript
Newtype ApproximateCount _
```

#### `AssociatePrincipalWithPortfolioInput`

``` purescript
newtype AssociatePrincipalWithPortfolioInput
  = AssociatePrincipalWithPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PrincipalARN" :: PrincipalARN, "PrincipalType" :: PrincipalType }
```

##### Instances
``` purescript
Newtype AssociatePrincipalWithPortfolioInput _
```

#### `AssociatePrincipalWithPortfolioOutput`

``` purescript
newtype AssociatePrincipalWithPortfolioOutput
  = AssociatePrincipalWithPortfolioOutput {  }
```

##### Instances
``` purescript
Newtype AssociatePrincipalWithPortfolioOutput _
```

#### `AssociateProductWithPortfolioInput`

``` purescript
newtype AssociateProductWithPortfolioInput
  = AssociateProductWithPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PortfolioId" :: Id, "SourcePortfolioId" :: NullOrUndefined (Id) }
```

##### Instances
``` purescript
Newtype AssociateProductWithPortfolioInput _
```

#### `AssociateProductWithPortfolioOutput`

``` purescript
newtype AssociateProductWithPortfolioOutput
  = AssociateProductWithPortfolioOutput {  }
```

##### Instances
``` purescript
Newtype AssociateProductWithPortfolioOutput _
```

#### `AssociateTagOptionWithResourceInput`

``` purescript
newtype AssociateTagOptionWithResourceInput
  = AssociateTagOptionWithResourceInput { "ResourceId" :: ResourceId, "TagOptionId" :: TagOptionId }
```

##### Instances
``` purescript
Newtype AssociateTagOptionWithResourceInput _
```

#### `AssociateTagOptionWithResourceOutput`

``` purescript
newtype AssociateTagOptionWithResourceOutput
  = AssociateTagOptionWithResourceOutput {  }
```

##### Instances
``` purescript
Newtype AssociateTagOptionWithResourceOutput _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `CausingEntity`

``` purescript
newtype CausingEntity
  = CausingEntity String
```

##### Instances
``` purescript
Newtype CausingEntity _
```

#### `ChangeAction`

``` purescript
newtype ChangeAction
  = ChangeAction String
```

##### Instances
``` purescript
Newtype ChangeAction _
```

#### `CloudWatchDashboard`

``` purescript
newtype CloudWatchDashboard
  = CloudWatchDashboard { "Name" :: NullOrUndefined (CloudWatchDashboardName) }
```

<p>Information about a CloudWatch dashboard.</p>

##### Instances
``` purescript
Newtype CloudWatchDashboard _
```

#### `CloudWatchDashboardName`

``` purescript
newtype CloudWatchDashboardName
  = CloudWatchDashboardName String
```

##### Instances
``` purescript
Newtype CloudWatchDashboardName _
```

#### `CloudWatchDashboards`

``` purescript
newtype CloudWatchDashboards
  = CloudWatchDashboards (Array CloudWatchDashboard)
```

##### Instances
``` purescript
Newtype CloudWatchDashboards _
```

#### `ConstraintDescription`

``` purescript
newtype ConstraintDescription
  = ConstraintDescription String
```

##### Instances
``` purescript
Newtype ConstraintDescription _
```

#### `ConstraintDetail`

``` purescript
newtype ConstraintDetail
  = ConstraintDetail { "ConstraintId" :: NullOrUndefined (Id), "Type" :: NullOrUndefined (ConstraintType), "Description" :: NullOrUndefined (ConstraintDescription), "Owner" :: NullOrUndefined (AccountId) }
```

<p>Information about a constraint.</p>

##### Instances
``` purescript
Newtype ConstraintDetail _
```

#### `ConstraintDetails`

``` purescript
newtype ConstraintDetails
  = ConstraintDetails (Array ConstraintDetail)
```

##### Instances
``` purescript
Newtype ConstraintDetails _
```

#### `ConstraintParameters`

``` purescript
newtype ConstraintParameters
  = ConstraintParameters String
```

##### Instances
``` purescript
Newtype ConstraintParameters _
```

#### `ConstraintSummaries`

``` purescript
newtype ConstraintSummaries
  = ConstraintSummaries (Array ConstraintSummary)
```

##### Instances
``` purescript
Newtype ConstraintSummaries _
```

#### `ConstraintSummary`

``` purescript
newtype ConstraintSummary
  = ConstraintSummary { "Type" :: NullOrUndefined (ConstraintType), "Description" :: NullOrUndefined (ConstraintDescription) }
```

<p>Summary information about a constraint.</p>

##### Instances
``` purescript
Newtype ConstraintSummary _
```

#### `ConstraintType`

``` purescript
newtype ConstraintType
  = ConstraintType String
```

##### Instances
``` purescript
Newtype ConstraintType _
```

#### `CopyOption`

``` purescript
newtype CopyOption
  = CopyOption String
```

##### Instances
``` purescript
Newtype CopyOption _
```

#### `CopyOptions`

``` purescript
newtype CopyOptions
  = CopyOptions (Array CopyOption)
```

##### Instances
``` purescript
Newtype CopyOptions _
```

#### `CopyProductInput`

``` purescript
newtype CopyProductInput
  = CopyProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "SourceProductArn" :: ProductArn, "TargetProductId" :: NullOrUndefined (Id), "TargetProductName" :: NullOrUndefined (ProductViewName), "SourceProvisioningArtifactIdentifiers" :: NullOrUndefined (SourceProvisioningArtifactProperties), "CopyOptions" :: NullOrUndefined (CopyOptions), "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype CopyProductInput _
```

#### `CopyProductOutput`

``` purescript
newtype CopyProductOutput
  = CopyProductOutput { "CopyProductToken" :: NullOrUndefined (Id) }
```

##### Instances
``` purescript
Newtype CopyProductOutput _
```

#### `CopyProductStatus`

``` purescript
newtype CopyProductStatus
  = CopyProductStatus String
```

##### Instances
``` purescript
Newtype CopyProductStatus _
```

#### `CreateConstraintInput`

``` purescript
newtype CreateConstraintInput
  = CreateConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "ProductId" :: Id, "Parameters" :: ConstraintParameters, "Type" :: ConstraintType, "Description" :: NullOrUndefined (ConstraintDescription), "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype CreateConstraintInput _
```

#### `CreateConstraintOutput`

``` purescript
newtype CreateConstraintOutput
  = CreateConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype CreateConstraintOutput _
```

#### `CreatePortfolioInput`

``` purescript
newtype CreatePortfolioInput
  = CreatePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "DisplayName" :: PortfolioDisplayName, "Description" :: NullOrUndefined (PortfolioDescription), "ProviderName" :: ProviderName, "Tags" :: NullOrUndefined (AddTags), "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype CreatePortfolioInput _
```

#### `CreatePortfolioOutput`

``` purescript
newtype CreatePortfolioOutput
  = CreatePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreatePortfolioOutput _
```

#### `CreatePortfolioShareInput`

``` purescript
newtype CreatePortfolioShareInput
  = CreatePortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "AccountId" :: AccountId }
```

##### Instances
``` purescript
Newtype CreatePortfolioShareInput _
```

#### `CreatePortfolioShareOutput`

``` purescript
newtype CreatePortfolioShareOutput
  = CreatePortfolioShareOutput {  }
```

##### Instances
``` purescript
Newtype CreatePortfolioShareOutput _
```

#### `CreateProductInput`

``` purescript
newtype CreateProductInput
  = CreateProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Name" :: ProductViewName, "Owner" :: ProductViewOwner, "Description" :: NullOrUndefined (ProductViewShortDescription), "Distributor" :: NullOrUndefined (ProductViewOwner), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportUrl" :: NullOrUndefined (SupportUrl), "ProductType" :: ProductType, "Tags" :: NullOrUndefined (AddTags), "ProvisioningArtifactParameters" :: ProvisioningArtifactProperties, "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype CreateProductInput _
```

#### `CreateProductOutput`

``` purescript
newtype CreateProductOutput
  = CreateProductOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreateProductOutput _
```

#### `CreateProvisionedProductPlanInput`

``` purescript
newtype CreateProvisionedProductPlanInput
  = CreateProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanName" :: ProvisionedProductPlanName, "PlanType" :: ProvisionedProductPlanType, "NotificationArns" :: NullOrUndefined (NotificationArns), "PathId" :: NullOrUndefined (Id), "ProductId" :: Id, "ProvisionedProductName" :: ProvisionedProductName, "ProvisioningArtifactId" :: Id, "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "IdempotencyToken" :: IdempotencyToken, "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreateProvisionedProductPlanInput _
```

#### `CreateProvisionedProductPlanOutput`

``` purescript
newtype CreateProvisionedProductPlanOutput
  = CreateProvisionedProductPlanOutput { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName), "ProvisioningArtifactId" :: NullOrUndefined (Id) }
```

##### Instances
``` purescript
Newtype CreateProvisionedProductPlanOutput _
```

#### `CreateProvisioningArtifactInput`

``` purescript
newtype CreateProvisioningArtifactInput
  = CreateProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "Parameters" :: ProvisioningArtifactProperties, "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype CreateProvisioningArtifactInput _
```

#### `CreateProvisioningArtifactOutput`

``` purescript
newtype CreateProvisioningArtifactOutput
  = CreateProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype CreateProvisioningArtifactOutput _
```

#### `CreateTagOptionInput`

``` purescript
newtype CreateTagOptionInput
  = CreateTagOptionInput { "Key" :: TagOptionKey, "Value" :: TagOptionValue }
```

##### Instances
``` purescript
Newtype CreateTagOptionInput _
```

#### `CreateTagOptionOutput`

``` purescript
newtype CreateTagOptionOutput
  = CreateTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

##### Instances
``` purescript
Newtype CreateTagOptionOutput _
```

#### `CreatedTime`

``` purescript
newtype CreatedTime
  = CreatedTime Number
```

##### Instances
``` purescript
Newtype CreatedTime _
```

#### `CreationTime`

``` purescript
newtype CreationTime
  = CreationTime Number
```

##### Instances
``` purescript
Newtype CreationTime _
```

#### `DefaultValue`

``` purescript
newtype DefaultValue
  = DefaultValue String
```

##### Instances
``` purescript
Newtype DefaultValue _
```

#### `DeleteConstraintInput`

``` purescript
newtype DeleteConstraintInput
  = DeleteConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DeleteConstraintInput _
```

#### `DeleteConstraintOutput`

``` purescript
newtype DeleteConstraintOutput
  = DeleteConstraintOutput {  }
```

##### Instances
``` purescript
Newtype DeleteConstraintOutput _
```

#### `DeletePortfolioInput`

``` purescript
newtype DeletePortfolioInput
  = DeletePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DeletePortfolioInput _
```

#### `DeletePortfolioOutput`

``` purescript
newtype DeletePortfolioOutput
  = DeletePortfolioOutput {  }
```

##### Instances
``` purescript
Newtype DeletePortfolioOutput _
```

#### `DeletePortfolioShareInput`

``` purescript
newtype DeletePortfolioShareInput
  = DeletePortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "AccountId" :: AccountId }
```

##### Instances
``` purescript
Newtype DeletePortfolioShareInput _
```

#### `DeletePortfolioShareOutput`

``` purescript
newtype DeletePortfolioShareOutput
  = DeletePortfolioShareOutput {  }
```

##### Instances
``` purescript
Newtype DeletePortfolioShareOutput _
```

#### `DeleteProductInput`

``` purescript
newtype DeleteProductInput
  = DeleteProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DeleteProductInput _
```

#### `DeleteProductOutput`

``` purescript
newtype DeleteProductOutput
  = DeleteProductOutput {  }
```

##### Instances
``` purescript
Newtype DeleteProductOutput _
```

#### `DeleteProvisionedProductPlanInput`

``` purescript
newtype DeleteProvisionedProductPlanInput
  = DeleteProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "IgnoreErrors" :: NullOrUndefined (IgnoreErrors) }
```

##### Instances
``` purescript
Newtype DeleteProvisionedProductPlanInput _
```

#### `DeleteProvisionedProductPlanOutput`

``` purescript
newtype DeleteProvisionedProductPlanOutput
  = DeleteProvisionedProductPlanOutput {  }
```

##### Instances
``` purescript
Newtype DeleteProvisionedProductPlanOutput _
```

#### `DeleteProvisioningArtifactInput`

``` purescript
newtype DeleteProvisioningArtifactInput
  = DeleteProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id }
```

##### Instances
``` purescript
Newtype DeleteProvisioningArtifactInput _
```

#### `DeleteProvisioningArtifactOutput`

``` purescript
newtype DeleteProvisioningArtifactOutput
  = DeleteProvisioningArtifactOutput {  }
```

##### Instances
``` purescript
Newtype DeleteProvisioningArtifactOutput _
```

#### `DescribeConstraintInput`

``` purescript
newtype DescribeConstraintInput
  = DescribeConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribeConstraintInput _
```

#### `DescribeConstraintOutput`

``` purescript
newtype DescribeConstraintOutput
  = DescribeConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype DescribeConstraintOutput _
```

#### `DescribeCopyProductStatusInput`

``` purescript
newtype DescribeCopyProductStatusInput
  = DescribeCopyProductStatusInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "CopyProductToken" :: Id }
```

##### Instances
``` purescript
Newtype DescribeCopyProductStatusInput _
```

#### `DescribeCopyProductStatusOutput`

``` purescript
newtype DescribeCopyProductStatusOutput
  = DescribeCopyProductStatusOutput { "CopyProductStatus" :: NullOrUndefined (CopyProductStatus), "TargetProductId" :: NullOrUndefined (Id), "StatusDetail" :: NullOrUndefined (StatusDetail) }
```

##### Instances
``` purescript
Newtype DescribeCopyProductStatusOutput _
```

#### `DescribePortfolioInput`

``` purescript
newtype DescribePortfolioInput
  = DescribePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribePortfolioInput _
```

#### `DescribePortfolioOutput`

``` purescript
newtype DescribePortfolioOutput
  = DescribePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags), "TagOptions" :: NullOrUndefined (TagOptionDetails) }
```

##### Instances
``` purescript
Newtype DescribePortfolioOutput _
```

#### `DescribeProductAsAdminInput`

``` purescript
newtype DescribeProductAsAdminInput
  = DescribeProductAsAdminInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribeProductAsAdminInput _
```

#### `DescribeProductAsAdminOutput`

``` purescript
newtype DescribeProductAsAdminOutput
  = DescribeProductAsAdminOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "ProvisioningArtifactSummaries" :: NullOrUndefined (ProvisioningArtifactSummaries), "Tags" :: NullOrUndefined (Tags), "TagOptions" :: NullOrUndefined (TagOptionDetails) }
```

##### Instances
``` purescript
Newtype DescribeProductAsAdminOutput _
```

#### `DescribeProductInput`

``` purescript
newtype DescribeProductInput
  = DescribeProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribeProductInput _
```

#### `DescribeProductOutput`

``` purescript
newtype DescribeProductOutput
  = DescribeProductOutput { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts) }
```

##### Instances
``` purescript
Newtype DescribeProductOutput _
```

#### `DescribeProductViewInput`

``` purescript
newtype DescribeProductViewInput
  = DescribeProductViewInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribeProductViewInput _
```

#### `DescribeProductViewOutput`

``` purescript
newtype DescribeProductViewOutput
  = DescribeProductViewOutput { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts) }
```

##### Instances
``` purescript
Newtype DescribeProductViewOutput _
```

#### `DescribeProvisionedProductInput`

``` purescript
newtype DescribeProvisionedProductInput
  = DescribeProvisionedProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

##### Instances
``` purescript
Newtype DescribeProvisionedProductInput _
```

#### `DescribeProvisionedProductOutput`

``` purescript
newtype DescribeProvisionedProductOutput
  = DescribeProvisionedProductOutput { "ProvisionedProductDetail" :: NullOrUndefined (ProvisionedProductDetail), "CloudWatchDashboards" :: NullOrUndefined (CloudWatchDashboards) }
```

##### Instances
``` purescript
Newtype DescribeProvisionedProductOutput _
```

#### `DescribeProvisionedProductPlanInput`

``` purescript
newtype DescribeProvisionedProductPlanInput
  = DescribeProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype DescribeProvisionedProductPlanInput _
```

#### `DescribeProvisionedProductPlanOutput`

``` purescript
newtype DescribeProvisionedProductPlanOutput
  = DescribeProvisionedProductPlanOutput { "ProvisionedProductPlanDetails" :: NullOrUndefined (ProvisionedProductPlanDetails), "ResourceChanges" :: NullOrUndefined (ResourceChanges), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype DescribeProvisionedProductPlanOutput _
```

#### `DescribeProvisioningArtifactInput`

``` purescript
newtype DescribeProvisioningArtifactInput
  = DescribeProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisioningArtifactId" :: Id, "ProductId" :: Id, "Verbose" :: NullOrUndefined (Verbose) }
```

##### Instances
``` purescript
Newtype DescribeProvisioningArtifactInput _
```

#### `DescribeProvisioningArtifactOutput`

``` purescript
newtype DescribeProvisioningArtifactOutput
  = DescribeProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype DescribeProvisioningArtifactOutput _
```

#### `DescribeProvisioningParametersInput`

``` purescript
newtype DescribeProvisioningParametersInput
  = DescribeProvisioningParametersInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "PathId" :: NullOrUndefined (Id) }
```

##### Instances
``` purescript
Newtype DescribeProvisioningParametersInput _
```

#### `DescribeProvisioningParametersOutput`

``` purescript
newtype DescribeProvisioningParametersOutput
  = DescribeProvisioningParametersOutput { "ProvisioningArtifactParameters" :: NullOrUndefined (ProvisioningArtifactParameters), "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries), "UsageInstructions" :: NullOrUndefined (UsageInstructions), "TagOptions" :: NullOrUndefined (TagOptionSummaries) }
```

##### Instances
``` purescript
Newtype DescribeProvisioningParametersOutput _
```

#### `DescribeRecordInput`

``` purescript
newtype DescribeRecordInput
  = DescribeRecordInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeRecordInput _
```

#### `DescribeRecordOutput`

``` purescript
newtype DescribeRecordOutput
  = DescribeRecordOutput { "RecordDetail" :: NullOrUndefined (RecordDetail), "RecordOutputs" :: NullOrUndefined (RecordOutputs), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype DescribeRecordOutput _
```

#### `DescribeTagOptionInput`

``` purescript
newtype DescribeTagOptionInput
  = DescribeTagOptionInput { "Id" :: TagOptionId }
```

##### Instances
``` purescript
Newtype DescribeTagOptionInput _
```

#### `DescribeTagOptionOutput`

``` purescript
newtype DescribeTagOptionOutput
  = DescribeTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

##### Instances
``` purescript
Newtype DescribeTagOptionOutput _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DisassociatePrincipalFromPortfolioInput`

``` purescript
newtype DisassociatePrincipalFromPortfolioInput
  = DisassociatePrincipalFromPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PrincipalARN" :: PrincipalARN }
```

##### Instances
``` purescript
Newtype DisassociatePrincipalFromPortfolioInput _
```

#### `DisassociatePrincipalFromPortfolioOutput`

``` purescript
newtype DisassociatePrincipalFromPortfolioOutput
  = DisassociatePrincipalFromPortfolioOutput {  }
```

##### Instances
``` purescript
Newtype DisassociatePrincipalFromPortfolioOutput _
```

#### `DisassociateProductFromPortfolioInput`

``` purescript
newtype DisassociateProductFromPortfolioInput
  = DisassociateProductFromPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PortfolioId" :: Id }
```

##### Instances
``` purescript
Newtype DisassociateProductFromPortfolioInput _
```

#### `DisassociateProductFromPortfolioOutput`

``` purescript
newtype DisassociateProductFromPortfolioOutput
  = DisassociateProductFromPortfolioOutput {  }
```

##### Instances
``` purescript
Newtype DisassociateProductFromPortfolioOutput _
```

#### `DisassociateTagOptionFromResourceInput`

``` purescript
newtype DisassociateTagOptionFromResourceInput
  = DisassociateTagOptionFromResourceInput { "ResourceId" :: ResourceId, "TagOptionId" :: TagOptionId }
```

##### Instances
``` purescript
Newtype DisassociateTagOptionFromResourceInput _
```

#### `DisassociateTagOptionFromResourceOutput`

``` purescript
newtype DisassociateTagOptionFromResourceOutput
  = DisassociateTagOptionFromResourceOutput {  }
```

##### Instances
``` purescript
Newtype DisassociateTagOptionFromResourceOutput _
```

#### `DuplicateResourceException`

``` purescript
newtype DuplicateResourceException
  = DuplicateResourceException {  }
```

<p>The specified resource is a duplicate.</p>

##### Instances
``` purescript
Newtype DuplicateResourceException _
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

##### Instances
``` purescript
Newtype ErrorCode _
```

#### `ErrorDescription`

``` purescript
newtype ErrorDescription
  = ErrorDescription String
```

##### Instances
``` purescript
Newtype ErrorDescription _
```

#### `EvaluationType`

``` purescript
newtype EvaluationType
  = EvaluationType String
```

##### Instances
``` purescript
Newtype EvaluationType _
```

#### `ExecuteProvisionedProductPlanInput`

``` purescript
newtype ExecuteProvisionedProductPlanInput
  = ExecuteProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "IdempotencyToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype ExecuteProvisionedProductPlanInput _
```

#### `ExecuteProvisionedProductPlanOutput`

``` purescript
newtype ExecuteProvisionedProductPlanOutput
  = ExecuteProvisionedProductPlanOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

##### Instances
``` purescript
Newtype ExecuteProvisionedProductPlanOutput _
```

#### `HasDefaultPath`

``` purescript
newtype HasDefaultPath
  = HasDefaultPath Boolean
```

##### Instances
``` purescript
Newtype HasDefaultPath _
```

#### `Id`

``` purescript
newtype Id
  = Id String
```

##### Instances
``` purescript
Newtype Id _
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

##### Instances
``` purescript
Newtype IdempotencyToken _
```

#### `IgnoreErrors`

``` purescript
newtype IgnoreErrors
  = IgnoreErrors Boolean
```

##### Instances
``` purescript
Newtype IgnoreErrors _
```

#### `InstructionType`

``` purescript
newtype InstructionType
  = InstructionType String
```

##### Instances
``` purescript
Newtype InstructionType _
```

#### `InstructionValue`

``` purescript
newtype InstructionValue
  = InstructionValue String
```

##### Instances
``` purescript
Newtype InstructionValue _
```

#### `InvalidParametersException`

``` purescript
newtype InvalidParametersException
  = InvalidParametersException {  }
```

<p>One or more parameters provided to the operation are not valid.</p>

##### Instances
``` purescript
Newtype InvalidParametersException _
```

#### `InvalidStateException`

``` purescript
newtype InvalidStateException
  = InvalidStateException {  }
```

<p>An attempt was made to modify a resource that is in a state that is not valid. Check your resources to ensure that they are in valid states before retrying the operation.</p>

##### Instances
``` purescript
Newtype InvalidStateException _
```

#### `LastRequestId`

``` purescript
newtype LastRequestId
  = LastRequestId String
```

##### Instances
``` purescript
Newtype LastRequestId _
```

#### `LaunchPathSummaries`

``` purescript
newtype LaunchPathSummaries
  = LaunchPathSummaries (Array LaunchPathSummary)
```

##### Instances
``` purescript
Newtype LaunchPathSummaries _
```

#### `LaunchPathSummary`

``` purescript
newtype LaunchPathSummary
  = LaunchPathSummary { "Id" :: NullOrUndefined (Id), "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries), "Tags" :: NullOrUndefined (Tags), "Name" :: NullOrUndefined (PortfolioName) }
```

<p>Summary information about a product path for a user.</p>

##### Instances
``` purescript
Newtype LaunchPathSummary _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The current limits of the service would have been exceeded by this operation. Decrease your resource use or increase your service limits and retry the operation.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListAcceptedPortfolioSharesInput`

``` purescript
newtype ListAcceptedPortfolioSharesInput
  = ListAcceptedPortfolioSharesInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype ListAcceptedPortfolioSharesInput _
```

#### `ListAcceptedPortfolioSharesOutput`

``` purescript
newtype ListAcceptedPortfolioSharesOutput
  = ListAcceptedPortfolioSharesOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListAcceptedPortfolioSharesOutput _
```

#### `ListConstraintsForPortfolioInput`

``` purescript
newtype ListConstraintsForPortfolioInput
  = ListConstraintsForPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "ProductId" :: NullOrUndefined (Id), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListConstraintsForPortfolioInput _
```

#### `ListConstraintsForPortfolioOutput`

``` purescript
newtype ListConstraintsForPortfolioOutput
  = ListConstraintsForPortfolioOutput { "ConstraintDetails" :: NullOrUndefined (ConstraintDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListConstraintsForPortfolioOutput _
```

#### `ListLaunchPathsInput`

``` purescript
newtype ListLaunchPathsInput
  = ListLaunchPathsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListLaunchPathsInput _
```

#### `ListLaunchPathsOutput`

``` purescript
newtype ListLaunchPathsOutput
  = ListLaunchPathsOutput { "LaunchPathSummaries" :: NullOrUndefined (LaunchPathSummaries), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListLaunchPathsOutput _
```

#### `ListPortfolioAccessInput`

``` purescript
newtype ListPortfolioAccessInput
  = ListPortfolioAccessInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

##### Instances
``` purescript
Newtype ListPortfolioAccessInput _
```

#### `ListPortfolioAccessOutput`

``` purescript
newtype ListPortfolioAccessOutput
  = ListPortfolioAccessOutput { "AccountIds" :: NullOrUndefined (AccountIds), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListPortfolioAccessOutput _
```

#### `ListPortfoliosForProductInput`

``` purescript
newtype ListPortfoliosForProductInput
  = ListPortfoliosForProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype ListPortfoliosForProductInput _
```

#### `ListPortfoliosForProductOutput`

``` purescript
newtype ListPortfoliosForProductOutput
  = ListPortfoliosForProductOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListPortfoliosForProductOutput _
```

#### `ListPortfoliosInput`

``` purescript
newtype ListPortfoliosInput
  = ListPortfoliosInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype ListPortfoliosInput _
```

#### `ListPortfoliosOutput`

``` purescript
newtype ListPortfoliosOutput
  = ListPortfoliosOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListPortfoliosOutput _
```

#### `ListPrincipalsForPortfolioInput`

``` purescript
newtype ListPrincipalsForPortfolioInput
  = ListPrincipalsForPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListPrincipalsForPortfolioInput _
```

#### `ListPrincipalsForPortfolioOutput`

``` purescript
newtype ListPrincipalsForPortfolioOutput
  = ListPrincipalsForPortfolioOutput { "Principals" :: NullOrUndefined (Principals), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListPrincipalsForPortfolioOutput _
```

#### `ListProvisionedProductPlansInput`

``` purescript
newtype ListProvisionedProductPlansInput
  = ListProvisionedProductPlansInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisionProductId" :: NullOrUndefined (Id), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter) }
```

##### Instances
``` purescript
Newtype ListProvisionedProductPlansInput _
```

#### `ListProvisionedProductPlansOutput`

``` purescript
newtype ListProvisionedProductPlansOutput
  = ListProvisionedProductPlansOutput { "ProvisionedProductPlans" :: NullOrUndefined (ProvisionedProductPlans), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListProvisionedProductPlansOutput _
```

#### `ListProvisioningArtifactsInput`

``` purescript
newtype ListProvisioningArtifactsInput
  = ListProvisioningArtifactsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id }
```

##### Instances
``` purescript
Newtype ListProvisioningArtifactsInput _
```

#### `ListProvisioningArtifactsOutput`

``` purescript
newtype ListProvisioningArtifactsOutput
  = ListProvisioningArtifactsOutput { "ProvisioningArtifactDetails" :: NullOrUndefined (ProvisioningArtifactDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListProvisioningArtifactsOutput _
```

#### `ListRecordHistoryInput`

``` purescript
newtype ListRecordHistoryInput
  = ListRecordHistoryInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "SearchFilter" :: NullOrUndefined (ListRecordHistorySearchFilter), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListRecordHistoryInput _
```

#### `ListRecordHistoryOutput`

``` purescript
newtype ListRecordHistoryOutput
  = ListRecordHistoryOutput { "RecordDetails" :: NullOrUndefined (RecordDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListRecordHistoryOutput _
```

#### `ListRecordHistorySearchFilter`

``` purescript
newtype ListRecordHistorySearchFilter
  = ListRecordHistorySearchFilter { "Key" :: NullOrUndefined (SearchFilterKey), "Value" :: NullOrUndefined (SearchFilterValue) }
```

<p>The search filter to use when listing history records.</p>

##### Instances
``` purescript
Newtype ListRecordHistorySearchFilter _
```

#### `ListResourcesForTagOptionInput`

``` purescript
newtype ListResourcesForTagOptionInput
  = ListResourcesForTagOptionInput { "TagOptionId" :: TagOptionId, "ResourceType" :: NullOrUndefined (ResourceType), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListResourcesForTagOptionInput _
```

#### `ListResourcesForTagOptionOutput`

``` purescript
newtype ListResourcesForTagOptionOutput
  = ListResourcesForTagOptionOutput { "ResourceDetails" :: NullOrUndefined (ResourceDetails), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListResourcesForTagOptionOutput _
```

#### `ListTagOptionsFilters`

``` purescript
newtype ListTagOptionsFilters
  = ListTagOptionsFilters { "Key" :: NullOrUndefined (TagOptionKey), "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive) }
```

<p>Filters to use when listing TagOptions.</p>

##### Instances
``` purescript
Newtype ListTagOptionsFilters _
```

#### `ListTagOptionsInput`

``` purescript
newtype ListTagOptionsInput
  = ListTagOptionsInput { "Filters" :: NullOrUndefined (ListTagOptionsFilters), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListTagOptionsInput _
```

#### `ListTagOptionsOutput`

``` purescript
newtype ListTagOptionsOutput
  = ListTagOptionsOutput { "TagOptionDetails" :: NullOrUndefined (TagOptionDetails), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ListTagOptionsOutput _
```

#### `LogicalResourceId`

``` purescript
newtype LogicalResourceId
  = LogicalResourceId String
```

##### Instances
``` purescript
Newtype LogicalResourceId _
```

#### `NoEcho`

``` purescript
newtype NoEcho
  = NoEcho Boolean
```

##### Instances
``` purescript
Newtype NoEcho _
```

#### `NotificationArn`

``` purescript
newtype NotificationArn
  = NotificationArn String
```

##### Instances
``` purescript
Newtype NotificationArn _
```

#### `NotificationArns`

``` purescript
newtype NotificationArns
  = NotificationArns (Array NotificationArn)
```

##### Instances
``` purescript
Newtype NotificationArns _
```

#### `OutputKey`

``` purescript
newtype OutputKey
  = OutputKey String
```

##### Instances
``` purescript
Newtype OutputKey _
```

#### `OutputValue`

``` purescript
newtype OutputValue
  = OutputValue String
```

##### Instances
``` purescript
Newtype OutputValue _
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

##### Instances
``` purescript
Newtype PageSize _
```

#### `PageToken`

``` purescript
newtype PageToken
  = PageToken String
```

##### Instances
``` purescript
Newtype PageToken _
```

#### `ParameterConstraints`

``` purescript
newtype ParameterConstraints
  = ParameterConstraints { "AllowedValues" :: NullOrUndefined (AllowedValues) }
```

<p>The constraints that the administrator has put on the parameter.</p>

##### Instances
``` purescript
Newtype ParameterConstraints _
```

#### `ParameterKey`

``` purescript
newtype ParameterKey
  = ParameterKey String
```

##### Instances
``` purescript
Newtype ParameterKey _
```

#### `ParameterType`

``` purescript
newtype ParameterType
  = ParameterType String
```

##### Instances
``` purescript
Newtype ParameterType _
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

##### Instances
``` purescript
Newtype ParameterValue _
```

#### `PhysicalId`

``` purescript
newtype PhysicalId
  = PhysicalId String
```

##### Instances
``` purescript
Newtype PhysicalId _
```

#### `PhysicalResourceId`

``` purescript
newtype PhysicalResourceId
  = PhysicalResourceId String
```

##### Instances
``` purescript
Newtype PhysicalResourceId _
```

#### `PlanResourceType`

``` purescript
newtype PlanResourceType
  = PlanResourceType String
```

##### Instances
``` purescript
Newtype PlanResourceType _
```

#### `PortfolioDescription`

``` purescript
newtype PortfolioDescription
  = PortfolioDescription String
```

##### Instances
``` purescript
Newtype PortfolioDescription _
```

#### `PortfolioDetail`

``` purescript
newtype PortfolioDetail
  = PortfolioDetail { "Id" :: NullOrUndefined (Id), "ARN" :: NullOrUndefined (ResourceARN), "DisplayName" :: NullOrUndefined (PortfolioDisplayName), "Description" :: NullOrUndefined (PortfolioDescription), "CreatedTime" :: NullOrUndefined (CreationTime), "ProviderName" :: NullOrUndefined (ProviderName) }
```

<p>Information about a portfolio.</p>

##### Instances
``` purescript
Newtype PortfolioDetail _
```

#### `PortfolioDetails`

``` purescript
newtype PortfolioDetails
  = PortfolioDetails (Array PortfolioDetail)
```

##### Instances
``` purescript
Newtype PortfolioDetails _
```

#### `PortfolioDisplayName`

``` purescript
newtype PortfolioDisplayName
  = PortfolioDisplayName String
```

##### Instances
``` purescript
Newtype PortfolioDisplayName _
```

#### `PortfolioName`

``` purescript
newtype PortfolioName
  = PortfolioName String
```

##### Instances
``` purescript
Newtype PortfolioName _
```

#### `Principal`

``` purescript
newtype Principal
  = Principal { "PrincipalARN" :: NullOrUndefined (PrincipalARN), "PrincipalType" :: NullOrUndefined (PrincipalType) }
```

<p>Information about a principal.</p>

##### Instances
``` purescript
Newtype Principal _
```

#### `PrincipalARN`

``` purescript
newtype PrincipalARN
  = PrincipalARN String
```

##### Instances
``` purescript
Newtype PrincipalARN _
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

##### Instances
``` purescript
Newtype PrincipalType _
```

#### `Principals`

``` purescript
newtype Principals
  = Principals (Array Principal)
```

##### Instances
``` purescript
Newtype Principals _
```

#### `ProductArn`

``` purescript
newtype ProductArn
  = ProductArn String
```

##### Instances
``` purescript
Newtype ProductArn _
```

#### `ProductSource`

``` purescript
newtype ProductSource
  = ProductSource String
```

##### Instances
``` purescript
Newtype ProductSource _
```

#### `ProductType`

``` purescript
newtype ProductType
  = ProductType String
```

##### Instances
``` purescript
Newtype ProductType _
```

#### `ProductViewAggregationType`

``` purescript
newtype ProductViewAggregationType
  = ProductViewAggregationType String
```

##### Instances
``` purescript
Newtype ProductViewAggregationType _
```

#### `ProductViewAggregationValue`

``` purescript
newtype ProductViewAggregationValue
  = ProductViewAggregationValue { "Value" :: NullOrUndefined (AttributeValue), "ApproximateCount" :: NullOrUndefined (ApproximateCount) }
```

<p>A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.</p>

##### Instances
``` purescript
Newtype ProductViewAggregationValue _
```

#### `ProductViewAggregationValues`

``` purescript
newtype ProductViewAggregationValues
  = ProductViewAggregationValues (Array ProductViewAggregationValue)
```

##### Instances
``` purescript
Newtype ProductViewAggregationValues _
```

#### `ProductViewAggregations`

``` purescript
newtype ProductViewAggregations
  = ProductViewAggregations (Map ProductViewAggregationType ProductViewAggregationValues)
```

##### Instances
``` purescript
Newtype ProductViewAggregations _
```

#### `ProductViewDetail`

``` purescript
newtype ProductViewDetail
  = ProductViewDetail { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "Status" :: NullOrUndefined (Status), "ProductARN" :: NullOrUndefined (ResourceARN), "CreatedTime" :: NullOrUndefined (CreatedTime) }
```

<p>Information about a product view.</p>

##### Instances
``` purescript
Newtype ProductViewDetail _
```

#### `ProductViewDetails`

``` purescript
newtype ProductViewDetails
  = ProductViewDetails (Array ProductViewDetail)
```

##### Instances
``` purescript
Newtype ProductViewDetails _
```

#### `ProductViewDistributor`

``` purescript
newtype ProductViewDistributor
  = ProductViewDistributor String
```

##### Instances
``` purescript
Newtype ProductViewDistributor _
```

#### `ProductViewFilterBy`

``` purescript
newtype ProductViewFilterBy
  = ProductViewFilterBy String
```

##### Instances
``` purescript
Newtype ProductViewFilterBy _
```

#### `ProductViewFilterValue`

``` purescript
newtype ProductViewFilterValue
  = ProductViewFilterValue String
```

##### Instances
``` purescript
Newtype ProductViewFilterValue _
```

#### `ProductViewFilterValues`

``` purescript
newtype ProductViewFilterValues
  = ProductViewFilterValues (Array ProductViewFilterValue)
```

##### Instances
``` purescript
Newtype ProductViewFilterValues _
```

#### `ProductViewFilters`

``` purescript
newtype ProductViewFilters
  = ProductViewFilters (Map ProductViewFilterBy ProductViewFilterValues)
```

##### Instances
``` purescript
Newtype ProductViewFilters _
```

#### `ProductViewName`

``` purescript
newtype ProductViewName
  = ProductViewName String
```

##### Instances
``` purescript
Newtype ProductViewName _
```

#### `ProductViewOwner`

``` purescript
newtype ProductViewOwner
  = ProductViewOwner String
```

##### Instances
``` purescript
Newtype ProductViewOwner _
```

#### `ProductViewShortDescription`

``` purescript
newtype ProductViewShortDescription
  = ProductViewShortDescription String
```

##### Instances
``` purescript
Newtype ProductViewShortDescription _
```

#### `ProductViewSortBy`

``` purescript
newtype ProductViewSortBy
  = ProductViewSortBy String
```

##### Instances
``` purescript
Newtype ProductViewSortBy _
```

#### `ProductViewSummaries`

``` purescript
newtype ProductViewSummaries
  = ProductViewSummaries (Array ProductViewSummary)
```

##### Instances
``` purescript
Newtype ProductViewSummaries _
```

#### `ProductViewSummary`

``` purescript
newtype ProductViewSummary
  = ProductViewSummary { "Id" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProductViewName), "Owner" :: NullOrUndefined (ProductViewOwner), "ShortDescription" :: NullOrUndefined (ProductViewShortDescription), "Type" :: NullOrUndefined (ProductType), "Distributor" :: NullOrUndefined (ProductViewDistributor), "HasDefaultPath" :: NullOrUndefined (HasDefaultPath), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportUrl" :: NullOrUndefined (SupportUrl) }
```

<p>Summary information about a product view.</p>

##### Instances
``` purescript
Newtype ProductViewSummary _
```

#### `PropertyName`

``` purescript
newtype PropertyName
  = PropertyName String
```

##### Instances
``` purescript
Newtype PropertyName _
```

#### `ProviderName`

``` purescript
newtype ProviderName
  = ProviderName String
```

##### Instances
``` purescript
Newtype ProviderName _
```

#### `ProvisionProductInput`

``` purescript
newtype ProvisionProductInput
  = ProvisionProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "PathId" :: NullOrUndefined (Id), "ProvisionedProductName" :: ProvisionedProductName, "ProvisioningParameters" :: NullOrUndefined (ProvisioningParameters), "Tags" :: NullOrUndefined (Tags), "NotificationArns" :: NullOrUndefined (NotificationArns), "ProvisionToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype ProvisionProductInput _
```

#### `ProvisionProductOutput`

``` purescript
newtype ProvisionProductOutput
  = ProvisionProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

##### Instances
``` purescript
Newtype ProvisionProductOutput _
```

#### `ProvisionedProductAttribute`

``` purescript
newtype ProvisionedProductAttribute
  = ProvisionedProductAttribute { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn), "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn), "Type" :: NullOrUndefined (ProvisionedProductType), "Id" :: NullOrUndefined (Id), "Status" :: NullOrUndefined (ProvisionedProductStatus), "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage), "CreatedTime" :: NullOrUndefined (CreatedTime), "IdempotencyToken" :: NullOrUndefined (IdempotencyToken), "LastRecordId" :: NullOrUndefined (Id), "Tags" :: NullOrUndefined (Tags), "PhysicalId" :: NullOrUndefined (PhysicalId), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "UserArn" :: NullOrUndefined (UserArn), "UserArnSession" :: NullOrUndefined (UserArnSession) }
```

<p>Information about a provisioned product.</p>

##### Instances
``` purescript
Newtype ProvisionedProductAttribute _
```

#### `ProvisionedProductAttributes`

``` purescript
newtype ProvisionedProductAttributes
  = ProvisionedProductAttributes (Array ProvisionedProductAttribute)
```

##### Instances
``` purescript
Newtype ProvisionedProductAttributes _
```

#### `ProvisionedProductDetail`

``` purescript
newtype ProvisionedProductDetail
  = ProvisionedProductDetail { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn), "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn), "Type" :: NullOrUndefined (ProvisionedProductType), "Id" :: NullOrUndefined (ProvisionedProductId), "Status" :: NullOrUndefined (ProvisionedProductStatus), "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage), "CreatedTime" :: NullOrUndefined (CreatedTime), "IdempotencyToken" :: NullOrUndefined (IdempotencyToken), "LastRecordId" :: NullOrUndefined (LastRequestId) }
```

<p>Information about a provisioned product.</p>

##### Instances
``` purescript
Newtype ProvisionedProductDetail _
```

#### `ProvisionedProductDetails`

``` purescript
newtype ProvisionedProductDetails
  = ProvisionedProductDetails (Array ProvisionedProductDetail)
```

##### Instances
``` purescript
Newtype ProvisionedProductDetails _
```

#### `ProvisionedProductFilters`

``` purescript
newtype ProvisionedProductFilters
  = ProvisionedProductFilters (Map ProvisionedProductViewFilterBy ProvisionedProductViewFilterValues)
```

##### Instances
``` purescript
Newtype ProvisionedProductFilters _
```

#### `ProvisionedProductId`

``` purescript
newtype ProvisionedProductId
  = ProvisionedProductId String
```

##### Instances
``` purescript
Newtype ProvisionedProductId _
```

#### `ProvisionedProductName`

``` purescript
newtype ProvisionedProductName
  = ProvisionedProductName String
```

##### Instances
``` purescript
Newtype ProvisionedProductName _
```

#### `ProvisionedProductNameOrArn`

``` purescript
newtype ProvisionedProductNameOrArn
  = ProvisionedProductNameOrArn String
```

##### Instances
``` purescript
Newtype ProvisionedProductNameOrArn _
```

#### `ProvisionedProductPlanDetails`

``` purescript
newtype ProvisionedProductPlanDetails
  = ProvisionedProductPlanDetails { "CreatedTime" :: NullOrUndefined (CreatedTime), "PathId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName), "PlanType" :: NullOrUndefined (ProvisionedProductPlanType), "ProvisioningArtifactId" :: NullOrUndefined (Id), "Status" :: NullOrUndefined (ProvisionedProductPlanStatus), "UpdatedTime" :: NullOrUndefined (UpdatedTime), "NotificationArns" :: NullOrUndefined (NotificationArns), "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "Tags" :: NullOrUndefined (Tags), "StatusMessage" :: NullOrUndefined (StatusMessage) }
```

<p>Information about a plan.</p>

##### Instances
``` purescript
Newtype ProvisionedProductPlanDetails _
```

#### `ProvisionedProductPlanName`

``` purescript
newtype ProvisionedProductPlanName
  = ProvisionedProductPlanName String
```

##### Instances
``` purescript
Newtype ProvisionedProductPlanName _
```

#### `ProvisionedProductPlanStatus`

``` purescript
newtype ProvisionedProductPlanStatus
  = ProvisionedProductPlanStatus String
```

##### Instances
``` purescript
Newtype ProvisionedProductPlanStatus _
```

#### `ProvisionedProductPlanSummary`

``` purescript
newtype ProvisionedProductPlanSummary
  = ProvisionedProductPlanSummary { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName), "PlanType" :: NullOrUndefined (ProvisionedProductPlanType), "ProvisioningArtifactId" :: NullOrUndefined (Id) }
```

<p>Summary information about a plan.</p>

##### Instances
``` purescript
Newtype ProvisionedProductPlanSummary _
```

#### `ProvisionedProductPlanType`

``` purescript
newtype ProvisionedProductPlanType
  = ProvisionedProductPlanType String
```

##### Instances
``` purescript
Newtype ProvisionedProductPlanType _
```

#### `ProvisionedProductPlans`

``` purescript
newtype ProvisionedProductPlans
  = ProvisionedProductPlans (Array ProvisionedProductPlanSummary)
```

##### Instances
``` purescript
Newtype ProvisionedProductPlans _
```

#### `ProvisionedProductStatus`

``` purescript
newtype ProvisionedProductStatus
  = ProvisionedProductStatus String
```

##### Instances
``` purescript
Newtype ProvisionedProductStatus _
```

#### `ProvisionedProductStatusMessage`

``` purescript
newtype ProvisionedProductStatusMessage
  = ProvisionedProductStatusMessage String
```

##### Instances
``` purescript
Newtype ProvisionedProductStatusMessage _
```

#### `ProvisionedProductType`

``` purescript
newtype ProvisionedProductType
  = ProvisionedProductType String
```

##### Instances
``` purescript
Newtype ProvisionedProductType _
```

#### `ProvisionedProductViewFilterBy`

``` purescript
newtype ProvisionedProductViewFilterBy
  = ProvisionedProductViewFilterBy String
```

##### Instances
``` purescript
Newtype ProvisionedProductViewFilterBy _
```

#### `ProvisionedProductViewFilterValue`

``` purescript
newtype ProvisionedProductViewFilterValue
  = ProvisionedProductViewFilterValue String
```

##### Instances
``` purescript
Newtype ProvisionedProductViewFilterValue _
```

#### `ProvisionedProductViewFilterValues`

``` purescript
newtype ProvisionedProductViewFilterValues
  = ProvisionedProductViewFilterValues (Array ProvisionedProductViewFilterValue)
```

##### Instances
``` purescript
Newtype ProvisionedProductViewFilterValues _
```

#### `ProvisioningArtifact`

``` purescript
newtype ProvisioningArtifact
  = ProvisioningArtifact { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime) }
```

<p>Information about a provisioning artifact. A provisioning artifact is also known as a product version.</p>

##### Instances
``` purescript
Newtype ProvisioningArtifact _
```

#### `ProvisioningArtifactActive`

``` purescript
newtype ProvisioningArtifactActive
  = ProvisioningArtifactActive Boolean
```

##### Instances
``` purescript
Newtype ProvisioningArtifactActive _
```

#### `ProvisioningArtifactCreatedTime`

``` purescript
newtype ProvisioningArtifactCreatedTime
  = ProvisioningArtifactCreatedTime Number
```

##### Instances
``` purescript
Newtype ProvisioningArtifactCreatedTime _
```

#### `ProvisioningArtifactDescription`

``` purescript
newtype ProvisioningArtifactDescription
  = ProvisioningArtifactDescription String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactDescription _
```

#### `ProvisioningArtifactDetail`

``` purescript
newtype ProvisioningArtifactDetail
  = ProvisioningArtifactDetail { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactName), "Type" :: NullOrUndefined (ProvisioningArtifactType), "CreatedTime" :: NullOrUndefined (CreationTime), "Active" :: NullOrUndefined (ProvisioningArtifactActive) }
```

<p>Information about a provisioning artifact (also known as a version) for a product.</p>

##### Instances
``` purescript
Newtype ProvisioningArtifactDetail _
```

#### `ProvisioningArtifactDetails`

``` purescript
newtype ProvisioningArtifactDetails
  = ProvisioningArtifactDetails (Array ProvisioningArtifactDetail)
```

##### Instances
``` purescript
Newtype ProvisioningArtifactDetails _
```

#### `ProvisioningArtifactInfo`

``` purescript
newtype ProvisioningArtifactInfo
  = ProvisioningArtifactInfo (Map ProvisioningArtifactInfoKey ProvisioningArtifactInfoValue)
```

##### Instances
``` purescript
Newtype ProvisioningArtifactInfo _
```

#### `ProvisioningArtifactInfoKey`

``` purescript
newtype ProvisioningArtifactInfoKey
  = ProvisioningArtifactInfoKey String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactInfoKey _
```

#### `ProvisioningArtifactInfoValue`

``` purescript
newtype ProvisioningArtifactInfoValue
  = ProvisioningArtifactInfoValue String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactInfoValue _
```

#### `ProvisioningArtifactName`

``` purescript
newtype ProvisioningArtifactName
  = ProvisioningArtifactName String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactName _
```

#### `ProvisioningArtifactParameter`

``` purescript
newtype ProvisioningArtifactParameter
  = ProvisioningArtifactParameter { "ParameterKey" :: NullOrUndefined (ParameterKey), "DefaultValue" :: NullOrUndefined (DefaultValue), "ParameterType" :: NullOrUndefined (ParameterType), "IsNoEcho" :: NullOrUndefined (NoEcho), "Description" :: NullOrUndefined (Description), "ParameterConstraints" :: NullOrUndefined (ParameterConstraints) }
```

<p>Information about a parameter used to provision a product.</p>

##### Instances
``` purescript
Newtype ProvisioningArtifactParameter _
```

#### `ProvisioningArtifactParameters`

``` purescript
newtype ProvisioningArtifactParameters
  = ProvisioningArtifactParameters (Array ProvisioningArtifactParameter)
```

##### Instances
``` purescript
Newtype ProvisioningArtifactParameters _
```

#### `ProvisioningArtifactProperties`

``` purescript
newtype ProvisioningArtifactProperties
  = ProvisioningArtifactProperties { "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "Info" :: ProvisioningArtifactInfo, "Type" :: NullOrUndefined (ProvisioningArtifactType) }
```

<p>Information about a provisioning artifact (also known as a version) for a product.</p>

##### Instances
``` purescript
Newtype ProvisioningArtifactProperties _
```

#### `ProvisioningArtifactPropertyName`

``` purescript
newtype ProvisioningArtifactPropertyName
  = ProvisioningArtifactPropertyName String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactPropertyName _
```

#### `ProvisioningArtifactPropertyValue`

``` purescript
newtype ProvisioningArtifactPropertyValue
  = ProvisioningArtifactPropertyValue String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactPropertyValue _
```

#### `ProvisioningArtifactSummaries`

``` purescript
newtype ProvisioningArtifactSummaries
  = ProvisioningArtifactSummaries (Array ProvisioningArtifactSummary)
```

##### Instances
``` purescript
Newtype ProvisioningArtifactSummaries _
```

#### `ProvisioningArtifactSummary`

``` purescript
newtype ProvisioningArtifactSummary
  = ProvisioningArtifactSummary { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime), "ProvisioningArtifactMetadata" :: NullOrUndefined (ProvisioningArtifactInfo) }
```

<p>Summary information about a provisioning artifact (also known as a version) for a product.</p>

##### Instances
``` purescript
Newtype ProvisioningArtifactSummary _
```

#### `ProvisioningArtifactType`

``` purescript
newtype ProvisioningArtifactType
  = ProvisioningArtifactType String
```

##### Instances
``` purescript
Newtype ProvisioningArtifactType _
```

#### `ProvisioningArtifacts`

``` purescript
newtype ProvisioningArtifacts
  = ProvisioningArtifacts (Array ProvisioningArtifact)
```

##### Instances
``` purescript
Newtype ProvisioningArtifacts _
```

#### `ProvisioningParameter`

``` purescript
newtype ProvisioningParameter
  = ProvisioningParameter { "Key" :: NullOrUndefined (ParameterKey), "Value" :: NullOrUndefined (ParameterValue) }
```

<p>Information about a parameter used to provision a product.</p>

##### Instances
``` purescript
Newtype ProvisioningParameter _
```

#### `ProvisioningParameters`

``` purescript
newtype ProvisioningParameters
  = ProvisioningParameters (Array ProvisioningParameter)
```

##### Instances
``` purescript
Newtype ProvisioningParameters _
```

#### `RecordDetail`

``` purescript
newtype RecordDetail
  = RecordDetail { "RecordId" :: NullOrUndefined (Id), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName), "Status" :: NullOrUndefined (RecordStatus), "CreatedTime" :: NullOrUndefined (CreatedTime), "UpdatedTime" :: NullOrUndefined (UpdatedTime), "ProvisionedProductType" :: NullOrUndefined (ProvisionedProductType), "RecordType" :: NullOrUndefined (RecordType), "ProvisionedProductId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "PathId" :: NullOrUndefined (Id), "RecordErrors" :: NullOrUndefined (RecordErrors), "RecordTags" :: NullOrUndefined (RecordTags) }
```

<p>Information about a request operation.</p>

##### Instances
``` purescript
Newtype RecordDetail _
```

#### `RecordDetails`

``` purescript
newtype RecordDetails
  = RecordDetails (Array RecordDetail)
```

##### Instances
``` purescript
Newtype RecordDetails _
```

#### `RecordError`

``` purescript
newtype RecordError
  = RecordError { "Code" :: NullOrUndefined (ErrorCode), "Description" :: NullOrUndefined (ErrorDescription) }
```

<p>The error code and description resulting from an operation.</p>

##### Instances
``` purescript
Newtype RecordError _
```

#### `RecordErrors`

``` purescript
newtype RecordErrors
  = RecordErrors (Array RecordError)
```

##### Instances
``` purescript
Newtype RecordErrors _
```

#### `RecordOutput`

``` purescript
newtype RecordOutput
  = RecordOutput { "OutputKey" :: NullOrUndefined (OutputKey), "OutputValue" :: NullOrUndefined (OutputValue), "Description" :: NullOrUndefined (Description) }
```

<p>The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.</p>

##### Instances
``` purescript
Newtype RecordOutput _
```

#### `RecordOutputs`

``` purescript
newtype RecordOutputs
  = RecordOutputs (Array RecordOutput)
```

##### Instances
``` purescript
Newtype RecordOutputs _
```

#### `RecordStatus`

``` purescript
newtype RecordStatus
  = RecordStatus String
```

##### Instances
``` purescript
Newtype RecordStatus _
```

#### `RecordTag`

``` purescript
newtype RecordTag
  = RecordTag { "Key" :: NullOrUndefined (RecordTagKey), "Value" :: NullOrUndefined (RecordTagValue) }
```

<p>Information about a tag, which is a key-value pair.</p>

##### Instances
``` purescript
Newtype RecordTag _
```

#### `RecordTagKey`

``` purescript
newtype RecordTagKey
  = RecordTagKey String
```

##### Instances
``` purescript
Newtype RecordTagKey _
```

#### `RecordTagValue`

``` purescript
newtype RecordTagValue
  = RecordTagValue String
```

##### Instances
``` purescript
Newtype RecordTagValue _
```

#### `RecordTags`

``` purescript
newtype RecordTags
  = RecordTags (Array RecordTag)
```

##### Instances
``` purescript
Newtype RecordTags _
```

#### `RecordType`

``` purescript
newtype RecordType
  = RecordType String
```

##### Instances
``` purescript
Newtype RecordType _
```

#### `RejectPortfolioShareInput`

``` purescript
newtype RejectPortfolioShareInput
  = RejectPortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

##### Instances
``` purescript
Newtype RejectPortfolioShareInput _
```

#### `RejectPortfolioShareOutput`

``` purescript
newtype RejectPortfolioShareOutput
  = RejectPortfolioShareOutput {  }
```

##### Instances
``` purescript
Newtype RejectPortfolioShareOutput _
```

#### `Replacement`

``` purescript
newtype Replacement
  = Replacement String
```

##### Instances
``` purescript
Newtype Replacement _
```

#### `RequiresRecreation`

``` purescript
newtype RequiresRecreation
  = RequiresRecreation String
```

##### Instances
``` purescript
Newtype RequiresRecreation _
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

##### Instances
``` purescript
Newtype ResourceARN _
```

#### `ResourceAttribute`

``` purescript
newtype ResourceAttribute
  = ResourceAttribute String
```

##### Instances
``` purescript
Newtype ResourceAttribute _
```

#### `ResourceChange`

``` purescript
newtype ResourceChange
  = ResourceChange { "Action" :: NullOrUndefined (ChangeAction), "LogicalResourceId" :: NullOrUndefined (LogicalResourceId), "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: NullOrUndefined (PlanResourceType), "Replacement" :: NullOrUndefined (Replacement), "Scope" :: NullOrUndefined (Scope), "Details" :: NullOrUndefined (ResourceChangeDetails) }
```

<p>Information about a resource change that will occur when a plan is executed.</p>

##### Instances
``` purescript
Newtype ResourceChange _
```

#### `ResourceChangeDetail`

``` purescript
newtype ResourceChangeDetail
  = ResourceChangeDetail { "Target" :: NullOrUndefined (ResourceTargetDefinition), "Evaluation" :: NullOrUndefined (EvaluationType), "CausingEntity" :: NullOrUndefined (CausingEntity) }
```

<p>Information about a change to a resource attribute.</p>

##### Instances
``` purescript
Newtype ResourceChangeDetail _
```

#### `ResourceChangeDetails`

``` purescript
newtype ResourceChangeDetails
  = ResourceChangeDetails (Array ResourceChangeDetail)
```

##### Instances
``` purescript
Newtype ResourceChangeDetails _
```

#### `ResourceChanges`

``` purescript
newtype ResourceChanges
  = ResourceChanges (Array ResourceChange)
```

##### Instances
``` purescript
Newtype ResourceChanges _
```

#### `ResourceDetail`

``` purescript
newtype ResourceDetail
  = ResourceDetail { "Id" :: NullOrUndefined (ResourceDetailId), "ARN" :: NullOrUndefined (ResourceDetailARN), "Name" :: NullOrUndefined (ResourceDetailName), "Description" :: NullOrUndefined (ResourceDetailDescription), "CreatedTime" :: NullOrUndefined (ResourceDetailCreatedTime) }
```

<p>Information about a resource.</p>

##### Instances
``` purescript
Newtype ResourceDetail _
```

#### `ResourceDetailARN`

``` purescript
newtype ResourceDetailARN
  = ResourceDetailARN String
```

##### Instances
``` purescript
Newtype ResourceDetailARN _
```

#### `ResourceDetailCreatedTime`

``` purescript
newtype ResourceDetailCreatedTime
  = ResourceDetailCreatedTime Number
```

##### Instances
``` purescript
Newtype ResourceDetailCreatedTime _
```

#### `ResourceDetailDescription`

``` purescript
newtype ResourceDetailDescription
  = ResourceDetailDescription String
```

##### Instances
``` purescript
Newtype ResourceDetailDescription _
```

#### `ResourceDetailId`

``` purescript
newtype ResourceDetailId
  = ResourceDetailId String
```

##### Instances
``` purescript
Newtype ResourceDetailId _
```

#### `ResourceDetailName`

``` purescript
newtype ResourceDetailName
  = ResourceDetailName String
```

##### Instances
``` purescript
Newtype ResourceDetailName _
```

#### `ResourceDetails`

``` purescript
newtype ResourceDetails
  = ResourceDetails (Array ResourceDetail)
```

##### Instances
``` purescript
Newtype ResourceDetails _
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

##### Instances
``` purescript
Newtype ResourceId _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException {  }
```

<p>A resource that is currently in use. Ensure that the resource is not in use and retry the operation.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The specified resource was not found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceTargetDefinition`

``` purescript
newtype ResourceTargetDefinition
  = ResourceTargetDefinition { "Attribute" :: NullOrUndefined (ResourceAttribute), "Name" :: NullOrUndefined (PropertyName), "RequiresRecreation" :: NullOrUndefined (RequiresRecreation) }
```

<p>Information about a change to a resource attribute.</p>

##### Instances
``` purescript
Newtype ResourceTargetDefinition _
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

##### Instances
``` purescript
Newtype ResourceType _
```

#### `ScanProvisionedProductsInput`

``` purescript
newtype ScanProvisionedProductsInput
  = ScanProvisionedProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ScanProvisionedProductsInput _
```

#### `ScanProvisionedProductsOutput`

``` purescript
newtype ScanProvisionedProductsOutput
  = ScanProvisionedProductsOutput { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype ScanProvisionedProductsOutput _
```

#### `Scope`

``` purescript
newtype Scope
  = Scope (Array ResourceAttribute)
```

##### Instances
``` purescript
Newtype Scope _
```

#### `SearchFilterKey`

``` purescript
newtype SearchFilterKey
  = SearchFilterKey String
```

##### Instances
``` purescript
Newtype SearchFilterKey _
```

#### `SearchFilterValue`

``` purescript
newtype SearchFilterValue
  = SearchFilterValue String
```

##### Instances
``` purescript
Newtype SearchFilterValue _
```

#### `SearchProductsAsAdminInput`

``` purescript
newtype SearchProductsAsAdminInput
  = SearchProductsAsAdminInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: NullOrUndefined (Id), "Filters" :: NullOrUndefined (ProductViewFilters), "SortBy" :: NullOrUndefined (ProductViewSortBy), "SortOrder" :: NullOrUndefined (SortOrder), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize), "ProductSource" :: NullOrUndefined (ProductSource) }
```

##### Instances
``` purescript
Newtype SearchProductsAsAdminInput _
```

#### `SearchProductsAsAdminOutput`

``` purescript
newtype SearchProductsAsAdminOutput
  = SearchProductsAsAdminOutput { "ProductViewDetails" :: NullOrUndefined (ProductViewDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype SearchProductsAsAdminOutput _
```

#### `SearchProductsInput`

``` purescript
newtype SearchProductsInput
  = SearchProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Filters" :: NullOrUndefined (ProductViewFilters), "PageSize" :: NullOrUndefined (PageSize), "SortBy" :: NullOrUndefined (ProductViewSortBy), "SortOrder" :: NullOrUndefined (SortOrder), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype SearchProductsInput _
```

#### `SearchProductsOutput`

``` purescript
newtype SearchProductsOutput
  = SearchProductsOutput { "ProductViewSummaries" :: NullOrUndefined (ProductViewSummaries), "ProductViewAggregations" :: NullOrUndefined (ProductViewAggregations), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype SearchProductsOutput _
```

#### `SearchProvisionedProductsInput`

``` purescript
newtype SearchProvisionedProductsInput
  = SearchProvisionedProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "Filters" :: NullOrUndefined (ProvisionedProductFilters), "SortBy" :: NullOrUndefined (SortField), "SortOrder" :: NullOrUndefined (SortOrder), "PageSize" :: NullOrUndefined (SearchProvisionedProductsPageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype SearchProvisionedProductsInput _
```

#### `SearchProvisionedProductsOutput`

``` purescript
newtype SearchProvisionedProductsOutput
  = SearchProvisionedProductsOutput { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductAttributes), "TotalResultsCount" :: NullOrUndefined (TotalResultsCount), "NextPageToken" :: NullOrUndefined (PageToken) }
```

##### Instances
``` purescript
Newtype SearchProvisionedProductsOutput _
```

#### `SearchProvisionedProductsPageSize`

``` purescript
newtype SearchProvisionedProductsPageSize
  = SearchProvisionedProductsPageSize Int
```

##### Instances
``` purescript
Newtype SearchProvisionedProductsPageSize _
```

#### `SortField`

``` purescript
newtype SortField
  = SortField String
```

##### Instances
``` purescript
Newtype SortField _
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

##### Instances
``` purescript
Newtype SortOrder _
```

#### `SourceProvisioningArtifactProperties`

``` purescript
newtype SourceProvisioningArtifactProperties
  = SourceProvisioningArtifactProperties (Array SourceProvisioningArtifactPropertiesMap)
```

##### Instances
``` purescript
Newtype SourceProvisioningArtifactProperties _
```

#### `SourceProvisioningArtifactPropertiesMap`

``` purescript
newtype SourceProvisioningArtifactPropertiesMap
  = SourceProvisioningArtifactPropertiesMap (Map ProvisioningArtifactPropertyName ProvisioningArtifactPropertyValue)
```

##### Instances
``` purescript
Newtype SourceProvisioningArtifactPropertiesMap _
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

##### Instances
``` purescript
Newtype Status _
```

#### `StatusDetail`

``` purescript
newtype StatusDetail
  = StatusDetail String
```

##### Instances
``` purescript
Newtype StatusDetail _
```

#### `StatusMessage`

``` purescript
newtype StatusMessage
  = StatusMessage String
```

##### Instances
``` purescript
Newtype StatusMessage _
```

#### `SupportDescription`

``` purescript
newtype SupportDescription
  = SupportDescription String
```

##### Instances
``` purescript
Newtype SupportDescription _
```

#### `SupportEmail`

``` purescript
newtype SupportEmail
  = SupportEmail String
```

##### Instances
``` purescript
Newtype SupportEmail _
```

#### `SupportUrl`

``` purescript
newtype SupportUrl
  = SupportUrl String
```

##### Instances
``` purescript
Newtype SupportUrl _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Information about a tag. A tag is a key-value pair. Tags are propagated to the resources created when provisioning a product.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeys _
```

#### `TagOptionActive`

``` purescript
newtype TagOptionActive
  = TagOptionActive Boolean
```

##### Instances
``` purescript
Newtype TagOptionActive _
```

#### `TagOptionDetail`

``` purescript
newtype TagOptionDetail
  = TagOptionDetail { "Key" :: NullOrUndefined (TagOptionKey), "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive), "Id" :: NullOrUndefined (TagOptionId) }
```

<p>Information about a TagOption.</p>

##### Instances
``` purescript
Newtype TagOptionDetail _
```

#### `TagOptionDetails`

``` purescript
newtype TagOptionDetails
  = TagOptionDetails (Array TagOptionDetail)
```

##### Instances
``` purescript
Newtype TagOptionDetails _
```

#### `TagOptionId`

``` purescript
newtype TagOptionId
  = TagOptionId String
```

##### Instances
``` purescript
Newtype TagOptionId _
```

#### `TagOptionKey`

``` purescript
newtype TagOptionKey
  = TagOptionKey String
```

##### Instances
``` purescript
Newtype TagOptionKey _
```

#### `TagOptionNotMigratedException`

``` purescript
newtype TagOptionNotMigratedException
  = TagOptionNotMigratedException {  }
```

<p>An operation requiring TagOptions failed because the TagOptions migration process has not been performed for this account. Please use the AWS console to perform the migration process before retrying the operation.</p>

##### Instances
``` purescript
Newtype TagOptionNotMigratedException _
```

#### `TagOptionSummaries`

``` purescript
newtype TagOptionSummaries
  = TagOptionSummaries (Array TagOptionSummary)
```

##### Instances
``` purescript
Newtype TagOptionSummaries _
```

#### `TagOptionSummary`

``` purescript
newtype TagOptionSummary
  = TagOptionSummary { "Key" :: NullOrUndefined (TagOptionKey), "Values" :: NullOrUndefined (TagOptionValues) }
```

<p>Summary information about a TagOption.</p>

##### Instances
``` purescript
Newtype TagOptionSummary _
```

#### `TagOptionValue`

``` purescript
newtype TagOptionValue
  = TagOptionValue String
```

##### Instances
``` purescript
Newtype TagOptionValue _
```

#### `TagOptionValues`

``` purescript
newtype TagOptionValues
  = TagOptionValues (Array TagOptionValue)
```

##### Instances
``` purescript
Newtype TagOptionValues _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TerminateProvisionedProductInput`

``` purescript
newtype TerminateProvisionedProductInput
  = TerminateProvisionedProductInput { "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn), "ProvisionedProductId" :: NullOrUndefined (Id), "TerminateToken" :: IdempotencyToken, "IgnoreErrors" :: NullOrUndefined (IgnoreErrors), "AcceptLanguage" :: NullOrUndefined (AcceptLanguage) }
```

##### Instances
``` purescript
Newtype TerminateProvisionedProductInput _
```

#### `TerminateProvisionedProductOutput`

``` purescript
newtype TerminateProvisionedProductOutput
  = TerminateProvisionedProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

##### Instances
``` purescript
Newtype TerminateProvisionedProductOutput _
```

#### `TotalResultsCount`

``` purescript
newtype TotalResultsCount
  = TotalResultsCount Int
```

##### Instances
``` purescript
Newtype TotalResultsCount _
```

#### `UpdateConstraintInput`

``` purescript
newtype UpdateConstraintInput
  = UpdateConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "Description" :: NullOrUndefined (ConstraintDescription) }
```

##### Instances
``` purescript
Newtype UpdateConstraintInput _
```

#### `UpdateConstraintOutput`

``` purescript
newtype UpdateConstraintOutput
  = UpdateConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype UpdateConstraintOutput _
```

#### `UpdatePortfolioInput`

``` purescript
newtype UpdatePortfolioInput
  = UpdatePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "DisplayName" :: NullOrUndefined (PortfolioDisplayName), "Description" :: NullOrUndefined (PortfolioDescription), "ProviderName" :: NullOrUndefined (ProviderName), "AddTags" :: NullOrUndefined (AddTags), "RemoveTags" :: NullOrUndefined (TagKeys) }
```

##### Instances
``` purescript
Newtype UpdatePortfolioInput _
```

#### `UpdatePortfolioOutput`

``` purescript
newtype UpdatePortfolioOutput
  = UpdatePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype UpdatePortfolioOutput _
```

#### `UpdateProductInput`

``` purescript
newtype UpdateProductInput
  = UpdateProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "Name" :: NullOrUndefined (ProductViewName), "Owner" :: NullOrUndefined (ProductViewOwner), "Description" :: NullOrUndefined (ProductViewShortDescription), "Distributor" :: NullOrUndefined (ProductViewOwner), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportUrl" :: NullOrUndefined (SupportUrl), "AddTags" :: NullOrUndefined (AddTags), "RemoveTags" :: NullOrUndefined (TagKeys) }
```

##### Instances
``` purescript
Newtype UpdateProductInput _
```

#### `UpdateProductOutput`

``` purescript
newtype UpdateProductOutput
  = UpdateProductOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype UpdateProductOutput _
```

#### `UpdateProvisionedProductInput`

``` purescript
newtype UpdateProvisionedProductInput
  = UpdateProvisionedProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn), "ProvisionedProductId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "PathId" :: NullOrUndefined (Id), "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "UpdateToken" :: IdempotencyToken }
```

##### Instances
``` purescript
Newtype UpdateProvisionedProductInput _
```

#### `UpdateProvisionedProductOutput`

``` purescript
newtype UpdateProvisionedProductOutput
  = UpdateProvisionedProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

##### Instances
``` purescript
Newtype UpdateProvisionedProductOutput _
```

#### `UpdateProvisioningArtifactInput`

``` purescript
newtype UpdateProvisioningArtifactInput
  = UpdateProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "Active" :: NullOrUndefined (ProvisioningArtifactActive) }
```

##### Instances
``` purescript
Newtype UpdateProvisioningArtifactInput _
```

#### `UpdateProvisioningArtifactOutput`

``` purescript
newtype UpdateProvisioningArtifactOutput
  = UpdateProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

##### Instances
``` purescript
Newtype UpdateProvisioningArtifactOutput _
```

#### `UpdateProvisioningParameter`

``` purescript
newtype UpdateProvisioningParameter
  = UpdateProvisioningParameter { "Key" :: NullOrUndefined (ParameterKey), "Value" :: NullOrUndefined (ParameterValue), "UsePreviousValue" :: NullOrUndefined (UsePreviousValue) }
```

<p>The parameter key-value pair used to update a provisioned product.</p>

##### Instances
``` purescript
Newtype UpdateProvisioningParameter _
```

#### `UpdateProvisioningParameters`

``` purescript
newtype UpdateProvisioningParameters
  = UpdateProvisioningParameters (Array UpdateProvisioningParameter)
```

##### Instances
``` purescript
Newtype UpdateProvisioningParameters _
```

#### `UpdateTagOptionInput`

``` purescript
newtype UpdateTagOptionInput
  = UpdateTagOptionInput { "Id" :: TagOptionId, "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive) }
```

##### Instances
``` purescript
Newtype UpdateTagOptionInput _
```

#### `UpdateTagOptionOutput`

``` purescript
newtype UpdateTagOptionOutput
  = UpdateTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

##### Instances
``` purescript
Newtype UpdateTagOptionOutput _
```

#### `UpdatedTime`

``` purescript
newtype UpdatedTime
  = UpdatedTime Number
```

##### Instances
``` purescript
Newtype UpdatedTime _
```

#### `UsageInstruction`

``` purescript
newtype UsageInstruction
  = UsageInstruction { "Type" :: NullOrUndefined (InstructionType), "Value" :: NullOrUndefined (InstructionValue) }
```

<p>Additional information provided by the administrator.</p>

##### Instances
``` purescript
Newtype UsageInstruction _
```

#### `UsageInstructions`

``` purescript
newtype UsageInstructions
  = UsageInstructions (Array UsageInstruction)
```

##### Instances
``` purescript
Newtype UsageInstructions _
```

#### `UsePreviousValue`

``` purescript
newtype UsePreviousValue
  = UsePreviousValue Boolean
```

##### Instances
``` purescript
Newtype UsePreviousValue _
```

#### `UserArn`

``` purescript
newtype UserArn
  = UserArn String
```

##### Instances
``` purescript
Newtype UserArn _
```

#### `UserArnSession`

``` purescript
newtype UserArnSession
  = UserArnSession String
```

##### Instances
``` purescript
Newtype UserArnSession _
```

#### `Verbose`

``` purescript
newtype Verbose
  = Verbose Boolean
```

##### Instances
``` purescript
Newtype Verbose _
```


