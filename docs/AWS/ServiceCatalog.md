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

#### `AcceptPortfolioShareInput`

``` purescript
newtype AcceptPortfolioShareInput
  = AcceptPortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

#### `AcceptPortfolioShareOutput`

``` purescript
newtype AcceptPortfolioShareOutput
  = AcceptPortfolioShareOutput {  }
```

#### `AccessLevelFilter`

``` purescript
newtype AccessLevelFilter
  = AccessLevelFilter { "Key" :: NullOrUndefined (AccessLevelFilterKey), "Value" :: NullOrUndefined (AccessLevelFilterValue) }
```

<p>The access level to use to filter results.</p>

#### `AccessLevelFilterKey`

``` purescript
newtype AccessLevelFilterKey
  = AccessLevelFilterKey String
```

#### `AccessLevelFilterValue`

``` purescript
newtype AccessLevelFilterValue
  = AccessLevelFilterValue String
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

#### `AccountIds`

``` purescript
newtype AccountIds
  = AccountIds (Array AccountId)
```

#### `AddTags`

``` purescript
newtype AddTags
  = AddTags (Array Tag)
```

#### `AllowedValue`

``` purescript
newtype AllowedValue
  = AllowedValue String
```

#### `AllowedValues`

``` purescript
newtype AllowedValues
  = AllowedValues (Array AllowedValue)
```

#### `ApproximateCount`

``` purescript
newtype ApproximateCount
  = ApproximateCount Int
```

#### `AssociatePrincipalWithPortfolioInput`

``` purescript
newtype AssociatePrincipalWithPortfolioInput
  = AssociatePrincipalWithPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PrincipalARN" :: PrincipalARN, "PrincipalType" :: PrincipalType }
```

#### `AssociatePrincipalWithPortfolioOutput`

``` purescript
newtype AssociatePrincipalWithPortfolioOutput
  = AssociatePrincipalWithPortfolioOutput {  }
```

#### `AssociateProductWithPortfolioInput`

``` purescript
newtype AssociateProductWithPortfolioInput
  = AssociateProductWithPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PortfolioId" :: Id, "SourcePortfolioId" :: NullOrUndefined (Id) }
```

#### `AssociateProductWithPortfolioOutput`

``` purescript
newtype AssociateProductWithPortfolioOutput
  = AssociateProductWithPortfolioOutput {  }
```

#### `AssociateTagOptionWithResourceInput`

``` purescript
newtype AssociateTagOptionWithResourceInput
  = AssociateTagOptionWithResourceInput { "ResourceId" :: ResourceId, "TagOptionId" :: TagOptionId }
```

#### `AssociateTagOptionWithResourceOutput`

``` purescript
newtype AssociateTagOptionWithResourceOutput
  = AssociateTagOptionWithResourceOutput {  }
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

#### `CausingEntity`

``` purescript
newtype CausingEntity
  = CausingEntity String
```

#### `ChangeAction`

``` purescript
newtype ChangeAction
  = ChangeAction String
```

#### `CloudWatchDashboard`

``` purescript
newtype CloudWatchDashboard
  = CloudWatchDashboard { "Name" :: NullOrUndefined (CloudWatchDashboardName) }
```

<p>Information about a CloudWatch dashboard.</p>

#### `CloudWatchDashboardName`

``` purescript
newtype CloudWatchDashboardName
  = CloudWatchDashboardName String
```

#### `CloudWatchDashboards`

``` purescript
newtype CloudWatchDashboards
  = CloudWatchDashboards (Array CloudWatchDashboard)
```

#### `ConstraintDescription`

``` purescript
newtype ConstraintDescription
  = ConstraintDescription String
```

#### `ConstraintDetail`

``` purescript
newtype ConstraintDetail
  = ConstraintDetail { "ConstraintId" :: NullOrUndefined (Id), "Type" :: NullOrUndefined (ConstraintType), "Description" :: NullOrUndefined (ConstraintDescription), "Owner" :: NullOrUndefined (AccountId) }
```

<p>Information about a constraint.</p>

#### `ConstraintDetails`

``` purescript
newtype ConstraintDetails
  = ConstraintDetails (Array ConstraintDetail)
```

#### `ConstraintParameters`

``` purescript
newtype ConstraintParameters
  = ConstraintParameters String
```

#### `ConstraintSummaries`

``` purescript
newtype ConstraintSummaries
  = ConstraintSummaries (Array ConstraintSummary)
```

#### `ConstraintSummary`

``` purescript
newtype ConstraintSummary
  = ConstraintSummary { "Type" :: NullOrUndefined (ConstraintType), "Description" :: NullOrUndefined (ConstraintDescription) }
```

<p>Summary information about a constraint.</p>

#### `ConstraintType`

``` purescript
newtype ConstraintType
  = ConstraintType String
```

#### `CopyOption`

``` purescript
newtype CopyOption
  = CopyOption String
```

#### `CopyOptions`

``` purescript
newtype CopyOptions
  = CopyOptions (Array CopyOption)
```

#### `CopyProductInput`

``` purescript
newtype CopyProductInput
  = CopyProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "SourceProductArn" :: ProductArn, "TargetProductId" :: NullOrUndefined (Id), "TargetProductName" :: NullOrUndefined (ProductViewName), "SourceProvisioningArtifactIdentifiers" :: NullOrUndefined (SourceProvisioningArtifactProperties), "CopyOptions" :: NullOrUndefined (CopyOptions), "IdempotencyToken" :: IdempotencyToken }
```

#### `CopyProductOutput`

``` purescript
newtype CopyProductOutput
  = CopyProductOutput { "CopyProductToken" :: NullOrUndefined (Id) }
```

#### `CopyProductStatus`

``` purescript
newtype CopyProductStatus
  = CopyProductStatus String
```

#### `CreateConstraintInput`

``` purescript
newtype CreateConstraintInput
  = CreateConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "ProductId" :: Id, "Parameters" :: ConstraintParameters, "Type" :: ConstraintType, "Description" :: NullOrUndefined (ConstraintDescription), "IdempotencyToken" :: IdempotencyToken }
```

#### `CreateConstraintOutput`

``` purescript
newtype CreateConstraintOutput
  = CreateConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

#### `CreatePortfolioInput`

``` purescript
newtype CreatePortfolioInput
  = CreatePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "DisplayName" :: PortfolioDisplayName, "Description" :: NullOrUndefined (PortfolioDescription), "ProviderName" :: ProviderName, "Tags" :: NullOrUndefined (AddTags), "IdempotencyToken" :: IdempotencyToken }
```

#### `CreatePortfolioOutput`

``` purescript
newtype CreatePortfolioOutput
  = CreatePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags) }
```

#### `CreatePortfolioShareInput`

``` purescript
newtype CreatePortfolioShareInput
  = CreatePortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "AccountId" :: AccountId }
```

#### `CreatePortfolioShareOutput`

``` purescript
newtype CreatePortfolioShareOutput
  = CreatePortfolioShareOutput {  }
```

#### `CreateProductInput`

``` purescript
newtype CreateProductInput
  = CreateProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Name" :: ProductViewName, "Owner" :: ProductViewOwner, "Description" :: NullOrUndefined (ProductViewShortDescription), "Distributor" :: NullOrUndefined (ProductViewOwner), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportUrl" :: NullOrUndefined (SupportUrl), "ProductType" :: ProductType, "Tags" :: NullOrUndefined (AddTags), "ProvisioningArtifactParameters" :: ProvisioningArtifactProperties, "IdempotencyToken" :: IdempotencyToken }
```

#### `CreateProductOutput`

``` purescript
newtype CreateProductOutput
  = CreateProductOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Tags" :: NullOrUndefined (Tags) }
```

#### `CreateProvisionedProductPlanInput`

``` purescript
newtype CreateProvisionedProductPlanInput
  = CreateProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanName" :: ProvisionedProductPlanName, "PlanType" :: ProvisionedProductPlanType, "NotificationArns" :: NullOrUndefined (NotificationArns), "PathId" :: NullOrUndefined (Id), "ProductId" :: Id, "ProvisionedProductName" :: ProvisionedProductName, "ProvisioningArtifactId" :: Id, "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "IdempotencyToken" :: IdempotencyToken, "Tags" :: NullOrUndefined (Tags) }
```

#### `CreateProvisionedProductPlanOutput`

``` purescript
newtype CreateProvisionedProductPlanOutput
  = CreateProvisionedProductPlanOutput { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName), "ProvisioningArtifactId" :: NullOrUndefined (Id) }
```

#### `CreateProvisioningArtifactInput`

``` purescript
newtype CreateProvisioningArtifactInput
  = CreateProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "Parameters" :: ProvisioningArtifactProperties, "IdempotencyToken" :: IdempotencyToken }
```

#### `CreateProvisioningArtifactOutput`

``` purescript
newtype CreateProvisioningArtifactOutput
  = CreateProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

#### `CreateTagOptionInput`

``` purescript
newtype CreateTagOptionInput
  = CreateTagOptionInput { "Key" :: TagOptionKey, "Value" :: TagOptionValue }
```

#### `CreateTagOptionOutput`

``` purescript
newtype CreateTagOptionOutput
  = CreateTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

#### `CreatedTime`

``` purescript
newtype CreatedTime
  = CreatedTime Number
```

#### `CreationTime`

``` purescript
newtype CreationTime
  = CreationTime Number
```

#### `DefaultValue`

``` purescript
newtype DefaultValue
  = DefaultValue String
```

#### `DeleteConstraintInput`

``` purescript
newtype DeleteConstraintInput
  = DeleteConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DeleteConstraintOutput`

``` purescript
newtype DeleteConstraintOutput
  = DeleteConstraintOutput {  }
```

#### `DeletePortfolioInput`

``` purescript
newtype DeletePortfolioInput
  = DeletePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DeletePortfolioOutput`

``` purescript
newtype DeletePortfolioOutput
  = DeletePortfolioOutput {  }
```

#### `DeletePortfolioShareInput`

``` purescript
newtype DeletePortfolioShareInput
  = DeletePortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "AccountId" :: AccountId }
```

#### `DeletePortfolioShareOutput`

``` purescript
newtype DeletePortfolioShareOutput
  = DeletePortfolioShareOutput {  }
```

#### `DeleteProductInput`

``` purescript
newtype DeleteProductInput
  = DeleteProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DeleteProductOutput`

``` purescript
newtype DeleteProductOutput
  = DeleteProductOutput {  }
```

#### `DeleteProvisionedProductPlanInput`

``` purescript
newtype DeleteProvisionedProductPlanInput
  = DeleteProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "IgnoreErrors" :: NullOrUndefined (IgnoreErrors) }
```

#### `DeleteProvisionedProductPlanOutput`

``` purescript
newtype DeleteProvisionedProductPlanOutput
  = DeleteProvisionedProductPlanOutput {  }
```

#### `DeleteProvisioningArtifactInput`

``` purescript
newtype DeleteProvisioningArtifactInput
  = DeleteProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id }
```

#### `DeleteProvisioningArtifactOutput`

``` purescript
newtype DeleteProvisioningArtifactOutput
  = DeleteProvisioningArtifactOutput {  }
```

#### `DescribeConstraintInput`

``` purescript
newtype DescribeConstraintInput
  = DescribeConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribeConstraintOutput`

``` purescript
newtype DescribeConstraintOutput
  = DescribeConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

#### `DescribeCopyProductStatusInput`

``` purescript
newtype DescribeCopyProductStatusInput
  = DescribeCopyProductStatusInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "CopyProductToken" :: Id }
```

#### `DescribeCopyProductStatusOutput`

``` purescript
newtype DescribeCopyProductStatusOutput
  = DescribeCopyProductStatusOutput { "CopyProductStatus" :: NullOrUndefined (CopyProductStatus), "TargetProductId" :: NullOrUndefined (Id), "StatusDetail" :: NullOrUndefined (StatusDetail) }
```

#### `DescribePortfolioInput`

``` purescript
newtype DescribePortfolioInput
  = DescribePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribePortfolioOutput`

``` purescript
newtype DescribePortfolioOutput
  = DescribePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags), "TagOptions" :: NullOrUndefined (TagOptionDetails) }
```

#### `DescribeProductAsAdminInput`

``` purescript
newtype DescribeProductAsAdminInput
  = DescribeProductAsAdminInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribeProductAsAdminOutput`

``` purescript
newtype DescribeProductAsAdminOutput
  = DescribeProductAsAdminOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "ProvisioningArtifactSummaries" :: NullOrUndefined (ProvisioningArtifactSummaries), "Tags" :: NullOrUndefined (Tags), "TagOptions" :: NullOrUndefined (TagOptionDetails) }
```

#### `DescribeProductInput`

``` purescript
newtype DescribeProductInput
  = DescribeProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribeProductOutput`

``` purescript
newtype DescribeProductOutput
  = DescribeProductOutput { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts) }
```

#### `DescribeProductViewInput`

``` purescript
newtype DescribeProductViewInput
  = DescribeProductViewInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribeProductViewOutput`

``` purescript
newtype DescribeProductViewOutput
  = DescribeProductViewOutput { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts) }
```

#### `DescribeProvisionedProductInput`

``` purescript
newtype DescribeProvisionedProductInput
  = DescribeProvisionedProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id }
```

#### `DescribeProvisionedProductOutput`

``` purescript
newtype DescribeProvisionedProductOutput
  = DescribeProvisionedProductOutput { "ProvisionedProductDetail" :: NullOrUndefined (ProvisionedProductDetail), "CloudWatchDashboards" :: NullOrUndefined (CloudWatchDashboards) }
```

#### `DescribeProvisionedProductPlanInput`

``` purescript
newtype DescribeProvisionedProductPlanInput
  = DescribeProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `DescribeProvisionedProductPlanOutput`

``` purescript
newtype DescribeProvisionedProductPlanOutput
  = DescribeProvisionedProductPlanOutput { "ProvisionedProductPlanDetails" :: NullOrUndefined (ProvisionedProductPlanDetails), "ResourceChanges" :: NullOrUndefined (ResourceChanges), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `DescribeProvisioningArtifactInput`

``` purescript
newtype DescribeProvisioningArtifactInput
  = DescribeProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisioningArtifactId" :: Id, "ProductId" :: Id, "Verbose" :: NullOrUndefined (Verbose) }
```

#### `DescribeProvisioningArtifactOutput`

``` purescript
newtype DescribeProvisioningArtifactOutput
  = DescribeProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

#### `DescribeProvisioningParametersInput`

``` purescript
newtype DescribeProvisioningParametersInput
  = DescribeProvisioningParametersInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "PathId" :: NullOrUndefined (Id) }
```

#### `DescribeProvisioningParametersOutput`

``` purescript
newtype DescribeProvisioningParametersOutput
  = DescribeProvisioningParametersOutput { "ProvisioningArtifactParameters" :: NullOrUndefined (ProvisioningArtifactParameters), "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries), "UsageInstructions" :: NullOrUndefined (UsageInstructions), "TagOptions" :: NullOrUndefined (TagOptionSummaries) }
```

#### `DescribeRecordInput`

``` purescript
newtype DescribeRecordInput
  = DescribeRecordInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

#### `DescribeRecordOutput`

``` purescript
newtype DescribeRecordOutput
  = DescribeRecordOutput { "RecordDetail" :: NullOrUndefined (RecordDetail), "RecordOutputs" :: NullOrUndefined (RecordOutputs), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `DescribeTagOptionInput`

``` purescript
newtype DescribeTagOptionInput
  = DescribeTagOptionInput { "Id" :: TagOptionId }
```

#### `DescribeTagOptionOutput`

``` purescript
newtype DescribeTagOptionOutput
  = DescribeTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DisassociatePrincipalFromPortfolioInput`

``` purescript
newtype DisassociatePrincipalFromPortfolioInput
  = DisassociatePrincipalFromPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PrincipalARN" :: PrincipalARN }
```

#### `DisassociatePrincipalFromPortfolioOutput`

``` purescript
newtype DisassociatePrincipalFromPortfolioOutput
  = DisassociatePrincipalFromPortfolioOutput {  }
```

#### `DisassociateProductFromPortfolioInput`

``` purescript
newtype DisassociateProductFromPortfolioInput
  = DisassociateProductFromPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PortfolioId" :: Id }
```

#### `DisassociateProductFromPortfolioOutput`

``` purescript
newtype DisassociateProductFromPortfolioOutput
  = DisassociateProductFromPortfolioOutput {  }
```

#### `DisassociateTagOptionFromResourceInput`

``` purescript
newtype DisassociateTagOptionFromResourceInput
  = DisassociateTagOptionFromResourceInput { "ResourceId" :: ResourceId, "TagOptionId" :: TagOptionId }
```

#### `DisassociateTagOptionFromResourceOutput`

``` purescript
newtype DisassociateTagOptionFromResourceOutput
  = DisassociateTagOptionFromResourceOutput {  }
```

#### `DuplicateResourceException`

``` purescript
newtype DuplicateResourceException
  = DuplicateResourceException {  }
```

<p>The specified resource is a duplicate.</p>

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `ErrorDescription`

``` purescript
newtype ErrorDescription
  = ErrorDescription String
```

#### `EvaluationType`

``` purescript
newtype EvaluationType
  = EvaluationType String
```

#### `ExecuteProvisionedProductPlanInput`

``` purescript
newtype ExecuteProvisionedProductPlanInput
  = ExecuteProvisionedProductPlanInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PlanId" :: Id, "IdempotencyToken" :: IdempotencyToken }
```

#### `ExecuteProvisionedProductPlanOutput`

``` purescript
newtype ExecuteProvisionedProductPlanOutput
  = ExecuteProvisionedProductPlanOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

#### `HasDefaultPath`

``` purescript
newtype HasDefaultPath
  = HasDefaultPath Boolean
```

#### `Id`

``` purescript
newtype Id
  = Id String
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

#### `IgnoreErrors`

``` purescript
newtype IgnoreErrors
  = IgnoreErrors Boolean
```

#### `InstructionType`

``` purescript
newtype InstructionType
  = InstructionType String
```

#### `InstructionValue`

``` purescript
newtype InstructionValue
  = InstructionValue String
```

#### `InvalidParametersException`

``` purescript
newtype InvalidParametersException
  = InvalidParametersException {  }
```

<p>One or more parameters provided to the operation are not valid.</p>

#### `InvalidStateException`

``` purescript
newtype InvalidStateException
  = InvalidStateException {  }
```

<p>An attempt was made to modify a resource that is in a state that is not valid. Check your resources to ensure that they are in valid states before retrying the operation.</p>

#### `LastRequestId`

``` purescript
newtype LastRequestId
  = LastRequestId String
```

#### `LaunchPathSummaries`

``` purescript
newtype LaunchPathSummaries
  = LaunchPathSummaries (Array LaunchPathSummary)
```

#### `LaunchPathSummary`

``` purescript
newtype LaunchPathSummary
  = LaunchPathSummary { "Id" :: NullOrUndefined (Id), "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries), "Tags" :: NullOrUndefined (Tags), "Name" :: NullOrUndefined (PortfolioName) }
```

<p>Summary information about a product path for a user.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The current limits of the service would have been exceeded by this operation. Decrease your resource use or increase your service limits and retry the operation.</p>

#### `ListAcceptedPortfolioSharesInput`

``` purescript
newtype ListAcceptedPortfolioSharesInput
  = ListAcceptedPortfolioSharesInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

#### `ListAcceptedPortfolioSharesOutput`

``` purescript
newtype ListAcceptedPortfolioSharesOutput
  = ListAcceptedPortfolioSharesOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListConstraintsForPortfolioInput`

``` purescript
newtype ListConstraintsForPortfolioInput
  = ListConstraintsForPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "ProductId" :: NullOrUndefined (Id), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListConstraintsForPortfolioOutput`

``` purescript
newtype ListConstraintsForPortfolioOutput
  = ListConstraintsForPortfolioOutput { "ConstraintDetails" :: NullOrUndefined (ConstraintDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListLaunchPathsInput`

``` purescript
newtype ListLaunchPathsInput
  = ListLaunchPathsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListLaunchPathsOutput`

``` purescript
newtype ListLaunchPathsOutput
  = ListLaunchPathsOutput { "LaunchPathSummaries" :: NullOrUndefined (LaunchPathSummaries), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListPortfolioAccessInput`

``` purescript
newtype ListPortfolioAccessInput
  = ListPortfolioAccessInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

#### `ListPortfolioAccessOutput`

``` purescript
newtype ListPortfolioAccessOutput
  = ListPortfolioAccessOutput { "AccountIds" :: NullOrUndefined (AccountIds), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListPortfoliosForProductInput`

``` purescript
newtype ListPortfoliosForProductInput
  = ListPortfoliosForProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

#### `ListPortfoliosForProductOutput`

``` purescript
newtype ListPortfoliosForProductOutput
  = ListPortfoliosForProductOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListPortfoliosInput`

``` purescript
newtype ListPortfoliosInput
  = ListPortfoliosInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize) }
```

#### `ListPortfoliosOutput`

``` purescript
newtype ListPortfoliosOutput
  = ListPortfoliosOutput { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListPrincipalsForPortfolioInput`

``` purescript
newtype ListPrincipalsForPortfolioInput
  = ListPrincipalsForPortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id, "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListPrincipalsForPortfolioOutput`

``` purescript
newtype ListPrincipalsForPortfolioOutput
  = ListPrincipalsForPortfolioOutput { "Principals" :: NullOrUndefined (Principals), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListProvisionedProductPlansInput`

``` purescript
newtype ListProvisionedProductPlansInput
  = ListProvisionedProductPlansInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisionProductId" :: NullOrUndefined (Id), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter) }
```

#### `ListProvisionedProductPlansOutput`

``` purescript
newtype ListProvisionedProductPlansOutput
  = ListProvisionedProductPlansOutput { "ProvisionedProductPlans" :: NullOrUndefined (ProvisionedProductPlans), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListProvisioningArtifactsInput`

``` purescript
newtype ListProvisioningArtifactsInput
  = ListProvisioningArtifactsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id }
```

#### `ListProvisioningArtifactsOutput`

``` purescript
newtype ListProvisioningArtifactsOutput
  = ListProvisioningArtifactsOutput { "ProvisioningArtifactDetails" :: NullOrUndefined (ProvisioningArtifactDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListRecordHistoryInput`

``` purescript
newtype ListRecordHistoryInput
  = ListRecordHistoryInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "SearchFilter" :: NullOrUndefined (ListRecordHistorySearchFilter), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListRecordHistoryOutput`

``` purescript
newtype ListRecordHistoryOutput
  = ListRecordHistoryOutput { "RecordDetails" :: NullOrUndefined (RecordDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `ListRecordHistorySearchFilter`

``` purescript
newtype ListRecordHistorySearchFilter
  = ListRecordHistorySearchFilter { "Key" :: NullOrUndefined (SearchFilterKey), "Value" :: NullOrUndefined (SearchFilterValue) }
```

<p>The search filter to use when listing history records.</p>

#### `ListResourcesForTagOptionInput`

``` purescript
newtype ListResourcesForTagOptionInput
  = ListResourcesForTagOptionInput { "TagOptionId" :: TagOptionId, "ResourceType" :: NullOrUndefined (ResourceType), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListResourcesForTagOptionOutput`

``` purescript
newtype ListResourcesForTagOptionOutput
  = ListResourcesForTagOptionOutput { "ResourceDetails" :: NullOrUndefined (ResourceDetails), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListTagOptionsFilters`

``` purescript
newtype ListTagOptionsFilters
  = ListTagOptionsFilters { "Key" :: NullOrUndefined (TagOptionKey), "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive) }
```

<p>Filters to use when listing TagOptions.</p>

#### `ListTagOptionsInput`

``` purescript
newtype ListTagOptionsInput
  = ListTagOptionsInput { "Filters" :: NullOrUndefined (ListTagOptionsFilters), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ListTagOptionsOutput`

``` purescript
newtype ListTagOptionsOutput
  = ListTagOptionsOutput { "TagOptionDetails" :: NullOrUndefined (TagOptionDetails), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `LogicalResourceId`

``` purescript
newtype LogicalResourceId
  = LogicalResourceId String
```

#### `NoEcho`

``` purescript
newtype NoEcho
  = NoEcho Boolean
```

#### `NotificationArn`

``` purescript
newtype NotificationArn
  = NotificationArn String
```

#### `NotificationArns`

``` purescript
newtype NotificationArns
  = NotificationArns (Array NotificationArn)
```

#### `OutputKey`

``` purescript
newtype OutputKey
  = OutputKey String
```

#### `OutputValue`

``` purescript
newtype OutputValue
  = OutputValue String
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

#### `PageToken`

``` purescript
newtype PageToken
  = PageToken String
```

#### `ParameterConstraints`

``` purescript
newtype ParameterConstraints
  = ParameterConstraints { "AllowedValues" :: NullOrUndefined (AllowedValues) }
```

<p>The constraints that the administrator has put on the parameter.</p>

#### `ParameterKey`

``` purescript
newtype ParameterKey
  = ParameterKey String
```

#### `ParameterType`

``` purescript
newtype ParameterType
  = ParameterType String
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

#### `PhysicalId`

``` purescript
newtype PhysicalId
  = PhysicalId String
```

#### `PhysicalResourceId`

``` purescript
newtype PhysicalResourceId
  = PhysicalResourceId String
```

#### `PlanResourceType`

``` purescript
newtype PlanResourceType
  = PlanResourceType String
```

#### `PortfolioDescription`

``` purescript
newtype PortfolioDescription
  = PortfolioDescription String
```

#### `PortfolioDetail`

``` purescript
newtype PortfolioDetail
  = PortfolioDetail { "Id" :: NullOrUndefined (Id), "ARN" :: NullOrUndefined (ResourceARN), "DisplayName" :: NullOrUndefined (PortfolioDisplayName), "Description" :: NullOrUndefined (PortfolioDescription), "CreatedTime" :: NullOrUndefined (CreationTime), "ProviderName" :: NullOrUndefined (ProviderName) }
```

<p>Information about a portfolio.</p>

#### `PortfolioDetails`

``` purescript
newtype PortfolioDetails
  = PortfolioDetails (Array PortfolioDetail)
```

#### `PortfolioDisplayName`

``` purescript
newtype PortfolioDisplayName
  = PortfolioDisplayName String
```

#### `PortfolioName`

``` purescript
newtype PortfolioName
  = PortfolioName String
```

#### `Principal`

``` purescript
newtype Principal
  = Principal { "PrincipalARN" :: NullOrUndefined (PrincipalARN), "PrincipalType" :: NullOrUndefined (PrincipalType) }
```

<p>Information about a principal.</p>

#### `PrincipalARN`

``` purescript
newtype PrincipalARN
  = PrincipalARN String
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

#### `Principals`

``` purescript
newtype Principals
  = Principals (Array Principal)
```

#### `ProductArn`

``` purescript
newtype ProductArn
  = ProductArn String
```

#### `ProductSource`

``` purescript
newtype ProductSource
  = ProductSource String
```

#### `ProductType`

``` purescript
newtype ProductType
  = ProductType String
```

#### `ProductViewAggregationType`

``` purescript
newtype ProductViewAggregationType
  = ProductViewAggregationType String
```

#### `ProductViewAggregationValue`

``` purescript
newtype ProductViewAggregationValue
  = ProductViewAggregationValue { "Value" :: NullOrUndefined (AttributeValue), "ApproximateCount" :: NullOrUndefined (ApproximateCount) }
```

<p>A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.</p>

#### `ProductViewAggregationValues`

``` purescript
newtype ProductViewAggregationValues
  = ProductViewAggregationValues (Array ProductViewAggregationValue)
```

#### `ProductViewAggregations`

``` purescript
newtype ProductViewAggregations
  = ProductViewAggregations (Map ProductViewAggregationType ProductViewAggregationValues)
```

#### `ProductViewDetail`

``` purescript
newtype ProductViewDetail
  = ProductViewDetail { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary), "Status" :: NullOrUndefined (Status), "ProductARN" :: NullOrUndefined (ResourceARN), "CreatedTime" :: NullOrUndefined (CreatedTime) }
```

<p>Information about a product view.</p>

#### `ProductViewDetails`

``` purescript
newtype ProductViewDetails
  = ProductViewDetails (Array ProductViewDetail)
```

#### `ProductViewDistributor`

``` purescript
newtype ProductViewDistributor
  = ProductViewDistributor String
```

#### `ProductViewFilterBy`

``` purescript
newtype ProductViewFilterBy
  = ProductViewFilterBy String
```

#### `ProductViewFilterValue`

``` purescript
newtype ProductViewFilterValue
  = ProductViewFilterValue String
```

#### `ProductViewFilterValues`

``` purescript
newtype ProductViewFilterValues
  = ProductViewFilterValues (Array ProductViewFilterValue)
```

#### `ProductViewFilters`

``` purescript
newtype ProductViewFilters
  = ProductViewFilters (Map ProductViewFilterBy ProductViewFilterValues)
```

#### `ProductViewName`

``` purescript
newtype ProductViewName
  = ProductViewName String
```

#### `ProductViewOwner`

``` purescript
newtype ProductViewOwner
  = ProductViewOwner String
```

#### `ProductViewShortDescription`

``` purescript
newtype ProductViewShortDescription
  = ProductViewShortDescription String
```

#### `ProductViewSortBy`

``` purescript
newtype ProductViewSortBy
  = ProductViewSortBy String
```

#### `ProductViewSummaries`

``` purescript
newtype ProductViewSummaries
  = ProductViewSummaries (Array ProductViewSummary)
```

#### `ProductViewSummary`

``` purescript
newtype ProductViewSummary
  = ProductViewSummary { "Id" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProductViewName), "Owner" :: NullOrUndefined (ProductViewOwner), "ShortDescription" :: NullOrUndefined (ProductViewShortDescription), "Type" :: NullOrUndefined (ProductType), "Distributor" :: NullOrUndefined (ProductViewDistributor), "HasDefaultPath" :: NullOrUndefined (HasDefaultPath), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportUrl" :: NullOrUndefined (SupportUrl) }
```

<p>Summary information about a product view.</p>

#### `PropertyName`

``` purescript
newtype PropertyName
  = PropertyName String
```

#### `ProviderName`

``` purescript
newtype ProviderName
  = ProviderName String
```

#### `ProvisionProductInput`

``` purescript
newtype ProvisionProductInput
  = ProvisionProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "PathId" :: NullOrUndefined (Id), "ProvisionedProductName" :: ProvisionedProductName, "ProvisioningParameters" :: NullOrUndefined (ProvisioningParameters), "Tags" :: NullOrUndefined (Tags), "NotificationArns" :: NullOrUndefined (NotificationArns), "ProvisionToken" :: IdempotencyToken }
```

#### `ProvisionProductOutput`

``` purescript
newtype ProvisionProductOutput
  = ProvisionProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

#### `ProvisionedProductAttribute`

``` purescript
newtype ProvisionedProductAttribute
  = ProvisionedProductAttribute { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn), "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn), "Type" :: NullOrUndefined (ProvisionedProductType), "Id" :: NullOrUndefined (Id), "Status" :: NullOrUndefined (ProvisionedProductStatus), "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage), "CreatedTime" :: NullOrUndefined (CreatedTime), "IdempotencyToken" :: NullOrUndefined (IdempotencyToken), "LastRecordId" :: NullOrUndefined (Id), "Tags" :: NullOrUndefined (Tags), "PhysicalId" :: NullOrUndefined (PhysicalId), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "UserArn" :: NullOrUndefined (UserArn), "UserArnSession" :: NullOrUndefined (UserArnSession) }
```

<p>Information about a provisioned product.</p>

#### `ProvisionedProductAttributes`

``` purescript
newtype ProvisionedProductAttributes
  = ProvisionedProductAttributes (Array ProvisionedProductAttribute)
```

#### `ProvisionedProductDetail`

``` purescript
newtype ProvisionedProductDetail
  = ProvisionedProductDetail { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn), "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn), "Type" :: NullOrUndefined (ProvisionedProductType), "Id" :: NullOrUndefined (ProvisionedProductId), "Status" :: NullOrUndefined (ProvisionedProductStatus), "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage), "CreatedTime" :: NullOrUndefined (CreatedTime), "IdempotencyToken" :: NullOrUndefined (IdempotencyToken), "LastRecordId" :: NullOrUndefined (LastRequestId) }
```

<p>Information about a provisioned product.</p>

#### `ProvisionedProductDetails`

``` purescript
newtype ProvisionedProductDetails
  = ProvisionedProductDetails (Array ProvisionedProductDetail)
```

#### `ProvisionedProductFilters`

``` purescript
newtype ProvisionedProductFilters
  = ProvisionedProductFilters (Map ProvisionedProductViewFilterBy ProvisionedProductViewFilterValues)
```

#### `ProvisionedProductId`

``` purescript
newtype ProvisionedProductId
  = ProvisionedProductId String
```

#### `ProvisionedProductName`

``` purescript
newtype ProvisionedProductName
  = ProvisionedProductName String
```

#### `ProvisionedProductNameOrArn`

``` purescript
newtype ProvisionedProductNameOrArn
  = ProvisionedProductNameOrArn String
```

#### `ProvisionedProductPlanDetails`

``` purescript
newtype ProvisionedProductPlanDetails
  = ProvisionedProductPlanDetails { "CreatedTime" :: NullOrUndefined (CreatedTime), "PathId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName), "PlanType" :: NullOrUndefined (ProvisionedProductPlanType), "ProvisioningArtifactId" :: NullOrUndefined (Id), "Status" :: NullOrUndefined (ProvisionedProductPlanStatus), "UpdatedTime" :: NullOrUndefined (UpdatedTime), "NotificationArns" :: NullOrUndefined (NotificationArns), "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "Tags" :: NullOrUndefined (Tags), "StatusMessage" :: NullOrUndefined (StatusMessage) }
```

<p>Information about a plan.</p>

#### `ProvisionedProductPlanName`

``` purescript
newtype ProvisionedProductPlanName
  = ProvisionedProductPlanName String
```

#### `ProvisionedProductPlanStatus`

``` purescript
newtype ProvisionedProductPlanStatus
  = ProvisionedProductPlanStatus String
```

#### `ProvisionedProductPlanSummary`

``` purescript
newtype ProvisionedProductPlanSummary
  = ProvisionedProductPlanSummary { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName), "PlanId" :: NullOrUndefined (Id), "ProvisionProductId" :: NullOrUndefined (Id), "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName), "PlanType" :: NullOrUndefined (ProvisionedProductPlanType), "ProvisioningArtifactId" :: NullOrUndefined (Id) }
```

<p>Summary information about a plan.</p>

#### `ProvisionedProductPlanType`

``` purescript
newtype ProvisionedProductPlanType
  = ProvisionedProductPlanType String
```

#### `ProvisionedProductPlans`

``` purescript
newtype ProvisionedProductPlans
  = ProvisionedProductPlans (Array ProvisionedProductPlanSummary)
```

#### `ProvisionedProductStatus`

``` purescript
newtype ProvisionedProductStatus
  = ProvisionedProductStatus String
```

#### `ProvisionedProductStatusMessage`

``` purescript
newtype ProvisionedProductStatusMessage
  = ProvisionedProductStatusMessage String
```

#### `ProvisionedProductType`

``` purescript
newtype ProvisionedProductType
  = ProvisionedProductType String
```

#### `ProvisionedProductViewFilterBy`

``` purescript
newtype ProvisionedProductViewFilterBy
  = ProvisionedProductViewFilterBy String
```

#### `ProvisionedProductViewFilterValue`

``` purescript
newtype ProvisionedProductViewFilterValue
  = ProvisionedProductViewFilterValue String
```

#### `ProvisionedProductViewFilterValues`

``` purescript
newtype ProvisionedProductViewFilterValues
  = ProvisionedProductViewFilterValues (Array ProvisionedProductViewFilterValue)
```

#### `ProvisioningArtifact`

``` purescript
newtype ProvisioningArtifact
  = ProvisioningArtifact { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime) }
```

<p>Information about a provisioning artifact. A provisioning artifact is also known as a product version.</p>

#### `ProvisioningArtifactActive`

``` purescript
newtype ProvisioningArtifactActive
  = ProvisioningArtifactActive Boolean
```

#### `ProvisioningArtifactCreatedTime`

``` purescript
newtype ProvisioningArtifactCreatedTime
  = ProvisioningArtifactCreatedTime Number
```

#### `ProvisioningArtifactDescription`

``` purescript
newtype ProvisioningArtifactDescription
  = ProvisioningArtifactDescription String
```

#### `ProvisioningArtifactDetail`

``` purescript
newtype ProvisioningArtifactDetail
  = ProvisioningArtifactDetail { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactName), "Type" :: NullOrUndefined (ProvisioningArtifactType), "CreatedTime" :: NullOrUndefined (CreationTime), "Active" :: NullOrUndefined (ProvisioningArtifactActive) }
```

<p>Information about a provisioning artifact (also known as a version) for a product.</p>

#### `ProvisioningArtifactDetails`

``` purescript
newtype ProvisioningArtifactDetails
  = ProvisioningArtifactDetails (Array ProvisioningArtifactDetail)
```

#### `ProvisioningArtifactInfo`

``` purescript
newtype ProvisioningArtifactInfo
  = ProvisioningArtifactInfo (Map ProvisioningArtifactInfoKey ProvisioningArtifactInfoValue)
```

#### `ProvisioningArtifactInfoKey`

``` purescript
newtype ProvisioningArtifactInfoKey
  = ProvisioningArtifactInfoKey String
```

#### `ProvisioningArtifactInfoValue`

``` purescript
newtype ProvisioningArtifactInfoValue
  = ProvisioningArtifactInfoValue String
```

#### `ProvisioningArtifactName`

``` purescript
newtype ProvisioningArtifactName
  = ProvisioningArtifactName String
```

#### `ProvisioningArtifactParameter`

``` purescript
newtype ProvisioningArtifactParameter
  = ProvisioningArtifactParameter { "ParameterKey" :: NullOrUndefined (ParameterKey), "DefaultValue" :: NullOrUndefined (DefaultValue), "ParameterType" :: NullOrUndefined (ParameterType), "IsNoEcho" :: NullOrUndefined (NoEcho), "Description" :: NullOrUndefined (Description), "ParameterConstraints" :: NullOrUndefined (ParameterConstraints) }
```

<p>Information about a parameter used to provision a product.</p>

#### `ProvisioningArtifactParameters`

``` purescript
newtype ProvisioningArtifactParameters
  = ProvisioningArtifactParameters (Array ProvisioningArtifactParameter)
```

#### `ProvisioningArtifactProperties`

``` purescript
newtype ProvisioningArtifactProperties
  = ProvisioningArtifactProperties { "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "Info" :: ProvisioningArtifactInfo, "Type" :: NullOrUndefined (ProvisioningArtifactType) }
```

<p>Information about a provisioning artifact (also known as a version) for a product.</p>

#### `ProvisioningArtifactPropertyName`

``` purescript
newtype ProvisioningArtifactPropertyName
  = ProvisioningArtifactPropertyName String
```

#### `ProvisioningArtifactPropertyValue`

``` purescript
newtype ProvisioningArtifactPropertyValue
  = ProvisioningArtifactPropertyValue String
```

#### `ProvisioningArtifactSummaries`

``` purescript
newtype ProvisioningArtifactSummaries
  = ProvisioningArtifactSummaries (Array ProvisioningArtifactSummary)
```

#### `ProvisioningArtifactSummary`

``` purescript
newtype ProvisioningArtifactSummary
  = ProvisioningArtifactSummary { "Id" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime), "ProvisioningArtifactMetadata" :: NullOrUndefined (ProvisioningArtifactInfo) }
```

<p>Summary information about a provisioning artifact (also known as a version) for a product.</p>

#### `ProvisioningArtifactType`

``` purescript
newtype ProvisioningArtifactType
  = ProvisioningArtifactType String
```

#### `ProvisioningArtifacts`

``` purescript
newtype ProvisioningArtifacts
  = ProvisioningArtifacts (Array ProvisioningArtifact)
```

#### `ProvisioningParameter`

``` purescript
newtype ProvisioningParameter
  = ProvisioningParameter { "Key" :: NullOrUndefined (ParameterKey), "Value" :: NullOrUndefined (ParameterValue) }
```

<p>Information about a parameter used to provision a product.</p>

#### `ProvisioningParameters`

``` purescript
newtype ProvisioningParameters
  = ProvisioningParameters (Array ProvisioningParameter)
```

#### `RecordDetail`

``` purescript
newtype RecordDetail
  = RecordDetail { "RecordId" :: NullOrUndefined (Id), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName), "Status" :: NullOrUndefined (RecordStatus), "CreatedTime" :: NullOrUndefined (CreatedTime), "UpdatedTime" :: NullOrUndefined (UpdatedTime), "ProvisionedProductType" :: NullOrUndefined (ProvisionedProductType), "RecordType" :: NullOrUndefined (RecordType), "ProvisionedProductId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "PathId" :: NullOrUndefined (Id), "RecordErrors" :: NullOrUndefined (RecordErrors), "RecordTags" :: NullOrUndefined (RecordTags) }
```

<p>Information about a request operation.</p>

#### `RecordDetails`

``` purescript
newtype RecordDetails
  = RecordDetails (Array RecordDetail)
```

#### `RecordError`

``` purescript
newtype RecordError
  = RecordError { "Code" :: NullOrUndefined (ErrorCode), "Description" :: NullOrUndefined (ErrorDescription) }
```

<p>The error code and description resulting from an operation.</p>

#### `RecordErrors`

``` purescript
newtype RecordErrors
  = RecordErrors (Array RecordError)
```

#### `RecordOutput`

``` purescript
newtype RecordOutput
  = RecordOutput { "OutputKey" :: NullOrUndefined (OutputKey), "OutputValue" :: NullOrUndefined (OutputValue), "Description" :: NullOrUndefined (Description) }
```

<p>The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.</p>

#### `RecordOutputs`

``` purescript
newtype RecordOutputs
  = RecordOutputs (Array RecordOutput)
```

#### `RecordStatus`

``` purescript
newtype RecordStatus
  = RecordStatus String
```

#### `RecordTag`

``` purescript
newtype RecordTag
  = RecordTag { "Key" :: NullOrUndefined (RecordTagKey), "Value" :: NullOrUndefined (RecordTagValue) }
```

<p>Information about a tag, which is a key-value pair.</p>

#### `RecordTagKey`

``` purescript
newtype RecordTagKey
  = RecordTagKey String
```

#### `RecordTagValue`

``` purescript
newtype RecordTagValue
  = RecordTagValue String
```

#### `RecordTags`

``` purescript
newtype RecordTags
  = RecordTags (Array RecordTag)
```

#### `RecordType`

``` purescript
newtype RecordType
  = RecordType String
```

#### `RejectPortfolioShareInput`

``` purescript
newtype RejectPortfolioShareInput
  = RejectPortfolioShareInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: Id }
```

#### `RejectPortfolioShareOutput`

``` purescript
newtype RejectPortfolioShareOutput
  = RejectPortfolioShareOutput {  }
```

#### `Replacement`

``` purescript
newtype Replacement
  = Replacement String
```

#### `RequiresRecreation`

``` purescript
newtype RequiresRecreation
  = RequiresRecreation String
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

#### `ResourceAttribute`

``` purescript
newtype ResourceAttribute
  = ResourceAttribute String
```

#### `ResourceChange`

``` purescript
newtype ResourceChange
  = ResourceChange { "Action" :: NullOrUndefined (ChangeAction), "LogicalResourceId" :: NullOrUndefined (LogicalResourceId), "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: NullOrUndefined (PlanResourceType), "Replacement" :: NullOrUndefined (Replacement), "Scope" :: NullOrUndefined (Scope), "Details" :: NullOrUndefined (ResourceChangeDetails) }
```

<p>Information about a resource change that will occur when a plan is executed.</p>

#### `ResourceChangeDetail`

``` purescript
newtype ResourceChangeDetail
  = ResourceChangeDetail { "Target" :: NullOrUndefined (ResourceTargetDefinition), "Evaluation" :: NullOrUndefined (EvaluationType), "CausingEntity" :: NullOrUndefined (CausingEntity) }
```

<p>Information about a change to a resource attribute.</p>

#### `ResourceChangeDetails`

``` purescript
newtype ResourceChangeDetails
  = ResourceChangeDetails (Array ResourceChangeDetail)
```

#### `ResourceChanges`

``` purescript
newtype ResourceChanges
  = ResourceChanges (Array ResourceChange)
```

#### `ResourceDetail`

``` purescript
newtype ResourceDetail
  = ResourceDetail { "Id" :: NullOrUndefined (ResourceDetailId), "ARN" :: NullOrUndefined (ResourceDetailARN), "Name" :: NullOrUndefined (ResourceDetailName), "Description" :: NullOrUndefined (ResourceDetailDescription), "CreatedTime" :: NullOrUndefined (ResourceDetailCreatedTime) }
```

<p>Information about a resource.</p>

#### `ResourceDetailARN`

``` purescript
newtype ResourceDetailARN
  = ResourceDetailARN String
```

#### `ResourceDetailCreatedTime`

``` purescript
newtype ResourceDetailCreatedTime
  = ResourceDetailCreatedTime Number
```

#### `ResourceDetailDescription`

``` purescript
newtype ResourceDetailDescription
  = ResourceDetailDescription String
```

#### `ResourceDetailId`

``` purescript
newtype ResourceDetailId
  = ResourceDetailId String
```

#### `ResourceDetailName`

``` purescript
newtype ResourceDetailName
  = ResourceDetailName String
```

#### `ResourceDetails`

``` purescript
newtype ResourceDetails
  = ResourceDetails (Array ResourceDetail)
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException {  }
```

<p>A resource that is currently in use. Ensure that the resource is not in use and retry the operation.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The specified resource was not found.</p>

#### `ResourceTargetDefinition`

``` purescript
newtype ResourceTargetDefinition
  = ResourceTargetDefinition { "Attribute" :: NullOrUndefined (ResourceAttribute), "Name" :: NullOrUndefined (PropertyName), "RequiresRecreation" :: NullOrUndefined (RequiresRecreation) }
```

<p>Information about a change to a resource attribute.</p>

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ScanProvisionedProductsInput`

``` purescript
newtype ScanProvisionedProductsInput
  = ScanProvisionedProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "PageSize" :: NullOrUndefined (PageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `ScanProvisionedProductsOutput`

``` purescript
newtype ScanProvisionedProductsOutput
  = ScanProvisionedProductsOutput { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `Scope`

``` purescript
newtype Scope
  = Scope (Array ResourceAttribute)
```

#### `SearchFilterKey`

``` purescript
newtype SearchFilterKey
  = SearchFilterKey String
```

#### `SearchFilterValue`

``` purescript
newtype SearchFilterValue
  = SearchFilterValue String
```

#### `SearchProductsAsAdminInput`

``` purescript
newtype SearchProductsAsAdminInput
  = SearchProductsAsAdminInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "PortfolioId" :: NullOrUndefined (Id), "Filters" :: NullOrUndefined (ProductViewFilters), "SortBy" :: NullOrUndefined (ProductViewSortBy), "SortOrder" :: NullOrUndefined (SortOrder), "PageToken" :: NullOrUndefined (PageToken), "PageSize" :: NullOrUndefined (PageSize), "ProductSource" :: NullOrUndefined (ProductSource) }
```

#### `SearchProductsAsAdminOutput`

``` purescript
newtype SearchProductsAsAdminOutput
  = SearchProductsAsAdminOutput { "ProductViewDetails" :: NullOrUndefined (ProductViewDetails), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `SearchProductsInput`

``` purescript
newtype SearchProductsInput
  = SearchProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Filters" :: NullOrUndefined (ProductViewFilters), "PageSize" :: NullOrUndefined (PageSize), "SortBy" :: NullOrUndefined (ProductViewSortBy), "SortOrder" :: NullOrUndefined (SortOrder), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `SearchProductsOutput`

``` purescript
newtype SearchProductsOutput
  = SearchProductsOutput { "ProductViewSummaries" :: NullOrUndefined (ProductViewSummaries), "ProductViewAggregations" :: NullOrUndefined (ProductViewAggregations), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `SearchProvisionedProductsInput`

``` purescript
newtype SearchProvisionedProductsInput
  = SearchProvisionedProductsInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter), "Filters" :: NullOrUndefined (ProvisionedProductFilters), "SortBy" :: NullOrUndefined (SortField), "SortOrder" :: NullOrUndefined (SortOrder), "PageSize" :: NullOrUndefined (SearchProvisionedProductsPageSize), "PageToken" :: NullOrUndefined (PageToken) }
```

#### `SearchProvisionedProductsOutput`

``` purescript
newtype SearchProvisionedProductsOutput
  = SearchProvisionedProductsOutput { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductAttributes), "TotalResultsCount" :: NullOrUndefined (TotalResultsCount), "NextPageToken" :: NullOrUndefined (PageToken) }
```

#### `SearchProvisionedProductsPageSize`

``` purescript
newtype SearchProvisionedProductsPageSize
  = SearchProvisionedProductsPageSize Int
```

#### `SortField`

``` purescript
newtype SortField
  = SortField String
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

#### `SourceProvisioningArtifactProperties`

``` purescript
newtype SourceProvisioningArtifactProperties
  = SourceProvisioningArtifactProperties (Array SourceProvisioningArtifactPropertiesMap)
```

#### `SourceProvisioningArtifactPropertiesMap`

``` purescript
newtype SourceProvisioningArtifactPropertiesMap
  = SourceProvisioningArtifactPropertiesMap (Map ProvisioningArtifactPropertyName ProvisioningArtifactPropertyValue)
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StatusDetail`

``` purescript
newtype StatusDetail
  = StatusDetail String
```

#### `StatusMessage`

``` purescript
newtype StatusMessage
  = StatusMessage String
```

#### `SupportDescription`

``` purescript
newtype SupportDescription
  = SupportDescription String
```

#### `SupportEmail`

``` purescript
newtype SupportEmail
  = SupportEmail String
```

#### `SupportUrl`

``` purescript
newtype SupportUrl
  = SupportUrl String
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Information about a tag. A tag is a key-value pair. Tags are propagated to the resources created when provisioning a product.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

#### `TagOptionActive`

``` purescript
newtype TagOptionActive
  = TagOptionActive Boolean
```

#### `TagOptionDetail`

``` purescript
newtype TagOptionDetail
  = TagOptionDetail { "Key" :: NullOrUndefined (TagOptionKey), "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive), "Id" :: NullOrUndefined (TagOptionId) }
```

<p>Information about a TagOption.</p>

#### `TagOptionDetails`

``` purescript
newtype TagOptionDetails
  = TagOptionDetails (Array TagOptionDetail)
```

#### `TagOptionId`

``` purescript
newtype TagOptionId
  = TagOptionId String
```

#### `TagOptionKey`

``` purescript
newtype TagOptionKey
  = TagOptionKey String
```

#### `TagOptionNotMigratedException`

``` purescript
newtype TagOptionNotMigratedException
  = TagOptionNotMigratedException {  }
```

<p>An operation requiring TagOptions failed because the TagOptions migration process has not been performed for this account. Please use the AWS console to perform the migration process before retrying the operation.</p>

#### `TagOptionSummaries`

``` purescript
newtype TagOptionSummaries
  = TagOptionSummaries (Array TagOptionSummary)
```

#### `TagOptionSummary`

``` purescript
newtype TagOptionSummary
  = TagOptionSummary { "Key" :: NullOrUndefined (TagOptionKey), "Values" :: NullOrUndefined (TagOptionValues) }
```

<p>Summary information about a TagOption.</p>

#### `TagOptionValue`

``` purescript
newtype TagOptionValue
  = TagOptionValue String
```

#### `TagOptionValues`

``` purescript
newtype TagOptionValues
  = TagOptionValues (Array TagOptionValue)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

#### `TerminateProvisionedProductInput`

``` purescript
newtype TerminateProvisionedProductInput
  = TerminateProvisionedProductInput { "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn), "ProvisionedProductId" :: NullOrUndefined (Id), "TerminateToken" :: IdempotencyToken, "IgnoreErrors" :: NullOrUndefined (IgnoreErrors), "AcceptLanguage" :: NullOrUndefined (AcceptLanguage) }
```

#### `TerminateProvisionedProductOutput`

``` purescript
newtype TerminateProvisionedProductOutput
  = TerminateProvisionedProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

#### `TotalResultsCount`

``` purescript
newtype TotalResultsCount
  = TotalResultsCount Int
```

#### `UpdateConstraintInput`

``` purescript
newtype UpdateConstraintInput
  = UpdateConstraintInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "Description" :: NullOrUndefined (ConstraintDescription) }
```

#### `UpdateConstraintOutput`

``` purescript
newtype UpdateConstraintOutput
  = UpdateConstraintOutput { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail), "ConstraintParameters" :: NullOrUndefined (ConstraintParameters), "Status" :: NullOrUndefined (Status) }
```

#### `UpdatePortfolioInput`

``` purescript
newtype UpdatePortfolioInput
  = UpdatePortfolioInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "DisplayName" :: NullOrUndefined (PortfolioDisplayName), "Description" :: NullOrUndefined (PortfolioDescription), "ProviderName" :: NullOrUndefined (ProviderName), "AddTags" :: NullOrUndefined (AddTags), "RemoveTags" :: NullOrUndefined (TagKeys) }
```

#### `UpdatePortfolioOutput`

``` purescript
newtype UpdatePortfolioOutput
  = UpdatePortfolioOutput { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail), "Tags" :: NullOrUndefined (Tags) }
```

#### `UpdateProductInput`

``` purescript
newtype UpdateProductInput
  = UpdateProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "Id" :: Id, "Name" :: NullOrUndefined (ProductViewName), "Owner" :: NullOrUndefined (ProductViewOwner), "Description" :: NullOrUndefined (ProductViewShortDescription), "Distributor" :: NullOrUndefined (ProductViewOwner), "SupportDescription" :: NullOrUndefined (SupportDescription), "SupportEmail" :: NullOrUndefined (SupportEmail), "SupportUrl" :: NullOrUndefined (SupportUrl), "AddTags" :: NullOrUndefined (AddTags), "RemoveTags" :: NullOrUndefined (TagKeys) }
```

#### `UpdateProductOutput`

``` purescript
newtype UpdateProductOutput
  = UpdateProductOutput { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail), "Tags" :: NullOrUndefined (Tags) }
```

#### `UpdateProvisionedProductInput`

``` purescript
newtype UpdateProvisionedProductInput
  = UpdateProvisionedProductInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn), "ProvisionedProductId" :: NullOrUndefined (Id), "ProductId" :: NullOrUndefined (Id), "ProvisioningArtifactId" :: NullOrUndefined (Id), "PathId" :: NullOrUndefined (Id), "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters), "UpdateToken" :: IdempotencyToken }
```

#### `UpdateProvisionedProductOutput`

``` purescript
newtype UpdateProvisionedProductOutput
  = UpdateProvisionedProductOutput { "RecordDetail" :: NullOrUndefined (RecordDetail) }
```

#### `UpdateProvisioningArtifactInput`

``` purescript
newtype UpdateProvisioningArtifactInput
  = UpdateProvisioningArtifactInput { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage), "ProductId" :: Id, "ProvisioningArtifactId" :: Id, "Name" :: NullOrUndefined (ProvisioningArtifactName), "Description" :: NullOrUndefined (ProvisioningArtifactDescription), "Active" :: NullOrUndefined (ProvisioningArtifactActive) }
```

#### `UpdateProvisioningArtifactOutput`

``` purescript
newtype UpdateProvisioningArtifactOutput
  = UpdateProvisioningArtifactOutput { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail), "Info" :: NullOrUndefined (ProvisioningArtifactInfo), "Status" :: NullOrUndefined (Status) }
```

#### `UpdateProvisioningParameter`

``` purescript
newtype UpdateProvisioningParameter
  = UpdateProvisioningParameter { "Key" :: NullOrUndefined (ParameterKey), "Value" :: NullOrUndefined (ParameterValue), "UsePreviousValue" :: NullOrUndefined (UsePreviousValue) }
```

<p>The parameter key-value pair used to update a provisioned product.</p>

#### `UpdateProvisioningParameters`

``` purescript
newtype UpdateProvisioningParameters
  = UpdateProvisioningParameters (Array UpdateProvisioningParameter)
```

#### `UpdateTagOptionInput`

``` purescript
newtype UpdateTagOptionInput
  = UpdateTagOptionInput { "Id" :: TagOptionId, "Value" :: NullOrUndefined (TagOptionValue), "Active" :: NullOrUndefined (TagOptionActive) }
```

#### `UpdateTagOptionOutput`

``` purescript
newtype UpdateTagOptionOutput
  = UpdateTagOptionOutput { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail) }
```

#### `UpdatedTime`

``` purescript
newtype UpdatedTime
  = UpdatedTime Number
```

#### `UsageInstruction`

``` purescript
newtype UsageInstruction
  = UsageInstruction { "Type" :: NullOrUndefined (InstructionType), "Value" :: NullOrUndefined (InstructionValue) }
```

<p>Additional information provided by the administrator.</p>

#### `UsageInstructions`

``` purescript
newtype UsageInstructions
  = UsageInstructions (Array UsageInstruction)
```

#### `UsePreviousValue`

``` purescript
newtype UsePreviousValue
  = UsePreviousValue Boolean
```

#### `UserArn`

``` purescript
newtype UserArn
  = UserArn String
```

#### `UserArnSession`

``` purescript
newtype UserArnSession
  = UserArnSession String
```

#### `Verbose`

``` purescript
newtype Verbose
  = Verbose Boolean
```


