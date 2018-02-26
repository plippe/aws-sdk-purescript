

-- | <fullname>AWS Service Catalog</fullname> <p> <a href="https://aws.amazon.com/servicecatalog/">AWS Service Catalog</a> enables organizations to create and manage catalogs of IT services that are approved for use on AWS. To get the most out of this documentation, you should be familiar with the terminology discussed in <a href="http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html">AWS Service Catalog Concepts</a>.</p>
module AWS.ServiceCatalog where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ServiceCatalog" :: String


-- | <p>Accepts an offer to share the specified portfolio.</p>
acceptPortfolioShare :: forall eff. AcceptPortfolioShareInput -> Aff (err :: AWS.RequestError | eff) AcceptPortfolioShareOutput
acceptPortfolioShare = AWS.request serviceName "AcceptPortfolioShare" 


-- | <p>Associates the specified principal ARN with the specified portfolio.</p>
associatePrincipalWithPortfolio :: forall eff. AssociatePrincipalWithPortfolioInput -> Aff (err :: AWS.RequestError | eff) AssociatePrincipalWithPortfolioOutput
associatePrincipalWithPortfolio = AWS.request serviceName "AssociatePrincipalWithPortfolio" 


-- | <p>Associates the specified product with the specified portfolio.</p>
associateProductWithPortfolio :: forall eff. AssociateProductWithPortfolioInput -> Aff (err :: AWS.RequestError | eff) AssociateProductWithPortfolioOutput
associateProductWithPortfolio = AWS.request serviceName "AssociateProductWithPortfolio" 


-- | <p>Associate the specified TagOption with the specified portfolio or product.</p>
associateTagOptionWithResource :: forall eff. AssociateTagOptionWithResourceInput -> Aff (err :: AWS.RequestError | eff) AssociateTagOptionWithResourceOutput
associateTagOptionWithResource = AWS.request serviceName "AssociateTagOptionWithResource" 


-- | <p>Copies the specified source product to the specified target product or a new product.</p> <p>You can copy a product to the same account or another account. You can copy a product to the same region or another region.</p> <p>This operation is performed asynchronously. To track the progress of the operation, use <a>DescribeCopyProductStatus</a>.</p>
copyProduct :: forall eff. CopyProductInput -> Aff (err :: AWS.RequestError | eff) CopyProductOutput
copyProduct = AWS.request serviceName "CopyProduct" 


-- | <p>Creates a constraint.</p>
createConstraint :: forall eff. CreateConstraintInput -> Aff (err :: AWS.RequestError | eff) CreateConstraintOutput
createConstraint = AWS.request serviceName "CreateConstraint" 


-- | <p>Creates a portfolio.</p>
createPortfolio :: forall eff. CreatePortfolioInput -> Aff (err :: AWS.RequestError | eff) CreatePortfolioOutput
createPortfolio = AWS.request serviceName "CreatePortfolio" 


-- | <p>Shares the specified portfolio with the specified account.</p>
createPortfolioShare :: forall eff. CreatePortfolioShareInput -> Aff (err :: AWS.RequestError | eff) CreatePortfolioShareOutput
createPortfolioShare = AWS.request serviceName "CreatePortfolioShare" 


-- | <p>Creates a product.</p>
createProduct :: forall eff. CreateProductInput -> Aff (err :: AWS.RequestError | eff) CreateProductOutput
createProduct = AWS.request serviceName "CreateProduct" 


-- | <p>Creates a plan. A plan includes the list of resources that will be created (when provisioning a new product) or modified (when updating a provisioned product) when the plan is executed.</p> <p>You can create one plan per provisioned product. To create a plan for an existing provisioned product, it's status must be AVAILBLE or TAINTED.</p> <p>To view the resource changes in the change set, use <a>DescribeProvisionedProductPlan</a>. To create or modify the provisioned product, use <a>ExecuteProvisionedProductPlan</a>.</p>
createProvisionedProductPlan :: forall eff. CreateProvisionedProductPlanInput -> Aff (err :: AWS.RequestError | eff) CreateProvisionedProductPlanOutput
createProvisionedProductPlan = AWS.request serviceName "CreateProvisionedProductPlan" 


-- | <p>Creates a provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot create a provisioning artifact for a product that was shared with you.</p>
createProvisioningArtifact :: forall eff. CreateProvisioningArtifactInput -> Aff (err :: AWS.RequestError | eff) CreateProvisioningArtifactOutput
createProvisioningArtifact = AWS.request serviceName "CreateProvisioningArtifact" 


-- | <p>Creates a TagOption.</p>
createTagOption :: forall eff. CreateTagOptionInput -> Aff (err :: AWS.RequestError | eff) CreateTagOptionOutput
createTagOption = AWS.request serviceName "CreateTagOption" 


-- | <p>Deletes the specified constraint.</p>
deleteConstraint :: forall eff. DeleteConstraintInput -> Aff (err :: AWS.RequestError | eff) DeleteConstraintOutput
deleteConstraint = AWS.request serviceName "DeleteConstraint" 


-- | <p>Deletes the specified portfolio.</p> <p>You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.</p>
deletePortfolio :: forall eff. DeletePortfolioInput -> Aff (err :: AWS.RequestError | eff) DeletePortfolioOutput
deletePortfolio = AWS.request serviceName "DeletePortfolio" 


-- | <p>Stops sharing the specified portfolio with the specified account.</p>
deletePortfolioShare :: forall eff. DeletePortfolioShareInput -> Aff (err :: AWS.RequestError | eff) DeletePortfolioShareOutput
deletePortfolioShare = AWS.request serviceName "DeletePortfolioShare" 


-- | <p>Deletes the specified product.</p> <p>You cannot delete a product if it was shared with you or is associated with a portfolio.</p>
deleteProduct :: forall eff. DeleteProductInput -> Aff (err :: AWS.RequestError | eff) DeleteProductOutput
deleteProduct = AWS.request serviceName "DeleteProduct" 


-- | <p>Deletes the specified plan.</p>
deleteProvisionedProductPlan :: forall eff. DeleteProvisionedProductPlanInput -> Aff (err :: AWS.RequestError | eff) DeleteProvisionedProductPlanOutput
deleteProvisionedProductPlan = AWS.request serviceName "DeleteProvisionedProductPlan" 


-- | <p>Deletes the specified provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot delete a provisioning artifact associated with a product that was shared with you. You cannot delete the last provisioning artifact for a product, because a product must have at least one provisioning artifact.</p>
deleteProvisioningArtifact :: forall eff. DeleteProvisioningArtifactInput -> Aff (err :: AWS.RequestError | eff) DeleteProvisioningArtifactOutput
deleteProvisioningArtifact = AWS.request serviceName "DeleteProvisioningArtifact" 


-- | <p>Gets information about the specified constraint.</p>
describeConstraint :: forall eff. DescribeConstraintInput -> Aff (err :: AWS.RequestError | eff) DescribeConstraintOutput
describeConstraint = AWS.request serviceName "DescribeConstraint" 


-- | <p>Gets the status of the specified copy product operation.</p>
describeCopyProductStatus :: forall eff. DescribeCopyProductStatusInput -> Aff (err :: AWS.RequestError | eff) DescribeCopyProductStatusOutput
describeCopyProductStatus = AWS.request serviceName "DescribeCopyProductStatus" 


-- | <p>Gets information about the specified portfolio.</p>
describePortfolio :: forall eff. DescribePortfolioInput -> Aff (err :: AWS.RequestError | eff) DescribePortfolioOutput
describePortfolio = AWS.request serviceName "DescribePortfolio" 


-- | <p>Gets information about the specified product.</p>
describeProduct :: forall eff. DescribeProductInput -> Aff (err :: AWS.RequestError | eff) DescribeProductOutput
describeProduct = AWS.request serviceName "DescribeProduct" 


-- | <p>Gets information about the specified product. This operation is run with administrator access.</p>
describeProductAsAdmin :: forall eff. DescribeProductAsAdminInput -> Aff (err :: AWS.RequestError | eff) DescribeProductAsAdminOutput
describeProductAsAdmin = AWS.request serviceName "DescribeProductAsAdmin" 


-- | <p>Gets information about the specified product.</p>
describeProductView :: forall eff. DescribeProductViewInput -> Aff (err :: AWS.RequestError | eff) DescribeProductViewOutput
describeProductView = AWS.request serviceName "DescribeProductView" 


-- | <p>Gets information about the specified provisioned product.</p>
describeProvisionedProduct :: forall eff. DescribeProvisionedProductInput -> Aff (err :: AWS.RequestError | eff) DescribeProvisionedProductOutput
describeProvisionedProduct = AWS.request serviceName "DescribeProvisionedProduct" 


-- | <p>Gets information about the resource changes for the specified plan.</p>
describeProvisionedProductPlan :: forall eff. DescribeProvisionedProductPlanInput -> Aff (err :: AWS.RequestError | eff) DescribeProvisionedProductPlanOutput
describeProvisionedProductPlan = AWS.request serviceName "DescribeProvisionedProductPlan" 


-- | <p>Gets information about the specified provisioning artifact (also known as a version) for the specified product.</p>
describeProvisioningArtifact :: forall eff. DescribeProvisioningArtifactInput -> Aff (err :: AWS.RequestError | eff) DescribeProvisioningArtifactOutput
describeProvisioningArtifact = AWS.request serviceName "DescribeProvisioningArtifact" 


-- | <p>Gets information about the configuration required to provision the specified product using the specified provisioning artifact.</p> <p>If the output contains a TagOption key with an empty list of values, there is a TagOption conflict for that key. The end user cannot take action to fix the conflict, and launch is not blocked. In subsequent calls to <a>ProvisionProduct</a>, do not include conflicted TagOption keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[<i>N</i>]:<i>Value</i>". Tag the provisioned product with the value <code>sc-tagoption-conflict-portfolioId-productId</code>.</p>
describeProvisioningParameters :: forall eff. DescribeProvisioningParametersInput -> Aff (err :: AWS.RequestError | eff) DescribeProvisioningParametersOutput
describeProvisioningParameters = AWS.request serviceName "DescribeProvisioningParameters" 


-- | <p>Gets information about the specified request operation.</p> <p>Use this operation after calling a request operation (for example, <a>ProvisionProduct</a>, <a>TerminateProvisionedProduct</a>, or <a>UpdateProvisionedProduct</a>). </p>
describeRecord :: forall eff. DescribeRecordInput -> Aff (err :: AWS.RequestError | eff) DescribeRecordOutput
describeRecord = AWS.request serviceName "DescribeRecord" 


-- | <p>Gets information about the specified TagOption.</p>
describeTagOption :: forall eff. DescribeTagOptionInput -> Aff (err :: AWS.RequestError | eff) DescribeTagOptionOutput
describeTagOption = AWS.request serviceName "DescribeTagOption" 


-- | <p>Disassociates a previously associated principal ARN from a specified portfolio.</p>
disassociatePrincipalFromPortfolio :: forall eff. DisassociatePrincipalFromPortfolioInput -> Aff (err :: AWS.RequestError | eff) DisassociatePrincipalFromPortfolioOutput
disassociatePrincipalFromPortfolio = AWS.request serviceName "DisassociatePrincipalFromPortfolio" 


-- | <p>Disassociates the specified product from the specified portfolio. </p>
disassociateProductFromPortfolio :: forall eff. DisassociateProductFromPortfolioInput -> Aff (err :: AWS.RequestError | eff) DisassociateProductFromPortfolioOutput
disassociateProductFromPortfolio = AWS.request serviceName "DisassociateProductFromPortfolio" 


-- | <p>Disassociates the specified TagOption from the specified resource.</p>
disassociateTagOptionFromResource :: forall eff. DisassociateTagOptionFromResourceInput -> Aff (err :: AWS.RequestError | eff) DisassociateTagOptionFromResourceOutput
disassociateTagOptionFromResource = AWS.request serviceName "DisassociateTagOptionFromResource" 


-- | <p>Provisions or modifies a product based on the resource changes for the specified plan.</p>
executeProvisionedProductPlan :: forall eff. ExecuteProvisionedProductPlanInput -> Aff (err :: AWS.RequestError | eff) ExecuteProvisionedProductPlanOutput
executeProvisionedProductPlan = AWS.request serviceName "ExecuteProvisionedProductPlan" 


-- | <p>Lists all portfolios for which sharing was accepted by this account.</p>
listAcceptedPortfolioShares :: forall eff. ListAcceptedPortfolioSharesInput -> Aff (err :: AWS.RequestError | eff) ListAcceptedPortfolioSharesOutput
listAcceptedPortfolioShares = AWS.request serviceName "ListAcceptedPortfolioShares" 


-- | <p>Lists the constraints for the specified portfolio and product.</p>
listConstraintsForPortfolio :: forall eff. ListConstraintsForPortfolioInput -> Aff (err :: AWS.RequestError | eff) ListConstraintsForPortfolioOutput
listConstraintsForPortfolio = AWS.request serviceName "ListConstraintsForPortfolio" 


-- | <p>Lists the paths to the specified product. A path is how the user has access to a specified product, and is necessary when provisioning a product. A path also determines the constraints put on the product.</p>
listLaunchPaths :: forall eff. ListLaunchPathsInput -> Aff (err :: AWS.RequestError | eff) ListLaunchPathsOutput
listLaunchPaths = AWS.request serviceName "ListLaunchPaths" 


-- | <p>Lists the account IDs that have access to the specified portfolio.</p>
listPortfolioAccess :: forall eff. ListPortfolioAccessInput -> Aff (err :: AWS.RequestError | eff) ListPortfolioAccessOutput
listPortfolioAccess = AWS.request serviceName "ListPortfolioAccess" 


-- | <p>Lists all portfolios in the catalog.</p>
listPortfolios :: forall eff. ListPortfoliosInput -> Aff (err :: AWS.RequestError | eff) ListPortfoliosOutput
listPortfolios = AWS.request serviceName "ListPortfolios" 


-- | <p>Lists all portfolios that the specified product is associated with.</p>
listPortfoliosForProduct :: forall eff. ListPortfoliosForProductInput -> Aff (err :: AWS.RequestError | eff) ListPortfoliosForProductOutput
listPortfoliosForProduct = AWS.request serviceName "ListPortfoliosForProduct" 


-- | <p>Lists all principal ARNs associated with the specified portfolio.</p>
listPrincipalsForPortfolio :: forall eff. ListPrincipalsForPortfolioInput -> Aff (err :: AWS.RequestError | eff) ListPrincipalsForPortfolioOutput
listPrincipalsForPortfolio = AWS.request serviceName "ListPrincipalsForPortfolio" 


-- | <p>Lists the plans for the specified provisioned product or all plans the user has access to.</p>
listProvisionedProductPlans :: forall eff. ListProvisionedProductPlansInput -> Aff (err :: AWS.RequestError | eff) ListProvisionedProductPlansOutput
listProvisionedProductPlans = AWS.request serviceName "ListProvisionedProductPlans" 


-- | <p>Lists all provisioning artifacts (also known as versions) for the specified product.</p>
listProvisioningArtifacts :: forall eff. ListProvisioningArtifactsInput -> Aff (err :: AWS.RequestError | eff) ListProvisioningArtifactsOutput
listProvisioningArtifacts = AWS.request serviceName "ListProvisioningArtifacts" 


-- | <p>Lists the specified requests or all performed requests.</p>
listRecordHistory :: forall eff. ListRecordHistoryInput -> Aff (err :: AWS.RequestError | eff) ListRecordHistoryOutput
listRecordHistory = AWS.request serviceName "ListRecordHistory" 


-- | <p>Lists the resources associated with the specified TagOption.</p>
listResourcesForTagOption :: forall eff. ListResourcesForTagOptionInput -> Aff (err :: AWS.RequestError | eff) ListResourcesForTagOptionOutput
listResourcesForTagOption = AWS.request serviceName "ListResourcesForTagOption" 


-- | <p>Lists the specified TagOptions or all TagOptions.</p>
listTagOptions :: forall eff. ListTagOptionsInput -> Aff (err :: AWS.RequestError | eff) ListTagOptionsOutput
listTagOptions = AWS.request serviceName "ListTagOptions" 


-- | <p>Provisions the specified product.</p> <p>A provisioned product is a resourced instance of a product. For example, provisioning a product based on a CloudFormation template launches a CloudFormation stack and its underlying resources. You can check the status of this request using <a>DescribeRecord</a>.</p> <p>If the request contains a tag key with an empty list of values, there is a tag conflict for that key. Do not include conflicted keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[<i>N</i>]:<i>Value</i>".</p>
provisionProduct :: forall eff. ProvisionProductInput -> Aff (err :: AWS.RequestError | eff) ProvisionProductOutput
provisionProduct = AWS.request serviceName "ProvisionProduct" 


-- | <p>Rejects an offer to share the specified portfolio.</p>
rejectPortfolioShare :: forall eff. RejectPortfolioShareInput -> Aff (err :: AWS.RequestError | eff) RejectPortfolioShareOutput
rejectPortfolioShare = AWS.request serviceName "RejectPortfolioShare" 


-- | <p>Lists the provisioned products that are available (not terminated).</p> <p>To use additional filtering, see <a>SearchProvisionedProducts</a>.</p>
scanProvisionedProducts :: forall eff. ScanProvisionedProductsInput -> Aff (err :: AWS.RequestError | eff) ScanProvisionedProductsOutput
scanProvisionedProducts = AWS.request serviceName "ScanProvisionedProducts" 


-- | <p>Gets information about the products to which the caller has access.</p>
searchProducts :: forall eff. SearchProductsInput -> Aff (err :: AWS.RequestError | eff) SearchProductsOutput
searchProducts = AWS.request serviceName "SearchProducts" 


-- | <p>Gets information about the products for the specified portfolio or all products.</p>
searchProductsAsAdmin :: forall eff. SearchProductsAsAdminInput -> Aff (err :: AWS.RequestError | eff) SearchProductsAsAdminOutput
searchProductsAsAdmin = AWS.request serviceName "SearchProductsAsAdmin" 


-- | <p>Gets information about the provisioned products that meet the specified criteria.</p>
searchProvisionedProducts :: forall eff. SearchProvisionedProductsInput -> Aff (err :: AWS.RequestError | eff) SearchProvisionedProductsOutput
searchProvisionedProducts = AWS.request serviceName "SearchProvisionedProducts" 


-- | <p>Terminates the specified provisioned product.</p> <p>This operation does not delete any records associated with the provisioned product.</p> <p>You can check the status of this request using <a>DescribeRecord</a>.</p>
terminateProvisionedProduct :: forall eff. TerminateProvisionedProductInput -> Aff (err :: AWS.RequestError | eff) TerminateProvisionedProductOutput
terminateProvisionedProduct = AWS.request serviceName "TerminateProvisionedProduct" 


-- | <p>Updates the specified constraint.</p>
updateConstraint :: forall eff. UpdateConstraintInput -> Aff (err :: AWS.RequestError | eff) UpdateConstraintOutput
updateConstraint = AWS.request serviceName "UpdateConstraint" 


-- | <p>Updates the specified portfolio.</p> <p>You cannot update a product that was shared with you.</p>
updatePortfolio :: forall eff. UpdatePortfolioInput -> Aff (err :: AWS.RequestError | eff) UpdatePortfolioOutput
updatePortfolio = AWS.request serviceName "UpdatePortfolio" 


-- | <p>Updates the specified product.</p>
updateProduct :: forall eff. UpdateProductInput -> Aff (err :: AWS.RequestError | eff) UpdateProductOutput
updateProduct = AWS.request serviceName "UpdateProduct" 


-- | <p>Requests updates to the configuration of the specified provisioned product.</p> <p>If there are tags associated with the object, they cannot be updated or added. Depending on the specific updates requested, this operation can update with no interruption, with some interruption, or replace the provisioned product entirely.</p> <p>You can check the status of this request using <a>DescribeRecord</a>.</p>
updateProvisionedProduct :: forall eff. UpdateProvisionedProductInput -> Aff (err :: AWS.RequestError | eff) UpdateProvisionedProductOutput
updateProvisionedProduct = AWS.request serviceName "UpdateProvisionedProduct" 


-- | <p>Updates the specified provisioning artifact (also known as a version) for the specified product.</p> <p>You cannot update a provisioning artifact for a product that was shared with you.</p>
updateProvisioningArtifact :: forall eff. UpdateProvisioningArtifactInput -> Aff (err :: AWS.RequestError | eff) UpdateProvisioningArtifactOutput
updateProvisioningArtifact = AWS.request serviceName "UpdateProvisioningArtifact" 


-- | <p>Updates the specified TagOption.</p>
updateTagOption :: forall eff. UpdateTagOptionInput -> Aff (err :: AWS.RequestError | eff) UpdateTagOptionOutput
updateTagOption = AWS.request serviceName "UpdateTagOption" 


newtype AcceptLanguage = AcceptLanguage String


newtype AcceptPortfolioShareInput = AcceptPortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }


newtype AcceptPortfolioShareOutput = AcceptPortfolioShareOutput 
  { 
  }


-- | <p>The access level to use to filter results.</p>
newtype AccessLevelFilter = AccessLevelFilter 
  { "Key" :: NullOrUndefined (AccessLevelFilterKey)
  , "Value" :: NullOrUndefined (AccessLevelFilterValue)
  }


newtype AccessLevelFilterKey = AccessLevelFilterKey String


newtype AccessLevelFilterValue = AccessLevelFilterValue String


newtype AccountId = AccountId String


newtype AccountIds = AccountIds (Array AccountId)


newtype AddTags = AddTags (Array Tag)


newtype AllowedValue = AllowedValue String


newtype AllowedValues = AllowedValues (Array AllowedValue)


newtype ApproximateCount = ApproximateCount Int


newtype AssociatePrincipalWithPortfolioInput = AssociatePrincipalWithPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PrincipalARN" :: (PrincipalARN)
  , "PrincipalType" :: (PrincipalType)
  }


newtype AssociatePrincipalWithPortfolioOutput = AssociatePrincipalWithPortfolioOutput 
  { 
  }


newtype AssociateProductWithPortfolioInput = AssociateProductWithPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PortfolioId" :: (Id)
  , "SourcePortfolioId" :: NullOrUndefined (Id)
  }


newtype AssociateProductWithPortfolioOutput = AssociateProductWithPortfolioOutput 
  { 
  }


newtype AssociateTagOptionWithResourceInput = AssociateTagOptionWithResourceInput 
  { "ResourceId" :: (ResourceId)
  , "TagOptionId" :: (TagOptionId)
  }


newtype AssociateTagOptionWithResourceOutput = AssociateTagOptionWithResourceOutput 
  { 
  }


newtype AttributeValue = AttributeValue String


newtype CausingEntity = CausingEntity String


newtype ChangeAction = ChangeAction String


-- | <p>Information about a CloudWatch dashboard.</p>
newtype CloudWatchDashboard = CloudWatchDashboard 
  { "Name" :: NullOrUndefined (CloudWatchDashboardName)
  }


newtype CloudWatchDashboardName = CloudWatchDashboardName String


newtype CloudWatchDashboards = CloudWatchDashboards (Array CloudWatchDashboard)


newtype ConstraintDescription = ConstraintDescription String


-- | <p>Information about a constraint.</p>
newtype ConstraintDetail = ConstraintDetail 
  { "ConstraintId" :: NullOrUndefined (Id)
  , "Type" :: NullOrUndefined (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  , "Owner" :: NullOrUndefined (AccountId)
  }


newtype ConstraintDetails = ConstraintDetails (Array ConstraintDetail)


newtype ConstraintParameters = ConstraintParameters String


newtype ConstraintSummaries = ConstraintSummaries (Array ConstraintSummary)


-- | <p>Summary information about a constraint.</p>
newtype ConstraintSummary = ConstraintSummary 
  { "Type" :: NullOrUndefined (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  }


newtype ConstraintType = ConstraintType String


newtype CopyOption = CopyOption String


newtype CopyOptions = CopyOptions (Array CopyOption)


newtype CopyProductInput = CopyProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "SourceProductArn" :: (ProductArn)
  , "TargetProductId" :: NullOrUndefined (Id)
  , "TargetProductName" :: NullOrUndefined (ProductViewName)
  , "SourceProvisioningArtifactIdentifiers" :: NullOrUndefined (SourceProvisioningArtifactProperties)
  , "CopyOptions" :: NullOrUndefined (CopyOptions)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype CopyProductOutput = CopyProductOutput 
  { "CopyProductToken" :: NullOrUndefined (Id)
  }


newtype CopyProductStatus = CopyProductStatus String


newtype CreateConstraintInput = CreateConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "ProductId" :: (Id)
  , "Parameters" :: (ConstraintParameters)
  , "Type" :: (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype CreateConstraintOutput = CreateConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }


newtype CreatePortfolioInput = CreatePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "DisplayName" :: (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "ProviderName" :: (ProviderName)
  , "Tags" :: NullOrUndefined (AddTags)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype CreatePortfolioOutput = CreatePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype CreatePortfolioShareInput = CreatePortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "AccountId" :: (AccountId)
  }


newtype CreatePortfolioShareOutput = CreatePortfolioShareOutput 
  { 
  }


newtype CreateProductInput = CreateProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Name" :: (ProductViewName)
  , "Owner" :: (ProductViewOwner)
  , "Description" :: NullOrUndefined (ProductViewShortDescription)
  , "Distributor" :: NullOrUndefined (ProductViewOwner)
  , "SupportDescription" :: NullOrUndefined (SupportDescription)
  , "SupportEmail" :: NullOrUndefined (SupportEmail)
  , "SupportUrl" :: NullOrUndefined (SupportUrl)
  , "ProductType" :: (ProductType)
  , "Tags" :: NullOrUndefined (AddTags)
  , "ProvisioningArtifactParameters" :: (ProvisioningArtifactProperties)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype CreateProductOutput = CreateProductOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype CreateProvisionedProductPlanInput = CreateProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanName" :: (ProvisionedProductPlanName)
  , "PlanType" :: (ProvisionedProductPlanType)
  , "NotificationArns" :: NullOrUndefined (NotificationArns)
  , "PathId" :: NullOrUndefined (Id)
  , "ProductId" :: (Id)
  , "ProvisionedProductName" :: (ProvisionedProductName)
  , "ProvisioningArtifactId" :: (Id)
  , "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters)
  , "IdempotencyToken" :: (IdempotencyToken)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype CreateProvisionedProductPlanOutput = CreateProvisionedProductPlanOutput 
  { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName)
  , "PlanId" :: NullOrUndefined (Id)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  }


newtype CreateProvisioningArtifactInput = CreateProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "Parameters" :: (ProvisioningArtifactProperties)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype CreateProvisioningArtifactOutput = CreateProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }


newtype CreateTagOptionInput = CreateTagOptionInput 
  { "Key" :: (TagOptionKey)
  , "Value" :: (TagOptionValue)
  }


newtype CreateTagOptionOutput = CreateTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }


newtype CreatedTime = CreatedTime Number


newtype CreationTime = CreationTime Number


newtype DefaultValue = DefaultValue String


newtype DeleteConstraintInput = DeleteConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DeleteConstraintOutput = DeleteConstraintOutput 
  { 
  }


newtype DeletePortfolioInput = DeletePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DeletePortfolioOutput = DeletePortfolioOutput 
  { 
  }


newtype DeletePortfolioShareInput = DeletePortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "AccountId" :: (AccountId)
  }


newtype DeletePortfolioShareOutput = DeletePortfolioShareOutput 
  { 
  }


newtype DeleteProductInput = DeleteProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DeleteProductOutput = DeleteProductOutput 
  { 
  }


newtype DeleteProvisionedProductPlanInput = DeleteProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "IgnoreErrors" :: NullOrUndefined (IgnoreErrors)
  }


newtype DeleteProvisionedProductPlanOutput = DeleteProvisionedProductPlanOutput 
  { 
  }


newtype DeleteProvisioningArtifactInput = DeleteProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  }


newtype DeleteProvisioningArtifactOutput = DeleteProvisioningArtifactOutput 
  { 
  }


newtype DescribeConstraintInput = DescribeConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribeConstraintOutput = DescribeConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }


newtype DescribeCopyProductStatusInput = DescribeCopyProductStatusInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "CopyProductToken" :: (Id)
  }


newtype DescribeCopyProductStatusOutput = DescribeCopyProductStatusOutput 
  { "CopyProductStatus" :: NullOrUndefined (CopyProductStatus)
  , "TargetProductId" :: NullOrUndefined (Id)
  , "StatusDetail" :: NullOrUndefined (StatusDetail)
  }


newtype DescribePortfolioInput = DescribePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribePortfolioOutput = DescribePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  , "TagOptions" :: NullOrUndefined (TagOptionDetails)
  }


newtype DescribeProductAsAdminInput = DescribeProductAsAdminInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribeProductAsAdminOutput = DescribeProductAsAdminOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "ProvisioningArtifactSummaries" :: NullOrUndefined (ProvisioningArtifactSummaries)
  , "Tags" :: NullOrUndefined (Tags)
  , "TagOptions" :: NullOrUndefined (TagOptionDetails)
  }


newtype DescribeProductInput = DescribeProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribeProductOutput = DescribeProductOutput 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts)
  }


newtype DescribeProductViewInput = DescribeProductViewInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribeProductViewOutput = DescribeProductViewOutput 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts)
  }


newtype DescribeProvisionedProductInput = DescribeProvisionedProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }


newtype DescribeProvisionedProductOutput = DescribeProvisionedProductOutput 
  { "ProvisionedProductDetail" :: NullOrUndefined (ProvisionedProductDetail)
  , "CloudWatchDashboards" :: NullOrUndefined (CloudWatchDashboards)
  }


newtype DescribeProvisionedProductPlanInput = DescribeProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype DescribeProvisionedProductPlanOutput = DescribeProvisionedProductPlanOutput 
  { "ProvisionedProductPlanDetails" :: NullOrUndefined (ProvisionedProductPlanDetails)
  , "ResourceChanges" :: NullOrUndefined (ResourceChanges)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype DescribeProvisioningArtifactInput = DescribeProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProvisioningArtifactId" :: (Id)
  , "ProductId" :: (Id)
  , "Verbose" :: NullOrUndefined (Verbose)
  }


newtype DescribeProvisioningArtifactOutput = DescribeProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }


newtype DescribeProvisioningParametersInput = DescribeProvisioningParametersInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  , "PathId" :: NullOrUndefined (Id)
  }


newtype DescribeProvisioningParametersOutput = DescribeProvisioningParametersOutput 
  { "ProvisioningArtifactParameters" :: NullOrUndefined (ProvisioningArtifactParameters)
  , "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries)
  , "UsageInstructions" :: NullOrUndefined (UsageInstructions)
  , "TagOptions" :: NullOrUndefined (TagOptionSummaries)
  }


newtype DescribeRecordInput = DescribeRecordInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeRecordOutput = DescribeRecordOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  , "RecordOutputs" :: NullOrUndefined (RecordOutputs)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype DescribeTagOptionInput = DescribeTagOptionInput 
  { "Id" :: (TagOptionId)
  }


newtype DescribeTagOptionOutput = DescribeTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }


newtype Description = Description String


newtype DisassociatePrincipalFromPortfolioInput = DisassociatePrincipalFromPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PrincipalARN" :: (PrincipalARN)
  }


newtype DisassociatePrincipalFromPortfolioOutput = DisassociatePrincipalFromPortfolioOutput 
  { 
  }


newtype DisassociateProductFromPortfolioInput = DisassociateProductFromPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PortfolioId" :: (Id)
  }


newtype DisassociateProductFromPortfolioOutput = DisassociateProductFromPortfolioOutput 
  { 
  }


newtype DisassociateTagOptionFromResourceInput = DisassociateTagOptionFromResourceInput 
  { "ResourceId" :: (ResourceId)
  , "TagOptionId" :: (TagOptionId)
  }


newtype DisassociateTagOptionFromResourceOutput = DisassociateTagOptionFromResourceOutput 
  { 
  }


-- | <p>The specified resource is a duplicate.</p>
newtype DuplicateResourceException = DuplicateResourceException 
  { 
  }


newtype ErrorCode = ErrorCode String


newtype ErrorDescription = ErrorDescription String


newtype EvaluationType = EvaluationType String


newtype ExecuteProvisionedProductPlanInput = ExecuteProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "IdempotencyToken" :: (IdempotencyToken)
  }


newtype ExecuteProvisionedProductPlanOutput = ExecuteProvisionedProductPlanOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }


newtype HasDefaultPath = HasDefaultPath Boolean


newtype Id = Id String


newtype IdempotencyToken = IdempotencyToken String


newtype IgnoreErrors = IgnoreErrors Boolean


newtype InstructionType = InstructionType String


newtype InstructionValue = InstructionValue String


-- | <p>One or more parameters provided to the operation are not valid.</p>
newtype InvalidParametersException = InvalidParametersException 
  { 
  }


-- | <p>An attempt was made to modify a resource that is in a state that is not valid. Check your resources to ensure that they are in valid states before retrying the operation.</p>
newtype InvalidStateException = InvalidStateException 
  { 
  }


newtype LastRequestId = LastRequestId String


newtype LaunchPathSummaries = LaunchPathSummaries (Array LaunchPathSummary)


-- | <p>Summary information about a product path for a user.</p>
newtype LaunchPathSummary = LaunchPathSummary 
  { "Id" :: NullOrUndefined (Id)
  , "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries)
  , "Tags" :: NullOrUndefined (Tags)
  , "Name" :: NullOrUndefined (PortfolioName)
  }


-- | <p>The current limits of the service would have been exceeded by this operation. Decrease your resource use or increase your service limits and retry the operation.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


newtype ListAcceptedPortfolioSharesInput = ListAcceptedPortfolioSharesInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype ListAcceptedPortfolioSharesOutput = ListAcceptedPortfolioSharesOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListConstraintsForPortfolioInput = ListConstraintsForPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListConstraintsForPortfolioOutput = ListConstraintsForPortfolioOutput 
  { "ConstraintDetails" :: NullOrUndefined (ConstraintDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListLaunchPathsInput = ListLaunchPathsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListLaunchPathsOutput = ListLaunchPathsOutput 
  { "LaunchPathSummaries" :: NullOrUndefined (LaunchPathSummaries)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListPortfolioAccessInput = ListPortfolioAccessInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }


newtype ListPortfolioAccessOutput = ListPortfolioAccessOutput 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListPortfoliosForProductInput = ListPortfoliosForProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype ListPortfoliosForProductOutput = ListPortfoliosForProductOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListPortfoliosInput = ListPortfoliosInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype ListPortfoliosOutput = ListPortfoliosOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListPrincipalsForPortfolioInput = ListPrincipalsForPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListPrincipalsForPortfolioOutput = ListPrincipalsForPortfolioOutput 
  { "Principals" :: NullOrUndefined (Principals)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListProvisionedProductPlansInput = ListProvisionedProductPlansInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  }


newtype ListProvisionedProductPlansOutput = ListProvisionedProductPlansOutput 
  { "ProvisionedProductPlans" :: NullOrUndefined (ProvisionedProductPlans)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListProvisioningArtifactsInput = ListProvisioningArtifactsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  }


newtype ListProvisioningArtifactsOutput = ListProvisioningArtifactsOutput 
  { "ProvisioningArtifactDetails" :: NullOrUndefined (ProvisioningArtifactDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype ListRecordHistoryInput = ListRecordHistoryInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "SearchFilter" :: NullOrUndefined (ListRecordHistorySearchFilter)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListRecordHistoryOutput = ListRecordHistoryOutput 
  { "RecordDetails" :: NullOrUndefined (RecordDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


-- | <p>The search filter to use when listing history records.</p>
newtype ListRecordHistorySearchFilter = ListRecordHistorySearchFilter 
  { "Key" :: NullOrUndefined (SearchFilterKey)
  , "Value" :: NullOrUndefined (SearchFilterValue)
  }


newtype ListResourcesForTagOptionInput = ListResourcesForTagOptionInput 
  { "TagOptionId" :: (TagOptionId)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListResourcesForTagOptionOutput = ListResourcesForTagOptionOutput 
  { "ResourceDetails" :: NullOrUndefined (ResourceDetails)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


-- | <p>Filters to use when listing TagOptions.</p>
newtype ListTagOptionsFilters = ListTagOptionsFilters 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  }


newtype ListTagOptionsInput = ListTagOptionsInput 
  { "Filters" :: NullOrUndefined (ListTagOptionsFilters)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ListTagOptionsOutput = ListTagOptionsOutput 
  { "TagOptionDetails" :: NullOrUndefined (TagOptionDetails)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype LogicalResourceId = LogicalResourceId String


newtype NoEcho = NoEcho Boolean


newtype NotificationArn = NotificationArn String


newtype NotificationArns = NotificationArns (Array NotificationArn)


newtype OutputKey = OutputKey String


newtype OutputValue = OutputValue String


newtype PageSize = PageSize Int


newtype PageToken = PageToken String


-- | <p>The constraints that the administrator has put on the parameter.</p>
newtype ParameterConstraints = ParameterConstraints 
  { "AllowedValues" :: NullOrUndefined (AllowedValues)
  }


newtype ParameterKey = ParameterKey String


newtype ParameterType = ParameterType String


newtype ParameterValue = ParameterValue String


newtype PhysicalId = PhysicalId String


newtype PhysicalResourceId = PhysicalResourceId String


newtype PlanResourceType = PlanResourceType String


newtype PortfolioDescription = PortfolioDescription String


-- | <p>Information about a portfolio.</p>
newtype PortfolioDetail = PortfolioDetail 
  { "Id" :: NullOrUndefined (Id)
  , "ARN" :: NullOrUndefined (ResourceARN)
  , "DisplayName" :: NullOrUndefined (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "CreatedTime" :: NullOrUndefined (CreationTime)
  , "ProviderName" :: NullOrUndefined (ProviderName)
  }


newtype PortfolioDetails = PortfolioDetails (Array PortfolioDetail)


newtype PortfolioDisplayName = PortfolioDisplayName String


newtype PortfolioName = PortfolioName String


-- | <p>Information about a principal.</p>
newtype Principal = Principal 
  { "PrincipalARN" :: NullOrUndefined (PrincipalARN)
  , "PrincipalType" :: NullOrUndefined (PrincipalType)
  }


newtype PrincipalARN = PrincipalARN String


newtype PrincipalType = PrincipalType String


newtype Principals = Principals (Array Principal)


newtype ProductArn = ProductArn String


newtype ProductSource = ProductSource String


newtype ProductType = ProductType String


newtype ProductViewAggregationType = ProductViewAggregationType String


-- | <p>A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.</p>
newtype ProductViewAggregationValue = ProductViewAggregationValue 
  { "Value" :: NullOrUndefined (AttributeValue)
  , "ApproximateCount" :: NullOrUndefined (ApproximateCount)
  }


newtype ProductViewAggregationValues = ProductViewAggregationValues (Array ProductViewAggregationValue)


newtype ProductViewAggregations = ProductViewAggregations (Map ProductViewAggregationType ProductViewAggregationValues)


-- | <p>Information about a product view.</p>
newtype ProductViewDetail = ProductViewDetail 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "Status" :: NullOrUndefined (Status)
  , "ProductARN" :: NullOrUndefined (ResourceARN)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  }


newtype ProductViewDetails = ProductViewDetails (Array ProductViewDetail)


newtype ProductViewDistributor = ProductViewDistributor String


newtype ProductViewFilterBy = ProductViewFilterBy String


newtype ProductViewFilterValue = ProductViewFilterValue String


newtype ProductViewFilterValues = ProductViewFilterValues (Array ProductViewFilterValue)


newtype ProductViewFilters = ProductViewFilters (Map ProductViewFilterBy ProductViewFilterValues)


newtype ProductViewName = ProductViewName String


newtype ProductViewOwner = ProductViewOwner String


newtype ProductViewShortDescription = ProductViewShortDescription String


newtype ProductViewSortBy = ProductViewSortBy String


newtype ProductViewSummaries = ProductViewSummaries (Array ProductViewSummary)


-- | <p>Summary information about a product view.</p>
newtype ProductViewSummary = ProductViewSummary 
  { "Id" :: NullOrUndefined (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProductViewName)
  , "Owner" :: NullOrUndefined (ProductViewOwner)
  , "ShortDescription" :: NullOrUndefined (ProductViewShortDescription)
  , "Type" :: NullOrUndefined (ProductType)
  , "Distributor" :: NullOrUndefined (ProductViewDistributor)
  , "HasDefaultPath" :: NullOrUndefined (HasDefaultPath)
  , "SupportEmail" :: NullOrUndefined (SupportEmail)
  , "SupportDescription" :: NullOrUndefined (SupportDescription)
  , "SupportUrl" :: NullOrUndefined (SupportUrl)
  }


newtype PropertyName = PropertyName String


newtype ProviderName = ProviderName String


newtype ProvisionProductInput = ProvisionProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  , "PathId" :: NullOrUndefined (Id)
  , "ProvisionedProductName" :: (ProvisionedProductName)
  , "ProvisioningParameters" :: NullOrUndefined (ProvisioningParameters)
  , "Tags" :: NullOrUndefined (Tags)
  , "NotificationArns" :: NullOrUndefined (NotificationArns)
  , "ProvisionToken" :: (IdempotencyToken)
  }


newtype ProvisionProductOutput = ProvisionProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }


-- | <p>Information about a provisioned product.</p>
newtype ProvisionedProductAttribute = ProvisionedProductAttribute 
  { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "Type" :: NullOrUndefined (ProvisionedProductType)
  , "Id" :: NullOrUndefined (Id)
  , "Status" :: NullOrUndefined (ProvisionedProductStatus)
  , "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "IdempotencyToken" :: NullOrUndefined (IdempotencyToken)
  , "LastRecordId" :: NullOrUndefined (Id)
  , "Tags" :: NullOrUndefined (Tags)
  , "PhysicalId" :: NullOrUndefined (PhysicalId)
  , "ProductId" :: NullOrUndefined (Id)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  , "UserArn" :: NullOrUndefined (UserArn)
  , "UserArnSession" :: NullOrUndefined (UserArnSession)
  }


newtype ProvisionedProductAttributes = ProvisionedProductAttributes (Array ProvisionedProductAttribute)


-- | <p>Information about a provisioned product.</p>
newtype ProvisionedProductDetail = ProvisionedProductDetail 
  { "Name" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "Arn" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "Type" :: NullOrUndefined (ProvisionedProductType)
  , "Id" :: NullOrUndefined (ProvisionedProductId)
  , "Status" :: NullOrUndefined (ProvisionedProductStatus)
  , "StatusMessage" :: NullOrUndefined (ProvisionedProductStatusMessage)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "IdempotencyToken" :: NullOrUndefined (IdempotencyToken)
  , "LastRecordId" :: NullOrUndefined (LastRequestId)
  }


newtype ProvisionedProductDetails = ProvisionedProductDetails (Array ProvisionedProductDetail)


newtype ProvisionedProductFilters = ProvisionedProductFilters (Map ProvisionedProductViewFilterBy ProvisionedProductViewFilterValues)


newtype ProvisionedProductId = ProvisionedProductId String


newtype ProvisionedProductName = ProvisionedProductName String


newtype ProvisionedProductNameOrArn = ProvisionedProductNameOrArn String


-- | <p>Information about a plan.</p>
newtype ProvisionedProductPlanDetails = ProvisionedProductPlanDetails 
  { "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "PathId" :: NullOrUndefined (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "PlanName" :: NullOrUndefined (ProvisionedProductPlanName)
  , "PlanId" :: NullOrUndefined (Id)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName)
  , "PlanType" :: NullOrUndefined (ProvisionedProductPlanType)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  , "Status" :: NullOrUndefined (ProvisionedProductPlanStatus)
  , "UpdatedTime" :: NullOrUndefined (UpdatedTime)
  , "NotificationArns" :: NullOrUndefined (NotificationArns)
  , "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters)
  , "Tags" :: NullOrUndefined (Tags)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  }


newtype ProvisionedProductPlanName = ProvisionedProductPlanName String


newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus String


-- | <p>Summary information about a plan.</p>
newtype ProvisionedProductPlanSummary = ProvisionedProductPlanSummary 
  { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName)
  , "PlanId" :: NullOrUndefined (Id)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName)
  , "PlanType" :: NullOrUndefined (ProvisionedProductPlanType)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  }


newtype ProvisionedProductPlanType = ProvisionedProductPlanType String


newtype ProvisionedProductPlans = ProvisionedProductPlans (Array ProvisionedProductPlanSummary)


newtype ProvisionedProductStatus = ProvisionedProductStatus String


newtype ProvisionedProductStatusMessage = ProvisionedProductStatusMessage String


newtype ProvisionedProductType = ProvisionedProductType String


newtype ProvisionedProductViewFilterBy = ProvisionedProductViewFilterBy String


newtype ProvisionedProductViewFilterValue = ProvisionedProductViewFilterValue String


newtype ProvisionedProductViewFilterValues = ProvisionedProductViewFilterValues (Array ProvisionedProductViewFilterValue)


-- | <p>Information about a provisioning artifact. A provisioning artifact is also known as a product version.</p>
newtype ProvisioningArtifact = ProvisioningArtifact 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime)
  }


newtype ProvisioningArtifactActive = ProvisioningArtifactActive Boolean


newtype ProvisioningArtifactCreatedTime = ProvisioningArtifactCreatedTime Number


newtype ProvisioningArtifactDescription = ProvisioningArtifactDescription String


-- | <p>Information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactDetail = ProvisioningArtifactDetail 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactName)
  , "Type" :: NullOrUndefined (ProvisioningArtifactType)
  , "CreatedTime" :: NullOrUndefined (CreationTime)
  , "Active" :: NullOrUndefined (ProvisioningArtifactActive)
  }


newtype ProvisioningArtifactDetails = ProvisioningArtifactDetails (Array ProvisioningArtifactDetail)


newtype ProvisioningArtifactInfo = ProvisioningArtifactInfo (Map ProvisioningArtifactInfoKey ProvisioningArtifactInfoValue)


newtype ProvisioningArtifactInfoKey = ProvisioningArtifactInfoKey String


newtype ProvisioningArtifactInfoValue = ProvisioningArtifactInfoValue String


newtype ProvisioningArtifactName = ProvisioningArtifactName String


-- | <p>Information about a parameter used to provision a product.</p>
newtype ProvisioningArtifactParameter = ProvisioningArtifactParameter 
  { "ParameterKey" :: NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined (DefaultValue)
  , "ParameterType" :: NullOrUndefined (ParameterType)
  , "IsNoEcho" :: NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined (Description)
  , "ParameterConstraints" :: NullOrUndefined (ParameterConstraints)
  }


newtype ProvisioningArtifactParameters = ProvisioningArtifactParameters (Array ProvisioningArtifactParameter)


-- | <p>Information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactProperties = ProvisioningArtifactProperties 
  { "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "Info" :: (ProvisioningArtifactInfo)
  , "Type" :: NullOrUndefined (ProvisioningArtifactType)
  }


newtype ProvisioningArtifactPropertyName = ProvisioningArtifactPropertyName String


newtype ProvisioningArtifactPropertyValue = ProvisioningArtifactPropertyValue String


newtype ProvisioningArtifactSummaries = ProvisioningArtifactSummaries (Array ProvisioningArtifactSummary)


-- | <p>Summary information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactSummary = ProvisioningArtifactSummary 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime)
  , "ProvisioningArtifactMetadata" :: NullOrUndefined (ProvisioningArtifactInfo)
  }


newtype ProvisioningArtifactType = ProvisioningArtifactType String


newtype ProvisioningArtifacts = ProvisioningArtifacts (Array ProvisioningArtifact)


-- | <p>Information about a parameter used to provision a product.</p>
newtype ProvisioningParameter = ProvisioningParameter 
  { "Key" :: NullOrUndefined (ParameterKey)
  , "Value" :: NullOrUndefined (ParameterValue)
  }


newtype ProvisioningParameters = ProvisioningParameters (Array ProvisioningParameter)


-- | <p>Information about a request operation.</p>
newtype RecordDetail = RecordDetail 
  { "RecordId" :: NullOrUndefined (Id)
  , "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName)
  , "Status" :: NullOrUndefined (RecordStatus)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "UpdatedTime" :: NullOrUndefined (UpdatedTime)
  , "ProvisionedProductType" :: NullOrUndefined (ProvisionedProductType)
  , "RecordType" :: NullOrUndefined (RecordType)
  , "ProvisionedProductId" :: NullOrUndefined (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  , "PathId" :: NullOrUndefined (Id)
  , "RecordErrors" :: NullOrUndefined (RecordErrors)
  , "RecordTags" :: NullOrUndefined (RecordTags)
  }


newtype RecordDetails = RecordDetails (Array RecordDetail)


-- | <p>The error code and description resulting from an operation.</p>
newtype RecordError = RecordError 
  { "Code" :: NullOrUndefined (ErrorCode)
  , "Description" :: NullOrUndefined (ErrorDescription)
  }


newtype RecordErrors = RecordErrors (Array RecordError)


-- | <p>The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.</p>
newtype RecordOutput = RecordOutput 
  { "OutputKey" :: NullOrUndefined (OutputKey)
  , "OutputValue" :: NullOrUndefined (OutputValue)
  , "Description" :: NullOrUndefined (Description)
  }


newtype RecordOutputs = RecordOutputs (Array RecordOutput)


newtype RecordStatus = RecordStatus String


-- | <p>Information about a tag, which is a key-value pair.</p>
newtype RecordTag = RecordTag 
  { "Key" :: NullOrUndefined (RecordTagKey)
  , "Value" :: NullOrUndefined (RecordTagValue)
  }


newtype RecordTagKey = RecordTagKey String


newtype RecordTagValue = RecordTagValue String


newtype RecordTags = RecordTags (Array RecordTag)


newtype RecordType = RecordType String


newtype RejectPortfolioShareInput = RejectPortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }


newtype RejectPortfolioShareOutput = RejectPortfolioShareOutput 
  { 
  }


newtype Replacement = Replacement String


newtype RequiresRecreation = RequiresRecreation String


newtype ResourceARN = ResourceARN String


newtype ResourceAttribute = ResourceAttribute String


-- | <p>Information about a resource change that will occur when a plan is executed.</p>
newtype ResourceChange = ResourceChange 
  { "Action" :: NullOrUndefined (ChangeAction)
  , "LogicalResourceId" :: NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: NullOrUndefined (PlanResourceType)
  , "Replacement" :: NullOrUndefined (Replacement)
  , "Scope" :: NullOrUndefined (Scope)
  , "Details" :: NullOrUndefined (ResourceChangeDetails)
  }


-- | <p>Information about a change to a resource attribute.</p>
newtype ResourceChangeDetail = ResourceChangeDetail 
  { "Target" :: NullOrUndefined (ResourceTargetDefinition)
  , "Evaluation" :: NullOrUndefined (EvaluationType)
  , "CausingEntity" :: NullOrUndefined (CausingEntity)
  }


newtype ResourceChangeDetails = ResourceChangeDetails (Array ResourceChangeDetail)


newtype ResourceChanges = ResourceChanges (Array ResourceChange)


-- | <p>Information about a resource.</p>
newtype ResourceDetail = ResourceDetail 
  { "Id" :: NullOrUndefined (ResourceDetailId)
  , "ARN" :: NullOrUndefined (ResourceDetailARN)
  , "Name" :: NullOrUndefined (ResourceDetailName)
  , "Description" :: NullOrUndefined (ResourceDetailDescription)
  , "CreatedTime" :: NullOrUndefined (ResourceDetailCreatedTime)
  }


newtype ResourceDetailARN = ResourceDetailARN String


newtype ResourceDetailCreatedTime = ResourceDetailCreatedTime Number


newtype ResourceDetailDescription = ResourceDetailDescription String


newtype ResourceDetailId = ResourceDetailId String


newtype ResourceDetailName = ResourceDetailName String


newtype ResourceDetails = ResourceDetails (Array ResourceDetail)


newtype ResourceId = ResourceId String


-- | <p>A resource that is currently in use. Ensure that the resource is not in use and retry the operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }


-- | <p>The specified resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


-- | <p>Information about a change to a resource attribute.</p>
newtype ResourceTargetDefinition = ResourceTargetDefinition 
  { "Attribute" :: NullOrUndefined (ResourceAttribute)
  , "Name" :: NullOrUndefined (PropertyName)
  , "RequiresRecreation" :: NullOrUndefined (RequiresRecreation)
  }


newtype ResourceType = ResourceType String


newtype ScanProvisionedProductsInput = ScanProvisionedProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype ScanProvisionedProductsOutput = ScanProvisionedProductsOutput 
  { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype Scope = Scope (Array ResourceAttribute)


newtype SearchFilterKey = SearchFilterKey String


newtype SearchFilterValue = SearchFilterValue String


newtype SearchProductsAsAdminInput = SearchProductsAsAdminInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: NullOrUndefined (Id)
  , "Filters" :: NullOrUndefined (ProductViewFilters)
  , "SortBy" :: NullOrUndefined (ProductViewSortBy)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "ProductSource" :: NullOrUndefined (ProductSource)
  }


newtype SearchProductsAsAdminOutput = SearchProductsAsAdminOutput 
  { "ProductViewDetails" :: NullOrUndefined (ProductViewDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype SearchProductsInput = SearchProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Filters" :: NullOrUndefined (ProductViewFilters)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "SortBy" :: NullOrUndefined (ProductViewSortBy)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype SearchProductsOutput = SearchProductsOutput 
  { "ProductViewSummaries" :: NullOrUndefined (ProductViewSummaries)
  , "ProductViewAggregations" :: NullOrUndefined (ProductViewAggregations)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype SearchProvisionedProductsInput = SearchProvisionedProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "Filters" :: NullOrUndefined (ProvisionedProductFilters)
  , "SortBy" :: NullOrUndefined (SortField)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "PageSize" :: NullOrUndefined (SearchProvisionedProductsPageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }


newtype SearchProvisionedProductsOutput = SearchProvisionedProductsOutput 
  { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductAttributes)
  , "TotalResultsCount" :: NullOrUndefined (TotalResultsCount)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }


newtype SearchProvisionedProductsPageSize = SearchProvisionedProductsPageSize Int


newtype SortField = SortField String


newtype SortOrder = SortOrder String


newtype SourceProvisioningArtifactProperties = SourceProvisioningArtifactProperties (Array SourceProvisioningArtifactPropertiesMap)


newtype SourceProvisioningArtifactPropertiesMap = SourceProvisioningArtifactPropertiesMap (Map ProvisioningArtifactPropertyName ProvisioningArtifactPropertyValue)


newtype Status = Status String


newtype StatusDetail = StatusDetail String


newtype StatusMessage = StatusMessage String


newtype SupportDescription = SupportDescription String


newtype SupportEmail = SupportEmail String


newtype SupportUrl = SupportUrl String


-- | <p>Information about a tag. A tag is a key-value pair. Tags are propagated to the resources created when provisioning a product.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeys = TagKeys (Array TagKey)


newtype TagOptionActive = TagOptionActive Boolean


-- | <p>Information about a TagOption.</p>
newtype TagOptionDetail = TagOptionDetail 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  , "Id" :: NullOrUndefined (TagOptionId)
  }


newtype TagOptionDetails = TagOptionDetails (Array TagOptionDetail)


newtype TagOptionId = TagOptionId String


newtype TagOptionKey = TagOptionKey String


-- | <p>An operation requiring TagOptions failed because the TagOptions migration process has not been performed for this account. Please use the AWS console to perform the migration process before retrying the operation.</p>
newtype TagOptionNotMigratedException = TagOptionNotMigratedException 
  { 
  }


newtype TagOptionSummaries = TagOptionSummaries (Array TagOptionSummary)


-- | <p>Summary information about a TagOption.</p>
newtype TagOptionSummary = TagOptionSummary 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Values" :: NullOrUndefined (TagOptionValues)
  }


newtype TagOptionValue = TagOptionValue String


newtype TagOptionValues = TagOptionValues (Array TagOptionValue)


newtype TagValue = TagValue String


newtype Tags = Tags (Array Tag)


newtype TerminateProvisionedProductInput = TerminateProvisionedProductInput 
  { "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "ProvisionedProductId" :: NullOrUndefined (Id)
  , "TerminateToken" :: (IdempotencyToken)
  , "IgnoreErrors" :: NullOrUndefined (IgnoreErrors)
  , "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  }


newtype TerminateProvisionedProductOutput = TerminateProvisionedProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }


newtype TotalResultsCount = TotalResultsCount Int


newtype UpdateConstraintInput = UpdateConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  }


newtype UpdateConstraintOutput = UpdateConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }


newtype UpdatePortfolioInput = UpdatePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "DisplayName" :: NullOrUndefined (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "ProviderName" :: NullOrUndefined (ProviderName)
  , "AddTags" :: NullOrUndefined (AddTags)
  , "RemoveTags" :: NullOrUndefined (TagKeys)
  }


newtype UpdatePortfolioOutput = UpdatePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype UpdateProductInput = UpdateProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "Name" :: NullOrUndefined (ProductViewName)
  , "Owner" :: NullOrUndefined (ProductViewOwner)
  , "Description" :: NullOrUndefined (ProductViewShortDescription)
  , "Distributor" :: NullOrUndefined (ProductViewOwner)
  , "SupportDescription" :: NullOrUndefined (SupportDescription)
  , "SupportEmail" :: NullOrUndefined (SupportEmail)
  , "SupportUrl" :: NullOrUndefined (SupportUrl)
  , "AddTags" :: NullOrUndefined (AddTags)
  , "RemoveTags" :: NullOrUndefined (TagKeys)
  }


newtype UpdateProductOutput = UpdateProductOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }


newtype UpdateProvisionedProductInput = UpdateProvisionedProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "ProvisionedProductId" :: NullOrUndefined (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  , "PathId" :: NullOrUndefined (Id)
  , "ProvisioningParameters" :: NullOrUndefined (UpdateProvisioningParameters)
  , "UpdateToken" :: (IdempotencyToken)
  }


newtype UpdateProvisionedProductOutput = UpdateProvisionedProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }


newtype UpdateProvisioningArtifactInput = UpdateProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "Active" :: NullOrUndefined (ProvisioningArtifactActive)
  }


newtype UpdateProvisioningArtifactOutput = UpdateProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }


-- | <p>The parameter key-value pair used to update a provisioned product.</p>
newtype UpdateProvisioningParameter = UpdateProvisioningParameter 
  { "Key" :: NullOrUndefined (ParameterKey)
  , "Value" :: NullOrUndefined (ParameterValue)
  , "UsePreviousValue" :: NullOrUndefined (UsePreviousValue)
  }


newtype UpdateProvisioningParameters = UpdateProvisioningParameters (Array UpdateProvisioningParameter)


newtype UpdateTagOptionInput = UpdateTagOptionInput 
  { "Id" :: (TagOptionId)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  }


newtype UpdateTagOptionOutput = UpdateTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }


newtype UpdatedTime = UpdatedTime Number


-- | <p>Additional information provided by the administrator.</p>
newtype UsageInstruction = UsageInstruction 
  { "Type" :: NullOrUndefined (InstructionType)
  , "Value" :: NullOrUndefined (InstructionValue)
  }


newtype UsageInstructions = UsageInstructions (Array UsageInstruction)


newtype UsePreviousValue = UsePreviousValue Boolean


newtype UserArn = UserArn String


newtype UserArnSession = UserArnSession String


newtype Verbose = Verbose Boolean
