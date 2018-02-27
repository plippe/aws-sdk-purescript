

-- | <fullname>AWS Service Catalog</fullname> <p> <a href="https://aws.amazon.com/servicecatalog/">AWS Service Catalog</a> enables organizations to create and manage catalogs of IT services that are approved for use on AWS. To get the most out of this documentation, you should be familiar with the terminology discussed in <a href="http://docs.aws.amazon.com/servicecatalog/latest/adminguide/what-is_concepts.html">AWS Service Catalog Concepts</a>.</p>
module AWS.ServiceCatalog where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAcceptLanguage :: Newtype AcceptLanguage _


newtype AcceptPortfolioShareInput = AcceptPortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }
derive instance newtypeAcceptPortfolioShareInput :: Newtype AcceptPortfolioShareInput _


newtype AcceptPortfolioShareOutput = AcceptPortfolioShareOutput 
  { 
  }
derive instance newtypeAcceptPortfolioShareOutput :: Newtype AcceptPortfolioShareOutput _


-- | <p>The access level to use to filter results.</p>
newtype AccessLevelFilter = AccessLevelFilter 
  { "Key" :: NullOrUndefined (AccessLevelFilterKey)
  , "Value" :: NullOrUndefined (AccessLevelFilterValue)
  }
derive instance newtypeAccessLevelFilter :: Newtype AccessLevelFilter _


newtype AccessLevelFilterKey = AccessLevelFilterKey String
derive instance newtypeAccessLevelFilterKey :: Newtype AccessLevelFilterKey _


newtype AccessLevelFilterValue = AccessLevelFilterValue String
derive instance newtypeAccessLevelFilterValue :: Newtype AccessLevelFilterValue _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


newtype AccountIds = AccountIds (Array AccountId)
derive instance newtypeAccountIds :: Newtype AccountIds _


newtype AddTags = AddTags (Array Tag)
derive instance newtypeAddTags :: Newtype AddTags _


newtype AllowedValue = AllowedValue String
derive instance newtypeAllowedValue :: Newtype AllowedValue _


newtype AllowedValues = AllowedValues (Array AllowedValue)
derive instance newtypeAllowedValues :: Newtype AllowedValues _


newtype ApproximateCount = ApproximateCount Int
derive instance newtypeApproximateCount :: Newtype ApproximateCount _


newtype AssociatePrincipalWithPortfolioInput = AssociatePrincipalWithPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PrincipalARN" :: (PrincipalARN)
  , "PrincipalType" :: (PrincipalType)
  }
derive instance newtypeAssociatePrincipalWithPortfolioInput :: Newtype AssociatePrincipalWithPortfolioInput _


newtype AssociatePrincipalWithPortfolioOutput = AssociatePrincipalWithPortfolioOutput 
  { 
  }
derive instance newtypeAssociatePrincipalWithPortfolioOutput :: Newtype AssociatePrincipalWithPortfolioOutput _


newtype AssociateProductWithPortfolioInput = AssociateProductWithPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PortfolioId" :: (Id)
  , "SourcePortfolioId" :: NullOrUndefined (Id)
  }
derive instance newtypeAssociateProductWithPortfolioInput :: Newtype AssociateProductWithPortfolioInput _


newtype AssociateProductWithPortfolioOutput = AssociateProductWithPortfolioOutput 
  { 
  }
derive instance newtypeAssociateProductWithPortfolioOutput :: Newtype AssociateProductWithPortfolioOutput _


newtype AssociateTagOptionWithResourceInput = AssociateTagOptionWithResourceInput 
  { "ResourceId" :: (ResourceId)
  , "TagOptionId" :: (TagOptionId)
  }
derive instance newtypeAssociateTagOptionWithResourceInput :: Newtype AssociateTagOptionWithResourceInput _


newtype AssociateTagOptionWithResourceOutput = AssociateTagOptionWithResourceOutput 
  { 
  }
derive instance newtypeAssociateTagOptionWithResourceOutput :: Newtype AssociateTagOptionWithResourceOutput _


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _


newtype CausingEntity = CausingEntity String
derive instance newtypeCausingEntity :: Newtype CausingEntity _


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _


-- | <p>Information about a CloudWatch dashboard.</p>
newtype CloudWatchDashboard = CloudWatchDashboard 
  { "Name" :: NullOrUndefined (CloudWatchDashboardName)
  }
derive instance newtypeCloudWatchDashboard :: Newtype CloudWatchDashboard _


newtype CloudWatchDashboardName = CloudWatchDashboardName String
derive instance newtypeCloudWatchDashboardName :: Newtype CloudWatchDashboardName _


newtype CloudWatchDashboards = CloudWatchDashboards (Array CloudWatchDashboard)
derive instance newtypeCloudWatchDashboards :: Newtype CloudWatchDashboards _


newtype ConstraintDescription = ConstraintDescription String
derive instance newtypeConstraintDescription :: Newtype ConstraintDescription _


-- | <p>Information about a constraint.</p>
newtype ConstraintDetail = ConstraintDetail 
  { "ConstraintId" :: NullOrUndefined (Id)
  , "Type" :: NullOrUndefined (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  , "Owner" :: NullOrUndefined (AccountId)
  }
derive instance newtypeConstraintDetail :: Newtype ConstraintDetail _


newtype ConstraintDetails = ConstraintDetails (Array ConstraintDetail)
derive instance newtypeConstraintDetails :: Newtype ConstraintDetails _


newtype ConstraintParameters = ConstraintParameters String
derive instance newtypeConstraintParameters :: Newtype ConstraintParameters _


newtype ConstraintSummaries = ConstraintSummaries (Array ConstraintSummary)
derive instance newtypeConstraintSummaries :: Newtype ConstraintSummaries _


-- | <p>Summary information about a constraint.</p>
newtype ConstraintSummary = ConstraintSummary 
  { "Type" :: NullOrUndefined (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  }
derive instance newtypeConstraintSummary :: Newtype ConstraintSummary _


newtype ConstraintType = ConstraintType String
derive instance newtypeConstraintType :: Newtype ConstraintType _


newtype CopyOption = CopyOption String
derive instance newtypeCopyOption :: Newtype CopyOption _


newtype CopyOptions = CopyOptions (Array CopyOption)
derive instance newtypeCopyOptions :: Newtype CopyOptions _


newtype CopyProductInput = CopyProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "SourceProductArn" :: (ProductArn)
  , "TargetProductId" :: NullOrUndefined (Id)
  , "TargetProductName" :: NullOrUndefined (ProductViewName)
  , "SourceProvisioningArtifactIdentifiers" :: NullOrUndefined (SourceProvisioningArtifactProperties)
  , "CopyOptions" :: NullOrUndefined (CopyOptions)
  , "IdempotencyToken" :: (IdempotencyToken)
  }
derive instance newtypeCopyProductInput :: Newtype CopyProductInput _


newtype CopyProductOutput = CopyProductOutput 
  { "CopyProductToken" :: NullOrUndefined (Id)
  }
derive instance newtypeCopyProductOutput :: Newtype CopyProductOutput _


newtype CopyProductStatus = CopyProductStatus String
derive instance newtypeCopyProductStatus :: Newtype CopyProductStatus _


newtype CreateConstraintInput = CreateConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "ProductId" :: (Id)
  , "Parameters" :: (ConstraintParameters)
  , "Type" :: (ConstraintType)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  , "IdempotencyToken" :: (IdempotencyToken)
  }
derive instance newtypeCreateConstraintInput :: Newtype CreateConstraintInput _


newtype CreateConstraintOutput = CreateConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeCreateConstraintOutput :: Newtype CreateConstraintOutput _


newtype CreatePortfolioInput = CreatePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "DisplayName" :: (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "ProviderName" :: (ProviderName)
  , "Tags" :: NullOrUndefined (AddTags)
  , "IdempotencyToken" :: (IdempotencyToken)
  }
derive instance newtypeCreatePortfolioInput :: Newtype CreatePortfolioInput _


newtype CreatePortfolioOutput = CreatePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeCreatePortfolioOutput :: Newtype CreatePortfolioOutput _


newtype CreatePortfolioShareInput = CreatePortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "AccountId" :: (AccountId)
  }
derive instance newtypeCreatePortfolioShareInput :: Newtype CreatePortfolioShareInput _


newtype CreatePortfolioShareOutput = CreatePortfolioShareOutput 
  { 
  }
derive instance newtypeCreatePortfolioShareOutput :: Newtype CreatePortfolioShareOutput _


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
derive instance newtypeCreateProductInput :: Newtype CreateProductInput _


newtype CreateProductOutput = CreateProductOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeCreateProductOutput :: Newtype CreateProductOutput _


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
derive instance newtypeCreateProvisionedProductPlanInput :: Newtype CreateProvisionedProductPlanInput _


newtype CreateProvisionedProductPlanOutput = CreateProvisionedProductPlanOutput 
  { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName)
  , "PlanId" :: NullOrUndefined (Id)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductName)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  }
derive instance newtypeCreateProvisionedProductPlanOutput :: Newtype CreateProvisionedProductPlanOutput _


newtype CreateProvisioningArtifactInput = CreateProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "Parameters" :: (ProvisioningArtifactProperties)
  , "IdempotencyToken" :: (IdempotencyToken)
  }
derive instance newtypeCreateProvisioningArtifactInput :: Newtype CreateProvisioningArtifactInput _


newtype CreateProvisioningArtifactOutput = CreateProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeCreateProvisioningArtifactOutput :: Newtype CreateProvisioningArtifactOutput _


newtype CreateTagOptionInput = CreateTagOptionInput 
  { "Key" :: (TagOptionKey)
  , "Value" :: (TagOptionValue)
  }
derive instance newtypeCreateTagOptionInput :: Newtype CreateTagOptionInput _


newtype CreateTagOptionOutput = CreateTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }
derive instance newtypeCreateTagOptionOutput :: Newtype CreateTagOptionOutput _


newtype CreatedTime = CreatedTime Number
derive instance newtypeCreatedTime :: Newtype CreatedTime _


newtype CreationTime = CreationTime Number
derive instance newtypeCreationTime :: Newtype CreationTime _


newtype DefaultValue = DefaultValue String
derive instance newtypeDefaultValue :: Newtype DefaultValue _


newtype DeleteConstraintInput = DeleteConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDeleteConstraintInput :: Newtype DeleteConstraintInput _


newtype DeleteConstraintOutput = DeleteConstraintOutput 
  { 
  }
derive instance newtypeDeleteConstraintOutput :: Newtype DeleteConstraintOutput _


newtype DeletePortfolioInput = DeletePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDeletePortfolioInput :: Newtype DeletePortfolioInput _


newtype DeletePortfolioOutput = DeletePortfolioOutput 
  { 
  }
derive instance newtypeDeletePortfolioOutput :: Newtype DeletePortfolioOutput _


newtype DeletePortfolioShareInput = DeletePortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "AccountId" :: (AccountId)
  }
derive instance newtypeDeletePortfolioShareInput :: Newtype DeletePortfolioShareInput _


newtype DeletePortfolioShareOutput = DeletePortfolioShareOutput 
  { 
  }
derive instance newtypeDeletePortfolioShareOutput :: Newtype DeletePortfolioShareOutput _


newtype DeleteProductInput = DeleteProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDeleteProductInput :: Newtype DeleteProductInput _


newtype DeleteProductOutput = DeleteProductOutput 
  { 
  }
derive instance newtypeDeleteProductOutput :: Newtype DeleteProductOutput _


newtype DeleteProvisionedProductPlanInput = DeleteProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "IgnoreErrors" :: NullOrUndefined (IgnoreErrors)
  }
derive instance newtypeDeleteProvisionedProductPlanInput :: Newtype DeleteProvisionedProductPlanInput _


newtype DeleteProvisionedProductPlanOutput = DeleteProvisionedProductPlanOutput 
  { 
  }
derive instance newtypeDeleteProvisionedProductPlanOutput :: Newtype DeleteProvisionedProductPlanOutput _


newtype DeleteProvisioningArtifactInput = DeleteProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  }
derive instance newtypeDeleteProvisioningArtifactInput :: Newtype DeleteProvisioningArtifactInput _


newtype DeleteProvisioningArtifactOutput = DeleteProvisioningArtifactOutput 
  { 
  }
derive instance newtypeDeleteProvisioningArtifactOutput :: Newtype DeleteProvisioningArtifactOutput _


newtype DescribeConstraintInput = DescribeConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribeConstraintInput :: Newtype DescribeConstraintInput _


newtype DescribeConstraintOutput = DescribeConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeDescribeConstraintOutput :: Newtype DescribeConstraintOutput _


newtype DescribeCopyProductStatusInput = DescribeCopyProductStatusInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "CopyProductToken" :: (Id)
  }
derive instance newtypeDescribeCopyProductStatusInput :: Newtype DescribeCopyProductStatusInput _


newtype DescribeCopyProductStatusOutput = DescribeCopyProductStatusOutput 
  { "CopyProductStatus" :: NullOrUndefined (CopyProductStatus)
  , "TargetProductId" :: NullOrUndefined (Id)
  , "StatusDetail" :: NullOrUndefined (StatusDetail)
  }
derive instance newtypeDescribeCopyProductStatusOutput :: Newtype DescribeCopyProductStatusOutput _


newtype DescribePortfolioInput = DescribePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribePortfolioInput :: Newtype DescribePortfolioInput _


newtype DescribePortfolioOutput = DescribePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  , "TagOptions" :: NullOrUndefined (TagOptionDetails)
  }
derive instance newtypeDescribePortfolioOutput :: Newtype DescribePortfolioOutput _


newtype DescribeProductAsAdminInput = DescribeProductAsAdminInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribeProductAsAdminInput :: Newtype DescribeProductAsAdminInput _


newtype DescribeProductAsAdminOutput = DescribeProductAsAdminOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "ProvisioningArtifactSummaries" :: NullOrUndefined (ProvisioningArtifactSummaries)
  , "Tags" :: NullOrUndefined (Tags)
  , "TagOptions" :: NullOrUndefined (TagOptionDetails)
  }
derive instance newtypeDescribeProductAsAdminOutput :: Newtype DescribeProductAsAdminOutput _


newtype DescribeProductInput = DescribeProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribeProductInput :: Newtype DescribeProductInput _


newtype DescribeProductOutput = DescribeProductOutput 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts)
  }
derive instance newtypeDescribeProductOutput :: Newtype DescribeProductOutput _


newtype DescribeProductViewInput = DescribeProductViewInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribeProductViewInput :: Newtype DescribeProductViewInput _


newtype DescribeProductViewOutput = DescribeProductViewOutput 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "ProvisioningArtifacts" :: NullOrUndefined (ProvisioningArtifacts)
  }
derive instance newtypeDescribeProductViewOutput :: Newtype DescribeProductViewOutput _


newtype DescribeProvisionedProductInput = DescribeProvisionedProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  }
derive instance newtypeDescribeProvisionedProductInput :: Newtype DescribeProvisionedProductInput _


newtype DescribeProvisionedProductOutput = DescribeProvisionedProductOutput 
  { "ProvisionedProductDetail" :: NullOrUndefined (ProvisionedProductDetail)
  , "CloudWatchDashboards" :: NullOrUndefined (CloudWatchDashboards)
  }
derive instance newtypeDescribeProvisionedProductOutput :: Newtype DescribeProvisionedProductOutput _


newtype DescribeProvisionedProductPlanInput = DescribeProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeDescribeProvisionedProductPlanInput :: Newtype DescribeProvisionedProductPlanInput _


newtype DescribeProvisionedProductPlanOutput = DescribeProvisionedProductPlanOutput 
  { "ProvisionedProductPlanDetails" :: NullOrUndefined (ProvisionedProductPlanDetails)
  , "ResourceChanges" :: NullOrUndefined (ResourceChanges)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeDescribeProvisionedProductPlanOutput :: Newtype DescribeProvisionedProductPlanOutput _


newtype DescribeProvisioningArtifactInput = DescribeProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProvisioningArtifactId" :: (Id)
  , "ProductId" :: (Id)
  , "Verbose" :: NullOrUndefined (Verbose)
  }
derive instance newtypeDescribeProvisioningArtifactInput :: Newtype DescribeProvisioningArtifactInput _


newtype DescribeProvisioningArtifactOutput = DescribeProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeDescribeProvisioningArtifactOutput :: Newtype DescribeProvisioningArtifactOutput _


newtype DescribeProvisioningParametersInput = DescribeProvisioningParametersInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  , "PathId" :: NullOrUndefined (Id)
  }
derive instance newtypeDescribeProvisioningParametersInput :: Newtype DescribeProvisioningParametersInput _


newtype DescribeProvisioningParametersOutput = DescribeProvisioningParametersOutput 
  { "ProvisioningArtifactParameters" :: NullOrUndefined (ProvisioningArtifactParameters)
  , "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries)
  , "UsageInstructions" :: NullOrUndefined (UsageInstructions)
  , "TagOptions" :: NullOrUndefined (TagOptionSummaries)
  }
derive instance newtypeDescribeProvisioningParametersOutput :: Newtype DescribeProvisioningParametersOutput _


newtype DescribeRecordInput = DescribeRecordInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeRecordInput :: Newtype DescribeRecordInput _


newtype DescribeRecordOutput = DescribeRecordOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  , "RecordOutputs" :: NullOrUndefined (RecordOutputs)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeDescribeRecordOutput :: Newtype DescribeRecordOutput _


newtype DescribeTagOptionInput = DescribeTagOptionInput 
  { "Id" :: (TagOptionId)
  }
derive instance newtypeDescribeTagOptionInput :: Newtype DescribeTagOptionInput _


newtype DescribeTagOptionOutput = DescribeTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }
derive instance newtypeDescribeTagOptionOutput :: Newtype DescribeTagOptionOutput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DisassociatePrincipalFromPortfolioInput = DisassociatePrincipalFromPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PrincipalARN" :: (PrincipalARN)
  }
derive instance newtypeDisassociatePrincipalFromPortfolioInput :: Newtype DisassociatePrincipalFromPortfolioInput _


newtype DisassociatePrincipalFromPortfolioOutput = DisassociatePrincipalFromPortfolioOutput 
  { 
  }
derive instance newtypeDisassociatePrincipalFromPortfolioOutput :: Newtype DisassociatePrincipalFromPortfolioOutput _


newtype DisassociateProductFromPortfolioInput = DisassociateProductFromPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PortfolioId" :: (Id)
  }
derive instance newtypeDisassociateProductFromPortfolioInput :: Newtype DisassociateProductFromPortfolioInput _


newtype DisassociateProductFromPortfolioOutput = DisassociateProductFromPortfolioOutput 
  { 
  }
derive instance newtypeDisassociateProductFromPortfolioOutput :: Newtype DisassociateProductFromPortfolioOutput _


newtype DisassociateTagOptionFromResourceInput = DisassociateTagOptionFromResourceInput 
  { "ResourceId" :: (ResourceId)
  , "TagOptionId" :: (TagOptionId)
  }
derive instance newtypeDisassociateTagOptionFromResourceInput :: Newtype DisassociateTagOptionFromResourceInput _


newtype DisassociateTagOptionFromResourceOutput = DisassociateTagOptionFromResourceOutput 
  { 
  }
derive instance newtypeDisassociateTagOptionFromResourceOutput :: Newtype DisassociateTagOptionFromResourceOutput _


-- | <p>The specified resource is a duplicate.</p>
newtype DuplicateResourceException = DuplicateResourceException 
  { 
  }
derive instance newtypeDuplicateResourceException :: Newtype DuplicateResourceException _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorDescription = ErrorDescription String
derive instance newtypeErrorDescription :: Newtype ErrorDescription _


newtype EvaluationType = EvaluationType String
derive instance newtypeEvaluationType :: Newtype EvaluationType _


newtype ExecuteProvisionedProductPlanInput = ExecuteProvisionedProductPlanInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PlanId" :: (Id)
  , "IdempotencyToken" :: (IdempotencyToken)
  }
derive instance newtypeExecuteProvisionedProductPlanInput :: Newtype ExecuteProvisionedProductPlanInput _


newtype ExecuteProvisionedProductPlanOutput = ExecuteProvisionedProductPlanOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }
derive instance newtypeExecuteProvisionedProductPlanOutput :: Newtype ExecuteProvisionedProductPlanOutput _


newtype HasDefaultPath = HasDefaultPath Boolean
derive instance newtypeHasDefaultPath :: Newtype HasDefaultPath _


newtype Id = Id String
derive instance newtypeId :: Newtype Id _


newtype IdempotencyToken = IdempotencyToken String
derive instance newtypeIdempotencyToken :: Newtype IdempotencyToken _


newtype IgnoreErrors = IgnoreErrors Boolean
derive instance newtypeIgnoreErrors :: Newtype IgnoreErrors _


newtype InstructionType = InstructionType String
derive instance newtypeInstructionType :: Newtype InstructionType _


newtype InstructionValue = InstructionValue String
derive instance newtypeInstructionValue :: Newtype InstructionValue _


-- | <p>One or more parameters provided to the operation are not valid.</p>
newtype InvalidParametersException = InvalidParametersException 
  { 
  }
derive instance newtypeInvalidParametersException :: Newtype InvalidParametersException _


-- | <p>An attempt was made to modify a resource that is in a state that is not valid. Check your resources to ensure that they are in valid states before retrying the operation.</p>
newtype InvalidStateException = InvalidStateException 
  { 
  }
derive instance newtypeInvalidStateException :: Newtype InvalidStateException _


newtype LastRequestId = LastRequestId String
derive instance newtypeLastRequestId :: Newtype LastRequestId _


newtype LaunchPathSummaries = LaunchPathSummaries (Array LaunchPathSummary)
derive instance newtypeLaunchPathSummaries :: Newtype LaunchPathSummaries _


-- | <p>Summary information about a product path for a user.</p>
newtype LaunchPathSummary = LaunchPathSummary 
  { "Id" :: NullOrUndefined (Id)
  , "ConstraintSummaries" :: NullOrUndefined (ConstraintSummaries)
  , "Tags" :: NullOrUndefined (Tags)
  , "Name" :: NullOrUndefined (PortfolioName)
  }
derive instance newtypeLaunchPathSummary :: Newtype LaunchPathSummary _


-- | <p>The current limits of the service would have been exceeded by this operation. Decrease your resource use or increase your service limits and retry the operation.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListAcceptedPortfolioSharesInput = ListAcceptedPortfolioSharesInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListAcceptedPortfolioSharesInput :: Newtype ListAcceptedPortfolioSharesInput _


newtype ListAcceptedPortfolioSharesOutput = ListAcceptedPortfolioSharesOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListAcceptedPortfolioSharesOutput :: Newtype ListAcceptedPortfolioSharesOutput _


newtype ListConstraintsForPortfolioInput = ListConstraintsForPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "ProductId" :: NullOrUndefined (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListConstraintsForPortfolioInput :: Newtype ListConstraintsForPortfolioInput _


newtype ListConstraintsForPortfolioOutput = ListConstraintsForPortfolioOutput 
  { "ConstraintDetails" :: NullOrUndefined (ConstraintDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListConstraintsForPortfolioOutput :: Newtype ListConstraintsForPortfolioOutput _


newtype ListLaunchPathsInput = ListLaunchPathsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListLaunchPathsInput :: Newtype ListLaunchPathsInput _


newtype ListLaunchPathsOutput = ListLaunchPathsOutput 
  { "LaunchPathSummaries" :: NullOrUndefined (LaunchPathSummaries)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListLaunchPathsOutput :: Newtype ListLaunchPathsOutput _


newtype ListPortfolioAccessInput = ListPortfolioAccessInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }
derive instance newtypeListPortfolioAccessInput :: Newtype ListPortfolioAccessInput _


newtype ListPortfolioAccessOutput = ListPortfolioAccessOutput 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListPortfolioAccessOutput :: Newtype ListPortfolioAccessOutput _


newtype ListPortfoliosForProductInput = ListPortfoliosForProductInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListPortfoliosForProductInput :: Newtype ListPortfoliosForProductInput _


newtype ListPortfoliosForProductOutput = ListPortfoliosForProductOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListPortfoliosForProductOutput :: Newtype ListPortfoliosForProductOutput _


newtype ListPortfoliosInput = ListPortfoliosInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListPortfoliosInput :: Newtype ListPortfoliosInput _


newtype ListPortfoliosOutput = ListPortfoliosOutput 
  { "PortfolioDetails" :: NullOrUndefined (PortfolioDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListPortfoliosOutput :: Newtype ListPortfoliosOutput _


newtype ListPrincipalsForPortfolioInput = ListPrincipalsForPortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListPrincipalsForPortfolioInput :: Newtype ListPrincipalsForPortfolioInput _


newtype ListPrincipalsForPortfolioOutput = ListPrincipalsForPortfolioOutput 
  { "Principals" :: NullOrUndefined (Principals)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListPrincipalsForPortfolioOutput :: Newtype ListPrincipalsForPortfolioOutput _


newtype ListProvisionedProductPlansInput = ListProvisionedProductPlansInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  }
derive instance newtypeListProvisionedProductPlansInput :: Newtype ListProvisionedProductPlansInput _


newtype ListProvisionedProductPlansOutput = ListProvisionedProductPlansOutput 
  { "ProvisionedProductPlans" :: NullOrUndefined (ProvisionedProductPlans)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListProvisionedProductPlansOutput :: Newtype ListProvisionedProductPlansOutput _


newtype ListProvisioningArtifactsInput = ListProvisioningArtifactsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  }
derive instance newtypeListProvisioningArtifactsInput :: Newtype ListProvisioningArtifactsInput _


newtype ListProvisioningArtifactsOutput = ListProvisioningArtifactsOutput 
  { "ProvisioningArtifactDetails" :: NullOrUndefined (ProvisioningArtifactDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListProvisioningArtifactsOutput :: Newtype ListProvisioningArtifactsOutput _


newtype ListRecordHistoryInput = ListRecordHistoryInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "SearchFilter" :: NullOrUndefined (ListRecordHistorySearchFilter)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListRecordHistoryInput :: Newtype ListRecordHistoryInput _


newtype ListRecordHistoryOutput = ListRecordHistoryOutput 
  { "RecordDetails" :: NullOrUndefined (RecordDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListRecordHistoryOutput :: Newtype ListRecordHistoryOutput _


-- | <p>The search filter to use when listing history records.</p>
newtype ListRecordHistorySearchFilter = ListRecordHistorySearchFilter 
  { "Key" :: NullOrUndefined (SearchFilterKey)
  , "Value" :: NullOrUndefined (SearchFilterValue)
  }
derive instance newtypeListRecordHistorySearchFilter :: Newtype ListRecordHistorySearchFilter _


newtype ListResourcesForTagOptionInput = ListResourcesForTagOptionInput 
  { "TagOptionId" :: (TagOptionId)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListResourcesForTagOptionInput :: Newtype ListResourcesForTagOptionInput _


newtype ListResourcesForTagOptionOutput = ListResourcesForTagOptionOutput 
  { "ResourceDetails" :: NullOrUndefined (ResourceDetails)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListResourcesForTagOptionOutput :: Newtype ListResourcesForTagOptionOutput _


-- | <p>Filters to use when listing TagOptions.</p>
newtype ListTagOptionsFilters = ListTagOptionsFilters 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  }
derive instance newtypeListTagOptionsFilters :: Newtype ListTagOptionsFilters _


newtype ListTagOptionsInput = ListTagOptionsInput 
  { "Filters" :: NullOrUndefined (ListTagOptionsFilters)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListTagOptionsInput :: Newtype ListTagOptionsInput _


newtype ListTagOptionsOutput = ListTagOptionsOutput 
  { "TagOptionDetails" :: NullOrUndefined (TagOptionDetails)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListTagOptionsOutput :: Newtype ListTagOptionsOutput _


newtype LogicalResourceId = LogicalResourceId String
derive instance newtypeLogicalResourceId :: Newtype LogicalResourceId _


newtype NoEcho = NoEcho Boolean
derive instance newtypeNoEcho :: Newtype NoEcho _


newtype NotificationArn = NotificationArn String
derive instance newtypeNotificationArn :: Newtype NotificationArn _


newtype NotificationArns = NotificationArns (Array NotificationArn)
derive instance newtypeNotificationArns :: Newtype NotificationArns _


newtype OutputKey = OutputKey String
derive instance newtypeOutputKey :: Newtype OutputKey _


newtype OutputValue = OutputValue String
derive instance newtypeOutputValue :: Newtype OutputValue _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype PageToken = PageToken String
derive instance newtypePageToken :: Newtype PageToken _


-- | <p>The constraints that the administrator has put on the parameter.</p>
newtype ParameterConstraints = ParameterConstraints 
  { "AllowedValues" :: NullOrUndefined (AllowedValues)
  }
derive instance newtypeParameterConstraints :: Newtype ParameterConstraints _


newtype ParameterKey = ParameterKey String
derive instance newtypeParameterKey :: Newtype ParameterKey _


newtype ParameterType = ParameterType String
derive instance newtypeParameterType :: Newtype ParameterType _


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _


newtype PhysicalId = PhysicalId String
derive instance newtypePhysicalId :: Newtype PhysicalId _


newtype PhysicalResourceId = PhysicalResourceId String
derive instance newtypePhysicalResourceId :: Newtype PhysicalResourceId _


newtype PlanResourceType = PlanResourceType String
derive instance newtypePlanResourceType :: Newtype PlanResourceType _


newtype PortfolioDescription = PortfolioDescription String
derive instance newtypePortfolioDescription :: Newtype PortfolioDescription _


-- | <p>Information about a portfolio.</p>
newtype PortfolioDetail = PortfolioDetail 
  { "Id" :: NullOrUndefined (Id)
  , "ARN" :: NullOrUndefined (ResourceARN)
  , "DisplayName" :: NullOrUndefined (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "CreatedTime" :: NullOrUndefined (CreationTime)
  , "ProviderName" :: NullOrUndefined (ProviderName)
  }
derive instance newtypePortfolioDetail :: Newtype PortfolioDetail _


newtype PortfolioDetails = PortfolioDetails (Array PortfolioDetail)
derive instance newtypePortfolioDetails :: Newtype PortfolioDetails _


newtype PortfolioDisplayName = PortfolioDisplayName String
derive instance newtypePortfolioDisplayName :: Newtype PortfolioDisplayName _


newtype PortfolioName = PortfolioName String
derive instance newtypePortfolioName :: Newtype PortfolioName _


-- | <p>Information about a principal.</p>
newtype Principal = Principal 
  { "PrincipalARN" :: NullOrUndefined (PrincipalARN)
  , "PrincipalType" :: NullOrUndefined (PrincipalType)
  }
derive instance newtypePrincipal :: Newtype Principal _


newtype PrincipalARN = PrincipalARN String
derive instance newtypePrincipalARN :: Newtype PrincipalARN _


newtype PrincipalType = PrincipalType String
derive instance newtypePrincipalType :: Newtype PrincipalType _


newtype Principals = Principals (Array Principal)
derive instance newtypePrincipals :: Newtype Principals _


newtype ProductArn = ProductArn String
derive instance newtypeProductArn :: Newtype ProductArn _


newtype ProductSource = ProductSource String
derive instance newtypeProductSource :: Newtype ProductSource _


newtype ProductType = ProductType String
derive instance newtypeProductType :: Newtype ProductType _


newtype ProductViewAggregationType = ProductViewAggregationType String
derive instance newtypeProductViewAggregationType :: Newtype ProductViewAggregationType _


-- | <p>A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.</p>
newtype ProductViewAggregationValue = ProductViewAggregationValue 
  { "Value" :: NullOrUndefined (AttributeValue)
  , "ApproximateCount" :: NullOrUndefined (ApproximateCount)
  }
derive instance newtypeProductViewAggregationValue :: Newtype ProductViewAggregationValue _


newtype ProductViewAggregationValues = ProductViewAggregationValues (Array ProductViewAggregationValue)
derive instance newtypeProductViewAggregationValues :: Newtype ProductViewAggregationValues _


newtype ProductViewAggregations = ProductViewAggregations (Map ProductViewAggregationType ProductViewAggregationValues)
derive instance newtypeProductViewAggregations :: Newtype ProductViewAggregations _


-- | <p>Information about a product view.</p>
newtype ProductViewDetail = ProductViewDetail 
  { "ProductViewSummary" :: NullOrUndefined (ProductViewSummary)
  , "Status" :: NullOrUndefined (Status)
  , "ProductARN" :: NullOrUndefined (ResourceARN)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  }
derive instance newtypeProductViewDetail :: Newtype ProductViewDetail _


newtype ProductViewDetails = ProductViewDetails (Array ProductViewDetail)
derive instance newtypeProductViewDetails :: Newtype ProductViewDetails _


newtype ProductViewDistributor = ProductViewDistributor String
derive instance newtypeProductViewDistributor :: Newtype ProductViewDistributor _


newtype ProductViewFilterBy = ProductViewFilterBy String
derive instance newtypeProductViewFilterBy :: Newtype ProductViewFilterBy _


newtype ProductViewFilterValue = ProductViewFilterValue String
derive instance newtypeProductViewFilterValue :: Newtype ProductViewFilterValue _


newtype ProductViewFilterValues = ProductViewFilterValues (Array ProductViewFilterValue)
derive instance newtypeProductViewFilterValues :: Newtype ProductViewFilterValues _


newtype ProductViewFilters = ProductViewFilters (Map ProductViewFilterBy ProductViewFilterValues)
derive instance newtypeProductViewFilters :: Newtype ProductViewFilters _


newtype ProductViewName = ProductViewName String
derive instance newtypeProductViewName :: Newtype ProductViewName _


newtype ProductViewOwner = ProductViewOwner String
derive instance newtypeProductViewOwner :: Newtype ProductViewOwner _


newtype ProductViewShortDescription = ProductViewShortDescription String
derive instance newtypeProductViewShortDescription :: Newtype ProductViewShortDescription _


newtype ProductViewSortBy = ProductViewSortBy String
derive instance newtypeProductViewSortBy :: Newtype ProductViewSortBy _


newtype ProductViewSummaries = ProductViewSummaries (Array ProductViewSummary)
derive instance newtypeProductViewSummaries :: Newtype ProductViewSummaries _


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
derive instance newtypeProductViewSummary :: Newtype ProductViewSummary _


newtype PropertyName = PropertyName String
derive instance newtypePropertyName :: Newtype PropertyName _


newtype ProviderName = ProviderName String
derive instance newtypeProviderName :: Newtype ProviderName _


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
derive instance newtypeProvisionProductInput :: Newtype ProvisionProductInput _


newtype ProvisionProductOutput = ProvisionProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }
derive instance newtypeProvisionProductOutput :: Newtype ProvisionProductOutput _


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
derive instance newtypeProvisionedProductAttribute :: Newtype ProvisionedProductAttribute _


newtype ProvisionedProductAttributes = ProvisionedProductAttributes (Array ProvisionedProductAttribute)
derive instance newtypeProvisionedProductAttributes :: Newtype ProvisionedProductAttributes _


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
derive instance newtypeProvisionedProductDetail :: Newtype ProvisionedProductDetail _


newtype ProvisionedProductDetails = ProvisionedProductDetails (Array ProvisionedProductDetail)
derive instance newtypeProvisionedProductDetails :: Newtype ProvisionedProductDetails _


newtype ProvisionedProductFilters = ProvisionedProductFilters (Map ProvisionedProductViewFilterBy ProvisionedProductViewFilterValues)
derive instance newtypeProvisionedProductFilters :: Newtype ProvisionedProductFilters _


newtype ProvisionedProductId = ProvisionedProductId String
derive instance newtypeProvisionedProductId :: Newtype ProvisionedProductId _


newtype ProvisionedProductName = ProvisionedProductName String
derive instance newtypeProvisionedProductName :: Newtype ProvisionedProductName _


newtype ProvisionedProductNameOrArn = ProvisionedProductNameOrArn String
derive instance newtypeProvisionedProductNameOrArn :: Newtype ProvisionedProductNameOrArn _


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
derive instance newtypeProvisionedProductPlanDetails :: Newtype ProvisionedProductPlanDetails _


newtype ProvisionedProductPlanName = ProvisionedProductPlanName String
derive instance newtypeProvisionedProductPlanName :: Newtype ProvisionedProductPlanName _


newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus String
derive instance newtypeProvisionedProductPlanStatus :: Newtype ProvisionedProductPlanStatus _


-- | <p>Summary information about a plan.</p>
newtype ProvisionedProductPlanSummary = ProvisionedProductPlanSummary 
  { "PlanName" :: NullOrUndefined (ProvisionedProductPlanName)
  , "PlanId" :: NullOrUndefined (Id)
  , "ProvisionProductId" :: NullOrUndefined (Id)
  , "ProvisionProductName" :: NullOrUndefined (ProvisionedProductName)
  , "PlanType" :: NullOrUndefined (ProvisionedProductPlanType)
  , "ProvisioningArtifactId" :: NullOrUndefined (Id)
  }
derive instance newtypeProvisionedProductPlanSummary :: Newtype ProvisionedProductPlanSummary _


newtype ProvisionedProductPlanType = ProvisionedProductPlanType String
derive instance newtypeProvisionedProductPlanType :: Newtype ProvisionedProductPlanType _


newtype ProvisionedProductPlans = ProvisionedProductPlans (Array ProvisionedProductPlanSummary)
derive instance newtypeProvisionedProductPlans :: Newtype ProvisionedProductPlans _


newtype ProvisionedProductStatus = ProvisionedProductStatus String
derive instance newtypeProvisionedProductStatus :: Newtype ProvisionedProductStatus _


newtype ProvisionedProductStatusMessage = ProvisionedProductStatusMessage String
derive instance newtypeProvisionedProductStatusMessage :: Newtype ProvisionedProductStatusMessage _


newtype ProvisionedProductType = ProvisionedProductType String
derive instance newtypeProvisionedProductType :: Newtype ProvisionedProductType _


newtype ProvisionedProductViewFilterBy = ProvisionedProductViewFilterBy String
derive instance newtypeProvisionedProductViewFilterBy :: Newtype ProvisionedProductViewFilterBy _


newtype ProvisionedProductViewFilterValue = ProvisionedProductViewFilterValue String
derive instance newtypeProvisionedProductViewFilterValue :: Newtype ProvisionedProductViewFilterValue _


newtype ProvisionedProductViewFilterValues = ProvisionedProductViewFilterValues (Array ProvisionedProductViewFilterValue)
derive instance newtypeProvisionedProductViewFilterValues :: Newtype ProvisionedProductViewFilterValues _


-- | <p>Information about a provisioning artifact. A provisioning artifact is also known as a product version.</p>
newtype ProvisioningArtifact = ProvisioningArtifact 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime)
  }
derive instance newtypeProvisioningArtifact :: Newtype ProvisioningArtifact _


newtype ProvisioningArtifactActive = ProvisioningArtifactActive Boolean
derive instance newtypeProvisioningArtifactActive :: Newtype ProvisioningArtifactActive _


newtype ProvisioningArtifactCreatedTime = ProvisioningArtifactCreatedTime Number
derive instance newtypeProvisioningArtifactCreatedTime :: Newtype ProvisioningArtifactCreatedTime _


newtype ProvisioningArtifactDescription = ProvisioningArtifactDescription String
derive instance newtypeProvisioningArtifactDescription :: Newtype ProvisioningArtifactDescription _


-- | <p>Information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactDetail = ProvisioningArtifactDetail 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactName)
  , "Type" :: NullOrUndefined (ProvisioningArtifactType)
  , "CreatedTime" :: NullOrUndefined (CreationTime)
  , "Active" :: NullOrUndefined (ProvisioningArtifactActive)
  }
derive instance newtypeProvisioningArtifactDetail :: Newtype ProvisioningArtifactDetail _


newtype ProvisioningArtifactDetails = ProvisioningArtifactDetails (Array ProvisioningArtifactDetail)
derive instance newtypeProvisioningArtifactDetails :: Newtype ProvisioningArtifactDetails _


newtype ProvisioningArtifactInfo = ProvisioningArtifactInfo (Map ProvisioningArtifactInfoKey ProvisioningArtifactInfoValue)
derive instance newtypeProvisioningArtifactInfo :: Newtype ProvisioningArtifactInfo _


newtype ProvisioningArtifactInfoKey = ProvisioningArtifactInfoKey String
derive instance newtypeProvisioningArtifactInfoKey :: Newtype ProvisioningArtifactInfoKey _


newtype ProvisioningArtifactInfoValue = ProvisioningArtifactInfoValue String
derive instance newtypeProvisioningArtifactInfoValue :: Newtype ProvisioningArtifactInfoValue _


newtype ProvisioningArtifactName = ProvisioningArtifactName String
derive instance newtypeProvisioningArtifactName :: Newtype ProvisioningArtifactName _


-- | <p>Information about a parameter used to provision a product.</p>
newtype ProvisioningArtifactParameter = ProvisioningArtifactParameter 
  { "ParameterKey" :: NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined (DefaultValue)
  , "ParameterType" :: NullOrUndefined (ParameterType)
  , "IsNoEcho" :: NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined (Description)
  , "ParameterConstraints" :: NullOrUndefined (ParameterConstraints)
  }
derive instance newtypeProvisioningArtifactParameter :: Newtype ProvisioningArtifactParameter _


newtype ProvisioningArtifactParameters = ProvisioningArtifactParameters (Array ProvisioningArtifactParameter)
derive instance newtypeProvisioningArtifactParameters :: Newtype ProvisioningArtifactParameters _


-- | <p>Information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactProperties = ProvisioningArtifactProperties 
  { "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "Info" :: (ProvisioningArtifactInfo)
  , "Type" :: NullOrUndefined (ProvisioningArtifactType)
  }
derive instance newtypeProvisioningArtifactProperties :: Newtype ProvisioningArtifactProperties _


newtype ProvisioningArtifactPropertyName = ProvisioningArtifactPropertyName String
derive instance newtypeProvisioningArtifactPropertyName :: Newtype ProvisioningArtifactPropertyName _


newtype ProvisioningArtifactPropertyValue = ProvisioningArtifactPropertyValue String
derive instance newtypeProvisioningArtifactPropertyValue :: Newtype ProvisioningArtifactPropertyValue _


newtype ProvisioningArtifactSummaries = ProvisioningArtifactSummaries (Array ProvisioningArtifactSummary)
derive instance newtypeProvisioningArtifactSummaries :: Newtype ProvisioningArtifactSummaries _


-- | <p>Summary information about a provisioning artifact (also known as a version) for a product.</p>
newtype ProvisioningArtifactSummary = ProvisioningArtifactSummary 
  { "Id" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "CreatedTime" :: NullOrUndefined (ProvisioningArtifactCreatedTime)
  , "ProvisioningArtifactMetadata" :: NullOrUndefined (ProvisioningArtifactInfo)
  }
derive instance newtypeProvisioningArtifactSummary :: Newtype ProvisioningArtifactSummary _


newtype ProvisioningArtifactType = ProvisioningArtifactType String
derive instance newtypeProvisioningArtifactType :: Newtype ProvisioningArtifactType _


newtype ProvisioningArtifacts = ProvisioningArtifacts (Array ProvisioningArtifact)
derive instance newtypeProvisioningArtifacts :: Newtype ProvisioningArtifacts _


-- | <p>Information about a parameter used to provision a product.</p>
newtype ProvisioningParameter = ProvisioningParameter 
  { "Key" :: NullOrUndefined (ParameterKey)
  , "Value" :: NullOrUndefined (ParameterValue)
  }
derive instance newtypeProvisioningParameter :: Newtype ProvisioningParameter _


newtype ProvisioningParameters = ProvisioningParameters (Array ProvisioningParameter)
derive instance newtypeProvisioningParameters :: Newtype ProvisioningParameters _


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
derive instance newtypeRecordDetail :: Newtype RecordDetail _


newtype RecordDetails = RecordDetails (Array RecordDetail)
derive instance newtypeRecordDetails :: Newtype RecordDetails _


-- | <p>The error code and description resulting from an operation.</p>
newtype RecordError = RecordError 
  { "Code" :: NullOrUndefined (ErrorCode)
  , "Description" :: NullOrUndefined (ErrorDescription)
  }
derive instance newtypeRecordError :: Newtype RecordError _


newtype RecordErrors = RecordErrors (Array RecordError)
derive instance newtypeRecordErrors :: Newtype RecordErrors _


-- | <p>The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.</p>
newtype RecordOutput = RecordOutput 
  { "OutputKey" :: NullOrUndefined (OutputKey)
  , "OutputValue" :: NullOrUndefined (OutputValue)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeRecordOutput :: Newtype RecordOutput _


newtype RecordOutputs = RecordOutputs (Array RecordOutput)
derive instance newtypeRecordOutputs :: Newtype RecordOutputs _


newtype RecordStatus = RecordStatus String
derive instance newtypeRecordStatus :: Newtype RecordStatus _


-- | <p>Information about a tag, which is a key-value pair.</p>
newtype RecordTag = RecordTag 
  { "Key" :: NullOrUndefined (RecordTagKey)
  , "Value" :: NullOrUndefined (RecordTagValue)
  }
derive instance newtypeRecordTag :: Newtype RecordTag _


newtype RecordTagKey = RecordTagKey String
derive instance newtypeRecordTagKey :: Newtype RecordTagKey _


newtype RecordTagValue = RecordTagValue String
derive instance newtypeRecordTagValue :: Newtype RecordTagValue _


newtype RecordTags = RecordTags (Array RecordTag)
derive instance newtypeRecordTags :: Newtype RecordTags _


newtype RecordType = RecordType String
derive instance newtypeRecordType :: Newtype RecordType _


newtype RejectPortfolioShareInput = RejectPortfolioShareInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "PortfolioId" :: (Id)
  }
derive instance newtypeRejectPortfolioShareInput :: Newtype RejectPortfolioShareInput _


newtype RejectPortfolioShareOutput = RejectPortfolioShareOutput 
  { 
  }
derive instance newtypeRejectPortfolioShareOutput :: Newtype RejectPortfolioShareOutput _


newtype Replacement = Replacement String
derive instance newtypeReplacement :: Newtype Replacement _


newtype RequiresRecreation = RequiresRecreation String
derive instance newtypeRequiresRecreation :: Newtype RequiresRecreation _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


newtype ResourceAttribute = ResourceAttribute String
derive instance newtypeResourceAttribute :: Newtype ResourceAttribute _


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
derive instance newtypeResourceChange :: Newtype ResourceChange _


-- | <p>Information about a change to a resource attribute.</p>
newtype ResourceChangeDetail = ResourceChangeDetail 
  { "Target" :: NullOrUndefined (ResourceTargetDefinition)
  , "Evaluation" :: NullOrUndefined (EvaluationType)
  , "CausingEntity" :: NullOrUndefined (CausingEntity)
  }
derive instance newtypeResourceChangeDetail :: Newtype ResourceChangeDetail _


newtype ResourceChangeDetails = ResourceChangeDetails (Array ResourceChangeDetail)
derive instance newtypeResourceChangeDetails :: Newtype ResourceChangeDetails _


newtype ResourceChanges = ResourceChanges (Array ResourceChange)
derive instance newtypeResourceChanges :: Newtype ResourceChanges _


-- | <p>Information about a resource.</p>
newtype ResourceDetail = ResourceDetail 
  { "Id" :: NullOrUndefined (ResourceDetailId)
  , "ARN" :: NullOrUndefined (ResourceDetailARN)
  , "Name" :: NullOrUndefined (ResourceDetailName)
  , "Description" :: NullOrUndefined (ResourceDetailDescription)
  , "CreatedTime" :: NullOrUndefined (ResourceDetailCreatedTime)
  }
derive instance newtypeResourceDetail :: Newtype ResourceDetail _


newtype ResourceDetailARN = ResourceDetailARN String
derive instance newtypeResourceDetailARN :: Newtype ResourceDetailARN _


newtype ResourceDetailCreatedTime = ResourceDetailCreatedTime Number
derive instance newtypeResourceDetailCreatedTime :: Newtype ResourceDetailCreatedTime _


newtype ResourceDetailDescription = ResourceDetailDescription String
derive instance newtypeResourceDetailDescription :: Newtype ResourceDetailDescription _


newtype ResourceDetailId = ResourceDetailId String
derive instance newtypeResourceDetailId :: Newtype ResourceDetailId _


newtype ResourceDetailName = ResourceDetailName String
derive instance newtypeResourceDetailName :: Newtype ResourceDetailName _


newtype ResourceDetails = ResourceDetails (Array ResourceDetail)
derive instance newtypeResourceDetails :: Newtype ResourceDetails _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


-- | <p>A resource that is currently in use. Ensure that the resource is not in use and retry the operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The specified resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Information about a change to a resource attribute.</p>
newtype ResourceTargetDefinition = ResourceTargetDefinition 
  { "Attribute" :: NullOrUndefined (ResourceAttribute)
  , "Name" :: NullOrUndefined (PropertyName)
  , "RequiresRecreation" :: NullOrUndefined (RequiresRecreation)
  }
derive instance newtypeResourceTargetDefinition :: Newtype ResourceTargetDefinition _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype ScanProvisionedProductsInput = ScanProvisionedProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeScanProvisionedProductsInput :: Newtype ScanProvisionedProductsInput _


newtype ScanProvisionedProductsOutput = ScanProvisionedProductsOutput 
  { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeScanProvisionedProductsOutput :: Newtype ScanProvisionedProductsOutput _


newtype Scope = Scope (Array ResourceAttribute)
derive instance newtypeScope :: Newtype Scope _


newtype SearchFilterKey = SearchFilterKey String
derive instance newtypeSearchFilterKey :: Newtype SearchFilterKey _


newtype SearchFilterValue = SearchFilterValue String
derive instance newtypeSearchFilterValue :: Newtype SearchFilterValue _


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
derive instance newtypeSearchProductsAsAdminInput :: Newtype SearchProductsAsAdminInput _


newtype SearchProductsAsAdminOutput = SearchProductsAsAdminOutput 
  { "ProductViewDetails" :: NullOrUndefined (ProductViewDetails)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeSearchProductsAsAdminOutput :: Newtype SearchProductsAsAdminOutput _


newtype SearchProductsInput = SearchProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Filters" :: NullOrUndefined (ProductViewFilters)
  , "PageSize" :: NullOrUndefined (PageSize)
  , "SortBy" :: NullOrUndefined (ProductViewSortBy)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeSearchProductsInput :: Newtype SearchProductsInput _


newtype SearchProductsOutput = SearchProductsOutput 
  { "ProductViewSummaries" :: NullOrUndefined (ProductViewSummaries)
  , "ProductViewAggregations" :: NullOrUndefined (ProductViewAggregations)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeSearchProductsOutput :: Newtype SearchProductsOutput _


newtype SearchProvisionedProductsInput = SearchProvisionedProductsInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "AccessLevelFilter" :: NullOrUndefined (AccessLevelFilter)
  , "Filters" :: NullOrUndefined (ProvisionedProductFilters)
  , "SortBy" :: NullOrUndefined (SortField)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "PageSize" :: NullOrUndefined (SearchProvisionedProductsPageSize)
  , "PageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeSearchProvisionedProductsInput :: Newtype SearchProvisionedProductsInput _


newtype SearchProvisionedProductsOutput = SearchProvisionedProductsOutput 
  { "ProvisionedProducts" :: NullOrUndefined (ProvisionedProductAttributes)
  , "TotalResultsCount" :: NullOrUndefined (TotalResultsCount)
  , "NextPageToken" :: NullOrUndefined (PageToken)
  }
derive instance newtypeSearchProvisionedProductsOutput :: Newtype SearchProvisionedProductsOutput _


newtype SearchProvisionedProductsPageSize = SearchProvisionedProductsPageSize Int
derive instance newtypeSearchProvisionedProductsPageSize :: Newtype SearchProvisionedProductsPageSize _


newtype SortField = SortField String
derive instance newtypeSortField :: Newtype SortField _


newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _


newtype SourceProvisioningArtifactProperties = SourceProvisioningArtifactProperties (Array SourceProvisioningArtifactPropertiesMap)
derive instance newtypeSourceProvisioningArtifactProperties :: Newtype SourceProvisioningArtifactProperties _


newtype SourceProvisioningArtifactPropertiesMap = SourceProvisioningArtifactPropertiesMap (Map ProvisioningArtifactPropertyName ProvisioningArtifactPropertyValue)
derive instance newtypeSourceProvisioningArtifactPropertiesMap :: Newtype SourceProvisioningArtifactPropertiesMap _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


newtype StatusDetail = StatusDetail String
derive instance newtypeStatusDetail :: Newtype StatusDetail _


newtype StatusMessage = StatusMessage String
derive instance newtypeStatusMessage :: Newtype StatusMessage _


newtype SupportDescription = SupportDescription String
derive instance newtypeSupportDescription :: Newtype SupportDescription _


newtype SupportEmail = SupportEmail String
derive instance newtypeSupportEmail :: Newtype SupportEmail _


newtype SupportUrl = SupportUrl String
derive instance newtypeSupportUrl :: Newtype SupportUrl _


-- | <p>Information about a tag. A tag is a key-value pair. Tags are propagated to the resources created when provisioning a product.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


newtype TagOptionActive = TagOptionActive Boolean
derive instance newtypeTagOptionActive :: Newtype TagOptionActive _


-- | <p>Information about a TagOption.</p>
newtype TagOptionDetail = TagOptionDetail 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  , "Id" :: NullOrUndefined (TagOptionId)
  }
derive instance newtypeTagOptionDetail :: Newtype TagOptionDetail _


newtype TagOptionDetails = TagOptionDetails (Array TagOptionDetail)
derive instance newtypeTagOptionDetails :: Newtype TagOptionDetails _


newtype TagOptionId = TagOptionId String
derive instance newtypeTagOptionId :: Newtype TagOptionId _


newtype TagOptionKey = TagOptionKey String
derive instance newtypeTagOptionKey :: Newtype TagOptionKey _


-- | <p>An operation requiring TagOptions failed because the TagOptions migration process has not been performed for this account. Please use the AWS console to perform the migration process before retrying the operation.</p>
newtype TagOptionNotMigratedException = TagOptionNotMigratedException 
  { 
  }
derive instance newtypeTagOptionNotMigratedException :: Newtype TagOptionNotMigratedException _


newtype TagOptionSummaries = TagOptionSummaries (Array TagOptionSummary)
derive instance newtypeTagOptionSummaries :: Newtype TagOptionSummaries _


-- | <p>Summary information about a TagOption.</p>
newtype TagOptionSummary = TagOptionSummary 
  { "Key" :: NullOrUndefined (TagOptionKey)
  , "Values" :: NullOrUndefined (TagOptionValues)
  }
derive instance newtypeTagOptionSummary :: Newtype TagOptionSummary _


newtype TagOptionValue = TagOptionValue String
derive instance newtypeTagOptionValue :: Newtype TagOptionValue _


newtype TagOptionValues = TagOptionValues (Array TagOptionValue)
derive instance newtypeTagOptionValues :: Newtype TagOptionValues _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


newtype TerminateProvisionedProductInput = TerminateProvisionedProductInput 
  { "ProvisionedProductName" :: NullOrUndefined (ProvisionedProductNameOrArn)
  , "ProvisionedProductId" :: NullOrUndefined (Id)
  , "TerminateToken" :: (IdempotencyToken)
  , "IgnoreErrors" :: NullOrUndefined (IgnoreErrors)
  , "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  }
derive instance newtypeTerminateProvisionedProductInput :: Newtype TerminateProvisionedProductInput _


newtype TerminateProvisionedProductOutput = TerminateProvisionedProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }
derive instance newtypeTerminateProvisionedProductOutput :: Newtype TerminateProvisionedProductOutput _


newtype TotalResultsCount = TotalResultsCount Int
derive instance newtypeTotalResultsCount :: Newtype TotalResultsCount _


newtype UpdateConstraintInput = UpdateConstraintInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "Description" :: NullOrUndefined (ConstraintDescription)
  }
derive instance newtypeUpdateConstraintInput :: Newtype UpdateConstraintInput _


newtype UpdateConstraintOutput = UpdateConstraintOutput 
  { "ConstraintDetail" :: NullOrUndefined (ConstraintDetail)
  , "ConstraintParameters" :: NullOrUndefined (ConstraintParameters)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeUpdateConstraintOutput :: Newtype UpdateConstraintOutput _


newtype UpdatePortfolioInput = UpdatePortfolioInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "Id" :: (Id)
  , "DisplayName" :: NullOrUndefined (PortfolioDisplayName)
  , "Description" :: NullOrUndefined (PortfolioDescription)
  , "ProviderName" :: NullOrUndefined (ProviderName)
  , "AddTags" :: NullOrUndefined (AddTags)
  , "RemoveTags" :: NullOrUndefined (TagKeys)
  }
derive instance newtypeUpdatePortfolioInput :: Newtype UpdatePortfolioInput _


newtype UpdatePortfolioOutput = UpdatePortfolioOutput 
  { "PortfolioDetail" :: NullOrUndefined (PortfolioDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeUpdatePortfolioOutput :: Newtype UpdatePortfolioOutput _


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
derive instance newtypeUpdateProductInput :: Newtype UpdateProductInput _


newtype UpdateProductOutput = UpdateProductOutput 
  { "ProductViewDetail" :: NullOrUndefined (ProductViewDetail)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeUpdateProductOutput :: Newtype UpdateProductOutput _


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
derive instance newtypeUpdateProvisionedProductInput :: Newtype UpdateProvisionedProductInput _


newtype UpdateProvisionedProductOutput = UpdateProvisionedProductOutput 
  { "RecordDetail" :: NullOrUndefined (RecordDetail)
  }
derive instance newtypeUpdateProvisionedProductOutput :: Newtype UpdateProvisionedProductOutput _


newtype UpdateProvisioningArtifactInput = UpdateProvisioningArtifactInput 
  { "AcceptLanguage" :: NullOrUndefined (AcceptLanguage)
  , "ProductId" :: (Id)
  , "ProvisioningArtifactId" :: (Id)
  , "Name" :: NullOrUndefined (ProvisioningArtifactName)
  , "Description" :: NullOrUndefined (ProvisioningArtifactDescription)
  , "Active" :: NullOrUndefined (ProvisioningArtifactActive)
  }
derive instance newtypeUpdateProvisioningArtifactInput :: Newtype UpdateProvisioningArtifactInput _


newtype UpdateProvisioningArtifactOutput = UpdateProvisioningArtifactOutput 
  { "ProvisioningArtifactDetail" :: NullOrUndefined (ProvisioningArtifactDetail)
  , "Info" :: NullOrUndefined (ProvisioningArtifactInfo)
  , "Status" :: NullOrUndefined (Status)
  }
derive instance newtypeUpdateProvisioningArtifactOutput :: Newtype UpdateProvisioningArtifactOutput _


-- | <p>The parameter key-value pair used to update a provisioned product.</p>
newtype UpdateProvisioningParameter = UpdateProvisioningParameter 
  { "Key" :: NullOrUndefined (ParameterKey)
  , "Value" :: NullOrUndefined (ParameterValue)
  , "UsePreviousValue" :: NullOrUndefined (UsePreviousValue)
  }
derive instance newtypeUpdateProvisioningParameter :: Newtype UpdateProvisioningParameter _


newtype UpdateProvisioningParameters = UpdateProvisioningParameters (Array UpdateProvisioningParameter)
derive instance newtypeUpdateProvisioningParameters :: Newtype UpdateProvisioningParameters _


newtype UpdateTagOptionInput = UpdateTagOptionInput 
  { "Id" :: (TagOptionId)
  , "Value" :: NullOrUndefined (TagOptionValue)
  , "Active" :: NullOrUndefined (TagOptionActive)
  }
derive instance newtypeUpdateTagOptionInput :: Newtype UpdateTagOptionInput _


newtype UpdateTagOptionOutput = UpdateTagOptionOutput 
  { "TagOptionDetail" :: NullOrUndefined (TagOptionDetail)
  }
derive instance newtypeUpdateTagOptionOutput :: Newtype UpdateTagOptionOutput _


newtype UpdatedTime = UpdatedTime Number
derive instance newtypeUpdatedTime :: Newtype UpdatedTime _


-- | <p>Additional information provided by the administrator.</p>
newtype UsageInstruction = UsageInstruction 
  { "Type" :: NullOrUndefined (InstructionType)
  , "Value" :: NullOrUndefined (InstructionValue)
  }
derive instance newtypeUsageInstruction :: Newtype UsageInstruction _


newtype UsageInstructions = UsageInstructions (Array UsageInstruction)
derive instance newtypeUsageInstructions :: Newtype UsageInstructions _


newtype UsePreviousValue = UsePreviousValue Boolean
derive instance newtypeUsePreviousValue :: Newtype UsePreviousValue _


newtype UserArn = UserArn String
derive instance newtypeUserArn :: Newtype UserArn _


newtype UserArnSession = UserArnSession String
derive instance newtypeUserArnSession :: Newtype UserArnSession _


newtype Verbose = Verbose Boolean
derive instance newtypeVerbose :: Newtype Verbose _
