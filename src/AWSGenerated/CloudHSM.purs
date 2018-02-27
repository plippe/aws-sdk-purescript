

-- | <fullname>AWS CloudHSM Service</fullname> <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p>
module AWS.CloudHSM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudHSM" :: String


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Adds or overwrites one or more tags for the specified AWS CloudHSM resource.</p> <p>Each tag consists of a key and a value. Tag keys must be unique to each resource.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceResponse
addTagsToResource = AWS.request serviceName "addTagsToResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.</p>
createHapg :: forall eff. CreateHapgRequest -> Aff (err :: AWS.RequestError | eff) CreateHapgResponse
createHapg = AWS.request serviceName "createHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an uninitialized HSM instance.</p> <p>There is an upfront fee charged for each HSM instance that you create with the <code>CreateHsm</code> operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the <a>DeleteHsm</a> operation, go to the <a href="https://console.aws.amazon.com/support/home">AWS Support Center</a>, create a new case, and select <b>Account and Billing Support</b>.</p> <important> <p>It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the <a>DescribeHsm</a> operation. The HSM is ready to be initialized when the status changes to <code>RUNNING</code>.</p> </important>
createHsm :: forall eff. CreateHsmRequest -> Aff (err :: AWS.RequestError | eff) CreateHsmResponse
createHsm = AWS.request serviceName "createHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an HSM client.</p>
createLunaClient :: forall eff. CreateLunaClientRequest -> Aff (err :: AWS.RequestError | eff) CreateLunaClientResponse
createLunaClient = AWS.request serviceName "createLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a high-availability partition group.</p>
deleteHapg :: forall eff. DeleteHapgRequest -> Aff (err :: AWS.RequestError | eff) DeleteHapgResponse
deleteHapg = AWS.request serviceName "deleteHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.</p>
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (err :: AWS.RequestError | eff) DeleteHsmResponse
deleteHsm = AWS.request serviceName "deleteHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a client.</p>
deleteLunaClient :: forall eff. DeleteLunaClientRequest -> Aff (err :: AWS.RequestError | eff) DeleteLunaClientResponse
deleteLunaClient = AWS.request serviceName "deleteLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about a high-availability partition group.</p>
describeHapg :: forall eff. DescribeHapgRequest -> Aff (err :: AWS.RequestError | eff) DescribeHapgResponse
describeHapg = AWS.request serviceName "describeHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.</p>
describeHsm :: forall eff. DescribeHsmRequest -> Aff (err :: AWS.RequestError | eff) DescribeHsmResponse
describeHsm = AWS.request serviceName "describeHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM client.</p>
describeLunaClient :: forall eff. DescribeLunaClientRequest -> Aff (err :: AWS.RequestError | eff) DescribeLunaClientResponse
describeLunaClient = AWS.request serviceName "describeLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.</p>
getConfig :: forall eff. GetConfigRequest -> Aff (err :: AWS.RequestError | eff) GetConfigResponse
getConfig = AWS.request serviceName "getConfig" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the Availability Zones that have available AWS CloudHSM capacity.</p>
listAvailableZones :: forall eff. ListAvailableZonesRequest -> Aff (err :: AWS.RequestError | eff) ListAvailableZonesResponse
listAvailableZones = AWS.request serviceName "listAvailableZones" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the high-availability partition groups for the account.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHapgs</code> to retrieve the next set of items.</p>
listHapgs :: forall eff. ListHapgsRequest -> Aff (err :: AWS.RequestError | eff) ListHapgsResponse
listHapgs = AWS.request serviceName "listHapgs" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves the identifiers of all of the HSMs provisioned for the current customer.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHsms</code> to retrieve the next set of items.</p>
listHsms :: forall eff. ListHsmsRequest -> Aff (err :: AWS.RequestError | eff) ListHsmsResponse
listHsms = AWS.request serviceName "listHsms" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists all of the clients.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListLunaClients</code> to retrieve the next set of items.</p>
listLunaClients :: forall eff. ListLunaClientsRequest -> Aff (err :: AWS.RequestError | eff) ListLunaClientsResponse
listLunaClients = AWS.request serviceName "listLunaClients" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Returns a list of all tags for the specified AWS CloudHSM resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "listTagsForResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an existing high-availability partition group.</p>
modifyHapg :: forall eff. ModifyHapgRequest -> Aff (err :: AWS.RequestError | eff) ModifyHapgResponse
modifyHapg = AWS.request serviceName "modifyHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an HSM.</p> <important> <p>This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.</p> </important>
modifyHsm :: forall eff. ModifyHsmRequest -> Aff (err :: AWS.RequestError | eff) ModifyHsmResponse
modifyHsm = AWS.request serviceName "modifyHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies the certificate used by the client.</p> <p>This action can potentially start a workflow to install the new certificate on the client's HSMs.</p>
modifyLunaClient :: forall eff. ModifyLunaClientRequest -> Aff (err :: AWS.RequestError | eff) ModifyLunaClientResponse
modifyLunaClient = AWS.request serviceName "modifyLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Removes one or more tags from the specified AWS CloudHSM resource.</p> <p>To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use <a>AddTagsToResource</a>.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceResponse
removeTagsFromResource = AWS.request serviceName "removeTagsFromResource" 


newtype AZ = AZ String
derive instance newtypeAZ :: Newtype AZ _


newtype AZList = AZList (Array AZ)
derive instance newtypeAZList :: Newtype AZList _


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceArn" :: (String)
  , "TagList" :: (TagList)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _


newtype AddTagsToResourceResponse = AddTagsToResourceResponse 
  { "Status" :: (String)
  }
derive instance newtypeAddTagsToResourceResponse :: Newtype AddTagsToResourceResponse _


newtype Certificate = Certificate String
derive instance newtypeCertificate :: Newtype Certificate _


newtype CertificateFingerprint = CertificateFingerprint String
derive instance newtypeCertificateFingerprint :: Newtype CertificateFingerprint _


newtype ClientArn = ClientArn String
derive instance newtypeClientArn :: Newtype ClientArn _


newtype ClientLabel = ClientLabel String
derive instance newtypeClientLabel :: Newtype ClientLabel _


newtype ClientList = ClientList (Array ClientArn)
derive instance newtypeClientList :: Newtype ClientList _


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _


newtype ClientVersion = ClientVersion String
derive instance newtypeClientVersion :: Newtype ClientVersion _


-- | <p>Indicates that an internal error occurred.</p>
newtype CloudHsmInternalException = CloudHsmInternalException 
  { 
  }
derive instance newtypeCloudHsmInternalException :: Newtype CloudHsmInternalException _


newtype CloudHsmObjectState = CloudHsmObjectState String
derive instance newtypeCloudHsmObjectState :: Newtype CloudHsmObjectState _


-- | <p>Indicates that an exception occurred in the AWS CloudHSM service.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message'" :: NullOrUndefined (String)
  , "Retryable'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCloudHsmServiceException :: Newtype CloudHsmServiceException _


-- | <p>Contains the inputs for the <a>CreateHapgRequest</a> action.</p>
newtype CreateHapgRequest = CreateHapgRequest 
  { "Label" :: (Label)
  }
derive instance newtypeCreateHapgRequest :: Newtype CreateHapgRequest _


-- | <p>Contains the output of the <a>CreateHAPartitionGroup</a> action.</p>
newtype CreateHapgResponse = CreateHapgResponse 
  { "HapgArn" :: NullOrUndefined (HapgArn)
  }
derive instance newtypeCreateHapgResponse :: Newtype CreateHapgResponse _


-- | <p>Contains the inputs for the <code>CreateHsm</code> operation.</p>
newtype CreateHsmRequest = CreateHsmRequest 
  { "SubnetId" :: (SubnetId)
  , "SshKey" :: (SshKey)
  , "EniIp" :: NullOrUndefined (IpAddress)
  , "IamRoleArn" :: (IamRoleArn)
  , "ExternalId" :: NullOrUndefined (ExternalId)
  , "SubscriptionType" :: (SubscriptionType)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  , "SyslogIp" :: NullOrUndefined (IpAddress)
  }
derive instance newtypeCreateHsmRequest :: Newtype CreateHsmRequest _


-- | <p>Contains the output of the <code>CreateHsm</code> operation.</p>
newtype CreateHsmResponse = CreateHsmResponse 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  }
derive instance newtypeCreateHsmResponse :: Newtype CreateHsmResponse _


-- | <p>Contains the inputs for the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientRequest = CreateLunaClientRequest 
  { "Label" :: NullOrUndefined (ClientLabel)
  , "Certificate" :: (Certificate)
  }
derive instance newtypeCreateLunaClientRequest :: Newtype CreateLunaClientRequest _


-- | <p>Contains the output of the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientResponse = CreateLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  }
derive instance newtypeCreateLunaClientResponse :: Newtype CreateLunaClientResponse _


-- | <p>Contains the inputs for the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgRequest = DeleteHapgRequest 
  { "HapgArn" :: (HapgArn)
  }
derive instance newtypeDeleteHapgRequest :: Newtype DeleteHapgRequest _


-- | <p>Contains the output of the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgResponse = DeleteHapgResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteHapgResponse :: Newtype DeleteHapgResponse _


-- | <p>Contains the inputs for the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmRequest = DeleteHsmRequest 
  { "HsmArn" :: (HsmArn)
  }
derive instance newtypeDeleteHsmRequest :: Newtype DeleteHsmRequest _


-- | <p>Contains the output of the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmResponse = DeleteHsmResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteHsmResponse :: Newtype DeleteHsmResponse _


newtype DeleteLunaClientRequest = DeleteLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  }
derive instance newtypeDeleteLunaClientRequest :: Newtype DeleteLunaClientRequest _


newtype DeleteLunaClientResponse = DeleteLunaClientResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteLunaClientResponse :: Newtype DeleteLunaClientResponse _


-- | <p>Contains the inputs for the <a>DescribeHapg</a> action.</p>
newtype DescribeHapgRequest = DescribeHapgRequest 
  { "HapgArn" :: (HapgArn)
  }
derive instance newtypeDescribeHapgRequest :: Newtype DescribeHapgRequest _


-- | <p>Contains the output of the <a>DescribeHapg</a> action.</p>
newtype DescribeHapgResponse = DescribeHapgResponse 
  { "HapgArn" :: NullOrUndefined (HapgArn)
  , "HapgSerial" :: NullOrUndefined (String)
  , "HsmsLastActionFailed" :: NullOrUndefined (HsmList)
  , "HsmsPendingDeletion" :: NullOrUndefined (HsmList)
  , "HsmsPendingRegistration" :: NullOrUndefined (HsmList)
  , "Label" :: NullOrUndefined (Label)
  , "LastModifiedTimestamp" :: NullOrUndefined (Number)
  , "PartitionSerialList" :: NullOrUndefined (PartitionSerialList)
  , "State" :: NullOrUndefined (CloudHsmObjectState)
  }
derive instance newtypeDescribeHapgResponse :: Newtype DescribeHapgResponse _


-- | <p>Contains the inputs for the <a>DescribeHsm</a> operation.</p>
newtype DescribeHsmRequest = DescribeHsmRequest 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  , "HsmSerialNumber" :: NullOrUndefined (HsmSerialNumber)
  }
derive instance newtypeDescribeHsmRequest :: Newtype DescribeHsmRequest _


-- | <p>Contains the output of the <a>DescribeHsm</a> operation.</p>
newtype DescribeHsmResponse = DescribeHsmResponse 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  , "Status" :: NullOrUndefined (HsmStatus)
  , "StatusDetails" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (AZ)
  , "EniId" :: NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  , "SubscriptionType" :: NullOrUndefined (SubscriptionType)
  , "SubscriptionStartDate" :: NullOrUndefined (Number)
  , "SubscriptionEndDate" :: NullOrUndefined (Number)
  , "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "IamRoleArn" :: NullOrUndefined (IamRoleArn)
  , "SerialNumber" :: NullOrUndefined (HsmSerialNumber)
  , "VendorName" :: NullOrUndefined (String)
  , "HsmType" :: NullOrUndefined (String)
  , "SoftwareVersion" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (SshKey)
  , "SshKeyLastUpdated" :: NullOrUndefined (Number)
  , "ServerCertUri" :: NullOrUndefined (String)
  , "ServerCertLastUpdated" :: NullOrUndefined (Number)
  , "Partitions" :: NullOrUndefined (PartitionList)
  }
derive instance newtypeDescribeHsmResponse :: Newtype DescribeHsmResponse _


newtype DescribeLunaClientRequest = DescribeLunaClientRequest 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  , "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint)
  }
derive instance newtypeDescribeLunaClientRequest :: Newtype DescribeLunaClientRequest _


newtype DescribeLunaClientResponse = DescribeLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  , "Certificate" :: NullOrUndefined (Certificate)
  , "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint)
  , "LastModifiedTimestamp" :: NullOrUndefined (Number)
  , "Label" :: NullOrUndefined (Label)
  }
derive instance newtypeDescribeLunaClientResponse :: Newtype DescribeLunaClientResponse _


newtype EniId = EniId String
derive instance newtypeEniId :: Newtype EniId _


newtype ExternalId = ExternalId String
derive instance newtypeExternalId :: Newtype ExternalId _


newtype GetConfigRequest = GetConfigRequest 
  { "ClientArn" :: (ClientArn)
  , "ClientVersion" :: (ClientVersion)
  , "HapgList" :: (HapgList)
  }
derive instance newtypeGetConfigRequest :: Newtype GetConfigRequest _


newtype GetConfigResponse = GetConfigResponse 
  { "ConfigType" :: NullOrUndefined (String)
  , "ConfigFile" :: NullOrUndefined (String)
  , "ConfigCred" :: NullOrUndefined (String)
  }
derive instance newtypeGetConfigResponse :: Newtype GetConfigResponse _


newtype HapgArn = HapgArn String
derive instance newtypeHapgArn :: Newtype HapgArn _


newtype HapgList = HapgList (Array HapgArn)
derive instance newtypeHapgList :: Newtype HapgList _


-- | <p>An ARN that identifies an HSM.</p>
newtype HsmArn = HsmArn String
derive instance newtypeHsmArn :: Newtype HsmArn _


-- | <p>Contains a list of ARNs that identify the HSMs.</p>
newtype HsmList = HsmList (Array HsmArn)
derive instance newtypeHsmList :: Newtype HsmList _


newtype HsmSerialNumber = HsmSerialNumber String
derive instance newtypeHsmSerialNumber :: Newtype HsmSerialNumber _


newtype HsmStatus = HsmStatus String
derive instance newtypeHsmStatus :: Newtype HsmStatus _


newtype IamRoleArn = IamRoleArn String
derive instance newtypeIamRoleArn :: Newtype IamRoleArn _


-- | <p>Indicates that one or more of the request parameters are not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { 
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


newtype Label = Label String
derive instance newtypeLabel :: Newtype Label _


-- | <p>Contains the inputs for the <a>ListAvailableZones</a> action.</p>
newtype ListAvailableZonesRequest = ListAvailableZonesRequest 
  { 
  }
derive instance newtypeListAvailableZonesRequest :: Newtype ListAvailableZonesRequest _


newtype ListAvailableZonesResponse = ListAvailableZonesResponse 
  { "AZList" :: NullOrUndefined (AZList)
  }
derive instance newtypeListAvailableZonesResponse :: Newtype ListAvailableZonesResponse _


newtype ListHapgsRequest = ListHapgsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHapgsRequest :: Newtype ListHapgsRequest _


newtype ListHapgsResponse = ListHapgsResponse 
  { "HapgList" :: (HapgList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHapgsResponse :: Newtype ListHapgsResponse _


newtype ListHsmsRequest = ListHsmsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHsmsRequest :: Newtype ListHsmsRequest _


-- | <p>Contains the output of the <code>ListHsms</code> operation.</p>
newtype ListHsmsResponse = ListHsmsResponse 
  { "HsmList" :: NullOrUndefined (HsmList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHsmsResponse :: Newtype ListHsmsResponse _


newtype ListLunaClientsRequest = ListLunaClientsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListLunaClientsRequest :: Newtype ListLunaClientsRequest _


newtype ListLunaClientsResponse = ListLunaClientsResponse 
  { "ClientList" :: (ClientList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListLunaClientsResponse :: Newtype ListLunaClientsResponse _


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (String)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "TagList" :: (TagList)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _


newtype ModifyHapgRequest = ModifyHapgRequest 
  { "HapgArn" :: (HapgArn)
  , "Label" :: NullOrUndefined (Label)
  , "PartitionSerialList" :: NullOrUndefined (PartitionSerialList)
  }
derive instance newtypeModifyHapgRequest :: Newtype ModifyHapgRequest _


newtype ModifyHapgResponse = ModifyHapgResponse 
  { "HapgArn" :: NullOrUndefined (HapgArn)
  }
derive instance newtypeModifyHapgResponse :: Newtype ModifyHapgResponse _


-- | <p>Contains the inputs for the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmRequest = ModifyHsmRequest 
  { "HsmArn" :: (HsmArn)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  , "IamRoleArn" :: NullOrUndefined (IamRoleArn)
  , "ExternalId" :: NullOrUndefined (ExternalId)
  , "SyslogIp" :: NullOrUndefined (IpAddress)
  }
derive instance newtypeModifyHsmRequest :: Newtype ModifyHsmRequest _


-- | <p>Contains the output of the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmResponse = ModifyHsmResponse 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  }
derive instance newtypeModifyHsmResponse :: Newtype ModifyHsmResponse _


newtype ModifyLunaClientRequest = ModifyLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  , "Certificate" :: (Certificate)
  }
derive instance newtypeModifyLunaClientRequest :: Newtype ModifyLunaClientRequest _


newtype ModifyLunaClientResponse = ModifyLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  }
derive instance newtypeModifyLunaClientResponse :: Newtype ModifyLunaClientResponse _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


newtype PartitionArn = PartitionArn String
derive instance newtypePartitionArn :: Newtype PartitionArn _


newtype PartitionList = PartitionList (Array PartitionArn)
derive instance newtypePartitionList :: Newtype PartitionList _


newtype PartitionSerial = PartitionSerial String
derive instance newtypePartitionSerial :: Newtype PartitionSerial _


newtype PartitionSerialList = PartitionSerialList (Array PartitionSerial)
derive instance newtypePartitionSerialList :: Newtype PartitionSerialList _


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceArn" :: (String)
  , "TagKeyList" :: (TagKeyList)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _


newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse 
  { "Status" :: (String)
  }
derive instance newtypeRemoveTagsFromResourceResponse :: Newtype RemoveTagsFromResourceResponse _


newtype SshKey = SshKey String
derive instance newtypeSshKey :: Newtype SshKey _


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _


-- | <p>Specifies the type of subscription for the HSM.</p> <ul> <li> <p> <b>PRODUCTION</b> - The HSM is being used in a production environment.</p> </li> <li> <p> <b>TRIAL</b> - The HSM is being used in a product trial.</p> </li> </ul>
newtype SubscriptionType = SubscriptionType String
derive instance newtypeSubscriptionType :: Newtype SubscriptionType _


-- | <p>A key-value pair that identifies or specifies metadata about an AWS CloudHSM resource.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
