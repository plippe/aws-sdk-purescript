

-- | <fullname>AWS CloudHSM Service</fullname> <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p>
module AWS.CloudHSM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudHSM" :: String


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Adds or overwrites one or more tags for the specified AWS CloudHSM resource.</p> <p>Each tag consists of a key and a value. Tag keys must be unique to each resource.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceResponse
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.</p>
createHapg :: forall eff. CreateHapgRequest -> Aff (err :: AWS.RequestError | eff) CreateHapgResponse
createHapg = AWS.request serviceName "CreateHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an uninitialized HSM instance.</p> <p>There is an upfront fee charged for each HSM instance that you create with the <code>CreateHsm</code> operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the <a>DeleteHsm</a> operation, go to the <a href="https://console.aws.amazon.com/support/home">AWS Support Center</a>, create a new case, and select <b>Account and Billing Support</b>.</p> <important> <p>It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the <a>DescribeHsm</a> operation. The HSM is ready to be initialized when the status changes to <code>RUNNING</code>.</p> </important>
createHsm :: forall eff. CreateHsmRequest -> Aff (err :: AWS.RequestError | eff) CreateHsmResponse
createHsm = AWS.request serviceName "CreateHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an HSM client.</p>
createLunaClient :: forall eff. CreateLunaClientRequest -> Aff (err :: AWS.RequestError | eff) CreateLunaClientResponse
createLunaClient = AWS.request serviceName "CreateLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a high-availability partition group.</p>
deleteHapg :: forall eff. DeleteHapgRequest -> Aff (err :: AWS.RequestError | eff) DeleteHapgResponse
deleteHapg = AWS.request serviceName "DeleteHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.</p>
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (err :: AWS.RequestError | eff) DeleteHsmResponse
deleteHsm = AWS.request serviceName "DeleteHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a client.</p>
deleteLunaClient :: forall eff. DeleteLunaClientRequest -> Aff (err :: AWS.RequestError | eff) DeleteLunaClientResponse
deleteLunaClient = AWS.request serviceName "DeleteLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about a high-availability partition group.</p>
describeHapg :: forall eff. DescribeHapgRequest -> Aff (err :: AWS.RequestError | eff) DescribeHapgResponse
describeHapg = AWS.request serviceName "DescribeHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.</p>
describeHsm :: forall eff. DescribeHsmRequest -> Aff (err :: AWS.RequestError | eff) DescribeHsmResponse
describeHsm = AWS.request serviceName "DescribeHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM client.</p>
describeLunaClient :: forall eff. DescribeLunaClientRequest -> Aff (err :: AWS.RequestError | eff) DescribeLunaClientResponse
describeLunaClient = AWS.request serviceName "DescribeLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.</p>
getConfig :: forall eff. GetConfigRequest -> Aff (err :: AWS.RequestError | eff) GetConfigResponse
getConfig = AWS.request serviceName "GetConfig" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the Availability Zones that have available AWS CloudHSM capacity.</p>
listAvailableZones :: forall eff. ListAvailableZonesRequest -> Aff (err :: AWS.RequestError | eff) ListAvailableZonesResponse
listAvailableZones = AWS.request serviceName "ListAvailableZones" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the high-availability partition groups for the account.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHapgs</code> to retrieve the next set of items.</p>
listHapgs :: forall eff. ListHapgsRequest -> Aff (err :: AWS.RequestError | eff) ListHapgsResponse
listHapgs = AWS.request serviceName "ListHapgs" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves the identifiers of all of the HSMs provisioned for the current customer.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHsms</code> to retrieve the next set of items.</p>
listHsms :: forall eff. ListHsmsRequest -> Aff (err :: AWS.RequestError | eff) ListHsmsResponse
listHsms = AWS.request serviceName "ListHsms" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists all of the clients.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListLunaClients</code> to retrieve the next set of items.</p>
listLunaClients :: forall eff. ListLunaClientsRequest -> Aff (err :: AWS.RequestError | eff) ListLunaClientsResponse
listLunaClients = AWS.request serviceName "ListLunaClients" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Returns a list of all tags for the specified AWS CloudHSM resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an existing high-availability partition group.</p>
modifyHapg :: forall eff. ModifyHapgRequest -> Aff (err :: AWS.RequestError | eff) ModifyHapgResponse
modifyHapg = AWS.request serviceName "ModifyHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an HSM.</p> <important> <p>This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.</p> </important>
modifyHsm :: forall eff. ModifyHsmRequest -> Aff (err :: AWS.RequestError | eff) ModifyHsmResponse
modifyHsm = AWS.request serviceName "ModifyHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies the certificate used by the client.</p> <p>This action can potentially start a workflow to install the new certificate on the client's HSMs.</p>
modifyLunaClient :: forall eff. ModifyLunaClientRequest -> Aff (err :: AWS.RequestError | eff) ModifyLunaClientResponse
modifyLunaClient = AWS.request serviceName "ModifyLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Removes one or more tags from the specified AWS CloudHSM resource.</p> <p>To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use <a>AddTagsToResource</a>.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceResponse
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


newtype AZ = AZ String


newtype AZList = AZList (Array AZ)


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceArn" :: (String)
  , "TagList" :: (TagList)
  }


newtype AddTagsToResourceResponse = AddTagsToResourceResponse 
  { "Status" :: (String)
  }


newtype Certificate = Certificate String


newtype CertificateFingerprint = CertificateFingerprint String


newtype ClientArn = ClientArn String


newtype ClientLabel = ClientLabel String


newtype ClientList = ClientList (Array ClientArn)


newtype ClientToken = ClientToken String


newtype ClientVersion = ClientVersion String


-- | <p>Indicates that an internal error occurred.</p>
newtype CloudHsmInternalException = CloudHsmInternalException 
  { 
  }


newtype CloudHsmObjectState = CloudHsmObjectState String


-- | <p>Indicates that an exception occurred in the AWS CloudHSM service.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message'" :: NullOrUndefined (String)
  , "Retryable'" :: NullOrUndefined (Boolean)
  }


-- | <p>Contains the inputs for the <a>CreateHapgRequest</a> action.</p>
newtype CreateHapgRequest = CreateHapgRequest 
  { "Label" :: (Label)
  }


-- | <p>Contains the output of the <a>CreateHAPartitionGroup</a> action.</p>
newtype CreateHapgResponse = CreateHapgResponse 
  { "HapgArn" :: NullOrUndefined (HapgArn)
  }


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


-- | <p>Contains the output of the <code>CreateHsm</code> operation.</p>
newtype CreateHsmResponse = CreateHsmResponse 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  }


-- | <p>Contains the inputs for the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientRequest = CreateLunaClientRequest 
  { "Label" :: NullOrUndefined (ClientLabel)
  , "Certificate" :: (Certificate)
  }


-- | <p>Contains the output of the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientResponse = CreateLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  }


-- | <p>Contains the inputs for the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgRequest = DeleteHapgRequest 
  { "HapgArn" :: (HapgArn)
  }


-- | <p>Contains the output of the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgResponse = DeleteHapgResponse 
  { "Status" :: (String)
  }


-- | <p>Contains the inputs for the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmRequest = DeleteHsmRequest 
  { "HsmArn" :: (HsmArn)
  }


-- | <p>Contains the output of the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmResponse = DeleteHsmResponse 
  { "Status" :: (String)
  }


newtype DeleteLunaClientRequest = DeleteLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  }


newtype DeleteLunaClientResponse = DeleteLunaClientResponse 
  { "Status" :: (String)
  }


-- | <p>Contains the inputs for the <a>DescribeHapg</a> action.</p>
newtype DescribeHapgRequest = DescribeHapgRequest 
  { "HapgArn" :: (HapgArn)
  }


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


-- | <p>Contains the inputs for the <a>DescribeHsm</a> operation.</p>
newtype DescribeHsmRequest = DescribeHsmRequest 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  , "HsmSerialNumber" :: NullOrUndefined (HsmSerialNumber)
  }


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


newtype DescribeLunaClientRequest = DescribeLunaClientRequest 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  , "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint)
  }


newtype DescribeLunaClientResponse = DescribeLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  , "Certificate" :: NullOrUndefined (Certificate)
  , "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint)
  , "LastModifiedTimestamp" :: NullOrUndefined (Number)
  , "Label" :: NullOrUndefined (Label)
  }


newtype EniId = EniId String


newtype ExternalId = ExternalId String


newtype GetConfigRequest = GetConfigRequest 
  { "ClientArn" :: (ClientArn)
  , "ClientVersion" :: (ClientVersion)
  , "HapgList" :: (HapgList)
  }


newtype GetConfigResponse = GetConfigResponse 
  { "ConfigType" :: NullOrUndefined (String)
  , "ConfigFile" :: NullOrUndefined (String)
  , "ConfigCred" :: NullOrUndefined (String)
  }


newtype HapgArn = HapgArn String


newtype HapgList = HapgList (Array HapgArn)


-- | <p>An ARN that identifies an HSM.</p>
newtype HsmArn = HsmArn String


-- | <p>Contains a list of ARNs that identify the HSMs.</p>
newtype HsmList = HsmList (Array HsmArn)


newtype HsmSerialNumber = HsmSerialNumber String


newtype HsmStatus = HsmStatus String


newtype IamRoleArn = IamRoleArn String


-- | <p>Indicates that one or more of the request parameters are not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { 
  }


newtype IpAddress = IpAddress String


newtype Label = Label String


-- | <p>Contains the inputs for the <a>ListAvailableZones</a> action.</p>
newtype ListAvailableZonesRequest = ListAvailableZonesRequest 
  { 
  }


newtype ListAvailableZonesResponse = ListAvailableZonesResponse 
  { "AZList" :: NullOrUndefined (AZList)
  }


newtype ListHapgsRequest = ListHapgsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListHapgsResponse = ListHapgsResponse 
  { "HapgList" :: (HapgList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListHsmsRequest = ListHsmsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }


-- | <p>Contains the output of the <code>ListHsms</code> operation.</p>
newtype ListHsmsResponse = ListHsmsResponse 
  { "HsmList" :: NullOrUndefined (HsmList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListLunaClientsRequest = ListLunaClientsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListLunaClientsResponse = ListLunaClientsResponse 
  { "ClientList" :: (ClientList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (String)
  }


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "TagList" :: (TagList)
  }


newtype ModifyHapgRequest = ModifyHapgRequest 
  { "HapgArn" :: (HapgArn)
  , "Label" :: NullOrUndefined (Label)
  , "PartitionSerialList" :: NullOrUndefined (PartitionSerialList)
  }


newtype ModifyHapgResponse = ModifyHapgResponse 
  { "HapgArn" :: NullOrUndefined (HapgArn)
  }


-- | <p>Contains the inputs for the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmRequest = ModifyHsmRequest 
  { "HsmArn" :: (HsmArn)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "EniIp" :: NullOrUndefined (IpAddress)
  , "IamRoleArn" :: NullOrUndefined (IamRoleArn)
  , "ExternalId" :: NullOrUndefined (ExternalId)
  , "SyslogIp" :: NullOrUndefined (IpAddress)
  }


-- | <p>Contains the output of the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmResponse = ModifyHsmResponse 
  { "HsmArn" :: NullOrUndefined (HsmArn)
  }


newtype ModifyLunaClientRequest = ModifyLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  , "Certificate" :: (Certificate)
  }


newtype ModifyLunaClientResponse = ModifyLunaClientResponse 
  { "ClientArn" :: NullOrUndefined (ClientArn)
  }


newtype PaginationToken = PaginationToken String


newtype PartitionArn = PartitionArn String


newtype PartitionList = PartitionList (Array PartitionArn)


newtype PartitionSerial = PartitionSerial String


newtype PartitionSerialList = PartitionSerialList (Array PartitionSerial)


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceArn" :: (String)
  , "TagKeyList" :: (TagKeyList)
  }


newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse 
  { "Status" :: (String)
  }


newtype SshKey = SshKey String


newtype SubnetId = SubnetId String


-- | <p>Specifies the type of subscription for the HSM.</p> <ul> <li> <p> <b>PRODUCTION</b> - The HSM is being used in a production environment.</p> </li> <li> <p> <b>TRIAL</b> - The HSM is being used in a product trial.</p> </li> </ul>
newtype SubscriptionType = SubscriptionType String


-- | <p>A key-value pair that identifies or specifies metadata about an AWS CloudHSM resource.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


newtype VpcId = VpcId String
