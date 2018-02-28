

-- | <fullname>AWS CloudHSM Service</fullname> <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p>
module AWS.CloudHSM where

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

serviceName = "CloudHSM" :: String


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Adds or overwrites one or more tags for the specified AWS CloudHSM resource.</p> <p>Each tag consists of a key and a value. Tag keys must be unique to each resource.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (exception :: EXCEPTION | eff) AddTagsToResourceResponse
addTagsToResource = Request.request serviceName "addTagsToResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.</p>
createHapg :: forall eff. CreateHapgRequest -> Aff (exception :: EXCEPTION | eff) CreateHapgResponse
createHapg = Request.request serviceName "createHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an uninitialized HSM instance.</p> <p>There is an upfront fee charged for each HSM instance that you create with the <code>CreateHsm</code> operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the <a>DeleteHsm</a> operation, go to the <a href="https://console.aws.amazon.com/support/home">AWS Support Center</a>, create a new case, and select <b>Account and Billing Support</b>.</p> <important> <p>It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the <a>DescribeHsm</a> operation. The HSM is ready to be initialized when the status changes to <code>RUNNING</code>.</p> </important>
createHsm :: forall eff. CreateHsmRequest -> Aff (exception :: EXCEPTION | eff) CreateHsmResponse
createHsm = Request.request serviceName "createHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an HSM client.</p>
createLunaClient :: forall eff. CreateLunaClientRequest -> Aff (exception :: EXCEPTION | eff) CreateLunaClientResponse
createLunaClient = Request.request serviceName "createLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a high-availability partition group.</p>
deleteHapg :: forall eff. DeleteHapgRequest -> Aff (exception :: EXCEPTION | eff) DeleteHapgResponse
deleteHapg = Request.request serviceName "deleteHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.</p>
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (exception :: EXCEPTION | eff) DeleteHsmResponse
deleteHsm = Request.request serviceName "deleteHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a client.</p>
deleteLunaClient :: forall eff. DeleteLunaClientRequest -> Aff (exception :: EXCEPTION | eff) DeleteLunaClientResponse
deleteLunaClient = Request.request serviceName "deleteLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about a high-availability partition group.</p>
describeHapg :: forall eff. DescribeHapgRequest -> Aff (exception :: EXCEPTION | eff) DescribeHapgResponse
describeHapg = Request.request serviceName "describeHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.</p>
describeHsm :: forall eff. DescribeHsmRequest -> Aff (exception :: EXCEPTION | eff) DescribeHsmResponse
describeHsm = Request.request serviceName "describeHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM client.</p>
describeLunaClient :: forall eff. DescribeLunaClientRequest -> Aff (exception :: EXCEPTION | eff) DescribeLunaClientResponse
describeLunaClient = Request.request serviceName "describeLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.</p>
getConfig :: forall eff. GetConfigRequest -> Aff (exception :: EXCEPTION | eff) GetConfigResponse
getConfig = Request.request serviceName "getConfig" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the Availability Zones that have available AWS CloudHSM capacity.</p>
listAvailableZones :: forall eff. ListAvailableZonesRequest -> Aff (exception :: EXCEPTION | eff) ListAvailableZonesResponse
listAvailableZones = Request.request serviceName "listAvailableZones" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the high-availability partition groups for the account.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHapgs</code> to retrieve the next set of items.</p>
listHapgs :: forall eff. ListHapgsRequest -> Aff (exception :: EXCEPTION | eff) ListHapgsResponse
listHapgs = Request.request serviceName "listHapgs" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves the identifiers of all of the HSMs provisioned for the current customer.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHsms</code> to retrieve the next set of items.</p>
listHsms :: forall eff. ListHsmsRequest -> Aff (exception :: EXCEPTION | eff) ListHsmsResponse
listHsms = Request.request serviceName "listHsms" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists all of the clients.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListLunaClients</code> to retrieve the next set of items.</p>
listLunaClients :: forall eff. ListLunaClientsRequest -> Aff (exception :: EXCEPTION | eff) ListLunaClientsResponse
listLunaClients = Request.request serviceName "listLunaClients" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Returns a list of all tags for the specified AWS CloudHSM resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResponse
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an existing high-availability partition group.</p>
modifyHapg :: forall eff. ModifyHapgRequest -> Aff (exception :: EXCEPTION | eff) ModifyHapgResponse
modifyHapg = Request.request serviceName "modifyHapg" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an HSM.</p> <important> <p>This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.</p> </important>
modifyHsm :: forall eff. ModifyHsmRequest -> Aff (exception :: EXCEPTION | eff) ModifyHsmResponse
modifyHsm = Request.request serviceName "modifyHsm" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies the certificate used by the client.</p> <p>This action can potentially start a workflow to install the new certificate on the client's HSMs.</p>
modifyLunaClient :: forall eff. ModifyLunaClientRequest -> Aff (exception :: EXCEPTION | eff) ModifyLunaClientResponse
modifyLunaClient = Request.request serviceName "modifyLunaClient" 


-- | <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Removes one or more tags from the specified AWS CloudHSM resource.</p> <p>To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use <a>AddTagsToResource</a>.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (exception :: EXCEPTION | eff) RemoveTagsFromResourceResponse
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


newtype AZ = AZ String
derive instance newtypeAZ :: Newtype AZ _
derive instance repGenericAZ :: Generic AZ _
instance showAZ :: Show AZ where
  show = genericShow
instance decodeAZ :: Decode AZ where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAZ :: Encode AZ where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AZList = AZList (Array AZ)
derive instance newtypeAZList :: Newtype AZList _
derive instance repGenericAZList :: Generic AZList _
instance showAZList :: Show AZList where
  show = genericShow
instance decodeAZList :: Decode AZList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAZList :: Encode AZList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceArn" :: (String)
  , "TagList" :: (TagList)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _
derive instance repGenericAddTagsToResourceRequest :: Generic AddTagsToResourceRequest _
instance showAddTagsToResourceRequest :: Show AddTagsToResourceRequest where
  show = genericShow
instance decodeAddTagsToResourceRequest :: Decode AddTagsToResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceRequest :: Encode AddTagsToResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceResponse = AddTagsToResourceResponse 
  { "Status" :: (String)
  }
derive instance newtypeAddTagsToResourceResponse :: Newtype AddTagsToResourceResponse _
derive instance repGenericAddTagsToResourceResponse :: Generic AddTagsToResourceResponse _
instance showAddTagsToResourceResponse :: Show AddTagsToResourceResponse where
  show = genericShow
instance decodeAddTagsToResourceResponse :: Decode AddTagsToResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceResponse :: Encode AddTagsToResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Certificate = Certificate String
derive instance newtypeCertificate :: Newtype Certificate _
derive instance repGenericCertificate :: Generic Certificate _
instance showCertificate :: Show Certificate where
  show = genericShow
instance decodeCertificate :: Decode Certificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificate :: Encode Certificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateFingerprint = CertificateFingerprint String
derive instance newtypeCertificateFingerprint :: Newtype CertificateFingerprint _
derive instance repGenericCertificateFingerprint :: Generic CertificateFingerprint _
instance showCertificateFingerprint :: Show CertificateFingerprint where
  show = genericShow
instance decodeCertificateFingerprint :: Decode CertificateFingerprint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateFingerprint :: Encode CertificateFingerprint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientArn = ClientArn String
derive instance newtypeClientArn :: Newtype ClientArn _
derive instance repGenericClientArn :: Generic ClientArn _
instance showClientArn :: Show ClientArn where
  show = genericShow
instance decodeClientArn :: Decode ClientArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientArn :: Encode ClientArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientLabel = ClientLabel String
derive instance newtypeClientLabel :: Newtype ClientLabel _
derive instance repGenericClientLabel :: Generic ClientLabel _
instance showClientLabel :: Show ClientLabel where
  show = genericShow
instance decodeClientLabel :: Decode ClientLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientLabel :: Encode ClientLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientList = ClientList (Array ClientArn)
derive instance newtypeClientList :: Newtype ClientList _
derive instance repGenericClientList :: Generic ClientList _
instance showClientList :: Show ClientList where
  show = genericShow
instance decodeClientList :: Decode ClientList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientList :: Encode ClientList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _
derive instance repGenericClientToken :: Generic ClientToken _
instance showClientToken :: Show ClientToken where
  show = genericShow
instance decodeClientToken :: Decode ClientToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientToken :: Encode ClientToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientVersion = ClientVersion String
derive instance newtypeClientVersion :: Newtype ClientVersion _
derive instance repGenericClientVersion :: Generic ClientVersion _
instance showClientVersion :: Show ClientVersion where
  show = genericShow
instance decodeClientVersion :: Decode ClientVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientVersion :: Encode ClientVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that an internal error occurred.</p>
newtype CloudHsmInternalException = CloudHsmInternalException Types.NoArguments
derive instance newtypeCloudHsmInternalException :: Newtype CloudHsmInternalException _
derive instance repGenericCloudHsmInternalException :: Generic CloudHsmInternalException _
instance showCloudHsmInternalException :: Show CloudHsmInternalException where
  show = genericShow
instance decodeCloudHsmInternalException :: Decode CloudHsmInternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmInternalException :: Encode CloudHsmInternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudHsmObjectState = CloudHsmObjectState String
derive instance newtypeCloudHsmObjectState :: Newtype CloudHsmObjectState _
derive instance repGenericCloudHsmObjectState :: Generic CloudHsmObjectState _
instance showCloudHsmObjectState :: Show CloudHsmObjectState where
  show = genericShow
instance decodeCloudHsmObjectState :: Decode CloudHsmObjectState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmObjectState :: Encode CloudHsmObjectState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that an exception occurred in the AWS CloudHSM service.</p>
newtype CloudHsmServiceException = CloudHsmServiceException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  , "Retryable'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCloudHsmServiceException :: Newtype CloudHsmServiceException _
derive instance repGenericCloudHsmServiceException :: Generic CloudHsmServiceException _
instance showCloudHsmServiceException :: Show CloudHsmServiceException where
  show = genericShow
instance decodeCloudHsmServiceException :: Decode CloudHsmServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudHsmServiceException :: Encode CloudHsmServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateHapgRequest</a> action.</p>
newtype CreateHapgRequest = CreateHapgRequest 
  { "Label" :: (Label)
  }
derive instance newtypeCreateHapgRequest :: Newtype CreateHapgRequest _
derive instance repGenericCreateHapgRequest :: Generic CreateHapgRequest _
instance showCreateHapgRequest :: Show CreateHapgRequest where
  show = genericShow
instance decodeCreateHapgRequest :: Decode CreateHapgRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHapgRequest :: Encode CreateHapgRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>CreateHAPartitionGroup</a> action.</p>
newtype CreateHapgResponse = CreateHapgResponse 
  { "HapgArn" :: NullOrUndefined.NullOrUndefined (HapgArn)
  }
derive instance newtypeCreateHapgResponse :: Newtype CreateHapgResponse _
derive instance repGenericCreateHapgResponse :: Generic CreateHapgResponse _
instance showCreateHapgResponse :: Show CreateHapgResponse where
  show = genericShow
instance decodeCreateHapgResponse :: Decode CreateHapgResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHapgResponse :: Encode CreateHapgResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <code>CreateHsm</code> operation.</p>
newtype CreateHsmRequest = CreateHsmRequest 
  { "SubnetId" :: (SubnetId)
  , "SshKey" :: (SshKey)
  , "EniIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "IamRoleArn" :: (IamRoleArn)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (ExternalId)
  , "SubscriptionType" :: (SubscriptionType)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  , "SyslogIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  }
derive instance newtypeCreateHsmRequest :: Newtype CreateHsmRequest _
derive instance repGenericCreateHsmRequest :: Generic CreateHsmRequest _
instance showCreateHsmRequest :: Show CreateHsmRequest where
  show = genericShow
instance decodeCreateHsmRequest :: Decode CreateHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmRequest :: Encode CreateHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <code>CreateHsm</code> operation.</p>
newtype CreateHsmResponse = CreateHsmResponse 
  { "HsmArn" :: NullOrUndefined.NullOrUndefined (HsmArn)
  }
derive instance newtypeCreateHsmResponse :: Newtype CreateHsmResponse _
derive instance repGenericCreateHsmResponse :: Generic CreateHsmResponse _
instance showCreateHsmResponse :: Show CreateHsmResponse where
  show = genericShow
instance decodeCreateHsmResponse :: Decode CreateHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHsmResponse :: Encode CreateHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientRequest = CreateLunaClientRequest 
  { "Label" :: NullOrUndefined.NullOrUndefined (ClientLabel)
  , "Certificate" :: (Certificate)
  }
derive instance newtypeCreateLunaClientRequest :: Newtype CreateLunaClientRequest _
derive instance repGenericCreateLunaClientRequest :: Generic CreateLunaClientRequest _
instance showCreateLunaClientRequest :: Show CreateLunaClientRequest where
  show = genericShow
instance decodeCreateLunaClientRequest :: Decode CreateLunaClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLunaClientRequest :: Encode CreateLunaClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>CreateLunaClient</a> action.</p>
newtype CreateLunaClientResponse = CreateLunaClientResponse 
  { "ClientArn" :: NullOrUndefined.NullOrUndefined (ClientArn)
  }
derive instance newtypeCreateLunaClientResponse :: Newtype CreateLunaClientResponse _
derive instance repGenericCreateLunaClientResponse :: Generic CreateLunaClientResponse _
instance showCreateLunaClientResponse :: Show CreateLunaClientResponse where
  show = genericShow
instance decodeCreateLunaClientResponse :: Decode CreateLunaClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLunaClientResponse :: Encode CreateLunaClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgRequest = DeleteHapgRequest 
  { "HapgArn" :: (HapgArn)
  }
derive instance newtypeDeleteHapgRequest :: Newtype DeleteHapgRequest _
derive instance repGenericDeleteHapgRequest :: Generic DeleteHapgRequest _
instance showDeleteHapgRequest :: Show DeleteHapgRequest where
  show = genericShow
instance decodeDeleteHapgRequest :: Decode DeleteHapgRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHapgRequest :: Encode DeleteHapgRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>DeleteHapg</a> action.</p>
newtype DeleteHapgResponse = DeleteHapgResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteHapgResponse :: Newtype DeleteHapgResponse _
derive instance repGenericDeleteHapgResponse :: Generic DeleteHapgResponse _
instance showDeleteHapgResponse :: Show DeleteHapgResponse where
  show = genericShow
instance decodeDeleteHapgResponse :: Decode DeleteHapgResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHapgResponse :: Encode DeleteHapgResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmRequest = DeleteHsmRequest 
  { "HsmArn" :: (HsmArn)
  }
derive instance newtypeDeleteHsmRequest :: Newtype DeleteHsmRequest _
derive instance repGenericDeleteHsmRequest :: Generic DeleteHsmRequest _
instance showDeleteHsmRequest :: Show DeleteHsmRequest where
  show = genericShow
instance decodeDeleteHsmRequest :: Decode DeleteHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmRequest :: Encode DeleteHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>DeleteHsm</a> operation.</p>
newtype DeleteHsmResponse = DeleteHsmResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteHsmResponse :: Newtype DeleteHsmResponse _
derive instance repGenericDeleteHsmResponse :: Generic DeleteHsmResponse _
instance showDeleteHsmResponse :: Show DeleteHsmResponse where
  show = genericShow
instance decodeDeleteHsmResponse :: Decode DeleteHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHsmResponse :: Encode DeleteHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLunaClientRequest = DeleteLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  }
derive instance newtypeDeleteLunaClientRequest :: Newtype DeleteLunaClientRequest _
derive instance repGenericDeleteLunaClientRequest :: Generic DeleteLunaClientRequest _
instance showDeleteLunaClientRequest :: Show DeleteLunaClientRequest where
  show = genericShow
instance decodeDeleteLunaClientRequest :: Decode DeleteLunaClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLunaClientRequest :: Encode DeleteLunaClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLunaClientResponse = DeleteLunaClientResponse 
  { "Status" :: (String)
  }
derive instance newtypeDeleteLunaClientResponse :: Newtype DeleteLunaClientResponse _
derive instance repGenericDeleteLunaClientResponse :: Generic DeleteLunaClientResponse _
instance showDeleteLunaClientResponse :: Show DeleteLunaClientResponse where
  show = genericShow
instance decodeDeleteLunaClientResponse :: Decode DeleteLunaClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLunaClientResponse :: Encode DeleteLunaClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DescribeHapg</a> action.</p>
newtype DescribeHapgRequest = DescribeHapgRequest 
  { "HapgArn" :: (HapgArn)
  }
derive instance newtypeDescribeHapgRequest :: Newtype DescribeHapgRequest _
derive instance repGenericDescribeHapgRequest :: Generic DescribeHapgRequest _
instance showDescribeHapgRequest :: Show DescribeHapgRequest where
  show = genericShow
instance decodeDescribeHapgRequest :: Decode DescribeHapgRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHapgRequest :: Encode DescribeHapgRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>DescribeHapg</a> action.</p>
newtype DescribeHapgResponse = DescribeHapgResponse 
  { "HapgArn" :: NullOrUndefined.NullOrUndefined (HapgArn)
  , "HapgSerial" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmsLastActionFailed" :: NullOrUndefined.NullOrUndefined (HsmList)
  , "HsmsPendingDeletion" :: NullOrUndefined.NullOrUndefined (HsmList)
  , "HsmsPendingRegistration" :: NullOrUndefined.NullOrUndefined (HsmList)
  , "Label" :: NullOrUndefined.NullOrUndefined (Label)
  , "LastModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "PartitionSerialList" :: NullOrUndefined.NullOrUndefined (PartitionSerialList)
  , "State" :: NullOrUndefined.NullOrUndefined (CloudHsmObjectState)
  }
derive instance newtypeDescribeHapgResponse :: Newtype DescribeHapgResponse _
derive instance repGenericDescribeHapgResponse :: Generic DescribeHapgResponse _
instance showDescribeHapgResponse :: Show DescribeHapgResponse where
  show = genericShow
instance decodeDescribeHapgResponse :: Decode DescribeHapgResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHapgResponse :: Encode DescribeHapgResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DescribeHsm</a> operation.</p>
newtype DescribeHsmRequest = DescribeHsmRequest 
  { "HsmArn" :: NullOrUndefined.NullOrUndefined (HsmArn)
  , "HsmSerialNumber" :: NullOrUndefined.NullOrUndefined (HsmSerialNumber)
  }
derive instance newtypeDescribeHsmRequest :: Newtype DescribeHsmRequest _
derive instance repGenericDescribeHsmRequest :: Generic DescribeHsmRequest _
instance showDescribeHsmRequest :: Show DescribeHsmRequest where
  show = genericShow
instance decodeDescribeHsmRequest :: Decode DescribeHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHsmRequest :: Encode DescribeHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>DescribeHsm</a> operation.</p>
newtype DescribeHsmResponse = DescribeHsmResponse 
  { "HsmArn" :: NullOrUndefined.NullOrUndefined (HsmArn)
  , "Status" :: NullOrUndefined.NullOrUndefined (HsmStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (AZ)
  , "EniId" :: NullOrUndefined.NullOrUndefined (EniId)
  , "EniIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "SubscriptionType" :: NullOrUndefined.NullOrUndefined (SubscriptionType)
  , "SubscriptionStartDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "SubscriptionEndDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "IamRoleArn" :: NullOrUndefined.NullOrUndefined (IamRoleArn)
  , "SerialNumber" :: NullOrUndefined.NullOrUndefined (HsmSerialNumber)
  , "VendorName" :: NullOrUndefined.NullOrUndefined (String)
  , "HsmType" :: NullOrUndefined.NullOrUndefined (String)
  , "SoftwareVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined.NullOrUndefined (SshKey)
  , "SshKeyLastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "ServerCertUri" :: NullOrUndefined.NullOrUndefined (String)
  , "ServerCertLastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Partitions" :: NullOrUndefined.NullOrUndefined (PartitionList)
  }
derive instance newtypeDescribeHsmResponse :: Newtype DescribeHsmResponse _
derive instance repGenericDescribeHsmResponse :: Generic DescribeHsmResponse _
instance showDescribeHsmResponse :: Show DescribeHsmResponse where
  show = genericShow
instance decodeDescribeHsmResponse :: Decode DescribeHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeHsmResponse :: Encode DescribeHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLunaClientRequest = DescribeLunaClientRequest 
  { "ClientArn" :: NullOrUndefined.NullOrUndefined (ClientArn)
  , "CertificateFingerprint" :: NullOrUndefined.NullOrUndefined (CertificateFingerprint)
  }
derive instance newtypeDescribeLunaClientRequest :: Newtype DescribeLunaClientRequest _
derive instance repGenericDescribeLunaClientRequest :: Generic DescribeLunaClientRequest _
instance showDescribeLunaClientRequest :: Show DescribeLunaClientRequest where
  show = genericShow
instance decodeDescribeLunaClientRequest :: Decode DescribeLunaClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLunaClientRequest :: Encode DescribeLunaClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLunaClientResponse = DescribeLunaClientResponse 
  { "ClientArn" :: NullOrUndefined.NullOrUndefined (ClientArn)
  , "Certificate" :: NullOrUndefined.NullOrUndefined (Certificate)
  , "CertificateFingerprint" :: NullOrUndefined.NullOrUndefined (CertificateFingerprint)
  , "LastModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "Label" :: NullOrUndefined.NullOrUndefined (Label)
  }
derive instance newtypeDescribeLunaClientResponse :: Newtype DescribeLunaClientResponse _
derive instance repGenericDescribeLunaClientResponse :: Generic DescribeLunaClientResponse _
instance showDescribeLunaClientResponse :: Show DescribeLunaClientResponse where
  show = genericShow
instance decodeDescribeLunaClientResponse :: Decode DescribeLunaClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLunaClientResponse :: Encode DescribeLunaClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EniId = EniId String
derive instance newtypeEniId :: Newtype EniId _
derive instance repGenericEniId :: Generic EniId _
instance showEniId :: Show EniId where
  show = genericShow
instance decodeEniId :: Decode EniId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEniId :: Encode EniId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExternalId = ExternalId String
derive instance newtypeExternalId :: Newtype ExternalId _
derive instance repGenericExternalId :: Generic ExternalId _
instance showExternalId :: Show ExternalId where
  show = genericShow
instance decodeExternalId :: Decode ExternalId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalId :: Encode ExternalId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConfigRequest = GetConfigRequest 
  { "ClientArn" :: (ClientArn)
  , "ClientVersion" :: (ClientVersion)
  , "HapgList" :: (HapgList)
  }
derive instance newtypeGetConfigRequest :: Newtype GetConfigRequest _
derive instance repGenericGetConfigRequest :: Generic GetConfigRequest _
instance showGetConfigRequest :: Show GetConfigRequest where
  show = genericShow
instance decodeGetConfigRequest :: Decode GetConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConfigRequest :: Encode GetConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConfigResponse = GetConfigResponse 
  { "ConfigType" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigFile" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigCred" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetConfigResponse :: Newtype GetConfigResponse _
derive instance repGenericGetConfigResponse :: Generic GetConfigResponse _
instance showGetConfigResponse :: Show GetConfigResponse where
  show = genericShow
instance decodeGetConfigResponse :: Decode GetConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConfigResponse :: Encode GetConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HapgArn = HapgArn String
derive instance newtypeHapgArn :: Newtype HapgArn _
derive instance repGenericHapgArn :: Generic HapgArn _
instance showHapgArn :: Show HapgArn where
  show = genericShow
instance decodeHapgArn :: Decode HapgArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHapgArn :: Encode HapgArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HapgList = HapgList (Array HapgArn)
derive instance newtypeHapgList :: Newtype HapgList _
derive instance repGenericHapgList :: Generic HapgList _
instance showHapgList :: Show HapgList where
  show = genericShow
instance decodeHapgList :: Decode HapgList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHapgList :: Encode HapgList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An ARN that identifies an HSM.</p>
newtype HsmArn = HsmArn String
derive instance newtypeHsmArn :: Newtype HsmArn _
derive instance repGenericHsmArn :: Generic HsmArn _
instance showHsmArn :: Show HsmArn where
  show = genericShow
instance decodeHsmArn :: Decode HsmArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmArn :: Encode HsmArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a list of ARNs that identify the HSMs.</p>
newtype HsmList = HsmList (Array HsmArn)
derive instance newtypeHsmList :: Newtype HsmList _
derive instance repGenericHsmList :: Generic HsmList _
instance showHsmList :: Show HsmList where
  show = genericShow
instance decodeHsmList :: Decode HsmList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmList :: Encode HsmList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmSerialNumber = HsmSerialNumber String
derive instance newtypeHsmSerialNumber :: Newtype HsmSerialNumber _
derive instance repGenericHsmSerialNumber :: Generic HsmSerialNumber _
instance showHsmSerialNumber :: Show HsmSerialNumber where
  show = genericShow
instance decodeHsmSerialNumber :: Decode HsmSerialNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmSerialNumber :: Encode HsmSerialNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HsmStatus = HsmStatus String
derive instance newtypeHsmStatus :: Newtype HsmStatus _
derive instance repGenericHsmStatus :: Generic HsmStatus _
instance showHsmStatus :: Show HsmStatus where
  show = genericShow
instance decodeHsmStatus :: Decode HsmStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHsmStatus :: Encode HsmStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IamRoleArn = IamRoleArn String
derive instance newtypeIamRoleArn :: Newtype IamRoleArn _
derive instance repGenericIamRoleArn :: Generic IamRoleArn _
instance showIamRoleArn :: Show IamRoleArn where
  show = genericShow
instance decodeIamRoleArn :: Decode IamRoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIamRoleArn :: Encode IamRoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that one or more of the request parameters are not valid.</p>
newtype InvalidRequestException = InvalidRequestException Types.NoArguments
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Label = Label String
derive instance newtypeLabel :: Newtype Label _
derive instance repGenericLabel :: Generic Label _
instance showLabel :: Show Label where
  show = genericShow
instance decodeLabel :: Decode Label where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabel :: Encode Label where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>ListAvailableZones</a> action.</p>
newtype ListAvailableZonesRequest = ListAvailableZonesRequest Types.NoArguments
derive instance newtypeListAvailableZonesRequest :: Newtype ListAvailableZonesRequest _
derive instance repGenericListAvailableZonesRequest :: Generic ListAvailableZonesRequest _
instance showListAvailableZonesRequest :: Show ListAvailableZonesRequest where
  show = genericShow
instance decodeListAvailableZonesRequest :: Decode ListAvailableZonesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAvailableZonesRequest :: Encode ListAvailableZonesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAvailableZonesResponse = ListAvailableZonesResponse 
  { "AZList" :: NullOrUndefined.NullOrUndefined (AZList)
  }
derive instance newtypeListAvailableZonesResponse :: Newtype ListAvailableZonesResponse _
derive instance repGenericListAvailableZonesResponse :: Generic ListAvailableZonesResponse _
instance showListAvailableZonesResponse :: Show ListAvailableZonesResponse where
  show = genericShow
instance decodeListAvailableZonesResponse :: Decode ListAvailableZonesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAvailableZonesResponse :: Encode ListAvailableZonesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListHapgsRequest = ListHapgsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHapgsRequest :: Newtype ListHapgsRequest _
derive instance repGenericListHapgsRequest :: Generic ListHapgsRequest _
instance showListHapgsRequest :: Show ListHapgsRequest where
  show = genericShow
instance decodeListHapgsRequest :: Decode ListHapgsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHapgsRequest :: Encode ListHapgsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListHapgsResponse = ListHapgsResponse 
  { "HapgList" :: (HapgList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHapgsResponse :: Newtype ListHapgsResponse _
derive instance repGenericListHapgsResponse :: Generic ListHapgsResponse _
instance showListHapgsResponse :: Show ListHapgsResponse where
  show = genericShow
instance decodeListHapgsResponse :: Decode ListHapgsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHapgsResponse :: Encode ListHapgsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListHsmsRequest = ListHsmsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHsmsRequest :: Newtype ListHsmsRequest _
derive instance repGenericListHsmsRequest :: Generic ListHsmsRequest _
instance showListHsmsRequest :: Show ListHsmsRequest where
  show = genericShow
instance decodeListHsmsRequest :: Decode ListHsmsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHsmsRequest :: Encode ListHsmsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <code>ListHsms</code> operation.</p>
newtype ListHsmsResponse = ListHsmsResponse 
  { "HsmList" :: NullOrUndefined.NullOrUndefined (HsmList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListHsmsResponse :: Newtype ListHsmsResponse _
derive instance repGenericListHsmsResponse :: Generic ListHsmsResponse _
instance showListHsmsResponse :: Show ListHsmsResponse where
  show = genericShow
instance decodeListHsmsResponse :: Decode ListHsmsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHsmsResponse :: Encode ListHsmsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLunaClientsRequest = ListLunaClientsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListLunaClientsRequest :: Newtype ListLunaClientsRequest _
derive instance repGenericListLunaClientsRequest :: Generic ListLunaClientsRequest _
instance showListLunaClientsRequest :: Show ListLunaClientsRequest where
  show = genericShow
instance decodeListLunaClientsRequest :: Decode ListLunaClientsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLunaClientsRequest :: Encode ListLunaClientsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLunaClientsResponse = ListLunaClientsResponse 
  { "ClientList" :: (ClientList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListLunaClientsResponse :: Newtype ListLunaClientsResponse _
derive instance repGenericListLunaClientsResponse :: Generic ListLunaClientsResponse _
instance showListLunaClientsResponse :: Show ListLunaClientsResponse where
  show = genericShow
instance decodeListLunaClientsResponse :: Decode ListLunaClientsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLunaClientsResponse :: Encode ListLunaClientsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (String)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "TagList" :: (TagList)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _
derive instance repGenericListTagsForResourceResponse :: Generic ListTagsForResourceResponse _
instance showListTagsForResourceResponse :: Show ListTagsForResourceResponse where
  show = genericShow
instance decodeListTagsForResourceResponse :: Decode ListTagsForResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResponse :: Encode ListTagsForResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyHapgRequest = ModifyHapgRequest 
  { "HapgArn" :: (HapgArn)
  , "Label" :: NullOrUndefined.NullOrUndefined (Label)
  , "PartitionSerialList" :: NullOrUndefined.NullOrUndefined (PartitionSerialList)
  }
derive instance newtypeModifyHapgRequest :: Newtype ModifyHapgRequest _
derive instance repGenericModifyHapgRequest :: Generic ModifyHapgRequest _
instance showModifyHapgRequest :: Show ModifyHapgRequest where
  show = genericShow
instance decodeModifyHapgRequest :: Decode ModifyHapgRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyHapgRequest :: Encode ModifyHapgRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyHapgResponse = ModifyHapgResponse 
  { "HapgArn" :: NullOrUndefined.NullOrUndefined (HapgArn)
  }
derive instance newtypeModifyHapgResponse :: Newtype ModifyHapgResponse _
derive instance repGenericModifyHapgResponse :: Generic ModifyHapgResponse _
instance showModifyHapgResponse :: Show ModifyHapgResponse where
  show = genericShow
instance decodeModifyHapgResponse :: Decode ModifyHapgResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyHapgResponse :: Encode ModifyHapgResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmRequest = ModifyHsmRequest 
  { "HsmArn" :: (HsmArn)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "EniIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "IamRoleArn" :: NullOrUndefined.NullOrUndefined (IamRoleArn)
  , "ExternalId" :: NullOrUndefined.NullOrUndefined (ExternalId)
  , "SyslogIp" :: NullOrUndefined.NullOrUndefined (IpAddress)
  }
derive instance newtypeModifyHsmRequest :: Newtype ModifyHsmRequest _
derive instance repGenericModifyHsmRequest :: Generic ModifyHsmRequest _
instance showModifyHsmRequest :: Show ModifyHsmRequest where
  show = genericShow
instance decodeModifyHsmRequest :: Decode ModifyHsmRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyHsmRequest :: Encode ModifyHsmRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of the <a>ModifyHsm</a> operation.</p>
newtype ModifyHsmResponse = ModifyHsmResponse 
  { "HsmArn" :: NullOrUndefined.NullOrUndefined (HsmArn)
  }
derive instance newtypeModifyHsmResponse :: Newtype ModifyHsmResponse _
derive instance repGenericModifyHsmResponse :: Generic ModifyHsmResponse _
instance showModifyHsmResponse :: Show ModifyHsmResponse where
  show = genericShow
instance decodeModifyHsmResponse :: Decode ModifyHsmResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyHsmResponse :: Encode ModifyHsmResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyLunaClientRequest = ModifyLunaClientRequest 
  { "ClientArn" :: (ClientArn)
  , "Certificate" :: (Certificate)
  }
derive instance newtypeModifyLunaClientRequest :: Newtype ModifyLunaClientRequest _
derive instance repGenericModifyLunaClientRequest :: Generic ModifyLunaClientRequest _
instance showModifyLunaClientRequest :: Show ModifyLunaClientRequest where
  show = genericShow
instance decodeModifyLunaClientRequest :: Decode ModifyLunaClientRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLunaClientRequest :: Encode ModifyLunaClientRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyLunaClientResponse = ModifyLunaClientResponse 
  { "ClientArn" :: NullOrUndefined.NullOrUndefined (ClientArn)
  }
derive instance newtypeModifyLunaClientResponse :: Newtype ModifyLunaClientResponse _
derive instance repGenericModifyLunaClientResponse :: Generic ModifyLunaClientResponse _
instance showModifyLunaClientResponse :: Show ModifyLunaClientResponse where
  show = genericShow
instance decodeModifyLunaClientResponse :: Decode ModifyLunaClientResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLunaClientResponse :: Encode ModifyLunaClientResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionArn = PartitionArn String
derive instance newtypePartitionArn :: Newtype PartitionArn _
derive instance repGenericPartitionArn :: Generic PartitionArn _
instance showPartitionArn :: Show PartitionArn where
  show = genericShow
instance decodePartitionArn :: Decode PartitionArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionArn :: Encode PartitionArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionList = PartitionList (Array PartitionArn)
derive instance newtypePartitionList :: Newtype PartitionList _
derive instance repGenericPartitionList :: Generic PartitionList _
instance showPartitionList :: Show PartitionList where
  show = genericShow
instance decodePartitionList :: Decode PartitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionList :: Encode PartitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionSerial = PartitionSerial String
derive instance newtypePartitionSerial :: Newtype PartitionSerial _
derive instance repGenericPartitionSerial :: Generic PartitionSerial _
instance showPartitionSerial :: Show PartitionSerial where
  show = genericShow
instance decodePartitionSerial :: Decode PartitionSerial where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionSerial :: Encode PartitionSerial where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionSerialList = PartitionSerialList (Array PartitionSerial)
derive instance newtypePartitionSerialList :: Newtype PartitionSerialList _
derive instance repGenericPartitionSerialList :: Generic PartitionSerialList _
instance showPartitionSerialList :: Show PartitionSerialList where
  show = genericShow
instance decodePartitionSerialList :: Decode PartitionSerialList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionSerialList :: Encode PartitionSerialList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceArn" :: (String)
  , "TagKeyList" :: (TagKeyList)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _
derive instance repGenericRemoveTagsFromResourceRequest :: Generic RemoveTagsFromResourceRequest _
instance showRemoveTagsFromResourceRequest :: Show RemoveTagsFromResourceRequest where
  show = genericShow
instance decodeRemoveTagsFromResourceRequest :: Decode RemoveTagsFromResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceRequest :: Encode RemoveTagsFromResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse 
  { "Status" :: (String)
  }
derive instance newtypeRemoveTagsFromResourceResponse :: Newtype RemoveTagsFromResourceResponse _
derive instance repGenericRemoveTagsFromResourceResponse :: Generic RemoveTagsFromResourceResponse _
instance showRemoveTagsFromResourceResponse :: Show RemoveTagsFromResourceResponse where
  show = genericShow
instance decodeRemoveTagsFromResourceResponse :: Decode RemoveTagsFromResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceResponse :: Encode RemoveTagsFromResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SshKey = SshKey String
derive instance newtypeSshKey :: Newtype SshKey _
derive instance repGenericSshKey :: Generic SshKey _
instance showSshKey :: Show SshKey where
  show = genericShow
instance decodeSshKey :: Decode SshKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSshKey :: Encode SshKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _
derive instance repGenericSubnetId :: Generic SubnetId _
instance showSubnetId :: Show SubnetId where
  show = genericShow
instance decodeSubnetId :: Decode SubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetId :: Encode SubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the type of subscription for the HSM.</p> <ul> <li> <p> <b>PRODUCTION</b> - The HSM is being used in a production environment.</p> </li> <li> <p> <b>TRIAL</b> - The HSM is being used in a product trial.</p> </li> </ul>
newtype SubscriptionType = SubscriptionType String
derive instance newtypeSubscriptionType :: Newtype SubscriptionType _
derive instance repGenericSubscriptionType :: Generic SubscriptionType _
instance showSubscriptionType :: Show SubscriptionType where
  show = genericShow
instance decodeSubscriptionType :: Decode SubscriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionType :: Encode SubscriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key-value pair that identifies or specifies metadata about an AWS CloudHSM resource.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
derive instance repGenericVpcId :: Generic VpcId _
instance showVpcId :: Show VpcId where
  show = genericShow
instance decodeVpcId :: Decode VpcId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcId :: Encode VpcId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
