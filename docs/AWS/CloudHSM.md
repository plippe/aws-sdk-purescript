## Module AWS.CloudHSM

<fullname>AWS CloudHSM Service</fullname> <p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: RequestError | eff) AddTagsToResourceResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Adds or overwrites one or more tags for the specified AWS CloudHSM resource.</p> <p>Each tag consists of a key and a value. Tag keys must be unique to each resource.</p>

#### `createHapg`

``` purescript
createHapg :: forall eff. CreateHapgRequest -> Aff (err :: RequestError | eff) CreateHapgResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.</p>

#### `createHsm`

``` purescript
createHsm :: forall eff. CreateHsmRequest -> Aff (err :: RequestError | eff) CreateHsmResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an uninitialized HSM instance.</p> <p>There is an upfront fee charged for each HSM instance that you create with the <code>CreateHsm</code> operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the <a>DeleteHsm</a> operation, go to the <a href="https://console.aws.amazon.com/support/home">AWS Support Center</a>, create a new case, and select <b>Account and Billing Support</b>.</p> <important> <p>It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the <a>DescribeHsm</a> operation. The HSM is ready to be initialized when the status changes to <code>RUNNING</code>.</p> </important>

#### `createLunaClient`

``` purescript
createLunaClient :: forall eff. CreateLunaClientRequest -> Aff (err :: RequestError | eff) CreateLunaClientResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Creates an HSM client.</p>

#### `deleteHapg`

``` purescript
deleteHapg :: forall eff. DeleteHapgRequest -> Aff (err :: RequestError | eff) DeleteHapgResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a high-availability partition group.</p>

#### `deleteHsm`

``` purescript
deleteHsm :: forall eff. DeleteHsmRequest -> Aff (err :: RequestError | eff) DeleteHsmResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.</p>

#### `deleteLunaClient`

``` purescript
deleteLunaClient :: forall eff. DeleteLunaClientRequest -> Aff (err :: RequestError | eff) DeleteLunaClientResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Deletes a client.</p>

#### `describeHapg`

``` purescript
describeHapg :: forall eff. DescribeHapgRequest -> Aff (err :: RequestError | eff) DescribeHapgResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about a high-availability partition group.</p>

#### `describeHsm`

``` purescript
describeHsm :: forall eff. DescribeHsmRequest -> Aff (err :: RequestError | eff) DescribeHsmResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM. You can identify the HSM by its ARN or its serial number.</p>

#### `describeLunaClient`

``` purescript
describeLunaClient :: forall eff. DescribeLunaClientRequest -> Aff (err :: RequestError | eff) DescribeLunaClientResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves information about an HSM client.</p>

#### `getConfig`

``` purescript
getConfig :: forall eff. GetConfigRequest -> Aff (err :: RequestError | eff) GetConfigResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.</p>

#### `listAvailableZones`

``` purescript
listAvailableZones :: forall eff. ListAvailableZonesRequest -> Aff (err :: RequestError | eff) ListAvailableZonesResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the Availability Zones that have available AWS CloudHSM capacity.</p>

#### `listHapgs`

``` purescript
listHapgs :: forall eff. ListHapgsRequest -> Aff (err :: RequestError | eff) ListHapgsResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists the high-availability partition groups for the account.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHapgs</code> to retrieve the next set of items.</p>

#### `listHsms`

``` purescript
listHsms :: forall eff. ListHsmsRequest -> Aff (err :: RequestError | eff) ListHsmsResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Retrieves the identifiers of all of the HSMs provisioned for the current customer.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListHsms</code> to retrieve the next set of items.</p>

#### `listLunaClients`

``` purescript
listLunaClients :: forall eff. ListLunaClientsRequest -> Aff (err :: RequestError | eff) ListLunaClientsResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Lists all of the clients.</p> <p>This operation supports pagination with the use of the <code>NextToken</code> member. If more results are available, the <code>NextToken</code> member of the response contains a token that you pass in the next call to <code>ListLunaClients</code> to retrieve the next set of items.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Returns a list of all tags for the specified AWS CloudHSM resource.</p>

#### `modifyHapg`

``` purescript
modifyHapg :: forall eff. ModifyHapgRequest -> Aff (err :: RequestError | eff) ModifyHapgResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an existing high-availability partition group.</p>

#### `modifyHsm`

``` purescript
modifyHsm :: forall eff. ModifyHsmRequest -> Aff (err :: RequestError | eff) ModifyHsmResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies an HSM.</p> <important> <p>This operation can result in the HSM being offline for up to 15 minutes while the AWS CloudHSM service is reconfigured. If you are modifying a production HSM, you should ensure that your AWS CloudHSM service is configured for high availability, and consider executing this operation during a maintenance window.</p> </important>

#### `modifyLunaClient`

``` purescript
modifyLunaClient :: forall eff. ModifyLunaClientRequest -> Aff (err :: RequestError | eff) ModifyLunaClientResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Modifies the certificate used by the client.</p> <p>This action can potentially start a workflow to install the new certificate on the client's HSMs.</p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: RequestError | eff) RemoveTagsFromResourceResponse
```

<p>This is documentation for <b>AWS CloudHSM Classic</b>. For more information, see <a href="http://aws.amazon.com/cloudhsm/faqs-classic/">AWS CloudHSM Classic FAQs</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/classic/userguide/">AWS CloudHSM Classic User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/classic/APIReference/">AWS CloudHSM Classic API Reference</a>.</p> <p> <b>For information about the current version of AWS CloudHSM</b>, see <a href="http://aws.amazon.com/cloudhsm/">AWS CloudHSM</a>, the <a href="http://docs.aws.amazon.com/cloudhsm/latest/userguide/">AWS CloudHSM User Guide</a>, and the <a href="http://docs.aws.amazon.com/cloudhsm/latest/APIReference/">AWS CloudHSM API Reference</a>.</p> <p>Removes one or more tags from the specified AWS CloudHSM resource.</p> <p>To remove a tag, specify only the tag key to remove (not the value). To overwrite the value for an existing tag, use <a>AddTagsToResource</a>.</p>

#### `AZ`

``` purescript
newtype AZ
  = AZ String
```

#### `AZList`

``` purescript
newtype AZList
  = AZList (Array AZ)
```

#### `AddTagsToResourceRequest`

``` purescript
newtype AddTagsToResourceRequest
  = AddTagsToResourceRequest { "ResourceArn" :: String, "TagList" :: TagList }
```

#### `AddTagsToResourceResponse`

``` purescript
newtype AddTagsToResourceResponse
  = AddTagsToResourceResponse { "Status" :: String }
```

#### `Certificate`

``` purescript
newtype Certificate
  = Certificate String
```

#### `CertificateFingerprint`

``` purescript
newtype CertificateFingerprint
  = CertificateFingerprint String
```

#### `ClientArn`

``` purescript
newtype ClientArn
  = ClientArn String
```

#### `ClientLabel`

``` purescript
newtype ClientLabel
  = ClientLabel String
```

#### `ClientList`

``` purescript
newtype ClientList
  = ClientList (Array ClientArn)
```

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

#### `ClientVersion`

``` purescript
newtype ClientVersion
  = ClientVersion String
```

#### `CloudHsmInternalException`

``` purescript
newtype CloudHsmInternalException
  = CloudHsmInternalException {  }
```

<p>Indicates that an internal error occurred.</p>

#### `CloudHsmObjectState`

``` purescript
newtype CloudHsmObjectState
  = CloudHsmObjectState String
```

#### `CloudHsmServiceException`

``` purescript
newtype CloudHsmServiceException
  = CloudHsmServiceException { "Message'" :: NullOrUndefined (String), "Retryable'" :: NullOrUndefined (Boolean) }
```

<p>Indicates that an exception occurred in the AWS CloudHSM service.</p>

#### `CreateHapgRequest`

``` purescript
newtype CreateHapgRequest
  = CreateHapgRequest { "Label" :: Label }
```

<p>Contains the inputs for the <a>CreateHapgRequest</a> action.</p>

#### `CreateHapgResponse`

``` purescript
newtype CreateHapgResponse
  = CreateHapgResponse { "HapgArn" :: NullOrUndefined (HapgArn) }
```

<p>Contains the output of the <a>CreateHAPartitionGroup</a> action.</p>

#### `CreateHsmRequest`

``` purescript
newtype CreateHsmRequest
  = CreateHsmRequest { "SubnetId" :: SubnetId, "SshKey" :: SshKey, "EniIp" :: NullOrUndefined (IpAddress), "IamRoleArn" :: IamRoleArn, "ExternalId" :: NullOrUndefined (ExternalId), "SubscriptionType" :: SubscriptionType, "ClientToken" :: NullOrUndefined (ClientToken), "SyslogIp" :: NullOrUndefined (IpAddress) }
```

<p>Contains the inputs for the <code>CreateHsm</code> operation.</p>

#### `CreateHsmResponse`

``` purescript
newtype CreateHsmResponse
  = CreateHsmResponse { "HsmArn" :: NullOrUndefined (HsmArn) }
```

<p>Contains the output of the <code>CreateHsm</code> operation.</p>

#### `CreateLunaClientRequest`

``` purescript
newtype CreateLunaClientRequest
  = CreateLunaClientRequest { "Label" :: NullOrUndefined (ClientLabel), "Certificate" :: Certificate }
```

<p>Contains the inputs for the <a>CreateLunaClient</a> action.</p>

#### `CreateLunaClientResponse`

``` purescript
newtype CreateLunaClientResponse
  = CreateLunaClientResponse { "ClientArn" :: NullOrUndefined (ClientArn) }
```

<p>Contains the output of the <a>CreateLunaClient</a> action.</p>

#### `DeleteHapgRequest`

``` purescript
newtype DeleteHapgRequest
  = DeleteHapgRequest { "HapgArn" :: HapgArn }
```

<p>Contains the inputs for the <a>DeleteHapg</a> action.</p>

#### `DeleteHapgResponse`

``` purescript
newtype DeleteHapgResponse
  = DeleteHapgResponse { "Status" :: String }
```

<p>Contains the output of the <a>DeleteHapg</a> action.</p>

#### `DeleteHsmRequest`

``` purescript
newtype DeleteHsmRequest
  = DeleteHsmRequest { "HsmArn" :: HsmArn }
```

<p>Contains the inputs for the <a>DeleteHsm</a> operation.</p>

#### `DeleteHsmResponse`

``` purescript
newtype DeleteHsmResponse
  = DeleteHsmResponse { "Status" :: String }
```

<p>Contains the output of the <a>DeleteHsm</a> operation.</p>

#### `DeleteLunaClientRequest`

``` purescript
newtype DeleteLunaClientRequest
  = DeleteLunaClientRequest { "ClientArn" :: ClientArn }
```

#### `DeleteLunaClientResponse`

``` purescript
newtype DeleteLunaClientResponse
  = DeleteLunaClientResponse { "Status" :: String }
```

#### `DescribeHapgRequest`

``` purescript
newtype DescribeHapgRequest
  = DescribeHapgRequest { "HapgArn" :: HapgArn }
```

<p>Contains the inputs for the <a>DescribeHapg</a> action.</p>

#### `DescribeHapgResponse`

``` purescript
newtype DescribeHapgResponse
  = DescribeHapgResponse { "HapgArn" :: NullOrUndefined (HapgArn), "HapgSerial" :: NullOrUndefined (String), "HsmsLastActionFailed" :: NullOrUndefined (HsmList), "HsmsPendingDeletion" :: NullOrUndefined (HsmList), "HsmsPendingRegistration" :: NullOrUndefined (HsmList), "Label" :: NullOrUndefined (Label), "LastModifiedTimestamp" :: NullOrUndefined (Number), "PartitionSerialList" :: NullOrUndefined (PartitionSerialList), "State" :: NullOrUndefined (CloudHsmObjectState) }
```

<p>Contains the output of the <a>DescribeHapg</a> action.</p>

#### `DescribeHsmRequest`

``` purescript
newtype DescribeHsmRequest
  = DescribeHsmRequest { "HsmArn" :: NullOrUndefined (HsmArn), "HsmSerialNumber" :: NullOrUndefined (HsmSerialNumber) }
```

<p>Contains the inputs for the <a>DescribeHsm</a> operation.</p>

#### `DescribeHsmResponse`

``` purescript
newtype DescribeHsmResponse
  = DescribeHsmResponse { "HsmArn" :: NullOrUndefined (HsmArn), "Status" :: NullOrUndefined (HsmStatus), "StatusDetails" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (AZ), "EniId" :: NullOrUndefined (EniId), "EniIp" :: NullOrUndefined (IpAddress), "SubscriptionType" :: NullOrUndefined (SubscriptionType), "SubscriptionStartDate" :: NullOrUndefined (Number), "SubscriptionEndDate" :: NullOrUndefined (Number), "VpcId" :: NullOrUndefined (VpcId), "SubnetId" :: NullOrUndefined (SubnetId), "IamRoleArn" :: NullOrUndefined (IamRoleArn), "SerialNumber" :: NullOrUndefined (HsmSerialNumber), "VendorName" :: NullOrUndefined (String), "HsmType" :: NullOrUndefined (String), "SoftwareVersion" :: NullOrUndefined (String), "SshPublicKey" :: NullOrUndefined (SshKey), "SshKeyLastUpdated" :: NullOrUndefined (Number), "ServerCertUri" :: NullOrUndefined (String), "ServerCertLastUpdated" :: NullOrUndefined (Number), "Partitions" :: NullOrUndefined (PartitionList) }
```

<p>Contains the output of the <a>DescribeHsm</a> operation.</p>

#### `DescribeLunaClientRequest`

``` purescript
newtype DescribeLunaClientRequest
  = DescribeLunaClientRequest { "ClientArn" :: NullOrUndefined (ClientArn), "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint) }
```

#### `DescribeLunaClientResponse`

``` purescript
newtype DescribeLunaClientResponse
  = DescribeLunaClientResponse { "ClientArn" :: NullOrUndefined (ClientArn), "Certificate" :: NullOrUndefined (Certificate), "CertificateFingerprint" :: NullOrUndefined (CertificateFingerprint), "LastModifiedTimestamp" :: NullOrUndefined (Number), "Label" :: NullOrUndefined (Label) }
```

#### `EniId`

``` purescript
newtype EniId
  = EniId String
```

#### `ExternalId`

``` purescript
newtype ExternalId
  = ExternalId String
```

#### `GetConfigRequest`

``` purescript
newtype GetConfigRequest
  = GetConfigRequest { "ClientArn" :: ClientArn, "ClientVersion" :: ClientVersion, "HapgList" :: HapgList }
```

#### `GetConfigResponse`

``` purescript
newtype GetConfigResponse
  = GetConfigResponse { "ConfigType" :: NullOrUndefined (String), "ConfigFile" :: NullOrUndefined (String), "ConfigCred" :: NullOrUndefined (String) }
```

#### `HapgArn`

``` purescript
newtype HapgArn
  = HapgArn String
```

#### `HapgList`

``` purescript
newtype HapgList
  = HapgList (Array HapgArn)
```

#### `HsmArn`

``` purescript
newtype HsmArn
  = HsmArn String
```

<p>An ARN that identifies an HSM.</p>

#### `HsmList`

``` purescript
newtype HsmList
  = HsmList (Array HsmArn)
```

<p>Contains a list of ARNs that identify the HSMs.</p>

#### `HsmSerialNumber`

``` purescript
newtype HsmSerialNumber
  = HsmSerialNumber String
```

#### `HsmStatus`

``` purescript
newtype HsmStatus
  = HsmStatus String
```

#### `IamRoleArn`

``` purescript
newtype IamRoleArn
  = IamRoleArn String
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException {  }
```

<p>Indicates that one or more of the request parameters are not valid.</p>

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

#### `Label`

``` purescript
newtype Label
  = Label String
```

#### `ListAvailableZonesRequest`

``` purescript
newtype ListAvailableZonesRequest
  = ListAvailableZonesRequest {  }
```

<p>Contains the inputs for the <a>ListAvailableZones</a> action.</p>

#### `ListAvailableZonesResponse`

``` purescript
newtype ListAvailableZonesResponse
  = ListAvailableZonesResponse { "AZList" :: NullOrUndefined (AZList) }
```

#### `ListHapgsRequest`

``` purescript
newtype ListHapgsRequest
  = ListHapgsRequest { "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListHapgsResponse`

``` purescript
newtype ListHapgsResponse
  = ListHapgsResponse { "HapgList" :: HapgList, "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListHsmsRequest`

``` purescript
newtype ListHsmsRequest
  = ListHsmsRequest { "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListHsmsResponse`

``` purescript
newtype ListHsmsResponse
  = ListHsmsResponse { "HsmList" :: NullOrUndefined (HsmList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

<p>Contains the output of the <code>ListHsms</code> operation.</p>

#### `ListLunaClientsRequest`

``` purescript
newtype ListLunaClientsRequest
  = ListLunaClientsRequest { "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListLunaClientsResponse`

``` purescript
newtype ListLunaClientsResponse
  = ListLunaClientsResponse { "ClientList" :: ClientList, "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceArn" :: String }
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "TagList" :: TagList }
```

#### `ModifyHapgRequest`

``` purescript
newtype ModifyHapgRequest
  = ModifyHapgRequest { "HapgArn" :: HapgArn, "Label" :: NullOrUndefined (Label), "PartitionSerialList" :: NullOrUndefined (PartitionSerialList) }
```

#### `ModifyHapgResponse`

``` purescript
newtype ModifyHapgResponse
  = ModifyHapgResponse { "HapgArn" :: NullOrUndefined (HapgArn) }
```

#### `ModifyHsmRequest`

``` purescript
newtype ModifyHsmRequest
  = ModifyHsmRequest { "HsmArn" :: HsmArn, "SubnetId" :: NullOrUndefined (SubnetId), "EniIp" :: NullOrUndefined (IpAddress), "IamRoleArn" :: NullOrUndefined (IamRoleArn), "ExternalId" :: NullOrUndefined (ExternalId), "SyslogIp" :: NullOrUndefined (IpAddress) }
```

<p>Contains the inputs for the <a>ModifyHsm</a> operation.</p>

#### `ModifyHsmResponse`

``` purescript
newtype ModifyHsmResponse
  = ModifyHsmResponse { "HsmArn" :: NullOrUndefined (HsmArn) }
```

<p>Contains the output of the <a>ModifyHsm</a> operation.</p>

#### `ModifyLunaClientRequest`

``` purescript
newtype ModifyLunaClientRequest
  = ModifyLunaClientRequest { "ClientArn" :: ClientArn, "Certificate" :: Certificate }
```

#### `ModifyLunaClientResponse`

``` purescript
newtype ModifyLunaClientResponse
  = ModifyLunaClientResponse { "ClientArn" :: NullOrUndefined (ClientArn) }
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `PartitionArn`

``` purescript
newtype PartitionArn
  = PartitionArn String
```

#### `PartitionList`

``` purescript
newtype PartitionList
  = PartitionList (Array PartitionArn)
```

#### `PartitionSerial`

``` purescript
newtype PartitionSerial
  = PartitionSerial String
```

#### `PartitionSerialList`

``` purescript
newtype PartitionSerialList
  = PartitionSerialList (Array PartitionSerial)
```

#### `RemoveTagsFromResourceRequest`

``` purescript
newtype RemoveTagsFromResourceRequest
  = RemoveTagsFromResourceRequest { "ResourceArn" :: String, "TagKeyList" :: TagKeyList }
```

#### `RemoveTagsFromResourceResponse`

``` purescript
newtype RemoveTagsFromResourceResponse
  = RemoveTagsFromResourceResponse { "Status" :: String }
```

#### `SshKey`

``` purescript
newtype SshKey
  = SshKey String
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

#### `SubscriptionType`

``` purescript
newtype SubscriptionType
  = SubscriptionType String
```

<p>Specifies the type of subscription for the HSM.</p> <ul> <li> <p> <b>PRODUCTION</b> - The HSM is being used in a production environment.</p> </li> <li> <p> <b>TRIAL</b> - The HSM is being used in a product trial.</p> </li> </ul>

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>A key-value pair that identifies or specifies metadata about an AWS CloudHSM resource.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `VpcId`

``` purescript
newtype VpcId
  = VpcId String
```

