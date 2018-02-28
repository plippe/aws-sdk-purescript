

-- | Amazon Server Migration Service automates the process of migrating servers to EC2.
module AWS.SMS where

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

serviceName = "SMS" :: String


-- | The CreateReplicationJob API is used to create a ReplicationJob to replicate a server on AWS. Call this API to first create a ReplicationJob, which will then schedule periodic ReplicationRuns to replicate your server to AWS. Each ReplicationRun will result in the creation of an AWS AMI.
createReplicationJob :: forall eff. CreateReplicationJobRequest -> Aff (exception :: EXCEPTION | eff) CreateReplicationJobResponse
createReplicationJob = Request.request serviceName "createReplicationJob" 


-- | The DeleteReplicationJob API is used to delete a ReplicationJob, resulting in no further ReplicationRuns. This will delete the contents of the S3 bucket used to store SMS artifacts, but will not delete any AMIs created by the SMS service.
deleteReplicationJob :: forall eff. DeleteReplicationJobRequest -> Aff (exception :: EXCEPTION | eff) DeleteReplicationJobResponse
deleteReplicationJob = Request.request serviceName "deleteReplicationJob" 


-- | The DeleteServerCatalog API clears all servers from your server catalog. This means that these servers will no longer be accessible to the Server Migration Service.
deleteServerCatalog :: forall eff. DeleteServerCatalogRequest -> Aff (exception :: EXCEPTION | eff) DeleteServerCatalogResponse
deleteServerCatalog = Request.request serviceName "deleteServerCatalog" 


-- | The DisassociateConnector API will disassociate a connector from the Server Migration Service, rendering it unavailable to support replication jobs.
disassociateConnector :: forall eff. DisassociateConnectorRequest -> Aff (exception :: EXCEPTION | eff) DisassociateConnectorResponse
disassociateConnector = Request.request serviceName "disassociateConnector" 


-- | The GetConnectors API returns a list of connectors that are registered with the Server Migration Service.
getConnectors :: forall eff. GetConnectorsRequest -> Aff (exception :: EXCEPTION | eff) GetConnectorsResponse
getConnectors = Request.request serviceName "getConnectors" 


-- | The GetReplicationJobs API will return all of your ReplicationJobs and their details. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationJobs.
getReplicationJobs :: forall eff. GetReplicationJobsRequest -> Aff (exception :: EXCEPTION | eff) GetReplicationJobsResponse
getReplicationJobs = Request.request serviceName "getReplicationJobs" 


-- | The GetReplicationRuns API will return all ReplicationRuns for a given ReplicationJob. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationRuns for a ReplicationJob.
getReplicationRuns :: forall eff. GetReplicationRunsRequest -> Aff (exception :: EXCEPTION | eff) GetReplicationRunsResponse
getReplicationRuns = Request.request serviceName "getReplicationRuns" 


-- | The GetServers API returns a list of all servers in your server catalog. For this call to succeed, you must previously have called ImportServerCatalog.
getServers :: forall eff. GetServersRequest -> Aff (exception :: EXCEPTION | eff) GetServersResponse
getServers = Request.request serviceName "getServers" 


-- | The ImportServerCatalog API is used to gather the complete list of on-premises servers on your premises. This API call requires connectors to be installed and monitoring all servers you would like imported. This API call returns immediately, but may take some time to retrieve all of the servers.
importServerCatalog :: forall eff. ImportServerCatalogRequest -> Aff (exception :: EXCEPTION | eff) ImportServerCatalogResponse
importServerCatalog = Request.request serviceName "importServerCatalog" 


-- | The StartOnDemandReplicationRun API is used to start a ReplicationRun on demand (in addition to those that are scheduled based on your frequency). This ReplicationRun will start immediately. StartOnDemandReplicationRun is subject to limits on how many on demand ReplicationRuns you may call per 24-hour period.
startOnDemandReplicationRun :: forall eff. StartOnDemandReplicationRunRequest -> Aff (exception :: EXCEPTION | eff) StartOnDemandReplicationRunResponse
startOnDemandReplicationRun = Request.request serviceName "startOnDemandReplicationRun" 


-- | The UpdateReplicationJob API is used to change the settings of your existing ReplicationJob created using CreateReplicationJob. Calling this API will affect the next scheduled ReplicationRun.
updateReplicationJob :: forall eff. UpdateReplicationJobRequest -> Aff (exception :: EXCEPTION | eff) UpdateReplicationJobResponse
updateReplicationJob = Request.request serviceName "updateReplicationJob" 


-- | The AMI id for the image resulting from a Replication Run.
newtype AmiId = AmiId String
derive instance newtypeAmiId :: Newtype AmiId _
derive instance repGenericAmiId :: Generic AmiId _
instance showAmiId :: Show AmiId where
  show = genericShow
instance decodeAmiId :: Decode AmiId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAmiId :: Encode AmiId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a Connector
newtype Connector = Connector 
  { "ConnectorId'" :: NullOrUndefined.NullOrUndefined (ConnectorId)
  , "Version'" :: NullOrUndefined.NullOrUndefined (ConnectorVersion)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ConnectorStatus)
  , "CapabilityList'" :: NullOrUndefined.NullOrUndefined (ConnectorCapabilityList)
  , "VmManagerName'" :: NullOrUndefined.NullOrUndefined (VmManagerName)
  , "VmManagerType'" :: NullOrUndefined.NullOrUndefined (VmManagerType)
  , "VmManagerId'" :: NullOrUndefined.NullOrUndefined (VmManagerId)
  , "IpAddress'" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "MacAddress'" :: NullOrUndefined.NullOrUndefined (MacAddress)
  , "AssociatedOn'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeConnector :: Newtype Connector _
derive instance repGenericConnector :: Generic Connector _
instance showConnector :: Show Connector where
  show = genericShow
instance decodeConnector :: Decode Connector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnector :: Encode Connector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Capabilities for a Connector
newtype ConnectorCapability = ConnectorCapability String
derive instance newtypeConnectorCapability :: Newtype ConnectorCapability _
derive instance repGenericConnectorCapability :: Generic ConnectorCapability _
instance showConnectorCapability :: Show ConnectorCapability where
  show = genericShow
instance decodeConnectorCapability :: Decode ConnectorCapability where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorCapability :: Encode ConnectorCapability where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of Connector Capabilities
newtype ConnectorCapabilityList = ConnectorCapabilityList (Array ConnectorCapability)
derive instance newtypeConnectorCapabilityList :: Newtype ConnectorCapabilityList _
derive instance repGenericConnectorCapabilityList :: Generic ConnectorCapabilityList _
instance showConnectorCapabilityList :: Show ConnectorCapabilityList where
  show = genericShow
instance decodeConnectorCapabilityList :: Decode ConnectorCapabilityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorCapabilityList :: Encode ConnectorCapabilityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Unique Identifier for Connector
newtype ConnectorId = ConnectorId String
derive instance newtypeConnectorId :: Newtype ConnectorId _
derive instance repGenericConnectorId :: Generic ConnectorId _
instance showConnectorId :: Show ConnectorId where
  show = genericShow
instance decodeConnectorId :: Decode ConnectorId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorId :: Encode ConnectorId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of connectors
newtype ConnectorList = ConnectorList (Array Connector)
derive instance newtypeConnectorList :: Newtype ConnectorList _
derive instance repGenericConnectorList :: Generic ConnectorList _
instance showConnectorList :: Show ConnectorList where
  show = genericShow
instance decodeConnectorList :: Decode ConnectorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorList :: Encode ConnectorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Status of on-premise Connector
newtype ConnectorStatus = ConnectorStatus String
derive instance newtypeConnectorStatus :: Newtype ConnectorStatus _
derive instance repGenericConnectorStatus :: Generic ConnectorStatus _
instance showConnectorStatus :: Show ConnectorStatus where
  show = genericShow
instance decodeConnectorStatus :: Decode ConnectorStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorStatus :: Encode ConnectorStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Connector version string
newtype ConnectorVersion = ConnectorVersion String
derive instance newtypeConnectorVersion :: Newtype ConnectorVersion _
derive instance repGenericConnectorVersion :: Generic ConnectorVersion _
instance showConnectorVersion :: Show ConnectorVersion where
  show = genericShow
instance decodeConnectorVersion :: Decode ConnectorVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectorVersion :: Encode ConnectorVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateReplicationJobRequest = CreateReplicationJobRequest 
  { "ServerId'" :: (ServerId)
  , "SeedReplicationTime'" :: (Number)
  , "Frequency'" :: (Frequency)
  , "LicenseType'" :: NullOrUndefined.NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined.NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeCreateReplicationJobRequest :: Newtype CreateReplicationJobRequest _
derive instance repGenericCreateReplicationJobRequest :: Generic CreateReplicationJobRequest _
instance showCreateReplicationJobRequest :: Show CreateReplicationJobRequest where
  show = genericShow
instance decodeCreateReplicationJobRequest :: Decode CreateReplicationJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReplicationJobRequest :: Encode CreateReplicationJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateReplicationJobResponse = CreateReplicationJobResponse 
  { "ReplicationJobId'" :: NullOrUndefined.NullOrUndefined (ReplicationJobId)
  }
derive instance newtypeCreateReplicationJobResponse :: Newtype CreateReplicationJobResponse _
derive instance repGenericCreateReplicationJobResponse :: Generic CreateReplicationJobResponse _
instance showCreateReplicationJobResponse :: Show CreateReplicationJobResponse where
  show = genericShow
instance decodeCreateReplicationJobResponse :: Decode CreateReplicationJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReplicationJobResponse :: Encode CreateReplicationJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteReplicationJobRequest = DeleteReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  }
derive instance newtypeDeleteReplicationJobRequest :: Newtype DeleteReplicationJobRequest _
derive instance repGenericDeleteReplicationJobRequest :: Generic DeleteReplicationJobRequest _
instance showDeleteReplicationJobRequest :: Show DeleteReplicationJobRequest where
  show = genericShow
instance decodeDeleteReplicationJobRequest :: Decode DeleteReplicationJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReplicationJobRequest :: Encode DeleteReplicationJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse Types.NoArguments
derive instance newtypeDeleteReplicationJobResponse :: Newtype DeleteReplicationJobResponse _
derive instance repGenericDeleteReplicationJobResponse :: Generic DeleteReplicationJobResponse _
instance showDeleteReplicationJobResponse :: Show DeleteReplicationJobResponse where
  show = genericShow
instance decodeDeleteReplicationJobResponse :: Decode DeleteReplicationJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReplicationJobResponse :: Encode DeleteReplicationJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServerCatalogRequest = DeleteServerCatalogRequest Types.NoArguments
derive instance newtypeDeleteServerCatalogRequest :: Newtype DeleteServerCatalogRequest _
derive instance repGenericDeleteServerCatalogRequest :: Generic DeleteServerCatalogRequest _
instance showDeleteServerCatalogRequest :: Show DeleteServerCatalogRequest where
  show = genericShow
instance decodeDeleteServerCatalogRequest :: Decode DeleteServerCatalogRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServerCatalogRequest :: Encode DeleteServerCatalogRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse Types.NoArguments
derive instance newtypeDeleteServerCatalogResponse :: Newtype DeleteServerCatalogResponse _
derive instance repGenericDeleteServerCatalogResponse :: Generic DeleteServerCatalogResponse _
instance showDeleteServerCatalogResponse :: Show DeleteServerCatalogResponse where
  show = genericShow
instance decodeDeleteServerCatalogResponse :: Decode DeleteServerCatalogResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServerCatalogResponse :: Encode DeleteServerCatalogResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The description for a Replication Job/Run.
newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateConnectorRequest = DisassociateConnectorRequest 
  { "ConnectorId'" :: (ConnectorId)
  }
derive instance newtypeDisassociateConnectorRequest :: Newtype DisassociateConnectorRequest _
derive instance repGenericDisassociateConnectorRequest :: Generic DisassociateConnectorRequest _
instance showDisassociateConnectorRequest :: Show DisassociateConnectorRequest where
  show = genericShow
instance decodeDisassociateConnectorRequest :: Decode DisassociateConnectorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateConnectorRequest :: Encode DisassociateConnectorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateConnectorResponse = DisassociateConnectorResponse Types.NoArguments
derive instance newtypeDisassociateConnectorResponse :: Newtype DisassociateConnectorResponse _
derive instance repGenericDisassociateConnectorResponse :: Generic DisassociateConnectorResponse _
instance showDisassociateConnectorResponse :: Show DisassociateConnectorResponse where
  show = genericShow
instance decodeDisassociateConnectorResponse :: Decode DisassociateConnectorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateConnectorResponse :: Encode DisassociateConnectorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Error Message string
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Interval between Replication Runs. This value is specified in hours, and represents the time between consecutive Replication Runs.
newtype Frequency = Frequency Int
derive instance newtypeFrequency :: Newtype Frequency _
derive instance repGenericFrequency :: Generic Frequency _
instance showFrequency :: Show Frequency where
  show = genericShow
instance decodeFrequency :: Decode Frequency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFrequency :: Encode Frequency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectorsRequest = GetConnectorsRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetConnectorsRequest :: Newtype GetConnectorsRequest _
derive instance repGenericGetConnectorsRequest :: Generic GetConnectorsRequest _
instance showGetConnectorsRequest :: Show GetConnectorsRequest where
  show = genericShow
instance decodeGetConnectorsRequest :: Decode GetConnectorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectorsRequest :: Encode GetConnectorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectorsResponse = GetConnectorsResponse 
  { "ConnectorList'" :: NullOrUndefined.NullOrUndefined (ConnectorList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetConnectorsResponse :: Newtype GetConnectorsResponse _
derive instance repGenericGetConnectorsResponse :: Generic GetConnectorsResponse _
instance showGetConnectorsResponse :: Show GetConnectorsResponse where
  show = genericShow
instance decodeGetConnectorsResponse :: Decode GetConnectorsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectorsResponse :: Encode GetConnectorsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReplicationJobsRequest = GetReplicationJobsRequest 
  { "ReplicationJobId'" :: NullOrUndefined.NullOrUndefined (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetReplicationJobsRequest :: Newtype GetReplicationJobsRequest _
derive instance repGenericGetReplicationJobsRequest :: Generic GetReplicationJobsRequest _
instance showGetReplicationJobsRequest :: Show GetReplicationJobsRequest where
  show = genericShow
instance decodeGetReplicationJobsRequest :: Decode GetReplicationJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReplicationJobsRequest :: Encode GetReplicationJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReplicationJobsResponse = GetReplicationJobsResponse 
  { "ReplicationJobList'" :: NullOrUndefined.NullOrUndefined (ReplicationJobList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetReplicationJobsResponse :: Newtype GetReplicationJobsResponse _
derive instance repGenericGetReplicationJobsResponse :: Generic GetReplicationJobsResponse _
instance showGetReplicationJobsResponse :: Show GetReplicationJobsResponse where
  show = genericShow
instance decodeGetReplicationJobsResponse :: Decode GetReplicationJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReplicationJobsResponse :: Encode GetReplicationJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReplicationRunsRequest = GetReplicationRunsRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetReplicationRunsRequest :: Newtype GetReplicationRunsRequest _
derive instance repGenericGetReplicationRunsRequest :: Generic GetReplicationRunsRequest _
instance showGetReplicationRunsRequest :: Show GetReplicationRunsRequest where
  show = genericShow
instance decodeGetReplicationRunsRequest :: Decode GetReplicationRunsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReplicationRunsRequest :: Encode GetReplicationRunsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReplicationRunsResponse = GetReplicationRunsResponse 
  { "ReplicationJob'" :: NullOrUndefined.NullOrUndefined (ReplicationJob)
  , "ReplicationRunList'" :: NullOrUndefined.NullOrUndefined (ReplicationRunList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetReplicationRunsResponse :: Newtype GetReplicationRunsResponse _
derive instance repGenericGetReplicationRunsResponse :: Generic GetReplicationRunsResponse _
instance showGetReplicationRunsResponse :: Show GetReplicationRunsResponse where
  show = genericShow
instance decodeGetReplicationRunsResponse :: Decode GetReplicationRunsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReplicationRunsResponse :: Encode GetReplicationRunsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServersRequest = GetServersRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetServersRequest :: Newtype GetServersRequest _
derive instance repGenericGetServersRequest :: Generic GetServersRequest _
instance showGetServersRequest :: Show GetServersRequest where
  show = genericShow
instance decodeGetServersRequest :: Decode GetServersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServersRequest :: Encode GetServersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServersResponse = GetServersResponse 
  { "LastModifiedOn'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ServerCatalogStatus'" :: NullOrUndefined.NullOrUndefined (ServerCatalogStatus)
  , "ServerList'" :: NullOrUndefined.NullOrUndefined (ServerList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetServersResponse :: Newtype GetServersResponse _
derive instance repGenericGetServersResponse :: Generic GetServersResponse _
instance showGetServersResponse :: Show GetServersResponse where
  show = genericShow
instance decodeGetServersResponse :: Decode GetServersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServersResponse :: Encode GetServersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportServerCatalogRequest = ImportServerCatalogRequest Types.NoArguments
derive instance newtypeImportServerCatalogRequest :: Newtype ImportServerCatalogRequest _
derive instance repGenericImportServerCatalogRequest :: Generic ImportServerCatalogRequest _
instance showImportServerCatalogRequest :: Show ImportServerCatalogRequest where
  show = genericShow
instance decodeImportServerCatalogRequest :: Decode ImportServerCatalogRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportServerCatalogRequest :: Encode ImportServerCatalogRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportServerCatalogResponse = ImportServerCatalogResponse Types.NoArguments
derive instance newtypeImportServerCatalogResponse :: Newtype ImportServerCatalogResponse _
derive instance repGenericImportServerCatalogResponse :: Generic ImportServerCatalogResponse _
instance showImportServerCatalogResponse :: Show ImportServerCatalogResponse where
  show = genericShow
instance decodeImportServerCatalogResponse :: Decode ImportServerCatalogResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportServerCatalogResponse :: Encode ImportServerCatalogResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An internal error has occured.
newtype InternalError = InternalError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalError :: Newtype InternalError _
derive instance repGenericInternalError :: Generic InternalError _
instance showInternalError :: Show InternalError where
  show = genericShow
instance decodeInternalError :: Decode InternalError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalError :: Encode InternalError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A parameter specified in the request is not valid, is unsupported, or cannot be used.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Internet Protocol (IP) Address
newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.
newtype LicenseType = LicenseType String
derive instance newtypeLicenseType :: Newtype LicenseType _
derive instance repGenericLicenseType :: Generic LicenseType _
instance showLicenseType :: Show LicenseType where
  show = genericShow
instance decodeLicenseType :: Decode LicenseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLicenseType :: Encode LicenseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Hardware (MAC) address
newtype MacAddress = MacAddress String
derive instance newtypeMacAddress :: Newtype MacAddress _
derive instance repGenericMacAddress :: Generic MacAddress _
instance showMacAddress :: Show MacAddress where
  show = genericShow
instance decodeMacAddress :: Decode MacAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMacAddress :: Encode MacAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The maximum number of results to return in one API call. If left empty, this will default to 50.
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.
newtype MissingRequiredParameterException = MissingRequiredParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingRequiredParameterException :: Newtype MissingRequiredParameterException _
derive instance repGenericMissingRequiredParameterException :: Generic MissingRequiredParameterException _
instance showMissingRequiredParameterException :: Show MissingRequiredParameterException where
  show = genericShow
instance decodeMissingRequiredParameterException :: Decode MissingRequiredParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingRequiredParameterException :: Encode MissingRequiredParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Pagination token to pass as input to API call
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.
newtype NoConnectorsAvailableException = NoConnectorsAvailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoConnectorsAvailableException :: Newtype NoConnectorsAvailableException _
derive instance repGenericNoConnectorsAvailableException :: Generic NoConnectorsAvailableException _
instance showNoConnectorsAvailableException :: Show NoConnectorsAvailableException where
  show = genericShow
instance decodeNoConnectorsAvailableException :: Decode NoConnectorsAvailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoConnectorsAvailableException :: Encode NoConnectorsAvailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _
derive instance repGenericOperationNotPermittedException :: Generic OperationNotPermittedException _
instance showOperationNotPermittedException :: Show OperationNotPermittedException where
  show = genericShow
instance decodeOperationNotPermittedException :: Decode OperationNotPermittedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotPermittedException :: Encode OperationNotPermittedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a Replication Job
newtype ReplicationJob = ReplicationJob 
  { "ReplicationJobId'" :: NullOrUndefined.NullOrUndefined (ReplicationJobId)
  , "ServerId'" :: NullOrUndefined.NullOrUndefined (ServerId)
  , "ServerType'" :: NullOrUndefined.NullOrUndefined (ServerType)
  , "VmServer'" :: NullOrUndefined.NullOrUndefined (VmServer)
  , "SeedReplicationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Frequency'" :: NullOrUndefined.NullOrUndefined (Frequency)
  , "NextReplicationRunStartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LicenseType'" :: NullOrUndefined.NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined.NullOrUndefined (RoleName)
  , "LatestAmiId'" :: NullOrUndefined.NullOrUndefined (AmiId)
  , "State'" :: NullOrUndefined.NullOrUndefined (ReplicationJobState)
  , "StatusMessage'" :: NullOrUndefined.NullOrUndefined (ReplicationJobStatusMessage)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "ReplicationRunList'" :: NullOrUndefined.NullOrUndefined (ReplicationRunList)
  }
derive instance newtypeReplicationJob :: Newtype ReplicationJob _
derive instance repGenericReplicationJob :: Generic ReplicationJob _
instance showReplicationJob :: Show ReplicationJob where
  show = genericShow
instance decodeReplicationJob :: Decode ReplicationJob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJob :: Encode ReplicationJob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An active Replication Job already exists for the specified server.
newtype ReplicationJobAlreadyExistsException = ReplicationJobAlreadyExistsException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationJobAlreadyExistsException :: Newtype ReplicationJobAlreadyExistsException _
derive instance repGenericReplicationJobAlreadyExistsException :: Generic ReplicationJobAlreadyExistsException _
instance showReplicationJobAlreadyExistsException :: Show ReplicationJobAlreadyExistsException where
  show = genericShow
instance decodeReplicationJobAlreadyExistsException :: Decode ReplicationJobAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobAlreadyExistsException :: Encode ReplicationJobAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for a Replication Job.
newtype ReplicationJobId = ReplicationJobId String
derive instance newtypeReplicationJobId :: Newtype ReplicationJobId _
derive instance repGenericReplicationJobId :: Generic ReplicationJobId _
instance showReplicationJobId :: Show ReplicationJobId where
  show = genericShow
instance decodeReplicationJobId :: Decode ReplicationJobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobId :: Encode ReplicationJobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of Replication Jobs
newtype ReplicationJobList = ReplicationJobList (Array ReplicationJob)
derive instance newtypeReplicationJobList :: Newtype ReplicationJobList _
derive instance repGenericReplicationJobList :: Generic ReplicationJobList _
instance showReplicationJobList :: Show ReplicationJobList where
  show = genericShow
instance decodeReplicationJobList :: Decode ReplicationJobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobList :: Encode ReplicationJobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The specified Replication Job cannot be found.
newtype ReplicationJobNotFoundException = ReplicationJobNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationJobNotFoundException :: Newtype ReplicationJobNotFoundException _
derive instance repGenericReplicationJobNotFoundException :: Generic ReplicationJobNotFoundException _
instance showReplicationJobNotFoundException :: Show ReplicationJobNotFoundException where
  show = genericShow
instance decodeReplicationJobNotFoundException :: Decode ReplicationJobNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobNotFoundException :: Encode ReplicationJobNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Current state of Replication Job
newtype ReplicationJobState = ReplicationJobState String
derive instance newtypeReplicationJobState :: Newtype ReplicationJobState _
derive instance repGenericReplicationJobState :: Generic ReplicationJobState _
instance showReplicationJobState :: Show ReplicationJobState where
  show = genericShow
instance decodeReplicationJobState :: Decode ReplicationJobState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobState :: Encode ReplicationJobState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | String describing current status of Replication Job
newtype ReplicationJobStatusMessage = ReplicationJobStatusMessage String
derive instance newtypeReplicationJobStatusMessage :: Newtype ReplicationJobStatusMessage _
derive instance repGenericReplicationJobStatusMessage :: Generic ReplicationJobStatusMessage _
instance showReplicationJobStatusMessage :: Show ReplicationJobStatusMessage where
  show = genericShow
instance decodeReplicationJobStatusMessage :: Decode ReplicationJobStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobStatusMessage :: Encode ReplicationJobStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An indicator of the Replication Job being deleted or failed.
newtype ReplicationJobTerminated = ReplicationJobTerminated Boolean
derive instance newtypeReplicationJobTerminated :: Newtype ReplicationJobTerminated _
derive instance repGenericReplicationJobTerminated :: Generic ReplicationJobTerminated _
instance showReplicationJobTerminated :: Show ReplicationJobTerminated where
  show = genericShow
instance decodeReplicationJobTerminated :: Decode ReplicationJobTerminated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationJobTerminated :: Encode ReplicationJobTerminated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a Replication Run
newtype ReplicationRun = ReplicationRun 
  { "ReplicationRunId'" :: NullOrUndefined.NullOrUndefined (ReplicationRunId)
  , "State'" :: NullOrUndefined.NullOrUndefined (ReplicationRunState)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ReplicationRunType)
  , "StatusMessage'" :: NullOrUndefined.NullOrUndefined (ReplicationRunStatusMessage)
  , "AmiId'" :: NullOrUndefined.NullOrUndefined (AmiId)
  , "ScheduledStartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CompletedTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeReplicationRun :: Newtype ReplicationRun _
derive instance repGenericReplicationRun :: Generic ReplicationRun _
instance showReplicationRun :: Show ReplicationRun where
  show = genericShow
instance decodeReplicationRun :: Decode ReplicationRun where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRun :: Encode ReplicationRun where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for a Replication Run.
newtype ReplicationRunId = ReplicationRunId String
derive instance newtypeReplicationRunId :: Newtype ReplicationRunId _
derive instance repGenericReplicationRunId :: Generic ReplicationRunId _
instance showReplicationRunId :: Show ReplicationRunId where
  show = genericShow
instance decodeReplicationRunId :: Decode ReplicationRunId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunId :: Encode ReplicationRunId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This user has exceeded the maximum allowed Replication Run limit.
newtype ReplicationRunLimitExceededException = ReplicationRunLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationRunLimitExceededException :: Newtype ReplicationRunLimitExceededException _
derive instance repGenericReplicationRunLimitExceededException :: Generic ReplicationRunLimitExceededException _
instance showReplicationRunLimitExceededException :: Show ReplicationRunLimitExceededException where
  show = genericShow
instance decodeReplicationRunLimitExceededException :: Decode ReplicationRunLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunLimitExceededException :: Encode ReplicationRunLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of Replication Runs
newtype ReplicationRunList = ReplicationRunList (Array ReplicationRun)
derive instance newtypeReplicationRunList :: Newtype ReplicationRunList _
derive instance repGenericReplicationRunList :: Generic ReplicationRunList _
instance showReplicationRunList :: Show ReplicationRunList where
  show = genericShow
instance decodeReplicationRunList :: Decode ReplicationRunList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunList :: Encode ReplicationRunList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Current state of Replication Run
newtype ReplicationRunState = ReplicationRunState String
derive instance newtypeReplicationRunState :: Newtype ReplicationRunState _
derive instance repGenericReplicationRunState :: Generic ReplicationRunState _
instance showReplicationRunState :: Show ReplicationRunState where
  show = genericShow
instance decodeReplicationRunState :: Decode ReplicationRunState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunState :: Encode ReplicationRunState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | String describing current status of Replication Run
newtype ReplicationRunStatusMessage = ReplicationRunStatusMessage String
derive instance newtypeReplicationRunStatusMessage :: Newtype ReplicationRunStatusMessage _
derive instance repGenericReplicationRunStatusMessage :: Generic ReplicationRunStatusMessage _
instance showReplicationRunStatusMessage :: Show ReplicationRunStatusMessage where
  show = genericShow
instance decodeReplicationRunStatusMessage :: Decode ReplicationRunStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunStatusMessage :: Encode ReplicationRunStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of Replication Run
newtype ReplicationRunType = ReplicationRunType String
derive instance newtypeReplicationRunType :: Newtype ReplicationRunType _
derive instance repGenericReplicationRunType :: Generic ReplicationRunType _
instance showReplicationRunType :: Show ReplicationRunType where
  show = genericShow
instance decodeReplicationRunType :: Decode ReplicationRunType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationRunType :: Encode ReplicationRunType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Name of service role in customer's account to be used by SMS service.
newtype RoleName = RoleName String
derive instance newtypeRoleName :: Newtype RoleName _
derive instance repGenericRoleName :: Generic RoleName _
instance showRoleName :: Show RoleName where
  show = genericShow
instance decodeRoleName :: Decode RoleName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleName :: Encode RoleName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a server
newtype Server = Server 
  { "ServerId'" :: NullOrUndefined.NullOrUndefined (ServerId)
  , "ServerType'" :: NullOrUndefined.NullOrUndefined (ServerType)
  , "VmServer'" :: NullOrUndefined.NullOrUndefined (VmServer)
  , "ReplicationJobId'" :: NullOrUndefined.NullOrUndefined (ReplicationJobId)
  , "ReplicationJobTerminated'" :: NullOrUndefined.NullOrUndefined (ReplicationJobTerminated)
  }
derive instance newtypeServer :: Newtype Server _
derive instance repGenericServer :: Generic Server _
instance showServer :: Show Server where
  show = genericShow
instance decodeServer :: Decode Server where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServer :: Encode Server where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The provided server cannot be replicated.
newtype ServerCannotBeReplicatedException = ServerCannotBeReplicatedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServerCannotBeReplicatedException :: Newtype ServerCannotBeReplicatedException _
derive instance repGenericServerCannotBeReplicatedException :: Generic ServerCannotBeReplicatedException _
instance showServerCannotBeReplicatedException :: Show ServerCannotBeReplicatedException where
  show = genericShow
instance decodeServerCannotBeReplicatedException :: Decode ServerCannotBeReplicatedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerCannotBeReplicatedException :: Encode ServerCannotBeReplicatedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Status of Server catalog
newtype ServerCatalogStatus = ServerCatalogStatus String
derive instance newtypeServerCatalogStatus :: Newtype ServerCatalogStatus _
derive instance repGenericServerCatalogStatus :: Generic ServerCatalogStatus _
instance showServerCatalogStatus :: Show ServerCatalogStatus where
  show = genericShow
instance decodeServerCatalogStatus :: Decode ServerCatalogStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerCatalogStatus :: Encode ServerCatalogStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Unique Identifier for a server
newtype ServerId = ServerId String
derive instance newtypeServerId :: Newtype ServerId _
derive instance repGenericServerId :: Generic ServerId _
instance showServerId :: Show ServerId where
  show = genericShow
instance decodeServerId :: Decode ServerId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerId :: Encode ServerId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | List of servers from catalog
newtype ServerList = ServerList (Array Server)
derive instance newtypeServerList :: Newtype ServerList _
derive instance repGenericServerList :: Generic ServerList _
instance showServerList :: Show ServerList where
  show = genericShow
instance decodeServerList :: Decode ServerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerList :: Encode ServerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of server.
newtype ServerType = ServerType String
derive instance newtypeServerType :: Newtype ServerType _
derive instance repGenericServerType :: Generic ServerType _
instance showServerType :: Show ServerType where
  show = genericShow
instance decodeServerType :: Decode ServerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerType :: Encode ServerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartOnDemandReplicationRunRequest = StartOnDemandReplicationRunRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeStartOnDemandReplicationRunRequest :: Newtype StartOnDemandReplicationRunRequest _
derive instance repGenericStartOnDemandReplicationRunRequest :: Generic StartOnDemandReplicationRunRequest _
instance showStartOnDemandReplicationRunRequest :: Show StartOnDemandReplicationRunRequest where
  show = genericShow
instance decodeStartOnDemandReplicationRunRequest :: Decode StartOnDemandReplicationRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartOnDemandReplicationRunRequest :: Encode StartOnDemandReplicationRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse 
  { "ReplicationRunId'" :: NullOrUndefined.NullOrUndefined (ReplicationRunId)
  }
derive instance newtypeStartOnDemandReplicationRunResponse :: Newtype StartOnDemandReplicationRunResponse _
derive instance repGenericStartOnDemandReplicationRunResponse :: Generic StartOnDemandReplicationRunResponse _
instance showStartOnDemandReplicationRunResponse :: Show StartOnDemandReplicationRunResponse where
  show = genericShow
instance decodeStartOnDemandReplicationRunResponse :: Decode StartOnDemandReplicationRunResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartOnDemandReplicationRunResponse :: Encode StartOnDemandReplicationRunResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This user does not have permissions to perform this operation.
newtype UnauthorizedOperationException = UnauthorizedOperationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedOperationException :: Newtype UnauthorizedOperationException _
derive instance repGenericUnauthorizedOperationException :: Generic UnauthorizedOperationException _
instance showUnauthorizedOperationException :: Show UnauthorizedOperationException where
  show = genericShow
instance decodeUnauthorizedOperationException :: Decode UnauthorizedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedOperationException :: Encode UnauthorizedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateReplicationJobRequest = UpdateReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Frequency'" :: NullOrUndefined.NullOrUndefined (Frequency)
  , "NextReplicationRunStartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LicenseType'" :: NullOrUndefined.NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined.NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeUpdateReplicationJobRequest :: Newtype UpdateReplicationJobRequest _
derive instance repGenericUpdateReplicationJobRequest :: Generic UpdateReplicationJobRequest _
instance showUpdateReplicationJobRequest :: Show UpdateReplicationJobRequest where
  show = genericShow
instance decodeUpdateReplicationJobRequest :: Decode UpdateReplicationJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateReplicationJobRequest :: Encode UpdateReplicationJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse Types.NoArguments
derive instance newtypeUpdateReplicationJobResponse :: Newtype UpdateReplicationJobResponse _
derive instance repGenericUpdateReplicationJobResponse :: Generic UpdateReplicationJobResponse _
instance showUpdateReplicationJobResponse :: Show UpdateReplicationJobResponse where
  show = genericShow
instance decodeUpdateReplicationJobResponse :: Decode UpdateReplicationJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateReplicationJobResponse :: Encode UpdateReplicationJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Unique Identifier for a VM
newtype VmId = VmId String
derive instance newtypeVmId :: Newtype VmId _
derive instance repGenericVmId :: Generic VmId _
instance showVmId :: Show VmId where
  show = genericShow
instance decodeVmId :: Decode VmId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmId :: Encode VmId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Unique Identifier for VM Manager
newtype VmManagerId = VmManagerId String
derive instance newtypeVmManagerId :: Newtype VmManagerId _
derive instance repGenericVmManagerId :: Generic VmManagerId _
instance showVmManagerId :: Show VmManagerId where
  show = genericShow
instance decodeVmManagerId :: Decode VmManagerId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmManagerId :: Encode VmManagerId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | VM Manager Name
newtype VmManagerName = VmManagerName String
derive instance newtypeVmManagerName :: Newtype VmManagerName _
derive instance repGenericVmManagerName :: Generic VmManagerName _
instance showVmManagerName :: Show VmManagerName where
  show = genericShow
instance decodeVmManagerName :: Decode VmManagerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmManagerName :: Encode VmManagerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | VM Management Product
newtype VmManagerType = VmManagerType String
derive instance newtypeVmManagerType :: Newtype VmManagerType _
derive instance repGenericVmManagerType :: Generic VmManagerType _
instance showVmManagerType :: Show VmManagerType where
  show = genericShow
instance decodeVmManagerType :: Decode VmManagerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmManagerType :: Encode VmManagerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Name of Virtual Machine
newtype VmName = VmName String
derive instance newtypeVmName :: Newtype VmName _
derive instance repGenericVmName :: Generic VmName _
instance showVmName :: Show VmName where
  show = genericShow
instance decodeVmName :: Decode VmName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmName :: Encode VmName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Path to VM
newtype VmPath = VmPath String
derive instance newtypeVmPath :: Newtype VmPath _
derive instance repGenericVmPath :: Generic VmPath _
instance showVmPath :: Show VmPath where
  show = genericShow
instance decodeVmPath :: Decode VmPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmPath :: Encode VmPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a VM server
newtype VmServer = VmServer 
  { "VmServerAddress'" :: NullOrUndefined.NullOrUndefined (VmServerAddress)
  , "VmName'" :: NullOrUndefined.NullOrUndefined (VmName)
  , "VmManagerName'" :: NullOrUndefined.NullOrUndefined (VmManagerName)
  , "VmManagerType'" :: NullOrUndefined.NullOrUndefined (VmManagerType)
  , "VmPath'" :: NullOrUndefined.NullOrUndefined (VmPath)
  }
derive instance newtypeVmServer :: Newtype VmServer _
derive instance repGenericVmServer :: Generic VmServer _
instance showVmServer :: Show VmServer where
  show = genericShow
instance decodeVmServer :: Decode VmServer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmServer :: Encode VmServer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Object representing a server's location
newtype VmServerAddress = VmServerAddress 
  { "VmManagerId'" :: NullOrUndefined.NullOrUndefined (VmManagerId)
  , "VmId'" :: NullOrUndefined.NullOrUndefined (VmId)
  }
derive instance newtypeVmServerAddress :: Newtype VmServerAddress _
derive instance repGenericVmServerAddress :: Generic VmServerAddress _
instance showVmServerAddress :: Show VmServerAddress where
  show = genericShow
instance decodeVmServerAddress :: Decode VmServerAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVmServerAddress :: Encode VmServerAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
