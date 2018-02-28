

module AWS.KinesisAnalytics where

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

serviceName = "KinesisAnalytics" :: String


-- | <p>Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>
addApplicationCloudWatchLoggingOption :: forall eff. AddApplicationCloudWatchLoggingOptionRequest -> Aff (exception :: EXCEPTION | eff) AddApplicationCloudWatchLoggingOptionResponse
addApplicationCloudWatchLoggingOption = Request.request serviceName "addApplicationCloudWatchLoggingOption" 


-- | <p> Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p> <p>You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see <a>CreateApplication</a>.</p> <p>Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationInput</code> action.</p>
addApplicationInput :: forall eff. AddApplicationInputRequest -> Aff (exception :: EXCEPTION | eff) AddApplicationInputResponse
addApplicationInput = Request.request serviceName "addApplicationInput" 


-- | <p>Adds an <a>InputProcessingConfiguration</a> to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
addApplicationInputProcessingConfiguration :: forall eff. AddApplicationInputProcessingConfigurationRequest -> Aff (exception :: EXCEPTION | eff) AddApplicationInputProcessingConfigurationResponse
addApplicationInputProcessingConfiguration = Request.request serviceName "addApplicationInputProcessingConfiguration" 


-- | <p>Adds an external destination to your Amazon Kinesis Analytics application.</p> <p>If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.</p> <p> You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Understanding Application Output (Destination)</a>. </p> <p> Note that any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version.</p> <p>For the limits on the number of application inputs and outputs you can configure, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action.</p>
addApplicationOutput :: forall eff. AddApplicationOutputRequest -> Aff (exception :: EXCEPTION | eff) AddApplicationOutputResponse
addApplicationOutput = Request.request serviceName "addApplicationOutput" 


-- | <p>Adds a reference data source to an existing application.</p> <p>Amazon Kinesis Analytics reads reference data (that is, an Amazon S3 object) and creates an in-application table within your application. In the request, you provide the source (S3 bucket name and object key name), name of the in-application table to create, and the necessary mapping information that describes how data in Amazon S3 object maps to columns in the resulting in-application table.</p> <p> For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. For the limits on data sources you can add to your application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action. </p>
addApplicationReferenceDataSource :: forall eff. AddApplicationReferenceDataSourceRequest -> Aff (exception :: EXCEPTION | eff) AddApplicationReferenceDataSourceResponse
addApplicationReferenceDataSource = Request.request serviceName "addApplicationReferenceDataSource" 


-- | <p> Creates an Amazon Kinesis Analytics application. You can configure each application with one streaming source as input, application code to process the input, and up to three destinations where you want Amazon Kinesis Analytics to write the output data from your application. For an overview, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html">How it Works</a>. </p> <p>In the input configuration, you map the streaming source to an in-application stream, which you can think of as a constantly updating table. In the mapping, you must provide a schema for the in-application stream and map each data column in the in-application stream to a data element in the streaming source.</p> <p>Your application code is one or more SQL statements that read input data, transform it, and generate output. Your application code can create one or more SQL artifacts like SQL streams or pumps.</p> <p>In the output configuration, you can configure the application to write data from in-application streams created in your applications to up to three destinations.</p> <p> To read data from your source stream or write data to destination streams, Amazon Kinesis Analytics needs your permissions. You grant these permissions by creating IAM roles. This operation requires permissions to perform the <code>kinesisanalytics:CreateApplication</code> action. </p> <p> For introductory exercises to create an Amazon Kinesis Analytics application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html">Getting Started</a>. </p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (exception :: EXCEPTION | eff) CreateApplicationResponse
createApplication = Request.request serviceName "createApplication" 


-- | <p>Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplication</code> action.</p>
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationResponse
deleteApplication = Request.request serviceName "deleteApplication" 


-- | <p>Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>
deleteApplicationCloudWatchLoggingOption :: forall eff. DeleteApplicationCloudWatchLoggingOptionRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationCloudWatchLoggingOptionResponse
deleteApplicationCloudWatchLoggingOption = Request.request serviceName "deleteApplicationCloudWatchLoggingOption" 


-- | <p>Deletes an <a>InputProcessingConfiguration</a> from an input.</p>
deleteApplicationInputProcessingConfiguration :: forall eff. DeleteApplicationInputProcessingConfigurationRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationInputProcessingConfigurationResponse
deleteApplicationInputProcessingConfiguration = Request.request serviceName "deleteApplicationInputProcessingConfiguration" 


-- | <p>Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplicationOutput</code> action.</p>
deleteApplicationOutput :: forall eff. DeleteApplicationOutputRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationOutputResponse
deleteApplicationOutput = Request.request serviceName "deleteApplicationOutput" 


-- | <p>Deletes a reference data source configuration from the specified application configuration.</p> <p>If the application is running, Amazon Kinesis Analytics immediately removes the in-application table that you created using the <a>AddApplicationReferenceDataSource</a> operation. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics.DeleteApplicationReferenceDataSource</code> action.</p>
deleteApplicationReferenceDataSource :: forall eff. DeleteApplicationReferenceDataSourceRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationReferenceDataSourceResponse
deleteApplicationReferenceDataSource = Request.request serviceName "deleteApplicationReferenceDataSource" 


-- | <p>Returns information about a specific Amazon Kinesis Analytics application.</p> <p>If you want to retrieve a list of all applications in your account, use the <a>ListApplications</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DescribeApplication</code> action. You can use <code>DescribeApplication</code> to get the current application versionId, which you need to call other operations such as <code>Update</code>. </p>
describeApplication :: forall eff. DescribeApplicationRequest -> Aff (exception :: EXCEPTION | eff) DescribeApplicationResponse
describeApplication = Request.request serviceName "describeApplication" 


-- | <p>Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream) or S3 object. In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.</p> <p> You can use the inferred schema when configuring a streaming source for your application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. Note that when you create an application using the Amazon Kinesis Analytics console, the console uses this operation to infer a schema and show it in the console user interface. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:DiscoverInputSchema</code> action. </p>
discoverInputSchema :: forall eff. DiscoverInputSchemaRequest -> Aff (exception :: EXCEPTION | eff) DiscoverInputSchemaResponse
discoverInputSchema = Request.request serviceName "discoverInputSchema" 


-- | <p>Returns a list of Amazon Kinesis Analytics applications in your account. For each application, the response includes the application name, Amazon Resource Name (ARN), and status. If the response returns the <code>HasMoreApplications</code> value as true, you can send another request by adding the <code>ExclusiveStartApplicationName</code> in the request body, and set the value of this to the last application name from the previous response. </p> <p>If you want detailed information about a specific application, use <a>DescribeApplication</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:ListApplications</code> action.</p>
listApplications :: forall eff. ListApplicationsRequest -> Aff (exception :: EXCEPTION | eff) ListApplicationsResponse
listApplications = Request.request serviceName "listApplications" 


-- | <p>Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.</p> <p>After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.</p> <p> The application status must be <code>READY</code> for you to start an application. You can get the application status in the console or using the <a>DescribeApplication</a> operation.</p> <p>After you start the application, you can stop the application from processing the input by calling the <a>StopApplication</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StartApplication</code> action.</p>
startApplication :: forall eff. StartApplicationRequest -> Aff (exception :: EXCEPTION | eff) StartApplicationResponse
startApplication = Request.request serviceName "startApplication" 


-- | <p>Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the <a>DescribeApplication</a> operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StopApplication</code> action.</p>
stopApplication :: forall eff. StopApplicationRequest -> Aff (exception :: EXCEPTION | eff) StopApplicationResponse
stopApplication = Request.request serviceName "stopApplication" 


-- | <p>Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration. </p> <p>Note that Amazon Kinesis Analytics updates the <code>CurrentApplicationVersionId</code> each time you update your application. </p> <p>This operation requires permission for the <code>kinesisanalytics:UpdateApplication</code> action.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (exception :: EXCEPTION | eff) UpdateApplicationResponse
updateApplication = Request.request serviceName "updateApplication" 


newtype AddApplicationCloudWatchLoggingOptionRequest = AddApplicationCloudWatchLoggingOptionRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "CloudWatchLoggingOption" :: (CloudWatchLoggingOption)
  }
derive instance newtypeAddApplicationCloudWatchLoggingOptionRequest :: Newtype AddApplicationCloudWatchLoggingOptionRequest _
derive instance repGenericAddApplicationCloudWatchLoggingOptionRequest :: Generic AddApplicationCloudWatchLoggingOptionRequest _
instance showAddApplicationCloudWatchLoggingOptionRequest :: Show AddApplicationCloudWatchLoggingOptionRequest where
  show = genericShow
instance decodeAddApplicationCloudWatchLoggingOptionRequest :: Decode AddApplicationCloudWatchLoggingOptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationCloudWatchLoggingOptionRequest :: Encode AddApplicationCloudWatchLoggingOptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse Types.NoArguments
derive instance newtypeAddApplicationCloudWatchLoggingOptionResponse :: Newtype AddApplicationCloudWatchLoggingOptionResponse _
derive instance repGenericAddApplicationCloudWatchLoggingOptionResponse :: Generic AddApplicationCloudWatchLoggingOptionResponse _
instance showAddApplicationCloudWatchLoggingOptionResponse :: Show AddApplicationCloudWatchLoggingOptionResponse where
  show = genericShow
instance decodeAddApplicationCloudWatchLoggingOptionResponse :: Decode AddApplicationCloudWatchLoggingOptionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationCloudWatchLoggingOptionResponse :: Encode AddApplicationCloudWatchLoggingOptionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddApplicationInputProcessingConfigurationRequest = AddApplicationInputProcessingConfigurationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "InputId" :: (Id)
  , "InputProcessingConfiguration" :: (InputProcessingConfiguration)
  }
derive instance newtypeAddApplicationInputProcessingConfigurationRequest :: Newtype AddApplicationInputProcessingConfigurationRequest _
derive instance repGenericAddApplicationInputProcessingConfigurationRequest :: Generic AddApplicationInputProcessingConfigurationRequest _
instance showAddApplicationInputProcessingConfigurationRequest :: Show AddApplicationInputProcessingConfigurationRequest where
  show = genericShow
instance decodeAddApplicationInputProcessingConfigurationRequest :: Decode AddApplicationInputProcessingConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationInputProcessingConfigurationRequest :: Encode AddApplicationInputProcessingConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse Types.NoArguments
derive instance newtypeAddApplicationInputProcessingConfigurationResponse :: Newtype AddApplicationInputProcessingConfigurationResponse _
derive instance repGenericAddApplicationInputProcessingConfigurationResponse :: Generic AddApplicationInputProcessingConfigurationResponse _
instance showAddApplicationInputProcessingConfigurationResponse :: Show AddApplicationInputProcessingConfigurationResponse where
  show = genericShow
instance decodeAddApplicationInputProcessingConfigurationResponse :: Decode AddApplicationInputProcessingConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationInputProcessingConfigurationResponse :: Encode AddApplicationInputProcessingConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationInputRequest = AddApplicationInputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "Input" :: (Input)
  }
derive instance newtypeAddApplicationInputRequest :: Newtype AddApplicationInputRequest _
derive instance repGenericAddApplicationInputRequest :: Generic AddApplicationInputRequest _
instance showAddApplicationInputRequest :: Show AddApplicationInputRequest where
  show = genericShow
instance decodeAddApplicationInputRequest :: Decode AddApplicationInputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationInputRequest :: Encode AddApplicationInputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationInputResponse = AddApplicationInputResponse Types.NoArguments
derive instance newtypeAddApplicationInputResponse :: Newtype AddApplicationInputResponse _
derive instance repGenericAddApplicationInputResponse :: Generic AddApplicationInputResponse _
instance showAddApplicationInputResponse :: Show AddApplicationInputResponse where
  show = genericShow
instance decodeAddApplicationInputResponse :: Decode AddApplicationInputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationInputResponse :: Encode AddApplicationInputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationOutputRequest = AddApplicationOutputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "Output" :: (Output)
  }
derive instance newtypeAddApplicationOutputRequest :: Newtype AddApplicationOutputRequest _
derive instance repGenericAddApplicationOutputRequest :: Generic AddApplicationOutputRequest _
instance showAddApplicationOutputRequest :: Show AddApplicationOutputRequest where
  show = genericShow
instance decodeAddApplicationOutputRequest :: Decode AddApplicationOutputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationOutputRequest :: Encode AddApplicationOutputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationOutputResponse = AddApplicationOutputResponse Types.NoArguments
derive instance newtypeAddApplicationOutputResponse :: Newtype AddApplicationOutputResponse _
derive instance repGenericAddApplicationOutputResponse :: Generic AddApplicationOutputResponse _
instance showAddApplicationOutputResponse :: Show AddApplicationOutputResponse where
  show = genericShow
instance decodeAddApplicationOutputResponse :: Decode AddApplicationOutputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationOutputResponse :: Encode AddApplicationOutputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationReferenceDataSourceRequest = AddApplicationReferenceDataSourceRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ReferenceDataSource" :: (ReferenceDataSource)
  }
derive instance newtypeAddApplicationReferenceDataSourceRequest :: Newtype AddApplicationReferenceDataSourceRequest _
derive instance repGenericAddApplicationReferenceDataSourceRequest :: Generic AddApplicationReferenceDataSourceRequest _
instance showAddApplicationReferenceDataSourceRequest :: Show AddApplicationReferenceDataSourceRequest where
  show = genericShow
instance decodeAddApplicationReferenceDataSourceRequest :: Decode AddApplicationReferenceDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationReferenceDataSourceRequest :: Encode AddApplicationReferenceDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse Types.NoArguments
derive instance newtypeAddApplicationReferenceDataSourceResponse :: Newtype AddApplicationReferenceDataSourceResponse _
derive instance repGenericAddApplicationReferenceDataSourceResponse :: Generic AddApplicationReferenceDataSourceResponse _
instance showAddApplicationReferenceDataSourceResponse :: Show AddApplicationReferenceDataSourceResponse where
  show = genericShow
instance decodeAddApplicationReferenceDataSourceResponse :: Decode AddApplicationReferenceDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddApplicationReferenceDataSourceResponse :: Encode AddApplicationReferenceDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationCode = ApplicationCode String
derive instance newtypeApplicationCode :: Newtype ApplicationCode _
derive instance repGenericApplicationCode :: Generic ApplicationCode _
instance showApplicationCode :: Show ApplicationCode where
  show = genericShow
instance decodeApplicationCode :: Decode ApplicationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationCode :: Encode ApplicationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationDescription = ApplicationDescription String
derive instance newtypeApplicationDescription :: Newtype ApplicationDescription _
derive instance repGenericApplicationDescription :: Generic ApplicationDescription _
instance showApplicationDescription :: Show ApplicationDescription where
  show = genericShow
instance decodeApplicationDescription :: Decode ApplicationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDescription :: Encode ApplicationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.</p>
newtype ApplicationDetail = ApplicationDetail 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationDescription" :: NullOrUndefined.NullOrUndefined (ApplicationDescription)
  , "ApplicationARN" :: (ResourceARN)
  , "ApplicationStatus" :: (ApplicationStatus)
  , "CreateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "InputDescriptions" :: NullOrUndefined.NullOrUndefined (InputDescriptions)
  , "OutputDescriptions" :: NullOrUndefined.NullOrUndefined (OutputDescriptions)
  , "ReferenceDataSourceDescriptions" :: NullOrUndefined.NullOrUndefined (ReferenceDataSourceDescriptions)
  , "CloudWatchLoggingOptionDescriptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptionDescriptions)
  , "ApplicationCode" :: NullOrUndefined.NullOrUndefined (ApplicationCode)
  , "ApplicationVersionId" :: (ApplicationVersionId)
  }
derive instance newtypeApplicationDetail :: Newtype ApplicationDetail _
derive instance repGenericApplicationDetail :: Generic ApplicationDetail _
instance showApplicationDetail :: Show ApplicationDetail where
  show = genericShow
instance decodeApplicationDetail :: Decode ApplicationDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDetail :: Encode ApplicationDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationName = ApplicationName String
derive instance newtypeApplicationName :: Newtype ApplicationName _
derive instance repGenericApplicationName :: Generic ApplicationName _
instance showApplicationName :: Show ApplicationName where
  show = genericShow
instance decodeApplicationName :: Decode ApplicationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationName :: Encode ApplicationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationStatus = ApplicationStatus String
derive instance newtypeApplicationStatus :: Newtype ApplicationStatus _
derive instance repGenericApplicationStatus :: Generic ApplicationStatus _
instance showApplicationStatus :: Show ApplicationStatus where
  show = genericShow
instance decodeApplicationStatus :: Decode ApplicationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationStatus :: Encode ApplicationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationSummaries = ApplicationSummaries (Array ApplicationSummary)
derive instance newtypeApplicationSummaries :: Newtype ApplicationSummaries _
derive instance repGenericApplicationSummaries :: Generic ApplicationSummaries _
instance showApplicationSummaries :: Show ApplicationSummaries where
  show = genericShow
instance decodeApplicationSummaries :: Decode ApplicationSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationSummaries :: Encode ApplicationSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.</p>
newtype ApplicationSummary = ApplicationSummary 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationARN" :: (ResourceARN)
  , "ApplicationStatus" :: (ApplicationStatus)
  }
derive instance newtypeApplicationSummary :: Newtype ApplicationSummary _
derive instance repGenericApplicationSummary :: Generic ApplicationSummary _
instance showApplicationSummary :: Show ApplicationSummary where
  show = genericShow
instance decodeApplicationSummary :: Decode ApplicationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationSummary :: Encode ApplicationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes updates to apply to an existing Amazon Kinesis Analytics application.</p>
newtype ApplicationUpdate = ApplicationUpdate 
  { "InputUpdates" :: NullOrUndefined.NullOrUndefined (InputUpdates)
  , "ApplicationCodeUpdate" :: NullOrUndefined.NullOrUndefined (ApplicationCode)
  , "OutputUpdates" :: NullOrUndefined.NullOrUndefined (OutputUpdates)
  , "ReferenceDataSourceUpdates" :: NullOrUndefined.NullOrUndefined (ReferenceDataSourceUpdates)
  , "CloudWatchLoggingOptionUpdates" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptionUpdates)
  }
derive instance newtypeApplicationUpdate :: Newtype ApplicationUpdate _
derive instance repGenericApplicationUpdate :: Generic ApplicationUpdate _
instance showApplicationUpdate :: Show ApplicationUpdate where
  show = genericShow
instance decodeApplicationUpdate :: Decode ApplicationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationUpdate :: Encode ApplicationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationVersionId = ApplicationVersionId Number
derive instance newtypeApplicationVersionId :: Newtype ApplicationVersionId _
derive instance repGenericApplicationVersionId :: Generic ApplicationVersionId _
instance showApplicationVersionId :: Show ApplicationVersionId where
  show = genericShow
instance decodeApplicationVersionId :: Decode ApplicationVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionId :: Encode ApplicationVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _
derive instance repGenericBooleanObject :: Generic BooleanObject _
instance showBooleanObject :: Show BooleanObject where
  show = genericShow
instance decodeBooleanObject :: Decode BooleanObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanObject :: Encode BooleanObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketARN = BucketARN String
derive instance newtypeBucketARN :: Newtype BucketARN _
derive instance repGenericBucketARN :: Generic BucketARN _
instance showBucketARN :: Show BucketARN where
  show = genericShow
instance decodeBucketARN :: Decode BucketARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketARN :: Encode BucketARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the <i>'\n'</i> as the row delimiter and a comma (",") as the column delimiter: </p> <p> <code>"name1", "address1" </code> </p> <p> <code>"name2, "address2"</code> </p>
newtype CSVMappingParameters = CSVMappingParameters 
  { "RecordRowDelimiter" :: (RecordRowDelimiter)
  , "RecordColumnDelimiter" :: (RecordColumnDelimiter)
  }
derive instance newtypeCSVMappingParameters :: Newtype CSVMappingParameters _
derive instance repGenericCSVMappingParameters :: Generic CSVMappingParameters _
instance showCSVMappingParameters :: Show CSVMappingParameters where
  show = genericShow
instance decodeCSVMappingParameters :: Decode CSVMappingParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCSVMappingParameters :: Encode CSVMappingParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.</p>
newtype CloudWatchLoggingOption = CloudWatchLoggingOption 
  { "LogStreamARN" :: (LogStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOption :: Newtype CloudWatchLoggingOption _
derive instance repGenericCloudWatchLoggingOption :: Generic CloudWatchLoggingOption _
instance showCloudWatchLoggingOption :: Show CloudWatchLoggingOption where
  show = genericShow
instance decodeCloudWatchLoggingOption :: Decode CloudWatchLoggingOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOption :: Encode CloudWatchLoggingOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Description of the CloudWatch logging option.</p>
newtype CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription 
  { "CloudWatchLoggingOptionId" :: NullOrUndefined.NullOrUndefined (Id)
  , "LogStreamARN" :: (LogStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOptionDescription :: Newtype CloudWatchLoggingOptionDescription _
derive instance repGenericCloudWatchLoggingOptionDescription :: Generic CloudWatchLoggingOptionDescription _
instance showCloudWatchLoggingOptionDescription :: Show CloudWatchLoggingOptionDescription where
  show = genericShow
instance decodeCloudWatchLoggingOptionDescription :: Decode CloudWatchLoggingOptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptionDescription :: Encode CloudWatchLoggingOptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchLoggingOptionDescriptions = CloudWatchLoggingOptionDescriptions (Array CloudWatchLoggingOptionDescription)
derive instance newtypeCloudWatchLoggingOptionDescriptions :: Newtype CloudWatchLoggingOptionDescriptions _
derive instance repGenericCloudWatchLoggingOptionDescriptions :: Generic CloudWatchLoggingOptionDescriptions _
instance showCloudWatchLoggingOptionDescriptions :: Show CloudWatchLoggingOptionDescriptions where
  show = genericShow
instance decodeCloudWatchLoggingOptionDescriptions :: Decode CloudWatchLoggingOptionDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptionDescriptions :: Encode CloudWatchLoggingOptionDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes CloudWatch logging option updates.</p>
newtype CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate 
  { "CloudWatchLoggingOptionId" :: (Id)
  , "LogStreamARNUpdate" :: NullOrUndefined.NullOrUndefined (LogStreamARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOptionUpdate :: Newtype CloudWatchLoggingOptionUpdate _
derive instance repGenericCloudWatchLoggingOptionUpdate :: Generic CloudWatchLoggingOptionUpdate _
instance showCloudWatchLoggingOptionUpdate :: Show CloudWatchLoggingOptionUpdate where
  show = genericShow
instance decodeCloudWatchLoggingOptionUpdate :: Decode CloudWatchLoggingOptionUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptionUpdate :: Encode CloudWatchLoggingOptionUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchLoggingOptionUpdates = CloudWatchLoggingOptionUpdates (Array CloudWatchLoggingOptionUpdate)
derive instance newtypeCloudWatchLoggingOptionUpdates :: Newtype CloudWatchLoggingOptionUpdates _
derive instance repGenericCloudWatchLoggingOptionUpdates :: Generic CloudWatchLoggingOptionUpdates _
instance showCloudWatchLoggingOptionUpdates :: Show CloudWatchLoggingOptionUpdates where
  show = genericShow
instance decodeCloudWatchLoggingOptionUpdates :: Decode CloudWatchLoggingOptionUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptionUpdates :: Encode CloudWatchLoggingOptionUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchLoggingOptions = CloudWatchLoggingOptions (Array CloudWatchLoggingOption)
derive instance newtypeCloudWatchLoggingOptions :: Newtype CloudWatchLoggingOptions _
derive instance repGenericCloudWatchLoggingOptions :: Generic CloudWatchLoggingOptions _
instance showCloudWatchLoggingOptions :: Show CloudWatchLoggingOptions where
  show = genericShow
instance decodeCloudWatchLoggingOptions :: Decode CloudWatchLoggingOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptions :: Encode CloudWatchLoggingOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>User-provided application code (query) is invalid. This can be a simple syntax error.</p>
newtype CodeValidationException = CodeValidationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCodeValidationException :: Newtype CodeValidationException _
derive instance repGenericCodeValidationException :: Generic CodeValidationException _
instance showCodeValidationException :: Show CodeValidationException where
  show = genericShow
instance decodeCodeValidationException :: Decode CodeValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeValidationException :: Encode CodeValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>TBD</p>
newtype CreateApplicationRequest = CreateApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationDescription" :: NullOrUndefined.NullOrUndefined (ApplicationDescription)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (Inputs)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (Outputs)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  , "ApplicationCode" :: NullOrUndefined.NullOrUndefined (ApplicationCode)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _
derive instance repGenericCreateApplicationRequest :: Generic CreateApplicationRequest _
instance showCreateApplicationRequest :: Show CreateApplicationRequest where
  show = genericShow
instance decodeCreateApplicationRequest :: Decode CreateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationRequest :: Encode CreateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>TBD</p>
newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ApplicationSummary" :: (ApplicationSummary)
  }
derive instance newtypeCreateApplicationResponse :: Newtype CreateApplicationResponse _
derive instance repGenericCreateApplicationResponse :: Generic CreateApplicationResponse _
instance showCreateApplicationResponse :: Show CreateApplicationResponse where
  show = genericShow
instance decodeCreateApplicationResponse :: Decode CreateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationResponse :: Encode CreateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationCloudWatchLoggingOptionRequest = DeleteApplicationCloudWatchLoggingOptionRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "CloudWatchLoggingOptionId" :: (Id)
  }
derive instance newtypeDeleteApplicationCloudWatchLoggingOptionRequest :: Newtype DeleteApplicationCloudWatchLoggingOptionRequest _
derive instance repGenericDeleteApplicationCloudWatchLoggingOptionRequest :: Generic DeleteApplicationCloudWatchLoggingOptionRequest _
instance showDeleteApplicationCloudWatchLoggingOptionRequest :: Show DeleteApplicationCloudWatchLoggingOptionRequest where
  show = genericShow
instance decodeDeleteApplicationCloudWatchLoggingOptionRequest :: Decode DeleteApplicationCloudWatchLoggingOptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationCloudWatchLoggingOptionRequest :: Encode DeleteApplicationCloudWatchLoggingOptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse Types.NoArguments
derive instance newtypeDeleteApplicationCloudWatchLoggingOptionResponse :: Newtype DeleteApplicationCloudWatchLoggingOptionResponse _
derive instance repGenericDeleteApplicationCloudWatchLoggingOptionResponse :: Generic DeleteApplicationCloudWatchLoggingOptionResponse _
instance showDeleteApplicationCloudWatchLoggingOptionResponse :: Show DeleteApplicationCloudWatchLoggingOptionResponse where
  show = genericShow
instance decodeDeleteApplicationCloudWatchLoggingOptionResponse :: Decode DeleteApplicationCloudWatchLoggingOptionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationCloudWatchLoggingOptionResponse :: Encode DeleteApplicationCloudWatchLoggingOptionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationInputProcessingConfigurationRequest = DeleteApplicationInputProcessingConfigurationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "InputId" :: (Id)
  }
derive instance newtypeDeleteApplicationInputProcessingConfigurationRequest :: Newtype DeleteApplicationInputProcessingConfigurationRequest _
derive instance repGenericDeleteApplicationInputProcessingConfigurationRequest :: Generic DeleteApplicationInputProcessingConfigurationRequest _
instance showDeleteApplicationInputProcessingConfigurationRequest :: Show DeleteApplicationInputProcessingConfigurationRequest where
  show = genericShow
instance decodeDeleteApplicationInputProcessingConfigurationRequest :: Decode DeleteApplicationInputProcessingConfigurationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationInputProcessingConfigurationRequest :: Encode DeleteApplicationInputProcessingConfigurationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse Types.NoArguments
derive instance newtypeDeleteApplicationInputProcessingConfigurationResponse :: Newtype DeleteApplicationInputProcessingConfigurationResponse _
derive instance repGenericDeleteApplicationInputProcessingConfigurationResponse :: Generic DeleteApplicationInputProcessingConfigurationResponse _
instance showDeleteApplicationInputProcessingConfigurationResponse :: Show DeleteApplicationInputProcessingConfigurationResponse where
  show = genericShow
instance decodeDeleteApplicationInputProcessingConfigurationResponse :: Decode DeleteApplicationInputProcessingConfigurationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationInputProcessingConfigurationResponse :: Encode DeleteApplicationInputProcessingConfigurationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteApplicationOutputRequest = DeleteApplicationOutputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "OutputId" :: (Id)
  }
derive instance newtypeDeleteApplicationOutputRequest :: Newtype DeleteApplicationOutputRequest _
derive instance repGenericDeleteApplicationOutputRequest :: Generic DeleteApplicationOutputRequest _
instance showDeleteApplicationOutputRequest :: Show DeleteApplicationOutputRequest where
  show = genericShow
instance decodeDeleteApplicationOutputRequest :: Decode DeleteApplicationOutputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationOutputRequest :: Encode DeleteApplicationOutputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteApplicationOutputResponse = DeleteApplicationOutputResponse Types.NoArguments
derive instance newtypeDeleteApplicationOutputResponse :: Newtype DeleteApplicationOutputResponse _
derive instance repGenericDeleteApplicationOutputResponse :: Generic DeleteApplicationOutputResponse _
instance showDeleteApplicationOutputResponse :: Show DeleteApplicationOutputResponse where
  show = genericShow
instance decodeDeleteApplicationOutputResponse :: Decode DeleteApplicationOutputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationOutputResponse :: Encode DeleteApplicationOutputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationReferenceDataSourceRequest = DeleteApplicationReferenceDataSourceRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ReferenceId" :: (Id)
  }
derive instance newtypeDeleteApplicationReferenceDataSourceRequest :: Newtype DeleteApplicationReferenceDataSourceRequest _
derive instance repGenericDeleteApplicationReferenceDataSourceRequest :: Generic DeleteApplicationReferenceDataSourceRequest _
instance showDeleteApplicationReferenceDataSourceRequest :: Show DeleteApplicationReferenceDataSourceRequest where
  show = genericShow
instance decodeDeleteApplicationReferenceDataSourceRequest :: Decode DeleteApplicationReferenceDataSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationReferenceDataSourceRequest :: Encode DeleteApplicationReferenceDataSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse Types.NoArguments
derive instance newtypeDeleteApplicationReferenceDataSourceResponse :: Newtype DeleteApplicationReferenceDataSourceResponse _
derive instance repGenericDeleteApplicationReferenceDataSourceResponse :: Generic DeleteApplicationReferenceDataSourceResponse _
instance showDeleteApplicationReferenceDataSourceResponse :: Show DeleteApplicationReferenceDataSourceResponse where
  show = genericShow
instance decodeDeleteApplicationReferenceDataSourceResponse :: Decode DeleteApplicationReferenceDataSourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationReferenceDataSourceResponse :: Encode DeleteApplicationReferenceDataSourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteApplicationRequest = DeleteApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CreateTimestamp" :: (Number)
  }
derive instance newtypeDeleteApplicationRequest :: Newtype DeleteApplicationRequest _
derive instance repGenericDeleteApplicationRequest :: Generic DeleteApplicationRequest _
instance showDeleteApplicationRequest :: Show DeleteApplicationRequest where
  show = genericShow
instance decodeDeleteApplicationRequest :: Decode DeleteApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationRequest :: Encode DeleteApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteApplicationResponse = DeleteApplicationResponse Types.NoArguments
derive instance newtypeDeleteApplicationResponse :: Newtype DeleteApplicationResponse _
derive instance repGenericDeleteApplicationResponse :: Generic DeleteApplicationResponse _
instance showDeleteApplicationResponse :: Show DeleteApplicationResponse where
  show = genericShow
instance decodeDeleteApplicationResponse :: Decode DeleteApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationResponse :: Encode DeleteApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeApplicationRequest = DescribeApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  }
derive instance newtypeDescribeApplicationRequest :: Newtype DescribeApplicationRequest _
derive instance repGenericDescribeApplicationRequest :: Generic DescribeApplicationRequest _
instance showDescribeApplicationRequest :: Show DescribeApplicationRequest where
  show = genericShow
instance decodeDescribeApplicationRequest :: Decode DescribeApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeApplicationRequest :: Encode DescribeApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeApplicationResponse = DescribeApplicationResponse 
  { "ApplicationDetail" :: (ApplicationDetail)
  }
derive instance newtypeDescribeApplicationResponse :: Newtype DescribeApplicationResponse _
derive instance repGenericDescribeApplicationResponse :: Generic DescribeApplicationResponse _
instance showDescribeApplicationResponse :: Show DescribeApplicationResponse where
  show = genericShow
instance decodeDescribeApplicationResponse :: Decode DescribeApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeApplicationResponse :: Encode DescribeApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the data format when records are written to the destination. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Configuring Application Output</a>. </p>
newtype DestinationSchema = DestinationSchema 
  { "RecordFormatType" :: NullOrUndefined.NullOrUndefined (RecordFormatType)
  }
derive instance newtypeDestinationSchema :: Newtype DestinationSchema _
derive instance repGenericDestinationSchema :: Generic DestinationSchema _
instance showDestinationSchema :: Show DestinationSchema where
  show = genericShow
instance decodeDestinationSchema :: Decode DestinationSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationSchema :: Encode DestinationSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiscoverInputSchemaRequest = DiscoverInputSchemaRequest 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "InputStartingPositionConfiguration" :: NullOrUndefined.NullOrUndefined (InputStartingPositionConfiguration)
  , "S3Configuration" :: NullOrUndefined.NullOrUndefined (S3Configuration)
  , "InputProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (InputProcessingConfiguration)
  }
derive instance newtypeDiscoverInputSchemaRequest :: Newtype DiscoverInputSchemaRequest _
derive instance repGenericDiscoverInputSchemaRequest :: Generic DiscoverInputSchemaRequest _
instance showDiscoverInputSchemaRequest :: Show DiscoverInputSchemaRequest where
  show = genericShow
instance decodeDiscoverInputSchemaRequest :: Decode DiscoverInputSchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiscoverInputSchemaRequest :: Encode DiscoverInputSchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DiscoverInputSchemaResponse = DiscoverInputSchemaResponse 
  { "InputSchema" :: NullOrUndefined.NullOrUndefined (SourceSchema)
  , "ParsedInputRecords" :: NullOrUndefined.NullOrUndefined (ParsedInputRecords)
  , "ProcessedInputRecords" :: NullOrUndefined.NullOrUndefined (ProcessedInputRecords)
  , "RawInputRecords" :: NullOrUndefined.NullOrUndefined (RawInputRecords)
  }
derive instance newtypeDiscoverInputSchemaResponse :: Newtype DiscoverInputSchemaResponse _
derive instance repGenericDiscoverInputSchemaResponse :: Generic DiscoverInputSchemaResponse _
instance showDiscoverInputSchemaResponse :: Show DiscoverInputSchemaResponse where
  show = genericShow
instance decodeDiscoverInputSchemaResponse :: Decode DiscoverInputSchemaResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiscoverInputSchemaResponse :: Encode DiscoverInputSchemaResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileKey = FileKey String
derive instance newtypeFileKey :: Newtype FileKey _
derive instance repGenericFileKey :: Generic FileKey _
instance showFileKey :: Show FileKey where
  show = genericShow
instance decodeFileKey :: Decode FileKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileKey :: Encode FileKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Id = Id String
derive instance newtypeId :: Newtype Id _
derive instance repGenericId :: Generic Id _
instance showId :: Show Id where
  show = genericShow
instance decodeId :: Decode Id where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeId :: Encode Id where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InAppStreamName = InAppStreamName String
derive instance newtypeInAppStreamName :: Newtype InAppStreamName _
derive instance repGenericInAppStreamName :: Generic InAppStreamName _
instance showInAppStreamName :: Show InAppStreamName where
  show = genericShow
instance decodeInAppStreamName :: Decode InAppStreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInAppStreamName :: Encode InAppStreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InAppStreamNames = InAppStreamNames (Array InAppStreamName)
derive instance newtypeInAppStreamNames :: Newtype InAppStreamNames _
derive instance repGenericInAppStreamNames :: Generic InAppStreamNames _
instance showInAppStreamNames :: Show InAppStreamNames where
  show = genericShow
instance decodeInAppStreamNames :: Decode InAppStreamNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInAppStreamNames :: Encode InAppStreamNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InAppTableName = InAppTableName String
derive instance newtypeInAppTableName :: Newtype InAppTableName _
derive instance repGenericInAppTableName :: Generic InAppTableName _
instance showInAppTableName :: Show InAppTableName where
  show = genericShow
instance decodeInAppTableName :: Decode InAppTableName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInAppTableName :: Encode InAppTableName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype Input = Input 
  { "NamePrefix" :: (InAppStreamName)
  , "InputProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (InputProcessingConfiguration)
  , "KinesisStreamsInput" :: NullOrUndefined.NullOrUndefined (KinesisStreamsInput)
  , "KinesisFirehoseInput" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseInput)
  , "InputParallelism" :: NullOrUndefined.NullOrUndefined (InputParallelism)
  , "InputSchema" :: (SourceSchema)
  }
derive instance newtypeInput :: Newtype Input _
derive instance repGenericInput :: Generic Input _
instance showInput :: Show Input where
  show = genericShow
instance decodeInput :: Decode Input where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInput :: Encode Input where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.</p>
newtype InputConfiguration = InputConfiguration 
  { "Id" :: (Id)
  , "InputStartingPositionConfiguration" :: (InputStartingPositionConfiguration)
  }
derive instance newtypeInputConfiguration :: Newtype InputConfiguration _
derive instance repGenericInputConfiguration :: Generic InputConfiguration _
instance showInputConfiguration :: Show InputConfiguration where
  show = genericShow
instance decodeInputConfiguration :: Decode InputConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputConfiguration :: Encode InputConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputConfigurations = InputConfigurations (Array InputConfiguration)
derive instance newtypeInputConfigurations :: Newtype InputConfigurations _
derive instance repGenericInputConfigurations :: Generic InputConfigurations _
instance showInputConfigurations :: Show InputConfigurations where
  show = genericShow
instance decodeInputConfigurations :: Decode InputConfigurations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputConfigurations :: Encode InputConfigurations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the application input configuration. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype InputDescription = InputDescription 
  { "InputId" :: NullOrUndefined.NullOrUndefined (Id)
  , "NamePrefix" :: NullOrUndefined.NullOrUndefined (InAppStreamName)
  , "InAppStreamNames" :: NullOrUndefined.NullOrUndefined (InAppStreamNames)
  , "InputProcessingConfigurationDescription" :: NullOrUndefined.NullOrUndefined (InputProcessingConfigurationDescription)
  , "KinesisStreamsInputDescription" :: NullOrUndefined.NullOrUndefined (KinesisStreamsInputDescription)
  , "KinesisFirehoseInputDescription" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseInputDescription)
  , "InputSchema" :: NullOrUndefined.NullOrUndefined (SourceSchema)
  , "InputParallelism" :: NullOrUndefined.NullOrUndefined (InputParallelism)
  , "InputStartingPositionConfiguration" :: NullOrUndefined.NullOrUndefined (InputStartingPositionConfiguration)
  }
derive instance newtypeInputDescription :: Newtype InputDescription _
derive instance repGenericInputDescription :: Generic InputDescription _
instance showInputDescription :: Show InputDescription where
  show = genericShow
instance decodeInputDescription :: Decode InputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDescription :: Encode InputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputDescriptions = InputDescriptions (Array InputDescription)
derive instance newtypeInputDescriptions :: Newtype InputDescriptions _
derive instance repGenericInputDescriptions :: Generic InputDescriptions _
instance showInputDescriptions :: Show InputDescriptions where
  show = genericShow
instance decodeInputDescriptions :: Decode InputDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDescriptions :: Encode InputDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function. </p>
newtype InputLambdaProcessor = InputLambdaProcessor 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeInputLambdaProcessor :: Newtype InputLambdaProcessor _
derive instance repGenericInputLambdaProcessor :: Generic InputLambdaProcessor _
instance showInputLambdaProcessor :: Show InputLambdaProcessor where
  show = genericShow
instance decodeInputLambdaProcessor :: Decode InputLambdaProcessor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLambdaProcessor :: Encode InputLambdaProcessor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.</p>
newtype InputLambdaProcessorDescription = InputLambdaProcessorDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeInputLambdaProcessorDescription :: Newtype InputLambdaProcessorDescription _
derive instance repGenericInputLambdaProcessorDescription :: Generic InputLambdaProcessorDescription _
instance showInputLambdaProcessorDescription :: Show InputLambdaProcessorDescription where
  show = genericShow
instance decodeInputLambdaProcessorDescription :: Decode InputLambdaProcessorDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLambdaProcessorDescription :: Encode InputLambdaProcessorDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an update to the <a>InputLambdaProcessor</a> that is used to preprocess the records in the stream.</p>
newtype InputLambdaProcessorUpdate = InputLambdaProcessorUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeInputLambdaProcessorUpdate :: Newtype InputLambdaProcessorUpdate _
derive instance repGenericInputLambdaProcessorUpdate :: Generic InputLambdaProcessorUpdate _
instance showInputLambdaProcessorUpdate :: Show InputLambdaProcessorUpdate where
  show = genericShow
instance decodeInputLambdaProcessorUpdate :: Decode InputLambdaProcessorUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLambdaProcessorUpdate :: Encode InputLambdaProcessorUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype InputParallelism = InputParallelism 
  { "Count" :: NullOrUndefined.NullOrUndefined (InputParallelismCount)
  }
derive instance newtypeInputParallelism :: Newtype InputParallelism _
derive instance repGenericInputParallelism :: Generic InputParallelism _
instance showInputParallelism :: Show InputParallelism where
  show = genericShow
instance decodeInputParallelism :: Decode InputParallelism where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputParallelism :: Encode InputParallelism where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputParallelismCount = InputParallelismCount Int
derive instance newtypeInputParallelismCount :: Newtype InputParallelismCount _
derive instance repGenericInputParallelismCount :: Generic InputParallelismCount _
instance showInputParallelismCount :: Show InputParallelismCount where
  show = genericShow
instance decodeInputParallelismCount :: Decode InputParallelismCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputParallelismCount :: Encode InputParallelismCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides updates to the parallelism count.</p>
newtype InputParallelismUpdate = InputParallelismUpdate 
  { "CountUpdate" :: NullOrUndefined.NullOrUndefined (InputParallelismCount)
  }
derive instance newtypeInputParallelismUpdate :: Newtype InputParallelismUpdate _
derive instance repGenericInputParallelismUpdate :: Generic InputParallelismUpdate _
instance showInputParallelismUpdate :: Show InputParallelismUpdate where
  show = genericShow
instance decodeInputParallelismUpdate :: Decode InputParallelismUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputParallelismUpdate :: Encode InputParallelismUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
newtype InputProcessingConfiguration = InputProcessingConfiguration 
  { "InputLambdaProcessor" :: (InputLambdaProcessor)
  }
derive instance newtypeInputProcessingConfiguration :: Newtype InputProcessingConfiguration _
derive instance repGenericInputProcessingConfiguration :: Generic InputProcessingConfiguration _
instance showInputProcessingConfiguration :: Show InputProcessingConfiguration where
  show = genericShow
instance decodeInputProcessingConfiguration :: Decode InputProcessingConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputProcessingConfiguration :: Encode InputProcessingConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides configuration information about an input processor. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription 
  { "InputLambdaProcessorDescription" :: NullOrUndefined.NullOrUndefined (InputLambdaProcessorDescription)
  }
derive instance newtypeInputProcessingConfigurationDescription :: Newtype InputProcessingConfigurationDescription _
derive instance repGenericInputProcessingConfigurationDescription :: Generic InputProcessingConfigurationDescription _
instance showInputProcessingConfigurationDescription :: Show InputProcessingConfigurationDescription where
  show = genericShow
instance decodeInputProcessingConfigurationDescription :: Decode InputProcessingConfigurationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputProcessingConfigurationDescription :: Encode InputProcessingConfigurationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes updates to an <a>InputProcessingConfiguration</a>. </p>
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate 
  { "InputLambdaProcessorUpdate" :: (InputLambdaProcessorUpdate)
  }
derive instance newtypeInputProcessingConfigurationUpdate :: Newtype InputProcessingConfigurationUpdate _
derive instance repGenericInputProcessingConfigurationUpdate :: Generic InputProcessingConfigurationUpdate _
instance showInputProcessingConfigurationUpdate :: Show InputProcessingConfigurationUpdate where
  show = genericShow
instance decodeInputProcessingConfigurationUpdate :: Decode InputProcessingConfigurationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputProcessingConfigurationUpdate :: Encode InputProcessingConfigurationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes updates for the application's input schema.</p>
newtype InputSchemaUpdate = InputSchemaUpdate 
  { "RecordFormatUpdate" :: NullOrUndefined.NullOrUndefined (RecordFormat)
  , "RecordEncodingUpdate" :: NullOrUndefined.NullOrUndefined (RecordEncoding)
  , "RecordColumnUpdates" :: NullOrUndefined.NullOrUndefined (RecordColumns)
  }
derive instance newtypeInputSchemaUpdate :: Newtype InputSchemaUpdate _
derive instance repGenericInputSchemaUpdate :: Generic InputSchemaUpdate _
instance showInputSchemaUpdate :: Show InputSchemaUpdate where
  show = genericShow
instance decodeInputSchemaUpdate :: Decode InputSchemaUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSchemaUpdate :: Encode InputSchemaUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputStartingPosition = InputStartingPosition String
derive instance newtypeInputStartingPosition :: Newtype InputStartingPosition _
derive instance repGenericInputStartingPosition :: Generic InputStartingPosition _
instance showInputStartingPosition :: Show InputStartingPosition where
  show = genericShow
instance decodeInputStartingPosition :: Decode InputStartingPosition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputStartingPosition :: Encode InputStartingPosition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the point at which the application reads from the streaming source.</p>
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration 
  { "InputStartingPosition" :: NullOrUndefined.NullOrUndefined (InputStartingPosition)
  }
derive instance newtypeInputStartingPositionConfiguration :: Newtype InputStartingPositionConfiguration _
derive instance repGenericInputStartingPositionConfiguration :: Generic InputStartingPositionConfiguration _
instance showInputStartingPositionConfiguration :: Show InputStartingPositionConfiguration where
  show = genericShow
instance decodeInputStartingPositionConfiguration :: Decode InputStartingPositionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputStartingPositionConfiguration :: Encode InputStartingPositionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes updates to a specific input configuration (identified by the <code>InputId</code> of an application). </p>
newtype InputUpdate = InputUpdate 
  { "InputId" :: (Id)
  , "NamePrefixUpdate" :: NullOrUndefined.NullOrUndefined (InAppStreamName)
  , "InputProcessingConfigurationUpdate" :: NullOrUndefined.NullOrUndefined (InputProcessingConfigurationUpdate)
  , "KinesisStreamsInputUpdate" :: NullOrUndefined.NullOrUndefined (KinesisStreamsInputUpdate)
  , "KinesisFirehoseInputUpdate" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseInputUpdate)
  , "InputSchemaUpdate" :: NullOrUndefined.NullOrUndefined (InputSchemaUpdate)
  , "InputParallelismUpdate" :: NullOrUndefined.NullOrUndefined (InputParallelismUpdate)
  }
derive instance newtypeInputUpdate :: Newtype InputUpdate _
derive instance repGenericInputUpdate :: Generic InputUpdate _
instance showInputUpdate :: Show InputUpdate where
  show = genericShow
instance decodeInputUpdate :: Decode InputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputUpdate :: Encode InputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputUpdates = InputUpdates (Array InputUpdate)
derive instance newtypeInputUpdates :: Newtype InputUpdates _
derive instance repGenericInputUpdates :: Generic InputUpdates _
instance showInputUpdates :: Show InputUpdates where
  show = genericShow
instance decodeInputUpdates :: Decode InputUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputUpdates :: Encode InputUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Inputs = Inputs (Array Input)
derive instance newtypeInputs :: Newtype Inputs _
derive instance repGenericInputs :: Generic Inputs _
instance showInputs :: Show Inputs where
  show = genericShow
instance decodeInputs :: Decode Inputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputs :: Encode Inputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>User-provided application configuration is not valid.</p>
newtype InvalidApplicationConfigurationException = InvalidApplicationConfigurationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidApplicationConfigurationException :: Newtype InvalidApplicationConfigurationException _
derive instance repGenericInvalidApplicationConfigurationException :: Generic InvalidApplicationConfigurationException _
instance showInvalidApplicationConfigurationException :: Show InvalidApplicationConfigurationException where
  show = genericShow
instance decodeInvalidApplicationConfigurationException :: Decode InvalidApplicationConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidApplicationConfigurationException :: Encode InvalidApplicationConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified input parameter value is invalid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _
derive instance repGenericInvalidArgumentException :: Generic InvalidArgumentException _
instance showInvalidArgumentException :: Show InvalidArgumentException where
  show = genericShow
instance decodeInvalidArgumentException :: Decode InvalidArgumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgumentException :: Encode InvalidArgumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides additional mapping information when JSON is the record format on the streaming source.</p>
newtype JSONMappingParameters = JSONMappingParameters 
  { "RecordRowPath" :: (RecordRowPath)
  }
derive instance newtypeJSONMappingParameters :: Newtype JSONMappingParameters _
derive instance repGenericJSONMappingParameters :: Generic JSONMappingParameters _
instance showJSONMappingParameters :: Show JSONMappingParameters where
  show = genericShow
instance decodeJSONMappingParameters :: Decode JSONMappingParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJSONMappingParameters :: Encode JSONMappingParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>
newtype KinesisFirehoseInput = KinesisFirehoseInput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisFirehoseInput :: Newtype KinesisFirehoseInput _
derive instance repGenericKinesisFirehoseInput :: Generic KinesisFirehoseInput _
instance showKinesisFirehoseInput :: Show KinesisFirehoseInput where
  show = genericShow
instance decodeKinesisFirehoseInput :: Decode KinesisFirehoseInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseInput :: Encode KinesisFirehoseInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration. </p>
newtype KinesisFirehoseInputDescription = KinesisFirehoseInputDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseInputDescription :: Newtype KinesisFirehoseInputDescription _
derive instance repGenericKinesisFirehoseInputDescription :: Generic KinesisFirehoseInputDescription _
instance showKinesisFirehoseInputDescription :: Show KinesisFirehoseInputDescription where
  show = genericShow
instance decodeKinesisFirehoseInputDescription :: Decode KinesisFirehoseInputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseInputDescription :: Encode KinesisFirehoseInputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.</p>
newtype KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseInputUpdate :: Newtype KinesisFirehoseInputUpdate _
derive instance repGenericKinesisFirehoseInputUpdate :: Generic KinesisFirehoseInputUpdate _
instance showKinesisFirehoseInputUpdate :: Show KinesisFirehoseInputUpdate where
  show = genericShow
instance decodeKinesisFirehoseInputUpdate :: Decode KinesisFirehoseInputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseInputUpdate :: Encode KinesisFirehoseInputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.</p>
newtype KinesisFirehoseOutput = KinesisFirehoseOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutput :: Newtype KinesisFirehoseOutput _
derive instance repGenericKinesisFirehoseOutput :: Generic KinesisFirehoseOutput _
instance showKinesisFirehoseOutput :: Show KinesisFirehoseOutput where
  show = genericShow
instance decodeKinesisFirehoseOutput :: Decode KinesisFirehoseOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseOutput :: Encode KinesisFirehoseOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination. </p>
newtype KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutputDescription :: Newtype KinesisFirehoseOutputDescription _
derive instance repGenericKinesisFirehoseOutputDescription :: Generic KinesisFirehoseOutputDescription _
instance showKinesisFirehoseOutputDescription :: Show KinesisFirehoseOutputDescription where
  show = genericShow
instance decodeKinesisFirehoseOutputDescription :: Decode KinesisFirehoseOutputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseOutputDescription :: Encode KinesisFirehoseOutputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination. </p>
newtype KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutputUpdate :: Newtype KinesisFirehoseOutputUpdate _
derive instance repGenericKinesisFirehoseOutputUpdate :: Generic KinesisFirehoseOutputUpdate _
instance showKinesisFirehoseOutputUpdate :: Show KinesisFirehoseOutputUpdate where
  show = genericShow
instance decodeKinesisFirehoseOutputUpdate :: Decode KinesisFirehoseOutputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisFirehoseOutputUpdate :: Encode KinesisFirehoseOutputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>
newtype KinesisStreamsInput = KinesisStreamsInput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamsInput :: Newtype KinesisStreamsInput _
derive instance repGenericKinesisStreamsInput :: Generic KinesisStreamsInput _
instance showKinesisStreamsInput :: Show KinesisStreamsInput where
  show = genericShow
instance decodeKinesisStreamsInput :: Decode KinesisStreamsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsInput :: Encode KinesisStreamsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration. </p>
newtype KinesisStreamsInputDescription = KinesisStreamsInputDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsInputDescription :: Newtype KinesisStreamsInputDescription _
derive instance repGenericKinesisStreamsInputDescription :: Generic KinesisStreamsInputDescription _
instance showKinesisStreamsInputDescription :: Show KinesisStreamsInputDescription where
  show = genericShow
instance decodeKinesisStreamsInputDescription :: Decode KinesisStreamsInputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsInputDescription :: Encode KinesisStreamsInputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.</p>
newtype KinesisStreamsInputUpdate = KinesisStreamsInputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsInputUpdate :: Newtype KinesisStreamsInputUpdate _
derive instance repGenericKinesisStreamsInputUpdate :: Generic KinesisStreamsInputUpdate _
instance showKinesisStreamsInputUpdate :: Show KinesisStreamsInputUpdate where
  show = genericShow
instance decodeKinesisStreamsInputUpdate :: Decode KinesisStreamsInputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsInputUpdate :: Encode KinesisStreamsInputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.</p>
newtype KinesisStreamsOutput = KinesisStreamsOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamsOutput :: Newtype KinesisStreamsOutput _
derive instance repGenericKinesisStreamsOutput :: Generic KinesisStreamsOutput _
instance showKinesisStreamsOutput :: Show KinesisStreamsOutput where
  show = genericShow
instance decodeKinesisStreamsOutput :: Decode KinesisStreamsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsOutput :: Encode KinesisStreamsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> For an application output, describes the Amazon Kinesis stream configured as its destination. </p>
newtype KinesisStreamsOutputDescription = KinesisStreamsOutputDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsOutputDescription :: Newtype KinesisStreamsOutputDescription _
derive instance repGenericKinesisStreamsOutputDescription :: Generic KinesisStreamsOutputDescription _
instance showKinesisStreamsOutputDescription :: Show KinesisStreamsOutputDescription where
  show = genericShow
instance decodeKinesisStreamsOutputDescription :: Decode KinesisStreamsOutputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsOutputDescription :: Encode KinesisStreamsOutputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis stream configured as the destination. </p>
newtype KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsOutputUpdate :: Newtype KinesisStreamsOutputUpdate _
derive instance repGenericKinesisStreamsOutputUpdate :: Generic KinesisStreamsOutputUpdate _
instance showKinesisStreamsOutputUpdate :: Show KinesisStreamsOutputUpdate where
  show = genericShow
instance decodeKinesisStreamsOutputUpdate :: Decode KinesisStreamsOutputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamsOutputUpdate :: Encode KinesisStreamsOutputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf. </p>
newtype LambdaOutput = LambdaOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeLambdaOutput :: Newtype LambdaOutput _
derive instance repGenericLambdaOutput :: Generic LambdaOutput _
instance showLambdaOutput :: Show LambdaOutput where
  show = genericShow
instance decodeLambdaOutput :: Decode LambdaOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaOutput :: Encode LambdaOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For an application output, describes the AWS Lambda function configured as its destination. </p>
newtype LambdaOutputDescription = LambdaOutputDescription 
  { "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeLambdaOutputDescription :: Newtype LambdaOutputDescription _
derive instance repGenericLambdaOutputDescription :: Generic LambdaOutputDescription _
instance showLambdaOutputDescription :: Show LambdaOutputDescription where
  show = genericShow
instance decodeLambdaOutputDescription :: Decode LambdaOutputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaOutputDescription :: Encode LambdaOutputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an AWS Lambda function configured as the destination.</p>
newtype LambdaOutputUpdate = LambdaOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeLambdaOutputUpdate :: Newtype LambdaOutputUpdate _
derive instance repGenericLambdaOutputUpdate :: Generic LambdaOutputUpdate _
instance showLambdaOutputUpdate :: Show LambdaOutputUpdate where
  show = genericShow
instance decodeLambdaOutputUpdate :: Decode LambdaOutputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaOutputUpdate :: Encode LambdaOutputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exceeded the number of applications allowed.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListApplicationsInputLimit = ListApplicationsInputLimit Int
derive instance newtypeListApplicationsInputLimit :: Newtype ListApplicationsInputLimit _
derive instance repGenericListApplicationsInputLimit :: Generic ListApplicationsInputLimit _
instance showListApplicationsInputLimit :: Show ListApplicationsInputLimit where
  show = genericShow
instance decodeListApplicationsInputLimit :: Decode ListApplicationsInputLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationsInputLimit :: Encode ListApplicationsInputLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListApplicationsRequest = ListApplicationsRequest 
  { "Limit" :: NullOrUndefined.NullOrUndefined (ListApplicationsInputLimit)
  , "ExclusiveStartApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  }
derive instance newtypeListApplicationsRequest :: Newtype ListApplicationsRequest _
derive instance repGenericListApplicationsRequest :: Generic ListApplicationsRequest _
instance showListApplicationsRequest :: Show ListApplicationsRequest where
  show = genericShow
instance decodeListApplicationsRequest :: Decode ListApplicationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationsRequest :: Encode ListApplicationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListApplicationsResponse = ListApplicationsResponse 
  { "ApplicationSummaries" :: (ApplicationSummaries)
  , "HasMoreApplications" :: (BooleanObject)
  }
derive instance newtypeListApplicationsResponse :: Newtype ListApplicationsResponse _
derive instance repGenericListApplicationsResponse :: Generic ListApplicationsResponse _
instance showListApplicationsResponse :: Show ListApplicationsResponse where
  show = genericShow
instance decodeListApplicationsResponse :: Decode ListApplicationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListApplicationsResponse :: Encode ListApplicationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStreamARN = LogStreamARN String
derive instance newtypeLogStreamARN :: Newtype LogStreamARN _
derive instance repGenericLogStreamARN :: Generic LogStreamARN _
instance showLogStreamARN :: Show LogStreamARN where
  show = genericShow
instance decodeLogStreamARN :: Decode LogStreamARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStreamARN :: Encode LogStreamARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.</p>
newtype MappingParameters = MappingParameters 
  { "JSONMappingParameters" :: NullOrUndefined.NullOrUndefined (JSONMappingParameters)
  , "CSVMappingParameters" :: NullOrUndefined.NullOrUndefined (CSVMappingParameters)
  }
derive instance newtypeMappingParameters :: Newtype MappingParameters _
derive instance repGenericMappingParameters :: Generic MappingParameters _
instance showMappingParameters :: Show MappingParameters where
  show = genericShow
instance decodeMappingParameters :: Decode MappingParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingParameters :: Encode MappingParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p> <p/> <p>For limits on how many destinations an application can write and other limitations, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p>
newtype Output = Output 
  { "Name" :: (InAppStreamName)
  , "KinesisStreamsOutput" :: NullOrUndefined.NullOrUndefined (KinesisStreamsOutput)
  , "KinesisFirehoseOutput" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseOutput)
  , "LambdaOutput" :: NullOrUndefined.NullOrUndefined (LambdaOutput)
  , "DestinationSchema" :: (DestinationSchema)
  }
derive instance newtypeOutput :: Newtype Output _
derive instance repGenericOutput :: Generic Output _
instance showOutput :: Show Output where
  show = genericShow
instance decodeOutput :: Decode Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutput :: Encode Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p>
newtype OutputDescription = OutputDescription 
  { "OutputId" :: NullOrUndefined.NullOrUndefined (Id)
  , "Name" :: NullOrUndefined.NullOrUndefined (InAppStreamName)
  , "KinesisStreamsOutputDescription" :: NullOrUndefined.NullOrUndefined (KinesisStreamsOutputDescription)
  , "KinesisFirehoseOutputDescription" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseOutputDescription)
  , "LambdaOutputDescription" :: NullOrUndefined.NullOrUndefined (LambdaOutputDescription)
  , "DestinationSchema" :: NullOrUndefined.NullOrUndefined (DestinationSchema)
  }
derive instance newtypeOutputDescription :: Newtype OutputDescription _
derive instance repGenericOutputDescription :: Generic OutputDescription _
instance showOutputDescription :: Show OutputDescription where
  show = genericShow
instance decodeOutputDescription :: Decode OutputDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputDescription :: Encode OutputDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputDescriptions = OutputDescriptions (Array OutputDescription)
derive instance newtypeOutputDescriptions :: Newtype OutputDescriptions _
derive instance repGenericOutputDescriptions :: Generic OutputDescriptions _
instance showOutputDescriptions :: Show OutputDescriptions where
  show = genericShow
instance decodeOutputDescriptions :: Decode OutputDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputDescriptions :: Encode OutputDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes updates to the output configuration identified by the <code>OutputId</code>. </p>
newtype OutputUpdate = OutputUpdate 
  { "OutputId" :: (Id)
  , "NameUpdate" :: NullOrUndefined.NullOrUndefined (InAppStreamName)
  , "KinesisStreamsOutputUpdate" :: NullOrUndefined.NullOrUndefined (KinesisStreamsOutputUpdate)
  , "KinesisFirehoseOutputUpdate" :: NullOrUndefined.NullOrUndefined (KinesisFirehoseOutputUpdate)
  , "LambdaOutputUpdate" :: NullOrUndefined.NullOrUndefined (LambdaOutputUpdate)
  , "DestinationSchemaUpdate" :: NullOrUndefined.NullOrUndefined (DestinationSchema)
  }
derive instance newtypeOutputUpdate :: Newtype OutputUpdate _
derive instance repGenericOutputUpdate :: Generic OutputUpdate _
instance showOutputUpdate :: Show OutputUpdate where
  show = genericShow
instance decodeOutputUpdate :: Decode OutputUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputUpdate :: Encode OutputUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputUpdates = OutputUpdates (Array OutputUpdate)
derive instance newtypeOutputUpdates :: Newtype OutputUpdates _
derive instance repGenericOutputUpdates :: Generic OutputUpdates _
instance showOutputUpdates :: Show OutputUpdates where
  show = genericShow
instance decodeOutputUpdates :: Decode OutputUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputUpdates :: Encode OutputUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Outputs = Outputs (Array Output)
derive instance newtypeOutputs :: Newtype Outputs _
derive instance repGenericOutputs :: Generic Outputs _
instance showOutputs :: Show Outputs where
  show = genericShow
instance decodeOutputs :: Decode Outputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputs :: Encode Outputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParsedInputRecord = ParsedInputRecord (Array ParsedInputRecordField)
derive instance newtypeParsedInputRecord :: Newtype ParsedInputRecord _
derive instance repGenericParsedInputRecord :: Generic ParsedInputRecord _
instance showParsedInputRecord :: Show ParsedInputRecord where
  show = genericShow
instance decodeParsedInputRecord :: Decode ParsedInputRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParsedInputRecord :: Encode ParsedInputRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParsedInputRecordField = ParsedInputRecordField String
derive instance newtypeParsedInputRecordField :: Newtype ParsedInputRecordField _
derive instance repGenericParsedInputRecordField :: Generic ParsedInputRecordField _
instance showParsedInputRecordField :: Show ParsedInputRecordField where
  show = genericShow
instance decodeParsedInputRecordField :: Decode ParsedInputRecordField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParsedInputRecordField :: Encode ParsedInputRecordField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParsedInputRecords = ParsedInputRecords (Array ParsedInputRecord)
derive instance newtypeParsedInputRecords :: Newtype ParsedInputRecords _
derive instance repGenericParsedInputRecords :: Generic ParsedInputRecords _
instance showParsedInputRecords :: Show ParsedInputRecords where
  show = genericShow
instance decodeParsedInputRecords :: Decode ParsedInputRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParsedInputRecords :: Encode ParsedInputRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessedInputRecord = ProcessedInputRecord String
derive instance newtypeProcessedInputRecord :: Newtype ProcessedInputRecord _
derive instance repGenericProcessedInputRecord :: Generic ProcessedInputRecord _
instance showProcessedInputRecord :: Show ProcessedInputRecord where
  show = genericShow
instance decodeProcessedInputRecord :: Decode ProcessedInputRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessedInputRecord :: Encode ProcessedInputRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessedInputRecords = ProcessedInputRecords (Array ProcessedInputRecord)
derive instance newtypeProcessedInputRecords :: Newtype ProcessedInputRecords _
derive instance repGenericProcessedInputRecords :: Generic ProcessedInputRecords _
instance showProcessedInputRecords :: Show ProcessedInputRecords where
  show = genericShow
instance decodeProcessedInputRecords :: Decode ProcessedInputRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessedInputRecords :: Encode ProcessedInputRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RawInputRecord = RawInputRecord String
derive instance newtypeRawInputRecord :: Newtype RawInputRecord _
derive instance repGenericRawInputRecord :: Generic RawInputRecord _
instance showRawInputRecord :: Show RawInputRecord where
  show = genericShow
instance decodeRawInputRecord :: Decode RawInputRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRawInputRecord :: Encode RawInputRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RawInputRecords = RawInputRecords (Array RawInputRecord)
derive instance newtypeRawInputRecords :: Newtype RawInputRecords _
derive instance repGenericRawInputRecords :: Generic RawInputRecords _
instance showRawInputRecords :: Show RawInputRecords where
  show = genericShow
instance decodeRawInputRecords :: Decode RawInputRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRawInputRecords :: Encode RawInputRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.</p> <p>Also used to describe the format of the reference data source.</p>
newtype RecordColumn = RecordColumn 
  { "Name" :: (RecordColumnName)
  , "Mapping" :: NullOrUndefined.NullOrUndefined (RecordColumnMapping)
  , "SqlType" :: (RecordColumnSqlType)
  }
derive instance newtypeRecordColumn :: Newtype RecordColumn _
derive instance repGenericRecordColumn :: Generic RecordColumn _
instance showRecordColumn :: Show RecordColumn where
  show = genericShow
instance decodeRecordColumn :: Decode RecordColumn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumn :: Encode RecordColumn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordColumnDelimiter = RecordColumnDelimiter String
derive instance newtypeRecordColumnDelimiter :: Newtype RecordColumnDelimiter _
derive instance repGenericRecordColumnDelimiter :: Generic RecordColumnDelimiter _
instance showRecordColumnDelimiter :: Show RecordColumnDelimiter where
  show = genericShow
instance decodeRecordColumnDelimiter :: Decode RecordColumnDelimiter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumnDelimiter :: Encode RecordColumnDelimiter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordColumnMapping = RecordColumnMapping String
derive instance newtypeRecordColumnMapping :: Newtype RecordColumnMapping _
derive instance repGenericRecordColumnMapping :: Generic RecordColumnMapping _
instance showRecordColumnMapping :: Show RecordColumnMapping where
  show = genericShow
instance decodeRecordColumnMapping :: Decode RecordColumnMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumnMapping :: Encode RecordColumnMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordColumnName = RecordColumnName String
derive instance newtypeRecordColumnName :: Newtype RecordColumnName _
derive instance repGenericRecordColumnName :: Generic RecordColumnName _
instance showRecordColumnName :: Show RecordColumnName where
  show = genericShow
instance decodeRecordColumnName :: Decode RecordColumnName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumnName :: Encode RecordColumnName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordColumnSqlType = RecordColumnSqlType String
derive instance newtypeRecordColumnSqlType :: Newtype RecordColumnSqlType _
derive instance repGenericRecordColumnSqlType :: Generic RecordColumnSqlType _
instance showRecordColumnSqlType :: Show RecordColumnSqlType where
  show = genericShow
instance decodeRecordColumnSqlType :: Decode RecordColumnSqlType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumnSqlType :: Encode RecordColumnSqlType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordColumns = RecordColumns (Array RecordColumn)
derive instance newtypeRecordColumns :: Newtype RecordColumns _
derive instance repGenericRecordColumns :: Generic RecordColumns _
instance showRecordColumns :: Show RecordColumns where
  show = genericShow
instance decodeRecordColumns :: Decode RecordColumns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordColumns :: Encode RecordColumns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordEncoding = RecordEncoding String
derive instance newtypeRecordEncoding :: Newtype RecordEncoding _
derive instance repGenericRecordEncoding :: Generic RecordEncoding _
instance showRecordEncoding :: Show RecordEncoding where
  show = genericShow
instance decodeRecordEncoding :: Decode RecordEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordEncoding :: Encode RecordEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the record format and relevant mapping information that should be applied to schematize the records on the stream. </p>
newtype RecordFormat = RecordFormat 
  { "RecordFormatType" :: (RecordFormatType)
  , "MappingParameters" :: NullOrUndefined.NullOrUndefined (MappingParameters)
  }
derive instance newtypeRecordFormat :: Newtype RecordFormat _
derive instance repGenericRecordFormat :: Generic RecordFormat _
instance showRecordFormat :: Show RecordFormat where
  show = genericShow
instance decodeRecordFormat :: Decode RecordFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordFormat :: Encode RecordFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordFormatType = RecordFormatType String
derive instance newtypeRecordFormatType :: Newtype RecordFormatType _
derive instance repGenericRecordFormatType :: Generic RecordFormatType _
instance showRecordFormatType :: Show RecordFormatType where
  show = genericShow
instance decodeRecordFormatType :: Decode RecordFormatType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordFormatType :: Encode RecordFormatType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordRowDelimiter = RecordRowDelimiter String
derive instance newtypeRecordRowDelimiter :: Newtype RecordRowDelimiter _
derive instance repGenericRecordRowDelimiter :: Generic RecordRowDelimiter _
instance showRecordRowDelimiter :: Show RecordRowDelimiter where
  show = genericShow
instance decodeRecordRowDelimiter :: Decode RecordRowDelimiter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordRowDelimiter :: Encode RecordRowDelimiter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordRowPath = RecordRowPath String
derive instance newtypeRecordRowPath :: Newtype RecordRowPath _
derive instance repGenericRecordRowPath :: Generic RecordRowPath _
instance showRecordRowPath :: Show RecordRowPath where
  show = genericShow
instance decodeRecordRowPath :: Decode RecordRowPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordRowPath :: Encode RecordRowPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.</p>
newtype ReferenceDataSource = ReferenceDataSource 
  { "TableName" :: (InAppTableName)
  , "S3ReferenceDataSource" :: NullOrUndefined.NullOrUndefined (S3ReferenceDataSource)
  , "ReferenceSchema" :: (SourceSchema)
  }
derive instance newtypeReferenceDataSource :: Newtype ReferenceDataSource _
derive instance repGenericReferenceDataSource :: Generic ReferenceDataSource _
instance showReferenceDataSource :: Show ReferenceDataSource where
  show = genericShow
instance decodeReferenceDataSource :: Decode ReferenceDataSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDataSource :: Encode ReferenceDataSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the reference data source configured for an application.</p>
newtype ReferenceDataSourceDescription = ReferenceDataSourceDescription 
  { "ReferenceId" :: (Id)
  , "TableName" :: (InAppTableName)
  , "S3ReferenceDataSourceDescription" :: (S3ReferenceDataSourceDescription)
  , "ReferenceSchema" :: NullOrUndefined.NullOrUndefined (SourceSchema)
  }
derive instance newtypeReferenceDataSourceDescription :: Newtype ReferenceDataSourceDescription _
derive instance repGenericReferenceDataSourceDescription :: Generic ReferenceDataSourceDescription _
instance showReferenceDataSourceDescription :: Show ReferenceDataSourceDescription where
  show = genericShow
instance decodeReferenceDataSourceDescription :: Decode ReferenceDataSourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDataSourceDescription :: Encode ReferenceDataSourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReferenceDataSourceDescriptions = ReferenceDataSourceDescriptions (Array ReferenceDataSourceDescription)
derive instance newtypeReferenceDataSourceDescriptions :: Newtype ReferenceDataSourceDescriptions _
derive instance repGenericReferenceDataSourceDescriptions :: Generic ReferenceDataSourceDescriptions _
instance showReferenceDataSourceDescriptions :: Show ReferenceDataSourceDescriptions where
  show = genericShow
instance decodeReferenceDataSourceDescriptions :: Decode ReferenceDataSourceDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDataSourceDescriptions :: Encode ReferenceDataSourceDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.</p>
newtype ReferenceDataSourceUpdate = ReferenceDataSourceUpdate 
  { "ReferenceId" :: (Id)
  , "TableNameUpdate" :: NullOrUndefined.NullOrUndefined (InAppTableName)
  , "S3ReferenceDataSourceUpdate" :: NullOrUndefined.NullOrUndefined (S3ReferenceDataSourceUpdate)
  , "ReferenceSchemaUpdate" :: NullOrUndefined.NullOrUndefined (SourceSchema)
  }
derive instance newtypeReferenceDataSourceUpdate :: Newtype ReferenceDataSourceUpdate _
derive instance repGenericReferenceDataSourceUpdate :: Generic ReferenceDataSourceUpdate _
instance showReferenceDataSourceUpdate :: Show ReferenceDataSourceUpdate where
  show = genericShow
instance decodeReferenceDataSourceUpdate :: Decode ReferenceDataSourceUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDataSourceUpdate :: Encode ReferenceDataSourceUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReferenceDataSourceUpdates = ReferenceDataSourceUpdates (Array ReferenceDataSourceUpdate)
derive instance newtypeReferenceDataSourceUpdates :: Newtype ReferenceDataSourceUpdates _
derive instance repGenericReferenceDataSourceUpdates :: Generic ReferenceDataSourceUpdates _
instance showReferenceDataSourceUpdates :: Show ReferenceDataSourceUpdates where
  show = genericShow
instance decodeReferenceDataSourceUpdates :: Decode ReferenceDataSourceUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReferenceDataSourceUpdates :: Encode ReferenceDataSourceUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Application is not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specified application can't be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html">GetRecords</a> in the Amazon Kinesis Streams API Reference.</p>
newtype ResourceProvisionedThroughputExceededException = ResourceProvisionedThroughputExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceProvisionedThroughputExceededException :: Newtype ResourceProvisionedThroughputExceededException _
derive instance repGenericResourceProvisionedThroughputExceededException :: Generic ResourceProvisionedThroughputExceededException _
instance showResourceProvisionedThroughputExceededException :: Show ResourceProvisionedThroughputExceededException where
  show = genericShow
instance decodeResourceProvisionedThroughputExceededException :: Decode ResourceProvisionedThroughputExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceProvisionedThroughputExceededException :: Encode ResourceProvisionedThroughputExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _
derive instance repGenericRoleARN :: Generic RoleARN _
instance showRoleARN :: Show RoleARN where
  show = genericShow
instance decodeRoleARN :: Decode RoleARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleARN :: Encode RoleARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the S3 object that contains the data.</p>
newtype S3Configuration = S3Configuration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  }
derive instance newtypeS3Configuration :: Newtype S3Configuration _
derive instance repGenericS3Configuration :: Generic S3Configuration _
instance showS3Configuration :: Show S3Configuration where
  show = genericShow
instance decodeS3Configuration :: Decode S3Configuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Configuration :: Encode S3Configuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.</p> <p>An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <a>UpdateApplication</a> operation to trigger reloading of data into your application.</p>
newtype S3ReferenceDataSource = S3ReferenceDataSource 
  { "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  , "ReferenceRoleARN" :: (RoleARN)
  }
derive instance newtypeS3ReferenceDataSource :: Newtype S3ReferenceDataSource _
derive instance repGenericS3ReferenceDataSource :: Generic S3ReferenceDataSource _
instance showS3ReferenceDataSource :: Show S3ReferenceDataSource where
  show = genericShow
instance decodeS3ReferenceDataSource :: Decode S3ReferenceDataSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ReferenceDataSource :: Encode S3ReferenceDataSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the bucket name and object key name that stores the reference data.</p>
newtype S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription 
  { "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  , "ReferenceRoleARN" :: (RoleARN)
  }
derive instance newtypeS3ReferenceDataSourceDescription :: Newtype S3ReferenceDataSourceDescription _
derive instance repGenericS3ReferenceDataSourceDescription :: Generic S3ReferenceDataSourceDescription _
instance showS3ReferenceDataSourceDescription :: Show S3ReferenceDataSourceDescription where
  show = genericShow
instance decodeS3ReferenceDataSourceDescription :: Decode S3ReferenceDataSourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ReferenceDataSourceDescription :: Encode S3ReferenceDataSourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.</p>
newtype S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate 
  { "BucketARNUpdate" :: NullOrUndefined.NullOrUndefined (BucketARN)
  , "FileKeyUpdate" :: NullOrUndefined.NullOrUndefined (FileKey)
  , "ReferenceRoleARNUpdate" :: NullOrUndefined.NullOrUndefined (RoleARN)
  }
derive instance newtypeS3ReferenceDataSourceUpdate :: Newtype S3ReferenceDataSourceUpdate _
derive instance repGenericS3ReferenceDataSourceUpdate :: Generic S3ReferenceDataSourceUpdate _
instance showS3ReferenceDataSourceUpdate :: Show S3ReferenceDataSourceUpdate where
  show = genericShow
instance decodeS3ReferenceDataSourceUpdate :: Decode S3ReferenceDataSourceUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ReferenceDataSourceUpdate :: Encode S3ReferenceDataSourceUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is unavailable, back off and retry the operation. </p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.</p>
newtype SourceSchema = SourceSchema 
  { "RecordFormat" :: (RecordFormat)
  , "RecordEncoding" :: NullOrUndefined.NullOrUndefined (RecordEncoding)
  , "RecordColumns" :: (RecordColumns)
  }
derive instance newtypeSourceSchema :: Newtype SourceSchema _
derive instance repGenericSourceSchema :: Generic SourceSchema _
instance showSourceSchema :: Show SourceSchema where
  show = genericShow
instance decodeSourceSchema :: Decode SourceSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceSchema :: Encode SourceSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StartApplicationRequest = StartApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "InputConfigurations" :: (InputConfigurations)
  }
derive instance newtypeStartApplicationRequest :: Newtype StartApplicationRequest _
derive instance repGenericStartApplicationRequest :: Generic StartApplicationRequest _
instance showStartApplicationRequest :: Show StartApplicationRequest where
  show = genericShow
instance decodeStartApplicationRequest :: Decode StartApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartApplicationRequest :: Encode StartApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StartApplicationResponse = StartApplicationResponse Types.NoArguments
derive instance newtypeStartApplicationResponse :: Newtype StartApplicationResponse _
derive instance repGenericStartApplicationResponse :: Generic StartApplicationResponse _
instance showStartApplicationResponse :: Show StartApplicationResponse where
  show = genericShow
instance decodeStartApplicationResponse :: Decode StartApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartApplicationResponse :: Encode StartApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StopApplicationRequest = StopApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  }
derive instance newtypeStopApplicationRequest :: Newtype StopApplicationRequest _
derive instance repGenericStopApplicationRequest :: Generic StopApplicationRequest _
instance showStopApplicationRequest :: Show StopApplicationRequest where
  show = genericShow
instance decodeStopApplicationRequest :: Decode StopApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopApplicationRequest :: Encode StopApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StopApplicationResponse = StopApplicationResponse Types.NoArguments
derive instance newtypeStopApplicationResponse :: Newtype StopApplicationResponse _
derive instance repGenericStopApplicationResponse :: Generic StopApplicationResponse _
instance showStopApplicationResponse :: Show StopApplicationResponse where
  show = genericShow
instance decodeStopApplicationResponse :: Decode StopApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopApplicationResponse :: Encode StopApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Data format is not valid, Amazon Kinesis Analytics is not able to detect schema for the given streaming source.</p>
newtype UnableToDetectSchemaException = UnableToDetectSchemaException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "RawInputRecords" :: NullOrUndefined.NullOrUndefined (RawInputRecords)
  , "ProcessedInputRecords" :: NullOrUndefined.NullOrUndefined (ProcessedInputRecords)
  }
derive instance newtypeUnableToDetectSchemaException :: Newtype UnableToDetectSchemaException _
derive instance repGenericUnableToDetectSchemaException :: Generic UnableToDetectSchemaException _
instance showUnableToDetectSchemaException :: Show UnableToDetectSchemaException where
  show = genericShow
instance decodeUnableToDetectSchemaException :: Decode UnableToDetectSchemaException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnableToDetectSchemaException :: Encode UnableToDetectSchemaException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ApplicationUpdate" :: (ApplicationUpdate)
  }
derive instance newtypeUpdateApplicationRequest :: Newtype UpdateApplicationRequest _
derive instance repGenericUpdateApplicationRequest :: Generic UpdateApplicationRequest _
instance showUpdateApplicationRequest :: Show UpdateApplicationRequest where
  show = genericShow
instance decodeUpdateApplicationRequest :: Decode UpdateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationRequest :: Encode UpdateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationResponse = UpdateApplicationResponse Types.NoArguments
derive instance newtypeUpdateApplicationResponse :: Newtype UpdateApplicationResponse _
derive instance repGenericUpdateApplicationResponse :: Generic UpdateApplicationResponse _
instance showUpdateApplicationResponse :: Show UpdateApplicationResponse where
  show = genericShow
instance decodeUpdateApplicationResponse :: Decode UpdateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationResponse :: Encode UpdateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
