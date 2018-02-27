

module AWS.KinesisAnalytics where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "KinesisAnalytics" :: String


-- | <p>Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>
addApplicationCloudWatchLoggingOption :: forall eff. AddApplicationCloudWatchLoggingOptionRequest -> Aff (err :: AWS.RequestError | eff) AddApplicationCloudWatchLoggingOptionResponse
addApplicationCloudWatchLoggingOption = AWS.request serviceName "addApplicationCloudWatchLoggingOption" 


-- | <p> Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p> <p>You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see <a>CreateApplication</a>.</p> <p>Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationInput</code> action.</p>
addApplicationInput :: forall eff. AddApplicationInputRequest -> Aff (err :: AWS.RequestError | eff) AddApplicationInputResponse
addApplicationInput = AWS.request serviceName "addApplicationInput" 


-- | <p>Adds an <a>InputProcessingConfiguration</a> to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
addApplicationInputProcessingConfiguration :: forall eff. AddApplicationInputProcessingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) AddApplicationInputProcessingConfigurationResponse
addApplicationInputProcessingConfiguration = AWS.request serviceName "addApplicationInputProcessingConfiguration" 


-- | <p>Adds an external destination to your Amazon Kinesis Analytics application.</p> <p>If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.</p> <p> You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Understanding Application Output (Destination)</a>. </p> <p> Note that any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version.</p> <p>For the limits on the number of application inputs and outputs you can configure, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action.</p>
addApplicationOutput :: forall eff. AddApplicationOutputRequest -> Aff (err :: AWS.RequestError | eff) AddApplicationOutputResponse
addApplicationOutput = AWS.request serviceName "addApplicationOutput" 


-- | <p>Adds a reference data source to an existing application.</p> <p>Amazon Kinesis Analytics reads reference data (that is, an Amazon S3 object) and creates an in-application table within your application. In the request, you provide the source (S3 bucket name and object key name), name of the in-application table to create, and the necessary mapping information that describes how data in Amazon S3 object maps to columns in the resulting in-application table.</p> <p> For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. For the limits on data sources you can add to your application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action. </p>
addApplicationReferenceDataSource :: forall eff. AddApplicationReferenceDataSourceRequest -> Aff (err :: AWS.RequestError | eff) AddApplicationReferenceDataSourceResponse
addApplicationReferenceDataSource = AWS.request serviceName "addApplicationReferenceDataSource" 


-- | <p> Creates an Amazon Kinesis Analytics application. You can configure each application with one streaming source as input, application code to process the input, and up to three destinations where you want Amazon Kinesis Analytics to write the output data from your application. For an overview, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html">How it Works</a>. </p> <p>In the input configuration, you map the streaming source to an in-application stream, which you can think of as a constantly updating table. In the mapping, you must provide a schema for the in-application stream and map each data column in the in-application stream to a data element in the streaming source.</p> <p>Your application code is one or more SQL statements that read input data, transform it, and generate output. Your application code can create one or more SQL artifacts like SQL streams or pumps.</p> <p>In the output configuration, you can configure the application to write data from in-application streams created in your applications to up to three destinations.</p> <p> To read data from your source stream or write data to destination streams, Amazon Kinesis Analytics needs your permissions. You grant these permissions by creating IAM roles. This operation requires permissions to perform the <code>kinesisanalytics:CreateApplication</code> action. </p> <p> For introductory exercises to create an Amazon Kinesis Analytics application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html">Getting Started</a>. </p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: AWS.RequestError | eff) CreateApplicationResponse
createApplication = AWS.request serviceName "createApplication" 


-- | <p>Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplication</code> action.</p>
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationResponse
deleteApplication = AWS.request serviceName "deleteApplication" 


-- | <p>Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>
deleteApplicationCloudWatchLoggingOption :: forall eff. DeleteApplicationCloudWatchLoggingOptionRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationCloudWatchLoggingOptionResponse
deleteApplicationCloudWatchLoggingOption = AWS.request serviceName "deleteApplicationCloudWatchLoggingOption" 


-- | <p>Deletes an <a>InputProcessingConfiguration</a> from an input.</p>
deleteApplicationInputProcessingConfiguration :: forall eff. DeleteApplicationInputProcessingConfigurationRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationInputProcessingConfigurationResponse
deleteApplicationInputProcessingConfiguration = AWS.request serviceName "deleteApplicationInputProcessingConfiguration" 


-- | <p>Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplicationOutput</code> action.</p>
deleteApplicationOutput :: forall eff. DeleteApplicationOutputRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationOutputResponse
deleteApplicationOutput = AWS.request serviceName "deleteApplicationOutput" 


-- | <p>Deletes a reference data source configuration from the specified application configuration.</p> <p>If the application is running, Amazon Kinesis Analytics immediately removes the in-application table that you created using the <a>AddApplicationReferenceDataSource</a> operation. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics.DeleteApplicationReferenceDataSource</code> action.</p>
deleteApplicationReferenceDataSource :: forall eff. DeleteApplicationReferenceDataSourceRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationReferenceDataSourceResponse
deleteApplicationReferenceDataSource = AWS.request serviceName "deleteApplicationReferenceDataSource" 


-- | <p>Returns information about a specific Amazon Kinesis Analytics application.</p> <p>If you want to retrieve a list of all applications in your account, use the <a>ListApplications</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DescribeApplication</code> action. You can use <code>DescribeApplication</code> to get the current application versionId, which you need to call other operations such as <code>Update</code>. </p>
describeApplication :: forall eff. DescribeApplicationRequest -> Aff (err :: AWS.RequestError | eff) DescribeApplicationResponse
describeApplication = AWS.request serviceName "describeApplication" 


-- | <p>Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream) or S3 object. In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.</p> <p> You can use the inferred schema when configuring a streaming source for your application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. Note that when you create an application using the Amazon Kinesis Analytics console, the console uses this operation to infer a schema and show it in the console user interface. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:DiscoverInputSchema</code> action. </p>
discoverInputSchema :: forall eff. DiscoverInputSchemaRequest -> Aff (err :: AWS.RequestError | eff) DiscoverInputSchemaResponse
discoverInputSchema = AWS.request serviceName "discoverInputSchema" 


-- | <p>Returns a list of Amazon Kinesis Analytics applications in your account. For each application, the response includes the application name, Amazon Resource Name (ARN), and status. If the response returns the <code>HasMoreApplications</code> value as true, you can send another request by adding the <code>ExclusiveStartApplicationName</code> in the request body, and set the value of this to the last application name from the previous response. </p> <p>If you want detailed information about a specific application, use <a>DescribeApplication</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:ListApplications</code> action.</p>
listApplications :: forall eff. ListApplicationsRequest -> Aff (err :: AWS.RequestError | eff) ListApplicationsResponse
listApplications = AWS.request serviceName "listApplications" 


-- | <p>Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.</p> <p>After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.</p> <p> The application status must be <code>READY</code> for you to start an application. You can get the application status in the console or using the <a>DescribeApplication</a> operation.</p> <p>After you start the application, you can stop the application from processing the input by calling the <a>StopApplication</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StartApplication</code> action.</p>
startApplication :: forall eff. StartApplicationRequest -> Aff (err :: AWS.RequestError | eff) StartApplicationResponse
startApplication = AWS.request serviceName "startApplication" 


-- | <p>Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the <a>DescribeApplication</a> operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StopApplication</code> action.</p>
stopApplication :: forall eff. StopApplicationRequest -> Aff (err :: AWS.RequestError | eff) StopApplicationResponse
stopApplication = AWS.request serviceName "stopApplication" 


-- | <p>Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration. </p> <p>Note that Amazon Kinesis Analytics updates the <code>CurrentApplicationVersionId</code> each time you update your application. </p> <p>This operation requires permission for the <code>kinesisanalytics:UpdateApplication</code> action.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: AWS.RequestError | eff) UpdateApplicationResponse
updateApplication = AWS.request serviceName "updateApplication" 


newtype AddApplicationCloudWatchLoggingOptionRequest = AddApplicationCloudWatchLoggingOptionRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "CloudWatchLoggingOption" :: (CloudWatchLoggingOption)
  }
derive instance newtypeAddApplicationCloudWatchLoggingOptionRequest :: Newtype AddApplicationCloudWatchLoggingOptionRequest _


newtype AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse 
  { 
  }
derive instance newtypeAddApplicationCloudWatchLoggingOptionResponse :: Newtype AddApplicationCloudWatchLoggingOptionResponse _


newtype AddApplicationInputProcessingConfigurationRequest = AddApplicationInputProcessingConfigurationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "InputId" :: (Id)
  , "InputProcessingConfiguration" :: (InputProcessingConfiguration)
  }
derive instance newtypeAddApplicationInputProcessingConfigurationRequest :: Newtype AddApplicationInputProcessingConfigurationRequest _


newtype AddApplicationInputProcessingConfigurationResponse = AddApplicationInputProcessingConfigurationResponse 
  { 
  }
derive instance newtypeAddApplicationInputProcessingConfigurationResponse :: Newtype AddApplicationInputProcessingConfigurationResponse _


-- | <p/>
newtype AddApplicationInputRequest = AddApplicationInputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "Input" :: (Input)
  }
derive instance newtypeAddApplicationInputRequest :: Newtype AddApplicationInputRequest _


-- | <p/>
newtype AddApplicationInputResponse = AddApplicationInputResponse 
  { 
  }
derive instance newtypeAddApplicationInputResponse :: Newtype AddApplicationInputResponse _


-- | <p/>
newtype AddApplicationOutputRequest = AddApplicationOutputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "Output" :: (Output)
  }
derive instance newtypeAddApplicationOutputRequest :: Newtype AddApplicationOutputRequest _


-- | <p/>
newtype AddApplicationOutputResponse = AddApplicationOutputResponse 
  { 
  }
derive instance newtypeAddApplicationOutputResponse :: Newtype AddApplicationOutputResponse _


-- | <p/>
newtype AddApplicationReferenceDataSourceRequest = AddApplicationReferenceDataSourceRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ReferenceDataSource" :: (ReferenceDataSource)
  }
derive instance newtypeAddApplicationReferenceDataSourceRequest :: Newtype AddApplicationReferenceDataSourceRequest _


-- | <p/>
newtype AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse 
  { 
  }
derive instance newtypeAddApplicationReferenceDataSourceResponse :: Newtype AddApplicationReferenceDataSourceResponse _


newtype ApplicationCode = ApplicationCode String
derive instance newtypeApplicationCode :: Newtype ApplicationCode _


newtype ApplicationDescription = ApplicationDescription String
derive instance newtypeApplicationDescription :: Newtype ApplicationDescription _


-- | <p>Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.</p>
newtype ApplicationDetail = ApplicationDetail 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationDescription" :: NullOrUndefined (ApplicationDescription)
  , "ApplicationARN" :: (ResourceARN)
  , "ApplicationStatus" :: (ApplicationStatus)
  , "CreateTimestamp" :: NullOrUndefined (Number)
  , "LastUpdateTimestamp" :: NullOrUndefined (Number)
  , "InputDescriptions" :: NullOrUndefined (InputDescriptions)
  , "OutputDescriptions" :: NullOrUndefined (OutputDescriptions)
  , "ReferenceDataSourceDescriptions" :: NullOrUndefined (ReferenceDataSourceDescriptions)
  , "CloudWatchLoggingOptionDescriptions" :: NullOrUndefined (CloudWatchLoggingOptionDescriptions)
  , "ApplicationCode" :: NullOrUndefined (ApplicationCode)
  , "ApplicationVersionId" :: (ApplicationVersionId)
  }
derive instance newtypeApplicationDetail :: Newtype ApplicationDetail _


newtype ApplicationName = ApplicationName String
derive instance newtypeApplicationName :: Newtype ApplicationName _


newtype ApplicationStatus = ApplicationStatus String
derive instance newtypeApplicationStatus :: Newtype ApplicationStatus _


newtype ApplicationSummaries = ApplicationSummaries (Array ApplicationSummary)
derive instance newtypeApplicationSummaries :: Newtype ApplicationSummaries _


-- | <p>Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.</p>
newtype ApplicationSummary = ApplicationSummary 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationARN" :: (ResourceARN)
  , "ApplicationStatus" :: (ApplicationStatus)
  }
derive instance newtypeApplicationSummary :: Newtype ApplicationSummary _


-- | <p>Describes updates to apply to an existing Amazon Kinesis Analytics application.</p>
newtype ApplicationUpdate = ApplicationUpdate 
  { "InputUpdates" :: NullOrUndefined (InputUpdates)
  , "ApplicationCodeUpdate" :: NullOrUndefined (ApplicationCode)
  , "OutputUpdates" :: NullOrUndefined (OutputUpdates)
  , "ReferenceDataSourceUpdates" :: NullOrUndefined (ReferenceDataSourceUpdates)
  , "CloudWatchLoggingOptionUpdates" :: NullOrUndefined (CloudWatchLoggingOptionUpdates)
  }
derive instance newtypeApplicationUpdate :: Newtype ApplicationUpdate _


newtype ApplicationVersionId = ApplicationVersionId Number
derive instance newtypeApplicationVersionId :: Newtype ApplicationVersionId _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


newtype BucketARN = BucketARN String
derive instance newtypeBucketARN :: Newtype BucketARN _


-- | <p>Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the <i>'\n'</i> as the row delimiter and a comma (",") as the column delimiter: </p> <p> <code>"name1", "address1" </code> </p> <p> <code>"name2, "address2"</code> </p>
newtype CSVMappingParameters = CSVMappingParameters 
  { "RecordRowDelimiter" :: (RecordRowDelimiter)
  , "RecordColumnDelimiter" :: (RecordColumnDelimiter)
  }
derive instance newtypeCSVMappingParameters :: Newtype CSVMappingParameters _


-- | <p>Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.</p>
newtype CloudWatchLoggingOption = CloudWatchLoggingOption 
  { "LogStreamARN" :: (LogStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOption :: Newtype CloudWatchLoggingOption _


-- | <p>Description of the CloudWatch logging option.</p>
newtype CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription 
  { "CloudWatchLoggingOptionId" :: NullOrUndefined (Id)
  , "LogStreamARN" :: (LogStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOptionDescription :: Newtype CloudWatchLoggingOptionDescription _


newtype CloudWatchLoggingOptionDescriptions = CloudWatchLoggingOptionDescriptions (Array CloudWatchLoggingOptionDescription)
derive instance newtypeCloudWatchLoggingOptionDescriptions :: Newtype CloudWatchLoggingOptionDescriptions _


-- | <p>Describes CloudWatch logging option updates.</p>
newtype CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate 
  { "CloudWatchLoggingOptionId" :: (Id)
  , "LogStreamARNUpdate" :: NullOrUndefined (LogStreamARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeCloudWatchLoggingOptionUpdate :: Newtype CloudWatchLoggingOptionUpdate _


newtype CloudWatchLoggingOptionUpdates = CloudWatchLoggingOptionUpdates (Array CloudWatchLoggingOptionUpdate)
derive instance newtypeCloudWatchLoggingOptionUpdates :: Newtype CloudWatchLoggingOptionUpdates _


newtype CloudWatchLoggingOptions = CloudWatchLoggingOptions (Array CloudWatchLoggingOption)
derive instance newtypeCloudWatchLoggingOptions :: Newtype CloudWatchLoggingOptions _


-- | <p>User-provided application code (query) is invalid. This can be a simple syntax error.</p>
newtype CodeValidationException = CodeValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCodeValidationException :: Newtype CodeValidationException _


-- | <p>Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>TBD</p>
newtype CreateApplicationRequest = CreateApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "ApplicationDescription" :: NullOrUndefined (ApplicationDescription)
  , "Inputs" :: NullOrUndefined (Inputs)
  , "Outputs" :: NullOrUndefined (Outputs)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  , "ApplicationCode" :: NullOrUndefined (ApplicationCode)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _


-- | <p>TBD</p>
newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ApplicationSummary" :: (ApplicationSummary)
  }
derive instance newtypeCreateApplicationResponse :: Newtype CreateApplicationResponse _


newtype DeleteApplicationCloudWatchLoggingOptionRequest = DeleteApplicationCloudWatchLoggingOptionRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "CloudWatchLoggingOptionId" :: (Id)
  }
derive instance newtypeDeleteApplicationCloudWatchLoggingOptionRequest :: Newtype DeleteApplicationCloudWatchLoggingOptionRequest _


newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse 
  { 
  }
derive instance newtypeDeleteApplicationCloudWatchLoggingOptionResponse :: Newtype DeleteApplicationCloudWatchLoggingOptionResponse _


newtype DeleteApplicationInputProcessingConfigurationRequest = DeleteApplicationInputProcessingConfigurationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "InputId" :: (Id)
  }
derive instance newtypeDeleteApplicationInputProcessingConfigurationRequest :: Newtype DeleteApplicationInputProcessingConfigurationRequest _


newtype DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse 
  { 
  }
derive instance newtypeDeleteApplicationInputProcessingConfigurationResponse :: Newtype DeleteApplicationInputProcessingConfigurationResponse _


-- | <p/>
newtype DeleteApplicationOutputRequest = DeleteApplicationOutputRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "OutputId" :: (Id)
  }
derive instance newtypeDeleteApplicationOutputRequest :: Newtype DeleteApplicationOutputRequest _


-- | <p/>
newtype DeleteApplicationOutputResponse = DeleteApplicationOutputResponse 
  { 
  }
derive instance newtypeDeleteApplicationOutputResponse :: Newtype DeleteApplicationOutputResponse _


newtype DeleteApplicationReferenceDataSourceRequest = DeleteApplicationReferenceDataSourceRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ReferenceId" :: (Id)
  }
derive instance newtypeDeleteApplicationReferenceDataSourceRequest :: Newtype DeleteApplicationReferenceDataSourceRequest _


newtype DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse 
  { 
  }
derive instance newtypeDeleteApplicationReferenceDataSourceResponse :: Newtype DeleteApplicationReferenceDataSourceResponse _


-- | <p/>
newtype DeleteApplicationRequest = DeleteApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CreateTimestamp" :: (Number)
  }
derive instance newtypeDeleteApplicationRequest :: Newtype DeleteApplicationRequest _


-- | <p/>
newtype DeleteApplicationResponse = DeleteApplicationResponse 
  { 
  }
derive instance newtypeDeleteApplicationResponse :: Newtype DeleteApplicationResponse _


-- | <p/>
newtype DescribeApplicationRequest = DescribeApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  }
derive instance newtypeDescribeApplicationRequest :: Newtype DescribeApplicationRequest _


-- | <p/>
newtype DescribeApplicationResponse = DescribeApplicationResponse 
  { "ApplicationDetail" :: (ApplicationDetail)
  }
derive instance newtypeDescribeApplicationResponse :: Newtype DescribeApplicationResponse _


-- | <p>Describes the data format when records are written to the destination. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Configuring Application Output</a>. </p>
newtype DestinationSchema = DestinationSchema 
  { "RecordFormatType" :: NullOrUndefined (RecordFormatType)
  }
derive instance newtypeDestinationSchema :: Newtype DestinationSchema _


newtype DiscoverInputSchemaRequest = DiscoverInputSchemaRequest 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "InputStartingPositionConfiguration" :: NullOrUndefined (InputStartingPositionConfiguration)
  , "S3Configuration" :: NullOrUndefined (S3Configuration)
  , "InputProcessingConfiguration" :: NullOrUndefined (InputProcessingConfiguration)
  }
derive instance newtypeDiscoverInputSchemaRequest :: Newtype DiscoverInputSchemaRequest _


-- | <p/>
newtype DiscoverInputSchemaResponse = DiscoverInputSchemaResponse 
  { "InputSchema" :: NullOrUndefined (SourceSchema)
  , "ParsedInputRecords" :: NullOrUndefined (ParsedInputRecords)
  , "ProcessedInputRecords" :: NullOrUndefined (ProcessedInputRecords)
  , "RawInputRecords" :: NullOrUndefined (RawInputRecords)
  }
derive instance newtypeDiscoverInputSchemaResponse :: Newtype DiscoverInputSchemaResponse _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype FileKey = FileKey String
derive instance newtypeFileKey :: Newtype FileKey _


newtype Id = Id String
derive instance newtypeId :: Newtype Id _


newtype InAppStreamName = InAppStreamName String
derive instance newtypeInAppStreamName :: Newtype InAppStreamName _


newtype InAppStreamNames = InAppStreamNames (Array InAppStreamName)
derive instance newtypeInAppStreamNames :: Newtype InAppStreamNames _


newtype InAppTableName = InAppTableName String
derive instance newtypeInAppTableName :: Newtype InAppTableName _


-- | <p>When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype Input = Input 
  { "NamePrefix" :: (InAppStreamName)
  , "InputProcessingConfiguration" :: NullOrUndefined (InputProcessingConfiguration)
  , "KinesisStreamsInput" :: NullOrUndefined (KinesisStreamsInput)
  , "KinesisFirehoseInput" :: NullOrUndefined (KinesisFirehoseInput)
  , "InputParallelism" :: NullOrUndefined (InputParallelism)
  , "InputSchema" :: (SourceSchema)
  }
derive instance newtypeInput :: Newtype Input _


-- | <p>When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.</p>
newtype InputConfiguration = InputConfiguration 
  { "Id" :: (Id)
  , "InputStartingPositionConfiguration" :: (InputStartingPositionConfiguration)
  }
derive instance newtypeInputConfiguration :: Newtype InputConfiguration _


newtype InputConfigurations = InputConfigurations (Array InputConfiguration)
derive instance newtypeInputConfigurations :: Newtype InputConfigurations _


-- | <p>Describes the application input configuration. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype InputDescription = InputDescription 
  { "InputId" :: NullOrUndefined (Id)
  , "NamePrefix" :: NullOrUndefined (InAppStreamName)
  , "InAppStreamNames" :: NullOrUndefined (InAppStreamNames)
  , "InputProcessingConfigurationDescription" :: NullOrUndefined (InputProcessingConfigurationDescription)
  , "KinesisStreamsInputDescription" :: NullOrUndefined (KinesisStreamsInputDescription)
  , "KinesisFirehoseInputDescription" :: NullOrUndefined (KinesisFirehoseInputDescription)
  , "InputSchema" :: NullOrUndefined (SourceSchema)
  , "InputParallelism" :: NullOrUndefined (InputParallelism)
  , "InputStartingPositionConfiguration" :: NullOrUndefined (InputStartingPositionConfiguration)
  }
derive instance newtypeInputDescription :: Newtype InputDescription _


newtype InputDescriptions = InputDescriptions (Array InputDescription)
derive instance newtypeInputDescriptions :: Newtype InputDescriptions _


-- | <p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function. </p>
newtype InputLambdaProcessor = InputLambdaProcessor 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeInputLambdaProcessor :: Newtype InputLambdaProcessor _


-- | <p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.</p>
newtype InputLambdaProcessorDescription = InputLambdaProcessorDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeInputLambdaProcessorDescription :: Newtype InputLambdaProcessorDescription _


-- | <p>Represents an update to the <a>InputLambdaProcessor</a> that is used to preprocess the records in the stream.</p>
newtype InputLambdaProcessorUpdate = InputLambdaProcessorUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeInputLambdaProcessorUpdate :: Newtype InputLambdaProcessorUpdate _


-- | <p>Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>
newtype InputParallelism = InputParallelism 
  { "Count" :: NullOrUndefined (InputParallelismCount)
  }
derive instance newtypeInputParallelism :: Newtype InputParallelism _


newtype InputParallelismCount = InputParallelismCount Int
derive instance newtypeInputParallelismCount :: Newtype InputParallelismCount _


-- | <p>Provides updates to the parallelism count.</p>
newtype InputParallelismUpdate = InputParallelismUpdate 
  { "CountUpdate" :: NullOrUndefined (InputParallelismCount)
  }
derive instance newtypeInputParallelismUpdate :: Newtype InputParallelismUpdate _


-- | <p>Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
newtype InputProcessingConfiguration = InputProcessingConfiguration 
  { "InputLambdaProcessor" :: (InputLambdaProcessor)
  }
derive instance newtypeInputProcessingConfiguration :: Newtype InputProcessingConfiguration _


-- | <p>Provides configuration information about an input processor. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription 
  { "InputLambdaProcessorDescription" :: NullOrUndefined (InputLambdaProcessorDescription)
  }
derive instance newtypeInputProcessingConfigurationDescription :: Newtype InputProcessingConfigurationDescription _


-- | <p>Describes updates to an <a>InputProcessingConfiguration</a>. </p>
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate 
  { "InputLambdaProcessorUpdate" :: (InputLambdaProcessorUpdate)
  }
derive instance newtypeInputProcessingConfigurationUpdate :: Newtype InputProcessingConfigurationUpdate _


-- | <p>Describes updates for the application's input schema.</p>
newtype InputSchemaUpdate = InputSchemaUpdate 
  { "RecordFormatUpdate" :: NullOrUndefined (RecordFormat)
  , "RecordEncodingUpdate" :: NullOrUndefined (RecordEncoding)
  , "RecordColumnUpdates" :: NullOrUndefined (RecordColumns)
  }
derive instance newtypeInputSchemaUpdate :: Newtype InputSchemaUpdate _


newtype InputStartingPosition = InputStartingPosition String
derive instance newtypeInputStartingPosition :: Newtype InputStartingPosition _


-- | <p>Describes the point at which the application reads from the streaming source.</p>
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration 
  { "InputStartingPosition" :: NullOrUndefined (InputStartingPosition)
  }
derive instance newtypeInputStartingPositionConfiguration :: Newtype InputStartingPositionConfiguration _


-- | <p>Describes updates to a specific input configuration (identified by the <code>InputId</code> of an application). </p>
newtype InputUpdate = InputUpdate 
  { "InputId" :: (Id)
  , "NamePrefixUpdate" :: NullOrUndefined (InAppStreamName)
  , "InputProcessingConfigurationUpdate" :: NullOrUndefined (InputProcessingConfigurationUpdate)
  , "KinesisStreamsInputUpdate" :: NullOrUndefined (KinesisStreamsInputUpdate)
  , "KinesisFirehoseInputUpdate" :: NullOrUndefined (KinesisFirehoseInputUpdate)
  , "InputSchemaUpdate" :: NullOrUndefined (InputSchemaUpdate)
  , "InputParallelismUpdate" :: NullOrUndefined (InputParallelismUpdate)
  }
derive instance newtypeInputUpdate :: Newtype InputUpdate _


newtype InputUpdates = InputUpdates (Array InputUpdate)
derive instance newtypeInputUpdates :: Newtype InputUpdates _


newtype Inputs = Inputs (Array Input)
derive instance newtypeInputs :: Newtype Inputs _


-- | <p>User-provided application configuration is not valid.</p>
newtype InvalidApplicationConfigurationException = InvalidApplicationConfigurationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidApplicationConfigurationException :: Newtype InvalidApplicationConfigurationException _


-- | <p>Specified input parameter value is invalid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _


-- | <p>Provides additional mapping information when JSON is the record format on the streaming source.</p>
newtype JSONMappingParameters = JSONMappingParameters 
  { "RecordRowPath" :: (RecordRowPath)
  }
derive instance newtypeJSONMappingParameters :: Newtype JSONMappingParameters _


-- | <p> Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>
newtype KinesisFirehoseInput = KinesisFirehoseInput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisFirehoseInput :: Newtype KinesisFirehoseInput _


-- | <p> Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration. </p>
newtype KinesisFirehoseInputDescription = KinesisFirehoseInputDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseInputDescription :: Newtype KinesisFirehoseInputDescription _


-- | <p>When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.</p>
newtype KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseInputUpdate :: Newtype KinesisFirehoseInputUpdate _


-- | <p>When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.</p>
newtype KinesisFirehoseOutput = KinesisFirehoseOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutput :: Newtype KinesisFirehoseOutput _


-- | <p> For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination. </p>
newtype KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutputDescription :: Newtype KinesisFirehoseOutputDescription _


-- | <p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination. </p>
newtype KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisFirehoseOutputUpdate :: Newtype KinesisFirehoseOutputUpdate _


-- | <p> Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>
newtype KinesisStreamsInput = KinesisStreamsInput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamsInput :: Newtype KinesisStreamsInput _


-- | <p> Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration. </p>
newtype KinesisStreamsInputDescription = KinesisStreamsInputDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsInputDescription :: Newtype KinesisStreamsInputDescription _


-- | <p>When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.</p>
newtype KinesisStreamsInputUpdate = KinesisStreamsInputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsInputUpdate :: Newtype KinesisStreamsInputUpdate _


-- | <p>When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.</p>
newtype KinesisStreamsOutput = KinesisStreamsOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamsOutput :: Newtype KinesisStreamsOutput _


-- | <p> For an application output, describes the Amazon Kinesis stream configured as its destination. </p>
newtype KinesisStreamsOutputDescription = KinesisStreamsOutputDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsOutputDescription :: Newtype KinesisStreamsOutputDescription _


-- | <p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis stream configured as the destination. </p>
newtype KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeKinesisStreamsOutputUpdate :: Newtype KinesisStreamsOutputUpdate _


-- | <p>When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf. </p>
newtype LambdaOutput = LambdaOutput 
  { "ResourceARN" :: (ResourceARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeLambdaOutput :: Newtype LambdaOutput _


-- | <p>For an application output, describes the AWS Lambda function configured as its destination. </p>
newtype LambdaOutputDescription = LambdaOutputDescription 
  { "ResourceARN" :: NullOrUndefined (ResourceARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeLambdaOutputDescription :: Newtype LambdaOutputDescription _


-- | <p>When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an AWS Lambda function configured as the destination.</p>
newtype LambdaOutputUpdate = LambdaOutputUpdate 
  { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN)
  , "RoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeLambdaOutputUpdate :: Newtype LambdaOutputUpdate _


-- | <p>Exceeded the number of applications allowed.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListApplicationsInputLimit = ListApplicationsInputLimit Int
derive instance newtypeListApplicationsInputLimit :: Newtype ListApplicationsInputLimit _


-- | <p/>
newtype ListApplicationsRequest = ListApplicationsRequest 
  { "Limit" :: NullOrUndefined (ListApplicationsInputLimit)
  , "ExclusiveStartApplicationName" :: NullOrUndefined (ApplicationName)
  }
derive instance newtypeListApplicationsRequest :: Newtype ListApplicationsRequest _


-- | <p/>
newtype ListApplicationsResponse = ListApplicationsResponse 
  { "ApplicationSummaries" :: (ApplicationSummaries)
  , "HasMoreApplications" :: (BooleanObject)
  }
derive instance newtypeListApplicationsResponse :: Newtype ListApplicationsResponse _


newtype LogStreamARN = LogStreamARN String
derive instance newtypeLogStreamARN :: Newtype LogStreamARN _


-- | <p>When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.</p>
newtype MappingParameters = MappingParameters 
  { "JSONMappingParameters" :: NullOrUndefined (JSONMappingParameters)
  , "CSVMappingParameters" :: NullOrUndefined (CSVMappingParameters)
  }
derive instance newtypeMappingParameters :: Newtype MappingParameters _


-- | <p> Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p> <p/> <p>For limits on how many destinations an application can write and other limitations, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p>
newtype Output = Output 
  { "Name" :: (InAppStreamName)
  , "KinesisStreamsOutput" :: NullOrUndefined (KinesisStreamsOutput)
  , "KinesisFirehoseOutput" :: NullOrUndefined (KinesisFirehoseOutput)
  , "LambdaOutput" :: NullOrUndefined (LambdaOutput)
  , "DestinationSchema" :: (DestinationSchema)
  }
derive instance newtypeOutput :: Newtype Output _


-- | <p>Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p>
newtype OutputDescription = OutputDescription 
  { "OutputId" :: NullOrUndefined (Id)
  , "Name" :: NullOrUndefined (InAppStreamName)
  , "KinesisStreamsOutputDescription" :: NullOrUndefined (KinesisStreamsOutputDescription)
  , "KinesisFirehoseOutputDescription" :: NullOrUndefined (KinesisFirehoseOutputDescription)
  , "LambdaOutputDescription" :: NullOrUndefined (LambdaOutputDescription)
  , "DestinationSchema" :: NullOrUndefined (DestinationSchema)
  }
derive instance newtypeOutputDescription :: Newtype OutputDescription _


newtype OutputDescriptions = OutputDescriptions (Array OutputDescription)
derive instance newtypeOutputDescriptions :: Newtype OutputDescriptions _


-- | <p> Describes updates to the output configuration identified by the <code>OutputId</code>. </p>
newtype OutputUpdate = OutputUpdate 
  { "OutputId" :: (Id)
  , "NameUpdate" :: NullOrUndefined (InAppStreamName)
  , "KinesisStreamsOutputUpdate" :: NullOrUndefined (KinesisStreamsOutputUpdate)
  , "KinesisFirehoseOutputUpdate" :: NullOrUndefined (KinesisFirehoseOutputUpdate)
  , "LambdaOutputUpdate" :: NullOrUndefined (LambdaOutputUpdate)
  , "DestinationSchemaUpdate" :: NullOrUndefined (DestinationSchema)
  }
derive instance newtypeOutputUpdate :: Newtype OutputUpdate _


newtype OutputUpdates = OutputUpdates (Array OutputUpdate)
derive instance newtypeOutputUpdates :: Newtype OutputUpdates _


newtype Outputs = Outputs (Array Output)
derive instance newtypeOutputs :: Newtype Outputs _


newtype ParsedInputRecord = ParsedInputRecord (Array ParsedInputRecordField)
derive instance newtypeParsedInputRecord :: Newtype ParsedInputRecord _


newtype ParsedInputRecordField = ParsedInputRecordField String
derive instance newtypeParsedInputRecordField :: Newtype ParsedInputRecordField _


newtype ParsedInputRecords = ParsedInputRecords (Array ParsedInputRecord)
derive instance newtypeParsedInputRecords :: Newtype ParsedInputRecords _


newtype ProcessedInputRecord = ProcessedInputRecord String
derive instance newtypeProcessedInputRecord :: Newtype ProcessedInputRecord _


newtype ProcessedInputRecords = ProcessedInputRecords (Array ProcessedInputRecord)
derive instance newtypeProcessedInputRecords :: Newtype ProcessedInputRecords _


newtype RawInputRecord = RawInputRecord String
derive instance newtypeRawInputRecord :: Newtype RawInputRecord _


newtype RawInputRecords = RawInputRecords (Array RawInputRecord)
derive instance newtypeRawInputRecords :: Newtype RawInputRecords _


-- | <p>Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.</p> <p>Also used to describe the format of the reference data source.</p>
newtype RecordColumn = RecordColumn 
  { "Name" :: (RecordColumnName)
  , "Mapping" :: NullOrUndefined (RecordColumnMapping)
  , "SqlType" :: (RecordColumnSqlType)
  }
derive instance newtypeRecordColumn :: Newtype RecordColumn _


newtype RecordColumnDelimiter = RecordColumnDelimiter String
derive instance newtypeRecordColumnDelimiter :: Newtype RecordColumnDelimiter _


newtype RecordColumnMapping = RecordColumnMapping String
derive instance newtypeRecordColumnMapping :: Newtype RecordColumnMapping _


newtype RecordColumnName = RecordColumnName String
derive instance newtypeRecordColumnName :: Newtype RecordColumnName _


newtype RecordColumnSqlType = RecordColumnSqlType String
derive instance newtypeRecordColumnSqlType :: Newtype RecordColumnSqlType _


newtype RecordColumns = RecordColumns (Array RecordColumn)
derive instance newtypeRecordColumns :: Newtype RecordColumns _


newtype RecordEncoding = RecordEncoding String
derive instance newtypeRecordEncoding :: Newtype RecordEncoding _


-- | <p> Describes the record format and relevant mapping information that should be applied to schematize the records on the stream. </p>
newtype RecordFormat = RecordFormat 
  { "RecordFormatType" :: (RecordFormatType)
  , "MappingParameters" :: NullOrUndefined (MappingParameters)
  }
derive instance newtypeRecordFormat :: Newtype RecordFormat _


newtype RecordFormatType = RecordFormatType String
derive instance newtypeRecordFormatType :: Newtype RecordFormatType _


newtype RecordRowDelimiter = RecordRowDelimiter String
derive instance newtypeRecordRowDelimiter :: Newtype RecordRowDelimiter _


newtype RecordRowPath = RecordRowPath String
derive instance newtypeRecordRowPath :: Newtype RecordRowPath _


-- | <p>Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.</p>
newtype ReferenceDataSource = ReferenceDataSource 
  { "TableName" :: (InAppTableName)
  , "S3ReferenceDataSource" :: NullOrUndefined (S3ReferenceDataSource)
  , "ReferenceSchema" :: (SourceSchema)
  }
derive instance newtypeReferenceDataSource :: Newtype ReferenceDataSource _


-- | <p>Describes the reference data source configured for an application.</p>
newtype ReferenceDataSourceDescription = ReferenceDataSourceDescription 
  { "ReferenceId" :: (Id)
  , "TableName" :: (InAppTableName)
  , "S3ReferenceDataSourceDescription" :: (S3ReferenceDataSourceDescription)
  , "ReferenceSchema" :: NullOrUndefined (SourceSchema)
  }
derive instance newtypeReferenceDataSourceDescription :: Newtype ReferenceDataSourceDescription _


newtype ReferenceDataSourceDescriptions = ReferenceDataSourceDescriptions (Array ReferenceDataSourceDescription)
derive instance newtypeReferenceDataSourceDescriptions :: Newtype ReferenceDataSourceDescriptions _


-- | <p>When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.</p>
newtype ReferenceDataSourceUpdate = ReferenceDataSourceUpdate 
  { "ReferenceId" :: (Id)
  , "TableNameUpdate" :: NullOrUndefined (InAppTableName)
  , "S3ReferenceDataSourceUpdate" :: NullOrUndefined (S3ReferenceDataSourceUpdate)
  , "ReferenceSchemaUpdate" :: NullOrUndefined (SourceSchema)
  }
derive instance newtypeReferenceDataSourceUpdate :: Newtype ReferenceDataSourceUpdate _


newtype ReferenceDataSourceUpdates = ReferenceDataSourceUpdates (Array ReferenceDataSourceUpdate)
derive instance newtypeReferenceDataSourceUpdates :: Newtype ReferenceDataSourceUpdates _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


-- | <p>Application is not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>Specified application can't be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html">GetRecords</a> in the Amazon Kinesis Streams API Reference.</p>
newtype ResourceProvisionedThroughputExceededException = ResourceProvisionedThroughputExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceProvisionedThroughputExceededException :: Newtype ResourceProvisionedThroughputExceededException _


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _


-- | <p>Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the S3 object that contains the data.</p>
newtype S3Configuration = S3Configuration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  }
derive instance newtypeS3Configuration :: Newtype S3Configuration _


-- | <p>Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.</p> <p>An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <a>UpdateApplication</a> operation to trigger reloading of data into your application.</p>
newtype S3ReferenceDataSource = S3ReferenceDataSource 
  { "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  , "ReferenceRoleARN" :: (RoleARN)
  }
derive instance newtypeS3ReferenceDataSource :: Newtype S3ReferenceDataSource _


-- | <p>Provides the bucket name and object key name that stores the reference data.</p>
newtype S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription 
  { "BucketARN" :: (BucketARN)
  , "FileKey" :: (FileKey)
  , "ReferenceRoleARN" :: (RoleARN)
  }
derive instance newtypeS3ReferenceDataSourceDescription :: Newtype S3ReferenceDataSourceDescription _


-- | <p>Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.</p>
newtype S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate 
  { "BucketARNUpdate" :: NullOrUndefined (BucketARN)
  , "FileKeyUpdate" :: NullOrUndefined (FileKey)
  , "ReferenceRoleARNUpdate" :: NullOrUndefined (RoleARN)
  }
derive instance newtypeS3ReferenceDataSourceUpdate :: Newtype S3ReferenceDataSourceUpdate _


-- | <p>The service is unavailable, back off and retry the operation. </p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


-- | <p>Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.</p>
newtype SourceSchema = SourceSchema 
  { "RecordFormat" :: (RecordFormat)
  , "RecordEncoding" :: NullOrUndefined (RecordEncoding)
  , "RecordColumns" :: (RecordColumns)
  }
derive instance newtypeSourceSchema :: Newtype SourceSchema _


-- | <p/>
newtype StartApplicationRequest = StartApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "InputConfigurations" :: (InputConfigurations)
  }
derive instance newtypeStartApplicationRequest :: Newtype StartApplicationRequest _


-- | <p/>
newtype StartApplicationResponse = StartApplicationResponse 
  { 
  }
derive instance newtypeStartApplicationResponse :: Newtype StartApplicationResponse _


-- | <p/>
newtype StopApplicationRequest = StopApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  }
derive instance newtypeStopApplicationRequest :: Newtype StopApplicationRequest _


-- | <p/>
newtype StopApplicationResponse = StopApplicationResponse 
  { 
  }
derive instance newtypeStopApplicationResponse :: Newtype StopApplicationResponse _


-- | <p>Data format is not valid, Amazon Kinesis Analytics is not able to detect schema for the given streaming source.</p>
newtype UnableToDetectSchemaException = UnableToDetectSchemaException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "RawInputRecords" :: NullOrUndefined (RawInputRecords)
  , "ProcessedInputRecords" :: NullOrUndefined (ProcessedInputRecords)
  }
derive instance newtypeUnableToDetectSchemaException :: Newtype UnableToDetectSchemaException _


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ApplicationName" :: (ApplicationName)
  , "CurrentApplicationVersionId" :: (ApplicationVersionId)
  , "ApplicationUpdate" :: (ApplicationUpdate)
  }
derive instance newtypeUpdateApplicationRequest :: Newtype UpdateApplicationRequest _


newtype UpdateApplicationResponse = UpdateApplicationResponse 
  { 
  }
derive instance newtypeUpdateApplicationResponse :: Newtype UpdateApplicationResponse _
