## Module AWS.KinesisAnalytics

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addApplicationCloudWatchLoggingOption`

``` purescript
addApplicationCloudWatchLoggingOption :: forall eff. AddApplicationCloudWatchLoggingOptionRequest -> Aff (err :: RequestError | eff) AddApplicationCloudWatchLoggingOptionResponse
```

<p>Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>

#### `addApplicationInput`

``` purescript
addApplicationInput :: forall eff. AddApplicationInputRequest -> Aff (err :: RequestError | eff) AddApplicationInputResponse
```

<p> Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p> <p>You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see <a>CreateApplication</a>.</p> <p>Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationInput</code> action.</p>

#### `addApplicationInputProcessingConfiguration`

``` purescript
addApplicationInputProcessingConfiguration :: forall eff. AddApplicationInputProcessingConfigurationRequest -> Aff (err :: RequestError | eff) AddApplicationInputProcessingConfigurationResponse
```

<p>Adds an <a>InputProcessingConfiguration</a> to an application. An input processor preprocesses records on the input stream before the application's SQL code executes. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>

#### `addApplicationOutput`

``` purescript
addApplicationOutput :: forall eff. AddApplicationOutputRequest -> Aff (err :: RequestError | eff) AddApplicationOutputResponse
```

<p>Adds an external destination to your Amazon Kinesis Analytics application.</p> <p>If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.</p> <p> You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Understanding Application Output (Destination)</a>. </p> <p> Note that any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <a>DescribeApplication</a> operation to find the current application version.</p> <p>For the limits on the number of application inputs and outputs you can configure, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action.</p>

#### `addApplicationReferenceDataSource`

``` purescript
addApplicationReferenceDataSource :: forall eff. AddApplicationReferenceDataSourceRequest -> Aff (err :: RequestError | eff) AddApplicationReferenceDataSourceResponse
```

<p>Adds a reference data source to an existing application.</p> <p>Amazon Kinesis Analytics reads reference data (that is, an Amazon S3 object) and creates an in-application table within your application. In the request, you provide the source (S3 bucket name and object key name), name of the in-application table to create, and the necessary mapping information that describes how data in Amazon S3 object maps to columns in the resulting in-application table.</p> <p> For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. For the limits on data sources you can add to your application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:AddApplicationOutput</code> action. </p>

#### `createApplication`

``` purescript
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: RequestError | eff) CreateApplicationResponse
```

<p> Creates an Amazon Kinesis Analytics application. You can configure each application with one streaming source as input, application code to process the input, and up to three destinations where you want Amazon Kinesis Analytics to write the output data from your application. For an overview, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html">How it Works</a>. </p> <p>In the input configuration, you map the streaming source to an in-application stream, which you can think of as a constantly updating table. In the mapping, you must provide a schema for the in-application stream and map each data column in the in-application stream to a data element in the streaming source.</p> <p>Your application code is one or more SQL statements that read input data, transform it, and generate output. Your application code can create one or more SQL artifacts like SQL streams or pumps.</p> <p>In the output configuration, you can configure the application to write data from in-application streams created in your applications to up to three destinations.</p> <p> To read data from your source stream or write data to destination streams, Amazon Kinesis Analytics needs your permissions. You grant these permissions by creating IAM roles. This operation requires permissions to perform the <code>kinesisanalytics:CreateApplication</code> action. </p> <p> For introductory exercises to create an Amazon Kinesis Analytics application, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html">Getting Started</a>. </p>

#### `deleteApplication`

``` purescript
deleteApplication :: forall eff. DeleteApplicationRequest -> Aff (err :: RequestError | eff) DeleteApplicationResponse
```

<p>Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplication</code> action.</p>

#### `deleteApplicationCloudWatchLoggingOption`

``` purescript
deleteApplicationCloudWatchLoggingOption :: forall eff. DeleteApplicationCloudWatchLoggingOptionRequest -> Aff (err :: RequestError | eff) DeleteApplicationCloudWatchLoggingOptionResponse
```

<p>Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html">Working with Amazon CloudWatch Logs</a>.</p>

#### `deleteApplicationInputProcessingConfiguration`

``` purescript
deleteApplicationInputProcessingConfiguration :: forall eff. DeleteApplicationInputProcessingConfigurationRequest -> Aff (err :: RequestError | eff) DeleteApplicationInputProcessingConfigurationResponse
```

<p>Deletes an <a>InputProcessingConfiguration</a> from an input.</p>

#### `deleteApplicationOutput`

``` purescript
deleteApplicationOutput :: forall eff. DeleteApplicationOutputRequest -> Aff (err :: RequestError | eff) DeleteApplicationOutputResponse
```

<p>Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DeleteApplicationOutput</code> action.</p>

#### `deleteApplicationReferenceDataSource`

``` purescript
deleteApplicationReferenceDataSource :: forall eff. DeleteApplicationReferenceDataSourceRequest -> Aff (err :: RequestError | eff) DeleteApplicationReferenceDataSourceResponse
```

<p>Deletes a reference data source configuration from the specified application configuration.</p> <p>If the application is running, Amazon Kinesis Analytics immediately removes the in-application table that you created using the <a>AddApplicationReferenceDataSource</a> operation. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics.DeleteApplicationReferenceDataSource</code> action.</p>

#### `describeApplication`

``` purescript
describeApplication :: forall eff. DescribeApplicationRequest -> Aff (err :: RequestError | eff) DescribeApplicationResponse
```

<p>Returns information about a specific Amazon Kinesis Analytics application.</p> <p>If you want to retrieve a list of all applications in your account, use the <a>ListApplications</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:DescribeApplication</code> action. You can use <code>DescribeApplication</code> to get the current application versionId, which you need to call other operations such as <code>Update</code>. </p>

#### `discoverInputSchema`

``` purescript
discoverInputSchema :: forall eff. DiscoverInputSchemaRequest -> Aff (err :: RequestError | eff) DiscoverInputSchemaResponse
```

<p>Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream) or S3 object. In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.</p> <p> You can use the inferred schema when configuring a streaming source for your application. For conceptual information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. Note that when you create an application using the Amazon Kinesis Analytics console, the console uses this operation to infer a schema and show it in the console user interface. </p> <p> This operation requires permissions to perform the <code>kinesisanalytics:DiscoverInputSchema</code> action. </p>

#### `listApplications`

``` purescript
listApplications :: forall eff. ListApplicationsRequest -> Aff (err :: RequestError | eff) ListApplicationsResponse
```

<p>Returns a list of Amazon Kinesis Analytics applications in your account. For each application, the response includes the application name, Amazon Resource Name (ARN), and status. If the response returns the <code>HasMoreApplications</code> value as true, you can send another request by adding the <code>ExclusiveStartApplicationName</code> in the request body, and set the value of this to the last application name from the previous response. </p> <p>If you want detailed information about a specific application, use <a>DescribeApplication</a>.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:ListApplications</code> action.</p>

#### `startApplication`

``` purescript
startApplication :: forall eff. StartApplicationRequest -> Aff (err :: RequestError | eff) StartApplicationResponse
```

<p>Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.</p> <p>After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.</p> <p> The application status must be <code>READY</code> for you to start an application. You can get the application status in the console or using the <a>DescribeApplication</a> operation.</p> <p>After you start the application, you can stop the application from processing the input by calling the <a>StopApplication</a> operation.</p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StartApplication</code> action.</p>

#### `stopApplication`

``` purescript
stopApplication :: forall eff. StopApplicationRequest -> Aff (err :: RequestError | eff) StopApplicationResponse
```

<p>Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the <a>DescribeApplication</a> operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination. </p> <p>This operation requires permissions to perform the <code>kinesisanalytics:StopApplication</code> action.</p>

#### `updateApplication`

``` purescript
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: RequestError | eff) UpdateApplicationResponse
```

<p>Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration. </p> <p>Note that Amazon Kinesis Analytics updates the <code>CurrentApplicationVersionId</code> each time you update your application. </p> <p>This operation requires permission for the <code>kinesisanalytics:UpdateApplication</code> action.</p>

#### `AddApplicationCloudWatchLoggingOptionRequest`

``` purescript
newtype AddApplicationCloudWatchLoggingOptionRequest
  = AddApplicationCloudWatchLoggingOptionRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "CloudWatchLoggingOption" :: CloudWatchLoggingOption }
```

##### Instances
``` purescript
Newtype AddApplicationCloudWatchLoggingOptionRequest _
```

#### `AddApplicationCloudWatchLoggingOptionResponse`

``` purescript
newtype AddApplicationCloudWatchLoggingOptionResponse
  = AddApplicationCloudWatchLoggingOptionResponse {  }
```

##### Instances
``` purescript
Newtype AddApplicationCloudWatchLoggingOptionResponse _
```

#### `AddApplicationInputProcessingConfigurationRequest`

``` purescript
newtype AddApplicationInputProcessingConfigurationRequest
  = AddApplicationInputProcessingConfigurationRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "InputId" :: Id, "InputProcessingConfiguration" :: InputProcessingConfiguration }
```

##### Instances
``` purescript
Newtype AddApplicationInputProcessingConfigurationRequest _
```

#### `AddApplicationInputProcessingConfigurationResponse`

``` purescript
newtype AddApplicationInputProcessingConfigurationResponse
  = AddApplicationInputProcessingConfigurationResponse {  }
```

##### Instances
``` purescript
Newtype AddApplicationInputProcessingConfigurationResponse _
```

#### `AddApplicationInputRequest`

``` purescript
newtype AddApplicationInputRequest
  = AddApplicationInputRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "Input" :: Input }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationInputRequest _
```

#### `AddApplicationInputResponse`

``` purescript
newtype AddApplicationInputResponse
  = AddApplicationInputResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationInputResponse _
```

#### `AddApplicationOutputRequest`

``` purescript
newtype AddApplicationOutputRequest
  = AddApplicationOutputRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "Output" :: Output }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationOutputRequest _
```

#### `AddApplicationOutputResponse`

``` purescript
newtype AddApplicationOutputResponse
  = AddApplicationOutputResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationOutputResponse _
```

#### `AddApplicationReferenceDataSourceRequest`

``` purescript
newtype AddApplicationReferenceDataSourceRequest
  = AddApplicationReferenceDataSourceRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "ReferenceDataSource" :: ReferenceDataSource }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationReferenceDataSourceRequest _
```

#### `AddApplicationReferenceDataSourceResponse`

``` purescript
newtype AddApplicationReferenceDataSourceResponse
  = AddApplicationReferenceDataSourceResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype AddApplicationReferenceDataSourceResponse _
```

#### `ApplicationCode`

``` purescript
newtype ApplicationCode
  = ApplicationCode String
```

##### Instances
``` purescript
Newtype ApplicationCode _
```

#### `ApplicationDescription`

``` purescript
newtype ApplicationDescription
  = ApplicationDescription String
```

##### Instances
``` purescript
Newtype ApplicationDescription _
```

#### `ApplicationDetail`

``` purescript
newtype ApplicationDetail
  = ApplicationDetail { "ApplicationName" :: ApplicationName, "ApplicationDescription" :: NullOrUndefined (ApplicationDescription), "ApplicationARN" :: ResourceARN, "ApplicationStatus" :: ApplicationStatus, "CreateTimestamp" :: NullOrUndefined (Number), "LastUpdateTimestamp" :: NullOrUndefined (Number), "InputDescriptions" :: NullOrUndefined (InputDescriptions), "OutputDescriptions" :: NullOrUndefined (OutputDescriptions), "ReferenceDataSourceDescriptions" :: NullOrUndefined (ReferenceDataSourceDescriptions), "CloudWatchLoggingOptionDescriptions" :: NullOrUndefined (CloudWatchLoggingOptionDescriptions), "ApplicationCode" :: NullOrUndefined (ApplicationCode), "ApplicationVersionId" :: ApplicationVersionId }
```

<p>Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.</p>

##### Instances
``` purescript
Newtype ApplicationDetail _
```

#### `ApplicationName`

``` purescript
newtype ApplicationName
  = ApplicationName String
```

##### Instances
``` purescript
Newtype ApplicationName _
```

#### `ApplicationStatus`

``` purescript
newtype ApplicationStatus
  = ApplicationStatus String
```

##### Instances
``` purescript
Newtype ApplicationStatus _
```

#### `ApplicationSummaries`

``` purescript
newtype ApplicationSummaries
  = ApplicationSummaries (Array ApplicationSummary)
```

##### Instances
``` purescript
Newtype ApplicationSummaries _
```

#### `ApplicationSummary`

``` purescript
newtype ApplicationSummary
  = ApplicationSummary { "ApplicationName" :: ApplicationName, "ApplicationARN" :: ResourceARN, "ApplicationStatus" :: ApplicationStatus }
```

<p>Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.</p>

##### Instances
``` purescript
Newtype ApplicationSummary _
```

#### `ApplicationUpdate`

``` purescript
newtype ApplicationUpdate
  = ApplicationUpdate { "InputUpdates" :: NullOrUndefined (InputUpdates), "ApplicationCodeUpdate" :: NullOrUndefined (ApplicationCode), "OutputUpdates" :: NullOrUndefined (OutputUpdates), "ReferenceDataSourceUpdates" :: NullOrUndefined (ReferenceDataSourceUpdates), "CloudWatchLoggingOptionUpdates" :: NullOrUndefined (CloudWatchLoggingOptionUpdates) }
```

<p>Describes updates to apply to an existing Amazon Kinesis Analytics application.</p>

##### Instances
``` purescript
Newtype ApplicationUpdate _
```

#### `ApplicationVersionId`

``` purescript
newtype ApplicationVersionId
  = ApplicationVersionId Number
```

##### Instances
``` purescript
Newtype ApplicationVersionId _
```

#### `BooleanObject`

``` purescript
newtype BooleanObject
  = BooleanObject Boolean
```

##### Instances
``` purescript
Newtype BooleanObject _
```

#### `BucketARN`

``` purescript
newtype BucketARN
  = BucketARN String
```

##### Instances
``` purescript
Newtype BucketARN _
```

#### `CSVMappingParameters`

``` purescript
newtype CSVMappingParameters
  = CSVMappingParameters { "RecordRowDelimiter" :: RecordRowDelimiter, "RecordColumnDelimiter" :: RecordColumnDelimiter }
```

<p>Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the <i>'\n'</i> as the row delimiter and a comma (",") as the column delimiter: </p> <p> <code>"name1", "address1" </code> </p> <p> <code>"name2, "address2"</code> </p>

##### Instances
``` purescript
Newtype CSVMappingParameters _
```

#### `CloudWatchLoggingOption`

``` purescript
newtype CloudWatchLoggingOption
  = CloudWatchLoggingOption { "LogStreamARN" :: LogStreamARN, "RoleARN" :: RoleARN }
```

<p>Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.</p>

##### Instances
``` purescript
Newtype CloudWatchLoggingOption _
```

#### `CloudWatchLoggingOptionDescription`

``` purescript
newtype CloudWatchLoggingOptionDescription
  = CloudWatchLoggingOptionDescription { "CloudWatchLoggingOptionId" :: NullOrUndefined (Id), "LogStreamARN" :: LogStreamARN, "RoleARN" :: RoleARN }
```

<p>Description of the CloudWatch logging option.</p>

##### Instances
``` purescript
Newtype CloudWatchLoggingOptionDescription _
```

#### `CloudWatchLoggingOptionDescriptions`

``` purescript
newtype CloudWatchLoggingOptionDescriptions
  = CloudWatchLoggingOptionDescriptions (Array CloudWatchLoggingOptionDescription)
```

##### Instances
``` purescript
Newtype CloudWatchLoggingOptionDescriptions _
```

#### `CloudWatchLoggingOptionUpdate`

``` purescript
newtype CloudWatchLoggingOptionUpdate
  = CloudWatchLoggingOptionUpdate { "CloudWatchLoggingOptionId" :: Id, "LogStreamARNUpdate" :: NullOrUndefined (LogStreamARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>Describes CloudWatch logging option updates.</p>

##### Instances
``` purescript
Newtype CloudWatchLoggingOptionUpdate _
```

#### `CloudWatchLoggingOptionUpdates`

``` purescript
newtype CloudWatchLoggingOptionUpdates
  = CloudWatchLoggingOptionUpdates (Array CloudWatchLoggingOptionUpdate)
```

##### Instances
``` purescript
Newtype CloudWatchLoggingOptionUpdates _
```

#### `CloudWatchLoggingOptions`

``` purescript
newtype CloudWatchLoggingOptions
  = CloudWatchLoggingOptions (Array CloudWatchLoggingOption)
```

##### Instances
``` purescript
Newtype CloudWatchLoggingOptions _
```

#### `CodeValidationException`

``` purescript
newtype CodeValidationException
  = CodeValidationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>User-provided application code (query) is invalid. This can be a simple syntax error.</p>

##### Instances
``` purescript
Newtype CodeValidationException _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `CreateApplicationRequest`

``` purescript
newtype CreateApplicationRequest
  = CreateApplicationRequest { "ApplicationName" :: ApplicationName, "ApplicationDescription" :: NullOrUndefined (ApplicationDescription), "Inputs" :: NullOrUndefined (Inputs), "Outputs" :: NullOrUndefined (Outputs), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions), "ApplicationCode" :: NullOrUndefined (ApplicationCode) }
```

<p>TBD</p>

##### Instances
``` purescript
Newtype CreateApplicationRequest _
```

#### `CreateApplicationResponse`

``` purescript
newtype CreateApplicationResponse
  = CreateApplicationResponse { "ApplicationSummary" :: ApplicationSummary }
```

<p>TBD</p>

##### Instances
``` purescript
Newtype CreateApplicationResponse _
```

#### `DeleteApplicationCloudWatchLoggingOptionRequest`

``` purescript
newtype DeleteApplicationCloudWatchLoggingOptionRequest
  = DeleteApplicationCloudWatchLoggingOptionRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "CloudWatchLoggingOptionId" :: Id }
```

##### Instances
``` purescript
Newtype DeleteApplicationCloudWatchLoggingOptionRequest _
```

#### `DeleteApplicationCloudWatchLoggingOptionResponse`

``` purescript
newtype DeleteApplicationCloudWatchLoggingOptionResponse
  = DeleteApplicationCloudWatchLoggingOptionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteApplicationCloudWatchLoggingOptionResponse _
```

#### `DeleteApplicationInputProcessingConfigurationRequest`

``` purescript
newtype DeleteApplicationInputProcessingConfigurationRequest
  = DeleteApplicationInputProcessingConfigurationRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "InputId" :: Id }
```

##### Instances
``` purescript
Newtype DeleteApplicationInputProcessingConfigurationRequest _
```

#### `DeleteApplicationInputProcessingConfigurationResponse`

``` purescript
newtype DeleteApplicationInputProcessingConfigurationResponse
  = DeleteApplicationInputProcessingConfigurationResponse {  }
```

##### Instances
``` purescript
Newtype DeleteApplicationInputProcessingConfigurationResponse _
```

#### `DeleteApplicationOutputRequest`

``` purescript
newtype DeleteApplicationOutputRequest
  = DeleteApplicationOutputRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "OutputId" :: Id }
```

<p/>

##### Instances
``` purescript
Newtype DeleteApplicationOutputRequest _
```

#### `DeleteApplicationOutputResponse`

``` purescript
newtype DeleteApplicationOutputResponse
  = DeleteApplicationOutputResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype DeleteApplicationOutputResponse _
```

#### `DeleteApplicationReferenceDataSourceRequest`

``` purescript
newtype DeleteApplicationReferenceDataSourceRequest
  = DeleteApplicationReferenceDataSourceRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "ReferenceId" :: Id }
```

##### Instances
``` purescript
Newtype DeleteApplicationReferenceDataSourceRequest _
```

#### `DeleteApplicationReferenceDataSourceResponse`

``` purescript
newtype DeleteApplicationReferenceDataSourceResponse
  = DeleteApplicationReferenceDataSourceResponse {  }
```

##### Instances
``` purescript
Newtype DeleteApplicationReferenceDataSourceResponse _
```

#### `DeleteApplicationRequest`

``` purescript
newtype DeleteApplicationRequest
  = DeleteApplicationRequest { "ApplicationName" :: ApplicationName, "CreateTimestamp" :: Number }
```

<p/>

##### Instances
``` purescript
Newtype DeleteApplicationRequest _
```

#### `DeleteApplicationResponse`

``` purescript
newtype DeleteApplicationResponse
  = DeleteApplicationResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype DeleteApplicationResponse _
```

#### `DescribeApplicationRequest`

``` purescript
newtype DescribeApplicationRequest
  = DescribeApplicationRequest { "ApplicationName" :: ApplicationName }
```

<p/>

##### Instances
``` purescript
Newtype DescribeApplicationRequest _
```

#### `DescribeApplicationResponse`

``` purescript
newtype DescribeApplicationResponse
  = DescribeApplicationResponse { "ApplicationDetail" :: ApplicationDetail }
```

<p/>

##### Instances
``` purescript
Newtype DescribeApplicationResponse _
```

#### `DestinationSchema`

``` purescript
newtype DestinationSchema
  = DestinationSchema { "RecordFormatType" :: NullOrUndefined (RecordFormatType) }
```

<p>Describes the data format when records are written to the destination. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html">Configuring Application Output</a>. </p>

##### Instances
``` purescript
Newtype DestinationSchema _
```

#### `DiscoverInputSchemaRequest`

``` purescript
newtype DiscoverInputSchemaRequest
  = DiscoverInputSchemaRequest { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN), "InputStartingPositionConfiguration" :: NullOrUndefined (InputStartingPositionConfiguration), "S3Configuration" :: NullOrUndefined (S3Configuration), "InputProcessingConfiguration" :: NullOrUndefined (InputProcessingConfiguration) }
```

##### Instances
``` purescript
Newtype DiscoverInputSchemaRequest _
```

#### `DiscoverInputSchemaResponse`

``` purescript
newtype DiscoverInputSchemaResponse
  = DiscoverInputSchemaResponse { "InputSchema" :: NullOrUndefined (SourceSchema), "ParsedInputRecords" :: NullOrUndefined (ParsedInputRecords), "ProcessedInputRecords" :: NullOrUndefined (ProcessedInputRecords), "RawInputRecords" :: NullOrUndefined (RawInputRecords) }
```

<p/>

##### Instances
``` purescript
Newtype DiscoverInputSchemaResponse _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `FileKey`

``` purescript
newtype FileKey
  = FileKey String
```

##### Instances
``` purescript
Newtype FileKey _
```

#### `Id`

``` purescript
newtype Id
  = Id String
```

##### Instances
``` purescript
Newtype Id _
```

#### `InAppStreamName`

``` purescript
newtype InAppStreamName
  = InAppStreamName String
```

##### Instances
``` purescript
Newtype InAppStreamName _
```

#### `InAppStreamNames`

``` purescript
newtype InAppStreamNames
  = InAppStreamNames (Array InAppStreamName)
```

##### Instances
``` purescript
Newtype InAppStreamNames _
```

#### `InAppTableName`

``` purescript
newtype InAppTableName
  = InAppTableName String
```

##### Instances
``` purescript
Newtype InAppTableName _
```

#### `Input`

``` purescript
newtype Input
  = Input { "NamePrefix" :: InAppStreamName, "InputProcessingConfiguration" :: NullOrUndefined (InputProcessingConfiguration), "KinesisStreamsInput" :: NullOrUndefined (KinesisStreamsInput), "KinesisFirehoseInput" :: NullOrUndefined (KinesisFirehoseInput), "InputParallelism" :: NullOrUndefined (InputParallelism), "InputSchema" :: SourceSchema }
```

<p>When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>

##### Instances
``` purescript
Newtype Input _
```

#### `InputConfiguration`

``` purescript
newtype InputConfiguration
  = InputConfiguration { "Id" :: Id, "InputStartingPositionConfiguration" :: InputStartingPositionConfiguration }
```

<p>When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.</p>

##### Instances
``` purescript
Newtype InputConfiguration _
```

#### `InputConfigurations`

``` purescript
newtype InputConfigurations
  = InputConfigurations (Array InputConfiguration)
```

##### Instances
``` purescript
Newtype InputConfigurations _
```

#### `InputDescription`

``` purescript
newtype InputDescription
  = InputDescription { "InputId" :: NullOrUndefined (Id), "NamePrefix" :: NullOrUndefined (InAppStreamName), "InAppStreamNames" :: NullOrUndefined (InAppStreamNames), "InputProcessingConfigurationDescription" :: NullOrUndefined (InputProcessingConfigurationDescription), "KinesisStreamsInputDescription" :: NullOrUndefined (KinesisStreamsInputDescription), "KinesisFirehoseInputDescription" :: NullOrUndefined (KinesisFirehoseInputDescription), "InputSchema" :: NullOrUndefined (SourceSchema), "InputParallelism" :: NullOrUndefined (InputParallelism), "InputStartingPositionConfiguration" :: NullOrUndefined (InputStartingPositionConfiguration) }
```

<p>Describes the application input configuration. For more information, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>

##### Instances
``` purescript
Newtype InputDescription _
```

#### `InputDescriptions`

``` purescript
newtype InputDescriptions
  = InputDescriptions (Array InputDescription)
```

##### Instances
``` purescript
Newtype InputDescriptions _
```

#### `InputLambdaProcessor`

``` purescript
newtype InputLambdaProcessor
  = InputLambdaProcessor { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function. </p>

##### Instances
``` purescript
Newtype InputLambdaProcessor _
```

#### `InputLambdaProcessorDescription`

``` purescript
newtype InputLambdaProcessorDescription
  = InputLambdaProcessorDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p>An object that contains the Amazon Resource Name (ARN) of the <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.</p>

##### Instances
``` purescript
Newtype InputLambdaProcessorDescription _
```

#### `InputLambdaProcessorUpdate`

``` purescript
newtype InputLambdaProcessorUpdate
  = InputLambdaProcessorUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>Represents an update to the <a>InputLambdaProcessor</a> that is used to preprocess the records in the stream.</p>

##### Instances
``` purescript
Newtype InputLambdaProcessorUpdate _
```

#### `InputParallelism`

``` purescript
newtype InputParallelism
  = InputParallelism { "Count" :: NullOrUndefined (InputParallelismCount) }
```

<p>Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html">Configuring Application Input</a>. </p>

##### Instances
``` purescript
Newtype InputParallelism _
```

#### `InputParallelismCount`

``` purescript
newtype InputParallelismCount
  = InputParallelismCount Int
```

##### Instances
``` purescript
Newtype InputParallelismCount _
```

#### `InputParallelismUpdate`

``` purescript
newtype InputParallelismUpdate
  = InputParallelismUpdate { "CountUpdate" :: NullOrUndefined (InputParallelismCount) }
```

<p>Provides updates to the parallelism count.</p>

##### Instances
``` purescript
Newtype InputParallelismUpdate _
```

#### `InputProcessingConfiguration`

``` purescript
newtype InputProcessingConfiguration
  = InputProcessingConfiguration { "InputLambdaProcessor" :: InputLambdaProcessor }
```

<p>Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>

##### Instances
``` purescript
Newtype InputProcessingConfiguration _
```

#### `InputProcessingConfigurationDescription`

``` purescript
newtype InputProcessingConfigurationDescription
  = InputProcessingConfigurationDescription { "InputLambdaProcessorDescription" :: NullOrUndefined (InputLambdaProcessorDescription) }
```

<p>Provides configuration information about an input processor. Currently, the only input processor available is <a href="https://aws.amazon.com/documentation/lambda/">AWS Lambda</a>.</p>

##### Instances
``` purescript
Newtype InputProcessingConfigurationDescription _
```

#### `InputProcessingConfigurationUpdate`

``` purescript
newtype InputProcessingConfigurationUpdate
  = InputProcessingConfigurationUpdate { "InputLambdaProcessorUpdate" :: InputLambdaProcessorUpdate }
```

<p>Describes updates to an <a>InputProcessingConfiguration</a>. </p>

##### Instances
``` purescript
Newtype InputProcessingConfigurationUpdate _
```

#### `InputSchemaUpdate`

``` purescript
newtype InputSchemaUpdate
  = InputSchemaUpdate { "RecordFormatUpdate" :: NullOrUndefined (RecordFormat), "RecordEncodingUpdate" :: NullOrUndefined (RecordEncoding), "RecordColumnUpdates" :: NullOrUndefined (RecordColumns) }
```

<p>Describes updates for the application's input schema.</p>

##### Instances
``` purescript
Newtype InputSchemaUpdate _
```

#### `InputStartingPosition`

``` purescript
newtype InputStartingPosition
  = InputStartingPosition String
```

##### Instances
``` purescript
Newtype InputStartingPosition _
```

#### `InputStartingPositionConfiguration`

``` purescript
newtype InputStartingPositionConfiguration
  = InputStartingPositionConfiguration { "InputStartingPosition" :: NullOrUndefined (InputStartingPosition) }
```

<p>Describes the point at which the application reads from the streaming source.</p>

##### Instances
``` purescript
Newtype InputStartingPositionConfiguration _
```

#### `InputUpdate`

``` purescript
newtype InputUpdate
  = InputUpdate { "InputId" :: Id, "NamePrefixUpdate" :: NullOrUndefined (InAppStreamName), "InputProcessingConfigurationUpdate" :: NullOrUndefined (InputProcessingConfigurationUpdate), "KinesisStreamsInputUpdate" :: NullOrUndefined (KinesisStreamsInputUpdate), "KinesisFirehoseInputUpdate" :: NullOrUndefined (KinesisFirehoseInputUpdate), "InputSchemaUpdate" :: NullOrUndefined (InputSchemaUpdate), "InputParallelismUpdate" :: NullOrUndefined (InputParallelismUpdate) }
```

<p>Describes updates to a specific input configuration (identified by the <code>InputId</code> of an application). </p>

##### Instances
``` purescript
Newtype InputUpdate _
```

#### `InputUpdates`

``` purescript
newtype InputUpdates
  = InputUpdates (Array InputUpdate)
```

##### Instances
``` purescript
Newtype InputUpdates _
```

#### `Inputs`

``` purescript
newtype Inputs
  = Inputs (Array Input)
```

##### Instances
``` purescript
Newtype Inputs _
```

#### `InvalidApplicationConfigurationException`

``` purescript
newtype InvalidApplicationConfigurationException
  = InvalidApplicationConfigurationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>User-provided application configuration is not valid.</p>

##### Instances
``` purescript
Newtype InvalidApplicationConfigurationException _
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Specified input parameter value is invalid.</p>

##### Instances
``` purescript
Newtype InvalidArgumentException _
```

#### `JSONMappingParameters`

``` purescript
newtype JSONMappingParameters
  = JSONMappingParameters { "RecordRowPath" :: RecordRowPath }
```

<p>Provides additional mapping information when JSON is the record format on the streaming source.</p>

##### Instances
``` purescript
Newtype JSONMappingParameters _
```

#### `KinesisFirehoseInput`

``` purescript
newtype KinesisFirehoseInput
  = KinesisFirehoseInput { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p> Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>

##### Instances
``` purescript
Newtype KinesisFirehoseInput _
```

#### `KinesisFirehoseInputDescription`

``` purescript
newtype KinesisFirehoseInputDescription
  = KinesisFirehoseInputDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p> Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration. </p>

##### Instances
``` purescript
Newtype KinesisFirehoseInputDescription _
```

#### `KinesisFirehoseInputUpdate`

``` purescript
newtype KinesisFirehoseInputUpdate
  = KinesisFirehoseInputUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.</p>

##### Instances
``` purescript
Newtype KinesisFirehoseInputUpdate _
```

#### `KinesisFirehoseOutput`

``` purescript
newtype KinesisFirehoseOutput
  = KinesisFirehoseOutput { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p>When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.</p>

##### Instances
``` purescript
Newtype KinesisFirehoseOutput _
```

#### `KinesisFirehoseOutputDescription`

``` purescript
newtype KinesisFirehoseOutputDescription
  = KinesisFirehoseOutputDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p> For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination. </p>

##### Instances
``` purescript
Newtype KinesisFirehoseOutputDescription _
```

#### `KinesisFirehoseOutputUpdate`

``` purescript
newtype KinesisFirehoseOutputUpdate
  = KinesisFirehoseOutputUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination. </p>

##### Instances
``` purescript
Newtype KinesisFirehoseOutputUpdate _
```

#### `KinesisStreamsInput`

``` purescript
newtype KinesisStreamsInput
  = KinesisStreamsInput { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p> Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.</p>

##### Instances
``` purescript
Newtype KinesisStreamsInput _
```

#### `KinesisStreamsInputDescription`

``` purescript
newtype KinesisStreamsInputDescription
  = KinesisStreamsInputDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p> Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration. </p>

##### Instances
``` purescript
Newtype KinesisStreamsInputDescription _
```

#### `KinesisStreamsInputUpdate`

``` purescript
newtype KinesisStreamsInputUpdate
  = KinesisStreamsInputUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.</p>

##### Instances
``` purescript
Newtype KinesisStreamsInputUpdate _
```

#### `KinesisStreamsOutput`

``` purescript
newtype KinesisStreamsOutput
  = KinesisStreamsOutput { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p>When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.</p>

##### Instances
``` purescript
Newtype KinesisStreamsOutput _
```

#### `KinesisStreamsOutputDescription`

``` purescript
newtype KinesisStreamsOutputDescription
  = KinesisStreamsOutputDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p> For an application output, describes the Amazon Kinesis stream configured as its destination. </p>

##### Instances
``` purescript
Newtype KinesisStreamsOutputDescription _
```

#### `KinesisStreamsOutputUpdate`

``` purescript
newtype KinesisStreamsOutputUpdate
  = KinesisStreamsOutputUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p> When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an Amazon Kinesis stream configured as the destination. </p>

##### Instances
``` purescript
Newtype KinesisStreamsOutputUpdate _
```

#### `LambdaOutput`

``` purescript
newtype LambdaOutput
  = LambdaOutput { "ResourceARN" :: ResourceARN, "RoleARN" :: RoleARN }
```

<p>When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf. </p>

##### Instances
``` purescript
Newtype LambdaOutput _
```

#### `LambdaOutputDescription`

``` purescript
newtype LambdaOutputDescription
  = LambdaOutputDescription { "ResourceARN" :: NullOrUndefined (ResourceARN), "RoleARN" :: NullOrUndefined (RoleARN) }
```

<p>For an application output, describes the AWS Lambda function configured as its destination. </p>

##### Instances
``` purescript
Newtype LambdaOutputDescription _
```

#### `LambdaOutputUpdate`

``` purescript
newtype LambdaOutputUpdate
  = LambdaOutputUpdate { "ResourceARNUpdate" :: NullOrUndefined (ResourceARN), "RoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>When updating an output configuration using the <a>UpdateApplication</a> operation, provides information about an AWS Lambda function configured as the destination.</p>

##### Instances
``` purescript
Newtype LambdaOutputUpdate _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Exceeded the number of applications allowed.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListApplicationsInputLimit`

``` purescript
newtype ListApplicationsInputLimit
  = ListApplicationsInputLimit Int
```

##### Instances
``` purescript
Newtype ListApplicationsInputLimit _
```

#### `ListApplicationsRequest`

``` purescript
newtype ListApplicationsRequest
  = ListApplicationsRequest { "Limit" :: NullOrUndefined (ListApplicationsInputLimit), "ExclusiveStartApplicationName" :: NullOrUndefined (ApplicationName) }
```

<p/>

##### Instances
``` purescript
Newtype ListApplicationsRequest _
```

#### `ListApplicationsResponse`

``` purescript
newtype ListApplicationsResponse
  = ListApplicationsResponse { "ApplicationSummaries" :: ApplicationSummaries, "HasMoreApplications" :: BooleanObject }
```

<p/>

##### Instances
``` purescript
Newtype ListApplicationsResponse _
```

#### `LogStreamARN`

``` purescript
newtype LogStreamARN
  = LogStreamARN String
```

##### Instances
``` purescript
Newtype LogStreamARN _
```

#### `MappingParameters`

``` purescript
newtype MappingParameters
  = MappingParameters { "JSONMappingParameters" :: NullOrUndefined (JSONMappingParameters), "CSVMappingParameters" :: NullOrUndefined (CSVMappingParameters) }
```

<p>When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.</p>

##### Instances
``` purescript
Newtype MappingParameters _
```

#### `Output`

``` purescript
newtype Output
  = Output { "Name" :: InAppStreamName, "KinesisStreamsOutput" :: NullOrUndefined (KinesisStreamsOutput), "KinesisFirehoseOutput" :: NullOrUndefined (KinesisFirehoseOutput), "LambdaOutput" :: NullOrUndefined (LambdaOutput), "DestinationSchema" :: DestinationSchema }
```

<p> Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p> <p/> <p>For limits on how many destinations an application can write and other limitations, see <a href="http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html">Limits</a>. </p>

##### Instances
``` purescript
Newtype Output _
```

#### `OutputDescription`

``` purescript
newtype OutputDescription
  = OutputDescription { "OutputId" :: NullOrUndefined (Id), "Name" :: NullOrUndefined (InAppStreamName), "KinesisStreamsOutputDescription" :: NullOrUndefined (KinesisStreamsOutputDescription), "KinesisFirehoseOutputDescription" :: NullOrUndefined (KinesisFirehoseOutputDescription), "LambdaOutputDescription" :: NullOrUndefined (LambdaOutputDescription), "DestinationSchema" :: NullOrUndefined (DestinationSchema) }
```

<p>Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream. </p>

##### Instances
``` purescript
Newtype OutputDescription _
```

#### `OutputDescriptions`

``` purescript
newtype OutputDescriptions
  = OutputDescriptions (Array OutputDescription)
```

##### Instances
``` purescript
Newtype OutputDescriptions _
```

#### `OutputUpdate`

``` purescript
newtype OutputUpdate
  = OutputUpdate { "OutputId" :: Id, "NameUpdate" :: NullOrUndefined (InAppStreamName), "KinesisStreamsOutputUpdate" :: NullOrUndefined (KinesisStreamsOutputUpdate), "KinesisFirehoseOutputUpdate" :: NullOrUndefined (KinesisFirehoseOutputUpdate), "LambdaOutputUpdate" :: NullOrUndefined (LambdaOutputUpdate), "DestinationSchemaUpdate" :: NullOrUndefined (DestinationSchema) }
```

<p> Describes updates to the output configuration identified by the <code>OutputId</code>. </p>

##### Instances
``` purescript
Newtype OutputUpdate _
```

#### `OutputUpdates`

``` purescript
newtype OutputUpdates
  = OutputUpdates (Array OutputUpdate)
```

##### Instances
``` purescript
Newtype OutputUpdates _
```

#### `Outputs`

``` purescript
newtype Outputs
  = Outputs (Array Output)
```

##### Instances
``` purescript
Newtype Outputs _
```

#### `ParsedInputRecord`

``` purescript
newtype ParsedInputRecord
  = ParsedInputRecord (Array ParsedInputRecordField)
```

##### Instances
``` purescript
Newtype ParsedInputRecord _
```

#### `ParsedInputRecordField`

``` purescript
newtype ParsedInputRecordField
  = ParsedInputRecordField String
```

##### Instances
``` purescript
Newtype ParsedInputRecordField _
```

#### `ParsedInputRecords`

``` purescript
newtype ParsedInputRecords
  = ParsedInputRecords (Array ParsedInputRecord)
```

##### Instances
``` purescript
Newtype ParsedInputRecords _
```

#### `ProcessedInputRecord`

``` purescript
newtype ProcessedInputRecord
  = ProcessedInputRecord String
```

##### Instances
``` purescript
Newtype ProcessedInputRecord _
```

#### `ProcessedInputRecords`

``` purescript
newtype ProcessedInputRecords
  = ProcessedInputRecords (Array ProcessedInputRecord)
```

##### Instances
``` purescript
Newtype ProcessedInputRecords _
```

#### `RawInputRecord`

``` purescript
newtype RawInputRecord
  = RawInputRecord String
```

##### Instances
``` purescript
Newtype RawInputRecord _
```

#### `RawInputRecords`

``` purescript
newtype RawInputRecords
  = RawInputRecords (Array RawInputRecord)
```

##### Instances
``` purescript
Newtype RawInputRecords _
```

#### `RecordColumn`

``` purescript
newtype RecordColumn
  = RecordColumn { "Name" :: RecordColumnName, "Mapping" :: NullOrUndefined (RecordColumnMapping), "SqlType" :: RecordColumnSqlType }
```

<p>Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.</p> <p>Also used to describe the format of the reference data source.</p>

##### Instances
``` purescript
Newtype RecordColumn _
```

#### `RecordColumnDelimiter`

``` purescript
newtype RecordColumnDelimiter
  = RecordColumnDelimiter String
```

##### Instances
``` purescript
Newtype RecordColumnDelimiter _
```

#### `RecordColumnMapping`

``` purescript
newtype RecordColumnMapping
  = RecordColumnMapping String
```

##### Instances
``` purescript
Newtype RecordColumnMapping _
```

#### `RecordColumnName`

``` purescript
newtype RecordColumnName
  = RecordColumnName String
```

##### Instances
``` purescript
Newtype RecordColumnName _
```

#### `RecordColumnSqlType`

``` purescript
newtype RecordColumnSqlType
  = RecordColumnSqlType String
```

##### Instances
``` purescript
Newtype RecordColumnSqlType _
```

#### `RecordColumns`

``` purescript
newtype RecordColumns
  = RecordColumns (Array RecordColumn)
```

##### Instances
``` purescript
Newtype RecordColumns _
```

#### `RecordEncoding`

``` purescript
newtype RecordEncoding
  = RecordEncoding String
```

##### Instances
``` purescript
Newtype RecordEncoding _
```

#### `RecordFormat`

``` purescript
newtype RecordFormat
  = RecordFormat { "RecordFormatType" :: RecordFormatType, "MappingParameters" :: NullOrUndefined (MappingParameters) }
```

<p> Describes the record format and relevant mapping information that should be applied to schematize the records on the stream. </p>

##### Instances
``` purescript
Newtype RecordFormat _
```

#### `RecordFormatType`

``` purescript
newtype RecordFormatType
  = RecordFormatType String
```

##### Instances
``` purescript
Newtype RecordFormatType _
```

#### `RecordRowDelimiter`

``` purescript
newtype RecordRowDelimiter
  = RecordRowDelimiter String
```

##### Instances
``` purescript
Newtype RecordRowDelimiter _
```

#### `RecordRowPath`

``` purescript
newtype RecordRowPath
  = RecordRowPath String
```

##### Instances
``` purescript
Newtype RecordRowPath _
```

#### `ReferenceDataSource`

``` purescript
newtype ReferenceDataSource
  = ReferenceDataSource { "TableName" :: InAppTableName, "S3ReferenceDataSource" :: NullOrUndefined (S3ReferenceDataSource), "ReferenceSchema" :: SourceSchema }
```

<p>Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.</p>

##### Instances
``` purescript
Newtype ReferenceDataSource _
```

#### `ReferenceDataSourceDescription`

``` purescript
newtype ReferenceDataSourceDescription
  = ReferenceDataSourceDescription { "ReferenceId" :: Id, "TableName" :: InAppTableName, "S3ReferenceDataSourceDescription" :: S3ReferenceDataSourceDescription, "ReferenceSchema" :: NullOrUndefined (SourceSchema) }
```

<p>Describes the reference data source configured for an application.</p>

##### Instances
``` purescript
Newtype ReferenceDataSourceDescription _
```

#### `ReferenceDataSourceDescriptions`

``` purescript
newtype ReferenceDataSourceDescriptions
  = ReferenceDataSourceDescriptions (Array ReferenceDataSourceDescription)
```

##### Instances
``` purescript
Newtype ReferenceDataSourceDescriptions _
```

#### `ReferenceDataSourceUpdate`

``` purescript
newtype ReferenceDataSourceUpdate
  = ReferenceDataSourceUpdate { "ReferenceId" :: Id, "TableNameUpdate" :: NullOrUndefined (InAppTableName), "S3ReferenceDataSourceUpdate" :: NullOrUndefined (S3ReferenceDataSourceUpdate), "ReferenceSchemaUpdate" :: NullOrUndefined (SourceSchema) }
```

<p>When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.</p>

##### Instances
``` purescript
Newtype ReferenceDataSourceUpdate _
```

#### `ReferenceDataSourceUpdates`

``` purescript
newtype ReferenceDataSourceUpdates
  = ReferenceDataSourceUpdates (Array ReferenceDataSourceUpdate)
```

##### Instances
``` purescript
Newtype ReferenceDataSourceUpdates _
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

##### Instances
``` purescript
Newtype ResourceARN _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Application is not available for this operation.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Specified application can't be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceProvisionedThroughputExceededException`

``` purescript
newtype ResourceProvisionedThroughputExceededException
  = ResourceProvisionedThroughputExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html">GetRecords</a> in the Amazon Kinesis Streams API Reference.</p>

##### Instances
``` purescript
Newtype ResourceProvisionedThroughputExceededException _
```

#### `RoleARN`

``` purescript
newtype RoleARN
  = RoleARN String
```

##### Instances
``` purescript
Newtype RoleARN _
```

#### `S3Configuration`

``` purescript
newtype S3Configuration
  = S3Configuration { "RoleARN" :: RoleARN, "BucketARN" :: BucketARN, "FileKey" :: FileKey }
```

<p>Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the S3 object that contains the data.</p>

##### Instances
``` purescript
Newtype S3Configuration _
```

#### `S3ReferenceDataSource`

``` purescript
newtype S3ReferenceDataSource
  = S3ReferenceDataSource { "BucketARN" :: BucketARN, "FileKey" :: FileKey, "ReferenceRoleARN" :: RoleARN }
```

<p>Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.</p> <p>An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <a>UpdateApplication</a> operation to trigger reloading of data into your application.</p>

##### Instances
``` purescript
Newtype S3ReferenceDataSource _
```

#### `S3ReferenceDataSourceDescription`

``` purescript
newtype S3ReferenceDataSourceDescription
  = S3ReferenceDataSourceDescription { "BucketARN" :: BucketARN, "FileKey" :: FileKey, "ReferenceRoleARN" :: RoleARN }
```

<p>Provides the bucket name and object key name that stores the reference data.</p>

##### Instances
``` purescript
Newtype S3ReferenceDataSourceDescription _
```

#### `S3ReferenceDataSourceUpdate`

``` purescript
newtype S3ReferenceDataSourceUpdate
  = S3ReferenceDataSourceUpdate { "BucketARNUpdate" :: NullOrUndefined (BucketARN), "FileKeyUpdate" :: NullOrUndefined (FileKey), "ReferenceRoleARNUpdate" :: NullOrUndefined (RoleARN) }
```

<p>Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.</p>

##### Instances
``` purescript
Newtype S3ReferenceDataSourceUpdate _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The service is unavailable, back off and retry the operation. </p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `SourceSchema`

``` purescript
newtype SourceSchema
  = SourceSchema { "RecordFormat" :: RecordFormat, "RecordEncoding" :: NullOrUndefined (RecordEncoding), "RecordColumns" :: RecordColumns }
```

<p>Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.</p>

##### Instances
``` purescript
Newtype SourceSchema _
```

#### `StartApplicationRequest`

``` purescript
newtype StartApplicationRequest
  = StartApplicationRequest { "ApplicationName" :: ApplicationName, "InputConfigurations" :: InputConfigurations }
```

<p/>

##### Instances
``` purescript
Newtype StartApplicationRequest _
```

#### `StartApplicationResponse`

``` purescript
newtype StartApplicationResponse
  = StartApplicationResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype StartApplicationResponse _
```

#### `StopApplicationRequest`

``` purescript
newtype StopApplicationRequest
  = StopApplicationRequest { "ApplicationName" :: ApplicationName }
```

<p/>

##### Instances
``` purescript
Newtype StopApplicationRequest _
```

#### `StopApplicationResponse`

``` purescript
newtype StopApplicationResponse
  = StopApplicationResponse {  }
```

<p/>

##### Instances
``` purescript
Newtype StopApplicationResponse _
```

#### `UnableToDetectSchemaException`

``` purescript
newtype UnableToDetectSchemaException
  = UnableToDetectSchemaException { "Message'" :: NullOrUndefined (ErrorMessage), "RawInputRecords" :: NullOrUndefined (RawInputRecords), "ProcessedInputRecords" :: NullOrUndefined (ProcessedInputRecords) }
```

<p>Data format is not valid, Amazon Kinesis Analytics is not able to detect schema for the given streaming source.</p>

##### Instances
``` purescript
Newtype UnableToDetectSchemaException _
```

#### `UpdateApplicationRequest`

``` purescript
newtype UpdateApplicationRequest
  = UpdateApplicationRequest { "ApplicationName" :: ApplicationName, "CurrentApplicationVersionId" :: ApplicationVersionId, "ApplicationUpdate" :: ApplicationUpdate }
```

##### Instances
``` purescript
Newtype UpdateApplicationRequest _
```

#### `UpdateApplicationResponse`

``` purescript
newtype UpdateApplicationResponse
  = UpdateApplicationResponse {  }
```

##### Instances
``` purescript
Newtype UpdateApplicationResponse _
```


