## Module AWS.MachineLearning

Definition of the public APIs exposed by Amazon Machine Learning

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTags`

``` purescript
addTags :: forall eff. AddTagsInput -> Aff (err :: RequestError | eff) AddTagsOutput
```

<p>Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, <code>AddTags</code> updates the tag's value.</p>

#### `createBatchPrediction`

``` purescript
createBatchPrediction :: forall eff. CreateBatchPredictionInput -> Aff (err :: RequestError | eff) CreateBatchPredictionOutput
```

<p>Generates predictions for a group of observations. The observations to process exist in one or more data files referenced by a <code>DataSource</code>. This operation creates a new <code>BatchPrediction</code>, and uses an <code>MLModel</code> and the data files referenced by the <code>DataSource</code> as information sources. </p> <p><code>CreateBatchPrediction</code> is an asynchronous operation. In response to <code>CreateBatchPrediction</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>BatchPrediction</code> status to <code>PENDING</code>. After the <code>BatchPrediction</code> completes, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can poll for status updates by using the <a>GetBatchPrediction</a> operation and checking the <code>Status</code> parameter of the result. After the <code>COMPLETED</code> status appears, the results are available in the location specified by the <code>OutputUri</code> parameter.</p>

#### `createDataSourceFromRDS`

``` purescript
createDataSourceFromRDS :: forall eff. CreateDataSourceFromRDSInput -> Aff (err :: RequestError | eff) CreateDataSourceFromRDSOutput
```

<p>Creates a <code>DataSource</code> object from an <a href="http://aws.amazon.com/rds/"> Amazon Relational Database Service</a> (Amazon RDS). A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRDS</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRDS</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used only to perform <code>&gt;CreateMLModel</code>&gt;, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML cannot accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p>

#### `createDataSourceFromRedshift`

``` purescript
createDataSourceFromRedshift :: forall eff. CreateDataSourceFromRedshiftInput -> Aff (err :: RequestError | eff) CreateDataSourceFromRedshiftOutput
```

<p>Creates a <code>DataSource</code> from a database hosted on an Amazon Redshift cluster. A <code>DataSource</code> references data that can be used to perform either <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRedshift</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRedshift</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in <code>COMPLETED</code> or <code>PENDING</code> states can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observations should be contained in the database hosted on an Amazon Redshift cluster and should be specified by a <code>SelectSqlQuery</code> query. Amazon ML executes an <code>Unload</code> command in Amazon Redshift to transfer the result set of the <code>SelectSqlQuery</code> query to <code>S3StagingLocation</code>.</p> <p>After the <code>DataSource</code> has been created, it's ready for use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also requires a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p> <?oxy_insert_start author="laurama" timestamp="20160406T153842-0700"><p>You can't change an existing datasource, but you can copy and modify the settings from an existing Amazon Redshift datasource to create a new datasource. To do so, call <code>GetDataSource</code> for an existing datasource and copy the values to a <code>CreateDataSource</code> call. Change the settings that you want to change and make sure that all required fields have the appropriate values.</p> <?oxy_insert_end>

#### `createDataSourceFromS3`

``` purescript
createDataSourceFromS3 :: forall eff. CreateDataSourceFromS3Input -> Aff (err :: RequestError | eff) CreateDataSourceFromS3Output
```

<p>Creates a <code>DataSource</code> object. A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromS3</code> is an asynchronous operation. In response to <code>CreateDataSourceFromS3</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> has been created and is ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code> or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observation data used in a <code>DataSource</code> should be ready to use; that is, it should have a consistent structure, and missing data values should be kept to a minimum. The observation data must reside in one or more .csv files in an Amazon Simple Storage Service (Amazon S3) location, along with a schema that describes the data items by name and type. The same schema must be used for all of the data files referenced by the <code>DataSource</code>. </p> <p>After the <code>DataSource</code> has been created, it's ready to use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also needs a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p>

#### `createEvaluation`

``` purescript
createEvaluation :: forall eff. CreateEvaluationInput -> Aff (err :: RequestError | eff) CreateEvaluationOutput
```

<p>Creates a new <code>Evaluation</code> of an <code>MLModel</code>. An <code>MLModel</code> is evaluated on a set of observations associated to a <code>DataSource</code>. Like a <code>DataSource</code> for an <code>MLModel</code>, the <code>DataSource</code> for an <code>Evaluation</code> contains values for the <code>Target Variable</code>. The <code>Evaluation</code> compares the predicted result for each observation to the actual outcome and provides a summary so that you know how effective the <code>MLModel</code> functions on the test data. Evaluation generates a relevant performance metric, such as BinaryAUC, RegressionRMSE or MulticlassAvgFScore based on the corresponding <code>MLModelType</code>: <code>BINARY</code>, <code>REGRESSION</code> or <code>MULTICLASS</code>. </p> <p><code>CreateEvaluation</code> is an asynchronous operation. In response to <code>CreateEvaluation</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the evaluation status to <code>PENDING</code>. After the <code>Evaluation</code> is created and ready for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetEvaluation</code> operation to check progress of the evaluation during the creation operation.</p>

#### `createMLModel`

``` purescript
createMLModel :: forall eff. CreateMLModelInput -> Aff (err :: RequestError | eff) CreateMLModelOutput
```

<p>Creates a new <code>MLModel</code> using the <code>DataSource</code> and the recipe as information sources. </p> <p>An <code>MLModel</code> is nearly immutable. Users can update only the <code>MLModelName</code> and the <code>ScoreThreshold</code> in an <code>MLModel</code> without creating a new <code>MLModel</code>. </p> <p><code>CreateMLModel</code> is an asynchronous operation. In response to <code>CreateMLModel</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>MLModel</code> status to <code>PENDING</code>. After the <code>MLModel</code> has been created and ready is for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetMLModel</code> operation to check the progress of the <code>MLModel</code> during the creation operation.</p> <p> <code>CreateMLModel</code> requires a <code>DataSource</code> with computed statistics, which can be created by setting <code>ComputeStatistics</code> to <code>true</code> in <code>CreateDataSourceFromRDS</code>, <code>CreateDataSourceFromS3</code>, or <code>CreateDataSourceFromRedshift</code> operations. </p>

#### `createRealtimeEndpoint`

``` purescript
createRealtimeEndpoint :: forall eff. CreateRealtimeEndpointInput -> Aff (err :: RequestError | eff) CreateRealtimeEndpointOutput
```

<p>Creates a real-time endpoint for the <code>MLModel</code>. The endpoint contains the URI of the <code>MLModel</code>; that is, the location to send real-time prediction requests for the specified <code>MLModel</code>.</p>

#### `deleteBatchPrediction`

``` purescript
deleteBatchPrediction :: forall eff. DeleteBatchPredictionInput -> Aff (err :: RequestError | eff) DeleteBatchPredictionOutput
```

<p>Assigns the DELETED status to a <code>BatchPrediction</code>, rendering it unusable.</p> <p>After using the <code>DeleteBatchPrediction</code> operation, you can use the <a>GetBatchPrediction</a> operation to verify that the status of the <code>BatchPrediction</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteBatchPrediction</code> operation is irreversible.</p>

#### `deleteDataSource`

``` purescript
deleteDataSource :: forall eff. DeleteDataSourceInput -> Aff (err :: RequestError | eff) DeleteDataSourceOutput
```

<p>Assigns the DELETED status to a <code>DataSource</code>, rendering it unusable.</p> <p>After using the <code>DeleteDataSource</code> operation, you can use the <a>GetDataSource</a> operation to verify that the status of the <code>DataSource</code> changed to DELETED.</p> <p><b>Caution:</b> The results of the <code>DeleteDataSource</code> operation are irreversible.</p>

#### `deleteEvaluation`

``` purescript
deleteEvaluation :: forall eff. DeleteEvaluationInput -> Aff (err :: RequestError | eff) DeleteEvaluationOutput
```

<p>Assigns the <code>DELETED</code> status to an <code>Evaluation</code>, rendering it unusable.</p> <p>After invoking the <code>DeleteEvaluation</code> operation, you can use the <code>GetEvaluation</code> operation to verify that the status of the <code>Evaluation</code> changed to <code>DELETED</code>.</p> <caution><title>Caution</title> <p>The results of the <code>DeleteEvaluation</code> operation are irreversible.</p></caution>

#### `deleteMLModel`

``` purescript
deleteMLModel :: forall eff. DeleteMLModelInput -> Aff (err :: RequestError | eff) DeleteMLModelOutput
```

<p>Assigns the <code>DELETED</code> status to an <code>MLModel</code>, rendering it unusable.</p> <p>After using the <code>DeleteMLModel</code> operation, you can use the <code>GetMLModel</code> operation to verify that the status of the <code>MLModel</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteMLModel</code> operation is irreversible.</p>

#### `deleteRealtimeEndpoint`

``` purescript
deleteRealtimeEndpoint :: forall eff. DeleteRealtimeEndpointInput -> Aff (err :: RequestError | eff) DeleteRealtimeEndpointOutput
```

<p>Deletes a real time endpoint of an <code>MLModel</code>.</p>

#### `deleteTags`

``` purescript
deleteTags :: forall eff. DeleteTagsInput -> Aff (err :: RequestError | eff) DeleteTagsOutput
```

<p>Deletes the specified tags associated with an ML object. After this operation is complete, you can't recover deleted tags.</p> <p>If you specify a tag that doesn't exist, Amazon ML ignores it.</p>

#### `describeBatchPredictions`

``` purescript
describeBatchPredictions :: forall eff. DescribeBatchPredictionsInput -> Aff (err :: RequestError | eff) DescribeBatchPredictionsOutput
```

<p>Returns a list of <code>BatchPrediction</code> operations that match the search criteria in the request.</p>

#### `describeDataSources`

``` purescript
describeDataSources :: forall eff. DescribeDataSourcesInput -> Aff (err :: RequestError | eff) DescribeDataSourcesOutput
```

<p>Returns a list of <code>DataSource</code> that match the search criteria in the request.</p>

#### `describeEvaluations`

``` purescript
describeEvaluations :: forall eff. DescribeEvaluationsInput -> Aff (err :: RequestError | eff) DescribeEvaluationsOutput
```

<p>Returns a list of <code>DescribeEvaluations</code> that match the search criteria in the request.</p>

#### `describeMLModels`

``` purescript
describeMLModels :: forall eff. DescribeMLModelsInput -> Aff (err :: RequestError | eff) DescribeMLModelsOutput
```

<p>Returns a list of <code>MLModel</code> that match the search criteria in the request.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsInput -> Aff (err :: RequestError | eff) DescribeTagsOutput
```

<p>Describes one or more of the tags for your Amazon ML object.</p>

#### `getBatchPrediction`

``` purescript
getBatchPrediction :: forall eff. GetBatchPredictionInput -> Aff (err :: RequestError | eff) GetBatchPredictionOutput
```

<p>Returns a <code>BatchPrediction</code> that includes detailed metadata, status, and data file information for a <code>Batch Prediction</code> request.</p>

#### `getDataSource`

``` purescript
getDataSource :: forall eff. GetDataSourceInput -> Aff (err :: RequestError | eff) GetDataSourceOutput
```

<p>Returns a <code>DataSource</code> that includes metadata and data file information, as well as the current status of the <code>DataSource</code>.</p> <p><code>GetDataSource</code> provides results in normal or verbose format. The verbose format adds the schema description and the list of files pointed to by the DataSource to the normal format.</p>

#### `getEvaluation`

``` purescript
getEvaluation :: forall eff. GetEvaluationInput -> Aff (err :: RequestError | eff) GetEvaluationOutput
```

<p>Returns an <code>Evaluation</code> that includes metadata as well as the current status of the <code>Evaluation</code>.</p>

#### `getMLModel`

``` purescript
getMLModel :: forall eff. GetMLModelInput -> Aff (err :: RequestError | eff) GetMLModelOutput
```

<p>Returns an <code>MLModel</code> that includes detailed metadata, data source information, and the current status of the <code>MLModel</code>.</p> <p><code>GetMLModel</code> provides results in normal or verbose format. </p>

#### `predict`

``` purescript
predict :: forall eff. PredictInput -> Aff (err :: RequestError | eff) PredictOutput
```

<p>Generates a prediction for the observation using the specified <code>ML Model</code>.</p> <note><title>Note</title> <p>Not all response parameters will be populated. Whether a response parameter is populated depends on the type of model requested.</p></note>

#### `updateBatchPrediction`

``` purescript
updateBatchPrediction :: forall eff. UpdateBatchPredictionInput -> Aff (err :: RequestError | eff) UpdateBatchPredictionOutput
```

<p>Updates the <code>BatchPredictionName</code> of a <code>BatchPrediction</code>.</p> <p>You can use the <code>GetBatchPrediction</code> operation to view the contents of the updated data element.</p>

#### `updateDataSource`

``` purescript
updateDataSource :: forall eff. UpdateDataSourceInput -> Aff (err :: RequestError | eff) UpdateDataSourceOutput
```

<p>Updates the <code>DataSourceName</code> of a <code>DataSource</code>.</p> <p>You can use the <code>GetDataSource</code> operation to view the contents of the updated data element.</p>

#### `updateEvaluation`

``` purescript
updateEvaluation :: forall eff. UpdateEvaluationInput -> Aff (err :: RequestError | eff) UpdateEvaluationOutput
```

<p>Updates the <code>EvaluationName</code> of an <code>Evaluation</code>.</p> <p>You can use the <code>GetEvaluation</code> operation to view the contents of the updated data element.</p>

#### `updateMLModel`

``` purescript
updateMLModel :: forall eff. UpdateMLModelInput -> Aff (err :: RequestError | eff) UpdateMLModelOutput
```

<p>Updates the <code>MLModelName</code> and the <code>ScoreThreshold</code> of an <code>MLModel</code>.</p> <p>You can use the <code>GetMLModel</code> operation to view the contents of the updated data element.</p>

#### `AddTagsInput`

``` purescript
newtype AddTagsInput
  = AddTagsInput { "Tags" :: TagList, "ResourceId" :: EntityId, "ResourceType" :: TaggableResourceType }
```

##### Instances
``` purescript
Newtype AddTagsInput _
```

#### `AddTagsOutput`

``` purescript
newtype AddTagsOutput
  = AddTagsOutput { "ResourceId" :: NullOrUndefined (EntityId), "ResourceType" :: NullOrUndefined (TaggableResourceType) }
```

<p>Amazon ML returns the following elements. </p>

##### Instances
``` purescript
Newtype AddTagsOutput _
```

#### `Algorithm`

``` purescript
newtype Algorithm
  = Algorithm String
```

<p>The function used to train an <code>MLModel</code>. Training choices supported by Amazon ML include the following:</p> <ul> <li> <code>SGD</code> - Stochastic Gradient Descent.</li> <li> <code>RandomForest</code> - Random forest of decision trees.</li> </ul>

##### Instances
``` purescript
Newtype Algorithm _
```

#### `AwsUserArn`

``` purescript
newtype AwsUserArn
  = AwsUserArn String
```

<p>An Amazon Web Service (AWS) user account identifier. The account identifier can be an AWS root account or an AWS Identity and Access Management (IAM) user.</p>

##### Instances
``` purescript
Newtype AwsUserArn _
```

#### `BatchPrediction`

``` purescript
newtype BatchPrediction
  = BatchPrediction { "BatchPredictionId" :: NullOrUndefined (EntityId), "MLModelId" :: NullOrUndefined (EntityId), "BatchPredictionDataSourceId" :: NullOrUndefined (EntityId), "InputDataLocationS3" :: NullOrUndefined (S3Url), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "OutputUri" :: NullOrUndefined (S3Url), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime), "TotalRecordCount" :: NullOrUndefined (LongType), "InvalidRecordCount" :: NullOrUndefined (LongType) }
```

<p> Represents the output of a <code>GetBatchPrediction</code> operation.</p> <p> The content consists of the detailed metadata, the status, and the data file information of a <code>Batch Prediction</code>.</p>

##### Instances
``` purescript
Newtype BatchPrediction _
```

#### `BatchPredictionFilterVariable`

``` purescript
newtype BatchPredictionFilterVariable
  = BatchPredictionFilterVariable String
```

<p>A list of the variables to use in searching or filtering <code>BatchPrediction</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>BatchPrediction</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>BatchPrediction</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>BatchPrediction</code><b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>BatchPrediction</code> creation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>MLModel</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataURI</code> - Sets the search criteria to the data file(s) used in the <code>BatchPrediction</code>. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>

##### Instances
``` purescript
Newtype BatchPredictionFilterVariable _
```

#### `BatchPredictions`

``` purescript
newtype BatchPredictions
  = BatchPredictions (Array BatchPrediction)
```

##### Instances
``` purescript
Newtype BatchPredictions _
```

#### `ComparatorValue`

``` purescript
newtype ComparatorValue
  = ComparatorValue String
```

<p>The value specified in a filtering condition. The <code>ComparatorValue</code> becomes the reference value when matching or evaluating data values in filtering and searching functions.</p>

##### Instances
``` purescript
Newtype ComparatorValue _
```

#### `ComputeStatistics`

``` purescript
newtype ComputeStatistics
  = ComputeStatistics Boolean
```

##### Instances
``` purescript
Newtype ComputeStatistics _
```

#### `CreateBatchPredictionInput`

``` purescript
newtype CreateBatchPredictionInput
  = CreateBatchPredictionInput { "BatchPredictionId" :: EntityId, "BatchPredictionName" :: NullOrUndefined (EntityName), "MLModelId" :: EntityId, "BatchPredictionDataSourceId" :: EntityId, "OutputUri" :: S3Url }
```

##### Instances
``` purescript
Newtype CreateBatchPredictionInput _
```

#### `CreateBatchPredictionOutput`

``` purescript
newtype CreateBatchPredictionOutput
  = CreateBatchPredictionOutput { "BatchPredictionId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateBatchPrediction</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateBatchPrediction</code> operation is asynchronous. You can poll for status updates by using the <code>&gt;GetBatchPrediction</code> operation and checking the <code>Status</code> parameter of the result. </p>

##### Instances
``` purescript
Newtype CreateBatchPredictionOutput _
```

#### `CreateDataSourceFromRDSInput`

``` purescript
newtype CreateDataSourceFromRDSInput
  = CreateDataSourceFromRDSInput { "DataSourceId" :: EntityId, "DataSourceName" :: NullOrUndefined (EntityName), "RDSData" :: RDSDataSpec, "RoleARN" :: RoleARN, "ComputeStatistics" :: NullOrUndefined (ComputeStatistics) }
```

##### Instances
``` purescript
Newtype CreateDataSourceFromRDSInput _
```

#### `CreateDataSourceFromRDSOutput`

``` purescript
newtype CreateDataSourceFromRDSOutput
  = CreateDataSourceFromRDSOutput { "DataSourceId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateDataSourceFromRDS</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRDS</code>&gt; operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. You can inspect the <code>Message</code> when <code>Status</code> shows up as <code>FAILED</code>. You can also check the progress of the copy operation by going to the <code>DataPipeline</code> console and looking up the pipeline using the <code>pipelineId </code> from the describe call.</p>

##### Instances
``` purescript
Newtype CreateDataSourceFromRDSOutput _
```

#### `CreateDataSourceFromRedshiftInput`

``` purescript
newtype CreateDataSourceFromRedshiftInput
  = CreateDataSourceFromRedshiftInput { "DataSourceId" :: EntityId, "DataSourceName" :: NullOrUndefined (EntityName), "DataSpec" :: RedshiftDataSpec, "RoleARN" :: RoleARN, "ComputeStatistics" :: NullOrUndefined (ComputeStatistics) }
```

##### Instances
``` purescript
Newtype CreateDataSourceFromRedshiftInput _
```

#### `CreateDataSourceFromRedshiftOutput`

``` purescript
newtype CreateDataSourceFromRedshiftOutput
  = CreateDataSourceFromRedshiftOutput { "DataSourceId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateDataSourceFromRedshift</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRedshift</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>

##### Instances
``` purescript
Newtype CreateDataSourceFromRedshiftOutput _
```

#### `CreateDataSourceFromS3Input`

``` purescript
newtype CreateDataSourceFromS3Input
  = CreateDataSourceFromS3Input { "DataSourceId" :: EntityId, "DataSourceName" :: NullOrUndefined (EntityName), "DataSpec" :: S3DataSpec, "ComputeStatistics" :: NullOrUndefined (ComputeStatistics) }
```

##### Instances
``` purescript
Newtype CreateDataSourceFromS3Input _
```

#### `CreateDataSourceFromS3Output`

``` purescript
newtype CreateDataSourceFromS3Output
  = CreateDataSourceFromS3Output { "DataSourceId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateDataSourceFromS3</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromS3</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>

##### Instances
``` purescript
Newtype CreateDataSourceFromS3Output _
```

#### `CreateEvaluationInput`

``` purescript
newtype CreateEvaluationInput
  = CreateEvaluationInput { "EvaluationId" :: EntityId, "EvaluationName" :: NullOrUndefined (EntityName), "MLModelId" :: EntityId, "EvaluationDataSourceId" :: EntityId }
```

##### Instances
``` purescript
Newtype CreateEvaluationInput _
```

#### `CreateEvaluationOutput`

``` purescript
newtype CreateEvaluationOutput
  = CreateEvaluationOutput { "EvaluationId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateEvaluation</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p><code>CreateEvaluation</code> operation is asynchronous. You can poll for status updates by using the <code>GetEvcaluation</code> operation and checking the <code>Status</code> parameter. </p>

##### Instances
``` purescript
Newtype CreateEvaluationOutput _
```

#### `CreateMLModelInput`

``` purescript
newtype CreateMLModelInput
  = CreateMLModelInput { "MLModelId" :: EntityId, "MLModelName" :: NullOrUndefined (EntityName), "MLModelType" :: MLModelType, "Parameters" :: NullOrUndefined (TrainingParameters), "TrainingDataSourceId" :: EntityId, "Recipe" :: NullOrUndefined (Recipe), "RecipeUri" :: NullOrUndefined (S3Url) }
```

##### Instances
``` purescript
Newtype CreateMLModelInput _
```

#### `CreateMLModelOutput`

``` purescript
newtype CreateMLModelOutput
  = CreateMLModelOutput { "MLModelId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>CreateMLModel</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateMLModel</code> operation is asynchronous. You can poll for status updates by using the <code>GetMLModel</code> operation and checking the <code>Status</code> parameter. </p>

##### Instances
``` purescript
Newtype CreateMLModelOutput _
```

#### `CreateRealtimeEndpointInput`

``` purescript
newtype CreateRealtimeEndpointInput
  = CreateRealtimeEndpointInput { "MLModelId" :: EntityId }
```

##### Instances
``` purescript
Newtype CreateRealtimeEndpointInput _
```

#### `CreateRealtimeEndpointOutput`

``` purescript
newtype CreateRealtimeEndpointOutput
  = CreateRealtimeEndpointOutput { "MLModelId" :: NullOrUndefined (EntityId), "RealtimeEndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo) }
```

<p>Represents the output of an <code>CreateRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>.</p> <note> <p>The endpoint information includes the URI of the <code>MLModel</code>; that is, the location to send online prediction requests for the specified <code>MLModel</code>.</p> </note>

##### Instances
``` purescript
Newtype CreateRealtimeEndpointOutput _
```

#### `DataRearrangement`

``` purescript
newtype DataRearrangement
  = DataRearrangement String
```

##### Instances
``` purescript
Newtype DataRearrangement _
```

#### `DataSchema`

``` purescript
newtype DataSchema
  = DataSchema String
```

<p>The schema of a <code>DataSource</code>. The <code>DataSchema</code> defines the structure of the observation data in the data file(s) referenced in the <code>DataSource</code>. The DataSource schema is expressed in JSON format.</p> <p><code>DataSchema</code> is not required if you specify a <code>DataSchemaUri</code></p> <p>{ "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "variables": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] } </p>

##### Instances
``` purescript
Newtype DataSchema _
```

#### `DataSource`

``` purescript
newtype DataSource
  = DataSource { "DataSourceId" :: NullOrUndefined (EntityId), "DataLocationS3" :: NullOrUndefined (S3Url), "DataRearrangement" :: NullOrUndefined (DataRearrangement), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "DataSizeInBytes" :: NullOrUndefined (LongType), "NumberOfFiles" :: NullOrUndefined (LongType), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "Message" :: NullOrUndefined (Message), "RedshiftMetadata" :: NullOrUndefined (RedshiftMetadata), "RDSMetadata" :: NullOrUndefined (RDSMetadata), "RoleARN" :: NullOrUndefined (RoleARN), "ComputeStatistics" :: NullOrUndefined (ComputeStatistics), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime) }
```

<p> Represents the output of the <code>GetDataSource</code> operation. </p> <p> The content consists of the detailed metadata and data file information and the current status of the <code>DataSource</code>. </p>

##### Instances
``` purescript
Newtype DataSource _
```

#### `DataSourceFilterVariable`

``` purescript
newtype DataSourceFilterVariable
  = DataSourceFilterVariable String
```

<p>A list of the variables to use in searching or filtering <code>DataSource</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>DataSource</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>DataSource</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>DataSource</code> <b> </b> <code>Name</code>.</li> <li> <code>DataUri</code> - Sets the search criteria to the URI of data files used to create the <code>DataSource</code>. The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>DataSource</code> creation.</li> </ul> <note><title>Note</title> <p>The variable names should match the variable names in the <code>DataSource</code>.</p> </note>

##### Instances
``` purescript
Newtype DataSourceFilterVariable _
```

#### `DataSources`

``` purescript
newtype DataSources
  = DataSources (Array DataSource)
```

##### Instances
``` purescript
Newtype DataSources _
```

#### `DeleteBatchPredictionInput`

``` purescript
newtype DeleteBatchPredictionInput
  = DeleteBatchPredictionInput { "BatchPredictionId" :: EntityId }
```

##### Instances
``` purescript
Newtype DeleteBatchPredictionInput _
```

#### `DeleteBatchPredictionOutput`

``` purescript
newtype DeleteBatchPredictionOutput
  = DeleteBatchPredictionOutput { "BatchPredictionId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>DeleteBatchPrediction</code> operation.</p> <p>You can use the <code>GetBatchPrediction</code> operation and check the value of the <code>Status</code> parameter to see whether a <code>BatchPrediction</code> is marked as <code>DELETED</code>.</p>

##### Instances
``` purescript
Newtype DeleteBatchPredictionOutput _
```

#### `DeleteDataSourceInput`

``` purescript
newtype DeleteDataSourceInput
  = DeleteDataSourceInput { "DataSourceId" :: EntityId }
```

##### Instances
``` purescript
Newtype DeleteDataSourceInput _
```

#### `DeleteDataSourceOutput`

``` purescript
newtype DeleteDataSourceOutput
  = DeleteDataSourceOutput { "DataSourceId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>DeleteDataSource</code> operation.</p>

##### Instances
``` purescript
Newtype DeleteDataSourceOutput _
```

#### `DeleteEvaluationInput`

``` purescript
newtype DeleteEvaluationInput
  = DeleteEvaluationInput { "EvaluationId" :: EntityId }
```

##### Instances
``` purescript
Newtype DeleteEvaluationInput _
```

#### `DeleteEvaluationOutput`

``` purescript
newtype DeleteEvaluationOutput
  = DeleteEvaluationOutput { "EvaluationId" :: NullOrUndefined (EntityId) }
```

<p> Represents the output of a <code>DeleteEvaluation</code> operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.</p> <p>You can use the <code>GetEvaluation</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>Evaluation</code> is marked as <code>DELETED</code>.</p>

##### Instances
``` purescript
Newtype DeleteEvaluationOutput _
```

#### `DeleteMLModelInput`

``` purescript
newtype DeleteMLModelInput
  = DeleteMLModelInput { "MLModelId" :: EntityId }
```

##### Instances
``` purescript
Newtype DeleteMLModelInput _
```

#### `DeleteMLModelOutput`

``` purescript
newtype DeleteMLModelOutput
  = DeleteMLModelOutput { "MLModelId" :: NullOrUndefined (EntityId) }
```

<p>Represents the output of a <code>DeleteMLModel</code> operation.</p> <p>You can use the <code>GetMLModel</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>MLModel</code> is marked as <code>DELETED</code>.</p>

##### Instances
``` purescript
Newtype DeleteMLModelOutput _
```

#### `DeleteRealtimeEndpointInput`

``` purescript
newtype DeleteRealtimeEndpointInput
  = DeleteRealtimeEndpointInput { "MLModelId" :: EntityId }
```

##### Instances
``` purescript
Newtype DeleteRealtimeEndpointInput _
```

#### `DeleteRealtimeEndpointOutput`

``` purescript
newtype DeleteRealtimeEndpointOutput
  = DeleteRealtimeEndpointOutput { "MLModelId" :: NullOrUndefined (EntityId), "RealtimeEndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo) }
```

<p>Represents the output of an <code>DeleteRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>. </p>

##### Instances
``` purescript
Newtype DeleteRealtimeEndpointOutput _
```

#### `DeleteTagsInput`

``` purescript
newtype DeleteTagsInput
  = DeleteTagsInput { "TagKeys" :: TagKeyList, "ResourceId" :: EntityId, "ResourceType" :: TaggableResourceType }
```

##### Instances
``` purescript
Newtype DeleteTagsInput _
```

#### `DeleteTagsOutput`

``` purescript
newtype DeleteTagsOutput
  = DeleteTagsOutput { "ResourceId" :: NullOrUndefined (EntityId), "ResourceType" :: NullOrUndefined (TaggableResourceType) }
```

<p>Amazon ML returns the following elements. </p>

##### Instances
``` purescript
Newtype DeleteTagsOutput _
```

#### `DescribeBatchPredictionsInput`

``` purescript
newtype DescribeBatchPredictionsInput
  = DescribeBatchPredictionsInput { "FilterVariable" :: NullOrUndefined (BatchPredictionFilterVariable), "EQ" :: NullOrUndefined (ComparatorValue), "GT" :: NullOrUndefined (ComparatorValue), "LT" :: NullOrUndefined (ComparatorValue), "GE" :: NullOrUndefined (ComparatorValue), "LE" :: NullOrUndefined (ComparatorValue), "NE" :: NullOrUndefined (ComparatorValue), "Prefix" :: NullOrUndefined (ComparatorValue), "SortOrder" :: NullOrUndefined (SortOrder), "NextToken" :: NullOrUndefined (StringType), "Limit" :: NullOrUndefined (PageLimit) }
```

##### Instances
``` purescript
Newtype DescribeBatchPredictionsInput _
```

#### `DescribeBatchPredictionsOutput`

``` purescript
newtype DescribeBatchPredictionsOutput
  = DescribeBatchPredictionsOutput { "Results" :: NullOrUndefined (BatchPredictions), "NextToken" :: NullOrUndefined (StringType) }
```

<p>Represents the output of a <code>DescribeBatchPredictions</code> operation. The content is essentially a list of <code>BatchPrediction</code>s.</p>

##### Instances
``` purescript
Newtype DescribeBatchPredictionsOutput _
```

#### `DescribeDataSourcesInput`

``` purescript
newtype DescribeDataSourcesInput
  = DescribeDataSourcesInput { "FilterVariable" :: NullOrUndefined (DataSourceFilterVariable), "EQ" :: NullOrUndefined (ComparatorValue), "GT" :: NullOrUndefined (ComparatorValue), "LT" :: NullOrUndefined (ComparatorValue), "GE" :: NullOrUndefined (ComparatorValue), "LE" :: NullOrUndefined (ComparatorValue), "NE" :: NullOrUndefined (ComparatorValue), "Prefix" :: NullOrUndefined (ComparatorValue), "SortOrder" :: NullOrUndefined (SortOrder), "NextToken" :: NullOrUndefined (StringType), "Limit" :: NullOrUndefined (PageLimit) }
```

##### Instances
``` purescript
Newtype DescribeDataSourcesInput _
```

#### `DescribeDataSourcesOutput`

``` purescript
newtype DescribeDataSourcesOutput
  = DescribeDataSourcesOutput { "Results" :: NullOrUndefined (DataSources), "NextToken" :: NullOrUndefined (StringType) }
```

<p>Represents the query results from a <a>DescribeDataSources</a> operation. The content is essentially a list of <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype DescribeDataSourcesOutput _
```

#### `DescribeEvaluationsInput`

``` purescript
newtype DescribeEvaluationsInput
  = DescribeEvaluationsInput { "FilterVariable" :: NullOrUndefined (EvaluationFilterVariable), "EQ" :: NullOrUndefined (ComparatorValue), "GT" :: NullOrUndefined (ComparatorValue), "LT" :: NullOrUndefined (ComparatorValue), "GE" :: NullOrUndefined (ComparatorValue), "LE" :: NullOrUndefined (ComparatorValue), "NE" :: NullOrUndefined (ComparatorValue), "Prefix" :: NullOrUndefined (ComparatorValue), "SortOrder" :: NullOrUndefined (SortOrder), "NextToken" :: NullOrUndefined (StringType), "Limit" :: NullOrUndefined (PageLimit) }
```

##### Instances
``` purescript
Newtype DescribeEvaluationsInput _
```

#### `DescribeEvaluationsOutput`

``` purescript
newtype DescribeEvaluationsOutput
  = DescribeEvaluationsOutput { "Results" :: NullOrUndefined (Evaluations), "NextToken" :: NullOrUndefined (StringType) }
```

<p>Represents the query results from a <code>DescribeEvaluations</code> operation. The content is essentially a list of <code>Evaluation</code>.</p>

##### Instances
``` purescript
Newtype DescribeEvaluationsOutput _
```

#### `DescribeMLModelsInput`

``` purescript
newtype DescribeMLModelsInput
  = DescribeMLModelsInput { "FilterVariable" :: NullOrUndefined (MLModelFilterVariable), "EQ" :: NullOrUndefined (ComparatorValue), "GT" :: NullOrUndefined (ComparatorValue), "LT" :: NullOrUndefined (ComparatorValue), "GE" :: NullOrUndefined (ComparatorValue), "LE" :: NullOrUndefined (ComparatorValue), "NE" :: NullOrUndefined (ComparatorValue), "Prefix" :: NullOrUndefined (ComparatorValue), "SortOrder" :: NullOrUndefined (SortOrder), "NextToken" :: NullOrUndefined (StringType), "Limit" :: NullOrUndefined (PageLimit) }
```

##### Instances
``` purescript
Newtype DescribeMLModelsInput _
```

#### `DescribeMLModelsOutput`

``` purescript
newtype DescribeMLModelsOutput
  = DescribeMLModelsOutput { "Results" :: NullOrUndefined (MLModels), "NextToken" :: NullOrUndefined (StringType) }
```

<p>Represents the output of a <code>DescribeMLModels</code> operation. The content is essentially a list of <code>MLModel</code>.</p>

##### Instances
``` purescript
Newtype DescribeMLModelsOutput _
```

#### `DescribeTagsInput`

``` purescript
newtype DescribeTagsInput
  = DescribeTagsInput { "ResourceId" :: EntityId, "ResourceType" :: TaggableResourceType }
```

##### Instances
``` purescript
Newtype DescribeTagsInput _
```

#### `DescribeTagsOutput`

``` purescript
newtype DescribeTagsOutput
  = DescribeTagsOutput { "ResourceId" :: NullOrUndefined (EntityId), "ResourceType" :: NullOrUndefined (TaggableResourceType), "Tags" :: NullOrUndefined (TagList) }
```

<p>Amazon ML returns the following elements. </p>

##### Instances
``` purescript
Newtype DescribeTagsOutput _
```

#### `DetailsAttributes`

``` purescript
newtype DetailsAttributes
  = DetailsAttributes String
```

Contains the key values of <code>DetailsMap</code>: <code>PredictiveModelType</code> - Indicates the type of the <code>MLModel</code>. <code>Algorithm</code> - Indicates the algorithm that was used for the <code>MLModel</code>.

##### Instances
``` purescript
Newtype DetailsAttributes _
```

#### `DetailsMap`

``` purescript
newtype DetailsMap
  = DetailsMap (Map DetailsAttributes DetailsValue)
```

Provides any additional details regarding the prediction.

##### Instances
``` purescript
Newtype DetailsMap _
```

#### `DetailsValue`

``` purescript
newtype DetailsValue
  = DetailsValue String
```

##### Instances
``` purescript
Newtype DetailsValue _
```

#### `EDPPipelineId`

``` purescript
newtype EDPPipelineId
  = EDPPipelineId String
```

##### Instances
``` purescript
Newtype EDPPipelineId _
```

#### `EDPResourceRole`

``` purescript
newtype EDPResourceRole
  = EDPResourceRole String
```

##### Instances
``` purescript
Newtype EDPResourceRole _
```

#### `EDPSecurityGroupId`

``` purescript
newtype EDPSecurityGroupId
  = EDPSecurityGroupId String
```

##### Instances
``` purescript
Newtype EDPSecurityGroupId _
```

#### `EDPSecurityGroupIds`

``` purescript
newtype EDPSecurityGroupIds
  = EDPSecurityGroupIds (Array EDPSecurityGroupId)
```

##### Instances
``` purescript
Newtype EDPSecurityGroupIds _
```

#### `EDPServiceRole`

``` purescript
newtype EDPServiceRole
  = EDPServiceRole String
```

##### Instances
``` purescript
Newtype EDPServiceRole _
```

#### `EDPSubnetId`

``` purescript
newtype EDPSubnetId
  = EDPSubnetId String
```

##### Instances
``` purescript
Newtype EDPSubnetId _
```

#### `EntityId`

``` purescript
newtype EntityId
  = EntityId String
```

##### Instances
``` purescript
Newtype EntityId _
```

#### `EntityName`

``` purescript
newtype EntityName
  = EntityName String
```

<p>A user-supplied name or description of the Amazon ML resource.</p>

##### Instances
``` purescript
Newtype EntityName _
```

#### `EntityStatus`

``` purescript
newtype EntityStatus
  = EntityStatus String
```

<p>Object status with the following possible values:</p> <ul> <li><code>PENDING</code></li> <li><code>INPROGRESS</code></li> <li><code>FAILED</code></li> <li><code>COMPLETED</code></li> <li><code>DELETED</code></li> </ul>

##### Instances
``` purescript
Newtype EntityStatus _
```

#### `EpochTime`

``` purescript
newtype EpochTime
  = EpochTime Number
```

<p>A timestamp represented in epoch time.</p>

##### Instances
``` purescript
Newtype EpochTime _
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode Int
```

##### Instances
``` purescript
Newtype ErrorCode _
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

#### `Evaluation`

``` purescript
newtype Evaluation
  = Evaluation { "EvaluationId" :: NullOrUndefined (EntityId), "MLModelId" :: NullOrUndefined (EntityId), "EvaluationDataSourceId" :: NullOrUndefined (EntityId), "InputDataLocationS3" :: NullOrUndefined (S3Url), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "PerformanceMetrics" :: NullOrUndefined (PerformanceMetrics), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime) }
```

<p> Represents the output of <code>GetEvaluation</code> operation. </p> <p>The content consists of the detailed metadata and data file information and the current status of the <code>Evaluation</code>.</p>

##### Instances
``` purescript
Newtype Evaluation _
```

#### `EvaluationFilterVariable`

``` purescript
newtype EvaluationFilterVariable
  = EvaluationFilterVariable String
```

<p>A list of the variables to use in searching or filtering <code>Evaluation</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>Evaluation</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>Evaluation</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>Evaluation</code> <b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked an evaluation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>Predictor</code> that was evaluated.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in evaluation.</li> <li> <code>DataUri</code> - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>

##### Instances
``` purescript
Newtype EvaluationFilterVariable _
```

#### `Evaluations`

``` purescript
newtype Evaluations
  = Evaluations (Array Evaluation)
```

##### Instances
``` purescript
Newtype Evaluations _
```

#### `GetBatchPredictionInput`

``` purescript
newtype GetBatchPredictionInput
  = GetBatchPredictionInput { "BatchPredictionId" :: EntityId }
```

##### Instances
``` purescript
Newtype GetBatchPredictionInput _
```

#### `GetBatchPredictionOutput`

``` purescript
newtype GetBatchPredictionOutput
  = GetBatchPredictionOutput { "BatchPredictionId" :: NullOrUndefined (EntityId), "MLModelId" :: NullOrUndefined (EntityId), "BatchPredictionDataSourceId" :: NullOrUndefined (EntityId), "InputDataLocationS3" :: NullOrUndefined (S3Url), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "OutputUri" :: NullOrUndefined (S3Url), "LogUri" :: NullOrUndefined (PresignedS3Url), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime), "TotalRecordCount" :: NullOrUndefined (LongType), "InvalidRecordCount" :: NullOrUndefined (LongType) }
```

<p>Represents the output of a <code>GetBatchPrediction</code> operation and describes a <code>BatchPrediction</code>.</p>

##### Instances
``` purescript
Newtype GetBatchPredictionOutput _
```

#### `GetDataSourceInput`

``` purescript
newtype GetDataSourceInput
  = GetDataSourceInput { "DataSourceId" :: EntityId, "Verbose" :: NullOrUndefined (Verbose) }
```

##### Instances
``` purescript
Newtype GetDataSourceInput _
```

#### `GetDataSourceOutput`

``` purescript
newtype GetDataSourceOutput
  = GetDataSourceOutput { "DataSourceId" :: NullOrUndefined (EntityId), "DataLocationS3" :: NullOrUndefined (S3Url), "DataRearrangement" :: NullOrUndefined (DataRearrangement), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "DataSizeInBytes" :: NullOrUndefined (LongType), "NumberOfFiles" :: NullOrUndefined (LongType), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "LogUri" :: NullOrUndefined (PresignedS3Url), "Message" :: NullOrUndefined (Message), "RedshiftMetadata" :: NullOrUndefined (RedshiftMetadata), "RDSMetadata" :: NullOrUndefined (RDSMetadata), "RoleARN" :: NullOrUndefined (RoleARN), "ComputeStatistics" :: NullOrUndefined (ComputeStatistics), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime), "DataSourceSchema" :: NullOrUndefined (DataSchema) }
```

<p>Represents the output of a <code>GetDataSource</code> operation and describes a <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype GetDataSourceOutput _
```

#### `GetEvaluationInput`

``` purescript
newtype GetEvaluationInput
  = GetEvaluationInput { "EvaluationId" :: EntityId }
```

##### Instances
``` purescript
Newtype GetEvaluationInput _
```

#### `GetEvaluationOutput`

``` purescript
newtype GetEvaluationOutput
  = GetEvaluationOutput { "EvaluationId" :: NullOrUndefined (EntityId), "MLModelId" :: NullOrUndefined (EntityId), "EvaluationDataSourceId" :: NullOrUndefined (EntityId), "InputDataLocationS3" :: NullOrUndefined (S3Url), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (EntityName), "Status" :: NullOrUndefined (EntityStatus), "PerformanceMetrics" :: NullOrUndefined (PerformanceMetrics), "LogUri" :: NullOrUndefined (PresignedS3Url), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime) }
```

<p>Represents the output of a <code>GetEvaluation</code> operation and describes an <code>Evaluation</code>.</p>

##### Instances
``` purescript
Newtype GetEvaluationOutput _
```

#### `GetMLModelInput`

``` purescript
newtype GetMLModelInput
  = GetMLModelInput { "MLModelId" :: EntityId, "Verbose" :: NullOrUndefined (Verbose) }
```

##### Instances
``` purescript
Newtype GetMLModelInput _
```

#### `GetMLModelOutput`

``` purescript
newtype GetMLModelOutput
  = GetMLModelOutput { "MLModelId" :: NullOrUndefined (EntityId), "TrainingDataSourceId" :: NullOrUndefined (EntityId), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (MLModelName), "Status" :: NullOrUndefined (EntityStatus), "SizeInBytes" :: NullOrUndefined (LongType), "EndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo), "TrainingParameters" :: NullOrUndefined (TrainingParameters), "InputDataLocationS3" :: NullOrUndefined (S3Url), "MLModelType" :: NullOrUndefined (MLModelType), "ScoreThreshold" :: NullOrUndefined (ScoreThreshold), "ScoreThresholdLastUpdatedAt" :: NullOrUndefined (EpochTime), "LogUri" :: NullOrUndefined (PresignedS3Url), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime), "Recipe" :: NullOrUndefined (Recipe), "Schema" :: NullOrUndefined (DataSchema) }
```

<p>Represents the output of a <code>GetMLModel</code> operation, and provides detailed information about a <code>MLModel</code>.</p>

##### Instances
``` purescript
Newtype GetMLModelOutput _
```

#### `IdempotentParameterMismatchException`

``` purescript
newtype IdempotentParameterMismatchException
  = IdempotentParameterMismatchException { "Message'" :: NullOrUndefined (ErrorMessage), "Code'" :: NullOrUndefined (ErrorCode) }
```

<p>A second request to use or change an object was not allowed. This can result from retrying a request using a parameter that was not present in the original request.</p>

##### Instances
``` purescript
Newtype IdempotentParameterMismatchException _
```

#### `IntegerType`

``` purescript
newtype IntegerType
  = IntegerType Int
```

<p>Integer type that is a 32-bit signed number.</p>

##### Instances
``` purescript
Newtype IntegerType _
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message'" :: NullOrUndefined (ErrorMessage), "Code'" :: NullOrUndefined (ErrorCode) }
```

<p>An error on the server occurred when trying to process a request.</p>

##### Instances
``` purescript
Newtype InternalServerException _
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message'" :: NullOrUndefined (ErrorMessage), "Code'" :: NullOrUndefined (ErrorCode) }
```

<p>An error on the client occurred. Typically, the cause is an invalid input value.</p>

##### Instances
``` purescript
Newtype InvalidInputException _
```

#### `InvalidTagException`

``` purescript
newtype InvalidTagException
  = InvalidTagException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

##### Instances
``` purescript
Newtype InvalidTagException _
```

#### `Label`

``` purescript
newtype Label
  = Label String
```

##### Instances
``` purescript
Newtype Label _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage), "Code'" :: NullOrUndefined (ErrorCode) }
```

<p>The subscriber exceeded the maximum number of operations. This exception can occur when listing objects such as <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `LongType`

``` purescript
newtype LongType
  = LongType Number
```

<p>Long integer type that is a 64-bit signed number.</p>

##### Instances
``` purescript
Newtype LongType _
```

#### `MLModel`

``` purescript
newtype MLModel
  = MLModel { "MLModelId" :: NullOrUndefined (EntityId), "TrainingDataSourceId" :: NullOrUndefined (EntityId), "CreatedByIamUser" :: NullOrUndefined (AwsUserArn), "CreatedAt" :: NullOrUndefined (EpochTime), "LastUpdatedAt" :: NullOrUndefined (EpochTime), "Name" :: NullOrUndefined (MLModelName), "Status" :: NullOrUndefined (EntityStatus), "SizeInBytes" :: NullOrUndefined (LongType), "EndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo), "TrainingParameters" :: NullOrUndefined (TrainingParameters), "InputDataLocationS3" :: NullOrUndefined (S3Url), "Algorithm" :: NullOrUndefined (Algorithm), "MLModelType" :: NullOrUndefined (MLModelType), "ScoreThreshold" :: NullOrUndefined (ScoreThreshold), "ScoreThresholdLastUpdatedAt" :: NullOrUndefined (EpochTime), "Message" :: NullOrUndefined (Message), "ComputeTime" :: NullOrUndefined (LongType), "FinishedAt" :: NullOrUndefined (EpochTime), "StartedAt" :: NullOrUndefined (EpochTime) }
```

<p> Represents the output of a <code>GetMLModel</code> operation. </p> <p>The content consists of the detailed metadata and the current status of the <code>MLModel</code>.</p>

##### Instances
``` purescript
Newtype MLModel _
```

#### `MLModelFilterVariable`

``` purescript
newtype MLModelFilterVariable
  = MLModelFilterVariable String
```

##### Instances
``` purescript
Newtype MLModelFilterVariable _
```

#### `MLModelName`

``` purescript
newtype MLModelName
  = MLModelName String
```

##### Instances
``` purescript
Newtype MLModelName _
```

#### `MLModelType`

``` purescript
newtype MLModelType
  = MLModelType String
```

##### Instances
``` purescript
Newtype MLModelType _
```

#### `MLModels`

``` purescript
newtype MLModels
  = MLModels (Array MLModel)
```

##### Instances
``` purescript
Newtype MLModels _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

<p> Description of the most recent details about an object.</p>

##### Instances
``` purescript
Newtype Message _
```

#### `PageLimit`

``` purescript
newtype PageLimit
  = PageLimit Int
```

##### Instances
``` purescript
Newtype PageLimit _
```

#### `PerformanceMetrics`

``` purescript
newtype PerformanceMetrics
  = PerformanceMetrics { "Properties" :: NullOrUndefined (PerformanceMetricsProperties) }
```

<p>Measurements of how well the <code>MLModel</code> performed on known observations. One of the following metrics is returned, based on the type of the <code>MLModel</code>: </p> <ul> <li> <p>BinaryAUC: The binary <code>MLModel</code> uses the Area Under the Curve (AUC) technique to measure performance. </p> </li> <li> <p>RegressionRMSE: The regression <code>MLModel</code> uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.</p> </li> <li> <p>MulticlassAvgFScore: The multiclass <code>MLModel</code> uses the F1 score technique to measure performance. </p> </li> </ul> <p> For more information about performance metrics, please see the <a href="http://docs.aws.amazon.com/machine-learning/latest/dg">Amazon Machine Learning Developer Guide</a>. </p>

##### Instances
``` purescript
Newtype PerformanceMetrics _
```

#### `PerformanceMetricsProperties`

``` purescript
newtype PerformanceMetricsProperties
  = PerformanceMetricsProperties (Map PerformanceMetricsPropertyKey PerformanceMetricsPropertyValue)
```

##### Instances
``` purescript
Newtype PerformanceMetricsProperties _
```

#### `PerformanceMetricsPropertyKey`

``` purescript
newtype PerformanceMetricsPropertyKey
  = PerformanceMetricsPropertyKey String
```

##### Instances
``` purescript
Newtype PerformanceMetricsPropertyKey _
```

#### `PerformanceMetricsPropertyValue`

``` purescript
newtype PerformanceMetricsPropertyValue
  = PerformanceMetricsPropertyValue String
```

##### Instances
``` purescript
Newtype PerformanceMetricsPropertyValue _
```

#### `PredictInput`

``` purescript
newtype PredictInput
  = PredictInput { "MLModelId" :: EntityId, "Record''" :: Record'', "PredictEndpoint" :: VipURL }
```

##### Instances
``` purescript
Newtype PredictInput _
```

#### `PredictOutput`

``` purescript
newtype PredictOutput
  = PredictOutput { "Prediction" :: NullOrUndefined (Prediction) }
```

##### Instances
``` purescript
Newtype PredictOutput _
```

#### `Prediction`

``` purescript
newtype Prediction
  = Prediction { "PredictedLabel'" :: NullOrUndefined (Label), "PredictedValue'" :: NullOrUndefined (FloatLabel'), "PredictedScores'" :: NullOrUndefined (ScoreValuePerLabelMap), "Details'" :: NullOrUndefined (DetailsMap) }
```

<p>The output from a <code>Predict</code> operation: </p> <ul> <li> <p> <code>Details</code> - Contains the following attributes: <code>DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS</code> <code>DetailsAttributes.ALGORITHM - SGD</code> </p> </li> <li> <p> <code>PredictedLabel</code> - Present for either a <code>BINARY</code> or <code>MULTICLASS</code> <code>MLModel</code> request. </p> </li> <li> <p> <code>PredictedScores</code> - Contains the raw classification score corresponding to each label. </p> </li> <li> <p> <code>PredictedValue</code> - Present for a <code>REGRESSION</code> <code>MLModel</code> request. </p> </li> </ul>

##### Instances
``` purescript
Newtype Prediction _
```

#### `PredictorNotMountedException`

``` purescript
newtype PredictorNotMountedException
  = PredictorNotMountedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The exception is thrown when a predict request is made to an unmounted <code>MLModel</code>.</p>

##### Instances
``` purescript
Newtype PredictorNotMountedException _
```

#### `PresignedS3Url`

``` purescript
newtype PresignedS3Url
  = PresignedS3Url String
```

##### Instances
``` purescript
Newtype PresignedS3Url _
```

#### `RDSDataSpec`

``` purescript
newtype RDSDataSpec
  = RDSDataSpec { "DatabaseInformation" :: RDSDatabase, "SelectSqlQuery" :: RDSSelectSqlQuery, "DatabaseCredentials" :: RDSDatabaseCredentials, "S3StagingLocation" :: S3Url, "DataRearrangement" :: NullOrUndefined (DataRearrangement), "DataSchema" :: NullOrUndefined (DataSchema), "DataSchemaUri" :: NullOrUndefined (S3Url), "ResourceRole" :: EDPResourceRole, "ServiceRole" :: EDPServiceRole, "SubnetId" :: EDPSubnetId, "SecurityGroupIds" :: EDPSecurityGroupIds }
```

<p>The data specification of an Amazon Relational Database Service (Amazon RDS) <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype RDSDataSpec _
```

#### `RDSDatabase`

``` purescript
newtype RDSDatabase
  = RDSDatabase { "InstanceIdentifier" :: RDSInstanceIdentifier, "DatabaseName" :: RDSDatabaseName }
```

<p>The database details of an Amazon RDS database.</p>

##### Instances
``` purescript
Newtype RDSDatabase _
```

#### `RDSDatabaseCredentials`

``` purescript
newtype RDSDatabaseCredentials
  = RDSDatabaseCredentials { "Username" :: RDSDatabaseUsername, "Password" :: RDSDatabasePassword }
```

<p>The database credentials to connect to a database on an RDS DB instance.</p>

##### Instances
``` purescript
Newtype RDSDatabaseCredentials _
```

#### `RDSDatabaseName`

``` purescript
newtype RDSDatabaseName
  = RDSDatabaseName String
```

<p>The name of a database hosted on an RDS DB instance.</p>

##### Instances
``` purescript
Newtype RDSDatabaseName _
```

#### `RDSDatabasePassword`

``` purescript
newtype RDSDatabasePassword
  = RDSDatabasePassword String
```

<p>The password to be used by Amazon ML to connect to a database on an RDS DB instance. The password should have sufficient permissions to execute the <code>RDSSelectQuery</code> query.</p>

##### Instances
``` purescript
Newtype RDSDatabasePassword _
```

#### `RDSDatabaseUsername`

``` purescript
newtype RDSDatabaseUsername
  = RDSDatabaseUsername String
```

<p>The username to be used by Amazon ML to connect to database on an Amazon RDS instance. The username should have sufficient permissions to execute an <code>RDSSelectSqlQuery</code> query.</p>

##### Instances
``` purescript
Newtype RDSDatabaseUsername _
```

#### `RDSInstanceIdentifier`

``` purescript
newtype RDSInstanceIdentifier
  = RDSInstanceIdentifier String
```

Identifier of RDS DB Instances.

##### Instances
``` purescript
Newtype RDSInstanceIdentifier _
```

#### `RDSMetadata`

``` purescript
newtype RDSMetadata
  = RDSMetadata { "Database" :: NullOrUndefined (RDSDatabase), "DatabaseUserName" :: NullOrUndefined (RDSDatabaseUsername), "SelectSqlQuery" :: NullOrUndefined (RDSSelectSqlQuery), "ResourceRole" :: NullOrUndefined (EDPResourceRole), "ServiceRole" :: NullOrUndefined (EDPServiceRole), "DataPipelineId" :: NullOrUndefined (EDPPipelineId) }
```

<p>The datasource details that are specific to Amazon RDS.</p>

##### Instances
``` purescript
Newtype RDSMetadata _
```

#### `RDSSelectSqlQuery`

``` purescript
newtype RDSSelectSqlQuery
  = RDSSelectSqlQuery String
```

<p>The SQL query to be executed against the Amazon RDS database. The SQL query should be valid for the Amazon RDS type being used. </p>

##### Instances
``` purescript
Newtype RDSSelectSqlQuery _
```

#### `RealtimeEndpointInfo`

``` purescript
newtype RealtimeEndpointInfo
  = RealtimeEndpointInfo { "PeakRequestsPerSecond" :: NullOrUndefined (IntegerType), "CreatedAt" :: NullOrUndefined (EpochTime), "EndpointUrl" :: NullOrUndefined (VipURL), "EndpointStatus" :: NullOrUndefined (RealtimeEndpointStatus) }
```

<p> Describes the real-time endpoint information for an <code>MLModel</code>.</p>

##### Instances
``` purescript
Newtype RealtimeEndpointInfo _
```

#### `RealtimeEndpointStatus`

``` purescript
newtype RealtimeEndpointStatus
  = RealtimeEndpointStatus String
```

##### Instances
``` purescript
Newtype RealtimeEndpointStatus _
```

#### `Recipe`

``` purescript
newtype Recipe
  = Recipe String
```

##### Instances
``` purescript
Newtype Recipe _
```

#### `Record''`

``` purescript
newtype Record''
  = Record'' (Map VariableName VariableValue)
```

<p>A map of variable name-value pairs that represent an observation.</p>

##### Instances
``` purescript
Newtype Record'' _
```

#### `RedshiftClusterIdentifier`

``` purescript
newtype RedshiftClusterIdentifier
  = RedshiftClusterIdentifier String
```

<p>The ID of an Amazon Redshift cluster.</p>

##### Instances
``` purescript
Newtype RedshiftClusterIdentifier _
```

#### `RedshiftDataSpec`

``` purescript
newtype RedshiftDataSpec
  = RedshiftDataSpec { "DatabaseInformation" :: RedshiftDatabase, "SelectSqlQuery" :: RedshiftSelectSqlQuery, "DatabaseCredentials" :: RedshiftDatabaseCredentials, "S3StagingLocation" :: S3Url, "DataRearrangement" :: NullOrUndefined (DataRearrangement), "DataSchema" :: NullOrUndefined (DataSchema), "DataSchemaUri" :: NullOrUndefined (S3Url) }
```

<p>Describes the data specification of an Amazon Redshift <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype RedshiftDataSpec _
```

#### `RedshiftDatabase`

``` purescript
newtype RedshiftDatabase
  = RedshiftDatabase { "DatabaseName" :: RedshiftDatabaseName, "ClusterIdentifier" :: RedshiftClusterIdentifier }
```

<p>Describes the database details required to connect to an Amazon Redshift database.</p>

##### Instances
``` purescript
Newtype RedshiftDatabase _
```

#### `RedshiftDatabaseCredentials`

``` purescript
newtype RedshiftDatabaseCredentials
  = RedshiftDatabaseCredentials { "Username" :: RedshiftDatabaseUsername, "Password" :: RedshiftDatabasePassword }
```

<p> Describes the database credentials for connecting to a database on an Amazon Redshift cluster.</p>

##### Instances
``` purescript
Newtype RedshiftDatabaseCredentials _
```

#### `RedshiftDatabaseName`

``` purescript
newtype RedshiftDatabaseName
  = RedshiftDatabaseName String
```

<p>The name of a database hosted on an Amazon Redshift cluster.</p>

##### Instances
``` purescript
Newtype RedshiftDatabaseName _
```

#### `RedshiftDatabasePassword`

``` purescript
newtype RedshiftDatabasePassword
  = RedshiftDatabasePassword String
```

<p>A password to be used by Amazon ML to connect to a database on an Amazon Redshift cluster. The password should have sufficient permissions to execute a <code>RedshiftSelectSqlQuery</code> query. The password should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>

##### Instances
``` purescript
Newtype RedshiftDatabasePassword _
```

#### `RedshiftDatabaseUsername`

``` purescript
newtype RedshiftDatabaseUsername
  = RedshiftDatabaseUsername String
```

<p>A username to be used by Amazon Machine Learning (Amazon ML)to connect to a database on an Amazon Redshift cluster. The username should have sufficient permissions to execute the <code>RedshiftSelectSqlQuery</code> query. The username should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>

##### Instances
``` purescript
Newtype RedshiftDatabaseUsername _
```

#### `RedshiftMetadata`

``` purescript
newtype RedshiftMetadata
  = RedshiftMetadata { "RedshiftDatabase" :: NullOrUndefined (RedshiftDatabase), "DatabaseUserName" :: NullOrUndefined (RedshiftDatabaseUsername), "SelectSqlQuery" :: NullOrUndefined (RedshiftSelectSqlQuery) }
```

<p>Describes the <code>DataSource</code> details specific to Amazon Redshift.</p>

##### Instances
``` purescript
Newtype RedshiftMetadata _
```

#### `RedshiftSelectSqlQuery`

``` purescript
newtype RedshiftSelectSqlQuery
  = RedshiftSelectSqlQuery String
```

<p> Describes the SQL query to execute on the Amazon Redshift database. The SQL query should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_SELECT_synopsis.html">SELECT</a>. </p>

##### Instances
``` purescript
Newtype RedshiftSelectSqlQuery _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage), "Code'" :: NullOrUndefined (ErrorCode) }
```

<p>A specified resource cannot be located.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `RoleARN`

``` purescript
newtype RoleARN
  = RoleARN String
```

<p>The Amazon Resource Name (ARN) of an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html#roles-about-termsandconcepts">AWS IAM Role</a>, such as the following: arn:aws:iam::account:role/rolename. </p>

##### Instances
``` purescript
Newtype RoleARN _
```

#### `S3DataSpec`

``` purescript
newtype S3DataSpec
  = S3DataSpec { "DataLocationS3" :: S3Url, "DataRearrangement" :: NullOrUndefined (DataRearrangement), "DataSchema" :: NullOrUndefined (DataSchema), "DataSchemaLocationS3" :: NullOrUndefined (S3Url) }
```

<p> Describes the data specification of a <code>DataSource</code>.</p>

##### Instances
``` purescript
Newtype S3DataSpec _
```

#### `S3Url`

``` purescript
newtype S3Url
  = S3Url String
```

<p>A reference to a file or bucket on Amazon Simple Storage Service (Amazon S3).</p>

##### Instances
``` purescript
Newtype S3Url _
```

#### `ScoreThreshold`

``` purescript
newtype ScoreThreshold
  = ScoreThreshold Number
```

##### Instances
``` purescript
Newtype ScoreThreshold _
```

#### `ScoreValue`

``` purescript
newtype ScoreValue
  = ScoreValue Number
```

##### Instances
``` purescript
Newtype ScoreValue _
```

#### `ScoreValuePerLabelMap`

``` purescript
newtype ScoreValuePerLabelMap
  = ScoreValuePerLabelMap (Map Label ScoreValue)
```

Provides the raw classification score corresponding to each label.

##### Instances
``` purescript
Newtype ScoreValuePerLabelMap _
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

<p>The sort order specified in a listing condition. Possible values include the following:</p> <ul> <li> <code>asc</code> - Present the information in ascending order (from A-Z).</li> <li> <code>dsc</code> - Present the information in descending order (from Z-A).</li> </ul>

##### Instances
``` purescript
Newtype SortOrder _
```

#### `StringType`

``` purescript
newtype StringType
  = StringType String
```

<p>String type.</p>

##### Instances
``` purescript
Newtype StringType _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>A custom key-value pair associated with an ML object, such as an ML model.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagLimitExceededException`

``` purescript
newtype TagLimitExceededException
  = TagLimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

##### Instances
``` purescript
Newtype TagLimitExceededException _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `TaggableResourceType`

``` purescript
newtype TaggableResourceType
  = TaggableResourceType String
```

##### Instances
``` purescript
Newtype TaggableResourceType _
```

#### `TrainingParameters`

``` purescript
newtype TrainingParameters
  = TrainingParameters (Map StringType StringType)
```

##### Instances
``` purescript
Newtype TrainingParameters _
```

#### `UpdateBatchPredictionInput`

``` purescript
newtype UpdateBatchPredictionInput
  = UpdateBatchPredictionInput { "BatchPredictionId" :: EntityId, "BatchPredictionName" :: EntityName }
```

##### Instances
``` purescript
Newtype UpdateBatchPredictionInput _
```

#### `UpdateBatchPredictionOutput`

``` purescript
newtype UpdateBatchPredictionOutput
  = UpdateBatchPredictionOutput { "BatchPredictionId" :: NullOrUndefined (EntityId) }
```

<p>Represents the output of an <code>UpdateBatchPrediction</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>

##### Instances
``` purescript
Newtype UpdateBatchPredictionOutput _
```

#### `UpdateDataSourceInput`

``` purescript
newtype UpdateDataSourceInput
  = UpdateDataSourceInput { "DataSourceId" :: EntityId, "DataSourceName" :: EntityName }
```

##### Instances
``` purescript
Newtype UpdateDataSourceInput _
```

#### `UpdateDataSourceOutput`

``` purescript
newtype UpdateDataSourceOutput
  = UpdateDataSourceOutput { "DataSourceId" :: NullOrUndefined (EntityId) }
```

<p>Represents the output of an <code>UpdateDataSource</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>

##### Instances
``` purescript
Newtype UpdateDataSourceOutput _
```

#### `UpdateEvaluationInput`

``` purescript
newtype UpdateEvaluationInput
  = UpdateEvaluationInput { "EvaluationId" :: EntityId, "EvaluationName" :: EntityName }
```

##### Instances
``` purescript
Newtype UpdateEvaluationInput _
```

#### `UpdateEvaluationOutput`

``` purescript
newtype UpdateEvaluationOutput
  = UpdateEvaluationOutput { "EvaluationId" :: NullOrUndefined (EntityId) }
```

<p>Represents the output of an <code>UpdateEvaluation</code> operation.</p> <p>You can see the updated content by using the <code>GetEvaluation</code> operation.</p>

##### Instances
``` purescript
Newtype UpdateEvaluationOutput _
```

#### `UpdateMLModelInput`

``` purescript
newtype UpdateMLModelInput
  = UpdateMLModelInput { "MLModelId" :: EntityId, "MLModelName" :: NullOrUndefined (EntityName), "ScoreThreshold" :: NullOrUndefined (ScoreThreshold) }
```

##### Instances
``` purescript
Newtype UpdateMLModelInput _
```

#### `UpdateMLModelOutput`

``` purescript
newtype UpdateMLModelOutput
  = UpdateMLModelOutput { "MLModelId" :: NullOrUndefined (EntityId) }
```

<p>Represents the output of an <code>UpdateMLModel</code> operation.</p> <p>You can see the updated content by using the <code>GetMLModel</code> operation.</p>

##### Instances
``` purescript
Newtype UpdateMLModelOutput _
```

#### `VariableName`

``` purescript
newtype VariableName
  = VariableName String
```

<p>The name of a variable. Currently it's used to specify the name of the target value, label, weight, and tags.</p>

##### Instances
``` purescript
Newtype VariableName _
```

#### `VariableValue`

``` purescript
newtype VariableValue
  = VariableValue String
```

<p>The value of a variable. Currently it's used to specify values of the target value, weights, and tag variables and for filtering variable values.</p>

##### Instances
``` purescript
Newtype VariableValue _
```

#### `Verbose`

``` purescript
newtype Verbose
  = Verbose Boolean
```

<p>Specifies whether a describe operation should return exhaustive or abbreviated information.</p>

##### Instances
``` purescript
Newtype Verbose _
```

#### `VipURL`

``` purescript
newtype VipURL
  = VipURL String
```

##### Instances
``` purescript
Newtype VipURL _
```

#### `FloatLabel'`

``` purescript
newtype FloatLabel'
  = FloatLabel' Number
```

##### Instances
``` purescript
Newtype FloatLabel' _
```


