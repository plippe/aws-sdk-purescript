

-- | Definition of the public APIs exposed by Amazon Machine Learning
module AWS.MachineLearning where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MachineLearning" :: String


-- | <p>Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, <code>AddTags</code> updates the tag's value.</p>
addTags :: forall eff. AddTagsInput -> Aff (err :: AWS.RequestError | eff) AddTagsOutput
addTags = AWS.request serviceName "addTags" 


-- | <p>Generates predictions for a group of observations. The observations to process exist in one or more data files referenced by a <code>DataSource</code>. This operation creates a new <code>BatchPrediction</code>, and uses an <code>MLModel</code> and the data files referenced by the <code>DataSource</code> as information sources. </p> <p><code>CreateBatchPrediction</code> is an asynchronous operation. In response to <code>CreateBatchPrediction</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>BatchPrediction</code> status to <code>PENDING</code>. After the <code>BatchPrediction</code> completes, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can poll for status updates by using the <a>GetBatchPrediction</a> operation and checking the <code>Status</code> parameter of the result. After the <code>COMPLETED</code> status appears, the results are available in the location specified by the <code>OutputUri</code> parameter.</p>
createBatchPrediction :: forall eff. CreateBatchPredictionInput -> Aff (err :: AWS.RequestError | eff) CreateBatchPredictionOutput
createBatchPrediction = AWS.request serviceName "createBatchPrediction" 


-- | <p>Creates a <code>DataSource</code> object from an <a href="http://aws.amazon.com/rds/"> Amazon Relational Database Service</a> (Amazon RDS). A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRDS</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRDS</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used only to perform <code>&gt;CreateMLModel</code>&gt;, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML cannot accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p>
createDataSourceFromRDS :: forall eff. CreateDataSourceFromRDSInput -> Aff (err :: AWS.RequestError | eff) CreateDataSourceFromRDSOutput
createDataSourceFromRDS = AWS.request serviceName "createDataSourceFromRDS" 


-- | <p>Creates a <code>DataSource</code> from a database hosted on an Amazon Redshift cluster. A <code>DataSource</code> references data that can be used to perform either <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRedshift</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRedshift</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in <code>COMPLETED</code> or <code>PENDING</code> states can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observations should be contained in the database hosted on an Amazon Redshift cluster and should be specified by a <code>SelectSqlQuery</code> query. Amazon ML executes an <code>Unload</code> command in Amazon Redshift to transfer the result set of the <code>SelectSqlQuery</code> query to <code>S3StagingLocation</code>.</p> <p>After the <code>DataSource</code> has been created, it's ready for use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also requires a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p> <?oxy_insert_start author="laurama" timestamp="20160406T153842-0700"><p>You can't change an existing datasource, but you can copy and modify the settings from an existing Amazon Redshift datasource to create a new datasource. To do so, call <code>GetDataSource</code> for an existing datasource and copy the values to a <code>CreateDataSource</code> call. Change the settings that you want to change and make sure that all required fields have the appropriate values.</p> <?oxy_insert_end>
createDataSourceFromRedshift :: forall eff. CreateDataSourceFromRedshiftInput -> Aff (err :: AWS.RequestError | eff) CreateDataSourceFromRedshiftOutput
createDataSourceFromRedshift = AWS.request serviceName "createDataSourceFromRedshift" 


-- | <p>Creates a <code>DataSource</code> object. A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromS3</code> is an asynchronous operation. In response to <code>CreateDataSourceFromS3</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> has been created and is ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code> or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observation data used in a <code>DataSource</code> should be ready to use; that is, it should have a consistent structure, and missing data values should be kept to a minimum. The observation data must reside in one or more .csv files in an Amazon Simple Storage Service (Amazon S3) location, along with a schema that describes the data items by name and type. The same schema must be used for all of the data files referenced by the <code>DataSource</code>. </p> <p>After the <code>DataSource</code> has been created, it's ready to use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also needs a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p>
createDataSourceFromS3 :: forall eff. CreateDataSourceFromS3Input -> Aff (err :: AWS.RequestError | eff) CreateDataSourceFromS3Output
createDataSourceFromS3 = AWS.request serviceName "createDataSourceFromS3" 


-- | <p>Creates a new <code>Evaluation</code> of an <code>MLModel</code>. An <code>MLModel</code> is evaluated on a set of observations associated to a <code>DataSource</code>. Like a <code>DataSource</code> for an <code>MLModel</code>, the <code>DataSource</code> for an <code>Evaluation</code> contains values for the <code>Target Variable</code>. The <code>Evaluation</code> compares the predicted result for each observation to the actual outcome and provides a summary so that you know how effective the <code>MLModel</code> functions on the test data. Evaluation generates a relevant performance metric, such as BinaryAUC, RegressionRMSE or MulticlassAvgFScore based on the corresponding <code>MLModelType</code>: <code>BINARY</code>, <code>REGRESSION</code> or <code>MULTICLASS</code>. </p> <p><code>CreateEvaluation</code> is an asynchronous operation. In response to <code>CreateEvaluation</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the evaluation status to <code>PENDING</code>. After the <code>Evaluation</code> is created and ready for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetEvaluation</code> operation to check progress of the evaluation during the creation operation.</p>
createEvaluation :: forall eff. CreateEvaluationInput -> Aff (err :: AWS.RequestError | eff) CreateEvaluationOutput
createEvaluation = AWS.request serviceName "createEvaluation" 


-- | <p>Creates a new <code>MLModel</code> using the <code>DataSource</code> and the recipe as information sources. </p> <p>An <code>MLModel</code> is nearly immutable. Users can update only the <code>MLModelName</code> and the <code>ScoreThreshold</code> in an <code>MLModel</code> without creating a new <code>MLModel</code>. </p> <p><code>CreateMLModel</code> is an asynchronous operation. In response to <code>CreateMLModel</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>MLModel</code> status to <code>PENDING</code>. After the <code>MLModel</code> has been created and ready is for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetMLModel</code> operation to check the progress of the <code>MLModel</code> during the creation operation.</p> <p> <code>CreateMLModel</code> requires a <code>DataSource</code> with computed statistics, which can be created by setting <code>ComputeStatistics</code> to <code>true</code> in <code>CreateDataSourceFromRDS</code>, <code>CreateDataSourceFromS3</code>, or <code>CreateDataSourceFromRedshift</code> operations. </p>
createMLModel :: forall eff. CreateMLModelInput -> Aff (err :: AWS.RequestError | eff) CreateMLModelOutput
createMLModel = AWS.request serviceName "createMLModel" 


-- | <p>Creates a real-time endpoint for the <code>MLModel</code>. The endpoint contains the URI of the <code>MLModel</code>; that is, the location to send real-time prediction requests for the specified <code>MLModel</code>.</p>
createRealtimeEndpoint :: forall eff. CreateRealtimeEndpointInput -> Aff (err :: AWS.RequestError | eff) CreateRealtimeEndpointOutput
createRealtimeEndpoint = AWS.request serviceName "createRealtimeEndpoint" 


-- | <p>Assigns the DELETED status to a <code>BatchPrediction</code>, rendering it unusable.</p> <p>After using the <code>DeleteBatchPrediction</code> operation, you can use the <a>GetBatchPrediction</a> operation to verify that the status of the <code>BatchPrediction</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteBatchPrediction</code> operation is irreversible.</p>
deleteBatchPrediction :: forall eff. DeleteBatchPredictionInput -> Aff (err :: AWS.RequestError | eff) DeleteBatchPredictionOutput
deleteBatchPrediction = AWS.request serviceName "deleteBatchPrediction" 


-- | <p>Assigns the DELETED status to a <code>DataSource</code>, rendering it unusable.</p> <p>After using the <code>DeleteDataSource</code> operation, you can use the <a>GetDataSource</a> operation to verify that the status of the <code>DataSource</code> changed to DELETED.</p> <p><b>Caution:</b> The results of the <code>DeleteDataSource</code> operation are irreversible.</p>
deleteDataSource :: forall eff. DeleteDataSourceInput -> Aff (err :: AWS.RequestError | eff) DeleteDataSourceOutput
deleteDataSource = AWS.request serviceName "deleteDataSource" 


-- | <p>Assigns the <code>DELETED</code> status to an <code>Evaluation</code>, rendering it unusable.</p> <p>After invoking the <code>DeleteEvaluation</code> operation, you can use the <code>GetEvaluation</code> operation to verify that the status of the <code>Evaluation</code> changed to <code>DELETED</code>.</p> <caution><title>Caution</title> <p>The results of the <code>DeleteEvaluation</code> operation are irreversible.</p></caution>
deleteEvaluation :: forall eff. DeleteEvaluationInput -> Aff (err :: AWS.RequestError | eff) DeleteEvaluationOutput
deleteEvaluation = AWS.request serviceName "deleteEvaluation" 


-- | <p>Assigns the <code>DELETED</code> status to an <code>MLModel</code>, rendering it unusable.</p> <p>After using the <code>DeleteMLModel</code> operation, you can use the <code>GetMLModel</code> operation to verify that the status of the <code>MLModel</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteMLModel</code> operation is irreversible.</p>
deleteMLModel :: forall eff. DeleteMLModelInput -> Aff (err :: AWS.RequestError | eff) DeleteMLModelOutput
deleteMLModel = AWS.request serviceName "deleteMLModel" 


-- | <p>Deletes a real time endpoint of an <code>MLModel</code>.</p>
deleteRealtimeEndpoint :: forall eff. DeleteRealtimeEndpointInput -> Aff (err :: AWS.RequestError | eff) DeleteRealtimeEndpointOutput
deleteRealtimeEndpoint = AWS.request serviceName "deleteRealtimeEndpoint" 


-- | <p>Deletes the specified tags associated with an ML object. After this operation is complete, you can't recover deleted tags.</p> <p>If you specify a tag that doesn't exist, Amazon ML ignores it.</p>
deleteTags :: forall eff. DeleteTagsInput -> Aff (err :: AWS.RequestError | eff) DeleteTagsOutput
deleteTags = AWS.request serviceName "deleteTags" 


-- | <p>Returns a list of <code>BatchPrediction</code> operations that match the search criteria in the request.</p>
describeBatchPredictions :: forall eff. DescribeBatchPredictionsInput -> Aff (err :: AWS.RequestError | eff) DescribeBatchPredictionsOutput
describeBatchPredictions = AWS.request serviceName "describeBatchPredictions" 


-- | <p>Returns a list of <code>DataSource</code> that match the search criteria in the request.</p>
describeDataSources :: forall eff. DescribeDataSourcesInput -> Aff (err :: AWS.RequestError | eff) DescribeDataSourcesOutput
describeDataSources = AWS.request serviceName "describeDataSources" 


-- | <p>Returns a list of <code>DescribeEvaluations</code> that match the search criteria in the request.</p>
describeEvaluations :: forall eff. DescribeEvaluationsInput -> Aff (err :: AWS.RequestError | eff) DescribeEvaluationsOutput
describeEvaluations = AWS.request serviceName "describeEvaluations" 


-- | <p>Returns a list of <code>MLModel</code> that match the search criteria in the request.</p>
describeMLModels :: forall eff. DescribeMLModelsInput -> Aff (err :: AWS.RequestError | eff) DescribeMLModelsOutput
describeMLModels = AWS.request serviceName "describeMLModels" 


-- | <p>Describes one or more of the tags for your Amazon ML object.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (err :: AWS.RequestError | eff) DescribeTagsOutput
describeTags = AWS.request serviceName "describeTags" 


-- | <p>Returns a <code>BatchPrediction</code> that includes detailed metadata, status, and data file information for a <code>Batch Prediction</code> request.</p>
getBatchPrediction :: forall eff. GetBatchPredictionInput -> Aff (err :: AWS.RequestError | eff) GetBatchPredictionOutput
getBatchPrediction = AWS.request serviceName "getBatchPrediction" 


-- | <p>Returns a <code>DataSource</code> that includes metadata and data file information, as well as the current status of the <code>DataSource</code>.</p> <p><code>GetDataSource</code> provides results in normal or verbose format. The verbose format adds the schema description and the list of files pointed to by the DataSource to the normal format.</p>
getDataSource :: forall eff. GetDataSourceInput -> Aff (err :: AWS.RequestError | eff) GetDataSourceOutput
getDataSource = AWS.request serviceName "getDataSource" 


-- | <p>Returns an <code>Evaluation</code> that includes metadata as well as the current status of the <code>Evaluation</code>.</p>
getEvaluation :: forall eff. GetEvaluationInput -> Aff (err :: AWS.RequestError | eff) GetEvaluationOutput
getEvaluation = AWS.request serviceName "getEvaluation" 


-- | <p>Returns an <code>MLModel</code> that includes detailed metadata, data source information, and the current status of the <code>MLModel</code>.</p> <p><code>GetMLModel</code> provides results in normal or verbose format. </p>
getMLModel :: forall eff. GetMLModelInput -> Aff (err :: AWS.RequestError | eff) GetMLModelOutput
getMLModel = AWS.request serviceName "getMLModel" 


-- | <p>Generates a prediction for the observation using the specified <code>ML Model</code>.</p> <note><title>Note</title> <p>Not all response parameters will be populated. Whether a response parameter is populated depends on the type of model requested.</p></note>
predict :: forall eff. PredictInput -> Aff (err :: AWS.RequestError | eff) PredictOutput
predict = AWS.request serviceName "predict" 


-- | <p>Updates the <code>BatchPredictionName</code> of a <code>BatchPrediction</code>.</p> <p>You can use the <code>GetBatchPrediction</code> operation to view the contents of the updated data element.</p>
updateBatchPrediction :: forall eff. UpdateBatchPredictionInput -> Aff (err :: AWS.RequestError | eff) UpdateBatchPredictionOutput
updateBatchPrediction = AWS.request serviceName "updateBatchPrediction" 


-- | <p>Updates the <code>DataSourceName</code> of a <code>DataSource</code>.</p> <p>You can use the <code>GetDataSource</code> operation to view the contents of the updated data element.</p>
updateDataSource :: forall eff. UpdateDataSourceInput -> Aff (err :: AWS.RequestError | eff) UpdateDataSourceOutput
updateDataSource = AWS.request serviceName "updateDataSource" 


-- | <p>Updates the <code>EvaluationName</code> of an <code>Evaluation</code>.</p> <p>You can use the <code>GetEvaluation</code> operation to view the contents of the updated data element.</p>
updateEvaluation :: forall eff. UpdateEvaluationInput -> Aff (err :: AWS.RequestError | eff) UpdateEvaluationOutput
updateEvaluation = AWS.request serviceName "updateEvaluation" 


-- | <p>Updates the <code>MLModelName</code> and the <code>ScoreThreshold</code> of an <code>MLModel</code>.</p> <p>You can use the <code>GetMLModel</code> operation to view the contents of the updated data element.</p>
updateMLModel :: forall eff. UpdateMLModelInput -> Aff (err :: AWS.RequestError | eff) UpdateMLModelOutput
updateMLModel = AWS.request serviceName "updateMLModel" 


newtype AddTagsInput = AddTagsInput 
  { "Tags" :: (TagList)
  , "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _


-- | <p>Amazon ML returns the following elements. </p>
newtype AddTagsOutput = AddTagsOutput 
  { "ResourceId" :: NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined (TaggableResourceType)
  }
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _


-- | <p>The function used to train an <code>MLModel</code>. Training choices supported by Amazon ML include the following:</p> <ul> <li> <code>SGD</code> - Stochastic Gradient Descent.</li> <li> <code>RandomForest</code> - Random forest of decision trees.</li> </ul>
newtype Algorithm = Algorithm String
derive instance newtypeAlgorithm :: Newtype Algorithm _


-- | <p>An Amazon Web Service (AWS) user account identifier. The account identifier can be an AWS root account or an AWS Identity and Access Management (IAM) user.</p>
newtype AwsUserArn = AwsUserArn String
derive instance newtypeAwsUserArn :: Newtype AwsUserArn _


-- | <p> Represents the output of a <code>GetBatchPrediction</code> operation.</p> <p> The content consists of the detailed metadata, the status, and the data file information of a <code>Batch Prediction</code>.</p>
newtype BatchPrediction = BatchPrediction 
  { "BatchPredictionId" :: NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined (EntityId)
  , "BatchPredictionDataSourceId" :: NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "OutputUri" :: NullOrUndefined (S3Url)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  , "TotalRecordCount" :: NullOrUndefined (LongType)
  , "InvalidRecordCount" :: NullOrUndefined (LongType)
  }
derive instance newtypeBatchPrediction :: Newtype BatchPrediction _


-- | <p>A list of the variables to use in searching or filtering <code>BatchPrediction</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>BatchPrediction</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>BatchPrediction</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>BatchPrediction</code><b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>BatchPrediction</code> creation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>MLModel</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataURI</code> - Sets the search criteria to the data file(s) used in the <code>BatchPrediction</code>. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable String
derive instance newtypeBatchPredictionFilterVariable :: Newtype BatchPredictionFilterVariable _


newtype BatchPredictions = BatchPredictions (Array BatchPrediction)
derive instance newtypeBatchPredictions :: Newtype BatchPredictions _


-- | <p>The value specified in a filtering condition. The <code>ComparatorValue</code> becomes the reference value when matching or evaluating data values in filtering and searching functions.</p>
newtype ComparatorValue = ComparatorValue String
derive instance newtypeComparatorValue :: Newtype ComparatorValue _


newtype ComputeStatistics = ComputeStatistics Boolean
derive instance newtypeComputeStatistics :: Newtype ComputeStatistics _


newtype CreateBatchPredictionInput = CreateBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  , "BatchPredictionName" :: NullOrUndefined (EntityName)
  , "MLModelId" :: (EntityId)
  , "BatchPredictionDataSourceId" :: (EntityId)
  , "OutputUri" :: (S3Url)
  }
derive instance newtypeCreateBatchPredictionInput :: Newtype CreateBatchPredictionInput _


-- | <p> Represents the output of a <code>CreateBatchPrediction</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateBatchPrediction</code> operation is asynchronous. You can poll for status updates by using the <code>&gt;GetBatchPrediction</code> operation and checking the <code>Status</code> parameter of the result. </p>
newtype CreateBatchPredictionOutput = CreateBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateBatchPredictionOutput :: Newtype CreateBatchPredictionOutput _


newtype CreateDataSourceFromRDSInput = CreateDataSourceFromRDSInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined (EntityName)
  , "RDSData" :: (RDSDataSpec)
  , "RoleARN" :: (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromRDSInput :: Newtype CreateDataSourceFromRDSInput _


-- | <p> Represents the output of a <code>CreateDataSourceFromRDS</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRDS</code>&gt; operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. You can inspect the <code>Message</code> when <code>Status</code> shows up as <code>FAILED</code>. You can also check the progress of the copy operation by going to the <code>DataPipeline</code> console and looking up the pipeline using the <code>pipelineId </code> from the describe call.</p>
newtype CreateDataSourceFromRDSOutput = CreateDataSourceFromRDSOutput 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromRDSOutput :: Newtype CreateDataSourceFromRDSOutput _


newtype CreateDataSourceFromRedshiftInput = CreateDataSourceFromRedshiftInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined (EntityName)
  , "DataSpec" :: (RedshiftDataSpec)
  , "RoleARN" :: (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromRedshiftInput :: Newtype CreateDataSourceFromRedshiftInput _


-- | <p> Represents the output of a <code>CreateDataSourceFromRedshift</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRedshift</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateDataSourceFromRedshiftOutput = CreateDataSourceFromRedshiftOutput 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromRedshiftOutput :: Newtype CreateDataSourceFromRedshiftOutput _


newtype CreateDataSourceFromS3Input = CreateDataSourceFromS3Input 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined (EntityName)
  , "DataSpec" :: (S3DataSpec)
  , "ComputeStatistics" :: NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromS3Input :: Newtype CreateDataSourceFromS3Input _


-- | <p> Represents the output of a <code>CreateDataSourceFromS3</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromS3</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateDataSourceFromS3Output = CreateDataSourceFromS3Output 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromS3Output :: Newtype CreateDataSourceFromS3Output _


newtype CreateEvaluationInput = CreateEvaluationInput 
  { "EvaluationId" :: (EntityId)
  , "EvaluationName" :: NullOrUndefined (EntityName)
  , "MLModelId" :: (EntityId)
  , "EvaluationDataSourceId" :: (EntityId)
  }
derive instance newtypeCreateEvaluationInput :: Newtype CreateEvaluationInput _


-- | <p> Represents the output of a <code>CreateEvaluation</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p><code>CreateEvaluation</code> operation is asynchronous. You can poll for status updates by using the <code>GetEvcaluation</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateEvaluationOutput = CreateEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateEvaluationOutput :: Newtype CreateEvaluationOutput _


newtype CreateMLModelInput = CreateMLModelInput 
  { "MLModelId" :: (EntityId)
  , "MLModelName" :: NullOrUndefined (EntityName)
  , "MLModelType" :: (MLModelType)
  , "Parameters" :: NullOrUndefined (TrainingParameters)
  , "TrainingDataSourceId" :: (EntityId)
  , "Recipe" :: NullOrUndefined (Recipe)
  , "RecipeUri" :: NullOrUndefined (S3Url)
  }
derive instance newtypeCreateMLModelInput :: Newtype CreateMLModelInput _


-- | <p> Represents the output of a <code>CreateMLModel</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateMLModel</code> operation is asynchronous. You can poll for status updates by using the <code>GetMLModel</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateMLModelOutput = CreateMLModelOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeCreateMLModelOutput :: Newtype CreateMLModelOutput _


newtype CreateRealtimeEndpointInput = CreateRealtimeEndpointInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeCreateRealtimeEndpointInput :: Newtype CreateRealtimeEndpointInput _


-- | <p>Represents the output of an <code>CreateRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>.</p> <note> <p>The endpoint information includes the URI of the <code>MLModel</code>; that is, the location to send online prediction requests for the specified <code>MLModel</code>.</p> </note>
newtype CreateRealtimeEndpointOutput = CreateRealtimeEndpointOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  , "RealtimeEndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo)
  }
derive instance newtypeCreateRealtimeEndpointOutput :: Newtype CreateRealtimeEndpointOutput _


newtype DataRearrangement = DataRearrangement String
derive instance newtypeDataRearrangement :: Newtype DataRearrangement _


-- | <p>The schema of a <code>DataSource</code>. The <code>DataSchema</code> defines the structure of the observation data in the data file(s) referenced in the <code>DataSource</code>. The DataSource schema is expressed in JSON format.</p> <p><code>DataSchema</code> is not required if you specify a <code>DataSchemaUri</code></p> <p>{ "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "variables": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] } </p>
newtype DataSchema = DataSchema String
derive instance newtypeDataSchema :: Newtype DataSchema _


-- | <p> Represents the output of the <code>GetDataSource</code> operation. </p> <p> The content consists of the detailed metadata and data file information and the current status of the <code>DataSource</code>. </p>
newtype DataSource = DataSource 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  , "DataLocationS3" :: NullOrUndefined (S3Url)
  , "DataRearrangement" :: NullOrUndefined (DataRearrangement)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "DataSizeInBytes" :: NullOrUndefined (LongType)
  , "NumberOfFiles" :: NullOrUndefined (LongType)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "Message" :: NullOrUndefined (Message)
  , "RedshiftMetadata" :: NullOrUndefined (RedshiftMetadata)
  , "RDSMetadata" :: NullOrUndefined (RDSMetadata)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined (ComputeStatistics)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  }
derive instance newtypeDataSource :: Newtype DataSource _


-- | <p>A list of the variables to use in searching or filtering <code>DataSource</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>DataSource</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>DataSource</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>DataSource</code> <b> </b> <code>Name</code>.</li> <li> <code>DataUri</code> - Sets the search criteria to the URI of data files used to create the <code>DataSource</code>. The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>DataSource</code> creation.</li> </ul> <note><title>Note</title> <p>The variable names should match the variable names in the <code>DataSource</code>.</p> </note>
newtype DataSourceFilterVariable = DataSourceFilterVariable String
derive instance newtypeDataSourceFilterVariable :: Newtype DataSourceFilterVariable _


newtype DataSources = DataSources (Array DataSource)
derive instance newtypeDataSources :: Newtype DataSources _


newtype DeleteBatchPredictionInput = DeleteBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  }
derive instance newtypeDeleteBatchPredictionInput :: Newtype DeleteBatchPredictionInput _


-- | <p> Represents the output of a <code>DeleteBatchPrediction</code> operation.</p> <p>You can use the <code>GetBatchPrediction</code> operation and check the value of the <code>Status</code> parameter to see whether a <code>BatchPrediction</code> is marked as <code>DELETED</code>.</p>
newtype DeleteBatchPredictionOutput = DeleteBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteBatchPredictionOutput :: Newtype DeleteBatchPredictionOutput _


newtype DeleteDataSourceInput = DeleteDataSourceInput 
  { "DataSourceId" :: (EntityId)
  }
derive instance newtypeDeleteDataSourceInput :: Newtype DeleteDataSourceInput _


-- | <p> Represents the output of a <code>DeleteDataSource</code> operation.</p>
newtype DeleteDataSourceOutput = DeleteDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteDataSourceOutput :: Newtype DeleteDataSourceOutput _


newtype DeleteEvaluationInput = DeleteEvaluationInput 
  { "EvaluationId" :: (EntityId)
  }
derive instance newtypeDeleteEvaluationInput :: Newtype DeleteEvaluationInput _


-- | <p> Represents the output of a <code>DeleteEvaluation</code> operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.</p> <p>You can use the <code>GetEvaluation</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>Evaluation</code> is marked as <code>DELETED</code>.</p>
newtype DeleteEvaluationOutput = DeleteEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteEvaluationOutput :: Newtype DeleteEvaluationOutput _


newtype DeleteMLModelInput = DeleteMLModelInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeDeleteMLModelInput :: Newtype DeleteMLModelInput _


-- | <p>Represents the output of a <code>DeleteMLModel</code> operation.</p> <p>You can use the <code>GetMLModel</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>MLModel</code> is marked as <code>DELETED</code>.</p>
newtype DeleteMLModelOutput = DeleteMLModelOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteMLModelOutput :: Newtype DeleteMLModelOutput _


newtype DeleteRealtimeEndpointInput = DeleteRealtimeEndpointInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeDeleteRealtimeEndpointInput :: Newtype DeleteRealtimeEndpointInput _


-- | <p>Represents the output of an <code>DeleteRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>. </p>
newtype DeleteRealtimeEndpointOutput = DeleteRealtimeEndpointOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  , "RealtimeEndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo)
  }
derive instance newtypeDeleteRealtimeEndpointOutput :: Newtype DeleteRealtimeEndpointOutput _


newtype DeleteTagsInput = DeleteTagsInput 
  { "TagKeys" :: (TagKeyList)
  , "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeDeleteTagsInput :: Newtype DeleteTagsInput _


-- | <p>Amazon ML returns the following elements. </p>
newtype DeleteTagsOutput = DeleteTagsOutput 
  { "ResourceId" :: NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined (TaggableResourceType)
  }
derive instance newtypeDeleteTagsOutput :: Newtype DeleteTagsOutput _


newtype DescribeBatchPredictionsInput = DescribeBatchPredictionsInput 
  { "FilterVariable" :: NullOrUndefined (BatchPredictionFilterVariable)
  , "EQ" :: NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeBatchPredictionsInput :: Newtype DescribeBatchPredictionsInput _


-- | <p>Represents the output of a <code>DescribeBatchPredictions</code> operation. The content is essentially a list of <code>BatchPrediction</code>s.</p>
newtype DescribeBatchPredictionsOutput = DescribeBatchPredictionsOutput 
  { "Results" :: NullOrUndefined (BatchPredictions)
  , "NextToken" :: NullOrUndefined (StringType)
  }
derive instance newtypeDescribeBatchPredictionsOutput :: Newtype DescribeBatchPredictionsOutput _


newtype DescribeDataSourcesInput = DescribeDataSourcesInput 
  { "FilterVariable" :: NullOrUndefined (DataSourceFilterVariable)
  , "EQ" :: NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeDataSourcesInput :: Newtype DescribeDataSourcesInput _


-- | <p>Represents the query results from a <a>DescribeDataSources</a> operation. The content is essentially a list of <code>DataSource</code>.</p>
newtype DescribeDataSourcesOutput = DescribeDataSourcesOutput 
  { "Results" :: NullOrUndefined (DataSources)
  , "NextToken" :: NullOrUndefined (StringType)
  }
derive instance newtypeDescribeDataSourcesOutput :: Newtype DescribeDataSourcesOutput _


newtype DescribeEvaluationsInput = DescribeEvaluationsInput 
  { "FilterVariable" :: NullOrUndefined (EvaluationFilterVariable)
  , "EQ" :: NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeEvaluationsInput :: Newtype DescribeEvaluationsInput _


-- | <p>Represents the query results from a <code>DescribeEvaluations</code> operation. The content is essentially a list of <code>Evaluation</code>.</p>
newtype DescribeEvaluationsOutput = DescribeEvaluationsOutput 
  { "Results" :: NullOrUndefined (Evaluations)
  , "NextToken" :: NullOrUndefined (StringType)
  }
derive instance newtypeDescribeEvaluationsOutput :: Newtype DescribeEvaluationsOutput _


newtype DescribeMLModelsInput = DescribeMLModelsInput 
  { "FilterVariable" :: NullOrUndefined (MLModelFilterVariable)
  , "EQ" :: NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeMLModelsInput :: Newtype DescribeMLModelsInput _


-- | <p>Represents the output of a <code>DescribeMLModels</code> operation. The content is essentially a list of <code>MLModel</code>.</p>
newtype DescribeMLModelsOutput = DescribeMLModelsOutput 
  { "Results" :: NullOrUndefined (MLModels)
  , "NextToken" :: NullOrUndefined (StringType)
  }
derive instance newtypeDescribeMLModelsOutput :: Newtype DescribeMLModelsOutput _


newtype DescribeTagsInput = DescribeTagsInput 
  { "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeDescribeTagsInput :: Newtype DescribeTagsInput _


-- | <p>Amazon ML returns the following elements. </p>
newtype DescribeTagsOutput = DescribeTagsOutput 
  { "ResourceId" :: NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined (TaggableResourceType)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeDescribeTagsOutput :: Newtype DescribeTagsOutput _


-- | Contains the key values of <code>DetailsMap</code>: <code>PredictiveModelType</code> - Indicates the type of the <code>MLModel</code>. <code>Algorithm</code> - Indicates the algorithm that was used for the <code>MLModel</code>.
newtype DetailsAttributes = DetailsAttributes String
derive instance newtypeDetailsAttributes :: Newtype DetailsAttributes _


-- | Provides any additional details regarding the prediction.
newtype DetailsMap = DetailsMap (Map DetailsAttributes DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _


newtype EDPPipelineId = EDPPipelineId String
derive instance newtypeEDPPipelineId :: Newtype EDPPipelineId _


newtype EDPResourceRole = EDPResourceRole String
derive instance newtypeEDPResourceRole :: Newtype EDPResourceRole _


newtype EDPSecurityGroupId = EDPSecurityGroupId String
derive instance newtypeEDPSecurityGroupId :: Newtype EDPSecurityGroupId _


newtype EDPSecurityGroupIds = EDPSecurityGroupIds (Array EDPSecurityGroupId)
derive instance newtypeEDPSecurityGroupIds :: Newtype EDPSecurityGroupIds _


newtype EDPServiceRole = EDPServiceRole String
derive instance newtypeEDPServiceRole :: Newtype EDPServiceRole _


newtype EDPSubnetId = EDPSubnetId String
derive instance newtypeEDPSubnetId :: Newtype EDPSubnetId _


newtype EntityId = EntityId String
derive instance newtypeEntityId :: Newtype EntityId _


-- | <p>A user-supplied name or description of the Amazon ML resource.</p>
newtype EntityName = EntityName String
derive instance newtypeEntityName :: Newtype EntityName _


-- | <p>Object status with the following possible values:</p> <ul> <li><code>PENDING</code></li> <li><code>INPROGRESS</code></li> <li><code>FAILED</code></li> <li><code>COMPLETED</code></li> <li><code>DELETED</code></li> </ul>
newtype EntityStatus = EntityStatus String
derive instance newtypeEntityStatus :: Newtype EntityStatus _


-- | <p>A timestamp represented in epoch time.</p>
newtype EpochTime = EpochTime Number
derive instance newtypeEpochTime :: Newtype EpochTime _


newtype ErrorCode = ErrorCode Int
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p> Represents the output of <code>GetEvaluation</code> operation. </p> <p>The content consists of the detailed metadata and data file information and the current status of the <code>Evaluation</code>.</p>
newtype Evaluation = Evaluation 
  { "EvaluationId" :: NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined (EntityId)
  , "EvaluationDataSourceId" :: NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "PerformanceMetrics" :: NullOrUndefined (PerformanceMetrics)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  }
derive instance newtypeEvaluation :: Newtype Evaluation _


-- | <p>A list of the variables to use in searching or filtering <code>Evaluation</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>Evaluation</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>Evaluation</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>Evaluation</code> <b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked an evaluation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>Predictor</code> that was evaluated.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in evaluation.</li> <li> <code>DataUri</code> - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>
newtype EvaluationFilterVariable = EvaluationFilterVariable String
derive instance newtypeEvaluationFilterVariable :: Newtype EvaluationFilterVariable _


newtype Evaluations = Evaluations (Array Evaluation)
derive instance newtypeEvaluations :: Newtype Evaluations _


newtype GetBatchPredictionInput = GetBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  }
derive instance newtypeGetBatchPredictionInput :: Newtype GetBatchPredictionInput _


-- | <p>Represents the output of a <code>GetBatchPrediction</code> operation and describes a <code>BatchPrediction</code>.</p>
newtype GetBatchPredictionOutput = GetBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined (EntityId)
  , "BatchPredictionDataSourceId" :: NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "OutputUri" :: NullOrUndefined (S3Url)
  , "LogUri" :: NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  , "TotalRecordCount" :: NullOrUndefined (LongType)
  , "InvalidRecordCount" :: NullOrUndefined (LongType)
  }
derive instance newtypeGetBatchPredictionOutput :: Newtype GetBatchPredictionOutput _


newtype GetDataSourceInput = GetDataSourceInput 
  { "DataSourceId" :: (EntityId)
  , "Verbose" :: NullOrUndefined (Verbose)
  }
derive instance newtypeGetDataSourceInput :: Newtype GetDataSourceInput _


-- | <p>Represents the output of a <code>GetDataSource</code> operation and describes a <code>DataSource</code>.</p>
newtype GetDataSourceOutput = GetDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  , "DataLocationS3" :: NullOrUndefined (S3Url)
  , "DataRearrangement" :: NullOrUndefined (DataRearrangement)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "DataSizeInBytes" :: NullOrUndefined (LongType)
  , "NumberOfFiles" :: NullOrUndefined (LongType)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "LogUri" :: NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined (Message)
  , "RedshiftMetadata" :: NullOrUndefined (RedshiftMetadata)
  , "RDSMetadata" :: NullOrUndefined (RDSMetadata)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined (ComputeStatistics)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  , "DataSourceSchema" :: NullOrUndefined (DataSchema)
  }
derive instance newtypeGetDataSourceOutput :: Newtype GetDataSourceOutput _


newtype GetEvaluationInput = GetEvaluationInput 
  { "EvaluationId" :: (EntityId)
  }
derive instance newtypeGetEvaluationInput :: Newtype GetEvaluationInput _


-- | <p>Represents the output of a <code>GetEvaluation</code> operation and describes an <code>Evaluation</code>.</p>
newtype GetEvaluationOutput = GetEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined (EntityId)
  , "EvaluationDataSourceId" :: NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "PerformanceMetrics" :: NullOrUndefined (PerformanceMetrics)
  , "LogUri" :: NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  }
derive instance newtypeGetEvaluationOutput :: Newtype GetEvaluationOutput _


newtype GetMLModelInput = GetMLModelInput 
  { "MLModelId" :: (EntityId)
  , "Verbose" :: NullOrUndefined (Verbose)
  }
derive instance newtypeGetMLModelInput :: Newtype GetMLModelInput _


-- | <p>Represents the output of a <code>GetMLModel</code> operation, and provides detailed information about a <code>MLModel</code>.</p>
newtype GetMLModelOutput = GetMLModelOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  , "TrainingDataSourceId" :: NullOrUndefined (EntityId)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (MLModelName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "SizeInBytes" :: NullOrUndefined (LongType)
  , "EndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo)
  , "TrainingParameters" :: NullOrUndefined (TrainingParameters)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "MLModelType" :: NullOrUndefined (MLModelType)
  , "ScoreThreshold" :: NullOrUndefined (ScoreThreshold)
  , "ScoreThresholdLastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "LogUri" :: NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  , "Recipe" :: NullOrUndefined (Recipe)
  , "Schema" :: NullOrUndefined (DataSchema)
  }
derive instance newtypeGetMLModelOutput :: Newtype GetMLModelOutput _


-- | <p>A second request to use or change an object was not allowed. This can result from retrying a request using a parameter that was not present in the original request.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _


-- | <p>Integer type that is a 32-bit signed number.</p>
newtype IntegerType = IntegerType Int
derive instance newtypeIntegerType :: Newtype IntegerType _


-- | <p>An error on the server occurred when trying to process a request.</p>
newtype InternalServerException = InternalServerException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _


-- | <p>An error on the client occurred. Typically, the cause is an invalid input value.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


newtype InvalidTagException = InvalidTagException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidTagException :: Newtype InvalidTagException _


newtype Label = Label String
derive instance newtypeLabel :: Newtype Label _


-- | <p>The subscriber exceeded the maximum number of operations. This exception can occur when listing objects such as <code>DataSource</code>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>Long integer type that is a 64-bit signed number.</p>
newtype LongType = LongType Number
derive instance newtypeLongType :: Newtype LongType _


-- | <p> Represents the output of a <code>GetMLModel</code> operation. </p> <p>The content consists of the detailed metadata and the current status of the <code>MLModel</code>.</p>
newtype MLModel = MLModel 
  { "MLModelId" :: NullOrUndefined (EntityId)
  , "TrainingDataSourceId" :: NullOrUndefined (EntityId)
  , "CreatedByIamUser" :: NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined (MLModelName)
  , "Status" :: NullOrUndefined (EntityStatus)
  , "SizeInBytes" :: NullOrUndefined (LongType)
  , "EndpointInfo" :: NullOrUndefined (RealtimeEndpointInfo)
  , "TrainingParameters" :: NullOrUndefined (TrainingParameters)
  , "InputDataLocationS3" :: NullOrUndefined (S3Url)
  , "Algorithm" :: NullOrUndefined (Algorithm)
  , "MLModelType" :: NullOrUndefined (MLModelType)
  , "ScoreThreshold" :: NullOrUndefined (ScoreThreshold)
  , "ScoreThresholdLastUpdatedAt" :: NullOrUndefined (EpochTime)
  , "Message" :: NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined (EpochTime)
  }
derive instance newtypeMLModel :: Newtype MLModel _


newtype MLModelFilterVariable = MLModelFilterVariable String
derive instance newtypeMLModelFilterVariable :: Newtype MLModelFilterVariable _


newtype MLModelName = MLModelName String
derive instance newtypeMLModelName :: Newtype MLModelName _


newtype MLModelType = MLModelType String
derive instance newtypeMLModelType :: Newtype MLModelType _


newtype MLModels = MLModels (Array MLModel)
derive instance newtypeMLModels :: Newtype MLModels _


-- | <p> Description of the most recent details about an object.</p>
newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype PageLimit = PageLimit Int
derive instance newtypePageLimit :: Newtype PageLimit _


-- | <p>Measurements of how well the <code>MLModel</code> performed on known observations. One of the following metrics is returned, based on the type of the <code>MLModel</code>: </p> <ul> <li> <p>BinaryAUC: The binary <code>MLModel</code> uses the Area Under the Curve (AUC) technique to measure performance. </p> </li> <li> <p>RegressionRMSE: The regression <code>MLModel</code> uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.</p> </li> <li> <p>MulticlassAvgFScore: The multiclass <code>MLModel</code> uses the F1 score technique to measure performance. </p> </li> </ul> <p> For more information about performance metrics, please see the <a href="http://docs.aws.amazon.com/machine-learning/latest/dg">Amazon Machine Learning Developer Guide</a>. </p>
newtype PerformanceMetrics = PerformanceMetrics 
  { "Properties" :: NullOrUndefined (PerformanceMetricsProperties)
  }
derive instance newtypePerformanceMetrics :: Newtype PerformanceMetrics _


newtype PerformanceMetricsProperties = PerformanceMetricsProperties (Map PerformanceMetricsPropertyKey PerformanceMetricsPropertyValue)
derive instance newtypePerformanceMetricsProperties :: Newtype PerformanceMetricsProperties _


newtype PerformanceMetricsPropertyKey = PerformanceMetricsPropertyKey String
derive instance newtypePerformanceMetricsPropertyKey :: Newtype PerformanceMetricsPropertyKey _


newtype PerformanceMetricsPropertyValue = PerformanceMetricsPropertyValue String
derive instance newtypePerformanceMetricsPropertyValue :: Newtype PerformanceMetricsPropertyValue _


newtype PredictInput = PredictInput 
  { "MLModelId" :: (EntityId)
  , "Record''" :: (Record'')
  , "PredictEndpoint" :: (VipURL)
  }
derive instance newtypePredictInput :: Newtype PredictInput _


newtype PredictOutput = PredictOutput 
  { "Prediction" :: NullOrUndefined (Prediction)
  }
derive instance newtypePredictOutput :: Newtype PredictOutput _


-- | <p>The output from a <code>Predict</code> operation: </p> <ul> <li> <p> <code>Details</code> - Contains the following attributes: <code>DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS</code> <code>DetailsAttributes.ALGORITHM - SGD</code> </p> </li> <li> <p> <code>PredictedLabel</code> - Present for either a <code>BINARY</code> or <code>MULTICLASS</code> <code>MLModel</code> request. </p> </li> <li> <p> <code>PredictedScores</code> - Contains the raw classification score corresponding to each label. </p> </li> <li> <p> <code>PredictedValue</code> - Present for a <code>REGRESSION</code> <code>MLModel</code> request. </p> </li> </ul>
newtype Prediction = Prediction 
  { "PredictedLabel'" :: NullOrUndefined (Label)
  , "PredictedValue'" :: NullOrUndefined (FloatLabel')
  , "PredictedScores'" :: NullOrUndefined (ScoreValuePerLabelMap)
  , "Details'" :: NullOrUndefined (DetailsMap)
  }
derive instance newtypePrediction :: Newtype Prediction _


-- | <p>The exception is thrown when a predict request is made to an unmounted <code>MLModel</code>.</p>
newtype PredictorNotMountedException = PredictorNotMountedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePredictorNotMountedException :: Newtype PredictorNotMountedException _


newtype PresignedS3Url = PresignedS3Url String
derive instance newtypePresignedS3Url :: Newtype PresignedS3Url _


-- | <p>The data specification of an Amazon Relational Database Service (Amazon RDS) <code>DataSource</code>.</p>
newtype RDSDataSpec = RDSDataSpec 
  { "DatabaseInformation" :: (RDSDatabase)
  , "SelectSqlQuery" :: (RDSSelectSqlQuery)
  , "DatabaseCredentials" :: (RDSDatabaseCredentials)
  , "S3StagingLocation" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined (DataSchema)
  , "DataSchemaUri" :: NullOrUndefined (S3Url)
  , "ResourceRole" :: (EDPResourceRole)
  , "ServiceRole" :: (EDPServiceRole)
  , "SubnetId" :: (EDPSubnetId)
  , "SecurityGroupIds" :: (EDPSecurityGroupIds)
  }
derive instance newtypeRDSDataSpec :: Newtype RDSDataSpec _


-- | <p>The database details of an Amazon RDS database.</p>
newtype RDSDatabase = RDSDatabase 
  { "InstanceIdentifier" :: (RDSInstanceIdentifier)
  , "DatabaseName" :: (RDSDatabaseName)
  }
derive instance newtypeRDSDatabase :: Newtype RDSDatabase _


-- | <p>The database credentials to connect to a database on an RDS DB instance.</p>
newtype RDSDatabaseCredentials = RDSDatabaseCredentials 
  { "Username" :: (RDSDatabaseUsername)
  , "Password" :: (RDSDatabasePassword)
  }
derive instance newtypeRDSDatabaseCredentials :: Newtype RDSDatabaseCredentials _


-- | <p>The name of a database hosted on an RDS DB instance.</p>
newtype RDSDatabaseName = RDSDatabaseName String
derive instance newtypeRDSDatabaseName :: Newtype RDSDatabaseName _


-- | <p>The password to be used by Amazon ML to connect to a database on an RDS DB instance. The password should have sufficient permissions to execute the <code>RDSSelectQuery</code> query.</p>
newtype RDSDatabasePassword = RDSDatabasePassword String
derive instance newtypeRDSDatabasePassword :: Newtype RDSDatabasePassword _


-- | <p>The username to be used by Amazon ML to connect to database on an Amazon RDS instance. The username should have sufficient permissions to execute an <code>RDSSelectSqlQuery</code> query.</p>
newtype RDSDatabaseUsername = RDSDatabaseUsername String
derive instance newtypeRDSDatabaseUsername :: Newtype RDSDatabaseUsername _


-- | Identifier of RDS DB Instances.
newtype RDSInstanceIdentifier = RDSInstanceIdentifier String
derive instance newtypeRDSInstanceIdentifier :: Newtype RDSInstanceIdentifier _


-- | <p>The datasource details that are specific to Amazon RDS.</p>
newtype RDSMetadata = RDSMetadata 
  { "Database" :: NullOrUndefined (RDSDatabase)
  , "DatabaseUserName" :: NullOrUndefined (RDSDatabaseUsername)
  , "SelectSqlQuery" :: NullOrUndefined (RDSSelectSqlQuery)
  , "ResourceRole" :: NullOrUndefined (EDPResourceRole)
  , "ServiceRole" :: NullOrUndefined (EDPServiceRole)
  , "DataPipelineId" :: NullOrUndefined (EDPPipelineId)
  }
derive instance newtypeRDSMetadata :: Newtype RDSMetadata _


-- | <p>The SQL query to be executed against the Amazon RDS database. The SQL query should be valid for the Amazon RDS type being used. </p>
newtype RDSSelectSqlQuery = RDSSelectSqlQuery String
derive instance newtypeRDSSelectSqlQuery :: Newtype RDSSelectSqlQuery _


-- | <p> Describes the real-time endpoint information for an <code>MLModel</code>.</p>
newtype RealtimeEndpointInfo = RealtimeEndpointInfo 
  { "PeakRequestsPerSecond" :: NullOrUndefined (IntegerType)
  , "CreatedAt" :: NullOrUndefined (EpochTime)
  , "EndpointUrl" :: NullOrUndefined (VipURL)
  , "EndpointStatus" :: NullOrUndefined (RealtimeEndpointStatus)
  }
derive instance newtypeRealtimeEndpointInfo :: Newtype RealtimeEndpointInfo _


newtype RealtimeEndpointStatus = RealtimeEndpointStatus String
derive instance newtypeRealtimeEndpointStatus :: Newtype RealtimeEndpointStatus _


newtype Recipe = Recipe String
derive instance newtypeRecipe :: Newtype Recipe _


-- | <p>A map of variable name-value pairs that represent an observation.</p>
newtype Record'' = Record'' (Map VariableName VariableValue)
derive instance newtypeRecord'' :: Newtype Record'' _


-- | <p>The ID of an Amazon Redshift cluster.</p>
newtype RedshiftClusterIdentifier = RedshiftClusterIdentifier String
derive instance newtypeRedshiftClusterIdentifier :: Newtype RedshiftClusterIdentifier _


-- | <p>Describes the data specification of an Amazon Redshift <code>DataSource</code>.</p>
newtype RedshiftDataSpec = RedshiftDataSpec 
  { "DatabaseInformation" :: (RedshiftDatabase)
  , "SelectSqlQuery" :: (RedshiftSelectSqlQuery)
  , "DatabaseCredentials" :: (RedshiftDatabaseCredentials)
  , "S3StagingLocation" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined (DataSchema)
  , "DataSchemaUri" :: NullOrUndefined (S3Url)
  }
derive instance newtypeRedshiftDataSpec :: Newtype RedshiftDataSpec _


-- | <p>Describes the database details required to connect to an Amazon Redshift database.</p>
newtype RedshiftDatabase = RedshiftDatabase 
  { "DatabaseName" :: (RedshiftDatabaseName)
  , "ClusterIdentifier" :: (RedshiftClusterIdentifier)
  }
derive instance newtypeRedshiftDatabase :: Newtype RedshiftDatabase _


-- | <p> Describes the database credentials for connecting to a database on an Amazon Redshift cluster.</p>
newtype RedshiftDatabaseCredentials = RedshiftDatabaseCredentials 
  { "Username" :: (RedshiftDatabaseUsername)
  , "Password" :: (RedshiftDatabasePassword)
  }
derive instance newtypeRedshiftDatabaseCredentials :: Newtype RedshiftDatabaseCredentials _


-- | <p>The name of a database hosted on an Amazon Redshift cluster.</p>
newtype RedshiftDatabaseName = RedshiftDatabaseName String
derive instance newtypeRedshiftDatabaseName :: Newtype RedshiftDatabaseName _


-- | <p>A password to be used by Amazon ML to connect to a database on an Amazon Redshift cluster. The password should have sufficient permissions to execute a <code>RedshiftSelectSqlQuery</code> query. The password should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>
newtype RedshiftDatabasePassword = RedshiftDatabasePassword String
derive instance newtypeRedshiftDatabasePassword :: Newtype RedshiftDatabasePassword _


-- | <p>A username to be used by Amazon Machine Learning (Amazon ML)to connect to a database on an Amazon Redshift cluster. The username should have sufficient permissions to execute the <code>RedshiftSelectSqlQuery</code> query. The username should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>
newtype RedshiftDatabaseUsername = RedshiftDatabaseUsername String
derive instance newtypeRedshiftDatabaseUsername :: Newtype RedshiftDatabaseUsername _


-- | <p>Describes the <code>DataSource</code> details specific to Amazon Redshift.</p>
newtype RedshiftMetadata = RedshiftMetadata 
  { "RedshiftDatabase" :: NullOrUndefined (RedshiftDatabase)
  , "DatabaseUserName" :: NullOrUndefined (RedshiftDatabaseUsername)
  , "SelectSqlQuery" :: NullOrUndefined (RedshiftSelectSqlQuery)
  }
derive instance newtypeRedshiftMetadata :: Newtype RedshiftMetadata _


-- | <p> Describes the SQL query to execute on the Amazon Redshift database. The SQL query should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_SELECT_synopsis.html">SELECT</a>. </p>
newtype RedshiftSelectSqlQuery = RedshiftSelectSqlQuery String
derive instance newtypeRedshiftSelectSqlQuery :: Newtype RedshiftSelectSqlQuery _


-- | <p>A specified resource cannot be located.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined (ErrorCode)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The Amazon Resource Name (ARN) of an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html#roles-about-termsandconcepts">AWS IAM Role</a>, such as the following: arn:aws:iam::account:role/rolename. </p>
newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _


-- | <p> Describes the data specification of a <code>DataSource</code>.</p>
newtype S3DataSpec = S3DataSpec 
  { "DataLocationS3" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined (DataSchema)
  , "DataSchemaLocationS3" :: NullOrUndefined (S3Url)
  }
derive instance newtypeS3DataSpec :: Newtype S3DataSpec _


-- | <p>A reference to a file or bucket on Amazon Simple Storage Service (Amazon S3).</p>
newtype S3Url = S3Url String
derive instance newtypeS3Url :: Newtype S3Url _


newtype ScoreThreshold = ScoreThreshold Number
derive instance newtypeScoreThreshold :: Newtype ScoreThreshold _


newtype ScoreValue = ScoreValue Number
derive instance newtypeScoreValue :: Newtype ScoreValue _


-- | Provides the raw classification score corresponding to each label.
newtype ScoreValuePerLabelMap = ScoreValuePerLabelMap (Map Label ScoreValue)
derive instance newtypeScoreValuePerLabelMap :: Newtype ScoreValuePerLabelMap _


-- | <p>The sort order specified in a listing condition. Possible values include the following:</p> <ul> <li> <code>asc</code> - Present the information in ascending order (from A-Z).</li> <li> <code>dsc</code> - Present the information in descending order (from Z-A).</li> </ul>
newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _


-- | <p>String type.</p>
newtype StringType = StringType String
derive instance newtypeStringType :: Newtype StringType _


-- | <p>A custom key-value pair associated with an ML object, such as an ML model.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagLimitExceededException = TagLimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTagLimitExceededException :: Newtype TagLimitExceededException _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype TaggableResourceType = TaggableResourceType String
derive instance newtypeTaggableResourceType :: Newtype TaggableResourceType _


newtype TrainingParameters = TrainingParameters (Map StringType StringType)
derive instance newtypeTrainingParameters :: Newtype TrainingParameters _


newtype UpdateBatchPredictionInput = UpdateBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  , "BatchPredictionName" :: (EntityName)
  }
derive instance newtypeUpdateBatchPredictionInput :: Newtype UpdateBatchPredictionInput _


-- | <p>Represents the output of an <code>UpdateBatchPrediction</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>
newtype UpdateBatchPredictionOutput = UpdateBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateBatchPredictionOutput :: Newtype UpdateBatchPredictionOutput _


newtype UpdateDataSourceInput = UpdateDataSourceInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: (EntityName)
  }
derive instance newtypeUpdateDataSourceInput :: Newtype UpdateDataSourceInput _


-- | <p>Represents the output of an <code>UpdateDataSource</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>
newtype UpdateDataSourceOutput = UpdateDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateDataSourceOutput :: Newtype UpdateDataSourceOutput _


newtype UpdateEvaluationInput = UpdateEvaluationInput 
  { "EvaluationId" :: (EntityId)
  , "EvaluationName" :: (EntityName)
  }
derive instance newtypeUpdateEvaluationInput :: Newtype UpdateEvaluationInput _


-- | <p>Represents the output of an <code>UpdateEvaluation</code> operation.</p> <p>You can see the updated content by using the <code>GetEvaluation</code> operation.</p>
newtype UpdateEvaluationOutput = UpdateEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateEvaluationOutput :: Newtype UpdateEvaluationOutput _


newtype UpdateMLModelInput = UpdateMLModelInput 
  { "MLModelId" :: (EntityId)
  , "MLModelName" :: NullOrUndefined (EntityName)
  , "ScoreThreshold" :: NullOrUndefined (ScoreThreshold)
  }
derive instance newtypeUpdateMLModelInput :: Newtype UpdateMLModelInput _


-- | <p>Represents the output of an <code>UpdateMLModel</code> operation.</p> <p>You can see the updated content by using the <code>GetMLModel</code> operation.</p>
newtype UpdateMLModelOutput = UpdateMLModelOutput 
  { "MLModelId" :: NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateMLModelOutput :: Newtype UpdateMLModelOutput _


-- | <p>The name of a variable. Currently it's used to specify the name of the target value, label, weight, and tags.</p>
newtype VariableName = VariableName String
derive instance newtypeVariableName :: Newtype VariableName _


-- | <p>The value of a variable. Currently it's used to specify values of the target value, weights, and tag variables and for filtering variable values.</p>
newtype VariableValue = VariableValue String
derive instance newtypeVariableValue :: Newtype VariableValue _


-- | <p>Specifies whether a describe operation should return exhaustive or abbreviated information.</p>
newtype Verbose = Verbose Boolean
derive instance newtypeVerbose :: Newtype Verbose _


newtype VipURL = VipURL String
derive instance newtypeVipURL :: Newtype VipURL _


newtype FloatLabel' = FloatLabel' Number
derive instance newtypeFloatLabel' :: Newtype FloatLabel' _
