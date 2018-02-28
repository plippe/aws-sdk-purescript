

-- | Definition of the public APIs exposed by Amazon Machine Learning
module AWS.MachineLearning where

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

serviceName = "MachineLearning" :: String


-- | <p>Adds one or more tags to an object, up to a limit of 10. Each tag consists of a key and an optional value. If you add a tag using a key that is already associated with the ML object, <code>AddTags</code> updates the tag's value.</p>
addTags :: forall eff. AddTagsInput -> Aff (exception :: EXCEPTION | eff) AddTagsOutput
addTags = Request.request serviceName "addTags" 


-- | <p>Generates predictions for a group of observations. The observations to process exist in one or more data files referenced by a <code>DataSource</code>. This operation creates a new <code>BatchPrediction</code>, and uses an <code>MLModel</code> and the data files referenced by the <code>DataSource</code> as information sources. </p> <p><code>CreateBatchPrediction</code> is an asynchronous operation. In response to <code>CreateBatchPrediction</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>BatchPrediction</code> status to <code>PENDING</code>. After the <code>BatchPrediction</code> completes, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can poll for status updates by using the <a>GetBatchPrediction</a> operation and checking the <code>Status</code> parameter of the result. After the <code>COMPLETED</code> status appears, the results are available in the location specified by the <code>OutputUri</code> parameter.</p>
createBatchPrediction :: forall eff. CreateBatchPredictionInput -> Aff (exception :: EXCEPTION | eff) CreateBatchPredictionOutput
createBatchPrediction = Request.request serviceName "createBatchPrediction" 


-- | <p>Creates a <code>DataSource</code> object from an <a href="http://aws.amazon.com/rds/"> Amazon Relational Database Service</a> (Amazon RDS). A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRDS</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRDS</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used only to perform <code>&gt;CreateMLModel</code>&gt;, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML cannot accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p>
createDataSourceFromRDS :: forall eff. CreateDataSourceFromRDSInput -> Aff (exception :: EXCEPTION | eff) CreateDataSourceFromRDSOutput
createDataSourceFromRDS = Request.request serviceName "createDataSourceFromRDS" 


-- | <p>Creates a <code>DataSource</code> from a database hosted on an Amazon Redshift cluster. A <code>DataSource</code> references data that can be used to perform either <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromRedshift</code> is an asynchronous operation. In response to <code>CreateDataSourceFromRedshift</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> is created and ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in <code>COMPLETED</code> or <code>PENDING</code> states can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observations should be contained in the database hosted on an Amazon Redshift cluster and should be specified by a <code>SelectSqlQuery</code> query. Amazon ML executes an <code>Unload</code> command in Amazon Redshift to transfer the result set of the <code>SelectSqlQuery</code> query to <code>S3StagingLocation</code>.</p> <p>After the <code>DataSource</code> has been created, it's ready for use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also requires a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p> <?oxy_insert_start author="laurama" timestamp="20160406T153842-0700"><p>You can't change an existing datasource, but you can copy and modify the settings from an existing Amazon Redshift datasource to create a new datasource. To do so, call <code>GetDataSource</code> for an existing datasource and copy the values to a <code>CreateDataSource</code> call. Change the settings that you want to change and make sure that all required fields have the appropriate values.</p> <?oxy_insert_end>
createDataSourceFromRedshift :: forall eff. CreateDataSourceFromRedshiftInput -> Aff (exception :: EXCEPTION | eff) CreateDataSourceFromRedshiftOutput
createDataSourceFromRedshift = Request.request serviceName "createDataSourceFromRedshift" 


-- | <p>Creates a <code>DataSource</code> object. A <code>DataSource</code> references data that can be used to perform <code>CreateMLModel</code>, <code>CreateEvaluation</code>, or <code>CreateBatchPrediction</code> operations.</p> <p><code>CreateDataSourceFromS3</code> is an asynchronous operation. In response to <code>CreateDataSourceFromS3</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>DataSource</code> status to <code>PENDING</code>. After the <code>DataSource</code> has been created and is ready for use, Amazon ML sets the <code>Status</code> parameter to <code>COMPLETED</code>. <code>DataSource</code> in the <code>COMPLETED</code> or <code>PENDING</code> state can be used to perform only <code>CreateMLModel</code>, <code>CreateEvaluation</code> or <code>CreateBatchPrediction</code> operations. </p> <p> If Amazon ML can't accept the input source, it sets the <code>Status</code> parameter to <code>FAILED</code> and includes an error message in the <code>Message</code> attribute of the <code>GetDataSource</code> operation response. </p> <p>The observation data used in a <code>DataSource</code> should be ready to use; that is, it should have a consistent structure, and missing data values should be kept to a minimum. The observation data must reside in one or more .csv files in an Amazon Simple Storage Service (Amazon S3) location, along with a schema that describes the data items by name and type. The same schema must be used for all of the data files referenced by the <code>DataSource</code>. </p> <p>After the <code>DataSource</code> has been created, it's ready to use in evaluations and batch predictions. If you plan to use the <code>DataSource</code> to train an <code>MLModel</code>, the <code>DataSource</code> also needs a recipe. A recipe describes how each input variable will be used in training an <code>MLModel</code>. Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.</p>
createDataSourceFromS3 :: forall eff. CreateDataSourceFromS3Input -> Aff (exception :: EXCEPTION | eff) CreateDataSourceFromS3Output
createDataSourceFromS3 = Request.request serviceName "createDataSourceFromS3" 


-- | <p>Creates a new <code>Evaluation</code> of an <code>MLModel</code>. An <code>MLModel</code> is evaluated on a set of observations associated to a <code>DataSource</code>. Like a <code>DataSource</code> for an <code>MLModel</code>, the <code>DataSource</code> for an <code>Evaluation</code> contains values for the <code>Target Variable</code>. The <code>Evaluation</code> compares the predicted result for each observation to the actual outcome and provides a summary so that you know how effective the <code>MLModel</code> functions on the test data. Evaluation generates a relevant performance metric, such as BinaryAUC, RegressionRMSE or MulticlassAvgFScore based on the corresponding <code>MLModelType</code>: <code>BINARY</code>, <code>REGRESSION</code> or <code>MULTICLASS</code>. </p> <p><code>CreateEvaluation</code> is an asynchronous operation. In response to <code>CreateEvaluation</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the evaluation status to <code>PENDING</code>. After the <code>Evaluation</code> is created and ready for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetEvaluation</code> operation to check progress of the evaluation during the creation operation.</p>
createEvaluation :: forall eff. CreateEvaluationInput -> Aff (exception :: EXCEPTION | eff) CreateEvaluationOutput
createEvaluation = Request.request serviceName "createEvaluation" 


-- | <p>Creates a new <code>MLModel</code> using the <code>DataSource</code> and the recipe as information sources. </p> <p>An <code>MLModel</code> is nearly immutable. Users can update only the <code>MLModelName</code> and the <code>ScoreThreshold</code> in an <code>MLModel</code> without creating a new <code>MLModel</code>. </p> <p><code>CreateMLModel</code> is an asynchronous operation. In response to <code>CreateMLModel</code>, Amazon Machine Learning (Amazon ML) immediately returns and sets the <code>MLModel</code> status to <code>PENDING</code>. After the <code>MLModel</code> has been created and ready is for use, Amazon ML sets the status to <code>COMPLETED</code>. </p> <p>You can use the <code>GetMLModel</code> operation to check the progress of the <code>MLModel</code> during the creation operation.</p> <p> <code>CreateMLModel</code> requires a <code>DataSource</code> with computed statistics, which can be created by setting <code>ComputeStatistics</code> to <code>true</code> in <code>CreateDataSourceFromRDS</code>, <code>CreateDataSourceFromS3</code>, or <code>CreateDataSourceFromRedshift</code> operations. </p>
createMLModel :: forall eff. CreateMLModelInput -> Aff (exception :: EXCEPTION | eff) CreateMLModelOutput
createMLModel = Request.request serviceName "createMLModel" 


-- | <p>Creates a real-time endpoint for the <code>MLModel</code>. The endpoint contains the URI of the <code>MLModel</code>; that is, the location to send real-time prediction requests for the specified <code>MLModel</code>.</p>
createRealtimeEndpoint :: forall eff. CreateRealtimeEndpointInput -> Aff (exception :: EXCEPTION | eff) CreateRealtimeEndpointOutput
createRealtimeEndpoint = Request.request serviceName "createRealtimeEndpoint" 


-- | <p>Assigns the DELETED status to a <code>BatchPrediction</code>, rendering it unusable.</p> <p>After using the <code>DeleteBatchPrediction</code> operation, you can use the <a>GetBatchPrediction</a> operation to verify that the status of the <code>BatchPrediction</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteBatchPrediction</code> operation is irreversible.</p>
deleteBatchPrediction :: forall eff. DeleteBatchPredictionInput -> Aff (exception :: EXCEPTION | eff) DeleteBatchPredictionOutput
deleteBatchPrediction = Request.request serviceName "deleteBatchPrediction" 


-- | <p>Assigns the DELETED status to a <code>DataSource</code>, rendering it unusable.</p> <p>After using the <code>DeleteDataSource</code> operation, you can use the <a>GetDataSource</a> operation to verify that the status of the <code>DataSource</code> changed to DELETED.</p> <p><b>Caution:</b> The results of the <code>DeleteDataSource</code> operation are irreversible.</p>
deleteDataSource :: forall eff. DeleteDataSourceInput -> Aff (exception :: EXCEPTION | eff) DeleteDataSourceOutput
deleteDataSource = Request.request serviceName "deleteDataSource" 


-- | <p>Assigns the <code>DELETED</code> status to an <code>Evaluation</code>, rendering it unusable.</p> <p>After invoking the <code>DeleteEvaluation</code> operation, you can use the <code>GetEvaluation</code> operation to verify that the status of the <code>Evaluation</code> changed to <code>DELETED</code>.</p> <caution><title>Caution</title> <p>The results of the <code>DeleteEvaluation</code> operation are irreversible.</p></caution>
deleteEvaluation :: forall eff. DeleteEvaluationInput -> Aff (exception :: EXCEPTION | eff) DeleteEvaluationOutput
deleteEvaluation = Request.request serviceName "deleteEvaluation" 


-- | <p>Assigns the <code>DELETED</code> status to an <code>MLModel</code>, rendering it unusable.</p> <p>After using the <code>DeleteMLModel</code> operation, you can use the <code>GetMLModel</code> operation to verify that the status of the <code>MLModel</code> changed to DELETED.</p> <p><b>Caution:</b> The result of the <code>DeleteMLModel</code> operation is irreversible.</p>
deleteMLModel :: forall eff. DeleteMLModelInput -> Aff (exception :: EXCEPTION | eff) DeleteMLModelOutput
deleteMLModel = Request.request serviceName "deleteMLModel" 


-- | <p>Deletes a real time endpoint of an <code>MLModel</code>.</p>
deleteRealtimeEndpoint :: forall eff. DeleteRealtimeEndpointInput -> Aff (exception :: EXCEPTION | eff) DeleteRealtimeEndpointOutput
deleteRealtimeEndpoint = Request.request serviceName "deleteRealtimeEndpoint" 


-- | <p>Deletes the specified tags associated with an ML object. After this operation is complete, you can't recover deleted tags.</p> <p>If you specify a tag that doesn't exist, Amazon ML ignores it.</p>
deleteTags :: forall eff. DeleteTagsInput -> Aff (exception :: EXCEPTION | eff) DeleteTagsOutput
deleteTags = Request.request serviceName "deleteTags" 


-- | <p>Returns a list of <code>BatchPrediction</code> operations that match the search criteria in the request.</p>
describeBatchPredictions :: forall eff. DescribeBatchPredictionsInput -> Aff (exception :: EXCEPTION | eff) DescribeBatchPredictionsOutput
describeBatchPredictions = Request.request serviceName "describeBatchPredictions" 


-- | <p>Returns a list of <code>DataSource</code> that match the search criteria in the request.</p>
describeDataSources :: forall eff. DescribeDataSourcesInput -> Aff (exception :: EXCEPTION | eff) DescribeDataSourcesOutput
describeDataSources = Request.request serviceName "describeDataSources" 


-- | <p>Returns a list of <code>DescribeEvaluations</code> that match the search criteria in the request.</p>
describeEvaluations :: forall eff. DescribeEvaluationsInput -> Aff (exception :: EXCEPTION | eff) DescribeEvaluationsOutput
describeEvaluations = Request.request serviceName "describeEvaluations" 


-- | <p>Returns a list of <code>MLModel</code> that match the search criteria in the request.</p>
describeMLModels :: forall eff. DescribeMLModelsInput -> Aff (exception :: EXCEPTION | eff) DescribeMLModelsOutput
describeMLModels = Request.request serviceName "describeMLModels" 


-- | <p>Describes one or more of the tags for your Amazon ML object.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (exception :: EXCEPTION | eff) DescribeTagsOutput
describeTags = Request.request serviceName "describeTags" 


-- | <p>Returns a <code>BatchPrediction</code> that includes detailed metadata, status, and data file information for a <code>Batch Prediction</code> request.</p>
getBatchPrediction :: forall eff. GetBatchPredictionInput -> Aff (exception :: EXCEPTION | eff) GetBatchPredictionOutput
getBatchPrediction = Request.request serviceName "getBatchPrediction" 


-- | <p>Returns a <code>DataSource</code> that includes metadata and data file information, as well as the current status of the <code>DataSource</code>.</p> <p><code>GetDataSource</code> provides results in normal or verbose format. The verbose format adds the schema description and the list of files pointed to by the DataSource to the normal format.</p>
getDataSource :: forall eff. GetDataSourceInput -> Aff (exception :: EXCEPTION | eff) GetDataSourceOutput
getDataSource = Request.request serviceName "getDataSource" 


-- | <p>Returns an <code>Evaluation</code> that includes metadata as well as the current status of the <code>Evaluation</code>.</p>
getEvaluation :: forall eff. GetEvaluationInput -> Aff (exception :: EXCEPTION | eff) GetEvaluationOutput
getEvaluation = Request.request serviceName "getEvaluation" 


-- | <p>Returns an <code>MLModel</code> that includes detailed metadata, data source information, and the current status of the <code>MLModel</code>.</p> <p><code>GetMLModel</code> provides results in normal or verbose format. </p>
getMLModel :: forall eff. GetMLModelInput -> Aff (exception :: EXCEPTION | eff) GetMLModelOutput
getMLModel = Request.request serviceName "getMLModel" 


-- | <p>Generates a prediction for the observation using the specified <code>ML Model</code>.</p> <note><title>Note</title> <p>Not all response parameters will be populated. Whether a response parameter is populated depends on the type of model requested.</p></note>
predict :: forall eff. PredictInput -> Aff (exception :: EXCEPTION | eff) PredictOutput
predict = Request.request serviceName "predict" 


-- | <p>Updates the <code>BatchPredictionName</code> of a <code>BatchPrediction</code>.</p> <p>You can use the <code>GetBatchPrediction</code> operation to view the contents of the updated data element.</p>
updateBatchPrediction :: forall eff. UpdateBatchPredictionInput -> Aff (exception :: EXCEPTION | eff) UpdateBatchPredictionOutput
updateBatchPrediction = Request.request serviceName "updateBatchPrediction" 


-- | <p>Updates the <code>DataSourceName</code> of a <code>DataSource</code>.</p> <p>You can use the <code>GetDataSource</code> operation to view the contents of the updated data element.</p>
updateDataSource :: forall eff. UpdateDataSourceInput -> Aff (exception :: EXCEPTION | eff) UpdateDataSourceOutput
updateDataSource = Request.request serviceName "updateDataSource" 


-- | <p>Updates the <code>EvaluationName</code> of an <code>Evaluation</code>.</p> <p>You can use the <code>GetEvaluation</code> operation to view the contents of the updated data element.</p>
updateEvaluation :: forall eff. UpdateEvaluationInput -> Aff (exception :: EXCEPTION | eff) UpdateEvaluationOutput
updateEvaluation = Request.request serviceName "updateEvaluation" 


-- | <p>Updates the <code>MLModelName</code> and the <code>ScoreThreshold</code> of an <code>MLModel</code>.</p> <p>You can use the <code>GetMLModel</code> operation to view the contents of the updated data element.</p>
updateMLModel :: forall eff. UpdateMLModelInput -> Aff (exception :: EXCEPTION | eff) UpdateMLModelOutput
updateMLModel = Request.request serviceName "updateMLModel" 


newtype AddTagsInput = AddTagsInput 
  { "Tags" :: (TagList)
  , "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _
derive instance repGenericAddTagsInput :: Generic AddTagsInput _
instance showAddTagsInput :: Show AddTagsInput where
  show = genericShow
instance decodeAddTagsInput :: Decode AddTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsInput :: Encode AddTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon ML returns the following elements. </p>
newtype AddTagsOutput = AddTagsOutput 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (TaggableResourceType)
  }
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _
derive instance repGenericAddTagsOutput :: Generic AddTagsOutput _
instance showAddTagsOutput :: Show AddTagsOutput where
  show = genericShow
instance decodeAddTagsOutput :: Decode AddTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsOutput :: Encode AddTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The function used to train an <code>MLModel</code>. Training choices supported by Amazon ML include the following:</p> <ul> <li> <code>SGD</code> - Stochastic Gradient Descent.</li> <li> <code>RandomForest</code> - Random forest of decision trees.</li> </ul>
newtype Algorithm = Algorithm String
derive instance newtypeAlgorithm :: Newtype Algorithm _
derive instance repGenericAlgorithm :: Generic Algorithm _
instance showAlgorithm :: Show Algorithm where
  show = genericShow
instance decodeAlgorithm :: Decode Algorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlgorithm :: Encode Algorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon Web Service (AWS) user account identifier. The account identifier can be an AWS root account or an AWS Identity and Access Management (IAM) user.</p>
newtype AwsUserArn = AwsUserArn String
derive instance newtypeAwsUserArn :: Newtype AwsUserArn _
derive instance repGenericAwsUserArn :: Generic AwsUserArn _
instance showAwsUserArn :: Show AwsUserArn where
  show = genericShow
instance decodeAwsUserArn :: Decode AwsUserArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsUserArn :: Encode AwsUserArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>GetBatchPrediction</code> operation.</p> <p> The content consists of the detailed metadata, the status, and the data file information of a <code>Batch Prediction</code>.</p>
newtype BatchPrediction = BatchPrediction 
  { "BatchPredictionId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "BatchPredictionDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "OutputUri" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "TotalRecordCount" :: NullOrUndefined.NullOrUndefined (LongType)
  , "InvalidRecordCount" :: NullOrUndefined.NullOrUndefined (LongType)
  }
derive instance newtypeBatchPrediction :: Newtype BatchPrediction _
derive instance repGenericBatchPrediction :: Generic BatchPrediction _
instance showBatchPrediction :: Show BatchPrediction where
  show = genericShow
instance decodeBatchPrediction :: Decode BatchPrediction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchPrediction :: Encode BatchPrediction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the variables to use in searching or filtering <code>BatchPrediction</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>BatchPrediction</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>BatchPrediction</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>BatchPrediction</code><b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>BatchPrediction</code> creation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>MLModel</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in the <code>BatchPrediction</code>.</li> <li> <code>DataURI</code> - Sets the search criteria to the data file(s) used in the <code>BatchPrediction</code>. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>
newtype BatchPredictionFilterVariable = BatchPredictionFilterVariable String
derive instance newtypeBatchPredictionFilterVariable :: Newtype BatchPredictionFilterVariable _
derive instance repGenericBatchPredictionFilterVariable :: Generic BatchPredictionFilterVariable _
instance showBatchPredictionFilterVariable :: Show BatchPredictionFilterVariable where
  show = genericShow
instance decodeBatchPredictionFilterVariable :: Decode BatchPredictionFilterVariable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchPredictionFilterVariable :: Encode BatchPredictionFilterVariable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchPredictions = BatchPredictions (Array BatchPrediction)
derive instance newtypeBatchPredictions :: Newtype BatchPredictions _
derive instance repGenericBatchPredictions :: Generic BatchPredictions _
instance showBatchPredictions :: Show BatchPredictions where
  show = genericShow
instance decodeBatchPredictions :: Decode BatchPredictions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchPredictions :: Encode BatchPredictions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value specified in a filtering condition. The <code>ComparatorValue</code> becomes the reference value when matching or evaluating data values in filtering and searching functions.</p>
newtype ComparatorValue = ComparatorValue String
derive instance newtypeComparatorValue :: Newtype ComparatorValue _
derive instance repGenericComparatorValue :: Generic ComparatorValue _
instance showComparatorValue :: Show ComparatorValue where
  show = genericShow
instance decodeComparatorValue :: Decode ComparatorValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparatorValue :: Encode ComparatorValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputeStatistics = ComputeStatistics Boolean
derive instance newtypeComputeStatistics :: Newtype ComputeStatistics _
derive instance repGenericComputeStatistics :: Generic ComputeStatistics _
instance showComputeStatistics :: Show ComputeStatistics where
  show = genericShow
instance decodeComputeStatistics :: Decode ComputeStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeStatistics :: Encode ComputeStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateBatchPredictionInput = CreateBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  , "BatchPredictionName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "MLModelId" :: (EntityId)
  , "BatchPredictionDataSourceId" :: (EntityId)
  , "OutputUri" :: (S3Url)
  }
derive instance newtypeCreateBatchPredictionInput :: Newtype CreateBatchPredictionInput _
derive instance repGenericCreateBatchPredictionInput :: Generic CreateBatchPredictionInput _
instance showCreateBatchPredictionInput :: Show CreateBatchPredictionInput where
  show = genericShow
instance decodeCreateBatchPredictionInput :: Decode CreateBatchPredictionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBatchPredictionInput :: Encode CreateBatchPredictionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateBatchPrediction</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateBatchPrediction</code> operation is asynchronous. You can poll for status updates by using the <code>&gt;GetBatchPrediction</code> operation and checking the <code>Status</code> parameter of the result. </p>
newtype CreateBatchPredictionOutput = CreateBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateBatchPredictionOutput :: Newtype CreateBatchPredictionOutput _
derive instance repGenericCreateBatchPredictionOutput :: Generic CreateBatchPredictionOutput _
instance showCreateBatchPredictionOutput :: Show CreateBatchPredictionOutput where
  show = genericShow
instance decodeCreateBatchPredictionOutput :: Decode CreateBatchPredictionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateBatchPredictionOutput :: Encode CreateBatchPredictionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDataSourceFromRDSInput = CreateDataSourceFromRDSInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "RDSData" :: (RDSDataSpec)
  , "RoleARN" :: (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined.NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromRDSInput :: Newtype CreateDataSourceFromRDSInput _
derive instance repGenericCreateDataSourceFromRDSInput :: Generic CreateDataSourceFromRDSInput _
instance showCreateDataSourceFromRDSInput :: Show CreateDataSourceFromRDSInput where
  show = genericShow
instance decodeCreateDataSourceFromRDSInput :: Decode CreateDataSourceFromRDSInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromRDSInput :: Encode CreateDataSourceFromRDSInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateDataSourceFromRDS</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRDS</code>&gt; operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. You can inspect the <code>Message</code> when <code>Status</code> shows up as <code>FAILED</code>. You can also check the progress of the copy operation by going to the <code>DataPipeline</code> console and looking up the pipeline using the <code>pipelineId </code> from the describe call.</p>
newtype CreateDataSourceFromRDSOutput = CreateDataSourceFromRDSOutput 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromRDSOutput :: Newtype CreateDataSourceFromRDSOutput _
derive instance repGenericCreateDataSourceFromRDSOutput :: Generic CreateDataSourceFromRDSOutput _
instance showCreateDataSourceFromRDSOutput :: Show CreateDataSourceFromRDSOutput where
  show = genericShow
instance decodeCreateDataSourceFromRDSOutput :: Decode CreateDataSourceFromRDSOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromRDSOutput :: Encode CreateDataSourceFromRDSOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDataSourceFromRedshiftInput = CreateDataSourceFromRedshiftInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "DataSpec" :: (RedshiftDataSpec)
  , "RoleARN" :: (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined.NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromRedshiftInput :: Newtype CreateDataSourceFromRedshiftInput _
derive instance repGenericCreateDataSourceFromRedshiftInput :: Generic CreateDataSourceFromRedshiftInput _
instance showCreateDataSourceFromRedshiftInput :: Show CreateDataSourceFromRedshiftInput where
  show = genericShow
instance decodeCreateDataSourceFromRedshiftInput :: Decode CreateDataSourceFromRedshiftInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromRedshiftInput :: Encode CreateDataSourceFromRedshiftInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateDataSourceFromRedshift</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromRedshift</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateDataSourceFromRedshiftOutput = CreateDataSourceFromRedshiftOutput 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromRedshiftOutput :: Newtype CreateDataSourceFromRedshiftOutput _
derive instance repGenericCreateDataSourceFromRedshiftOutput :: Generic CreateDataSourceFromRedshiftOutput _
instance showCreateDataSourceFromRedshiftOutput :: Show CreateDataSourceFromRedshiftOutput where
  show = genericShow
instance decodeCreateDataSourceFromRedshiftOutput :: Decode CreateDataSourceFromRedshiftOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromRedshiftOutput :: Encode CreateDataSourceFromRedshiftOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDataSourceFromS3Input = CreateDataSourceFromS3Input 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "DataSpec" :: (S3DataSpec)
  , "ComputeStatistics" :: NullOrUndefined.NullOrUndefined (ComputeStatistics)
  }
derive instance newtypeCreateDataSourceFromS3Input :: Newtype CreateDataSourceFromS3Input _
derive instance repGenericCreateDataSourceFromS3Input :: Generic CreateDataSourceFromS3Input _
instance showCreateDataSourceFromS3Input :: Show CreateDataSourceFromS3Input where
  show = genericShow
instance decodeCreateDataSourceFromS3Input :: Decode CreateDataSourceFromS3Input where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromS3Input :: Encode CreateDataSourceFromS3Input where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateDataSourceFromS3</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateDataSourceFromS3</code> operation is asynchronous. You can poll for updates by using the <code>GetBatchPrediction</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateDataSourceFromS3Output = CreateDataSourceFromS3Output 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateDataSourceFromS3Output :: Newtype CreateDataSourceFromS3Output _
derive instance repGenericCreateDataSourceFromS3Output :: Generic CreateDataSourceFromS3Output _
instance showCreateDataSourceFromS3Output :: Show CreateDataSourceFromS3Output where
  show = genericShow
instance decodeCreateDataSourceFromS3Output :: Decode CreateDataSourceFromS3Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDataSourceFromS3Output :: Encode CreateDataSourceFromS3Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateEvaluationInput = CreateEvaluationInput 
  { "EvaluationId" :: (EntityId)
  , "EvaluationName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "MLModelId" :: (EntityId)
  , "EvaluationDataSourceId" :: (EntityId)
  }
derive instance newtypeCreateEvaluationInput :: Newtype CreateEvaluationInput _
derive instance repGenericCreateEvaluationInput :: Generic CreateEvaluationInput _
instance showCreateEvaluationInput :: Show CreateEvaluationInput where
  show = genericShow
instance decodeCreateEvaluationInput :: Decode CreateEvaluationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEvaluationInput :: Encode CreateEvaluationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateEvaluation</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p><code>CreateEvaluation</code> operation is asynchronous. You can poll for status updates by using the <code>GetEvcaluation</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateEvaluationOutput = CreateEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateEvaluationOutput :: Newtype CreateEvaluationOutput _
derive instance repGenericCreateEvaluationOutput :: Generic CreateEvaluationOutput _
instance showCreateEvaluationOutput :: Show CreateEvaluationOutput where
  show = genericShow
instance decodeCreateEvaluationOutput :: Decode CreateEvaluationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEvaluationOutput :: Encode CreateEvaluationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMLModelInput = CreateMLModelInput 
  { "MLModelId" :: (EntityId)
  , "MLModelName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "MLModelType" :: (MLModelType)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (TrainingParameters)
  , "TrainingDataSourceId" :: (EntityId)
  , "Recipe" :: NullOrUndefined.NullOrUndefined (Recipe)
  , "RecipeUri" :: NullOrUndefined.NullOrUndefined (S3Url)
  }
derive instance newtypeCreateMLModelInput :: Newtype CreateMLModelInput _
derive instance repGenericCreateMLModelInput :: Generic CreateMLModelInput _
instance showCreateMLModelInput :: Show CreateMLModelInput where
  show = genericShow
instance decodeCreateMLModelInput :: Decode CreateMLModelInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMLModelInput :: Encode CreateMLModelInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>CreateMLModel</code> operation, and is an acknowledgement that Amazon ML received the request.</p> <p>The <code>CreateMLModel</code> operation is asynchronous. You can poll for status updates by using the <code>GetMLModel</code> operation and checking the <code>Status</code> parameter. </p>
newtype CreateMLModelOutput = CreateMLModelOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeCreateMLModelOutput :: Newtype CreateMLModelOutput _
derive instance repGenericCreateMLModelOutput :: Generic CreateMLModelOutput _
instance showCreateMLModelOutput :: Show CreateMLModelOutput where
  show = genericShow
instance decodeCreateMLModelOutput :: Decode CreateMLModelOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMLModelOutput :: Encode CreateMLModelOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRealtimeEndpointInput = CreateRealtimeEndpointInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeCreateRealtimeEndpointInput :: Newtype CreateRealtimeEndpointInput _
derive instance repGenericCreateRealtimeEndpointInput :: Generic CreateRealtimeEndpointInput _
instance showCreateRealtimeEndpointInput :: Show CreateRealtimeEndpointInput where
  show = genericShow
instance decodeCreateRealtimeEndpointInput :: Decode CreateRealtimeEndpointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRealtimeEndpointInput :: Encode CreateRealtimeEndpointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>CreateRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>.</p> <note> <p>The endpoint information includes the URI of the <code>MLModel</code>; that is, the location to send online prediction requests for the specified <code>MLModel</code>.</p> </note>
newtype CreateRealtimeEndpointOutput = CreateRealtimeEndpointOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "RealtimeEndpointInfo" :: NullOrUndefined.NullOrUndefined (RealtimeEndpointInfo)
  }
derive instance newtypeCreateRealtimeEndpointOutput :: Newtype CreateRealtimeEndpointOutput _
derive instance repGenericCreateRealtimeEndpointOutput :: Generic CreateRealtimeEndpointOutput _
instance showCreateRealtimeEndpointOutput :: Show CreateRealtimeEndpointOutput where
  show = genericShow
instance decodeCreateRealtimeEndpointOutput :: Decode CreateRealtimeEndpointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRealtimeEndpointOutput :: Encode CreateRealtimeEndpointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataRearrangement = DataRearrangement String
derive instance newtypeDataRearrangement :: Newtype DataRearrangement _
derive instance repGenericDataRearrangement :: Generic DataRearrangement _
instance showDataRearrangement :: Show DataRearrangement where
  show = genericShow
instance decodeDataRearrangement :: Decode DataRearrangement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataRearrangement :: Encode DataRearrangement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The schema of a <code>DataSource</code>. The <code>DataSchema</code> defines the structure of the observation data in the data file(s) referenced in the <code>DataSource</code>. The DataSource schema is expressed in JSON format.</p> <p><code>DataSchema</code> is not required if you specify a <code>DataSchemaUri</code></p> <p>{ "version": "1.0", "recordAnnotationFieldName": "F1", "recordWeightFieldName": "F2", "targetFieldName": "F3", "dataFormat": "CSV", "dataFileContainsHeader": true, "variables": [ { "fieldName": "F1", "fieldType": "TEXT" }, { "fieldName": "F2", "fieldType": "NUMERIC" }, { "fieldName": "F3", "fieldType": "CATEGORICAL" }, { "fieldName": "F4", "fieldType": "NUMERIC" }, { "fieldName": "F5", "fieldType": "CATEGORICAL" }, { "fieldName": "F6", "fieldType": "TEXT" }, { "fieldName": "F7", "fieldType": "WEIGHTED_INT_SEQUENCE" }, { "fieldName": "F8", "fieldType": "WEIGHTED_STRING_SEQUENCE" } ], "excludedVariableNames": [ "F6" ] } </p>
newtype DataSchema = DataSchema String
derive instance newtypeDataSchema :: Newtype DataSchema _
derive instance repGenericDataSchema :: Generic DataSchema _
instance showDataSchema :: Show DataSchema where
  show = genericShow
instance decodeDataSchema :: Decode DataSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSchema :: Encode DataSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of the <code>GetDataSource</code> operation. </p> <p> The content consists of the detailed metadata and data file information and the current status of the <code>DataSource</code>. </p>
newtype DataSource = DataSource 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "DataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "DataRearrangement" :: NullOrUndefined.NullOrUndefined (DataRearrangement)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "DataSizeInBytes" :: NullOrUndefined.NullOrUndefined (LongType)
  , "NumberOfFiles" :: NullOrUndefined.NullOrUndefined (LongType)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "RedshiftMetadata" :: NullOrUndefined.NullOrUndefined (RedshiftMetadata)
  , "RDSMetadata" :: NullOrUndefined.NullOrUndefined (RDSMetadata)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined.NullOrUndefined (ComputeStatistics)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  }
derive instance newtypeDataSource :: Newtype DataSource _
derive instance repGenericDataSource :: Generic DataSource _
instance showDataSource :: Show DataSource where
  show = genericShow
instance decodeDataSource :: Decode DataSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSource :: Encode DataSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the variables to use in searching or filtering <code>DataSource</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>DataSource</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>DataSource</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>DataSource</code> <b> </b> <code>Name</code>.</li> <li> <code>DataUri</code> - Sets the search criteria to the URI of data files used to create the <code>DataSource</code>. The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked the <code>DataSource</code> creation.</li> </ul> <note><title>Note</title> <p>The variable names should match the variable names in the <code>DataSource</code>.</p> </note>
newtype DataSourceFilterVariable = DataSourceFilterVariable String
derive instance newtypeDataSourceFilterVariable :: Newtype DataSourceFilterVariable _
derive instance repGenericDataSourceFilterVariable :: Generic DataSourceFilterVariable _
instance showDataSourceFilterVariable :: Show DataSourceFilterVariable where
  show = genericShow
instance decodeDataSourceFilterVariable :: Decode DataSourceFilterVariable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSourceFilterVariable :: Encode DataSourceFilterVariable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataSources = DataSources (Array DataSource)
derive instance newtypeDataSources :: Newtype DataSources _
derive instance repGenericDataSources :: Generic DataSources _
instance showDataSources :: Show DataSources where
  show = genericShow
instance decodeDataSources :: Decode DataSources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataSources :: Encode DataSources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBatchPredictionInput = DeleteBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  }
derive instance newtypeDeleteBatchPredictionInput :: Newtype DeleteBatchPredictionInput _
derive instance repGenericDeleteBatchPredictionInput :: Generic DeleteBatchPredictionInput _
instance showDeleteBatchPredictionInput :: Show DeleteBatchPredictionInput where
  show = genericShow
instance decodeDeleteBatchPredictionInput :: Decode DeleteBatchPredictionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBatchPredictionInput :: Encode DeleteBatchPredictionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>DeleteBatchPrediction</code> operation.</p> <p>You can use the <code>GetBatchPrediction</code> operation and check the value of the <code>Status</code> parameter to see whether a <code>BatchPrediction</code> is marked as <code>DELETED</code>.</p>
newtype DeleteBatchPredictionOutput = DeleteBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteBatchPredictionOutput :: Newtype DeleteBatchPredictionOutput _
derive instance repGenericDeleteBatchPredictionOutput :: Generic DeleteBatchPredictionOutput _
instance showDeleteBatchPredictionOutput :: Show DeleteBatchPredictionOutput where
  show = genericShow
instance decodeDeleteBatchPredictionOutput :: Decode DeleteBatchPredictionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBatchPredictionOutput :: Encode DeleteBatchPredictionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDataSourceInput = DeleteDataSourceInput 
  { "DataSourceId" :: (EntityId)
  }
derive instance newtypeDeleteDataSourceInput :: Newtype DeleteDataSourceInput _
derive instance repGenericDeleteDataSourceInput :: Generic DeleteDataSourceInput _
instance showDeleteDataSourceInput :: Show DeleteDataSourceInput where
  show = genericShow
instance decodeDeleteDataSourceInput :: Decode DeleteDataSourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDataSourceInput :: Encode DeleteDataSourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>DeleteDataSource</code> operation.</p>
newtype DeleteDataSourceOutput = DeleteDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteDataSourceOutput :: Newtype DeleteDataSourceOutput _
derive instance repGenericDeleteDataSourceOutput :: Generic DeleteDataSourceOutput _
instance showDeleteDataSourceOutput :: Show DeleteDataSourceOutput where
  show = genericShow
instance decodeDeleteDataSourceOutput :: Decode DeleteDataSourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDataSourceOutput :: Encode DeleteDataSourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteEvaluationInput = DeleteEvaluationInput 
  { "EvaluationId" :: (EntityId)
  }
derive instance newtypeDeleteEvaluationInput :: Newtype DeleteEvaluationInput _
derive instance repGenericDeleteEvaluationInput :: Generic DeleteEvaluationInput _
instance showDeleteEvaluationInput :: Show DeleteEvaluationInput where
  show = genericShow
instance decodeDeleteEvaluationInput :: Decode DeleteEvaluationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEvaluationInput :: Encode DeleteEvaluationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>DeleteEvaluation</code> operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.</p> <p>You can use the <code>GetEvaluation</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>Evaluation</code> is marked as <code>DELETED</code>.</p>
newtype DeleteEvaluationOutput = DeleteEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteEvaluationOutput :: Newtype DeleteEvaluationOutput _
derive instance repGenericDeleteEvaluationOutput :: Generic DeleteEvaluationOutput _
instance showDeleteEvaluationOutput :: Show DeleteEvaluationOutput where
  show = genericShow
instance decodeDeleteEvaluationOutput :: Decode DeleteEvaluationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEvaluationOutput :: Encode DeleteEvaluationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMLModelInput = DeleteMLModelInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeDeleteMLModelInput :: Newtype DeleteMLModelInput _
derive instance repGenericDeleteMLModelInput :: Generic DeleteMLModelInput _
instance showDeleteMLModelInput :: Show DeleteMLModelInput where
  show = genericShow
instance decodeDeleteMLModelInput :: Decode DeleteMLModelInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMLModelInput :: Encode DeleteMLModelInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DeleteMLModel</code> operation.</p> <p>You can use the <code>GetMLModel</code> operation and check the value of the <code>Status</code> parameter to see whether an <code>MLModel</code> is marked as <code>DELETED</code>.</p>
newtype DeleteMLModelOutput = DeleteMLModelOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeDeleteMLModelOutput :: Newtype DeleteMLModelOutput _
derive instance repGenericDeleteMLModelOutput :: Generic DeleteMLModelOutput _
instance showDeleteMLModelOutput :: Show DeleteMLModelOutput where
  show = genericShow
instance decodeDeleteMLModelOutput :: Decode DeleteMLModelOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMLModelOutput :: Encode DeleteMLModelOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRealtimeEndpointInput = DeleteRealtimeEndpointInput 
  { "MLModelId" :: (EntityId)
  }
derive instance newtypeDeleteRealtimeEndpointInput :: Newtype DeleteRealtimeEndpointInput _
derive instance repGenericDeleteRealtimeEndpointInput :: Generic DeleteRealtimeEndpointInput _
instance showDeleteRealtimeEndpointInput :: Show DeleteRealtimeEndpointInput where
  show = genericShow
instance decodeDeleteRealtimeEndpointInput :: Decode DeleteRealtimeEndpointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRealtimeEndpointInput :: Encode DeleteRealtimeEndpointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>DeleteRealtimeEndpoint</code> operation.</p> <p>The result contains the <code>MLModelId</code> and the endpoint information for the <code>MLModel</code>. </p>
newtype DeleteRealtimeEndpointOutput = DeleteRealtimeEndpointOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "RealtimeEndpointInfo" :: NullOrUndefined.NullOrUndefined (RealtimeEndpointInfo)
  }
derive instance newtypeDeleteRealtimeEndpointOutput :: Newtype DeleteRealtimeEndpointOutput _
derive instance repGenericDeleteRealtimeEndpointOutput :: Generic DeleteRealtimeEndpointOutput _
instance showDeleteRealtimeEndpointOutput :: Show DeleteRealtimeEndpointOutput where
  show = genericShow
instance decodeDeleteRealtimeEndpointOutput :: Decode DeleteRealtimeEndpointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRealtimeEndpointOutput :: Encode DeleteRealtimeEndpointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTagsInput = DeleteTagsInput 
  { "TagKeys" :: (TagKeyList)
  , "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeDeleteTagsInput :: Newtype DeleteTagsInput _
derive instance repGenericDeleteTagsInput :: Generic DeleteTagsInput _
instance showDeleteTagsInput :: Show DeleteTagsInput where
  show = genericShow
instance decodeDeleteTagsInput :: Decode DeleteTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsInput :: Encode DeleteTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon ML returns the following elements. </p>
newtype DeleteTagsOutput = DeleteTagsOutput 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (TaggableResourceType)
  }
derive instance newtypeDeleteTagsOutput :: Newtype DeleteTagsOutput _
derive instance repGenericDeleteTagsOutput :: Generic DeleteTagsOutput _
instance showDeleteTagsOutput :: Show DeleteTagsOutput where
  show = genericShow
instance decodeDeleteTagsOutput :: Decode DeleteTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsOutput :: Encode DeleteTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeBatchPredictionsInput = DescribeBatchPredictionsInput 
  { "FilterVariable" :: NullOrUndefined.NullOrUndefined (BatchPredictionFilterVariable)
  , "EQ" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined.NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeBatchPredictionsInput :: Newtype DescribeBatchPredictionsInput _
derive instance repGenericDescribeBatchPredictionsInput :: Generic DescribeBatchPredictionsInput _
instance showDescribeBatchPredictionsInput :: Show DescribeBatchPredictionsInput where
  show = genericShow
instance decodeDescribeBatchPredictionsInput :: Decode DescribeBatchPredictionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBatchPredictionsInput :: Encode DescribeBatchPredictionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeBatchPredictions</code> operation. The content is essentially a list of <code>BatchPrediction</code>s.</p>
newtype DescribeBatchPredictionsOutput = DescribeBatchPredictionsOutput 
  { "Results" :: NullOrUndefined.NullOrUndefined (BatchPredictions)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeDescribeBatchPredictionsOutput :: Newtype DescribeBatchPredictionsOutput _
derive instance repGenericDescribeBatchPredictionsOutput :: Generic DescribeBatchPredictionsOutput _
instance showDescribeBatchPredictionsOutput :: Show DescribeBatchPredictionsOutput where
  show = genericShow
instance decodeDescribeBatchPredictionsOutput :: Decode DescribeBatchPredictionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBatchPredictionsOutput :: Encode DescribeBatchPredictionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDataSourcesInput = DescribeDataSourcesInput 
  { "FilterVariable" :: NullOrUndefined.NullOrUndefined (DataSourceFilterVariable)
  , "EQ" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined.NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeDataSourcesInput :: Newtype DescribeDataSourcesInput _
derive instance repGenericDescribeDataSourcesInput :: Generic DescribeDataSourcesInput _
instance showDescribeDataSourcesInput :: Show DescribeDataSourcesInput where
  show = genericShow
instance decodeDescribeDataSourcesInput :: Decode DescribeDataSourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDataSourcesInput :: Encode DescribeDataSourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the query results from a <a>DescribeDataSources</a> operation. The content is essentially a list of <code>DataSource</code>.</p>
newtype DescribeDataSourcesOutput = DescribeDataSourcesOutput 
  { "Results" :: NullOrUndefined.NullOrUndefined (DataSources)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeDescribeDataSourcesOutput :: Newtype DescribeDataSourcesOutput _
derive instance repGenericDescribeDataSourcesOutput :: Generic DescribeDataSourcesOutput _
instance showDescribeDataSourcesOutput :: Show DescribeDataSourcesOutput where
  show = genericShow
instance decodeDescribeDataSourcesOutput :: Decode DescribeDataSourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDataSourcesOutput :: Encode DescribeDataSourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEvaluationsInput = DescribeEvaluationsInput 
  { "FilterVariable" :: NullOrUndefined.NullOrUndefined (EvaluationFilterVariable)
  , "EQ" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined.NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeEvaluationsInput :: Newtype DescribeEvaluationsInput _
derive instance repGenericDescribeEvaluationsInput :: Generic DescribeEvaluationsInput _
instance showDescribeEvaluationsInput :: Show DescribeEvaluationsInput where
  show = genericShow
instance decodeDescribeEvaluationsInput :: Decode DescribeEvaluationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEvaluationsInput :: Encode DescribeEvaluationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the query results from a <code>DescribeEvaluations</code> operation. The content is essentially a list of <code>Evaluation</code>.</p>
newtype DescribeEvaluationsOutput = DescribeEvaluationsOutput 
  { "Results" :: NullOrUndefined.NullOrUndefined (Evaluations)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeDescribeEvaluationsOutput :: Newtype DescribeEvaluationsOutput _
derive instance repGenericDescribeEvaluationsOutput :: Generic DescribeEvaluationsOutput _
instance showDescribeEvaluationsOutput :: Show DescribeEvaluationsOutput where
  show = genericShow
instance decodeDescribeEvaluationsOutput :: Decode DescribeEvaluationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEvaluationsOutput :: Encode DescribeEvaluationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMLModelsInput = DescribeMLModelsInput 
  { "FilterVariable" :: NullOrUndefined.NullOrUndefined (MLModelFilterVariable)
  , "EQ" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LT" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "GE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "LE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "NE" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (ComparatorValue)
  , "SortOrder" :: NullOrUndefined.NullOrUndefined (SortOrder)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PageLimit)
  }
derive instance newtypeDescribeMLModelsInput :: Newtype DescribeMLModelsInput _
derive instance repGenericDescribeMLModelsInput :: Generic DescribeMLModelsInput _
instance showDescribeMLModelsInput :: Show DescribeMLModelsInput where
  show = genericShow
instance decodeDescribeMLModelsInput :: Decode DescribeMLModelsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMLModelsInput :: Encode DescribeMLModelsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeMLModels</code> operation. The content is essentially a list of <code>MLModel</code>.</p>
newtype DescribeMLModelsOutput = DescribeMLModelsOutput 
  { "Results" :: NullOrUndefined.NullOrUndefined (MLModels)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (StringType)
  }
derive instance newtypeDescribeMLModelsOutput :: Newtype DescribeMLModelsOutput _
derive instance repGenericDescribeMLModelsOutput :: Generic DescribeMLModelsOutput _
instance showDescribeMLModelsOutput :: Show DescribeMLModelsOutput where
  show = genericShow
instance decodeDescribeMLModelsOutput :: Decode DescribeMLModelsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMLModelsOutput :: Encode DescribeMLModelsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsInput = DescribeTagsInput 
  { "ResourceId" :: (EntityId)
  , "ResourceType" :: (TaggableResourceType)
  }
derive instance newtypeDescribeTagsInput :: Newtype DescribeTagsInput _
derive instance repGenericDescribeTagsInput :: Generic DescribeTagsInput _
instance showDescribeTagsInput :: Show DescribeTagsInput where
  show = genericShow
instance decodeDescribeTagsInput :: Decode DescribeTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsInput :: Encode DescribeTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon ML returns the following elements. </p>
newtype DescribeTagsOutput = DescribeTagsOutput 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (TaggableResourceType)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeDescribeTagsOutput :: Newtype DescribeTagsOutput _
derive instance repGenericDescribeTagsOutput :: Generic DescribeTagsOutput _
instance showDescribeTagsOutput :: Show DescribeTagsOutput where
  show = genericShow
instance decodeDescribeTagsOutput :: Decode DescribeTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsOutput :: Encode DescribeTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Contains the key values of <code>DetailsMap</code>: <code>PredictiveModelType</code> - Indicates the type of the <code>MLModel</code>. <code>Algorithm</code> - Indicates the algorithm that was used for the <code>MLModel</code>.
newtype DetailsAttributes = DetailsAttributes String
derive instance newtypeDetailsAttributes :: Newtype DetailsAttributes _
derive instance repGenericDetailsAttributes :: Generic DetailsAttributes _
instance showDetailsAttributes :: Show DetailsAttributes where
  show = genericShow
instance decodeDetailsAttributes :: Decode DetailsAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsAttributes :: Encode DetailsAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Provides any additional details regarding the prediction.
newtype DetailsMap = DetailsMap (StrMap.StrMap DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _
derive instance repGenericDetailsMap :: Generic DetailsMap _
instance showDetailsMap :: Show DetailsMap where
  show = genericShow
instance decodeDetailsMap :: Decode DetailsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsMap :: Encode DetailsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _
derive instance repGenericDetailsValue :: Generic DetailsValue _
instance showDetailsValue :: Show DetailsValue where
  show = genericShow
instance decodeDetailsValue :: Decode DetailsValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsValue :: Encode DetailsValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPPipelineId = EDPPipelineId String
derive instance newtypeEDPPipelineId :: Newtype EDPPipelineId _
derive instance repGenericEDPPipelineId :: Generic EDPPipelineId _
instance showEDPPipelineId :: Show EDPPipelineId where
  show = genericShow
instance decodeEDPPipelineId :: Decode EDPPipelineId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPPipelineId :: Encode EDPPipelineId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPResourceRole = EDPResourceRole String
derive instance newtypeEDPResourceRole :: Newtype EDPResourceRole _
derive instance repGenericEDPResourceRole :: Generic EDPResourceRole _
instance showEDPResourceRole :: Show EDPResourceRole where
  show = genericShow
instance decodeEDPResourceRole :: Decode EDPResourceRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPResourceRole :: Encode EDPResourceRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPSecurityGroupId = EDPSecurityGroupId String
derive instance newtypeEDPSecurityGroupId :: Newtype EDPSecurityGroupId _
derive instance repGenericEDPSecurityGroupId :: Generic EDPSecurityGroupId _
instance showEDPSecurityGroupId :: Show EDPSecurityGroupId where
  show = genericShow
instance decodeEDPSecurityGroupId :: Decode EDPSecurityGroupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPSecurityGroupId :: Encode EDPSecurityGroupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPSecurityGroupIds = EDPSecurityGroupIds (Array EDPSecurityGroupId)
derive instance newtypeEDPSecurityGroupIds :: Newtype EDPSecurityGroupIds _
derive instance repGenericEDPSecurityGroupIds :: Generic EDPSecurityGroupIds _
instance showEDPSecurityGroupIds :: Show EDPSecurityGroupIds where
  show = genericShow
instance decodeEDPSecurityGroupIds :: Decode EDPSecurityGroupIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPSecurityGroupIds :: Encode EDPSecurityGroupIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPServiceRole = EDPServiceRole String
derive instance newtypeEDPServiceRole :: Newtype EDPServiceRole _
derive instance repGenericEDPServiceRole :: Generic EDPServiceRole _
instance showEDPServiceRole :: Show EDPServiceRole where
  show = genericShow
instance decodeEDPServiceRole :: Decode EDPServiceRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPServiceRole :: Encode EDPServiceRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EDPSubnetId = EDPSubnetId String
derive instance newtypeEDPSubnetId :: Newtype EDPSubnetId _
derive instance repGenericEDPSubnetId :: Generic EDPSubnetId _
instance showEDPSubnetId :: Show EDPSubnetId where
  show = genericShow
instance decodeEDPSubnetId :: Decode EDPSubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEDPSubnetId :: Encode EDPSubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityId = EntityId String
derive instance newtypeEntityId :: Newtype EntityId _
derive instance repGenericEntityId :: Generic EntityId _
instance showEntityId :: Show EntityId where
  show = genericShow
instance decodeEntityId :: Decode EntityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityId :: Encode EntityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A user-supplied name or description of the Amazon ML resource.</p>
newtype EntityName = EntityName String
derive instance newtypeEntityName :: Newtype EntityName _
derive instance repGenericEntityName :: Generic EntityName _
instance showEntityName :: Show EntityName where
  show = genericShow
instance decodeEntityName :: Decode EntityName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityName :: Encode EntityName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Object status with the following possible values:</p> <ul> <li><code>PENDING</code></li> <li><code>INPROGRESS</code></li> <li><code>FAILED</code></li> <li><code>COMPLETED</code></li> <li><code>DELETED</code></li> </ul>
newtype EntityStatus = EntityStatus String
derive instance newtypeEntityStatus :: Newtype EntityStatus _
derive instance repGenericEntityStatus :: Generic EntityStatus _
instance showEntityStatus :: Show EntityStatus where
  show = genericShow
instance decodeEntityStatus :: Decode EntityStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityStatus :: Encode EntityStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A timestamp represented in epoch time.</p>
newtype EpochTime = EpochTime Number
derive instance newtypeEpochTime :: Newtype EpochTime _
derive instance repGenericEpochTime :: Generic EpochTime _
instance showEpochTime :: Show EpochTime where
  show = genericShow
instance decodeEpochTime :: Decode EpochTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEpochTime :: Encode EpochTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode Int
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


-- | <p> Represents the output of <code>GetEvaluation</code> operation. </p> <p>The content consists of the detailed metadata and data file information and the current status of the <code>Evaluation</code>.</p>
newtype Evaluation = Evaluation 
  { "EvaluationId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "EvaluationDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "PerformanceMetrics" :: NullOrUndefined.NullOrUndefined (PerformanceMetrics)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  }
derive instance newtypeEvaluation :: Newtype Evaluation _
derive instance repGenericEvaluation :: Generic Evaluation _
instance showEvaluation :: Show Evaluation where
  show = genericShow
instance decodeEvaluation :: Decode Evaluation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluation :: Encode Evaluation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the variables to use in searching or filtering <code>Evaluation</code>.</p> <ul> <li> <code>CreatedAt</code> - Sets the search criteria to <code>Evaluation</code> creation date.</li> <li> <code>Status</code> - Sets the search criteria to <code>Evaluation</code> status.</li> <li> <code>Name</code> - Sets the search criteria to the contents of <code>Evaluation</code> <b> </b> <code>Name</code>.</li> <li> <code>IAMUser</code> - Sets the search criteria to the user account that invoked an evaluation.</li> <li> <code>MLModelId</code> - Sets the search criteria to the <code>Predictor</code> that was evaluated.</li> <li> <code>DataSourceId</code> - Sets the search criteria to the <code>DataSource</code> used in evaluation.</li> <li> <code>DataUri</code> - Sets the search criteria to the data file(s) used in evaluation. The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.</li> </ul>
newtype EvaluationFilterVariable = EvaluationFilterVariable String
derive instance newtypeEvaluationFilterVariable :: Newtype EvaluationFilterVariable _
derive instance repGenericEvaluationFilterVariable :: Generic EvaluationFilterVariable _
instance showEvaluationFilterVariable :: Show EvaluationFilterVariable where
  show = genericShow
instance decodeEvaluationFilterVariable :: Decode EvaluationFilterVariable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationFilterVariable :: Encode EvaluationFilterVariable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Evaluations = Evaluations (Array Evaluation)
derive instance newtypeEvaluations :: Newtype Evaluations _
derive instance repGenericEvaluations :: Generic Evaluations _
instance showEvaluations :: Show Evaluations where
  show = genericShow
instance decodeEvaluations :: Decode Evaluations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluations :: Encode Evaluations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetBatchPredictionInput = GetBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  }
derive instance newtypeGetBatchPredictionInput :: Newtype GetBatchPredictionInput _
derive instance repGenericGetBatchPredictionInput :: Generic GetBatchPredictionInput _
instance showGetBatchPredictionInput :: Show GetBatchPredictionInput where
  show = genericShow
instance decodeGetBatchPredictionInput :: Decode GetBatchPredictionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBatchPredictionInput :: Encode GetBatchPredictionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetBatchPrediction</code> operation and describes a <code>BatchPrediction</code>.</p>
newtype GetBatchPredictionOutput = GetBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "BatchPredictionDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "OutputUri" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "TotalRecordCount" :: NullOrUndefined.NullOrUndefined (LongType)
  , "InvalidRecordCount" :: NullOrUndefined.NullOrUndefined (LongType)
  }
derive instance newtypeGetBatchPredictionOutput :: Newtype GetBatchPredictionOutput _
derive instance repGenericGetBatchPredictionOutput :: Generic GetBatchPredictionOutput _
instance showGetBatchPredictionOutput :: Show GetBatchPredictionOutput where
  show = genericShow
instance decodeGetBatchPredictionOutput :: Decode GetBatchPredictionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetBatchPredictionOutput :: Encode GetBatchPredictionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDataSourceInput = GetDataSourceInput 
  { "DataSourceId" :: (EntityId)
  , "Verbose" :: NullOrUndefined.NullOrUndefined (Verbose)
  }
derive instance newtypeGetDataSourceInput :: Newtype GetDataSourceInput _
derive instance repGenericGetDataSourceInput :: Generic GetDataSourceInput _
instance showGetDataSourceInput :: Show GetDataSourceInput where
  show = genericShow
instance decodeGetDataSourceInput :: Decode GetDataSourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataSourceInput :: Encode GetDataSourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetDataSource</code> operation and describes a <code>DataSource</code>.</p>
newtype GetDataSourceOutput = GetDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "DataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "DataRearrangement" :: NullOrUndefined.NullOrUndefined (DataRearrangement)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "DataSizeInBytes" :: NullOrUndefined.NullOrUndefined (LongType)
  , "NumberOfFiles" :: NullOrUndefined.NullOrUndefined (LongType)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "RedshiftMetadata" :: NullOrUndefined.NullOrUndefined (RedshiftMetadata)
  , "RDSMetadata" :: NullOrUndefined.NullOrUndefined (RDSMetadata)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "ComputeStatistics" :: NullOrUndefined.NullOrUndefined (ComputeStatistics)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "DataSourceSchema" :: NullOrUndefined.NullOrUndefined (DataSchema)
  }
derive instance newtypeGetDataSourceOutput :: Newtype GetDataSourceOutput _
derive instance repGenericGetDataSourceOutput :: Generic GetDataSourceOutput _
instance showGetDataSourceOutput :: Show GetDataSourceOutput where
  show = genericShow
instance decodeGetDataSourceOutput :: Decode GetDataSourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataSourceOutput :: Encode GetDataSourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEvaluationInput = GetEvaluationInput 
  { "EvaluationId" :: (EntityId)
  }
derive instance newtypeGetEvaluationInput :: Newtype GetEvaluationInput _
derive instance repGenericGetEvaluationInput :: Generic GetEvaluationInput _
instance showGetEvaluationInput :: Show GetEvaluationInput where
  show = genericShow
instance decodeGetEvaluationInput :: Decode GetEvaluationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEvaluationInput :: Encode GetEvaluationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetEvaluation</code> operation and describes an <code>Evaluation</code>.</p>
newtype GetEvaluationOutput = GetEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "EvaluationDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "PerformanceMetrics" :: NullOrUndefined.NullOrUndefined (PerformanceMetrics)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  }
derive instance newtypeGetEvaluationOutput :: Newtype GetEvaluationOutput _
derive instance repGenericGetEvaluationOutput :: Generic GetEvaluationOutput _
instance showGetEvaluationOutput :: Show GetEvaluationOutput where
  show = genericShow
instance decodeGetEvaluationOutput :: Decode GetEvaluationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEvaluationOutput :: Encode GetEvaluationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMLModelInput = GetMLModelInput 
  { "MLModelId" :: (EntityId)
  , "Verbose" :: NullOrUndefined.NullOrUndefined (Verbose)
  }
derive instance newtypeGetMLModelInput :: Newtype GetMLModelInput _
derive instance repGenericGetMLModelInput :: Generic GetMLModelInput _
instance showGetMLModelInput :: Show GetMLModelInput where
  show = genericShow
instance decodeGetMLModelInput :: Decode GetMLModelInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMLModelInput :: Encode GetMLModelInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetMLModel</code> operation, and provides detailed information about a <code>MLModel</code>.</p>
newtype GetMLModelOutput = GetMLModelOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "TrainingDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (MLModelName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "SizeInBytes" :: NullOrUndefined.NullOrUndefined (LongType)
  , "EndpointInfo" :: NullOrUndefined.NullOrUndefined (RealtimeEndpointInfo)
  , "TrainingParameters" :: NullOrUndefined.NullOrUndefined (TrainingParameters)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "MLModelType" :: NullOrUndefined.NullOrUndefined (MLModelType)
  , "ScoreThreshold" :: NullOrUndefined.NullOrUndefined (ScoreThreshold)
  , "ScoreThresholdLastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (PresignedS3Url)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Recipe" :: NullOrUndefined.NullOrUndefined (Recipe)
  , "Schema" :: NullOrUndefined.NullOrUndefined (DataSchema)
  }
derive instance newtypeGetMLModelOutput :: Newtype GetMLModelOutput _
derive instance repGenericGetMLModelOutput :: Generic GetMLModelOutput _
instance showGetMLModelOutput :: Show GetMLModelOutput where
  show = genericShow
instance decodeGetMLModelOutput :: Decode GetMLModelOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMLModelOutput :: Encode GetMLModelOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A second request to use or change an object was not allowed. This can result from retrying a request using a parameter that was not present in the original request.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _
derive instance repGenericIdempotentParameterMismatchException :: Generic IdempotentParameterMismatchException _
instance showIdempotentParameterMismatchException :: Show IdempotentParameterMismatchException where
  show = genericShow
instance decodeIdempotentParameterMismatchException :: Decode IdempotentParameterMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotentParameterMismatchException :: Encode IdempotentParameterMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Integer type that is a 32-bit signed number.</p>
newtype IntegerType = IntegerType Int
derive instance newtypeIntegerType :: Newtype IntegerType _
derive instance repGenericIntegerType :: Generic IntegerType _
instance showIntegerType :: Show IntegerType where
  show = genericShow
instance decodeIntegerType :: Decode IntegerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerType :: Encode IntegerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error on the server occurred when trying to process a request.</p>
newtype InternalServerException = InternalServerException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _
derive instance repGenericInternalServerException :: Generic InternalServerException _
instance showInternalServerException :: Show InternalServerException where
  show = genericShow
instance decodeInternalServerException :: Decode InternalServerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerException :: Encode InternalServerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error on the client occurred. Typically, the cause is an invalid input value.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _
derive instance repGenericInvalidInputException :: Generic InvalidInputException _
instance showInvalidInputException :: Show InvalidInputException where
  show = genericShow
instance decodeInvalidInputException :: Decode InvalidInputException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInputException :: Encode InvalidInputException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidTagException = InvalidTagException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidTagException :: Newtype InvalidTagException _
derive instance repGenericInvalidTagException :: Generic InvalidTagException _
instance showInvalidTagException :: Show InvalidTagException where
  show = genericShow
instance decodeInvalidTagException :: Decode InvalidTagException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTagException :: Encode InvalidTagException where
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


-- | <p>The subscriber exceeded the maximum number of operations. This exception can occur when listing objects such as <code>DataSource</code>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Long integer type that is a 64-bit signed number.</p>
newtype LongType = LongType Number
derive instance newtypeLongType :: Newtype LongType _
derive instance repGenericLongType :: Generic LongType _
instance showLongType :: Show LongType where
  show = genericShow
instance decodeLongType :: Decode LongType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLongType :: Encode LongType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Represents the output of a <code>GetMLModel</code> operation. </p> <p>The content consists of the detailed metadata and the current status of the <code>MLModel</code>.</p>
newtype MLModel = MLModel 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "TrainingDataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  , "CreatedByIamUser" :: NullOrUndefined.NullOrUndefined (AwsUserArn)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "LastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (MLModelName)
  , "Status" :: NullOrUndefined.NullOrUndefined (EntityStatus)
  , "SizeInBytes" :: NullOrUndefined.NullOrUndefined (LongType)
  , "EndpointInfo" :: NullOrUndefined.NullOrUndefined (RealtimeEndpointInfo)
  , "TrainingParameters" :: NullOrUndefined.NullOrUndefined (TrainingParameters)
  , "InputDataLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "Algorithm" :: NullOrUndefined.NullOrUndefined (Algorithm)
  , "MLModelType" :: NullOrUndefined.NullOrUndefined (MLModelType)
  , "ScoreThreshold" :: NullOrUndefined.NullOrUndefined (ScoreThreshold)
  , "ScoreThresholdLastUpdatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  , "ComputeTime" :: NullOrUndefined.NullOrUndefined (LongType)
  , "FinishedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "StartedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  }
derive instance newtypeMLModel :: Newtype MLModel _
derive instance repGenericMLModel :: Generic MLModel _
instance showMLModel :: Show MLModel where
  show = genericShow
instance decodeMLModel :: Decode MLModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMLModel :: Encode MLModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MLModelFilterVariable = MLModelFilterVariable String
derive instance newtypeMLModelFilterVariable :: Newtype MLModelFilterVariable _
derive instance repGenericMLModelFilterVariable :: Generic MLModelFilterVariable _
instance showMLModelFilterVariable :: Show MLModelFilterVariable where
  show = genericShow
instance decodeMLModelFilterVariable :: Decode MLModelFilterVariable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMLModelFilterVariable :: Encode MLModelFilterVariable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MLModelName = MLModelName String
derive instance newtypeMLModelName :: Newtype MLModelName _
derive instance repGenericMLModelName :: Generic MLModelName _
instance showMLModelName :: Show MLModelName where
  show = genericShow
instance decodeMLModelName :: Decode MLModelName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMLModelName :: Encode MLModelName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MLModelType = MLModelType String
derive instance newtypeMLModelType :: Newtype MLModelType _
derive instance repGenericMLModelType :: Generic MLModelType _
instance showMLModelType :: Show MLModelType where
  show = genericShow
instance decodeMLModelType :: Decode MLModelType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMLModelType :: Encode MLModelType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MLModels = MLModels (Array MLModel)
derive instance newtypeMLModels :: Newtype MLModels _
derive instance repGenericMLModels :: Generic MLModels _
instance showMLModels :: Show MLModels where
  show = genericShow
instance decodeMLModels :: Decode MLModels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMLModels :: Encode MLModels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Description of the most recent details about an object.</p>
newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageLimit = PageLimit Int
derive instance newtypePageLimit :: Newtype PageLimit _
derive instance repGenericPageLimit :: Generic PageLimit _
instance showPageLimit :: Show PageLimit where
  show = genericShow
instance decodePageLimit :: Decode PageLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageLimit :: Encode PageLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Measurements of how well the <code>MLModel</code> performed on known observations. One of the following metrics is returned, based on the type of the <code>MLModel</code>: </p> <ul> <li> <p>BinaryAUC: The binary <code>MLModel</code> uses the Area Under the Curve (AUC) technique to measure performance. </p> </li> <li> <p>RegressionRMSE: The regression <code>MLModel</code> uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.</p> </li> <li> <p>MulticlassAvgFScore: The multiclass <code>MLModel</code> uses the F1 score technique to measure performance. </p> </li> </ul> <p> For more information about performance metrics, please see the <a href="http://docs.aws.amazon.com/machine-learning/latest/dg">Amazon Machine Learning Developer Guide</a>. </p>
newtype PerformanceMetrics = PerformanceMetrics 
  { "Properties" :: NullOrUndefined.NullOrUndefined (PerformanceMetricsProperties)
  }
derive instance newtypePerformanceMetrics :: Newtype PerformanceMetrics _
derive instance repGenericPerformanceMetrics :: Generic PerformanceMetrics _
instance showPerformanceMetrics :: Show PerformanceMetrics where
  show = genericShow
instance decodePerformanceMetrics :: Decode PerformanceMetrics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePerformanceMetrics :: Encode PerformanceMetrics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PerformanceMetricsProperties = PerformanceMetricsProperties (StrMap.StrMap PerformanceMetricsPropertyValue)
derive instance newtypePerformanceMetricsProperties :: Newtype PerformanceMetricsProperties _
derive instance repGenericPerformanceMetricsProperties :: Generic PerformanceMetricsProperties _
instance showPerformanceMetricsProperties :: Show PerformanceMetricsProperties where
  show = genericShow
instance decodePerformanceMetricsProperties :: Decode PerformanceMetricsProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePerformanceMetricsProperties :: Encode PerformanceMetricsProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PerformanceMetricsPropertyKey = PerformanceMetricsPropertyKey String
derive instance newtypePerformanceMetricsPropertyKey :: Newtype PerformanceMetricsPropertyKey _
derive instance repGenericPerformanceMetricsPropertyKey :: Generic PerformanceMetricsPropertyKey _
instance showPerformanceMetricsPropertyKey :: Show PerformanceMetricsPropertyKey where
  show = genericShow
instance decodePerformanceMetricsPropertyKey :: Decode PerformanceMetricsPropertyKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePerformanceMetricsPropertyKey :: Encode PerformanceMetricsPropertyKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PerformanceMetricsPropertyValue = PerformanceMetricsPropertyValue String
derive instance newtypePerformanceMetricsPropertyValue :: Newtype PerformanceMetricsPropertyValue _
derive instance repGenericPerformanceMetricsPropertyValue :: Generic PerformanceMetricsPropertyValue _
instance showPerformanceMetricsPropertyValue :: Show PerformanceMetricsPropertyValue where
  show = genericShow
instance decodePerformanceMetricsPropertyValue :: Decode PerformanceMetricsPropertyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePerformanceMetricsPropertyValue :: Encode PerformanceMetricsPropertyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PredictInput = PredictInput 
  { "MLModelId" :: (EntityId)
  , "Record''" :: (Record'')
  , "PredictEndpoint" :: (VipURL)
  }
derive instance newtypePredictInput :: Newtype PredictInput _
derive instance repGenericPredictInput :: Generic PredictInput _
instance showPredictInput :: Show PredictInput where
  show = genericShow
instance decodePredictInput :: Decode PredictInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredictInput :: Encode PredictInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PredictOutput = PredictOutput 
  { "Prediction" :: NullOrUndefined.NullOrUndefined (Prediction)
  }
derive instance newtypePredictOutput :: Newtype PredictOutput _
derive instance repGenericPredictOutput :: Generic PredictOutput _
instance showPredictOutput :: Show PredictOutput where
  show = genericShow
instance decodePredictOutput :: Decode PredictOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredictOutput :: Encode PredictOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output from a <code>Predict</code> operation: </p> <ul> <li> <p> <code>Details</code> - Contains the following attributes: <code>DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS</code> <code>DetailsAttributes.ALGORITHM - SGD</code> </p> </li> <li> <p> <code>PredictedLabel</code> - Present for either a <code>BINARY</code> or <code>MULTICLASS</code> <code>MLModel</code> request. </p> </li> <li> <p> <code>PredictedScores</code> - Contains the raw classification score corresponding to each label. </p> </li> <li> <p> <code>PredictedValue</code> - Present for a <code>REGRESSION</code> <code>MLModel</code> request. </p> </li> </ul>
newtype Prediction = Prediction 
  { "PredictedLabel'" :: NullOrUndefined.NullOrUndefined (Label)
  , "PredictedValue'" :: NullOrUndefined.NullOrUndefined (FloatLabel')
  , "PredictedScores'" :: NullOrUndefined.NullOrUndefined (ScoreValuePerLabelMap)
  , "Details'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  }
derive instance newtypePrediction :: Newtype Prediction _
derive instance repGenericPrediction :: Generic Prediction _
instance showPrediction :: Show Prediction where
  show = genericShow
instance decodePrediction :: Decode Prediction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrediction :: Encode Prediction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The exception is thrown when a predict request is made to an unmounted <code>MLModel</code>.</p>
newtype PredictorNotMountedException = PredictorNotMountedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePredictorNotMountedException :: Newtype PredictorNotMountedException _
derive instance repGenericPredictorNotMountedException :: Generic PredictorNotMountedException _
instance showPredictorNotMountedException :: Show PredictorNotMountedException where
  show = genericShow
instance decodePredictorNotMountedException :: Decode PredictorNotMountedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredictorNotMountedException :: Encode PredictorNotMountedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PresignedS3Url = PresignedS3Url String
derive instance newtypePresignedS3Url :: Newtype PresignedS3Url _
derive instance repGenericPresignedS3Url :: Generic PresignedS3Url _
instance showPresignedS3Url :: Show PresignedS3Url where
  show = genericShow
instance decodePresignedS3Url :: Decode PresignedS3Url where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresignedS3Url :: Encode PresignedS3Url where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The data specification of an Amazon Relational Database Service (Amazon RDS) <code>DataSource</code>.</p>
newtype RDSDataSpec = RDSDataSpec 
  { "DatabaseInformation" :: (RDSDatabase)
  , "SelectSqlQuery" :: (RDSSelectSqlQuery)
  , "DatabaseCredentials" :: (RDSDatabaseCredentials)
  , "S3StagingLocation" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined.NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined.NullOrUndefined (DataSchema)
  , "DataSchemaUri" :: NullOrUndefined.NullOrUndefined (S3Url)
  , "ResourceRole" :: (EDPResourceRole)
  , "ServiceRole" :: (EDPServiceRole)
  , "SubnetId" :: (EDPSubnetId)
  , "SecurityGroupIds" :: (EDPSecurityGroupIds)
  }
derive instance newtypeRDSDataSpec :: Newtype RDSDataSpec _
derive instance repGenericRDSDataSpec :: Generic RDSDataSpec _
instance showRDSDataSpec :: Show RDSDataSpec where
  show = genericShow
instance decodeRDSDataSpec :: Decode RDSDataSpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDataSpec :: Encode RDSDataSpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The database details of an Amazon RDS database.</p>
newtype RDSDatabase = RDSDatabase 
  { "InstanceIdentifier" :: (RDSInstanceIdentifier)
  , "DatabaseName" :: (RDSDatabaseName)
  }
derive instance newtypeRDSDatabase :: Newtype RDSDatabase _
derive instance repGenericRDSDatabase :: Generic RDSDatabase _
instance showRDSDatabase :: Show RDSDatabase where
  show = genericShow
instance decodeRDSDatabase :: Decode RDSDatabase where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDatabase :: Encode RDSDatabase where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The database credentials to connect to a database on an RDS DB instance.</p>
newtype RDSDatabaseCredentials = RDSDatabaseCredentials 
  { "Username" :: (RDSDatabaseUsername)
  , "Password" :: (RDSDatabasePassword)
  }
derive instance newtypeRDSDatabaseCredentials :: Newtype RDSDatabaseCredentials _
derive instance repGenericRDSDatabaseCredentials :: Generic RDSDatabaseCredentials _
instance showRDSDatabaseCredentials :: Show RDSDatabaseCredentials where
  show = genericShow
instance decodeRDSDatabaseCredentials :: Decode RDSDatabaseCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDatabaseCredentials :: Encode RDSDatabaseCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of a database hosted on an RDS DB instance.</p>
newtype RDSDatabaseName = RDSDatabaseName String
derive instance newtypeRDSDatabaseName :: Newtype RDSDatabaseName _
derive instance repGenericRDSDatabaseName :: Generic RDSDatabaseName _
instance showRDSDatabaseName :: Show RDSDatabaseName where
  show = genericShow
instance decodeRDSDatabaseName :: Decode RDSDatabaseName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDatabaseName :: Encode RDSDatabaseName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The password to be used by Amazon ML to connect to a database on an RDS DB instance. The password should have sufficient permissions to execute the <code>RDSSelectQuery</code> query.</p>
newtype RDSDatabasePassword = RDSDatabasePassword String
derive instance newtypeRDSDatabasePassword :: Newtype RDSDatabasePassword _
derive instance repGenericRDSDatabasePassword :: Generic RDSDatabasePassword _
instance showRDSDatabasePassword :: Show RDSDatabasePassword where
  show = genericShow
instance decodeRDSDatabasePassword :: Decode RDSDatabasePassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDatabasePassword :: Encode RDSDatabasePassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The username to be used by Amazon ML to connect to database on an Amazon RDS instance. The username should have sufficient permissions to execute an <code>RDSSelectSqlQuery</code> query.</p>
newtype RDSDatabaseUsername = RDSDatabaseUsername String
derive instance newtypeRDSDatabaseUsername :: Newtype RDSDatabaseUsername _
derive instance repGenericRDSDatabaseUsername :: Generic RDSDatabaseUsername _
instance showRDSDatabaseUsername :: Show RDSDatabaseUsername where
  show = genericShow
instance decodeRDSDatabaseUsername :: Decode RDSDatabaseUsername where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSDatabaseUsername :: Encode RDSDatabaseUsername where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Identifier of RDS DB Instances.
newtype RDSInstanceIdentifier = RDSInstanceIdentifier String
derive instance newtypeRDSInstanceIdentifier :: Newtype RDSInstanceIdentifier _
derive instance repGenericRDSInstanceIdentifier :: Generic RDSInstanceIdentifier _
instance showRDSInstanceIdentifier :: Show RDSInstanceIdentifier where
  show = genericShow
instance decodeRDSInstanceIdentifier :: Decode RDSInstanceIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSInstanceIdentifier :: Encode RDSInstanceIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The datasource details that are specific to Amazon RDS.</p>
newtype RDSMetadata = RDSMetadata 
  { "Database" :: NullOrUndefined.NullOrUndefined (RDSDatabase)
  , "DatabaseUserName" :: NullOrUndefined.NullOrUndefined (RDSDatabaseUsername)
  , "SelectSqlQuery" :: NullOrUndefined.NullOrUndefined (RDSSelectSqlQuery)
  , "ResourceRole" :: NullOrUndefined.NullOrUndefined (EDPResourceRole)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (EDPServiceRole)
  , "DataPipelineId" :: NullOrUndefined.NullOrUndefined (EDPPipelineId)
  }
derive instance newtypeRDSMetadata :: Newtype RDSMetadata _
derive instance repGenericRDSMetadata :: Generic RDSMetadata _
instance showRDSMetadata :: Show RDSMetadata where
  show = genericShow
instance decodeRDSMetadata :: Decode RDSMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSMetadata :: Encode RDSMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SQL query to be executed against the Amazon RDS database. The SQL query should be valid for the Amazon RDS type being used. </p>
newtype RDSSelectSqlQuery = RDSSelectSqlQuery String
derive instance newtypeRDSSelectSqlQuery :: Newtype RDSSelectSqlQuery _
derive instance repGenericRDSSelectSqlQuery :: Generic RDSSelectSqlQuery _
instance showRDSSelectSqlQuery :: Show RDSSelectSqlQuery where
  show = genericShow
instance decodeRDSSelectSqlQuery :: Decode RDSSelectSqlQuery where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRDSSelectSqlQuery :: Encode RDSSelectSqlQuery where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the real-time endpoint information for an <code>MLModel</code>.</p>
newtype RealtimeEndpointInfo = RealtimeEndpointInfo 
  { "PeakRequestsPerSecond" :: NullOrUndefined.NullOrUndefined (IntegerType)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (EpochTime)
  , "EndpointUrl" :: NullOrUndefined.NullOrUndefined (VipURL)
  , "EndpointStatus" :: NullOrUndefined.NullOrUndefined (RealtimeEndpointStatus)
  }
derive instance newtypeRealtimeEndpointInfo :: Newtype RealtimeEndpointInfo _
derive instance repGenericRealtimeEndpointInfo :: Generic RealtimeEndpointInfo _
instance showRealtimeEndpointInfo :: Show RealtimeEndpointInfo where
  show = genericShow
instance decodeRealtimeEndpointInfo :: Decode RealtimeEndpointInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRealtimeEndpointInfo :: Encode RealtimeEndpointInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RealtimeEndpointStatus = RealtimeEndpointStatus String
derive instance newtypeRealtimeEndpointStatus :: Newtype RealtimeEndpointStatus _
derive instance repGenericRealtimeEndpointStatus :: Generic RealtimeEndpointStatus _
instance showRealtimeEndpointStatus :: Show RealtimeEndpointStatus where
  show = genericShow
instance decodeRealtimeEndpointStatus :: Decode RealtimeEndpointStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRealtimeEndpointStatus :: Encode RealtimeEndpointStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Recipe = Recipe String
derive instance newtypeRecipe :: Newtype Recipe _
derive instance repGenericRecipe :: Generic Recipe _
instance showRecipe :: Show Recipe where
  show = genericShow
instance decodeRecipe :: Decode Recipe where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecipe :: Encode Recipe where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A map of variable name-value pairs that represent an observation.</p>
newtype Record'' = Record'' (StrMap.StrMap VariableValue)
derive instance newtypeRecord'' :: Newtype Record'' _
derive instance repGenericRecord'' :: Generic Record'' _
instance showRecord'' :: Show Record'' where
  show = genericShow
instance decodeRecord'' :: Decode Record'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecord'' :: Encode Record'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of an Amazon Redshift cluster.</p>
newtype RedshiftClusterIdentifier = RedshiftClusterIdentifier String
derive instance newtypeRedshiftClusterIdentifier :: Newtype RedshiftClusterIdentifier _
derive instance repGenericRedshiftClusterIdentifier :: Generic RedshiftClusterIdentifier _
instance showRedshiftClusterIdentifier :: Show RedshiftClusterIdentifier where
  show = genericShow
instance decodeRedshiftClusterIdentifier :: Decode RedshiftClusterIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftClusterIdentifier :: Encode RedshiftClusterIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the data specification of an Amazon Redshift <code>DataSource</code>.</p>
newtype RedshiftDataSpec = RedshiftDataSpec 
  { "DatabaseInformation" :: (RedshiftDatabase)
  , "SelectSqlQuery" :: (RedshiftSelectSqlQuery)
  , "DatabaseCredentials" :: (RedshiftDatabaseCredentials)
  , "S3StagingLocation" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined.NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined.NullOrUndefined (DataSchema)
  , "DataSchemaUri" :: NullOrUndefined.NullOrUndefined (S3Url)
  }
derive instance newtypeRedshiftDataSpec :: Newtype RedshiftDataSpec _
derive instance repGenericRedshiftDataSpec :: Generic RedshiftDataSpec _
instance showRedshiftDataSpec :: Show RedshiftDataSpec where
  show = genericShow
instance decodeRedshiftDataSpec :: Decode RedshiftDataSpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDataSpec :: Encode RedshiftDataSpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the database details required to connect to an Amazon Redshift database.</p>
newtype RedshiftDatabase = RedshiftDatabase 
  { "DatabaseName" :: (RedshiftDatabaseName)
  , "ClusterIdentifier" :: (RedshiftClusterIdentifier)
  }
derive instance newtypeRedshiftDatabase :: Newtype RedshiftDatabase _
derive instance repGenericRedshiftDatabase :: Generic RedshiftDatabase _
instance showRedshiftDatabase :: Show RedshiftDatabase where
  show = genericShow
instance decodeRedshiftDatabase :: Decode RedshiftDatabase where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDatabase :: Encode RedshiftDatabase where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the database credentials for connecting to a database on an Amazon Redshift cluster.</p>
newtype RedshiftDatabaseCredentials = RedshiftDatabaseCredentials 
  { "Username" :: (RedshiftDatabaseUsername)
  , "Password" :: (RedshiftDatabasePassword)
  }
derive instance newtypeRedshiftDatabaseCredentials :: Newtype RedshiftDatabaseCredentials _
derive instance repGenericRedshiftDatabaseCredentials :: Generic RedshiftDatabaseCredentials _
instance showRedshiftDatabaseCredentials :: Show RedshiftDatabaseCredentials where
  show = genericShow
instance decodeRedshiftDatabaseCredentials :: Decode RedshiftDatabaseCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDatabaseCredentials :: Encode RedshiftDatabaseCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of a database hosted on an Amazon Redshift cluster.</p>
newtype RedshiftDatabaseName = RedshiftDatabaseName String
derive instance newtypeRedshiftDatabaseName :: Newtype RedshiftDatabaseName _
derive instance repGenericRedshiftDatabaseName :: Generic RedshiftDatabaseName _
instance showRedshiftDatabaseName :: Show RedshiftDatabaseName where
  show = genericShow
instance decodeRedshiftDatabaseName :: Decode RedshiftDatabaseName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDatabaseName :: Encode RedshiftDatabaseName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A password to be used by Amazon ML to connect to a database on an Amazon Redshift cluster. The password should have sufficient permissions to execute a <code>RedshiftSelectSqlQuery</code> query. The password should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>
newtype RedshiftDatabasePassword = RedshiftDatabasePassword String
derive instance newtypeRedshiftDatabasePassword :: Newtype RedshiftDatabasePassword _
derive instance repGenericRedshiftDatabasePassword :: Generic RedshiftDatabasePassword _
instance showRedshiftDatabasePassword :: Show RedshiftDatabasePassword where
  show = genericShow
instance decodeRedshiftDatabasePassword :: Decode RedshiftDatabasePassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDatabasePassword :: Encode RedshiftDatabasePassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A username to be used by Amazon Machine Learning (Amazon ML)to connect to a database on an Amazon Redshift cluster. The username should have sufficient permissions to execute the <code>RedshiftSelectSqlQuery</code> query. The username should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html">USER</a>.</p>
newtype RedshiftDatabaseUsername = RedshiftDatabaseUsername String
derive instance newtypeRedshiftDatabaseUsername :: Newtype RedshiftDatabaseUsername _
derive instance repGenericRedshiftDatabaseUsername :: Generic RedshiftDatabaseUsername _
instance showRedshiftDatabaseUsername :: Show RedshiftDatabaseUsername where
  show = genericShow
instance decodeRedshiftDatabaseUsername :: Decode RedshiftDatabaseUsername where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDatabaseUsername :: Encode RedshiftDatabaseUsername where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the <code>DataSource</code> details specific to Amazon Redshift.</p>
newtype RedshiftMetadata = RedshiftMetadata 
  { "RedshiftDatabase" :: NullOrUndefined.NullOrUndefined (RedshiftDatabase)
  , "DatabaseUserName" :: NullOrUndefined.NullOrUndefined (RedshiftDatabaseUsername)
  , "SelectSqlQuery" :: NullOrUndefined.NullOrUndefined (RedshiftSelectSqlQuery)
  }
derive instance newtypeRedshiftMetadata :: Newtype RedshiftMetadata _
derive instance repGenericRedshiftMetadata :: Generic RedshiftMetadata _
instance showRedshiftMetadata :: Show RedshiftMetadata where
  show = genericShow
instance decodeRedshiftMetadata :: Decode RedshiftMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftMetadata :: Encode RedshiftMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the SQL query to execute on the Amazon Redshift database. The SQL query should be valid for an Amazon Redshift <a href="http://docs.aws.amazon.com/redshift/latest/dg/r_SELECT_synopsis.html">SELECT</a>. </p>
newtype RedshiftSelectSqlQuery = RedshiftSelectSqlQuery String
derive instance newtypeRedshiftSelectSqlQuery :: Newtype RedshiftSelectSqlQuery _
derive instance repGenericRedshiftSelectSqlQuery :: Generic RedshiftSelectSqlQuery _
instance showRedshiftSelectSqlQuery :: Show RedshiftSelectSqlQuery where
  show = genericShow
instance decodeRedshiftSelectSqlQuery :: Decode RedshiftSelectSqlQuery where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftSelectSqlQuery :: Encode RedshiftSelectSqlQuery where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specified resource cannot be located.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Code'" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Resource Name (ARN) of an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html#roles-about-termsandconcepts">AWS IAM Role</a>, such as the following: arn:aws:iam::account:role/rolename. </p>
newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _
derive instance repGenericRoleARN :: Generic RoleARN _
instance showRoleARN :: Show RoleARN where
  show = genericShow
instance decodeRoleARN :: Decode RoleARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleARN :: Encode RoleARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Describes the data specification of a <code>DataSource</code>.</p>
newtype S3DataSpec = S3DataSpec 
  { "DataLocationS3" :: (S3Url)
  , "DataRearrangement" :: NullOrUndefined.NullOrUndefined (DataRearrangement)
  , "DataSchema" :: NullOrUndefined.NullOrUndefined (DataSchema)
  , "DataSchemaLocationS3" :: NullOrUndefined.NullOrUndefined (S3Url)
  }
derive instance newtypeS3DataSpec :: Newtype S3DataSpec _
derive instance repGenericS3DataSpec :: Generic S3DataSpec _
instance showS3DataSpec :: Show S3DataSpec where
  show = genericShow
instance decodeS3DataSpec :: Decode S3DataSpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3DataSpec :: Encode S3DataSpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A reference to a file or bucket on Amazon Simple Storage Service (Amazon S3).</p>
newtype S3Url = S3Url String
derive instance newtypeS3Url :: Newtype S3Url _
derive instance repGenericS3Url :: Generic S3Url _
instance showS3Url :: Show S3Url where
  show = genericShow
instance decodeS3Url :: Decode S3Url where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Url :: Encode S3Url where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScoreThreshold = ScoreThreshold Number
derive instance newtypeScoreThreshold :: Newtype ScoreThreshold _
derive instance repGenericScoreThreshold :: Generic ScoreThreshold _
instance showScoreThreshold :: Show ScoreThreshold where
  show = genericShow
instance decodeScoreThreshold :: Decode ScoreThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScoreThreshold :: Encode ScoreThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScoreValue = ScoreValue Number
derive instance newtypeScoreValue :: Newtype ScoreValue _
derive instance repGenericScoreValue :: Generic ScoreValue _
instance showScoreValue :: Show ScoreValue where
  show = genericShow
instance decodeScoreValue :: Decode ScoreValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScoreValue :: Encode ScoreValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Provides the raw classification score corresponding to each label.
newtype ScoreValuePerLabelMap = ScoreValuePerLabelMap (StrMap.StrMap ScoreValue)
derive instance newtypeScoreValuePerLabelMap :: Newtype ScoreValuePerLabelMap _
derive instance repGenericScoreValuePerLabelMap :: Generic ScoreValuePerLabelMap _
instance showScoreValuePerLabelMap :: Show ScoreValuePerLabelMap where
  show = genericShow
instance decodeScoreValuePerLabelMap :: Decode ScoreValuePerLabelMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScoreValuePerLabelMap :: Encode ScoreValuePerLabelMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The sort order specified in a listing condition. Possible values include the following:</p> <ul> <li> <code>asc</code> - Present the information in ascending order (from A-Z).</li> <li> <code>dsc</code> - Present the information in descending order (from Z-A).</li> </ul>
newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _
derive instance repGenericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow
instance decodeSortOrder :: Decode SortOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSortOrder :: Encode SortOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>String type.</p>
newtype StringType = StringType String
derive instance newtypeStringType :: Newtype StringType _
derive instance repGenericStringType :: Generic StringType _
instance showStringType :: Show StringType where
  show = genericShow
instance decodeStringType :: Decode StringType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringType :: Encode StringType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A custom key-value pair associated with an ML object, such as an ML model.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
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


newtype TagLimitExceededException = TagLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTagLimitExceededException :: Newtype TagLimitExceededException _
derive instance repGenericTagLimitExceededException :: Generic TagLimitExceededException _
instance showTagLimitExceededException :: Show TagLimitExceededException where
  show = genericShow
instance decodeTagLimitExceededException :: Decode TagLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagLimitExceededException :: Encode TagLimitExceededException where
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


newtype TaggableResourceType = TaggableResourceType String
derive instance newtypeTaggableResourceType :: Newtype TaggableResourceType _
derive instance repGenericTaggableResourceType :: Generic TaggableResourceType _
instance showTaggableResourceType :: Show TaggableResourceType where
  show = genericShow
instance decodeTaggableResourceType :: Decode TaggableResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaggableResourceType :: Encode TaggableResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrainingParameters = TrainingParameters (StrMap.StrMap StringType)
derive instance newtypeTrainingParameters :: Newtype TrainingParameters _
derive instance repGenericTrainingParameters :: Generic TrainingParameters _
instance showTrainingParameters :: Show TrainingParameters where
  show = genericShow
instance decodeTrainingParameters :: Decode TrainingParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrainingParameters :: Encode TrainingParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateBatchPredictionInput = UpdateBatchPredictionInput 
  { "BatchPredictionId" :: (EntityId)
  , "BatchPredictionName" :: (EntityName)
  }
derive instance newtypeUpdateBatchPredictionInput :: Newtype UpdateBatchPredictionInput _
derive instance repGenericUpdateBatchPredictionInput :: Generic UpdateBatchPredictionInput _
instance showUpdateBatchPredictionInput :: Show UpdateBatchPredictionInput where
  show = genericShow
instance decodeUpdateBatchPredictionInput :: Decode UpdateBatchPredictionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBatchPredictionInput :: Encode UpdateBatchPredictionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>UpdateBatchPrediction</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>
newtype UpdateBatchPredictionOutput = UpdateBatchPredictionOutput 
  { "BatchPredictionId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateBatchPredictionOutput :: Newtype UpdateBatchPredictionOutput _
derive instance repGenericUpdateBatchPredictionOutput :: Generic UpdateBatchPredictionOutput _
instance showUpdateBatchPredictionOutput :: Show UpdateBatchPredictionOutput where
  show = genericShow
instance decodeUpdateBatchPredictionOutput :: Decode UpdateBatchPredictionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBatchPredictionOutput :: Encode UpdateBatchPredictionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDataSourceInput = UpdateDataSourceInput 
  { "DataSourceId" :: (EntityId)
  , "DataSourceName" :: (EntityName)
  }
derive instance newtypeUpdateDataSourceInput :: Newtype UpdateDataSourceInput _
derive instance repGenericUpdateDataSourceInput :: Generic UpdateDataSourceInput _
instance showUpdateDataSourceInput :: Show UpdateDataSourceInput where
  show = genericShow
instance decodeUpdateDataSourceInput :: Decode UpdateDataSourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDataSourceInput :: Encode UpdateDataSourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>UpdateDataSource</code> operation.</p> <p>You can see the updated content by using the <code>GetBatchPrediction</code> operation.</p>
newtype UpdateDataSourceOutput = UpdateDataSourceOutput 
  { "DataSourceId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateDataSourceOutput :: Newtype UpdateDataSourceOutput _
derive instance repGenericUpdateDataSourceOutput :: Generic UpdateDataSourceOutput _
instance showUpdateDataSourceOutput :: Show UpdateDataSourceOutput where
  show = genericShow
instance decodeUpdateDataSourceOutput :: Decode UpdateDataSourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDataSourceOutput :: Encode UpdateDataSourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateEvaluationInput = UpdateEvaluationInput 
  { "EvaluationId" :: (EntityId)
  , "EvaluationName" :: (EntityName)
  }
derive instance newtypeUpdateEvaluationInput :: Newtype UpdateEvaluationInput _
derive instance repGenericUpdateEvaluationInput :: Generic UpdateEvaluationInput _
instance showUpdateEvaluationInput :: Show UpdateEvaluationInput where
  show = genericShow
instance decodeUpdateEvaluationInput :: Decode UpdateEvaluationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEvaluationInput :: Encode UpdateEvaluationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>UpdateEvaluation</code> operation.</p> <p>You can see the updated content by using the <code>GetEvaluation</code> operation.</p>
newtype UpdateEvaluationOutput = UpdateEvaluationOutput 
  { "EvaluationId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateEvaluationOutput :: Newtype UpdateEvaluationOutput _
derive instance repGenericUpdateEvaluationOutput :: Generic UpdateEvaluationOutput _
instance showUpdateEvaluationOutput :: Show UpdateEvaluationOutput where
  show = genericShow
instance decodeUpdateEvaluationOutput :: Decode UpdateEvaluationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEvaluationOutput :: Encode UpdateEvaluationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMLModelInput = UpdateMLModelInput 
  { "MLModelId" :: (EntityId)
  , "MLModelName" :: NullOrUndefined.NullOrUndefined (EntityName)
  , "ScoreThreshold" :: NullOrUndefined.NullOrUndefined (ScoreThreshold)
  }
derive instance newtypeUpdateMLModelInput :: Newtype UpdateMLModelInput _
derive instance repGenericUpdateMLModelInput :: Generic UpdateMLModelInput _
instance showUpdateMLModelInput :: Show UpdateMLModelInput where
  show = genericShow
instance decodeUpdateMLModelInput :: Decode UpdateMLModelInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMLModelInput :: Encode UpdateMLModelInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an <code>UpdateMLModel</code> operation.</p> <p>You can see the updated content by using the <code>GetMLModel</code> operation.</p>
newtype UpdateMLModelOutput = UpdateMLModelOutput 
  { "MLModelId" :: NullOrUndefined.NullOrUndefined (EntityId)
  }
derive instance newtypeUpdateMLModelOutput :: Newtype UpdateMLModelOutput _
derive instance repGenericUpdateMLModelOutput :: Generic UpdateMLModelOutput _
instance showUpdateMLModelOutput :: Show UpdateMLModelOutput where
  show = genericShow
instance decodeUpdateMLModelOutput :: Decode UpdateMLModelOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMLModelOutput :: Encode UpdateMLModelOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of a variable. Currently it's used to specify the name of the target value, label, weight, and tags.</p>
newtype VariableName = VariableName String
derive instance newtypeVariableName :: Newtype VariableName _
derive instance repGenericVariableName :: Generic VariableName _
instance showVariableName :: Show VariableName where
  show = genericShow
instance decodeVariableName :: Decode VariableName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVariableName :: Encode VariableName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of a variable. Currently it's used to specify values of the target value, weights, and tag variables and for filtering variable values.</p>
newtype VariableValue = VariableValue String
derive instance newtypeVariableValue :: Newtype VariableValue _
derive instance repGenericVariableValue :: Generic VariableValue _
instance showVariableValue :: Show VariableValue where
  show = genericShow
instance decodeVariableValue :: Decode VariableValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVariableValue :: Encode VariableValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies whether a describe operation should return exhaustive or abbreviated information.</p>
newtype Verbose = Verbose Boolean
derive instance newtypeVerbose :: Newtype Verbose _
derive instance repGenericVerbose :: Generic Verbose _
instance showVerbose :: Show Verbose where
  show = genericShow
instance decodeVerbose :: Decode Verbose where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerbose :: Encode Verbose where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VipURL = VipURL String
derive instance newtypeVipURL :: Newtype VipURL _
derive instance repGenericVipURL :: Generic VipURL _
instance showVipURL :: Show VipURL where
  show = genericShow
instance decodeVipURL :: Decode VipURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVipURL :: Encode VipURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FloatLabel' = FloatLabel' Number
derive instance newtypeFloatLabel' :: Newtype FloatLabel' _
derive instance repGenericFloatLabel' :: Generic FloatLabel' _
instance showFloatLabel' :: Show FloatLabel' where
  show = genericShow
instance decodeFloatLabel' :: Decode FloatLabel' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFloatLabel' :: Encode FloatLabel' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
