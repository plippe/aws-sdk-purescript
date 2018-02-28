

-- | <p>This is the Amazon Rekognition API reference.</p>
module AWS.Rekognition where

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

serviceName = "Rekognition" :: String


-- | <p>Compares a face in the <i>source</i> input image with each of the 100 largest faces detected in the <i>target</i> input image. </p> <note> <p> If the source image contains multiple faces, the service detects the largest face and compares it with each face detected in the target image. </p> </note> <p>You pass the input and target images either as base64-encoded image bytes or as a references to images in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>In response, the operation returns an array of face matches ordered by similarity score in descending order. For each face match, the response provides a bounding box of the face, facial landmarks, pose details (pitch, role, and yaw), quality (brightness and sharpness), and confidence value (indicating the level of confidence that the bounding box contains a face). The response also provides a similarity score, which indicates how closely the faces match. </p> <note> <p>By default, only faces with a similarity score of greater than or equal to 80% are returned in the response. You can change this value by specifying the <code>SimilarityThreshold</code> parameter.</p> </note> <p> <code>CompareFaces</code> also returns an array of faces that don't match the source image. For each face, it returns a bounding box, confidence value, landmarks, pose details, and quality. The response also returns information about the face in the source image, including the bounding box of the face and confidence value.</p> <p>If the image doesn't contain Exif metadata, <code>CompareFaces</code> returns orientation information for the source and target images. Use these values to display the images with the correct image orientation.</p> <p>If no faces are detected in the source or target images, <code>CompareFaces</code> returns an <code>InvalidParameterException</code> error. </p> <note> <p> This is a stateless API operation. That is, data returned by this operation doesn't persist.</p> </note> <p>For an example, see <a>faces-compare-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:CompareFaces</code> action.</p>
compareFaces :: forall eff. CompareFacesRequest -> Aff (exception :: EXCEPTION | eff) CompareFacesResponse
compareFaces = Request.request serviceName "compareFaces" 


-- | <p>Creates a collection in an AWS Region. You can add faces to the collection using the operation. </p> <p>For example, you might create collections, one for each of your application users. A user can then index faces using the <code>IndexFaces</code> operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container. </p> <note> <p>Collection names are case-sensitive.</p> </note> <p>This operation requires permissions to perform the <code>rekognition:CreateCollection</code> action.</p>
createCollection :: forall eff. CreateCollectionRequest -> Aff (exception :: EXCEPTION | eff) CreateCollectionResponse
createCollection = Request.request serviceName "createCollection" 


-- | <p>Creates an Amazon Rekognition stream processor that you can use to detect and recognize faces in a streaming video.</p> <p>Rekognition Video is a consumer of live video from Amazon Kinesis Video Streams. Rekognition Video sends analysis results to Amazon Kinesis Data Streams.</p> <p>You provide as input a Kinesis video stream (<code>Input</code>) and a Kinesis data stream (<code>Output</code>) stream. You also specify the face recognition criteria in <code>Settings</code>. For example, the collection containing faces that you want to recognize. Use <code>Name</code> to assign an identifier for the stream processor. You use <code>Name</code> to manage the stream processor. For example, you can start processing the source video by calling with the <code>Name</code> field. </p> <p>After you have finished analyzing a streaming video, use to stop processing. You can delete the stream processor by calling .</p>
createStreamProcessor :: forall eff. CreateStreamProcessorRequest -> Aff (exception :: EXCEPTION | eff) CreateStreamProcessorResponse
createStreamProcessor = Request.request serviceName "createStreamProcessor" 


-- | <p>Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see <a>delete-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteCollection</code> action.</p>
deleteCollection :: forall eff. DeleteCollectionRequest -> Aff (exception :: EXCEPTION | eff) DeleteCollectionResponse
deleteCollection = Request.request serviceName "deleteCollection" 


-- | <p>Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteFaces</code> action.</p>
deleteFaces :: forall eff. DeleteFacesRequest -> Aff (exception :: EXCEPTION | eff) DeleteFacesResponse
deleteFaces = Request.request serviceName "deleteFaces" 


-- | <p>Deletes the stream processor identified by <code>Name</code>. You assign the value for <code>Name</code> when you create the stream processor with . You might not be able to use the same name for a stream processor for a few seconds after calling <code>DeleteStreamProcessor</code>.</p>
deleteStreamProcessor :: forall eff. DeleteStreamProcessorRequest -> Aff (exception :: EXCEPTION | eff) DeleteStreamProcessorResponse
deleteStreamProcessor = Request.request serviceName "deleteStreamProcessor" 


-- | <p>Provides information about a stream processor created by . You can get information about the input and output streams, the input parameters for the face recognition being performed, and the current status of the stream processor.</p>
describeStreamProcessor :: forall eff. DescribeStreamProcessorRequest -> Aff (exception :: EXCEPTION | eff) DescribeStreamProcessorResponse
describeStreamProcessor = Request.request serviceName "describeStreamProcessor" 


-- | <p>Detects faces within an image that is provided as input.</p> <p> <code>DetectFaces</code> detects the 100 largest faces in the image. For each face detected, the operation returns face details including a bounding box of the face, a confidence value (that the bounding box contains a face), and a fixed set of attributes such as facial landmarks (for example, coordinates of eye and mouth), gender, presence of beard, sunglasses, etc. </p> <p>The face-detection algorithm is most effective on frontal faces. For non-frontal or obscured faces, the algorithm may not detect the faces or might detect faces with lower confidence. </p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> </note> <p>For an example, see <a>procedure-detecting-faces-in-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectFaces</code> action. </p>
detectFaces :: forall eff. DetectFacesRequest -> Aff (exception :: EXCEPTION | eff) DetectFacesResponse
detectFaces = Request.request serviceName "detectFaces" 


-- | <p>Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature. For an example, see <a>images-s3</a>.</p> <note> <p> <code>DetectLabels</code> does not support the detection of activities. However, activity detection is supported for label detection in videos. For more information, see .</p> </note> <p>You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response will include all three labels, one for each object. </p> <p> <code>{Name: lighthouse, Confidence: 98.4629}</code> </p> <p> <code>{Name: rock,Confidence: 79.2097}</code> </p> <p> <code> {Name: sea,Confidence: 75.061}</code> </p> <p> In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels. </p> <p> <code>{Name: flower,Confidence: 99.0562}</code> </p> <p> <code>{Name: plant,Confidence: 99.0562}</code> </p> <p> <code>{Name: tulip,Confidence: 99.0562}</code> </p> <p>In this example, the detection algorithm more precisely identifies the flower as a tulip.</p> <p>In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify <code>MinConfidence</code> to control the confidence threshold for the labels returned. The default is 50%. You can also add the <code>MaxLabels</code> parameter to limit the number of labels returned. </p> <note> <p>If the object detected is a person, the operation doesn't provide the same facial details that the <a>DetectFaces</a> operation provides.</p> </note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectLabels</code> action. </p>
detectLabels :: forall eff. DetectLabelsRequest -> Aff (exception :: EXCEPTION | eff) DetectLabelsResponse
detectLabels = Request.request serviceName "detectLabels" 


-- | <p>Detects explicit or suggestive adult content in a specified JPEG or PNG format image. Use <code>DetectModerationLabels</code> to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.</p> <p>To filter images, use the labels returned by <code>DetectModerationLabels</code> to determine which types of content are appropriate. For information about moderation labels, see <a>moderation</a>.</p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p>
detectModerationLabels :: forall eff. DetectModerationLabelsRequest -> Aff (exception :: EXCEPTION | eff) DetectModerationLabelsResponse
detectModerationLabels = Request.request serviceName "detectModerationLabels" 


-- | <p>Detects text in the input image and converts it into machine-readable text.</p> <p>Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file. </p> <p>The <code>DetectText</code> operation returns text in an array of elements, <code>TextDetections</code>. Each <code>TextDetection</code> element provides information about a single word or line of text that was detected in the image. </p> <p>A word is one or more ISO basic latin script characters that are not separated by spaces. <code>DetectText</code> can detect up to 50 words in an image.</p> <p>A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the <code>DetectText</code> operation returns multiple lines.</p> <p>To determine whether a <code>TextDetection</code> element is a line of text or a word, use the <code>TextDetection</code> object <code>Type</code> field. </p> <p>To be detected, text must be within +/- 30 degrees orientation of the horizontal axis.</p> <p>For more information, see <a>text-detection</a>.</p>
detectText :: forall eff. DetectTextRequest -> Aff (exception :: EXCEPTION | eff) DetectTextResponse
detectText = Request.request serviceName "detectText" 


-- | <p>Gets the name and additional information about a celebrity based on his or her Rekognition ID. The additional information is returned as an array of URLs. If there is no additional information about the celebrity, this list is empty. For more information, see <a>get-celebrity-info-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:GetCelebrityInfo</code> action. </p>
getCelebrityInfo :: forall eff. GetCelebrityInfoRequest -> Aff (exception :: EXCEPTION | eff) GetCelebrityInfoResponse
getCelebrityInfo = Request.request serviceName "getCelebrityInfo" 


-- | <p>Gets the celebrity recognition results for a Rekognition Video analysis started by .</p> <p>Celebrity recognition in a video is an asynchronous operation. Analysis is started by a call to which returns a job identifier (<code>JobId</code>). When the celebrity recognition operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartCelebrityRecognition</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>.</p> <p> <code>GetCelebrityRecognition</code> returns detected celebrities and the time(s) they are detected in an array (<code>Celebrities</code>) of objects. Each <code>CelebrityRecognition</code> contains information about the celebrity in a object and the time, <code>Timestamp</code>, the celebrity was detected. </p> <p>By default, the <code>Celebrities</code> array is sorted by time (milliseconds from the start of the video). You can also sort the array by celebrity by specifying the value <code>ID</code> in the <code>SortBy</code> input parameter.</p> <p>The <code>CelebrityDetail</code> object includes the celebrity identifer and additional information urls. If you don't store the additional information urls, you can get them later by calling with the celebrity identifer.</p> <p>No information is returned for faces not recognized as celebrities.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetCelebrityDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetCelebrityRecognition</code>.</p>
getCelebrityRecognition :: forall eff. GetCelebrityRecognitionRequest -> Aff (exception :: EXCEPTION | eff) GetCelebrityRecognitionResponse
getCelebrityRecognition = Request.request serviceName "getCelebrityRecognition" 


-- | <p>Gets the content moderation analysis results for a Rekognition Video analysis started by .</p> <p>Content moderation analysis of a video is an asynchronous operation. You start analysis by calling . which returns a job identifier (<code>JobId</code>). When analysis finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartContentModeration</code>. To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>. </p> <p> <code>GetContentModeration</code> returns detected content moderation labels, and the time they are detected, in an array, <code>ModerationLabels</code>, of objects. </p> <p>By default, the moderated labels are returned sorted by time, in milliseconds from the start of the video. You can also sort them by moderated label by specifying <code>NAME</code> for the <code>SortBy</code> input parameter. </p> <p>Since video analysis can return a large number of results, use the <code>MaxResults</code> parameter to limit the number of labels returned in a single call to <code>GetContentModeration</code>. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetContentModeration</code> and populate the <code>NextToken</code> request parameter with the value of <code>NextToken</code> returned from the previous call to <code>GetContentModeration</code>.</p> <p>For more information, see <a>moderation</a>.</p>
getContentModeration :: forall eff. GetContentModerationRequest -> Aff (exception :: EXCEPTION | eff) GetContentModerationResponse
getContentModeration = Request.request serviceName "getContentModeration" 


-- | <p>Gets face detection results for a Rekognition Video analysis started by .</p> <p>Face detection with Rekognition Video is an asynchronous operation. You start face detection by calling which returns a job identifier (<code>JobId</code>). When the face detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceDetection</code>. To get the results of the face detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>.</p> <p> <code>GetFaceDetection</code> returns an array of detected faces (<code>Faces</code>) sorted by the time the faces were detected. </p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetFaceDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetFaceDetection</code>.</p>
getFaceDetection :: forall eff. GetFaceDetectionRequest -> Aff (exception :: EXCEPTION | eff) GetFaceDetectionResponse
getFaceDetection = Request.request serviceName "getFaceDetection" 


-- | <p>Gets the face search results for Rekognition Video face search started by . The search returns faces in a collection that match the faces of persons detected in a video. It also includes the time(s) that faces are matched in the video.</p> <p>Face search in a video is an asynchronous operation. You start face search by calling to which returns a job identifier (<code>JobId</code>). When the search operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceSearch</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetFaceSearch</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections</a>.</p> <p>The search results are retured in an array, <code>Persons</code>, of objects. Each<code>PersonMatch</code> element contains details about the matching faces in the input collection, person information for the matched person, and the time the person was matched in the video.</p> <p>By default, the <code>Persons</code> array is sorted by the time, in milliseconds from the start of the video, persons are matched. You can also sort by persons by specifying <code>INDEX</code> for the <code>SORTBY</code> input parameter.</p>
getFaceSearch :: forall eff. GetFaceSearchRequest -> Aff (exception :: EXCEPTION | eff) GetFaceSearchResponse
getFaceSearch = Request.request serviceName "getFaceSearch" 


-- | <p>Gets the label detection results of a Rekognition Video analysis started by . </p> <p>The label detection operation is started by a call to which returns a job identifier (<code>JobId</code>). When the label detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartlabelDetection</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p> <code>GetLabelDetection</code> returns an array of detected labels (<code>Labels</code>) sorted by the time the labels were detected. You can also sort by the label name by specifying <code>NAME</code> for the <code>SortBy</code> input parameter.</p> <p>The labels returned include the label name, the percentage confidence in the accuracy of the detected label, and the time the label was detected in the video.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetlabelDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetLabelDetection</code>.</p>
getLabelDetection :: forall eff. GetLabelDetectionRequest -> Aff (exception :: EXCEPTION | eff) GetLabelDetectionResponse
getLabelDetection = Request.request serviceName "getLabelDetection" 


-- | <p>Gets the person tracking results of a Rekognition Video analysis started by .</p> <p>The person detection operation is started by a call to <code>StartPersonTracking</code> which returns a job identifier (<code>JobId</code>). When the person detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartPersonTracking</code>.</p> <p>To get the results of the person tracking operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p> <p> <code>GetPersonTracking</code> returns an array, <code>Persons</code>, of tracked persons and the time(s) they were tracked in the video. </p> <p>By default, the array is sorted by the time(s) a person is tracked in the video. You can sort by tracked persons by specifying <code>INDEX</code> for the <code>SortBy</code> input parameter.</p> <p>Use the <code>MaxResults</code> parameter to limit the number of items returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetPersonTracking</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetPersonTracking</code>.</p>
getPersonTracking :: forall eff. GetPersonTrackingRequest -> Aff (exception :: EXCEPTION | eff) GetPersonTrackingResponse
getPersonTracking = Request.request serviceName "getPersonTracking" 


-- | <p>Detects faces in the input image and adds them to the specified collection. </p> <p>Amazon Rekognition does not save the actual faces detected. Instead, the underlying detection algorithm first detects the faces in the input image, and for each face extracts facial features into a feature vector, and stores it in the back-end database. Amazon Rekognition uses feature vectors when performing face match and search operations using the and operations.</p> <p>If you are using version 1.0 of the face detection model, <code>IndexFaces</code> indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image. To determine which version of the model you are using, check the the value of <code>FaceModelVersion</code> in the response from <code>IndexFaces</code>. For more information, see <a>face-detection-model</a>.</p> <p>If you provide the optional <code>ExternalImageID</code> for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image. </p> <p>In response, the operation returns an array of metadata for all detected faces. This includes, the bounding box of the detected face, confidence value (indicating the bounding box contains a face), a face ID assigned by the service for each face that is detected and stored, and an image ID assigned by the service for the input image. If you request all facial attributes (using the <code>detectionAttributes</code> parameter, Amazon Rekognition returns detailed facial attributes such as facial landmarks (for example, location of eye and mount) and other facial attributes such gender. If you provide the same image, specify the same collection, and use the same external ID in the <code>IndexFaces</code> operation, Amazon Rekognition doesn't save duplicate face metadata. </p> <p>The input image is passed either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>This operation requires permissions to perform the <code>rekognition:IndexFaces</code> action.</p>
indexFaces :: forall eff. IndexFacesRequest -> Aff (exception :: EXCEPTION | eff) IndexFacesResponse
indexFaces = Request.request serviceName "indexFaces" 


-- | <p>Returns list of collection IDs in your account. If the result is truncated, the response also provides a <code>NextToken</code> that you can use in the subsequent request to fetch the next set of collection IDs.</p> <p>For an example, see <a>list-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:ListCollections</code> action.</p>
listCollections :: forall eff. ListCollectionsRequest -> Aff (exception :: EXCEPTION | eff) ListCollectionsResponse
listCollections = Request.request serviceName "listCollections" 


-- | <p>Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see <a>list-faces-in-collection-procedure</a>. </p> <p>This operation requires permissions to perform the <code>rekognition:ListFaces</code> action.</p>
listFaces :: forall eff. ListFacesRequest -> Aff (exception :: EXCEPTION | eff) ListFacesResponse
listFaces = Request.request serviceName "listFaces" 


-- | <p>Gets a list of stream processors that you have created with . </p>
listStreamProcessors :: forall eff. ListStreamProcessorsRequest -> Aff (exception :: EXCEPTION | eff) ListStreamProcessorsResponse
listStreamProcessors = Request.request serviceName "listStreamProcessors" 


-- | <p>Returns an array of celebrities recognized in the input image. For more information, see <a>celebrities</a>. </p> <p> <code>RecognizeCelebrities</code> returns the 100 largest faces in the image. It lists recognized celebrities in the <code>CelebrityFaces</code> array and unrecognized faces in the <code>UnrecognizedFaces</code> array. <code>RecognizeCelebrities</code> doesn't return celebrities whose faces are not amongst the largest 100 faces in the image.</p> <p>For each celebrity recognized, the <code>RecognizeCelebrities</code> returns a <code>Celebrity</code> object. The <code>Celebrity</code> object contains the celebrity name, ID, URL links to additional information, match confidence, and a <code>ComparedFace</code> object that you can use to locate the celebrity's face on the image.</p> <p>Rekognition does not retain information about which images a celebrity has been recognized in. Your application must store this information and use the <code>Celebrity</code> ID property as a unique identifier for the celebrity. If you don't store the celebrity name or additional information URLs returned by <code>RecognizeCelebrities</code>, you will need the ID to identify the celebrity in a call to the operation.</p> <p>You pass the imput image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>For an example, see <a>celebrities-procedure-image</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:RecognizeCelebrities</code> operation.</p>
recognizeCelebrities :: forall eff. RecognizeCelebritiesRequest -> Aff (exception :: EXCEPTION | eff) RecognizeCelebritiesResponse
recognizeCelebrities = Request.request serviceName "recognizeCelebrities" 


-- | <p>For a given input face ID, searches for matching faces in the collection the face belongs to. You get a face ID when you add a face to the collection using the <a>IndexFaces</a> operation. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p>You can also search faces without indexing faces by using the <code>SearchFacesByImage</code> operation.</p> </note> <p> The operation response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match that is found. Along with the metadata, the response also includes a <code>confidence</code> value for each face match, indicating the confidence that the specific face matches the input face. </p> <p>For an example, see <a>search-face-with-id-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFaces</code> action.</p>
searchFaces :: forall eff. SearchFacesRequest -> Aff (exception :: EXCEPTION | eff) SearchFacesResponse
searchFaces = Request.request serviceName "searchFaces" 


-- | <p>For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p> To search for all faces in an input image, you might first call the operation, and then use the face IDs returned in subsequent calls to the operation. </p> <p> You can also call the <code>DetectFaces</code> operation and use the bounding boxes in the response to make face crops, which then you can pass in to the <code>SearchFacesByImage</code> operation. </p> </note> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a <code>similarity</code> indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image. </p> <p>For an example, see <a>search-face-with-image-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFacesByImage</code> action.</p>
searchFacesByImage :: forall eff. SearchFacesByImageRequest -> Aff (exception :: EXCEPTION | eff) SearchFacesByImageResponse
searchFacesByImage = Request.request serviceName "searchFacesByImage" 


-- | <p>Starts asynchronous recognition of celebrities in a stored video.</p> <p>Rekognition Video can detect celebrities in a video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartCelebrityRecognition</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When celebrity recognition analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityRecognition</code>. For more information, see <a>celebrities</a>.</p>
startCelebrityRecognition :: forall eff. StartCelebrityRecognitionRequest -> Aff (exception :: EXCEPTION | eff) StartCelebrityRecognitionResponse
startCelebrityRecognition = Request.request serviceName "startCelebrityRecognition" 


-- | <p> Starts asynchronous detection of explicit or suggestive adult content in a stored video.</p> <p>Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartContentModeration</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When content moderation analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartContentModeration</code>. For more information, see <a>moderation</a>.</p>
startContentModeration :: forall eff. StartContentModerationRequest -> Aff (exception :: EXCEPTION | eff) StartContentModerationResponse
startContentModeration = Request.request serviceName "startContentModeration" 


-- | <p>Starts asynchronous detection of faces in a stored video.</p> <p>Rekognition Video can detect faces in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceDetection</code> returns a job identifier (<code>JobId</code>) that you use to get the results of the operation. When face detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>. For more information, see <a>faces-video</a>.</p>
startFaceDetection :: forall eff. StartFaceDetectionRequest -> Aff (exception :: EXCEPTION | eff) StartFaceDetectionResponse
startFaceDetection = Request.request serviceName "startFaceDetection" 


-- | <p>Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceSearch</code> returns a job identifier (<code>JobId</code>) which you use to get the search results once the search has completed. When searching is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections-search-person</a>.</p>
startFaceSearch :: forall eff. StartFaceSearchRequest -> Aff (exception :: EXCEPTION | eff) StartFaceSearchResponse
startFaceSearch = Request.request serviceName "startFaceSearch" 


-- | <p>Starts asynchronous detection of labels in a stored video.</p> <p>Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartLabelDetection</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p/>
startLabelDetection :: forall eff. StartLabelDetectionRequest -> Aff (exception :: EXCEPTION | eff) StartLabelDetectionResponse
startLabelDetection = Request.request serviceName "startLabelDetection" 


-- | <p>Starts the asynchronous tracking of persons in a stored video.</p> <p>Rekognition Video can track persons in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartPersonTracking</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. </p> <p>To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p>
startPersonTracking :: forall eff. StartPersonTrackingRequest -> Aff (exception :: EXCEPTION | eff) StartPersonTrackingResponse
startPersonTracking = Request.request serviceName "startPersonTracking" 


-- | <p>Starts processing a stream processor. You create a stream processor by calling . To tell <code>StartStreamProcessor</code> which stream processor to start, use the value of the <code>Name</code> field specified in the call to <code>CreateStreamProcessor</code>.</p>
startStreamProcessor :: forall eff. StartStreamProcessorRequest -> Aff (exception :: EXCEPTION | eff) StartStreamProcessorResponse
startStreamProcessor = Request.request serviceName "startStreamProcessor" 


-- | <p>Stops a running stream processor that was created by .</p>
stopStreamProcessor :: forall eff. StopStreamProcessorRequest -> Aff (exception :: EXCEPTION | eff) StopStreamProcessorResponse
stopStreamProcessor = Request.request serviceName "stopStreamProcessor" 


-- | <p>You are not authorized to perform the action.</p>
newtype AccessDeniedException = AccessDeniedException Types.NoArguments
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure containing the estimated age range, in years, for a face.</p> <p>Rekognition estimates an age-range for faces detected in the input image. Estimated age ranges can overlap; a face of a 5 year old may have an estimated range of 4-6 whilst the face of a 6 year old may have an estimated range of 4-8.</p>
newtype AgeRange = AgeRange 
  { "Low" :: NullOrUndefined.NullOrUndefined (UInteger)
  , "High" :: NullOrUndefined.NullOrUndefined (UInteger)
  }
derive instance newtypeAgeRange :: Newtype AgeRange _
derive instance repGenericAgeRange :: Generic AgeRange _
instance showAgeRange :: Show AgeRange where
  show = genericShow
instance decodeAgeRange :: Decode AgeRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgeRange :: Encode AgeRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attribute = Attribute String
derive instance newtypeAttribute :: Newtype Attribute _
derive instance repGenericAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where
  show = genericShow
instance decodeAttribute :: Decode Attribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttribute :: Encode Attribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the face has a beard, and the confidence level in the determination.</p>
newtype Beard = Beard 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeBeard :: Newtype Beard _
derive instance repGenericBeard :: Generic Beard _
instance showBeard :: Show Beard where
  show = genericShow
instance decodeBeard :: Decode Beard where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBeard :: Encode Beard where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the bounding box around the object, face or text. The <code>left</code> (x-coordinate) and <code>top</code> (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0). </p> <p>The <code>top</code> and <code>left</code> values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a <code>left</code> value of 0.5 (350/700) and a <code>top</code> value of 0.25 (50/200).</p> <p>The <code>width</code> and <code>height</code> values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1. </p> <note> <p> The bounding box coordinates can have negative values. For example, if Amazon Rekognition is able to detect a face that is at the image edge and is only partially visible, the service can return coordinates that are outside the image bounds and, depending on the image edge, you might get negative values or values greater than 1 for the <code>left</code> or <code>top</code> values. </p> </note>
newtype BoundingBox = BoundingBox 
  { "Width" :: NullOrUndefined.NullOrUndefined (Number)
  , "Height" :: NullOrUndefined.NullOrUndefined (Number)
  , "Left" :: NullOrUndefined.NullOrUndefined (Number)
  , "Top" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeBoundingBox :: Newtype BoundingBox _
derive instance repGenericBoundingBox :: Generic BoundingBox _
instance showBoundingBox :: Show BoundingBox where
  show = genericShow
instance decodeBoundingBox :: Decode BoundingBox where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoundingBox :: Encode BoundingBox where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a celebrity recognized by the operation.</p>
newtype Celebrity = Celebrity 
  { "Urls" :: NullOrUndefined.NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (RekognitionUniqueId)
  , "Face" :: NullOrUndefined.NullOrUndefined (ComparedFace)
  , "MatchConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeCelebrity :: Newtype Celebrity _
derive instance repGenericCelebrity :: Generic Celebrity _
instance showCelebrity :: Show Celebrity where
  show = genericShow
instance decodeCelebrity :: Decode Celebrity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrity :: Encode Celebrity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a recognized celebrity.</p>
newtype CelebrityDetail = CelebrityDetail 
  { "Urls" :: NullOrUndefined.NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (RekognitionUniqueId)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined.NullOrUndefined (FaceDetail)
  }
derive instance newtypeCelebrityDetail :: Newtype CelebrityDetail _
derive instance repGenericCelebrityDetail :: Generic CelebrityDetail _
instance showCelebrityDetail :: Show CelebrityDetail where
  show = genericShow
instance decodeCelebrityDetail :: Decode CelebrityDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrityDetail :: Encode CelebrityDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CelebrityList = CelebrityList (Array Celebrity)
derive instance newtypeCelebrityList :: Newtype CelebrityList _
derive instance repGenericCelebrityList :: Generic CelebrityList _
instance showCelebrityList :: Show CelebrityList where
  show = genericShow
instance decodeCelebrityList :: Decode CelebrityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrityList :: Encode CelebrityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see .</p>
newtype CelebrityRecognition = CelebrityRecognition 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Celebrity" :: NullOrUndefined.NullOrUndefined (CelebrityDetail)
  }
derive instance newtypeCelebrityRecognition :: Newtype CelebrityRecognition _
derive instance repGenericCelebrityRecognition :: Generic CelebrityRecognition _
instance showCelebrityRecognition :: Show CelebrityRecognition where
  show = genericShow
instance decodeCelebrityRecognition :: Decode CelebrityRecognition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrityRecognition :: Encode CelebrityRecognition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CelebrityRecognitionSortBy = CelebrityRecognitionSortBy String
derive instance newtypeCelebrityRecognitionSortBy :: Newtype CelebrityRecognitionSortBy _
derive instance repGenericCelebrityRecognitionSortBy :: Generic CelebrityRecognitionSortBy _
instance showCelebrityRecognitionSortBy :: Show CelebrityRecognitionSortBy where
  show = genericShow
instance decodeCelebrityRecognitionSortBy :: Decode CelebrityRecognitionSortBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrityRecognitionSortBy :: Encode CelebrityRecognitionSortBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CelebrityRecognitions = CelebrityRecognitions (Array CelebrityRecognition)
derive instance newtypeCelebrityRecognitions :: Newtype CelebrityRecognitions _
derive instance repGenericCelebrityRecognitions :: Generic CelebrityRecognitions _
instance showCelebrityRecognitions :: Show CelebrityRecognitions where
  show = genericShow
instance decodeCelebrityRecognitions :: Decode CelebrityRecognitions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCelebrityRecognitions :: Encode CelebrityRecognitions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _
derive instance repGenericClientRequestToken :: Generic ClientRequestToken _
instance showClientRequestToken :: Show ClientRequestToken where
  show = genericShow
instance decodeClientRequestToken :: Decode ClientRequestToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientRequestToken :: Encode ClientRequestToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CollectionId = CollectionId String
derive instance newtypeCollectionId :: Newtype CollectionId _
derive instance repGenericCollectionId :: Generic CollectionId _
instance showCollectionId :: Show CollectionId where
  show = genericShow
instance decodeCollectionId :: Decode CollectionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCollectionId :: Encode CollectionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CollectionIdList = CollectionIdList (Array CollectionId)
derive instance newtypeCollectionIdList :: Newtype CollectionIdList _
derive instance repGenericCollectionIdList :: Generic CollectionIdList _
instance showCollectionIdList :: Show CollectionIdList where
  show = genericShow
instance decodeCollectionIdList :: Decode CollectionIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCollectionIdList :: Encode CollectionIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a face in a target image that matches the source image face analysed by <code>CompareFaces</code>. The <code>Face</code> property contains the bounding box of the face in the target image. The <code>Similarity</code> property is the confidence that the source image face matches the face in the bounding box.</p>
newtype CompareFacesMatch = CompareFacesMatch 
  { "Similarity" :: NullOrUndefined.NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined.NullOrUndefined (ComparedFace)
  }
derive instance newtypeCompareFacesMatch :: Newtype CompareFacesMatch _
derive instance repGenericCompareFacesMatch :: Generic CompareFacesMatch _
instance showCompareFacesMatch :: Show CompareFacesMatch where
  show = genericShow
instance decodeCompareFacesMatch :: Decode CompareFacesMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompareFacesMatch :: Encode CompareFacesMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompareFacesMatchList = CompareFacesMatchList (Array CompareFacesMatch)
derive instance newtypeCompareFacesMatchList :: Newtype CompareFacesMatchList _
derive instance repGenericCompareFacesMatchList :: Generic CompareFacesMatchList _
instance showCompareFacesMatchList :: Show CompareFacesMatchList where
  show = genericShow
instance decodeCompareFacesMatchList :: Decode CompareFacesMatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompareFacesMatchList :: Encode CompareFacesMatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompareFacesRequest = CompareFacesRequest 
  { "SourceImage" :: (Image)
  , "TargetImage" :: (Image)
  , "SimilarityThreshold" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeCompareFacesRequest :: Newtype CompareFacesRequest _
derive instance repGenericCompareFacesRequest :: Generic CompareFacesRequest _
instance showCompareFacesRequest :: Show CompareFacesRequest where
  show = genericShow
instance decodeCompareFacesRequest :: Decode CompareFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompareFacesRequest :: Encode CompareFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompareFacesResponse = CompareFacesResponse 
  { "SourceImageFace" :: NullOrUndefined.NullOrUndefined (ComparedSourceImageFace)
  , "FaceMatches" :: NullOrUndefined.NullOrUndefined (CompareFacesMatchList)
  , "UnmatchedFaces" :: NullOrUndefined.NullOrUndefined (CompareFacesUnmatchList)
  , "SourceImageOrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  , "TargetImageOrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeCompareFacesResponse :: Newtype CompareFacesResponse _
derive instance repGenericCompareFacesResponse :: Generic CompareFacesResponse _
instance showCompareFacesResponse :: Show CompareFacesResponse where
  show = genericShow
instance decodeCompareFacesResponse :: Decode CompareFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompareFacesResponse :: Encode CompareFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompareFacesUnmatchList = CompareFacesUnmatchList (Array ComparedFace)
derive instance newtypeCompareFacesUnmatchList :: Newtype CompareFacesUnmatchList _
derive instance repGenericCompareFacesUnmatchList :: Generic CompareFacesUnmatchList _
instance showCompareFacesUnmatchList :: Show CompareFacesUnmatchList where
  show = genericShow
instance decodeCompareFacesUnmatchList :: Decode CompareFacesUnmatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompareFacesUnmatchList :: Encode CompareFacesUnmatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides face metadata for target image faces that are analysed by <code>CompareFaces</code> and <code>RecognizeCelebrities</code>.</p>
newtype ComparedFace = ComparedFace 
  { "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "Landmarks" :: NullOrUndefined.NullOrUndefined (Landmarks)
  , "Pose" :: NullOrUndefined.NullOrUndefined (Pose)
  , "Quality" :: NullOrUndefined.NullOrUndefined (ImageQuality)
  }
derive instance newtypeComparedFace :: Newtype ComparedFace _
derive instance repGenericComparedFace :: Generic ComparedFace _
instance showComparedFace :: Show ComparedFace where
  show = genericShow
instance decodeComparedFace :: Decode ComparedFace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparedFace :: Encode ComparedFace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComparedFaceList = ComparedFaceList (Array ComparedFace)
derive instance newtypeComparedFaceList :: Newtype ComparedFaceList _
derive instance repGenericComparedFaceList :: Generic ComparedFaceList _
instance showComparedFaceList :: Show ComparedFaceList where
  show = genericShow
instance decodeComparedFaceList :: Decode ComparedFaceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparedFaceList :: Encode ComparedFaceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison. </p>
newtype ComparedSourceImageFace = ComparedSourceImageFace 
  { "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeComparedSourceImageFace :: Newtype ComparedSourceImageFace _
derive instance repGenericComparedSourceImageFace :: Generic ComparedSourceImageFace _
instance showComparedSourceImageFace :: Show ComparedSourceImageFace where
  show = genericShow
instance decodeComparedSourceImageFace :: Decode ComparedSourceImageFace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparedSourceImageFace :: Encode ComparedSourceImageFace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a moderation label detection in a stored video.</p>
newtype ContentModerationDetection = ContentModerationDetection 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "ModerationLabel" :: NullOrUndefined.NullOrUndefined (ModerationLabel)
  }
derive instance newtypeContentModerationDetection :: Newtype ContentModerationDetection _
derive instance repGenericContentModerationDetection :: Generic ContentModerationDetection _
instance showContentModerationDetection :: Show ContentModerationDetection where
  show = genericShow
instance decodeContentModerationDetection :: Decode ContentModerationDetection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentModerationDetection :: Encode ContentModerationDetection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentModerationDetections = ContentModerationDetections (Array ContentModerationDetection)
derive instance newtypeContentModerationDetections :: Newtype ContentModerationDetections _
derive instance repGenericContentModerationDetections :: Generic ContentModerationDetections _
instance showContentModerationDetections :: Show ContentModerationDetections where
  show = genericShow
instance decodeContentModerationDetections :: Decode ContentModerationDetections where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentModerationDetections :: Encode ContentModerationDetections where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentModerationSortBy = ContentModerationSortBy String
derive instance newtypeContentModerationSortBy :: Newtype ContentModerationSortBy _
derive instance repGenericContentModerationSortBy :: Generic ContentModerationSortBy _
instance showContentModerationSortBy :: Show ContentModerationSortBy where
  show = genericShow
instance decodeContentModerationSortBy :: Decode ContentModerationSortBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentModerationSortBy :: Encode ContentModerationSortBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCollectionRequest = CreateCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }
derive instance newtypeCreateCollectionRequest :: Newtype CreateCollectionRequest _
derive instance repGenericCreateCollectionRequest :: Generic CreateCollectionRequest _
instance showCreateCollectionRequest :: Show CreateCollectionRequest where
  show = genericShow
instance decodeCreateCollectionRequest :: Decode CreateCollectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCollectionRequest :: Encode CreateCollectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCollectionResponse = CreateCollectionResponse 
  { "StatusCode" :: NullOrUndefined.NullOrUndefined (UInteger)
  , "CollectionArn" :: NullOrUndefined.NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateCollectionResponse :: Newtype CreateCollectionResponse _
derive instance repGenericCreateCollectionResponse :: Generic CreateCollectionResponse _
instance showCreateCollectionResponse :: Show CreateCollectionResponse where
  show = genericShow
instance decodeCreateCollectionResponse :: Decode CreateCollectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCollectionResponse :: Encode CreateCollectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStreamProcessorRequest = CreateStreamProcessorRequest 
  { "Input" :: (StreamProcessorInput)
  , "Output" :: (StreamProcessorOutput)
  , "Name" :: (StreamProcessorName)
  , "Settings" :: (StreamProcessorSettings)
  , "RoleArn" :: (RoleArn)
  }
derive instance newtypeCreateStreamProcessorRequest :: Newtype CreateStreamProcessorRequest _
derive instance repGenericCreateStreamProcessorRequest :: Generic CreateStreamProcessorRequest _
instance showCreateStreamProcessorRequest :: Show CreateStreamProcessorRequest where
  show = genericShow
instance decodeCreateStreamProcessorRequest :: Decode CreateStreamProcessorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamProcessorRequest :: Encode CreateStreamProcessorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStreamProcessorResponse = CreateStreamProcessorResponse 
  { "StreamProcessorArn" :: NullOrUndefined.NullOrUndefined (StreamProcessorArn)
  }
derive instance newtypeCreateStreamProcessorResponse :: Newtype CreateStreamProcessorResponse _
derive instance repGenericCreateStreamProcessorResponse :: Generic CreateStreamProcessorResponse _
instance showCreateStreamProcessorResponse :: Show CreateStreamProcessorResponse where
  show = genericShow
instance decodeCreateStreamProcessorResponse :: Decode CreateStreamProcessorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStreamProcessorResponse :: Encode CreateStreamProcessorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _
derive instance repGenericDateTime :: Generic DateTime _
instance showDateTime :: Show DateTime where
  show = genericShow
instance decodeDateTime :: Decode DateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTime :: Encode DateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Degree = Degree Number
derive instance newtypeDegree :: Newtype Degree _
derive instance repGenericDegree :: Generic Degree _
instance showDegree :: Show Degree where
  show = genericShow
instance decodeDegree :: Decode Degree where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDegree :: Encode Degree where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCollectionRequest = DeleteCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }
derive instance newtypeDeleteCollectionRequest :: Newtype DeleteCollectionRequest _
derive instance repGenericDeleteCollectionRequest :: Generic DeleteCollectionRequest _
instance showDeleteCollectionRequest :: Show DeleteCollectionRequest where
  show = genericShow
instance decodeDeleteCollectionRequest :: Decode DeleteCollectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCollectionRequest :: Encode DeleteCollectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCollectionResponse = DeleteCollectionResponse 
  { "StatusCode" :: NullOrUndefined.NullOrUndefined (UInteger)
  }
derive instance newtypeDeleteCollectionResponse :: Newtype DeleteCollectionResponse _
derive instance repGenericDeleteCollectionResponse :: Generic DeleteCollectionResponse _
instance showDeleteCollectionResponse :: Show DeleteCollectionResponse where
  show = genericShow
instance decodeDeleteCollectionResponse :: Decode DeleteCollectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCollectionResponse :: Encode DeleteCollectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFacesRequest = DeleteFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceIds" :: (FaceIdList)
  }
derive instance newtypeDeleteFacesRequest :: Newtype DeleteFacesRequest _
derive instance repGenericDeleteFacesRequest :: Generic DeleteFacesRequest _
instance showDeleteFacesRequest :: Show DeleteFacesRequest where
  show = genericShow
instance decodeDeleteFacesRequest :: Decode DeleteFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFacesRequest :: Encode DeleteFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFacesResponse = DeleteFacesResponse 
  { "DeletedFaces" :: NullOrUndefined.NullOrUndefined (FaceIdList)
  }
derive instance newtypeDeleteFacesResponse :: Newtype DeleteFacesResponse _
derive instance repGenericDeleteFacesResponse :: Generic DeleteFacesResponse _
instance showDeleteFacesResponse :: Show DeleteFacesResponse where
  show = genericShow
instance decodeDeleteFacesResponse :: Decode DeleteFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFacesResponse :: Encode DeleteFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStreamProcessorRequest = DeleteStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeDeleteStreamProcessorRequest :: Newtype DeleteStreamProcessorRequest _
derive instance repGenericDeleteStreamProcessorRequest :: Generic DeleteStreamProcessorRequest _
instance showDeleteStreamProcessorRequest :: Show DeleteStreamProcessorRequest where
  show = genericShow
instance decodeDeleteStreamProcessorRequest :: Decode DeleteStreamProcessorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStreamProcessorRequest :: Encode DeleteStreamProcessorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse Types.NoArguments
derive instance newtypeDeleteStreamProcessorResponse :: Newtype DeleteStreamProcessorResponse _
derive instance repGenericDeleteStreamProcessorResponse :: Generic DeleteStreamProcessorResponse _
instance showDeleteStreamProcessorResponse :: Show DeleteStreamProcessorResponse where
  show = genericShow
instance decodeDeleteStreamProcessorResponse :: Decode DeleteStreamProcessorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStreamProcessorResponse :: Encode DeleteStreamProcessorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStreamProcessorRequest = DescribeStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeDescribeStreamProcessorRequest :: Newtype DescribeStreamProcessorRequest _
derive instance repGenericDescribeStreamProcessorRequest :: Generic DescribeStreamProcessorRequest _
instance showDescribeStreamProcessorRequest :: Show DescribeStreamProcessorRequest where
  show = genericShow
instance decodeDescribeStreamProcessorRequest :: Decode DescribeStreamProcessorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamProcessorRequest :: Encode DescribeStreamProcessorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStreamProcessorResponse = DescribeStreamProcessorResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (StreamProcessorName)
  , "StreamProcessorArn" :: NullOrUndefined.NullOrUndefined (StreamProcessorArn)
  , "Status" :: NullOrUndefined.NullOrUndefined (StreamProcessorStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastUpdateTimestamp" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Input" :: NullOrUndefined.NullOrUndefined (StreamProcessorInput)
  , "Output" :: NullOrUndefined.NullOrUndefined (StreamProcessorOutput)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Settings" :: NullOrUndefined.NullOrUndefined (StreamProcessorSettings)
  }
derive instance newtypeDescribeStreamProcessorResponse :: Newtype DescribeStreamProcessorResponse _
derive instance repGenericDescribeStreamProcessorResponse :: Generic DescribeStreamProcessorResponse _
instance showDescribeStreamProcessorResponse :: Show DescribeStreamProcessorResponse where
  show = genericShow
instance decodeDescribeStreamProcessorResponse :: Decode DescribeStreamProcessorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamProcessorResponse :: Encode DescribeStreamProcessorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectFacesRequest = DetectFacesRequest 
  { "Image" :: (Image)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeDetectFacesRequest :: Newtype DetectFacesRequest _
derive instance repGenericDetectFacesRequest :: Generic DetectFacesRequest _
instance showDetectFacesRequest :: Show DetectFacesRequest where
  show = genericShow
instance decodeDetectFacesRequest :: Decode DetectFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectFacesRequest :: Encode DetectFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectFacesResponse = DetectFacesResponse 
  { "FaceDetails" :: NullOrUndefined.NullOrUndefined (FaceDetailList)
  , "OrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeDetectFacesResponse :: Newtype DetectFacesResponse _
derive instance repGenericDetectFacesResponse :: Generic DetectFacesResponse _
instance showDetectFacesResponse :: Show DetectFacesResponse where
  show = genericShow
instance decodeDetectFacesResponse :: Decode DetectFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectFacesResponse :: Encode DetectFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectLabelsRequest = DetectLabelsRequest 
  { "Image" :: (Image)
  , "MaxLabels" :: NullOrUndefined.NullOrUndefined (UInteger)
  , "MinConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeDetectLabelsRequest :: Newtype DetectLabelsRequest _
derive instance repGenericDetectLabelsRequest :: Generic DetectLabelsRequest _
instance showDetectLabelsRequest :: Show DetectLabelsRequest where
  show = genericShow
instance decodeDetectLabelsRequest :: Decode DetectLabelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectLabelsRequest :: Encode DetectLabelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectLabelsResponse = DetectLabelsResponse 
  { "Labels" :: NullOrUndefined.NullOrUndefined (Labels)
  , "OrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeDetectLabelsResponse :: Newtype DetectLabelsResponse _
derive instance repGenericDetectLabelsResponse :: Generic DetectLabelsResponse _
instance showDetectLabelsResponse :: Show DetectLabelsResponse where
  show = genericShow
instance decodeDetectLabelsResponse :: Decode DetectLabelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectLabelsResponse :: Encode DetectLabelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectModerationLabelsRequest = DetectModerationLabelsRequest 
  { "Image" :: (Image)
  , "MinConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeDetectModerationLabelsRequest :: Newtype DetectModerationLabelsRequest _
derive instance repGenericDetectModerationLabelsRequest :: Generic DetectModerationLabelsRequest _
instance showDetectModerationLabelsRequest :: Show DetectModerationLabelsRequest where
  show = genericShow
instance decodeDetectModerationLabelsRequest :: Decode DetectModerationLabelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectModerationLabelsRequest :: Encode DetectModerationLabelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectModerationLabelsResponse = DetectModerationLabelsResponse 
  { "ModerationLabels" :: NullOrUndefined.NullOrUndefined (ModerationLabels)
  }
derive instance newtypeDetectModerationLabelsResponse :: Newtype DetectModerationLabelsResponse _
derive instance repGenericDetectModerationLabelsResponse :: Generic DetectModerationLabelsResponse _
instance showDetectModerationLabelsResponse :: Show DetectModerationLabelsResponse where
  show = genericShow
instance decodeDetectModerationLabelsResponse :: Decode DetectModerationLabelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectModerationLabelsResponse :: Encode DetectModerationLabelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectTextRequest = DetectTextRequest 
  { "Image" :: (Image)
  }
derive instance newtypeDetectTextRequest :: Newtype DetectTextRequest _
derive instance repGenericDetectTextRequest :: Generic DetectTextRequest _
instance showDetectTextRequest :: Show DetectTextRequest where
  show = genericShow
instance decodeDetectTextRequest :: Decode DetectTextRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectTextRequest :: Encode DetectTextRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetectTextResponse = DetectTextResponse 
  { "TextDetections" :: NullOrUndefined.NullOrUndefined (TextDetectionList)
  }
derive instance newtypeDetectTextResponse :: Newtype DetectTextResponse _
derive instance repGenericDetectTextResponse :: Generic DetectTextResponse _
instance showDetectTextResponse :: Show DetectTextResponse where
  show = genericShow
instance decodeDetectTextResponse :: Decode DetectTextResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectTextResponse :: Encode DetectTextResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.</p>
newtype Emotion = Emotion 
  { "Type" :: NullOrUndefined.NullOrUndefined (EmotionName)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeEmotion :: Newtype Emotion _
derive instance repGenericEmotion :: Generic Emotion _
instance showEmotion :: Show Emotion where
  show = genericShow
instance decodeEmotion :: Decode Emotion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmotion :: Encode Emotion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmotionName = EmotionName String
derive instance newtypeEmotionName :: Newtype EmotionName _
derive instance repGenericEmotionName :: Generic EmotionName _
instance showEmotionName :: Show EmotionName where
  show = genericShow
instance decodeEmotionName :: Decode EmotionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmotionName :: Encode EmotionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Emotions = Emotions (Array Emotion)
derive instance newtypeEmotions :: Newtype Emotions _
derive instance repGenericEmotions :: Generic Emotions _
instance showEmotions :: Show Emotions where
  show = genericShow
instance decodeEmotions :: Decode Emotions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmotions :: Encode Emotions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExternalImageId = ExternalImageId String
derive instance newtypeExternalImageId :: Newtype ExternalImageId _
derive instance repGenericExternalImageId :: Generic ExternalImageId _
instance showExternalImageId :: Show ExternalImageId where
  show = genericShow
instance decodeExternalImageId :: Decode ExternalImageId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalImageId :: Encode ExternalImageId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the eyes on the face are open, and the confidence level in the determination.</p>
newtype EyeOpen = EyeOpen 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeEyeOpen :: Newtype EyeOpen _
derive instance repGenericEyeOpen :: Generic EyeOpen _
instance showEyeOpen :: Show EyeOpen where
  show = genericShow
instance decodeEyeOpen :: Decode EyeOpen where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEyeOpen :: Encode EyeOpen where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.</p>
newtype Eyeglasses = Eyeglasses 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeEyeglasses :: Newtype Eyeglasses _
derive instance repGenericEyeglasses :: Generic Eyeglasses _
instance showEyeglasses :: Show Eyeglasses where
  show = genericShow
instance decodeEyeglasses :: Decode Eyeglasses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEyeglasses :: Encode Eyeglasses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned. </p>
newtype Face = Face 
  { "FaceId" :: NullOrUndefined.NullOrUndefined (FaceId)
  , "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "ImageId" :: NullOrUndefined.NullOrUndefined (ImageId)
  , "ExternalImageId" :: NullOrUndefined.NullOrUndefined (ExternalImageId)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeFace :: Newtype Face _
derive instance repGenericFace :: Generic Face _
instance showFace :: Show Face where
  show = genericShow
instance decodeFace :: Decode Face where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFace :: Encode Face where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceAttributes = FaceAttributes String
derive instance newtypeFaceAttributes :: Newtype FaceAttributes _
derive instance repGenericFaceAttributes :: Generic FaceAttributes _
instance showFaceAttributes :: Show FaceAttributes where
  show = genericShow
instance decodeFaceAttributes :: Decode FaceAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceAttributes :: Encode FaceAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure containing attributes of the face that the algorithm detected.</p>
newtype FaceDetail = FaceDetail 
  { "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "AgeRange" :: NullOrUndefined.NullOrUndefined (AgeRange)
  , "Smile" :: NullOrUndefined.NullOrUndefined (Smile)
  , "Eyeglasses" :: NullOrUndefined.NullOrUndefined (Eyeglasses)
  , "Sunglasses" :: NullOrUndefined.NullOrUndefined (Sunglasses)
  , "Gender" :: NullOrUndefined.NullOrUndefined (Gender)
  , "Beard" :: NullOrUndefined.NullOrUndefined (Beard)
  , "Mustache" :: NullOrUndefined.NullOrUndefined (Mustache)
  , "EyesOpen" :: NullOrUndefined.NullOrUndefined (EyeOpen)
  , "MouthOpen" :: NullOrUndefined.NullOrUndefined (MouthOpen)
  , "Emotions" :: NullOrUndefined.NullOrUndefined (Emotions)
  , "Landmarks" :: NullOrUndefined.NullOrUndefined (Landmarks)
  , "Pose" :: NullOrUndefined.NullOrUndefined (Pose)
  , "Quality" :: NullOrUndefined.NullOrUndefined (ImageQuality)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeFaceDetail :: Newtype FaceDetail _
derive instance repGenericFaceDetail :: Generic FaceDetail _
instance showFaceDetail :: Show FaceDetail where
  show = genericShow
instance decodeFaceDetail :: Decode FaceDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceDetail :: Encode FaceDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceDetailList = FaceDetailList (Array FaceDetail)
derive instance newtypeFaceDetailList :: Newtype FaceDetailList _
derive instance repGenericFaceDetailList :: Generic FaceDetailList _
instance showFaceDetailList :: Show FaceDetailList where
  show = genericShow
instance decodeFaceDetailList :: Decode FaceDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceDetailList :: Encode FaceDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a face detected in a video analysis request and the time the face was detected in the video. </p>
newtype FaceDetection = FaceDetection 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Face" :: NullOrUndefined.NullOrUndefined (FaceDetail)
  }
derive instance newtypeFaceDetection :: Newtype FaceDetection _
derive instance repGenericFaceDetection :: Generic FaceDetection _
instance showFaceDetection :: Show FaceDetection where
  show = genericShow
instance decodeFaceDetection :: Decode FaceDetection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceDetection :: Encode FaceDetection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceDetections = FaceDetections (Array FaceDetection)
derive instance newtypeFaceDetections :: Newtype FaceDetections _
derive instance repGenericFaceDetections :: Generic FaceDetections _
instance showFaceDetections :: Show FaceDetections where
  show = genericShow
instance decodeFaceDetections :: Decode FaceDetections where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceDetections :: Encode FaceDetections where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceId = FaceId String
derive instance newtypeFaceId :: Newtype FaceId _
derive instance repGenericFaceId :: Generic FaceId _
instance showFaceId :: Show FaceId where
  show = genericShow
instance decodeFaceId :: Decode FaceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceId :: Encode FaceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceIdList = FaceIdList (Array FaceId)
derive instance newtypeFaceIdList :: Newtype FaceIdList _
derive instance repGenericFaceIdList :: Generic FaceIdList _
instance showFaceIdList :: Show FaceIdList where
  show = genericShow
instance decodeFaceIdList :: Decode FaceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceIdList :: Encode FaceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceList = FaceList (Array Face)
derive instance newtypeFaceList :: Newtype FaceList _
derive instance repGenericFaceList :: Generic FaceList _
instance showFaceList :: Show FaceList where
  show = genericShow
instance decodeFaceList :: Decode FaceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceList :: Encode FaceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.</p>
newtype FaceMatch = FaceMatch 
  { "Similarity" :: NullOrUndefined.NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined.NullOrUndefined (Face)
  }
derive instance newtypeFaceMatch :: Newtype FaceMatch _
derive instance repGenericFaceMatch :: Generic FaceMatch _
instance showFaceMatch :: Show FaceMatch where
  show = genericShow
instance decodeFaceMatch :: Decode FaceMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceMatch :: Encode FaceMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceMatchList = FaceMatchList (Array FaceMatch)
derive instance newtypeFaceMatchList :: Newtype FaceMatchList _
derive instance repGenericFaceMatchList :: Generic FaceMatchList _
instance showFaceMatchList :: Show FaceMatchList where
  show = genericShow
instance decodeFaceMatchList :: Decode FaceMatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceMatchList :: Encode FaceMatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceModelVersionList = FaceModelVersionList (Array String)
derive instance newtypeFaceModelVersionList :: Newtype FaceModelVersionList _
derive instance repGenericFaceModelVersionList :: Generic FaceModelVersionList _
instance showFaceModelVersionList :: Show FaceModelVersionList where
  show = genericShow
instance decodeFaceModelVersionList :: Decode FaceModelVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceModelVersionList :: Encode FaceModelVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Object containing both the face metadata (stored in the back-end database) and facial attributes that are detected but aren't stored in the database.</p>
newtype FaceRecord = FaceRecord 
  { "Face" :: NullOrUndefined.NullOrUndefined (Face)
  , "FaceDetail" :: NullOrUndefined.NullOrUndefined (FaceDetail)
  }
derive instance newtypeFaceRecord :: Newtype FaceRecord _
derive instance repGenericFaceRecord :: Generic FaceRecord _
instance showFaceRecord :: Show FaceRecord where
  show = genericShow
instance decodeFaceRecord :: Decode FaceRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceRecord :: Encode FaceRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceRecordList = FaceRecordList (Array FaceRecord)
derive instance newtypeFaceRecordList :: Newtype FaceRecordList _
derive instance repGenericFaceRecordList :: Generic FaceRecordList _
instance showFaceRecordList :: Show FaceRecordList where
  show = genericShow
instance decodeFaceRecordList :: Decode FaceRecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceRecordList :: Encode FaceRecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input face recognition parameters for an Amazon Rekognition stream processor. <code>FaceRecognitionSettings</code> is a request parameter for .</p>
newtype FaceSearchSettings = FaceSearchSettings 
  { "CollectionId" :: NullOrUndefined.NullOrUndefined (CollectionId)
  , "FaceMatchThreshold" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeFaceSearchSettings :: Newtype FaceSearchSettings _
derive instance repGenericFaceSearchSettings :: Generic FaceSearchSettings _
instance showFaceSearchSettings :: Show FaceSearchSettings where
  show = genericShow
instance decodeFaceSearchSettings :: Decode FaceSearchSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceSearchSettings :: Encode FaceSearchSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaceSearchSortBy = FaceSearchSortBy String
derive instance newtypeFaceSearchSortBy :: Newtype FaceSearchSortBy _
derive instance repGenericFaceSearchSortBy :: Generic FaceSearchSortBy _
instance showFaceSearchSortBy :: Show FaceSearchSortBy where
  show = genericShow
instance decodeFaceSearchSortBy :: Decode FaceSearchSortBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaceSearchSortBy :: Encode FaceSearchSortBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gender of the face and the confidence level in the determination.</p>
newtype Gender = Gender 
  { "Value" :: NullOrUndefined.NullOrUndefined (GenderType)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeGender :: Newtype Gender _
derive instance repGenericGender :: Generic Gender _
instance showGender :: Show Gender where
  show = genericShow
instance decodeGender :: Decode Gender where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGender :: Encode Gender where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenderType = GenderType String
derive instance newtypeGenderType :: Newtype GenderType _
derive instance repGenericGenderType :: Generic GenderType _
instance showGenderType :: Show GenderType where
  show = genericShow
instance decodeGenderType :: Decode GenderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenderType :: Encode GenderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about where text detected by is located on an image.</p>
newtype Geometry = Geometry 
  { "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "Polygon" :: NullOrUndefined.NullOrUndefined (Polygon)
  }
derive instance newtypeGeometry :: Newtype Geometry _
derive instance repGenericGeometry :: Generic Geometry _
instance showGeometry :: Show Geometry where
  show = genericShow
instance decodeGeometry :: Decode Geometry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeometry :: Encode Geometry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCelebrityInfoRequest = GetCelebrityInfoRequest 
  { "Id" :: (RekognitionUniqueId)
  }
derive instance newtypeGetCelebrityInfoRequest :: Newtype GetCelebrityInfoRequest _
derive instance repGenericGetCelebrityInfoRequest :: Generic GetCelebrityInfoRequest _
instance showGetCelebrityInfoRequest :: Show GetCelebrityInfoRequest where
  show = genericShow
instance decodeGetCelebrityInfoRequest :: Decode GetCelebrityInfoRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCelebrityInfoRequest :: Encode GetCelebrityInfoRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCelebrityInfoResponse = GetCelebrityInfoResponse 
  { "Urls" :: NullOrUndefined.NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetCelebrityInfoResponse :: Newtype GetCelebrityInfoResponse _
derive instance repGenericGetCelebrityInfoResponse :: Generic GetCelebrityInfoResponse _
instance showGetCelebrityInfoResponse :: Show GetCelebrityInfoResponse where
  show = genericShow
instance decodeGetCelebrityInfoResponse :: Decode GetCelebrityInfoResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCelebrityInfoResponse :: Encode GetCelebrityInfoResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCelebrityRecognitionRequest = GetCelebrityRecognitionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined.NullOrUndefined (CelebrityRecognitionSortBy)
  }
derive instance newtypeGetCelebrityRecognitionRequest :: Newtype GetCelebrityRecognitionRequest _
derive instance repGenericGetCelebrityRecognitionRequest :: Generic GetCelebrityRecognitionRequest _
instance showGetCelebrityRecognitionRequest :: Show GetCelebrityRecognitionRequest where
  show = genericShow
instance decodeGetCelebrityRecognitionRequest :: Decode GetCelebrityRecognitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCelebrityRecognitionRequest :: Encode GetCelebrityRecognitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "Celebrities" :: NullOrUndefined.NullOrUndefined (CelebrityRecognitions)
  }
derive instance newtypeGetCelebrityRecognitionResponse :: Newtype GetCelebrityRecognitionResponse _
derive instance repGenericGetCelebrityRecognitionResponse :: Generic GetCelebrityRecognitionResponse _
instance showGetCelebrityRecognitionResponse :: Show GetCelebrityRecognitionResponse where
  show = genericShow
instance decodeGetCelebrityRecognitionResponse :: Decode GetCelebrityRecognitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCelebrityRecognitionResponse :: Encode GetCelebrityRecognitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetContentModerationRequest = GetContentModerationRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined.NullOrUndefined (ContentModerationSortBy)
  }
derive instance newtypeGetContentModerationRequest :: Newtype GetContentModerationRequest _
derive instance repGenericGetContentModerationRequest :: Generic GetContentModerationRequest _
instance showGetContentModerationRequest :: Show GetContentModerationRequest where
  show = genericShow
instance decodeGetContentModerationRequest :: Decode GetContentModerationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetContentModerationRequest :: Encode GetContentModerationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetContentModerationResponse = GetContentModerationResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "ModerationLabels" :: NullOrUndefined.NullOrUndefined (ContentModerationDetections)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetContentModerationResponse :: Newtype GetContentModerationResponse _
derive instance repGenericGetContentModerationResponse :: Generic GetContentModerationResponse _
instance showGetContentModerationResponse :: Show GetContentModerationResponse where
  show = genericShow
instance decodeGetContentModerationResponse :: Decode GetContentModerationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetContentModerationResponse :: Encode GetContentModerationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFaceDetectionRequest = GetFaceDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetFaceDetectionRequest :: Newtype GetFaceDetectionRequest _
derive instance repGenericGetFaceDetectionRequest :: Generic GetFaceDetectionRequest _
instance showGetFaceDetectionRequest :: Show GetFaceDetectionRequest where
  show = genericShow
instance decodeGetFaceDetectionRequest :: Decode GetFaceDetectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFaceDetectionRequest :: Encode GetFaceDetectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFaceDetectionResponse = GetFaceDetectionResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "Faces" :: NullOrUndefined.NullOrUndefined (FaceDetections)
  }
derive instance newtypeGetFaceDetectionResponse :: Newtype GetFaceDetectionResponse _
derive instance repGenericGetFaceDetectionResponse :: Generic GetFaceDetectionResponse _
instance showGetFaceDetectionResponse :: Show GetFaceDetectionResponse where
  show = genericShow
instance decodeGetFaceDetectionResponse :: Decode GetFaceDetectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFaceDetectionResponse :: Encode GetFaceDetectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFaceSearchRequest = GetFaceSearchRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined.NullOrUndefined (FaceSearchSortBy)
  }
derive instance newtypeGetFaceSearchRequest :: Newtype GetFaceSearchRequest _
derive instance repGenericGetFaceSearchRequest :: Generic GetFaceSearchRequest _
instance showGetFaceSearchRequest :: Show GetFaceSearchRequest where
  show = genericShow
instance decodeGetFaceSearchRequest :: Decode GetFaceSearchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFaceSearchRequest :: Encode GetFaceSearchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFaceSearchResponse = GetFaceSearchResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "Persons" :: NullOrUndefined.NullOrUndefined (PersonMatches)
  }
derive instance newtypeGetFaceSearchResponse :: Newtype GetFaceSearchResponse _
derive instance repGenericGetFaceSearchResponse :: Generic GetFaceSearchResponse _
instance showGetFaceSearchResponse :: Show GetFaceSearchResponse where
  show = genericShow
instance decodeGetFaceSearchResponse :: Decode GetFaceSearchResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFaceSearchResponse :: Encode GetFaceSearchResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLabelDetectionRequest = GetLabelDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined.NullOrUndefined (LabelDetectionSortBy)
  }
derive instance newtypeGetLabelDetectionRequest :: Newtype GetLabelDetectionRequest _
derive instance repGenericGetLabelDetectionRequest :: Generic GetLabelDetectionRequest _
instance showGetLabelDetectionRequest :: Show GetLabelDetectionRequest where
  show = genericShow
instance decodeGetLabelDetectionRequest :: Decode GetLabelDetectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLabelDetectionRequest :: Encode GetLabelDetectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLabelDetectionResponse = GetLabelDetectionResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "Labels" :: NullOrUndefined.NullOrUndefined (LabelDetections)
  }
derive instance newtypeGetLabelDetectionResponse :: Newtype GetLabelDetectionResponse _
derive instance repGenericGetLabelDetectionResponse :: Generic GetLabelDetectionResponse _
instance showGetLabelDetectionResponse :: Show GetLabelDetectionResponse where
  show = genericShow
instance decodeGetLabelDetectionResponse :: Decode GetLabelDetectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLabelDetectionResponse :: Encode GetLabelDetectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPersonTrackingRequest = GetPersonTrackingRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined.NullOrUndefined (PersonTrackingSortBy)
  }
derive instance newtypeGetPersonTrackingRequest :: Newtype GetPersonTrackingRequest _
derive instance repGenericGetPersonTrackingRequest :: Generic GetPersonTrackingRequest _
instance showGetPersonTrackingRequest :: Show GetPersonTrackingRequest where
  show = genericShow
instance decodeGetPersonTrackingRequest :: Decode GetPersonTrackingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPersonTrackingRequest :: Encode GetPersonTrackingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPersonTrackingResponse = GetPersonTrackingResponse 
  { "JobStatus" :: NullOrUndefined.NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined.NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "Persons" :: NullOrUndefined.NullOrUndefined (PersonDetections)
  }
derive instance newtypeGetPersonTrackingResponse :: Newtype GetPersonTrackingResponse _
derive instance repGenericGetPersonTrackingResponse :: Generic GetPersonTrackingResponse _
instance showGetPersonTrackingResponse :: Show GetPersonTrackingResponse where
  show = genericShow
instance decodeGetPersonTrackingResponse :: Decode GetPersonTrackingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPersonTrackingResponse :: Encode GetPersonTrackingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A <code>ClientRequestToken</code> input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException Types.NoArguments
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _
derive instance repGenericIdempotentParameterMismatchException :: Generic IdempotentParameterMismatchException _
instance showIdempotentParameterMismatchException :: Show IdempotentParameterMismatchException where
  show = genericShow
instance decodeIdempotentParameterMismatchException :: Decode IdempotentParameterMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotentParameterMismatchException :: Encode IdempotentParameterMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the input image either as bytes or an S3 object.</p> <p>You pass image bytes to a Rekognition API operation by using the <code>Bytes</code> property. For example, you would use the <code>Bytes</code> property to pass an image loaded from a local file system. Image bytes passed by using the <code>Bytes</code> property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Rekognition API operations. For more information, see <a>images-bytes</a>.</p> <p> You pass images stored in an S3 bucket to a Rekognition API operation by using the <code>S3Object</code> property. Images stored in an S3 bucket do not need to be base64-encoded.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype Image = Image 
  { "Bytes" :: NullOrUndefined.NullOrUndefined (ImageBlob)
  , "S3Object" :: NullOrUndefined.NullOrUndefined (S3Object)
  }
derive instance newtypeImage :: Newtype Image _
derive instance repGenericImage :: Generic Image _
instance showImage :: Show Image where
  show = genericShow
instance decodeImage :: Decode Image where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImage :: Encode Image where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageBlob = ImageBlob String
derive instance newtypeImageBlob :: Newtype ImageBlob _
derive instance repGenericImageBlob :: Generic ImageBlob _
instance showImageBlob :: Show ImageBlob where
  show = genericShow
instance decodeImageBlob :: Decode ImageBlob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageBlob :: Encode ImageBlob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageId = ImageId String
derive instance newtypeImageId :: Newtype ImageId _
derive instance repGenericImageId :: Generic ImageId _
instance showImageId :: Show ImageId where
  show = genericShow
instance decodeImageId :: Decode ImageId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageId :: Encode ImageId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies face image brightness and sharpness. </p>
newtype ImageQuality = ImageQuality 
  { "Brightness" :: NullOrUndefined.NullOrUndefined (Number)
  , "Sharpness" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeImageQuality :: Newtype ImageQuality _
derive instance repGenericImageQuality :: Generic ImageQuality _
instance showImageQuality :: Show ImageQuality where
  show = genericShow
instance decodeImageQuality :: Decode ImageQuality where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageQuality :: Encode ImageQuality where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input image size exceeds the allowed limit. For more information, see <a>limits</a>. </p>
newtype ImageTooLargeException = ImageTooLargeException Types.NoArguments
derive instance newtypeImageTooLargeException :: Newtype ImageTooLargeException _
derive instance repGenericImageTooLargeException :: Generic ImageTooLargeException _
instance showImageTooLargeException :: Show ImageTooLargeException where
  show = genericShow
instance decodeImageTooLargeException :: Decode ImageTooLargeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageTooLargeException :: Encode ImageTooLargeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexFacesRequest = IndexFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "ExternalImageId" :: NullOrUndefined.NullOrUndefined (ExternalImageId)
  , "DetectionAttributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeIndexFacesRequest :: Newtype IndexFacesRequest _
derive instance repGenericIndexFacesRequest :: Generic IndexFacesRequest _
instance showIndexFacesRequest :: Show IndexFacesRequest where
  show = genericShow
instance decodeIndexFacesRequest :: Decode IndexFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexFacesRequest :: Encode IndexFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IndexFacesResponse = IndexFacesResponse 
  { "FaceRecords" :: NullOrUndefined.NullOrUndefined (FaceRecordList)
  , "OrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  , "FaceModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIndexFacesResponse :: Newtype IndexFacesResponse _
derive instance repGenericIndexFacesResponse :: Generic IndexFacesResponse _
instance showIndexFacesResponse :: Show IndexFacesResponse where
  show = genericShow
instance decodeIndexFacesResponse :: Decode IndexFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIndexFacesResponse :: Encode IndexFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Rekognition experienced a service issue. Try your call again.</p>
newtype InternalServerError = InternalServerError Types.NoArguments
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided image format is not supported. </p>
newtype InvalidImageFormatException = InvalidImageFormatException Types.NoArguments
derive instance newtypeInvalidImageFormatException :: Newtype InvalidImageFormatException _
derive instance repGenericInvalidImageFormatException :: Generic InvalidImageFormatException _
instance showInvalidImageFormatException :: Show InvalidImageFormatException where
  show = genericShow
instance decodeInvalidImageFormatException :: Decode InvalidImageFormatException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidImageFormatException :: Encode InvalidImageFormatException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Pagination token in the request is not valid.</p>
newtype InvalidPaginationTokenException = InvalidPaginationTokenException Types.NoArguments
derive instance newtypeInvalidPaginationTokenException :: Newtype InvalidPaginationTokenException _
derive instance repGenericInvalidPaginationTokenException :: Generic InvalidPaginationTokenException _
instance showInvalidPaginationTokenException :: Show InvalidPaginationTokenException where
  show = genericShow
instance decodeInvalidPaginationTokenException :: Decode InvalidPaginationTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPaginationTokenException :: Encode InvalidPaginationTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input parameter violated a constraint. Validate your parameter before calling the API operation again.</p>
newtype InvalidParameterException = InvalidParameterException Types.NoArguments
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Rekognition is unable to access the S3 object specified in the request.</p>
newtype InvalidS3ObjectException = InvalidS3ObjectException Types.NoArguments
derive instance newtypeInvalidS3ObjectException :: Newtype InvalidS3ObjectException _
derive instance repGenericInvalidS3ObjectException :: Generic InvalidS3ObjectException _
instance showInvalidS3ObjectException :: Show InvalidS3ObjectException where
  show = genericShow
instance decodeInvalidS3ObjectException :: Decode InvalidS3ObjectException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3ObjectException :: Encode InvalidS3ObjectException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _
derive instance repGenericJobId :: Generic JobId _
instance showJobId :: Show JobId where
  show = genericShow
instance decodeJobId :: Decode JobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobId :: Encode JobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobTag = JobTag String
derive instance newtypeJobTag :: Newtype JobTag _
derive instance repGenericJobTag :: Generic JobTag _
instance showJobTag :: Show JobTag where
  show = genericShow
instance decodeJobTag :: Decode JobTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTag :: Encode JobTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KinesisDataArn = KinesisDataArn String
derive instance newtypeKinesisDataArn :: Newtype KinesisDataArn _
derive instance repGenericKinesisDataArn :: Generic KinesisDataArn _
instance showKinesisDataArn :: Show KinesisDataArn where
  show = genericShow
instance decodeKinesisDataArn :: Decode KinesisDataArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisDataArn :: Encode KinesisDataArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see .</p>
newtype KinesisDataStream = KinesisDataStream 
  { "Arn" :: NullOrUndefined.NullOrUndefined (KinesisDataArn)
  }
derive instance newtypeKinesisDataStream :: Newtype KinesisDataStream _
derive instance repGenericKinesisDataStream :: Generic KinesisDataStream _
instance showKinesisDataStream :: Show KinesisDataStream where
  show = genericShow
instance decodeKinesisDataStream :: Decode KinesisDataStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisDataStream :: Encode KinesisDataStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KinesisVideoArn = KinesisVideoArn String
derive instance newtypeKinesisVideoArn :: Newtype KinesisVideoArn _
derive instance repGenericKinesisVideoArn :: Generic KinesisVideoArn _
instance showKinesisVideoArn :: Show KinesisVideoArn where
  show = genericShow
instance decodeKinesisVideoArn :: Decode KinesisVideoArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisVideoArn :: Encode KinesisVideoArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Kinesis video stream stream that provides the source streaming video for a Rekognition Video stream processor. For more information, see .</p>
newtype KinesisVideoStream = KinesisVideoStream 
  { "Arn" :: NullOrUndefined.NullOrUndefined (KinesisVideoArn)
  }
derive instance newtypeKinesisVideoStream :: Newtype KinesisVideoStream _
derive instance repGenericKinesisVideoStream :: Generic KinesisVideoStream _
instance showKinesisVideoStream :: Show KinesisVideoStream where
  show = genericShow
instance decodeKinesisVideoStream :: Decode KinesisVideoStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisVideoStream :: Encode KinesisVideoStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure containing details about the detected label, including name, and level of confidence.</p>
newtype Label = Label 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeLabel :: Newtype Label _
derive instance repGenericLabel :: Generic Label _
instance showLabel :: Show Label where
  show = genericShow
instance decodeLabel :: Decode Label where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabel :: Encode Label where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a label detected in a video analysis request and the time the label was detected in the video. </p>
newtype LabelDetection = LabelDetection 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Label" :: NullOrUndefined.NullOrUndefined (Label)
  }
derive instance newtypeLabelDetection :: Newtype LabelDetection _
derive instance repGenericLabelDetection :: Generic LabelDetection _
instance showLabelDetection :: Show LabelDetection where
  show = genericShow
instance decodeLabelDetection :: Decode LabelDetection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabelDetection :: Encode LabelDetection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LabelDetectionSortBy = LabelDetectionSortBy String
derive instance newtypeLabelDetectionSortBy :: Newtype LabelDetectionSortBy _
derive instance repGenericLabelDetectionSortBy :: Generic LabelDetectionSortBy _
instance showLabelDetectionSortBy :: Show LabelDetectionSortBy where
  show = genericShow
instance decodeLabelDetectionSortBy :: Decode LabelDetectionSortBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabelDetectionSortBy :: Encode LabelDetectionSortBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LabelDetections = LabelDetections (Array LabelDetection)
derive instance newtypeLabelDetections :: Newtype LabelDetections _
derive instance repGenericLabelDetections :: Generic LabelDetections _
instance showLabelDetections :: Show LabelDetections where
  show = genericShow
instance decodeLabelDetections :: Decode LabelDetections where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabelDetections :: Encode LabelDetections where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Labels = Labels (Array Label)
derive instance newtypeLabels :: Newtype Labels _
derive instance repGenericLabels :: Generic Labels _
instance showLabels :: Show Labels where
  show = genericShow
instance decodeLabels :: Decode Labels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLabels :: Encode Labels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates the location of the landmark on the face.</p>
newtype Landmark = Landmark 
  { "Type" :: NullOrUndefined.NullOrUndefined (LandmarkType)
  , "X" :: NullOrUndefined.NullOrUndefined (Number)
  , "Y" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeLandmark :: Newtype Landmark _
derive instance repGenericLandmark :: Generic Landmark _
instance showLandmark :: Show Landmark where
  show = genericShow
instance decodeLandmark :: Decode Landmark where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLandmark :: Encode Landmark where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LandmarkType = LandmarkType String
derive instance newtypeLandmarkType :: Newtype LandmarkType _
derive instance repGenericLandmarkType :: Generic LandmarkType _
instance showLandmarkType :: Show LandmarkType where
  show = genericShow
instance decodeLandmarkType :: Decode LandmarkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLandmarkType :: Encode LandmarkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Landmarks = Landmarks (Array Landmark)
derive instance newtypeLandmarks :: Newtype Landmarks _
derive instance repGenericLandmarks :: Generic Landmarks _
instance showLandmarks :: Show Landmarks where
  show = genericShow
instance decodeLandmarks :: Decode Landmarks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLandmarks :: Encode Landmarks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCollectionsRequest = ListCollectionsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeListCollectionsRequest :: Newtype ListCollectionsRequest _
derive instance repGenericListCollectionsRequest :: Generic ListCollectionsRequest _
instance showListCollectionsRequest :: Show ListCollectionsRequest where
  show = genericShow
instance decodeListCollectionsRequest :: Decode ListCollectionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCollectionsRequest :: Encode ListCollectionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCollectionsResponse = ListCollectionsResponse 
  { "CollectionIds" :: NullOrUndefined.NullOrUndefined (CollectionIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "FaceModelVersions" :: NullOrUndefined.NullOrUndefined (FaceModelVersionList)
  }
derive instance newtypeListCollectionsResponse :: Newtype ListCollectionsResponse _
derive instance repGenericListCollectionsResponse :: Generic ListCollectionsResponse _
instance showListCollectionsResponse :: Show ListCollectionsResponse where
  show = genericShow
instance decodeListCollectionsResponse :: Decode ListCollectionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCollectionsResponse :: Encode ListCollectionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacesRequest = ListFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeListFacesRequest :: Newtype ListFacesRequest _
derive instance repGenericListFacesRequest :: Generic ListFacesRequest _
instance showListFacesRequest :: Show ListFacesRequest where
  show = genericShow
instance decodeListFacesRequest :: Decode ListFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacesRequest :: Encode ListFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFacesResponse = ListFacesResponse 
  { "Faces" :: NullOrUndefined.NullOrUndefined (FaceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListFacesResponse :: Newtype ListFacesResponse _
derive instance repGenericListFacesResponse :: Generic ListFacesResponse _
instance showListFacesResponse :: Show ListFacesResponse where
  show = genericShow
instance decodeListFacesResponse :: Decode ListFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFacesResponse :: Encode ListFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStreamProcessorsRequest = ListStreamProcessorsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListStreamProcessorsRequest :: Newtype ListStreamProcessorsRequest _
derive instance repGenericListStreamProcessorsRequest :: Generic ListStreamProcessorsRequest _
instance showListStreamProcessorsRequest :: Show ListStreamProcessorsRequest where
  show = genericShow
instance decodeListStreamProcessorsRequest :: Decode ListStreamProcessorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamProcessorsRequest :: Encode ListStreamProcessorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStreamProcessorsResponse = ListStreamProcessorsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "StreamProcessors" :: NullOrUndefined.NullOrUndefined (StreamProcessorList)
  }
derive instance newtypeListStreamProcessorsResponse :: Newtype ListStreamProcessorsResponse _
derive instance repGenericListStreamProcessorsResponse :: Generic ListStreamProcessorsResponse _
instance showListStreamProcessorsResponse :: Show ListStreamProcessorsResponse where
  show = genericShow
instance decodeListStreamProcessorsResponse :: Decode ListStreamProcessorsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamProcessorsResponse :: Encode ListStreamProcessorsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxFaces = MaxFaces Int
derive instance newtypeMaxFaces :: Newtype MaxFaces _
derive instance repGenericMaxFaces :: Generic MaxFaces _
instance showMaxFaces :: Show MaxFaces where
  show = genericShow
instance decodeMaxFaces :: Decode MaxFaces where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxFaces :: Encode MaxFaces where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about a single type of moderated content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see <a>moderation</a>.</p>
newtype ModerationLabel = ModerationLabel 
  { "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ParentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeModerationLabel :: Newtype ModerationLabel _
derive instance repGenericModerationLabel :: Generic ModerationLabel _
instance showModerationLabel :: Show ModerationLabel where
  show = genericShow
instance decodeModerationLabel :: Decode ModerationLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModerationLabel :: Encode ModerationLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModerationLabels = ModerationLabels (Array ModerationLabel)
derive instance newtypeModerationLabels :: Newtype ModerationLabels _
derive instance repGenericModerationLabels :: Generic ModerationLabels _
instance showModerationLabels :: Show ModerationLabels where
  show = genericShow
instance decodeModerationLabels :: Decode ModerationLabels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModerationLabels :: Encode ModerationLabels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the mouth on the face is open, and the confidence level in the determination.</p>
newtype MouthOpen = MouthOpen 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeMouthOpen :: Newtype MouthOpen _
derive instance repGenericMouthOpen :: Generic MouthOpen _
instance showMouthOpen :: Show MouthOpen where
  show = genericShow
instance decodeMouthOpen :: Decode MouthOpen where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMouthOpen :: Encode MouthOpen where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the face has a mustache, and the confidence level in the determination.</p>
newtype Mustache = Mustache 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeMustache :: Newtype Mustache _
derive instance repGenericMustache :: Generic Mustache _
instance showMustache :: Show Mustache where
  show = genericShow
instance decodeMustache :: Decode Mustache where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMustache :: Encode Mustache where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see <a>api-video</a>.</p>
newtype NotificationChannel = NotificationChannel 
  { "SNSTopicArn" :: (SNSTopicArn)
  , "RoleArn" :: (RoleArn)
  }
derive instance newtypeNotificationChannel :: Newtype NotificationChannel _
derive instance repGenericNotificationChannel :: Generic NotificationChannel _
instance showNotificationChannel :: Show NotificationChannel where
  show = genericShow
instance decodeNotificationChannel :: Decode NotificationChannel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationChannel :: Encode NotificationChannel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrientationCorrection = OrientationCorrection String
derive instance newtypeOrientationCorrection :: Newtype OrientationCorrection _
derive instance repGenericOrientationCorrection :: Generic OrientationCorrection _
instance showOrientationCorrection :: Show OrientationCorrection where
  show = genericShow
instance decodeOrientationCorrection :: Decode OrientationCorrection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrientationCorrection :: Encode OrientationCorrection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
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


newtype Percent = Percent Number
derive instance newtypePercent :: Newtype Percent _
derive instance repGenericPercent :: Generic Percent _
instance showPercent :: Show Percent where
  show = genericShow
instance decodePercent :: Decode Percent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePercent :: Encode Percent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details about a person detected in a video analysis request.</p>
newtype PersonDetail = PersonDetail 
  { "Index" :: NullOrUndefined.NullOrUndefined (PersonIndex)
  , "BoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined.NullOrUndefined (FaceDetail)
  }
derive instance newtypePersonDetail :: Newtype PersonDetail _
derive instance repGenericPersonDetail :: Generic PersonDetail _
instance showPersonDetail :: Show PersonDetail where
  show = genericShow
instance decodePersonDetail :: Decode PersonDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonDetail :: Encode PersonDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details and tracking information for a single time a person is tracked in a video. Amazon Rekognition operations that track persons return an array of <code>PersonDetection</code> objects with elements for each time a person is tracked in a video. For more information, see . </p>
newtype PersonDetection = PersonDetection 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Person" :: NullOrUndefined.NullOrUndefined (PersonDetail)
  }
derive instance newtypePersonDetection :: Newtype PersonDetection _
derive instance repGenericPersonDetection :: Generic PersonDetection _
instance showPersonDetection :: Show PersonDetection where
  show = genericShow
instance decodePersonDetection :: Decode PersonDetection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonDetection :: Encode PersonDetection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PersonDetections = PersonDetections (Array PersonDetection)
derive instance newtypePersonDetections :: Newtype PersonDetections _
derive instance repGenericPersonDetections :: Generic PersonDetections _
instance showPersonDetections :: Show PersonDetections where
  show = genericShow
instance decodePersonDetections :: Decode PersonDetections where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonDetections :: Encode PersonDetections where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PersonIndex = PersonIndex Number
derive instance newtypePersonIndex :: Newtype PersonIndex _
derive instance repGenericPersonIndex :: Generic PersonIndex _
instance showPersonIndex :: Show PersonIndex where
  show = genericShow
instance decodePersonIndex :: Decode PersonIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonIndex :: Encode PersonIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a person whose face matches a face(s) in a Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection (,information about the person (<a>PersonDetail</a>) and the timestamp for when the person was detected in a video. An array of <code>PersonMatch</code> objects is returned by . </p>
newtype PersonMatch = PersonMatch 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Person" :: NullOrUndefined.NullOrUndefined (PersonDetail)
  , "FaceMatches" :: NullOrUndefined.NullOrUndefined (FaceMatchList)
  }
derive instance newtypePersonMatch :: Newtype PersonMatch _
derive instance repGenericPersonMatch :: Generic PersonMatch _
instance showPersonMatch :: Show PersonMatch where
  show = genericShow
instance decodePersonMatch :: Decode PersonMatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonMatch :: Encode PersonMatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PersonMatches = PersonMatches (Array PersonMatch)
derive instance newtypePersonMatches :: Newtype PersonMatches _
derive instance repGenericPersonMatches :: Generic PersonMatches _
instance showPersonMatches :: Show PersonMatches where
  show = genericShow
instance decodePersonMatches :: Decode PersonMatches where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonMatches :: Encode PersonMatches where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PersonTrackingSortBy = PersonTrackingSortBy String
derive instance newtypePersonTrackingSortBy :: Newtype PersonTrackingSortBy _
derive instance repGenericPersonTrackingSortBy :: Generic PersonTrackingSortBy _
instance showPersonTrackingSortBy :: Show PersonTrackingSortBy where
  show = genericShow
instance decodePersonTrackingSortBy :: Decode PersonTrackingSortBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePersonTrackingSortBy :: Encode PersonTrackingSortBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.</p> <p>An array of <code>Point</code> objects, <code>Polygon</code>, is returned by . <code>Polygon</code> represents a fine-grained polygon around detected text. For more information, see . </p>
newtype Point = Point 
  { "X" :: NullOrUndefined.NullOrUndefined (Number)
  , "Y" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePoint :: Newtype Point _
derive instance repGenericPoint :: Generic Point _
instance showPoint :: Show Point where
  show = genericShow
instance decodePoint :: Decode Point where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePoint :: Encode Point where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Polygon = Polygon (Array Point)
derive instance newtypePolygon :: Newtype Polygon _
derive instance repGenericPolygon :: Generic Polygon _
instance showPolygon :: Show Polygon where
  show = genericShow
instance decodePolygon :: Decode Polygon where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolygon :: Encode Polygon where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates the pose of the face as determined by its pitch, roll, and yaw.</p>
newtype Pose = Pose 
  { "Roll" :: NullOrUndefined.NullOrUndefined (Degree)
  , "Yaw" :: NullOrUndefined.NullOrUndefined (Degree)
  , "Pitch" :: NullOrUndefined.NullOrUndefined (Degree)
  }
derive instance newtypePose :: Newtype Pose _
derive instance repGenericPose :: Generic Pose _
instance showPose :: Show Pose where
  show = genericShow
instance decodePose :: Decode Pose where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePose :: Encode Pose where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.</p>
newtype ProvisionedThroughputExceededException = ProvisionedThroughputExceededException Types.NoArguments
derive instance newtypeProvisionedThroughputExceededException :: Newtype ProvisionedThroughputExceededException _
derive instance repGenericProvisionedThroughputExceededException :: Generic ProvisionedThroughputExceededException _
instance showProvisionedThroughputExceededException :: Show ProvisionedThroughputExceededException where
  show = genericShow
instance decodeProvisionedThroughputExceededException :: Decode ProvisionedThroughputExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProvisionedThroughputExceededException :: Encode ProvisionedThroughputExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecognizeCelebritiesRequest = RecognizeCelebritiesRequest 
  { "Image" :: (Image)
  }
derive instance newtypeRecognizeCelebritiesRequest :: Newtype RecognizeCelebritiesRequest _
derive instance repGenericRecognizeCelebritiesRequest :: Generic RecognizeCelebritiesRequest _
instance showRecognizeCelebritiesRequest :: Show RecognizeCelebritiesRequest where
  show = genericShow
instance decodeRecognizeCelebritiesRequest :: Decode RecognizeCelebritiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecognizeCelebritiesRequest :: Encode RecognizeCelebritiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecognizeCelebritiesResponse = RecognizeCelebritiesResponse 
  { "CelebrityFaces" :: NullOrUndefined.NullOrUndefined (CelebrityList)
  , "UnrecognizedFaces" :: NullOrUndefined.NullOrUndefined (ComparedFaceList)
  , "OrientationCorrection" :: NullOrUndefined.NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeRecognizeCelebritiesResponse :: Newtype RecognizeCelebritiesResponse _
derive instance repGenericRecognizeCelebritiesResponse :: Generic RecognizeCelebritiesResponse _
instance showRecognizeCelebritiesResponse :: Show RecognizeCelebritiesResponse where
  show = genericShow
instance decodeRecognizeCelebritiesResponse :: Decode RecognizeCelebritiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecognizeCelebritiesResponse :: Encode RecognizeCelebritiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RekognitionUniqueId = RekognitionUniqueId String
derive instance newtypeRekognitionUniqueId :: Newtype RekognitionUniqueId _
derive instance repGenericRekognitionUniqueId :: Generic RekognitionUniqueId _
instance showRekognitionUniqueId :: Show RekognitionUniqueId where
  show = genericShow
instance decodeRekognitionUniqueId :: Decode RekognitionUniqueId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRekognitionUniqueId :: Encode RekognitionUniqueId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection with the specified ID already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException Types.NoArguments
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ResourceInUseException = ResourceInUseException Types.NoArguments
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The collection specified in the request cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _
derive instance repGenericRoleArn :: Generic RoleArn _
instance showRoleArn :: Show RoleArn where
  show = genericShow
instance decodeRoleArn :: Decode RoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleArn :: Encode RoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _
derive instance repGenericS3Bucket :: Generic S3Bucket _
instance showS3Bucket :: Show S3Bucket where
  show = genericShow
instance decodeS3Bucket :: Decode S3Bucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Bucket :: Encode S3Bucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the S3 bucket name and object name.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype S3Object = S3Object 
  { "Bucket" :: NullOrUndefined.NullOrUndefined (S3Bucket)
  , "Name" :: NullOrUndefined.NullOrUndefined (S3ObjectName)
  , "Version" :: NullOrUndefined.NullOrUndefined (S3ObjectVersion)
  }
derive instance newtypeS3Object :: Newtype S3Object _
derive instance repGenericS3Object :: Generic S3Object _
instance showS3Object :: Show S3Object where
  show = genericShow
instance decodeS3Object :: Decode S3Object where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Object :: Encode S3Object where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3ObjectName = S3ObjectName String
derive instance newtypeS3ObjectName :: Newtype S3ObjectName _
derive instance repGenericS3ObjectName :: Generic S3ObjectName _
instance showS3ObjectName :: Show S3ObjectName where
  show = genericShow
instance decodeS3ObjectName :: Decode S3ObjectName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ObjectName :: Encode S3ObjectName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3ObjectVersion = S3ObjectVersion String
derive instance newtypeS3ObjectVersion :: Newtype S3ObjectVersion _
derive instance repGenericS3ObjectVersion :: Generic S3ObjectVersion _
instance showS3ObjectVersion :: Show S3ObjectVersion where
  show = genericShow
instance decodeS3ObjectVersion :: Decode S3ObjectVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ObjectVersion :: Encode S3ObjectVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SNSTopicArn = SNSTopicArn String
derive instance newtypeSNSTopicArn :: Newtype SNSTopicArn _
derive instance repGenericSNSTopicArn :: Generic SNSTopicArn _
instance showSNSTopicArn :: Show SNSTopicArn where
  show = genericShow
instance decodeSNSTopicArn :: Decode SNSTopicArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSNSTopicArn :: Encode SNSTopicArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchFacesByImageRequest = SearchFacesByImageRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "MaxFaces" :: NullOrUndefined.NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeSearchFacesByImageRequest :: Newtype SearchFacesByImageRequest _
derive instance repGenericSearchFacesByImageRequest :: Generic SearchFacesByImageRequest _
instance showSearchFacesByImageRequest :: Show SearchFacesByImageRequest where
  show = genericShow
instance decodeSearchFacesByImageRequest :: Decode SearchFacesByImageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchFacesByImageRequest :: Encode SearchFacesByImageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchFacesByImageResponse = SearchFacesByImageResponse 
  { "SearchedFaceBoundingBox" :: NullOrUndefined.NullOrUndefined (BoundingBox)
  , "SearchedFaceConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "FaceMatches" :: NullOrUndefined.NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSearchFacesByImageResponse :: Newtype SearchFacesByImageResponse _
derive instance repGenericSearchFacesByImageResponse :: Generic SearchFacesByImageResponse _
instance showSearchFacesByImageResponse :: Show SearchFacesByImageResponse where
  show = genericShow
instance decodeSearchFacesByImageResponse :: Decode SearchFacesByImageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchFacesByImageResponse :: Encode SearchFacesByImageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchFacesRequest = SearchFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceId" :: (FaceId)
  , "MaxFaces" :: NullOrUndefined.NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeSearchFacesRequest :: Newtype SearchFacesRequest _
derive instance repGenericSearchFacesRequest :: Generic SearchFacesRequest _
instance showSearchFacesRequest :: Show SearchFacesRequest where
  show = genericShow
instance decodeSearchFacesRequest :: Decode SearchFacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchFacesRequest :: Encode SearchFacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchFacesResponse = SearchFacesResponse 
  { "SearchedFaceId" :: NullOrUndefined.NullOrUndefined (FaceId)
  , "FaceMatches" :: NullOrUndefined.NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSearchFacesResponse :: Newtype SearchFacesResponse _
derive instance repGenericSearchFacesResponse :: Generic SearchFacesResponse _
instance showSearchFacesResponse :: Show SearchFacesResponse where
  show = genericShow
instance decodeSearchFacesResponse :: Decode SearchFacesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchFacesResponse :: Encode SearchFacesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the face is smiling, and the confidence level in the determination.</p>
newtype Smile = Smile 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeSmile :: Newtype Smile _
derive instance repGenericSmile :: Generic Smile _
instance showSmile :: Show Smile where
  show = genericShow
instance decodeSmile :: Decode Smile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmile :: Encode Smile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCelebrityRecognitionRequest = StartCelebrityRecognitionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartCelebrityRecognitionRequest :: Newtype StartCelebrityRecognitionRequest _
derive instance repGenericStartCelebrityRecognitionRequest :: Generic StartCelebrityRecognitionRequest _
instance showStartCelebrityRecognitionRequest :: Show StartCelebrityRecognitionRequest where
  show = genericShow
instance decodeStartCelebrityRecognitionRequest :: Decode StartCelebrityRecognitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCelebrityRecognitionRequest :: Encode StartCelebrityRecognitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartCelebrityRecognitionResponse :: Newtype StartCelebrityRecognitionResponse _
derive instance repGenericStartCelebrityRecognitionResponse :: Generic StartCelebrityRecognitionResponse _
instance showStartCelebrityRecognitionResponse :: Show StartCelebrityRecognitionResponse where
  show = genericShow
instance decodeStartCelebrityRecognitionResponse :: Decode StartCelebrityRecognitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCelebrityRecognitionResponse :: Encode StartCelebrityRecognitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartContentModerationRequest = StartContentModerationRequest 
  { "Video" :: (Video)
  , "MinConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartContentModerationRequest :: Newtype StartContentModerationRequest _
derive instance repGenericStartContentModerationRequest :: Generic StartContentModerationRequest _
instance showStartContentModerationRequest :: Show StartContentModerationRequest where
  show = genericShow
instance decodeStartContentModerationRequest :: Decode StartContentModerationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartContentModerationRequest :: Encode StartContentModerationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartContentModerationResponse = StartContentModerationResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartContentModerationResponse :: Newtype StartContentModerationResponse _
derive instance repGenericStartContentModerationResponse :: Generic StartContentModerationResponse _
instance showStartContentModerationResponse :: Show StartContentModerationResponse where
  show = genericShow
instance decodeStartContentModerationResponse :: Decode StartContentModerationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartContentModerationResponse :: Encode StartContentModerationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartFaceDetectionRequest = StartFaceDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "FaceAttributes" :: NullOrUndefined.NullOrUndefined (FaceAttributes)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartFaceDetectionRequest :: Newtype StartFaceDetectionRequest _
derive instance repGenericStartFaceDetectionRequest :: Generic StartFaceDetectionRequest _
instance showStartFaceDetectionRequest :: Show StartFaceDetectionRequest where
  show = genericShow
instance decodeStartFaceDetectionRequest :: Decode StartFaceDetectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartFaceDetectionRequest :: Encode StartFaceDetectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartFaceDetectionResponse = StartFaceDetectionResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartFaceDetectionResponse :: Newtype StartFaceDetectionResponse _
derive instance repGenericStartFaceDetectionResponse :: Generic StartFaceDetectionResponse _
instance showStartFaceDetectionResponse :: Show StartFaceDetectionResponse where
  show = genericShow
instance decodeStartFaceDetectionResponse :: Decode StartFaceDetectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartFaceDetectionResponse :: Encode StartFaceDetectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartFaceSearchRequest = StartFaceSearchRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "FaceMatchThreshold" :: NullOrUndefined.NullOrUndefined (Percent)
  , "CollectionId" :: (CollectionId)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartFaceSearchRequest :: Newtype StartFaceSearchRequest _
derive instance repGenericStartFaceSearchRequest :: Generic StartFaceSearchRequest _
instance showStartFaceSearchRequest :: Show StartFaceSearchRequest where
  show = genericShow
instance decodeStartFaceSearchRequest :: Decode StartFaceSearchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartFaceSearchRequest :: Encode StartFaceSearchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartFaceSearchResponse = StartFaceSearchResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartFaceSearchResponse :: Newtype StartFaceSearchResponse _
derive instance repGenericStartFaceSearchResponse :: Generic StartFaceSearchResponse _
instance showStartFaceSearchResponse :: Show StartFaceSearchResponse where
  show = genericShow
instance decodeStartFaceSearchResponse :: Decode StartFaceSearchResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartFaceSearchResponse :: Encode StartFaceSearchResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartLabelDetectionRequest = StartLabelDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "MinConfidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartLabelDetectionRequest :: Newtype StartLabelDetectionRequest _
derive instance repGenericStartLabelDetectionRequest :: Generic StartLabelDetectionRequest _
instance showStartLabelDetectionRequest :: Show StartLabelDetectionRequest where
  show = genericShow
instance decodeStartLabelDetectionRequest :: Decode StartLabelDetectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLabelDetectionRequest :: Encode StartLabelDetectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartLabelDetectionResponse = StartLabelDetectionResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartLabelDetectionResponse :: Newtype StartLabelDetectionResponse _
derive instance repGenericStartLabelDetectionResponse :: Generic StartLabelDetectionResponse _
instance showStartLabelDetectionResponse :: Show StartLabelDetectionResponse where
  show = genericShow
instance decodeStartLabelDetectionResponse :: Decode StartLabelDetectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLabelDetectionResponse :: Encode StartLabelDetectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartPersonTrackingRequest = StartPersonTrackingRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined.NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined.NullOrUndefined (JobTag)
  }
derive instance newtypeStartPersonTrackingRequest :: Newtype StartPersonTrackingRequest _
derive instance repGenericStartPersonTrackingRequest :: Generic StartPersonTrackingRequest _
instance showStartPersonTrackingRequest :: Show StartPersonTrackingRequest where
  show = genericShow
instance decodeStartPersonTrackingRequest :: Decode StartPersonTrackingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartPersonTrackingRequest :: Encode StartPersonTrackingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartPersonTrackingResponse = StartPersonTrackingResponse 
  { "JobId" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeStartPersonTrackingResponse :: Newtype StartPersonTrackingResponse _
derive instance repGenericStartPersonTrackingResponse :: Generic StartPersonTrackingResponse _
instance showStartPersonTrackingResponse :: Show StartPersonTrackingResponse where
  show = genericShow
instance decodeStartPersonTrackingResponse :: Decode StartPersonTrackingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartPersonTrackingResponse :: Encode StartPersonTrackingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartStreamProcessorRequest = StartStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeStartStreamProcessorRequest :: Newtype StartStreamProcessorRequest _
derive instance repGenericStartStreamProcessorRequest :: Generic StartStreamProcessorRequest _
instance showStartStreamProcessorRequest :: Show StartStreamProcessorRequest where
  show = genericShow
instance decodeStartStreamProcessorRequest :: Decode StartStreamProcessorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartStreamProcessorRequest :: Encode StartStreamProcessorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartStreamProcessorResponse = StartStreamProcessorResponse Types.NoArguments
derive instance newtypeStartStreamProcessorResponse :: Newtype StartStreamProcessorResponse _
derive instance repGenericStartStreamProcessorResponse :: Generic StartStreamProcessorResponse _
instance showStartStreamProcessorResponse :: Show StartStreamProcessorResponse where
  show = genericShow
instance decodeStartStreamProcessorResponse :: Decode StartStreamProcessorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartStreamProcessorResponse :: Encode StartStreamProcessorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusMessage = StatusMessage String
derive instance newtypeStatusMessage :: Newtype StatusMessage _
derive instance repGenericStatusMessage :: Generic StatusMessage _
instance showStatusMessage :: Show StatusMessage where
  show = genericShow
instance decodeStatusMessage :: Decode StatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusMessage :: Encode StatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopStreamProcessorRequest = StopStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeStopStreamProcessorRequest :: Newtype StopStreamProcessorRequest _
derive instance repGenericStopStreamProcessorRequest :: Generic StopStreamProcessorRequest _
instance showStopStreamProcessorRequest :: Show StopStreamProcessorRequest where
  show = genericShow
instance decodeStopStreamProcessorRequest :: Decode StopStreamProcessorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopStreamProcessorRequest :: Encode StopStreamProcessorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopStreamProcessorResponse = StopStreamProcessorResponse Types.NoArguments
derive instance newtypeStopStreamProcessorResponse :: Newtype StopStreamProcessorResponse _
derive instance repGenericStopStreamProcessorResponse :: Generic StopStreamProcessorResponse _
instance showStopStreamProcessorResponse :: Show StopStreamProcessorResponse where
  show = genericShow
instance decodeStopStreamProcessorResponse :: Decode StopStreamProcessorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopStreamProcessorResponse :: Encode StopStreamProcessorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to . The request parameters for <code>CreateStreamProcessor</code> describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts. </p>
newtype StreamProcessor = StreamProcessor 
  { "Name" :: NullOrUndefined.NullOrUndefined (StreamProcessorName)
  , "Status" :: NullOrUndefined.NullOrUndefined (StreamProcessorStatus)
  }
derive instance newtypeStreamProcessor :: Newtype StreamProcessor _
derive instance repGenericStreamProcessor :: Generic StreamProcessor _
instance showStreamProcessor :: Show StreamProcessor where
  show = genericShow
instance decodeStreamProcessor :: Decode StreamProcessor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessor :: Encode StreamProcessor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamProcessorArn = StreamProcessorArn String
derive instance newtypeStreamProcessorArn :: Newtype StreamProcessorArn _
derive instance repGenericStreamProcessorArn :: Generic StreamProcessorArn _
instance showStreamProcessorArn :: Show StreamProcessorArn where
  show = genericShow
instance decodeStreamProcessorArn :: Decode StreamProcessorArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorArn :: Encode StreamProcessorArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the source streaming video. </p>
newtype StreamProcessorInput = StreamProcessorInput 
  { "KinesisVideoStream" :: NullOrUndefined.NullOrUndefined (KinesisVideoStream)
  }
derive instance newtypeStreamProcessorInput :: Newtype StreamProcessorInput _
derive instance repGenericStreamProcessorInput :: Generic StreamProcessorInput _
instance showStreamProcessorInput :: Show StreamProcessorInput where
  show = genericShow
instance decodeStreamProcessorInput :: Decode StreamProcessorInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorInput :: Encode StreamProcessorInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamProcessorList = StreamProcessorList (Array StreamProcessor)
derive instance newtypeStreamProcessorList :: Newtype StreamProcessorList _
derive instance repGenericStreamProcessorList :: Generic StreamProcessorList _
instance showStreamProcessorList :: Show StreamProcessorList where
  show = genericShow
instance decodeStreamProcessorList :: Decode StreamProcessorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorList :: Encode StreamProcessorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamProcessorName = StreamProcessorName String
derive instance newtypeStreamProcessorName :: Newtype StreamProcessorName _
derive instance repGenericStreamProcessorName :: Generic StreamProcessorName _
instance showStreamProcessorName :: Show StreamProcessorName where
  show = genericShow
instance decodeStreamProcessorName :: Decode StreamProcessorName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorName :: Encode StreamProcessorName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the Amazon Kinesis Data Streams stream to which a Rekognition Video stream processor streams the results of a video analysis. For more information, see .</p>
newtype StreamProcessorOutput = StreamProcessorOutput 
  { "KinesisDataStream" :: NullOrUndefined.NullOrUndefined (KinesisDataStream)
  }
derive instance newtypeStreamProcessorOutput :: Newtype StreamProcessorOutput _
derive instance repGenericStreamProcessorOutput :: Generic StreamProcessorOutput _
instance showStreamProcessorOutput :: Show StreamProcessorOutput where
  show = genericShow
instance decodeStreamProcessorOutput :: Decode StreamProcessorOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorOutput :: Encode StreamProcessorOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.</p>
newtype StreamProcessorSettings = StreamProcessorSettings 
  { "FaceSearch" :: NullOrUndefined.NullOrUndefined (FaceSearchSettings)
  }
derive instance newtypeStreamProcessorSettings :: Newtype StreamProcessorSettings _
derive instance repGenericStreamProcessorSettings :: Generic StreamProcessorSettings _
instance showStreamProcessorSettings :: Show StreamProcessorSettings where
  show = genericShow
instance decodeStreamProcessorSettings :: Decode StreamProcessorSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorSettings :: Encode StreamProcessorSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamProcessorStatus = StreamProcessorStatus String
derive instance newtypeStreamProcessorStatus :: Newtype StreamProcessorStatus _
derive instance repGenericStreamProcessorStatus :: Generic StreamProcessorStatus _
instance showStreamProcessorStatus :: Show StreamProcessorStatus where
  show = genericShow
instance decodeStreamProcessorStatus :: Decode StreamProcessorStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamProcessorStatus :: Encode StreamProcessorStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.</p>
newtype Sunglasses = Sunglasses 
  { "Value" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  }
derive instance newtypeSunglasses :: Newtype Sunglasses _
derive instance repGenericSunglasses :: Generic Sunglasses _
instance showSunglasses :: Show Sunglasses where
  show = genericShow
instance decodeSunglasses :: Decode Sunglasses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSunglasses :: Encode Sunglasses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a word or line of text detected by .</p> <p>The <code>DetectedText</code> field contains the text that Amazon Rekognition detected in the image. </p> <p>Every word and line has an identifier (<code>Id</code>). Each word belongs to a line and has a parent identifier (<code>ParentId</code>) that identifies the line of text in which the word appears. The word <code>Id</code> is also an index for the word within a line of words. </p> <p>For more information, see <a>text-detection</a>.</p>
newtype TextDetection = TextDetection 
  { "DetectedText" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (TextTypes)
  , "Id" :: NullOrUndefined.NullOrUndefined (UInteger)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (UInteger)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Percent)
  , "Geometry" :: NullOrUndefined.NullOrUndefined (Geometry)
  }
derive instance newtypeTextDetection :: Newtype TextDetection _
derive instance repGenericTextDetection :: Generic TextDetection _
instance showTextDetection :: Show TextDetection where
  show = genericShow
instance decodeTextDetection :: Decode TextDetection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextDetection :: Encode TextDetection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TextDetectionList = TextDetectionList (Array TextDetection)
derive instance newtypeTextDetectionList :: Newtype TextDetectionList _
derive instance repGenericTextDetectionList :: Generic TextDetectionList _
instance showTextDetectionList :: Show TextDetectionList where
  show = genericShow
instance decodeTextDetectionList :: Decode TextDetectionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextDetectionList :: Encode TextDetectionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TextTypes = TextTypes String
derive instance newtypeTextTypes :: Newtype TextTypes _
derive instance repGenericTextTypes :: Generic TextTypes _
instance showTextTypes :: Show TextTypes where
  show = genericShow
instance decodeTextTypes :: Decode TextTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextTypes :: Encode TextTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Rekognition is temporarily unable to process the request. Try your call again.</p>
newtype ThrottlingException = ThrottlingException Types.NoArguments
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UInteger = UInteger Int
derive instance newtypeUInteger :: Newtype UInteger _
derive instance repGenericUInteger :: Generic UInteger _
instance showUInteger :: Show UInteger where
  show = genericShow
instance decodeUInteger :: Decode UInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUInteger :: Encode UInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ULong = ULong Number
derive instance newtypeULong :: Newtype ULong _
derive instance repGenericULong :: Generic ULong _
instance showULong :: Show ULong where
  show = genericShow
instance decodeULong :: Decode ULong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeULong :: Encode ULong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive instance repGenericUrl :: Generic Url _
instance showUrl :: Show Url where
  show = genericShow
instance decodeUrl :: Decode Url where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrl :: Encode Url where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Urls = Urls (Array Url)
derive instance newtypeUrls :: Newtype Urls _
derive instance repGenericUrls :: Generic Urls _
instance showUrls :: Show Urls where
  show = genericShow
instance decodeUrls :: Decode Urls where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrls :: Encode Urls where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as use <code>Video</code> to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.</p>
newtype Video = Video 
  { "S3Object" :: NullOrUndefined.NullOrUndefined (S3Object)
  }
derive instance newtypeVideo :: Newtype Video _
derive instance repGenericVideo :: Generic Video _
instance showVideo :: Show Video where
  show = genericShow
instance decodeVideo :: Decode Video where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideo :: Encode Video where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VideoJobStatus = VideoJobStatus String
derive instance newtypeVideoJobStatus :: Newtype VideoJobStatus _
derive instance repGenericVideoJobStatus :: Generic VideoJobStatus _
instance showVideoJobStatus :: Show VideoJobStatus where
  show = genericShow
instance decodeVideoJobStatus :: Decode VideoJobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoJobStatus :: Encode VideoJobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a video that Amazon Rekognition analyzed. <code>Videometadata</code> is returned in every page of paginated responses from a Amazon Rekognition video operation.</p>
newtype VideoMetadata = VideoMetadata 
  { "Codec" :: NullOrUndefined.NullOrUndefined (String)
  , "DurationMillis" :: NullOrUndefined.NullOrUndefined (ULong)
  , "Format" :: NullOrUndefined.NullOrUndefined (String)
  , "FrameRate" :: NullOrUndefined.NullOrUndefined (Number)
  , "FrameHeight" :: NullOrUndefined.NullOrUndefined (ULong)
  , "FrameWidth" :: NullOrUndefined.NullOrUndefined (ULong)
  }
derive instance newtypeVideoMetadata :: Newtype VideoMetadata _
derive instance repGenericVideoMetadata :: Generic VideoMetadata _
instance showVideoMetadata :: Show VideoMetadata where
  show = genericShow
instance decodeVideoMetadata :: Decode VideoMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoMetadata :: Encode VideoMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file size or duration of the supplied media is too large. The maximum file size is 8GB. The maximum duration is 2 hours. </p>
newtype VideoTooLargeException = VideoTooLargeException Types.NoArguments
derive instance newtypeVideoTooLargeException :: Newtype VideoTooLargeException _
derive instance repGenericVideoTooLargeException :: Generic VideoTooLargeException _
instance showVideoTooLargeException :: Show VideoTooLargeException where
  show = genericShow
instance decodeVideoTooLargeException :: Decode VideoTooLargeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoTooLargeException :: Encode VideoTooLargeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
