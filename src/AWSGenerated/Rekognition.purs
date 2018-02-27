

-- | <p>This is the Amazon Rekognition API reference.</p>
module AWS.Rekognition where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Rekognition" :: String


-- | <p>Compares a face in the <i>source</i> input image with each of the 100 largest faces detected in the <i>target</i> input image. </p> <note> <p> If the source image contains multiple faces, the service detects the largest face and compares it with each face detected in the target image. </p> </note> <p>You pass the input and target images either as base64-encoded image bytes or as a references to images in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>In response, the operation returns an array of face matches ordered by similarity score in descending order. For each face match, the response provides a bounding box of the face, facial landmarks, pose details (pitch, role, and yaw), quality (brightness and sharpness), and confidence value (indicating the level of confidence that the bounding box contains a face). The response also provides a similarity score, which indicates how closely the faces match. </p> <note> <p>By default, only faces with a similarity score of greater than or equal to 80% are returned in the response. You can change this value by specifying the <code>SimilarityThreshold</code> parameter.</p> </note> <p> <code>CompareFaces</code> also returns an array of faces that don't match the source image. For each face, it returns a bounding box, confidence value, landmarks, pose details, and quality. The response also returns information about the face in the source image, including the bounding box of the face and confidence value.</p> <p>If the image doesn't contain Exif metadata, <code>CompareFaces</code> returns orientation information for the source and target images. Use these values to display the images with the correct image orientation.</p> <p>If no faces are detected in the source or target images, <code>CompareFaces</code> returns an <code>InvalidParameterException</code> error. </p> <note> <p> This is a stateless API operation. That is, data returned by this operation doesn't persist.</p> </note> <p>For an example, see <a>faces-compare-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:CompareFaces</code> action.</p>
compareFaces :: forall eff. CompareFacesRequest -> Aff (err :: AWS.RequestError | eff) CompareFacesResponse
compareFaces = AWS.request serviceName "compareFaces" 


-- | <p>Creates a collection in an AWS Region. You can add faces to the collection using the operation. </p> <p>For example, you might create collections, one for each of your application users. A user can then index faces using the <code>IndexFaces</code> operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container. </p> <note> <p>Collection names are case-sensitive.</p> </note> <p>This operation requires permissions to perform the <code>rekognition:CreateCollection</code> action.</p>
createCollection :: forall eff. CreateCollectionRequest -> Aff (err :: AWS.RequestError | eff) CreateCollectionResponse
createCollection = AWS.request serviceName "createCollection" 


-- | <p>Creates an Amazon Rekognition stream processor that you can use to detect and recognize faces in a streaming video.</p> <p>Rekognition Video is a consumer of live video from Amazon Kinesis Video Streams. Rekognition Video sends analysis results to Amazon Kinesis Data Streams.</p> <p>You provide as input a Kinesis video stream (<code>Input</code>) and a Kinesis data stream (<code>Output</code>) stream. You also specify the face recognition criteria in <code>Settings</code>. For example, the collection containing faces that you want to recognize. Use <code>Name</code> to assign an identifier for the stream processor. You use <code>Name</code> to manage the stream processor. For example, you can start processing the source video by calling with the <code>Name</code> field. </p> <p>After you have finished analyzing a streaming video, use to stop processing. You can delete the stream processor by calling .</p>
createStreamProcessor :: forall eff. CreateStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamProcessorResponse
createStreamProcessor = AWS.request serviceName "createStreamProcessor" 


-- | <p>Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see <a>delete-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteCollection</code> action.</p>
deleteCollection :: forall eff. DeleteCollectionRequest -> Aff (err :: AWS.RequestError | eff) DeleteCollectionResponse
deleteCollection = AWS.request serviceName "deleteCollection" 


-- | <p>Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteFaces</code> action.</p>
deleteFaces :: forall eff. DeleteFacesRequest -> Aff (err :: AWS.RequestError | eff) DeleteFacesResponse
deleteFaces = AWS.request serviceName "deleteFaces" 


-- | <p>Deletes the stream processor identified by <code>Name</code>. You assign the value for <code>Name</code> when you create the stream processor with . You might not be able to use the same name for a stream processor for a few seconds after calling <code>DeleteStreamProcessor</code>.</p>
deleteStreamProcessor :: forall eff. DeleteStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) DeleteStreamProcessorResponse
deleteStreamProcessor = AWS.request serviceName "deleteStreamProcessor" 


-- | <p>Provides information about a stream processor created by . You can get information about the input and output streams, the input parameters for the face recognition being performed, and the current status of the stream processor.</p>
describeStreamProcessor :: forall eff. DescribeStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) DescribeStreamProcessorResponse
describeStreamProcessor = AWS.request serviceName "describeStreamProcessor" 


-- | <p>Detects faces within an image that is provided as input.</p> <p> <code>DetectFaces</code> detects the 100 largest faces in the image. For each face detected, the operation returns face details including a bounding box of the face, a confidence value (that the bounding box contains a face), and a fixed set of attributes such as facial landmarks (for example, coordinates of eye and mouth), gender, presence of beard, sunglasses, etc. </p> <p>The face-detection algorithm is most effective on frontal faces. For non-frontal or obscured faces, the algorithm may not detect the faces or might detect faces with lower confidence. </p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> </note> <p>For an example, see <a>procedure-detecting-faces-in-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectFaces</code> action. </p>
detectFaces :: forall eff. DetectFacesRequest -> Aff (err :: AWS.RequestError | eff) DetectFacesResponse
detectFaces = AWS.request serviceName "detectFaces" 


-- | <p>Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature. For an example, see <a>images-s3</a>.</p> <note> <p> <code>DetectLabels</code> does not support the detection of activities. However, activity detection is supported for label detection in videos. For more information, see .</p> </note> <p>You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response will include all three labels, one for each object. </p> <p> <code>{Name: lighthouse, Confidence: 98.4629}</code> </p> <p> <code>{Name: rock,Confidence: 79.2097}</code> </p> <p> <code> {Name: sea,Confidence: 75.061}</code> </p> <p> In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels. </p> <p> <code>{Name: flower,Confidence: 99.0562}</code> </p> <p> <code>{Name: plant,Confidence: 99.0562}</code> </p> <p> <code>{Name: tulip,Confidence: 99.0562}</code> </p> <p>In this example, the detection algorithm more precisely identifies the flower as a tulip.</p> <p>In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify <code>MinConfidence</code> to control the confidence threshold for the labels returned. The default is 50%. You can also add the <code>MaxLabels</code> parameter to limit the number of labels returned. </p> <note> <p>If the object detected is a person, the operation doesn't provide the same facial details that the <a>DetectFaces</a> operation provides.</p> </note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectLabels</code> action. </p>
detectLabels :: forall eff. DetectLabelsRequest -> Aff (err :: AWS.RequestError | eff) DetectLabelsResponse
detectLabels = AWS.request serviceName "detectLabels" 


-- | <p>Detects explicit or suggestive adult content in a specified JPEG or PNG format image. Use <code>DetectModerationLabels</code> to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.</p> <p>To filter images, use the labels returned by <code>DetectModerationLabels</code> to determine which types of content are appropriate. For information about moderation labels, see <a>moderation</a>.</p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p>
detectModerationLabels :: forall eff. DetectModerationLabelsRequest -> Aff (err :: AWS.RequestError | eff) DetectModerationLabelsResponse
detectModerationLabels = AWS.request serviceName "detectModerationLabels" 


-- | <p>Detects text in the input image and converts it into machine-readable text.</p> <p>Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file. </p> <p>The <code>DetectText</code> operation returns text in an array of elements, <code>TextDetections</code>. Each <code>TextDetection</code> element provides information about a single word or line of text that was detected in the image. </p> <p>A word is one or more ISO basic latin script characters that are not separated by spaces. <code>DetectText</code> can detect up to 50 words in an image.</p> <p>A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the <code>DetectText</code> operation returns multiple lines.</p> <p>To determine whether a <code>TextDetection</code> element is a line of text or a word, use the <code>TextDetection</code> object <code>Type</code> field. </p> <p>To be detected, text must be within +/- 30 degrees orientation of the horizontal axis.</p> <p>For more information, see <a>text-detection</a>.</p>
detectText :: forall eff. DetectTextRequest -> Aff (err :: AWS.RequestError | eff) DetectTextResponse
detectText = AWS.request serviceName "detectText" 


-- | <p>Gets the name and additional information about a celebrity based on his or her Rekognition ID. The additional information is returned as an array of URLs. If there is no additional information about the celebrity, this list is empty. For more information, see <a>get-celebrity-info-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:GetCelebrityInfo</code> action. </p>
getCelebrityInfo :: forall eff. GetCelebrityInfoRequest -> Aff (err :: AWS.RequestError | eff) GetCelebrityInfoResponse
getCelebrityInfo = AWS.request serviceName "getCelebrityInfo" 


-- | <p>Gets the celebrity recognition results for a Rekognition Video analysis started by .</p> <p>Celebrity recognition in a video is an asynchronous operation. Analysis is started by a call to which returns a job identifier (<code>JobId</code>). When the celebrity recognition operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartCelebrityRecognition</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>.</p> <p> <code>GetCelebrityRecognition</code> returns detected celebrities and the time(s) they are detected in an array (<code>Celebrities</code>) of objects. Each <code>CelebrityRecognition</code> contains information about the celebrity in a object and the time, <code>Timestamp</code>, the celebrity was detected. </p> <p>By default, the <code>Celebrities</code> array is sorted by time (milliseconds from the start of the video). You can also sort the array by celebrity by specifying the value <code>ID</code> in the <code>SortBy</code> input parameter.</p> <p>The <code>CelebrityDetail</code> object includes the celebrity identifer and additional information urls. If you don't store the additional information urls, you can get them later by calling with the celebrity identifer.</p> <p>No information is returned for faces not recognized as celebrities.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetCelebrityDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetCelebrityRecognition</code>.</p>
getCelebrityRecognition :: forall eff. GetCelebrityRecognitionRequest -> Aff (err :: AWS.RequestError | eff) GetCelebrityRecognitionResponse
getCelebrityRecognition = AWS.request serviceName "getCelebrityRecognition" 


-- | <p>Gets the content moderation analysis results for a Rekognition Video analysis started by .</p> <p>Content moderation analysis of a video is an asynchronous operation. You start analysis by calling . which returns a job identifier (<code>JobId</code>). When analysis finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartContentModeration</code>. To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>. </p> <p> <code>GetContentModeration</code> returns detected content moderation labels, and the time they are detected, in an array, <code>ModerationLabels</code>, of objects. </p> <p>By default, the moderated labels are returned sorted by time, in milliseconds from the start of the video. You can also sort them by moderated label by specifying <code>NAME</code> for the <code>SortBy</code> input parameter. </p> <p>Since video analysis can return a large number of results, use the <code>MaxResults</code> parameter to limit the number of labels returned in a single call to <code>GetContentModeration</code>. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetContentModeration</code> and populate the <code>NextToken</code> request parameter with the value of <code>NextToken</code> returned from the previous call to <code>GetContentModeration</code>.</p> <p>For more information, see <a>moderation</a>.</p>
getContentModeration :: forall eff. GetContentModerationRequest -> Aff (err :: AWS.RequestError | eff) GetContentModerationResponse
getContentModeration = AWS.request serviceName "getContentModeration" 


-- | <p>Gets face detection results for a Rekognition Video analysis started by .</p> <p>Face detection with Rekognition Video is an asynchronous operation. You start face detection by calling which returns a job identifier (<code>JobId</code>). When the face detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceDetection</code>. To get the results of the face detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>.</p> <p> <code>GetFaceDetection</code> returns an array of detected faces (<code>Faces</code>) sorted by the time the faces were detected. </p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetFaceDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetFaceDetection</code>.</p>
getFaceDetection :: forall eff. GetFaceDetectionRequest -> Aff (err :: AWS.RequestError | eff) GetFaceDetectionResponse
getFaceDetection = AWS.request serviceName "getFaceDetection" 


-- | <p>Gets the face search results for Rekognition Video face search started by . The search returns faces in a collection that match the faces of persons detected in a video. It also includes the time(s) that faces are matched in the video.</p> <p>Face search in a video is an asynchronous operation. You start face search by calling to which returns a job identifier (<code>JobId</code>). When the search operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceSearch</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetFaceSearch</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections</a>.</p> <p>The search results are retured in an array, <code>Persons</code>, of objects. Each<code>PersonMatch</code> element contains details about the matching faces in the input collection, person information for the matched person, and the time the person was matched in the video.</p> <p>By default, the <code>Persons</code> array is sorted by the time, in milliseconds from the start of the video, persons are matched. You can also sort by persons by specifying <code>INDEX</code> for the <code>SORTBY</code> input parameter.</p>
getFaceSearch :: forall eff. GetFaceSearchRequest -> Aff (err :: AWS.RequestError | eff) GetFaceSearchResponse
getFaceSearch = AWS.request serviceName "getFaceSearch" 


-- | <p>Gets the label detection results of a Rekognition Video analysis started by . </p> <p>The label detection operation is started by a call to which returns a job identifier (<code>JobId</code>). When the label detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartlabelDetection</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p> <code>GetLabelDetection</code> returns an array of detected labels (<code>Labels</code>) sorted by the time the labels were detected. You can also sort by the label name by specifying <code>NAME</code> for the <code>SortBy</code> input parameter.</p> <p>The labels returned include the label name, the percentage confidence in the accuracy of the detected label, and the time the label was detected in the video.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetlabelDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetLabelDetection</code>.</p>
getLabelDetection :: forall eff. GetLabelDetectionRequest -> Aff (err :: AWS.RequestError | eff) GetLabelDetectionResponse
getLabelDetection = AWS.request serviceName "getLabelDetection" 


-- | <p>Gets the person tracking results of a Rekognition Video analysis started by .</p> <p>The person detection operation is started by a call to <code>StartPersonTracking</code> which returns a job identifier (<code>JobId</code>). When the person detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartPersonTracking</code>.</p> <p>To get the results of the person tracking operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p> <p> <code>GetPersonTracking</code> returns an array, <code>Persons</code>, of tracked persons and the time(s) they were tracked in the video. </p> <p>By default, the array is sorted by the time(s) a person is tracked in the video. You can sort by tracked persons by specifying <code>INDEX</code> for the <code>SortBy</code> input parameter.</p> <p>Use the <code>MaxResults</code> parameter to limit the number of items returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetPersonTracking</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetPersonTracking</code>.</p>
getPersonTracking :: forall eff. GetPersonTrackingRequest -> Aff (err :: AWS.RequestError | eff) GetPersonTrackingResponse
getPersonTracking = AWS.request serviceName "getPersonTracking" 


-- | <p>Detects faces in the input image and adds them to the specified collection. </p> <p>Amazon Rekognition does not save the actual faces detected. Instead, the underlying detection algorithm first detects the faces in the input image, and for each face extracts facial features into a feature vector, and stores it in the back-end database. Amazon Rekognition uses feature vectors when performing face match and search operations using the and operations.</p> <p>If you are using version 1.0 of the face detection model, <code>IndexFaces</code> indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image. To determine which version of the model you are using, check the the value of <code>FaceModelVersion</code> in the response from <code>IndexFaces</code>. For more information, see <a>face-detection-model</a>.</p> <p>If you provide the optional <code>ExternalImageID</code> for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image. </p> <p>In response, the operation returns an array of metadata for all detected faces. This includes, the bounding box of the detected face, confidence value (indicating the bounding box contains a face), a face ID assigned by the service for each face that is detected and stored, and an image ID assigned by the service for the input image. If you request all facial attributes (using the <code>detectionAttributes</code> parameter, Amazon Rekognition returns detailed facial attributes such as facial landmarks (for example, location of eye and mount) and other facial attributes such gender. If you provide the same image, specify the same collection, and use the same external ID in the <code>IndexFaces</code> operation, Amazon Rekognition doesn't save duplicate face metadata. </p> <p>The input image is passed either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>This operation requires permissions to perform the <code>rekognition:IndexFaces</code> action.</p>
indexFaces :: forall eff. IndexFacesRequest -> Aff (err :: AWS.RequestError | eff) IndexFacesResponse
indexFaces = AWS.request serviceName "indexFaces" 


-- | <p>Returns list of collection IDs in your account. If the result is truncated, the response also provides a <code>NextToken</code> that you can use in the subsequent request to fetch the next set of collection IDs.</p> <p>For an example, see <a>list-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:ListCollections</code> action.</p>
listCollections :: forall eff. ListCollectionsRequest -> Aff (err :: AWS.RequestError | eff) ListCollectionsResponse
listCollections = AWS.request serviceName "listCollections" 


-- | <p>Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see <a>list-faces-in-collection-procedure</a>. </p> <p>This operation requires permissions to perform the <code>rekognition:ListFaces</code> action.</p>
listFaces :: forall eff. ListFacesRequest -> Aff (err :: AWS.RequestError | eff) ListFacesResponse
listFaces = AWS.request serviceName "listFaces" 


-- | <p>Gets a list of stream processors that you have created with . </p>
listStreamProcessors :: forall eff. ListStreamProcessorsRequest -> Aff (err :: AWS.RequestError | eff) ListStreamProcessorsResponse
listStreamProcessors = AWS.request serviceName "listStreamProcessors" 


-- | <p>Returns an array of celebrities recognized in the input image. For more information, see <a>celebrities</a>. </p> <p> <code>RecognizeCelebrities</code> returns the 100 largest faces in the image. It lists recognized celebrities in the <code>CelebrityFaces</code> array and unrecognized faces in the <code>UnrecognizedFaces</code> array. <code>RecognizeCelebrities</code> doesn't return celebrities whose faces are not amongst the largest 100 faces in the image.</p> <p>For each celebrity recognized, the <code>RecognizeCelebrities</code> returns a <code>Celebrity</code> object. The <code>Celebrity</code> object contains the celebrity name, ID, URL links to additional information, match confidence, and a <code>ComparedFace</code> object that you can use to locate the celebrity's face on the image.</p> <p>Rekognition does not retain information about which images a celebrity has been recognized in. Your application must store this information and use the <code>Celebrity</code> ID property as a unique identifier for the celebrity. If you don't store the celebrity name or additional information URLs returned by <code>RecognizeCelebrities</code>, you will need the ID to identify the celebrity in a call to the operation.</p> <p>You pass the imput image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>For an example, see <a>celebrities-procedure-image</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:RecognizeCelebrities</code> operation.</p>
recognizeCelebrities :: forall eff. RecognizeCelebritiesRequest -> Aff (err :: AWS.RequestError | eff) RecognizeCelebritiesResponse
recognizeCelebrities = AWS.request serviceName "recognizeCelebrities" 


-- | <p>For a given input face ID, searches for matching faces in the collection the face belongs to. You get a face ID when you add a face to the collection using the <a>IndexFaces</a> operation. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p>You can also search faces without indexing faces by using the <code>SearchFacesByImage</code> operation.</p> </note> <p> The operation response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match that is found. Along with the metadata, the response also includes a <code>confidence</code> value for each face match, indicating the confidence that the specific face matches the input face. </p> <p>For an example, see <a>search-face-with-id-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFaces</code> action.</p>
searchFaces :: forall eff. SearchFacesRequest -> Aff (err :: AWS.RequestError | eff) SearchFacesResponse
searchFaces = AWS.request serviceName "searchFaces" 


-- | <p>For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p> To search for all faces in an input image, you might first call the operation, and then use the face IDs returned in subsequent calls to the operation. </p> <p> You can also call the <code>DetectFaces</code> operation and use the bounding boxes in the response to make face crops, which then you can pass in to the <code>SearchFacesByImage</code> operation. </p> </note> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a <code>similarity</code> indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image. </p> <p>For an example, see <a>search-face-with-image-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFacesByImage</code> action.</p>
searchFacesByImage :: forall eff. SearchFacesByImageRequest -> Aff (err :: AWS.RequestError | eff) SearchFacesByImageResponse
searchFacesByImage = AWS.request serviceName "searchFacesByImage" 


-- | <p>Starts asynchronous recognition of celebrities in a stored video.</p> <p>Rekognition Video can detect celebrities in a video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartCelebrityRecognition</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When celebrity recognition analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityRecognition</code>. For more information, see <a>celebrities</a>.</p>
startCelebrityRecognition :: forall eff. StartCelebrityRecognitionRequest -> Aff (err :: AWS.RequestError | eff) StartCelebrityRecognitionResponse
startCelebrityRecognition = AWS.request serviceName "startCelebrityRecognition" 


-- | <p> Starts asynchronous detection of explicit or suggestive adult content in a stored video.</p> <p>Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartContentModeration</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When content moderation analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartContentModeration</code>. For more information, see <a>moderation</a>.</p>
startContentModeration :: forall eff. StartContentModerationRequest -> Aff (err :: AWS.RequestError | eff) StartContentModerationResponse
startContentModeration = AWS.request serviceName "startContentModeration" 


-- | <p>Starts asynchronous detection of faces in a stored video.</p> <p>Rekognition Video can detect faces in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceDetection</code> returns a job identifier (<code>JobId</code>) that you use to get the results of the operation. When face detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>. For more information, see <a>faces-video</a>.</p>
startFaceDetection :: forall eff. StartFaceDetectionRequest -> Aff (err :: AWS.RequestError | eff) StartFaceDetectionResponse
startFaceDetection = AWS.request serviceName "startFaceDetection" 


-- | <p>Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceSearch</code> returns a job identifier (<code>JobId</code>) which you use to get the search results once the search has completed. When searching is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections-search-person</a>.</p>
startFaceSearch :: forall eff. StartFaceSearchRequest -> Aff (err :: AWS.RequestError | eff) StartFaceSearchResponse
startFaceSearch = AWS.request serviceName "startFaceSearch" 


-- | <p>Starts asynchronous detection of labels in a stored video.</p> <p>Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartLabelDetection</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p/>
startLabelDetection :: forall eff. StartLabelDetectionRequest -> Aff (err :: AWS.RequestError | eff) StartLabelDetectionResponse
startLabelDetection = AWS.request serviceName "startLabelDetection" 


-- | <p>Starts the asynchronous tracking of persons in a stored video.</p> <p>Rekognition Video can track persons in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartPersonTracking</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. </p> <p>To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p>
startPersonTracking :: forall eff. StartPersonTrackingRequest -> Aff (err :: AWS.RequestError | eff) StartPersonTrackingResponse
startPersonTracking = AWS.request serviceName "startPersonTracking" 


-- | <p>Starts processing a stream processor. You create a stream processor by calling . To tell <code>StartStreamProcessor</code> which stream processor to start, use the value of the <code>Name</code> field specified in the call to <code>CreateStreamProcessor</code>.</p>
startStreamProcessor :: forall eff. StartStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) StartStreamProcessorResponse
startStreamProcessor = AWS.request serviceName "startStreamProcessor" 


-- | <p>Stops a running stream processor that was created by .</p>
stopStreamProcessor :: forall eff. StopStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) StopStreamProcessorResponse
stopStreamProcessor = AWS.request serviceName "stopStreamProcessor" 


-- | <p>You are not authorized to perform the action.</p>
newtype AccessDeniedException = AccessDeniedException 
  { 
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


-- | <p>Structure containing the estimated age range, in years, for a face.</p> <p>Rekognition estimates an age-range for faces detected in the input image. Estimated age ranges can overlap; a face of a 5 year old may have an estimated range of 4-6 whilst the face of a 6 year old may have an estimated range of 4-8.</p>
newtype AgeRange = AgeRange 
  { "Low" :: NullOrUndefined (UInteger)
  , "High" :: NullOrUndefined (UInteger)
  }
derive instance newtypeAgeRange :: Newtype AgeRange _


newtype Attribute = Attribute String
derive instance newtypeAttribute :: Newtype Attribute _


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _


-- | <p>Indicates whether or not the face has a beard, and the confidence level in the determination.</p>
newtype Beard = Beard 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeBeard :: Newtype Beard _


-- | <p>Identifies the bounding box around the object, face or text. The <code>left</code> (x-coordinate) and <code>top</code> (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0). </p> <p>The <code>top</code> and <code>left</code> values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a <code>left</code> value of 0.5 (350/700) and a <code>top</code> value of 0.25 (50/200).</p> <p>The <code>width</code> and <code>height</code> values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1. </p> <note> <p> The bounding box coordinates can have negative values. For example, if Amazon Rekognition is able to detect a face that is at the image edge and is only partially visible, the service can return coordinates that are outside the image bounds and, depending on the image edge, you might get negative values or values greater than 1 for the <code>left</code> or <code>top</code> values. </p> </note>
newtype BoundingBox = BoundingBox 
  { "Width" :: NullOrUndefined (Number)
  , "Height" :: NullOrUndefined (Number)
  , "Left" :: NullOrUndefined (Number)
  , "Top" :: NullOrUndefined (Number)
  }
derive instance newtypeBoundingBox :: Newtype BoundingBox _


-- | <p>Provides information about a celebrity recognized by the operation.</p>
newtype Celebrity = Celebrity 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (RekognitionUniqueId)
  , "Face" :: NullOrUndefined (ComparedFace)
  , "MatchConfidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeCelebrity :: Newtype Celebrity _


-- | <p>Information about a recognized celebrity.</p>
newtype CelebrityDetail = CelebrityDetail 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (RekognitionUniqueId)
  , "Confidence" :: NullOrUndefined (Percent)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined (FaceDetail)
  }
derive instance newtypeCelebrityDetail :: Newtype CelebrityDetail _


newtype CelebrityList = CelebrityList (Array Celebrity)
derive instance newtypeCelebrityList :: Newtype CelebrityList _


-- | <p>Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see .</p>
newtype CelebrityRecognition = CelebrityRecognition 
  { "Number" :: NullOrUndefined (Number)
  , "Celebrity" :: NullOrUndefined (CelebrityDetail)
  }
derive instance newtypeCelebrityRecognition :: Newtype CelebrityRecognition _


newtype CelebrityRecognitionSortBy = CelebrityRecognitionSortBy String
derive instance newtypeCelebrityRecognitionSortBy :: Newtype CelebrityRecognitionSortBy _


newtype CelebrityRecognitions = CelebrityRecognitions (Array CelebrityRecognition)
derive instance newtypeCelebrityRecognitions :: Newtype CelebrityRecognitions _


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _


newtype CollectionId = CollectionId String
derive instance newtypeCollectionId :: Newtype CollectionId _


newtype CollectionIdList = CollectionIdList (Array CollectionId)
derive instance newtypeCollectionIdList :: Newtype CollectionIdList _


-- | <p>Provides information about a face in a target image that matches the source image face analysed by <code>CompareFaces</code>. The <code>Face</code> property contains the bounding box of the face in the target image. The <code>Similarity</code> property is the confidence that the source image face matches the face in the bounding box.</p>
newtype CompareFacesMatch = CompareFacesMatch 
  { "Similarity" :: NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined (ComparedFace)
  }
derive instance newtypeCompareFacesMatch :: Newtype CompareFacesMatch _


newtype CompareFacesMatchList = CompareFacesMatchList (Array CompareFacesMatch)
derive instance newtypeCompareFacesMatchList :: Newtype CompareFacesMatchList _


newtype CompareFacesRequest = CompareFacesRequest 
  { "SourceImage" :: (Image)
  , "TargetImage" :: (Image)
  , "SimilarityThreshold" :: NullOrUndefined (Percent)
  }
derive instance newtypeCompareFacesRequest :: Newtype CompareFacesRequest _


newtype CompareFacesResponse = CompareFacesResponse 
  { "SourceImageFace" :: NullOrUndefined (ComparedSourceImageFace)
  , "FaceMatches" :: NullOrUndefined (CompareFacesMatchList)
  , "UnmatchedFaces" :: NullOrUndefined (CompareFacesUnmatchList)
  , "SourceImageOrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  , "TargetImageOrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeCompareFacesResponse :: Newtype CompareFacesResponse _


newtype CompareFacesUnmatchList = CompareFacesUnmatchList (Array ComparedFace)
derive instance newtypeCompareFacesUnmatchList :: Newtype CompareFacesUnmatchList _


-- | <p>Provides face metadata for target image faces that are analysed by <code>CompareFaces</code> and <code>RecognizeCelebrities</code>.</p>
newtype ComparedFace = ComparedFace 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined (Percent)
  , "Landmarks" :: NullOrUndefined (Landmarks)
  , "Pose" :: NullOrUndefined (Pose)
  , "Quality" :: NullOrUndefined (ImageQuality)
  }
derive instance newtypeComparedFace :: Newtype ComparedFace _


newtype ComparedFaceList = ComparedFaceList (Array ComparedFace)
derive instance newtypeComparedFaceList :: Newtype ComparedFaceList _


-- | <p>Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison. </p>
newtype ComparedSourceImageFace = ComparedSourceImageFace 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeComparedSourceImageFace :: Newtype ComparedSourceImageFace _


-- | <p>Information about a moderation label detection in a stored video.</p>
newtype ContentModerationDetection = ContentModerationDetection 
  { "Number" :: NullOrUndefined (Number)
  , "ModerationLabel" :: NullOrUndefined (ModerationLabel)
  }
derive instance newtypeContentModerationDetection :: Newtype ContentModerationDetection _


newtype ContentModerationDetections = ContentModerationDetections (Array ContentModerationDetection)
derive instance newtypeContentModerationDetections :: Newtype ContentModerationDetections _


newtype ContentModerationSortBy = ContentModerationSortBy String
derive instance newtypeContentModerationSortBy :: Newtype ContentModerationSortBy _


newtype CreateCollectionRequest = CreateCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }
derive instance newtypeCreateCollectionRequest :: Newtype CreateCollectionRequest _


newtype CreateCollectionResponse = CreateCollectionResponse 
  { "StatusCode" :: NullOrUndefined (UInteger)
  , "CollectionArn" :: NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }
derive instance newtypeCreateCollectionResponse :: Newtype CreateCollectionResponse _


newtype CreateStreamProcessorRequest = CreateStreamProcessorRequest 
  { "Input" :: (StreamProcessorInput)
  , "Output" :: (StreamProcessorOutput)
  , "Name" :: (StreamProcessorName)
  , "Settings" :: (StreamProcessorSettings)
  , "RoleArn" :: (RoleArn)
  }
derive instance newtypeCreateStreamProcessorRequest :: Newtype CreateStreamProcessorRequest _


newtype CreateStreamProcessorResponse = CreateStreamProcessorResponse 
  { "StreamProcessorArn" :: NullOrUndefined (StreamProcessorArn)
  }
derive instance newtypeCreateStreamProcessorResponse :: Newtype CreateStreamProcessorResponse _


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _


newtype Degree = Degree Number
derive instance newtypeDegree :: Newtype Degree _


newtype DeleteCollectionRequest = DeleteCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }
derive instance newtypeDeleteCollectionRequest :: Newtype DeleteCollectionRequest _


newtype DeleteCollectionResponse = DeleteCollectionResponse 
  { "StatusCode" :: NullOrUndefined (UInteger)
  }
derive instance newtypeDeleteCollectionResponse :: Newtype DeleteCollectionResponse _


newtype DeleteFacesRequest = DeleteFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceIds" :: (FaceIdList)
  }
derive instance newtypeDeleteFacesRequest :: Newtype DeleteFacesRequest _


newtype DeleteFacesResponse = DeleteFacesResponse 
  { "DeletedFaces" :: NullOrUndefined (FaceIdList)
  }
derive instance newtypeDeleteFacesResponse :: Newtype DeleteFacesResponse _


newtype DeleteStreamProcessorRequest = DeleteStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeDeleteStreamProcessorRequest :: Newtype DeleteStreamProcessorRequest _


newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse 
  { 
  }
derive instance newtypeDeleteStreamProcessorResponse :: Newtype DeleteStreamProcessorResponse _


newtype DescribeStreamProcessorRequest = DescribeStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeDescribeStreamProcessorRequest :: Newtype DescribeStreamProcessorRequest _


newtype DescribeStreamProcessorResponse = DescribeStreamProcessorResponse 
  { "Name" :: NullOrUndefined (StreamProcessorName)
  , "StreamProcessorArn" :: NullOrUndefined (StreamProcessorArn)
  , "Status" :: NullOrUndefined (StreamProcessorStatus)
  , "StatusMessage" :: NullOrUndefined (String)
  , "CreationTimestamp" :: NullOrUndefined (DateTime)
  , "LastUpdateTimestamp" :: NullOrUndefined (DateTime)
  , "Input" :: NullOrUndefined (StreamProcessorInput)
  , "Output" :: NullOrUndefined (StreamProcessorOutput)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  , "Settings" :: NullOrUndefined (StreamProcessorSettings)
  }
derive instance newtypeDescribeStreamProcessorResponse :: Newtype DescribeStreamProcessorResponse _


newtype DetectFacesRequest = DetectFacesRequest 
  { "Image" :: (Image)
  , "Attributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeDetectFacesRequest :: Newtype DetectFacesRequest _


newtype DetectFacesResponse = DetectFacesResponse 
  { "FaceDetails" :: NullOrUndefined (FaceDetailList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeDetectFacesResponse :: Newtype DetectFacesResponse _


newtype DetectLabelsRequest = DetectLabelsRequest 
  { "Image" :: (Image)
  , "MaxLabels" :: NullOrUndefined (UInteger)
  , "MinConfidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeDetectLabelsRequest :: Newtype DetectLabelsRequest _


newtype DetectLabelsResponse = DetectLabelsResponse 
  { "Labels" :: NullOrUndefined (Labels)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeDetectLabelsResponse :: Newtype DetectLabelsResponse _


newtype DetectModerationLabelsRequest = DetectModerationLabelsRequest 
  { "Image" :: (Image)
  , "MinConfidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeDetectModerationLabelsRequest :: Newtype DetectModerationLabelsRequest _


newtype DetectModerationLabelsResponse = DetectModerationLabelsResponse 
  { "ModerationLabels" :: NullOrUndefined (ModerationLabels)
  }
derive instance newtypeDetectModerationLabelsResponse :: Newtype DetectModerationLabelsResponse _


newtype DetectTextRequest = DetectTextRequest 
  { "Image" :: (Image)
  }
derive instance newtypeDetectTextRequest :: Newtype DetectTextRequest _


newtype DetectTextResponse = DetectTextResponse 
  { "TextDetections" :: NullOrUndefined (TextDetectionList)
  }
derive instance newtypeDetectTextResponse :: Newtype DetectTextResponse _


-- | <p>The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.</p>
newtype Emotion = Emotion 
  { "Type" :: NullOrUndefined (EmotionName)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeEmotion :: Newtype Emotion _


newtype EmotionName = EmotionName String
derive instance newtypeEmotionName :: Newtype EmotionName _


newtype Emotions = Emotions (Array Emotion)
derive instance newtypeEmotions :: Newtype Emotions _


newtype ExternalImageId = ExternalImageId String
derive instance newtypeExternalImageId :: Newtype ExternalImageId _


-- | <p>Indicates whether or not the eyes on the face are open, and the confidence level in the determination.</p>
newtype EyeOpen = EyeOpen 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeEyeOpen :: Newtype EyeOpen _


-- | <p>Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.</p>
newtype Eyeglasses = Eyeglasses 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeEyeglasses :: Newtype Eyeglasses _


-- | <p>Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned. </p>
newtype Face = Face 
  { "FaceId" :: NullOrUndefined (FaceId)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "ImageId" :: NullOrUndefined (ImageId)
  , "ExternalImageId" :: NullOrUndefined (ExternalImageId)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeFace :: Newtype Face _


newtype FaceAttributes = FaceAttributes String
derive instance newtypeFaceAttributes :: Newtype FaceAttributes _


-- | <p>Structure containing attributes of the face that the algorithm detected.</p>
newtype FaceDetail = FaceDetail 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "AgeRange" :: NullOrUndefined (AgeRange)
  , "Smile" :: NullOrUndefined (Smile)
  , "Eyeglasses" :: NullOrUndefined (Eyeglasses)
  , "Sunglasses" :: NullOrUndefined (Sunglasses)
  , "Gender" :: NullOrUndefined (Gender)
  , "Beard" :: NullOrUndefined (Beard)
  , "Mustache" :: NullOrUndefined (Mustache)
  , "EyesOpen" :: NullOrUndefined (EyeOpen)
  , "MouthOpen" :: NullOrUndefined (MouthOpen)
  , "Emotions" :: NullOrUndefined (Emotions)
  , "Landmarks" :: NullOrUndefined (Landmarks)
  , "Pose" :: NullOrUndefined (Pose)
  , "Quality" :: NullOrUndefined (ImageQuality)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeFaceDetail :: Newtype FaceDetail _


newtype FaceDetailList = FaceDetailList (Array FaceDetail)
derive instance newtypeFaceDetailList :: Newtype FaceDetailList _


-- | <p>Information about a face detected in a video analysis request and the time the face was detected in the video. </p>
newtype FaceDetection = FaceDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Face" :: NullOrUndefined (FaceDetail)
  }
derive instance newtypeFaceDetection :: Newtype FaceDetection _


newtype FaceDetections = FaceDetections (Array FaceDetection)
derive instance newtypeFaceDetections :: Newtype FaceDetections _


newtype FaceId = FaceId String
derive instance newtypeFaceId :: Newtype FaceId _


newtype FaceIdList = FaceIdList (Array FaceId)
derive instance newtypeFaceIdList :: Newtype FaceIdList _


newtype FaceList = FaceList (Array Face)
derive instance newtypeFaceList :: Newtype FaceList _


-- | <p>Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.</p>
newtype FaceMatch = FaceMatch 
  { "Similarity" :: NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined (Face)
  }
derive instance newtypeFaceMatch :: Newtype FaceMatch _


newtype FaceMatchList = FaceMatchList (Array FaceMatch)
derive instance newtypeFaceMatchList :: Newtype FaceMatchList _


newtype FaceModelVersionList = FaceModelVersionList (Array String)
derive instance newtypeFaceModelVersionList :: Newtype FaceModelVersionList _


-- | <p>Object containing both the face metadata (stored in the back-end database) and facial attributes that are detected but aren't stored in the database.</p>
newtype FaceRecord = FaceRecord 
  { "Face" :: NullOrUndefined (Face)
  , "FaceDetail" :: NullOrUndefined (FaceDetail)
  }
derive instance newtypeFaceRecord :: Newtype FaceRecord _


newtype FaceRecordList = FaceRecordList (Array FaceRecord)
derive instance newtypeFaceRecordList :: Newtype FaceRecordList _


-- | <p>Input face recognition parameters for an Amazon Rekognition stream processor. <code>FaceRecognitionSettings</code> is a request parameter for .</p>
newtype FaceSearchSettings = FaceSearchSettings 
  { "CollectionId" :: NullOrUndefined (CollectionId)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }
derive instance newtypeFaceSearchSettings :: Newtype FaceSearchSettings _


newtype FaceSearchSortBy = FaceSearchSortBy String
derive instance newtypeFaceSearchSortBy :: Newtype FaceSearchSortBy _


-- | <p>Gender of the face and the confidence level in the determination.</p>
newtype Gender = Gender 
  { "Value" :: NullOrUndefined (GenderType)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeGender :: Newtype Gender _


newtype GenderType = GenderType String
derive instance newtypeGenderType :: Newtype GenderType _


-- | <p>Information about where text detected by is located on an image.</p>
newtype Geometry = Geometry 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Polygon" :: NullOrUndefined (Polygon)
  }
derive instance newtypeGeometry :: Newtype Geometry _


newtype GetCelebrityInfoRequest = GetCelebrityInfoRequest 
  { "Id" :: (RekognitionUniqueId)
  }
derive instance newtypeGetCelebrityInfoRequest :: Newtype GetCelebrityInfoRequest _


newtype GetCelebrityInfoResponse = GetCelebrityInfoResponse 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeGetCelebrityInfoResponse :: Newtype GetCelebrityInfoResponse _


newtype GetCelebrityRecognitionRequest = GetCelebrityRecognitionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (CelebrityRecognitionSortBy)
  }
derive instance newtypeGetCelebrityRecognitionRequest :: Newtype GetCelebrityRecognitionRequest _


newtype GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Celebrities" :: NullOrUndefined (CelebrityRecognitions)
  }
derive instance newtypeGetCelebrityRecognitionResponse :: Newtype GetCelebrityRecognitionResponse _


newtype GetContentModerationRequest = GetContentModerationRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (ContentModerationSortBy)
  }
derive instance newtypeGetContentModerationRequest :: Newtype GetContentModerationRequest _


newtype GetContentModerationResponse = GetContentModerationResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "ModerationLabels" :: NullOrUndefined (ContentModerationDetections)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetContentModerationResponse :: Newtype GetContentModerationResponse _


newtype GetFaceDetectionRequest = GetFaceDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeGetFaceDetectionRequest :: Newtype GetFaceDetectionRequest _


newtype GetFaceDetectionResponse = GetFaceDetectionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Faces" :: NullOrUndefined (FaceDetections)
  }
derive instance newtypeGetFaceDetectionResponse :: Newtype GetFaceDetectionResponse _


newtype GetFaceSearchRequest = GetFaceSearchRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (FaceSearchSortBy)
  }
derive instance newtypeGetFaceSearchRequest :: Newtype GetFaceSearchRequest _


newtype GetFaceSearchResponse = GetFaceSearchResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "Persons" :: NullOrUndefined (PersonMatches)
  }
derive instance newtypeGetFaceSearchResponse :: Newtype GetFaceSearchResponse _


newtype GetLabelDetectionRequest = GetLabelDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (LabelDetectionSortBy)
  }
derive instance newtypeGetLabelDetectionRequest :: Newtype GetLabelDetectionRequest _


newtype GetLabelDetectionResponse = GetLabelDetectionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Labels" :: NullOrUndefined (LabelDetections)
  }
derive instance newtypeGetLabelDetectionResponse :: Newtype GetLabelDetectionResponse _


newtype GetPersonTrackingRequest = GetPersonTrackingRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (PersonTrackingSortBy)
  }
derive instance newtypeGetPersonTrackingRequest :: Newtype GetPersonTrackingRequest _


newtype GetPersonTrackingResponse = GetPersonTrackingResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Persons" :: NullOrUndefined (PersonDetections)
  }
derive instance newtypeGetPersonTrackingResponse :: Newtype GetPersonTrackingResponse _


-- | <p>A <code>ClientRequestToken</code> input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { 
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _


-- | <p>Provides the input image either as bytes or an S3 object.</p> <p>You pass image bytes to a Rekognition API operation by using the <code>Bytes</code> property. For example, you would use the <code>Bytes</code> property to pass an image loaded from a local file system. Image bytes passed by using the <code>Bytes</code> property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Rekognition API operations. For more information, see <a>images-bytes</a>.</p> <p> You pass images stored in an S3 bucket to a Rekognition API operation by using the <code>S3Object</code> property. Images stored in an S3 bucket do not need to be base64-encoded.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype Image = Image 
  { "Bytes" :: NullOrUndefined (ImageBlob)
  , "S3Object" :: NullOrUndefined (S3Object)
  }
derive instance newtypeImage :: Newtype Image _


newtype ImageBlob = ImageBlob String
derive instance newtypeImageBlob :: Newtype ImageBlob _


newtype ImageId = ImageId String
derive instance newtypeImageId :: Newtype ImageId _


-- | <p>Identifies face image brightness and sharpness. </p>
newtype ImageQuality = ImageQuality 
  { "Brightness" :: NullOrUndefined (Number)
  , "Sharpness" :: NullOrUndefined (Number)
  }
derive instance newtypeImageQuality :: Newtype ImageQuality _


-- | <p>The input image size exceeds the allowed limit. For more information, see <a>limits</a>. </p>
newtype ImageTooLargeException = ImageTooLargeException 
  { 
  }
derive instance newtypeImageTooLargeException :: Newtype ImageTooLargeException _


newtype IndexFacesRequest = IndexFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "ExternalImageId" :: NullOrUndefined (ExternalImageId)
  , "DetectionAttributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeIndexFacesRequest :: Newtype IndexFacesRequest _


newtype IndexFacesResponse = IndexFacesResponse 
  { "FaceRecords" :: NullOrUndefined (FaceRecordList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }
derive instance newtypeIndexFacesResponse :: Newtype IndexFacesResponse _


-- | <p>Amazon Rekognition experienced a service issue. Try your call again.</p>
newtype InternalServerError = InternalServerError 
  { 
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>The provided image format is not supported. </p>
newtype InvalidImageFormatException = InvalidImageFormatException 
  { 
  }
derive instance newtypeInvalidImageFormatException :: Newtype InvalidImageFormatException _


-- | <p>Pagination token in the request is not valid.</p>
newtype InvalidPaginationTokenException = InvalidPaginationTokenException 
  { 
  }
derive instance newtypeInvalidPaginationTokenException :: Newtype InvalidPaginationTokenException _


-- | <p>Input parameter violated a constraint. Validate your parameter before calling the API operation again.</p>
newtype InvalidParameterException = InvalidParameterException 
  { 
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>Amazon Rekognition is unable to access the S3 object specified in the request.</p>
newtype InvalidS3ObjectException = InvalidS3ObjectException 
  { 
  }
derive instance newtypeInvalidS3ObjectException :: Newtype InvalidS3ObjectException _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


newtype JobTag = JobTag String
derive instance newtypeJobTag :: Newtype JobTag _


newtype KinesisDataArn = KinesisDataArn String
derive instance newtypeKinesisDataArn :: Newtype KinesisDataArn _


-- | <p>The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see .</p>
newtype KinesisDataStream = KinesisDataStream 
  { "Arn" :: NullOrUndefined (KinesisDataArn)
  }
derive instance newtypeKinesisDataStream :: Newtype KinesisDataStream _


newtype KinesisVideoArn = KinesisVideoArn String
derive instance newtypeKinesisVideoArn :: Newtype KinesisVideoArn _


-- | <p>Kinesis video stream stream that provides the source streaming video for a Rekognition Video stream processor. For more information, see .</p>
newtype KinesisVideoStream = KinesisVideoStream 
  { "Arn" :: NullOrUndefined (KinesisVideoArn)
  }
derive instance newtypeKinesisVideoStream :: Newtype KinesisVideoStream _


-- | <p>Structure containing details about the detected label, including name, and level of confidence.</p>
newtype Label = Label 
  { "Name" :: NullOrUndefined (String)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeLabel :: Newtype Label _


-- | <p>Information about a label detected in a video analysis request and the time the label was detected in the video. </p>
newtype LabelDetection = LabelDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Label" :: NullOrUndefined (Label)
  }
derive instance newtypeLabelDetection :: Newtype LabelDetection _


newtype LabelDetectionSortBy = LabelDetectionSortBy String
derive instance newtypeLabelDetectionSortBy :: Newtype LabelDetectionSortBy _


newtype LabelDetections = LabelDetections (Array LabelDetection)
derive instance newtypeLabelDetections :: Newtype LabelDetections _


newtype Labels = Labels (Array Label)
derive instance newtypeLabels :: Newtype Labels _


-- | <p>Indicates the location of the landmark on the face.</p>
newtype Landmark = Landmark 
  { "Type" :: NullOrUndefined (LandmarkType)
  , "X" :: NullOrUndefined (Number)
  , "Y" :: NullOrUndefined (Number)
  }
derive instance newtypeLandmark :: Newtype Landmark _


newtype LandmarkType = LandmarkType String
derive instance newtypeLandmarkType :: Newtype LandmarkType _


newtype Landmarks = Landmarks (Array Landmark)
derive instance newtypeLandmarks :: Newtype Landmarks _


-- | <p/>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListCollectionsRequest = ListCollectionsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListCollectionsRequest :: Newtype ListCollectionsRequest _


newtype ListCollectionsResponse = ListCollectionsResponse 
  { "CollectionIds" :: NullOrUndefined (CollectionIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "FaceModelVersions" :: NullOrUndefined (FaceModelVersionList)
  }
derive instance newtypeListCollectionsResponse :: Newtype ListCollectionsResponse _


newtype ListFacesRequest = ListFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeListFacesRequest :: Newtype ListFacesRequest _


newtype ListFacesResponse = ListFacesResponse 
  { "Faces" :: NullOrUndefined (FaceList)
  , "NextToken" :: NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }
derive instance newtypeListFacesResponse :: Newtype ListFacesResponse _


newtype ListStreamProcessorsRequest = ListStreamProcessorsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListStreamProcessorsRequest :: Newtype ListStreamProcessorsRequest _


newtype ListStreamProcessorsResponse = ListStreamProcessorsResponse 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "StreamProcessors" :: NullOrUndefined (StreamProcessorList)
  }
derive instance newtypeListStreamProcessorsResponse :: Newtype ListStreamProcessorsResponse _


newtype MaxFaces = MaxFaces Int
derive instance newtypeMaxFaces :: Newtype MaxFaces _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>Provides information about a single type of moderated content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see <a>moderation</a>.</p>
newtype ModerationLabel = ModerationLabel 
  { "Confidence" :: NullOrUndefined (Percent)
  , "Name" :: NullOrUndefined (String)
  , "ParentName" :: NullOrUndefined (String)
  }
derive instance newtypeModerationLabel :: Newtype ModerationLabel _


newtype ModerationLabels = ModerationLabels (Array ModerationLabel)
derive instance newtypeModerationLabels :: Newtype ModerationLabels _


-- | <p>Indicates whether or not the mouth on the face is open, and the confidence level in the determination.</p>
newtype MouthOpen = MouthOpen 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeMouthOpen :: Newtype MouthOpen _


-- | <p>Indicates whether or not the face has a mustache, and the confidence level in the determination.</p>
newtype Mustache = Mustache 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeMustache :: Newtype Mustache _


-- | <p>The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see <a>api-video</a>.</p>
newtype NotificationChannel = NotificationChannel 
  { "SNSTopicArn" :: (SNSTopicArn)
  , "RoleArn" :: (RoleArn)
  }
derive instance newtypeNotificationChannel :: Newtype NotificationChannel _


newtype OrientationCorrection = OrientationCorrection String
derive instance newtypeOrientationCorrection :: Newtype OrientationCorrection _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


newtype Percent = Percent Number
derive instance newtypePercent :: Newtype Percent _


-- | <p>Details about a person detected in a video analysis request.</p>
newtype PersonDetail = PersonDetail 
  { "Index" :: NullOrUndefined (PersonIndex)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined (FaceDetail)
  }
derive instance newtypePersonDetail :: Newtype PersonDetail _


-- | <p>Details and tracking information for a single time a person is tracked in a video. Amazon Rekognition operations that track persons return an array of <code>PersonDetection</code> objects with elements for each time a person is tracked in a video. For more information, see . </p>
newtype PersonDetection = PersonDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Person" :: NullOrUndefined (PersonDetail)
  }
derive instance newtypePersonDetection :: Newtype PersonDetection _


newtype PersonDetections = PersonDetections (Array PersonDetection)
derive instance newtypePersonDetections :: Newtype PersonDetections _


newtype PersonIndex = PersonIndex Number
derive instance newtypePersonIndex :: Newtype PersonIndex _


-- | <p>Information about a person whose face matches a face(s) in a Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection (,information about the person (<a>PersonDetail</a>) and the timestamp for when the person was detected in a video. An array of <code>PersonMatch</code> objects is returned by . </p>
newtype PersonMatch = PersonMatch 
  { "Number" :: NullOrUndefined (Number)
  , "Person" :: NullOrUndefined (PersonDetail)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  }
derive instance newtypePersonMatch :: Newtype PersonMatch _


newtype PersonMatches = PersonMatches (Array PersonMatch)
derive instance newtypePersonMatches :: Newtype PersonMatches _


newtype PersonTrackingSortBy = PersonTrackingSortBy String
derive instance newtypePersonTrackingSortBy :: Newtype PersonTrackingSortBy _


-- | <p>The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.</p> <p>An array of <code>Point</code> objects, <code>Polygon</code>, is returned by . <code>Polygon</code> represents a fine-grained polygon around detected text. For more information, see . </p>
newtype Point = Point 
  { "X" :: NullOrUndefined (Number)
  , "Y" :: NullOrUndefined (Number)
  }
derive instance newtypePoint :: Newtype Point _


newtype Polygon = Polygon (Array Point)
derive instance newtypePolygon :: Newtype Polygon _


-- | <p>Indicates the pose of the face as determined by its pitch, roll, and yaw.</p>
newtype Pose = Pose 
  { "Roll" :: NullOrUndefined (Degree)
  , "Yaw" :: NullOrUndefined (Degree)
  , "Pitch" :: NullOrUndefined (Degree)
  }
derive instance newtypePose :: Newtype Pose _


-- | <p>The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.</p>
newtype ProvisionedThroughputExceededException = ProvisionedThroughputExceededException 
  { 
  }
derive instance newtypeProvisionedThroughputExceededException :: Newtype ProvisionedThroughputExceededException _


newtype RecognizeCelebritiesRequest = RecognizeCelebritiesRequest 
  { "Image" :: (Image)
  }
derive instance newtypeRecognizeCelebritiesRequest :: Newtype RecognizeCelebritiesRequest _


newtype RecognizeCelebritiesResponse = RecognizeCelebritiesResponse 
  { "CelebrityFaces" :: NullOrUndefined (CelebrityList)
  , "UnrecognizedFaces" :: NullOrUndefined (ComparedFaceList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }
derive instance newtypeRecognizeCelebritiesResponse :: Newtype RecognizeCelebritiesResponse _


newtype RekognitionUniqueId = RekognitionUniqueId String
derive instance newtypeRekognitionUniqueId :: Newtype RekognitionUniqueId _


-- | <p>A collection with the specified ID already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


-- | <p/>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The collection specified in the request cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _


-- | <p>Provides the S3 bucket name and object name.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype S3Object = S3Object 
  { "Bucket" :: NullOrUndefined (S3Bucket)
  , "Name" :: NullOrUndefined (S3ObjectName)
  , "Version" :: NullOrUndefined (S3ObjectVersion)
  }
derive instance newtypeS3Object :: Newtype S3Object _


newtype S3ObjectName = S3ObjectName String
derive instance newtypeS3ObjectName :: Newtype S3ObjectName _


newtype S3ObjectVersion = S3ObjectVersion String
derive instance newtypeS3ObjectVersion :: Newtype S3ObjectVersion _


newtype SNSTopicArn = SNSTopicArn String
derive instance newtypeSNSTopicArn :: Newtype SNSTopicArn _


newtype SearchFacesByImageRequest = SearchFacesByImageRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "MaxFaces" :: NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }
derive instance newtypeSearchFacesByImageRequest :: Newtype SearchFacesByImageRequest _


newtype SearchFacesByImageResponse = SearchFacesByImageResponse 
  { "SearchedFaceBoundingBox" :: NullOrUndefined (BoundingBox)
  , "SearchedFaceConfidence" :: NullOrUndefined (Percent)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }
derive instance newtypeSearchFacesByImageResponse :: Newtype SearchFacesByImageResponse _


newtype SearchFacesRequest = SearchFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceId" :: (FaceId)
  , "MaxFaces" :: NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }
derive instance newtypeSearchFacesRequest :: Newtype SearchFacesRequest _


newtype SearchFacesResponse = SearchFacesResponse 
  { "SearchedFaceId" :: NullOrUndefined (FaceId)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }
derive instance newtypeSearchFacesResponse :: Newtype SearchFacesResponse _


-- | <p>Indicates whether or not the face is smiling, and the confidence level in the determination.</p>
newtype Smile = Smile 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeSmile :: Newtype Smile _


newtype StartCelebrityRecognitionRequest = StartCelebrityRecognitionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartCelebrityRecognitionRequest :: Newtype StartCelebrityRecognitionRequest _


newtype StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartCelebrityRecognitionResponse :: Newtype StartCelebrityRecognitionResponse _


newtype StartContentModerationRequest = StartContentModerationRequest 
  { "Video" :: (Video)
  , "MinConfidence" :: NullOrUndefined (Percent)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartContentModerationRequest :: Newtype StartContentModerationRequest _


newtype StartContentModerationResponse = StartContentModerationResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartContentModerationResponse :: Newtype StartContentModerationResponse _


newtype StartFaceDetectionRequest = StartFaceDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "FaceAttributes" :: NullOrUndefined (FaceAttributes)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartFaceDetectionRequest :: Newtype StartFaceDetectionRequest _


newtype StartFaceDetectionResponse = StartFaceDetectionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartFaceDetectionResponse :: Newtype StartFaceDetectionResponse _


newtype StartFaceSearchRequest = StartFaceSearchRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  , "CollectionId" :: (CollectionId)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartFaceSearchRequest :: Newtype StartFaceSearchRequest _


newtype StartFaceSearchResponse = StartFaceSearchResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartFaceSearchResponse :: Newtype StartFaceSearchResponse _


newtype StartLabelDetectionRequest = StartLabelDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "MinConfidence" :: NullOrUndefined (Percent)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartLabelDetectionRequest :: Newtype StartLabelDetectionRequest _


newtype StartLabelDetectionResponse = StartLabelDetectionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartLabelDetectionResponse :: Newtype StartLabelDetectionResponse _


newtype StartPersonTrackingRequest = StartPersonTrackingRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }
derive instance newtypeStartPersonTrackingRequest :: Newtype StartPersonTrackingRequest _


newtype StartPersonTrackingResponse = StartPersonTrackingResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }
derive instance newtypeStartPersonTrackingResponse :: Newtype StartPersonTrackingResponse _


newtype StartStreamProcessorRequest = StartStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeStartStreamProcessorRequest :: Newtype StartStreamProcessorRequest _


newtype StartStreamProcessorResponse = StartStreamProcessorResponse 
  { 
  }
derive instance newtypeStartStreamProcessorResponse :: Newtype StartStreamProcessorResponse _


newtype StatusMessage = StatusMessage String
derive instance newtypeStatusMessage :: Newtype StatusMessage _


newtype StopStreamProcessorRequest = StopStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }
derive instance newtypeStopStreamProcessorRequest :: Newtype StopStreamProcessorRequest _


newtype StopStreamProcessorResponse = StopStreamProcessorResponse 
  { 
  }
derive instance newtypeStopStreamProcessorResponse :: Newtype StopStreamProcessorResponse _


-- | <p>An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to . The request parameters for <code>CreateStreamProcessor</code> describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts. </p>
newtype StreamProcessor = StreamProcessor 
  { "Name" :: NullOrUndefined (StreamProcessorName)
  , "Status" :: NullOrUndefined (StreamProcessorStatus)
  }
derive instance newtypeStreamProcessor :: Newtype StreamProcessor _


newtype StreamProcessorArn = StreamProcessorArn String
derive instance newtypeStreamProcessorArn :: Newtype StreamProcessorArn _


-- | <p>Information about the source streaming video. </p>
newtype StreamProcessorInput = StreamProcessorInput 
  { "KinesisVideoStream" :: NullOrUndefined (KinesisVideoStream)
  }
derive instance newtypeStreamProcessorInput :: Newtype StreamProcessorInput _


newtype StreamProcessorList = StreamProcessorList (Array StreamProcessor)
derive instance newtypeStreamProcessorList :: Newtype StreamProcessorList _


newtype StreamProcessorName = StreamProcessorName String
derive instance newtypeStreamProcessorName :: Newtype StreamProcessorName _


-- | <p>Information about the Amazon Kinesis Data Streams stream to which a Rekognition Video stream processor streams the results of a video analysis. For more information, see .</p>
newtype StreamProcessorOutput = StreamProcessorOutput 
  { "KinesisDataStream" :: NullOrUndefined (KinesisDataStream)
  }
derive instance newtypeStreamProcessorOutput :: Newtype StreamProcessorOutput _


-- | <p>Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.</p>
newtype StreamProcessorSettings = StreamProcessorSettings 
  { "FaceSearch" :: NullOrUndefined (FaceSearchSettings)
  }
derive instance newtypeStreamProcessorSettings :: Newtype StreamProcessorSettings _


newtype StreamProcessorStatus = StreamProcessorStatus String
derive instance newtypeStreamProcessorStatus :: Newtype StreamProcessorStatus _


-- | <p>Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.</p>
newtype Sunglasses = Sunglasses 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }
derive instance newtypeSunglasses :: Newtype Sunglasses _


-- | <p>Information about a word or line of text detected by .</p> <p>The <code>DetectedText</code> field contains the text that Amazon Rekognition detected in the image. </p> <p>Every word and line has an identifier (<code>Id</code>). Each word belongs to a line and has a parent identifier (<code>ParentId</code>) that identifies the line of text in which the word appears. The word <code>Id</code> is also an index for the word within a line of words. </p> <p>For more information, see <a>text-detection</a>.</p>
newtype TextDetection = TextDetection 
  { "DetectedText" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (TextTypes)
  , "Id" :: NullOrUndefined (UInteger)
  , "ParentId" :: NullOrUndefined (UInteger)
  , "Confidence" :: NullOrUndefined (Percent)
  , "Geometry" :: NullOrUndefined (Geometry)
  }
derive instance newtypeTextDetection :: Newtype TextDetection _


newtype TextDetectionList = TextDetectionList (Array TextDetection)
derive instance newtypeTextDetectionList :: Newtype TextDetectionList _


newtype TextTypes = TextTypes String
derive instance newtypeTextTypes :: Newtype TextTypes _


-- | <p>Amazon Rekognition is temporarily unable to process the request. Try your call again.</p>
newtype ThrottlingException = ThrottlingException 
  { 
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


newtype UInteger = UInteger Int
derive instance newtypeUInteger :: Newtype UInteger _


newtype ULong = ULong Number
derive instance newtypeULong :: Newtype ULong _


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _


newtype Urls = Urls (Array Url)
derive instance newtypeUrls :: Newtype Urls _


-- | <p>Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as use <code>Video</code> to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.</p>
newtype Video = Video 
  { "S3Object" :: NullOrUndefined (S3Object)
  }
derive instance newtypeVideo :: Newtype Video _


newtype VideoJobStatus = VideoJobStatus String
derive instance newtypeVideoJobStatus :: Newtype VideoJobStatus _


-- | <p>Information about a video that Amazon Rekognition analyzed. <code>Videometadata</code> is returned in every page of paginated responses from a Amazon Rekognition video operation.</p>
newtype VideoMetadata = VideoMetadata 
  { "Codec" :: NullOrUndefined (String)
  , "DurationMillis" :: NullOrUndefined (ULong)
  , "Format" :: NullOrUndefined (String)
  , "FrameRate" :: NullOrUndefined (Number)
  , "FrameHeight" :: NullOrUndefined (ULong)
  , "FrameWidth" :: NullOrUndefined (ULong)
  }
derive instance newtypeVideoMetadata :: Newtype VideoMetadata _


-- | <p>The file size or duration of the supplied media is too large. The maximum file size is 8GB. The maximum duration is 2 hours. </p>
newtype VideoTooLargeException = VideoTooLargeException 
  { 
  }
derive instance newtypeVideoTooLargeException :: Newtype VideoTooLargeException _
