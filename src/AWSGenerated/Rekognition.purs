

-- | <p>This is the Amazon Rekognition API reference.</p>
module AWS.Rekognition where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Rekognition" :: String


-- | <p>Compares a face in the <i>source</i> input image with each of the 100 largest faces detected in the <i>target</i> input image. </p> <note> <p> If the source image contains multiple faces, the service detects the largest face and compares it with each face detected in the target image. </p> </note> <p>You pass the input and target images either as base64-encoded image bytes or as a references to images in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>In response, the operation returns an array of face matches ordered by similarity score in descending order. For each face match, the response provides a bounding box of the face, facial landmarks, pose details (pitch, role, and yaw), quality (brightness and sharpness), and confidence value (indicating the level of confidence that the bounding box contains a face). The response also provides a similarity score, which indicates how closely the faces match. </p> <note> <p>By default, only faces with a similarity score of greater than or equal to 80% are returned in the response. You can change this value by specifying the <code>SimilarityThreshold</code> parameter.</p> </note> <p> <code>CompareFaces</code> also returns an array of faces that don't match the source image. For each face, it returns a bounding box, confidence value, landmarks, pose details, and quality. The response also returns information about the face in the source image, including the bounding box of the face and confidence value.</p> <p>If the image doesn't contain Exif metadata, <code>CompareFaces</code> returns orientation information for the source and target images. Use these values to display the images with the correct image orientation.</p> <p>If no faces are detected in the source or target images, <code>CompareFaces</code> returns an <code>InvalidParameterException</code> error. </p> <note> <p> This is a stateless API operation. That is, data returned by this operation doesn't persist.</p> </note> <p>For an example, see <a>faces-compare-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:CompareFaces</code> action.</p>
compareFaces :: forall eff. CompareFacesRequest -> Aff (err :: AWS.RequestError | eff) CompareFacesResponse
compareFaces = AWS.request serviceName "CompareFaces" 


-- | <p>Creates a collection in an AWS Region. You can add faces to the collection using the operation. </p> <p>For example, you might create collections, one for each of your application users. A user can then index faces using the <code>IndexFaces</code> operation and persist results in a specific collection. Then, a user can search the collection for faces in the user-specific container. </p> <note> <p>Collection names are case-sensitive.</p> </note> <p>This operation requires permissions to perform the <code>rekognition:CreateCollection</code> action.</p>
createCollection :: forall eff. CreateCollectionRequest -> Aff (err :: AWS.RequestError | eff) CreateCollectionResponse
createCollection = AWS.request serviceName "CreateCollection" 


-- | <p>Creates an Amazon Rekognition stream processor that you can use to detect and recognize faces in a streaming video.</p> <p>Rekognition Video is a consumer of live video from Amazon Kinesis Video Streams. Rekognition Video sends analysis results to Amazon Kinesis Data Streams.</p> <p>You provide as input a Kinesis video stream (<code>Input</code>) and a Kinesis data stream (<code>Output</code>) stream. You also specify the face recognition criteria in <code>Settings</code>. For example, the collection containing faces that you want to recognize. Use <code>Name</code> to assign an identifier for the stream processor. You use <code>Name</code> to manage the stream processor. For example, you can start processing the source video by calling with the <code>Name</code> field. </p> <p>After you have finished analyzing a streaming video, use to stop processing. You can delete the stream processor by calling .</p>
createStreamProcessor :: forall eff. CreateStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamProcessorResponse
createStreamProcessor = AWS.request serviceName "CreateStreamProcessor" 


-- | <p>Deletes the specified collection. Note that this operation removes all faces in the collection. For an example, see <a>delete-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteCollection</code> action.</p>
deleteCollection :: forall eff. DeleteCollectionRequest -> Aff (err :: AWS.RequestError | eff) DeleteCollectionResponse
deleteCollection = AWS.request serviceName "DeleteCollection" 


-- | <p>Deletes faces from a collection. You specify a collection ID and an array of face IDs to remove from the collection.</p> <p>This operation requires permissions to perform the <code>rekognition:DeleteFaces</code> action.</p>
deleteFaces :: forall eff. DeleteFacesRequest -> Aff (err :: AWS.RequestError | eff) DeleteFacesResponse
deleteFaces = AWS.request serviceName "DeleteFaces" 


-- | <p>Deletes the stream processor identified by <code>Name</code>. You assign the value for <code>Name</code> when you create the stream processor with . You might not be able to use the same name for a stream processor for a few seconds after calling <code>DeleteStreamProcessor</code>.</p>
deleteStreamProcessor :: forall eff. DeleteStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) DeleteStreamProcessorResponse
deleteStreamProcessor = AWS.request serviceName "DeleteStreamProcessor" 


-- | <p>Provides information about a stream processor created by . You can get information about the input and output streams, the input parameters for the face recognition being performed, and the current status of the stream processor.</p>
describeStreamProcessor :: forall eff. DescribeStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) DescribeStreamProcessorResponse
describeStreamProcessor = AWS.request serviceName "DescribeStreamProcessor" 


-- | <p>Detects faces within an image that is provided as input.</p> <p> <code>DetectFaces</code> detects the 100 largest faces in the image. For each face detected, the operation returns face details including a bounding box of the face, a confidence value (that the bounding box contains a face), and a fixed set of attributes such as facial landmarks (for example, coordinates of eye and mouth), gender, presence of beard, sunglasses, etc. </p> <p>The face-detection algorithm is most effective on frontal faces. For non-frontal or obscured faces, the algorithm may not detect the faces or might detect faces with lower confidence. </p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> </note> <p>For an example, see <a>procedure-detecting-faces-in-images</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectFaces</code> action. </p>
detectFaces :: forall eff. DetectFacesRequest -> Aff (err :: AWS.RequestError | eff) DetectFacesResponse
detectFaces = AWS.request serviceName "DetectFaces" 


-- | <p>Detects instances of real-world entities within an image (JPEG or PNG) provided as input. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; and concepts like landscape, evening, and nature. For an example, see <a>images-s3</a>.</p> <note> <p> <code>DetectLabels</code> does not support the detection of activities. However, activity detection is supported for label detection in videos. For more information, see .</p> </note> <p>You pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> For each object, scene, and concept the API returns one or more labels. Each label provides the object name, and the level of confidence that the image contains the object. For example, suppose the input image has a lighthouse, the sea, and a rock. The response will include all three labels, one for each object. </p> <p> <code>{Name: lighthouse, Confidence: 98.4629}</code> </p> <p> <code>{Name: rock,Confidence: 79.2097}</code> </p> <p> <code> {Name: sea,Confidence: 75.061}</code> </p> <p> In the preceding example, the operation returns one label for each of the three objects. The operation can also return multiple labels for the same object in the image. For example, if the input image shows a flower (for example, a tulip), the operation might return the following three labels. </p> <p> <code>{Name: flower,Confidence: 99.0562}</code> </p> <p> <code>{Name: plant,Confidence: 99.0562}</code> </p> <p> <code>{Name: tulip,Confidence: 99.0562}</code> </p> <p>In this example, the detection algorithm more precisely identifies the flower as a tulip.</p> <p>In response, the API returns an array of labels. In addition, the response also includes the orientation correction. Optionally, you can specify <code>MinConfidence</code> to control the confidence threshold for the labels returned. The default is 50%. You can also add the <code>MaxLabels</code> parameter to limit the number of labels returned. </p> <note> <p>If the object detected is a person, the operation doesn't provide the same facial details that the <a>DetectFaces</a> operation provides.</p> </note> <p>This is a stateless API operation. That is, the operation does not persist any data.</p> <p>This operation requires permissions to perform the <code>rekognition:DetectLabels</code> action. </p>
detectLabels :: forall eff. DetectLabelsRequest -> Aff (err :: AWS.RequestError | eff) DetectLabelsResponse
detectLabels = AWS.request serviceName "DetectLabels" 


-- | <p>Detects explicit or suggestive adult content in a specified JPEG or PNG format image. Use <code>DetectModerationLabels</code> to moderate images depending on your requirements. For example, you might want to filter images that contain nudity, but not images containing suggestive content.</p> <p>To filter images, use the labels returned by <code>DetectModerationLabels</code> to determine which types of content are appropriate. For information about moderation labels, see <a>moderation</a>.</p> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p>
detectModerationLabels :: forall eff. DetectModerationLabelsRequest -> Aff (err :: AWS.RequestError | eff) DetectModerationLabelsResponse
detectModerationLabels = AWS.request serviceName "DetectModerationLabels" 


-- | <p>Detects text in the input image and converts it into machine-readable text.</p> <p>Pass the input image as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the AWS CLI to call Amazon Rekognition operations, you must pass it as a reference to an image in an Amazon S3 bucket. For the AWS CLI, passing image bytes is not supported. The image must be either a .png or .jpeg formatted file. </p> <p>The <code>DetectText</code> operation returns text in an array of elements, <code>TextDetections</code>. Each <code>TextDetection</code> element provides information about a single word or line of text that was detected in the image. </p> <p>A word is one or more ISO basic latin script characters that are not separated by spaces. <code>DetectText</code> can detect up to 50 words in an image.</p> <p>A line is a string of equally spaced words. A line isn't necessarily a complete sentence. For example, a driver's license number is detected as a line. A line ends when there is no aligned text after it. Also, a line ends when there is a large gap between words, relative to the length of the words. This means, depending on the gap between words, Amazon Rekognition may detect multiple lines in text aligned in the same direction. Periods don't represent the end of a line. If a sentence spans multiple lines, the <code>DetectText</code> operation returns multiple lines.</p> <p>To determine whether a <code>TextDetection</code> element is a line of text or a word, use the <code>TextDetection</code> object <code>Type</code> field. </p> <p>To be detected, text must be within +/- 30 degrees orientation of the horizontal axis.</p> <p>For more information, see <a>text-detection</a>.</p>
detectText :: forall eff. DetectTextRequest -> Aff (err :: AWS.RequestError | eff) DetectTextResponse
detectText = AWS.request serviceName "DetectText" 


-- | <p>Gets the name and additional information about a celebrity based on his or her Rekognition ID. The additional information is returned as an array of URLs. If there is no additional information about the celebrity, this list is empty. For more information, see <a>get-celebrity-info-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:GetCelebrityInfo</code> action. </p>
getCelebrityInfo :: forall eff. GetCelebrityInfoRequest -> Aff (err :: AWS.RequestError | eff) GetCelebrityInfoResponse
getCelebrityInfo = AWS.request serviceName "GetCelebrityInfo" 


-- | <p>Gets the celebrity recognition results for a Rekognition Video analysis started by .</p> <p>Celebrity recognition in a video is an asynchronous operation. Analysis is started by a call to which returns a job identifier (<code>JobId</code>). When the celebrity recognition operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartCelebrityRecognition</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>.</p> <p> <code>GetCelebrityRecognition</code> returns detected celebrities and the time(s) they are detected in an array (<code>Celebrities</code>) of objects. Each <code>CelebrityRecognition</code> contains information about the celebrity in a object and the time, <code>Timestamp</code>, the celebrity was detected. </p> <p>By default, the <code>Celebrities</code> array is sorted by time (milliseconds from the start of the video). You can also sort the array by celebrity by specifying the value <code>ID</code> in the <code>SortBy</code> input parameter.</p> <p>The <code>CelebrityDetail</code> object includes the celebrity identifer and additional information urls. If you don't store the additional information urls, you can get them later by calling with the celebrity identifer.</p> <p>No information is returned for faces not recognized as celebrities.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetCelebrityDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetCelebrityRecognition</code>.</p>
getCelebrityRecognition :: forall eff. GetCelebrityRecognitionRequest -> Aff (err :: AWS.RequestError | eff) GetCelebrityRecognitionResponse
getCelebrityRecognition = AWS.request serviceName "GetCelebrityRecognition" 


-- | <p>Gets the content moderation analysis results for a Rekognition Video analysis started by .</p> <p>Content moderation analysis of a video is an asynchronous operation. You start analysis by calling . which returns a job identifier (<code>JobId</code>). When analysis finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartContentModeration</code>. To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetCelebrityDetection</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityDetection</code>. For more information, see <a>video</a>. </p> <p> <code>GetContentModeration</code> returns detected content moderation labels, and the time they are detected, in an array, <code>ModerationLabels</code>, of objects. </p> <p>By default, the moderated labels are returned sorted by time, in milliseconds from the start of the video. You can also sort them by moderated label by specifying <code>NAME</code> for the <code>SortBy</code> input parameter. </p> <p>Since video analysis can return a large number of results, use the <code>MaxResults</code> parameter to limit the number of labels returned in a single call to <code>GetContentModeration</code>. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetContentModeration</code> and populate the <code>NextToken</code> request parameter with the value of <code>NextToken</code> returned from the previous call to <code>GetContentModeration</code>.</p> <p>For more information, see <a>moderation</a>.</p>
getContentModeration :: forall eff. GetContentModerationRequest -> Aff (err :: AWS.RequestError | eff) GetContentModerationResponse
getContentModeration = AWS.request serviceName "GetContentModeration" 


-- | <p>Gets face detection results for a Rekognition Video analysis started by .</p> <p>Face detection with Rekognition Video is an asynchronous operation. You start face detection by calling which returns a job identifier (<code>JobId</code>). When the face detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceDetection</code>. To get the results of the face detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>.</p> <p> <code>GetFaceDetection</code> returns an array of detected faces (<code>Faces</code>) sorted by the time the faces were detected. </p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetFaceDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetFaceDetection</code>.</p>
getFaceDetection :: forall eff. GetFaceDetectionRequest -> Aff (err :: AWS.RequestError | eff) GetFaceDetectionResponse
getFaceDetection = AWS.request serviceName "GetFaceDetection" 


-- | <p>Gets the face search results for Rekognition Video face search started by . The search returns faces in a collection that match the faces of persons detected in a video. It also includes the time(s) that faces are matched in the video.</p> <p>Face search in a video is an asynchronous operation. You start face search by calling to which returns a job identifier (<code>JobId</code>). When the search operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartFaceSearch</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call <code>GetFaceSearch</code> and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections</a>.</p> <p>The search results are retured in an array, <code>Persons</code>, of objects. Each<code>PersonMatch</code> element contains details about the matching faces in the input collection, person information for the matched person, and the time the person was matched in the video.</p> <p>By default, the <code>Persons</code> array is sorted by the time, in milliseconds from the start of the video, persons are matched. You can also sort by persons by specifying <code>INDEX</code> for the <code>SORTBY</code> input parameter.</p>
getFaceSearch :: forall eff. GetFaceSearchRequest -> Aff (err :: AWS.RequestError | eff) GetFaceSearchResponse
getFaceSearch = AWS.request serviceName "GetFaceSearch" 


-- | <p>Gets the label detection results of a Rekognition Video analysis started by . </p> <p>The label detection operation is started by a call to which returns a job identifier (<code>JobId</code>). When the label detection operation finishes, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartlabelDetection</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p> <code>GetLabelDetection</code> returns an array of detected labels (<code>Labels</code>) sorted by the time the labels were detected. You can also sort by the label name by specifying <code>NAME</code> for the <code>SortBy</code> input parameter.</p> <p>The labels returned include the label name, the percentage confidence in the accuracy of the detected label, and the time the label was detected in the video.</p> <p>Use MaxResults parameter to limit the number of labels returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetlabelDetection</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetLabelDetection</code>.</p>
getLabelDetection :: forall eff. GetLabelDetectionRequest -> Aff (err :: AWS.RequestError | eff) GetLabelDetectionResponse
getLabelDetection = AWS.request serviceName "GetLabelDetection" 


-- | <p>Gets the person tracking results of a Rekognition Video analysis started by .</p> <p>The person detection operation is started by a call to <code>StartPersonTracking</code> which returns a job identifier (<code>JobId</code>). When the person detection operation finishes, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic registered in the initial call to <code>StartPersonTracking</code>.</p> <p>To get the results of the person tracking operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p> <p> <code>GetPersonTracking</code> returns an array, <code>Persons</code>, of tracked persons and the time(s) they were tracked in the video. </p> <p>By default, the array is sorted by the time(s) a person is tracked in the video. You can sort by tracked persons by specifying <code>INDEX</code> for the <code>SortBy</code> input parameter.</p> <p>Use the <code>MaxResults</code> parameter to limit the number of items returned. If there are more results than specified in <code>MaxResults</code>, the value of <code>NextToken</code> in the operation response contains a pagination token for getting the next set of results. To get the next page of results, call <code>GetPersonTracking</code> and populate the <code>NextToken</code> request parameter with the token value returned from the previous call to <code>GetPersonTracking</code>.</p>
getPersonTracking :: forall eff. GetPersonTrackingRequest -> Aff (err :: AWS.RequestError | eff) GetPersonTrackingResponse
getPersonTracking = AWS.request serviceName "GetPersonTracking" 


-- | <p>Detects faces in the input image and adds them to the specified collection. </p> <p>Amazon Rekognition does not save the actual faces detected. Instead, the underlying detection algorithm first detects the faces in the input image, and for each face extracts facial features into a feature vector, and stores it in the back-end database. Amazon Rekognition uses feature vectors when performing face match and search operations using the and operations.</p> <p>If you are using version 1.0 of the face detection model, <code>IndexFaces</code> indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image. To determine which version of the model you are using, check the the value of <code>FaceModelVersion</code> in the response from <code>IndexFaces</code>. For more information, see <a>face-detection-model</a>.</p> <p>If you provide the optional <code>ExternalImageID</code> for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image. </p> <p>In response, the operation returns an array of metadata for all detected faces. This includes, the bounding box of the detected face, confidence value (indicating the bounding box contains a face), a face ID assigned by the service for each face that is detected and stored, and an image ID assigned by the service for the input image. If you request all facial attributes (using the <code>detectionAttributes</code> parameter, Amazon Rekognition returns detailed facial attributes such as facial landmarks (for example, location of eye and mount) and other facial attributes such gender. If you provide the same image, specify the same collection, and use the same external ID in the <code>IndexFaces</code> operation, Amazon Rekognition doesn't save duplicate face metadata. </p> <p>The input image is passed either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>This operation requires permissions to perform the <code>rekognition:IndexFaces</code> action.</p>
indexFaces :: forall eff. IndexFacesRequest -> Aff (err :: AWS.RequestError | eff) IndexFacesResponse
indexFaces = AWS.request serviceName "IndexFaces" 


-- | <p>Returns list of collection IDs in your account. If the result is truncated, the response also provides a <code>NextToken</code> that you can use in the subsequent request to fetch the next set of collection IDs.</p> <p>For an example, see <a>list-collection-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:ListCollections</code> action.</p>
listCollections :: forall eff. ListCollectionsRequest -> Aff (err :: AWS.RequestError | eff) ListCollectionsResponse
listCollections = AWS.request serviceName "ListCollections" 


-- | <p>Returns metadata for faces in the specified collection. This metadata includes information such as the bounding box coordinates, the confidence (that the bounding box contains a face), and face ID. For an example, see <a>list-faces-in-collection-procedure</a>. </p> <p>This operation requires permissions to perform the <code>rekognition:ListFaces</code> action.</p>
listFaces :: forall eff. ListFacesRequest -> Aff (err :: AWS.RequestError | eff) ListFacesResponse
listFaces = AWS.request serviceName "ListFaces" 


-- | <p>Gets a list of stream processors that you have created with . </p>
listStreamProcessors :: forall eff. ListStreamProcessorsRequest -> Aff (err :: AWS.RequestError | eff) ListStreamProcessorsResponse
listStreamProcessors = AWS.request serviceName "ListStreamProcessors" 


-- | <p>Returns an array of celebrities recognized in the input image. For more information, see <a>celebrities</a>. </p> <p> <code>RecognizeCelebrities</code> returns the 100 largest faces in the image. It lists recognized celebrities in the <code>CelebrityFaces</code> array and unrecognized faces in the <code>UnrecognizedFaces</code> array. <code>RecognizeCelebrities</code> doesn't return celebrities whose faces are not amongst the largest 100 faces in the image.</p> <p>For each celebrity recognized, the <code>RecognizeCelebrities</code> returns a <code>Celebrity</code> object. The <code>Celebrity</code> object contains the celebrity name, ID, URL links to additional information, match confidence, and a <code>ComparedFace</code> object that you can use to locate the celebrity's face on the image.</p> <p>Rekognition does not retain information about which images a celebrity has been recognized in. Your application must store this information and use the <code>Celebrity</code> ID property as a unique identifier for the celebrity. If you don't store the celebrity name or additional information URLs returned by <code>RecognizeCelebrities</code>, you will need the ID to identify the celebrity in a call to the operation.</p> <p>You pass the imput image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p>For an example, see <a>celebrities-procedure-image</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:RecognizeCelebrities</code> operation.</p>
recognizeCelebrities :: forall eff. RecognizeCelebritiesRequest -> Aff (err :: AWS.RequestError | eff) RecognizeCelebritiesResponse
recognizeCelebrities = AWS.request serviceName "RecognizeCelebrities" 


-- | <p>For a given input face ID, searches for matching faces in the collection the face belongs to. You get a face ID when you add a face to the collection using the <a>IndexFaces</a> operation. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p>You can also search faces without indexing faces by using the <code>SearchFacesByImage</code> operation.</p> </note> <p> The operation response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match that is found. Along with the metadata, the response also includes a <code>confidence</code> value for each face match, indicating the confidence that the specific face matches the input face. </p> <p>For an example, see <a>search-face-with-id-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFaces</code> action.</p>
searchFaces :: forall eff. SearchFacesRequest -> Aff (err :: AWS.RequestError | eff) SearchFacesResponse
searchFaces = AWS.request serviceName "SearchFaces" 


-- | <p>For a given input image, first detects the largest face in the image, and then searches the specified collection for matching faces. The operation compares the features of the input face with faces in the specified collection. </p> <note> <p> To search for all faces in an input image, you might first call the operation, and then use the face IDs returned in subsequent calls to the operation. </p> <p> You can also call the <code>DetectFaces</code> operation and use the bounding boxes in the response to make face crops, which then you can pass in to the <code>SearchFacesByImage</code> operation. </p> </note> <p>You pass the input image either as base64-encoded image bytes or as a reference to an image in an Amazon S3 bucket. If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes is not supported. The image must be either a PNG or JPEG formatted file. </p> <p> The response returns an array of faces that match, ordered by similarity score with the highest similarity first. More specifically, it is an array of metadata for each face match found. Along with the metadata, the response also includes a <code>similarity</code> indicating how similar the face is to the input face. In the response, the operation also returns the bounding box (and a confidence level that the bounding box contains a face) of the face that Amazon Rekognition used for the input image. </p> <p>For an example, see <a>search-face-with-image-procedure</a>.</p> <p>This operation requires permissions to perform the <code>rekognition:SearchFacesByImage</code> action.</p>
searchFacesByImage :: forall eff. SearchFacesByImageRequest -> Aff (err :: AWS.RequestError | eff) SearchFacesByImageResponse
searchFacesByImage = AWS.request serviceName "SearchFacesByImage" 


-- | <p>Starts asynchronous recognition of celebrities in a stored video.</p> <p>Rekognition Video can detect celebrities in a video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartCelebrityRecognition</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When celebrity recognition analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the celebrity recognition analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartCelebrityRecognition</code>. For more information, see <a>celebrities</a>.</p>
startCelebrityRecognition :: forall eff. StartCelebrityRecognitionRequest -> Aff (err :: AWS.RequestError | eff) StartCelebrityRecognitionResponse
startCelebrityRecognition = AWS.request serviceName "StartCelebrityRecognition" 


-- | <p> Starts asynchronous detection of explicit or suggestive adult content in a stored video.</p> <p>Rekognition Video can moderate content in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartContentModeration</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the analysis. When content moderation analysis is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the content moderation analysis, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartContentModeration</code>. For more information, see <a>moderation</a>.</p>
startContentModeration :: forall eff. StartContentModerationRequest -> Aff (err :: AWS.RequestError | eff) StartContentModerationResponse
startContentModeration = AWS.request serviceName "StartContentModeration" 


-- | <p>Starts asynchronous detection of faces in a stored video.</p> <p>Rekognition Video can detect faces in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceDetection</code> returns a job identifier (<code>JobId</code>) that you use to get the results of the operation. When face detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceDetection</code>. For more information, see <a>faces-video</a>.</p>
startFaceDetection :: forall eff. StartFaceDetectionRequest -> Aff (err :: AWS.RequestError | eff) StartFaceDetectionResponse
startFaceDetection = AWS.request serviceName "StartFaceDetection" 


-- | <p>Starts the asynchronous search for faces in a collection that match the faces of persons detected in a stored video.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartFaceSearch</code> returns a job identifier (<code>JobId</code>) which you use to get the search results once the search has completed. When searching is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. To get the search results, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartFaceSearch</code>. For more information, see <a>collections-search-person</a>.</p>
startFaceSearch :: forall eff. StartFaceSearchRequest -> Aff (err :: AWS.RequestError | eff) StartFaceSearchResponse
startFaceSearch = AWS.request serviceName "StartFaceSearch" 


-- | <p>Starts asynchronous detection of labels in a stored video.</p> <p>Rekognition Video can detect labels in a video. Labels are instances of real-world entities. This includes objects like flower, tree, and table; events like wedding, graduation, and birthday party; concepts like landscape, evening, and nature; and activities like a person getting out of a car or a person skiing.</p> <p>The video must be stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartLabelDetection</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Rekognition Video publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>.</p> <p>To get the results of the label detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartLabelDetection</code>.</p> <p/>
startLabelDetection :: forall eff. StartLabelDetectionRequest -> Aff (err :: AWS.RequestError | eff) StartLabelDetectionResponse
startLabelDetection = AWS.request serviceName "StartLabelDetection" 


-- | <p>Starts the asynchronous tracking of persons in a stored video.</p> <p>Rekognition Video can track persons in a video stored in an Amazon S3 bucket. Use <a>Video</a> to specify the bucket name and the filename of the video. <code>StartPersonTracking</code> returns a job identifier (<code>JobId</code>) which you use to get the results of the operation. When label detection is finished, Amazon Rekognition publishes a completion status to the Amazon Simple Notification Service topic that you specify in <code>NotificationChannel</code>. </p> <p>To get the results of the person detection operation, first check that the status value published to the Amazon SNS topic is <code>SUCCEEDED</code>. If so, call and pass the job identifier (<code>JobId</code>) from the initial call to <code>StartPersonTracking</code>.</p>
startPersonTracking :: forall eff. StartPersonTrackingRequest -> Aff (err :: AWS.RequestError | eff) StartPersonTrackingResponse
startPersonTracking = AWS.request serviceName "StartPersonTracking" 


-- | <p>Starts processing a stream processor. You create a stream processor by calling . To tell <code>StartStreamProcessor</code> which stream processor to start, use the value of the <code>Name</code> field specified in the call to <code>CreateStreamProcessor</code>.</p>
startStreamProcessor :: forall eff. StartStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) StartStreamProcessorResponse
startStreamProcessor = AWS.request serviceName "StartStreamProcessor" 


-- | <p>Stops a running stream processor that was created by .</p>
stopStreamProcessor :: forall eff. StopStreamProcessorRequest -> Aff (err :: AWS.RequestError | eff) StopStreamProcessorResponse
stopStreamProcessor = AWS.request serviceName "StopStreamProcessor" 


-- | <p>You are not authorized to perform the action.</p>
newtype AccessDeniedException = AccessDeniedException 
  { 
  }


-- | <p>Structure containing the estimated age range, in years, for a face.</p> <p>Rekognition estimates an age-range for faces detected in the input image. Estimated age ranges can overlap; a face of a 5 year old may have an estimated range of 4-6 whilst the face of a 6 year old may have an estimated range of 4-8.</p>
newtype AgeRange = AgeRange 
  { "Low" :: NullOrUndefined (UInteger)
  , "High" :: NullOrUndefined (UInteger)
  }


newtype Attribute = Attribute String


newtype Attributes = Attributes (Array Attribute)


-- | <p>Indicates whether or not the face has a beard, and the confidence level in the determination.</p>
newtype Beard = Beard 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Identifies the bounding box around the object, face or text. The <code>left</code> (x-coordinate) and <code>top</code> (y-coordinate) are coordinates representing the top and left sides of the bounding box. Note that the upper-left corner of the image is the origin (0,0). </p> <p>The <code>top</code> and <code>left</code> values returned are ratios of the overall image size. For example, if the input image is 700x200 pixels, and the top-left coordinate of the bounding box is 350x50 pixels, the API returns a <code>left</code> value of 0.5 (350/700) and a <code>top</code> value of 0.25 (50/200).</p> <p>The <code>width</code> and <code>height</code> values represent the dimensions of the bounding box as a ratio of the overall image dimension. For example, if the input image is 700x200 pixels, and the bounding box width is 70 pixels, the width returned is 0.1. </p> <note> <p> The bounding box coordinates can have negative values. For example, if Amazon Rekognition is able to detect a face that is at the image edge and is only partially visible, the service can return coordinates that are outside the image bounds and, depending on the image edge, you might get negative values or values greater than 1 for the <code>left</code> or <code>top</code> values. </p> </note>
newtype BoundingBox = BoundingBox 
  { "Width" :: NullOrUndefined (Number)
  , "Height" :: NullOrUndefined (Number)
  , "Left" :: NullOrUndefined (Number)
  , "Top" :: NullOrUndefined (Number)
  }


-- | <p>Provides information about a celebrity recognized by the operation.</p>
newtype Celebrity = Celebrity 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (RekognitionUniqueId)
  , "Face" :: NullOrUndefined (ComparedFace)
  , "MatchConfidence" :: NullOrUndefined (Percent)
  }


-- | <p>Information about a recognized celebrity.</p>
newtype CelebrityDetail = CelebrityDetail 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (RekognitionUniqueId)
  , "Confidence" :: NullOrUndefined (Percent)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined (FaceDetail)
  }


newtype CelebrityList = CelebrityList (Array Celebrity)


-- | <p>Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see .</p>
newtype CelebrityRecognition = CelebrityRecognition 
  { "Number" :: NullOrUndefined (Number)
  , "Celebrity" :: NullOrUndefined (CelebrityDetail)
  }


newtype CelebrityRecognitionSortBy = CelebrityRecognitionSortBy String


newtype CelebrityRecognitions = CelebrityRecognitions (Array CelebrityRecognition)


newtype ClientRequestToken = ClientRequestToken String


newtype CollectionId = CollectionId String


newtype CollectionIdList = CollectionIdList (Array CollectionId)


-- | <p>Provides information about a face in a target image that matches the source image face analysed by <code>CompareFaces</code>. The <code>Face</code> property contains the bounding box of the face in the target image. The <code>Similarity</code> property is the confidence that the source image face matches the face in the bounding box.</p>
newtype CompareFacesMatch = CompareFacesMatch 
  { "Similarity" :: NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined (ComparedFace)
  }


newtype CompareFacesMatchList = CompareFacesMatchList (Array CompareFacesMatch)


newtype CompareFacesRequest = CompareFacesRequest 
  { "SourceImage" :: (Image)
  , "TargetImage" :: (Image)
  , "SimilarityThreshold" :: NullOrUndefined (Percent)
  }


newtype CompareFacesResponse = CompareFacesResponse 
  { "SourceImageFace" :: NullOrUndefined (ComparedSourceImageFace)
  , "FaceMatches" :: NullOrUndefined (CompareFacesMatchList)
  , "UnmatchedFaces" :: NullOrUndefined (CompareFacesUnmatchList)
  , "SourceImageOrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  , "TargetImageOrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }


newtype CompareFacesUnmatchList = CompareFacesUnmatchList (Array ComparedFace)


-- | <p>Provides face metadata for target image faces that are analysed by <code>CompareFaces</code> and <code>RecognizeCelebrities</code>.</p>
newtype ComparedFace = ComparedFace 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined (Percent)
  , "Landmarks" :: NullOrUndefined (Landmarks)
  , "Pose" :: NullOrUndefined (Pose)
  , "Quality" :: NullOrUndefined (ImageQuality)
  }


newtype ComparedFaceList = ComparedFaceList (Array ComparedFace)


-- | <p>Type that describes the face Amazon Rekognition chose to compare with the faces in the target. This contains a bounding box for the selected face and confidence level that the bounding box contains a face. Note that Amazon Rekognition selects the largest face in the source image for this comparison. </p>
newtype ComparedSourceImageFace = ComparedSourceImageFace 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Information about a moderation label detection in a stored video.</p>
newtype ContentModerationDetection = ContentModerationDetection 
  { "Number" :: NullOrUndefined (Number)
  , "ModerationLabel" :: NullOrUndefined (ModerationLabel)
  }


newtype ContentModerationDetections = ContentModerationDetections (Array ContentModerationDetection)


newtype ContentModerationSortBy = ContentModerationSortBy String


newtype CreateCollectionRequest = CreateCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }


newtype CreateCollectionResponse = CreateCollectionResponse 
  { "StatusCode" :: NullOrUndefined (UInteger)
  , "CollectionArn" :: NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }


newtype CreateStreamProcessorRequest = CreateStreamProcessorRequest 
  { "Input" :: (StreamProcessorInput)
  , "Output" :: (StreamProcessorOutput)
  , "Name" :: (StreamProcessorName)
  , "Settings" :: (StreamProcessorSettings)
  , "RoleArn" :: (RoleArn)
  }


newtype CreateStreamProcessorResponse = CreateStreamProcessorResponse 
  { "StreamProcessorArn" :: NullOrUndefined (StreamProcessorArn)
  }


newtype DateTime = DateTime Number


newtype Degree = Degree Number


newtype DeleteCollectionRequest = DeleteCollectionRequest 
  { "CollectionId" :: (CollectionId)
  }


newtype DeleteCollectionResponse = DeleteCollectionResponse 
  { "StatusCode" :: NullOrUndefined (UInteger)
  }


newtype DeleteFacesRequest = DeleteFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceIds" :: (FaceIdList)
  }


newtype DeleteFacesResponse = DeleteFacesResponse 
  { "DeletedFaces" :: NullOrUndefined (FaceIdList)
  }


newtype DeleteStreamProcessorRequest = DeleteStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }


newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse 
  { 
  }


newtype DescribeStreamProcessorRequest = DescribeStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }


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


newtype DetectFacesRequest = DetectFacesRequest 
  { "Image" :: (Image)
  , "Attributes" :: NullOrUndefined (Attributes)
  }


newtype DetectFacesResponse = DetectFacesResponse 
  { "FaceDetails" :: NullOrUndefined (FaceDetailList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }


newtype DetectLabelsRequest = DetectLabelsRequest 
  { "Image" :: (Image)
  , "MaxLabels" :: NullOrUndefined (UInteger)
  , "MinConfidence" :: NullOrUndefined (Percent)
  }


newtype DetectLabelsResponse = DetectLabelsResponse 
  { "Labels" :: NullOrUndefined (Labels)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }


newtype DetectModerationLabelsRequest = DetectModerationLabelsRequest 
  { "Image" :: (Image)
  , "MinConfidence" :: NullOrUndefined (Percent)
  }


newtype DetectModerationLabelsResponse = DetectModerationLabelsResponse 
  { "ModerationLabels" :: NullOrUndefined (ModerationLabels)
  }


newtype DetectTextRequest = DetectTextRequest 
  { "Image" :: (Image)
  }


newtype DetectTextResponse = DetectTextResponse 
  { "TextDetections" :: NullOrUndefined (TextDetectionList)
  }


-- | <p>The emotions detected on the face, and the confidence level in the determination. For example, HAPPY, SAD, and ANGRY.</p>
newtype Emotion = Emotion 
  { "Type" :: NullOrUndefined (EmotionName)
  , "Confidence" :: NullOrUndefined (Percent)
  }


newtype EmotionName = EmotionName String


newtype Emotions = Emotions (Array Emotion)


newtype ExternalImageId = ExternalImageId String


-- | <p>Indicates whether or not the eyes on the face are open, and the confidence level in the determination.</p>
newtype EyeOpen = EyeOpen 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.</p>
newtype Eyeglasses = Eyeglasses 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned. </p>
newtype Face = Face 
  { "FaceId" :: NullOrUndefined (FaceId)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "ImageId" :: NullOrUndefined (ImageId)
  , "ExternalImageId" :: NullOrUndefined (ExternalImageId)
  , "Confidence" :: NullOrUndefined (Percent)
  }


newtype FaceAttributes = FaceAttributes String


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


newtype FaceDetailList = FaceDetailList (Array FaceDetail)


-- | <p>Information about a face detected in a video analysis request and the time the face was detected in the video. </p>
newtype FaceDetection = FaceDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Face" :: NullOrUndefined (FaceDetail)
  }


newtype FaceDetections = FaceDetections (Array FaceDetection)


newtype FaceId = FaceId String


newtype FaceIdList = FaceIdList (Array FaceId)


newtype FaceList = FaceList (Array Face)


-- | <p>Provides face metadata. In addition, it also provides the confidence in the match of this face with the input face.</p>
newtype FaceMatch = FaceMatch 
  { "Similarity" :: NullOrUndefined (Percent)
  , "Face" :: NullOrUndefined (Face)
  }


newtype FaceMatchList = FaceMatchList (Array FaceMatch)


newtype FaceModelVersionList = FaceModelVersionList (Array String)


-- | <p>Object containing both the face metadata (stored in the back-end database) and facial attributes that are detected but aren't stored in the database.</p>
newtype FaceRecord = FaceRecord 
  { "Face" :: NullOrUndefined (Face)
  , "FaceDetail" :: NullOrUndefined (FaceDetail)
  }


newtype FaceRecordList = FaceRecordList (Array FaceRecord)


-- | <p>Input face recognition parameters for an Amazon Rekognition stream processor. <code>FaceRecognitionSettings</code> is a request parameter for .</p>
newtype FaceSearchSettings = FaceSearchSettings 
  { "CollectionId" :: NullOrUndefined (CollectionId)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }


newtype FaceSearchSortBy = FaceSearchSortBy String


-- | <p>Gender of the face and the confidence level in the determination.</p>
newtype Gender = Gender 
  { "Value" :: NullOrUndefined (GenderType)
  , "Confidence" :: NullOrUndefined (Percent)
  }


newtype GenderType = GenderType String


-- | <p>Information about where text detected by is located on an image.</p>
newtype Geometry = Geometry 
  { "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Polygon" :: NullOrUndefined (Polygon)
  }


newtype GetCelebrityInfoRequest = GetCelebrityInfoRequest 
  { "Id" :: (RekognitionUniqueId)
  }


newtype GetCelebrityInfoResponse = GetCelebrityInfoResponse 
  { "Urls" :: NullOrUndefined (Urls)
  , "Name" :: NullOrUndefined (String)
  }


newtype GetCelebrityRecognitionRequest = GetCelebrityRecognitionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (CelebrityRecognitionSortBy)
  }


newtype GetCelebrityRecognitionResponse = GetCelebrityRecognitionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Celebrities" :: NullOrUndefined (CelebrityRecognitions)
  }


newtype GetContentModerationRequest = GetContentModerationRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (ContentModerationSortBy)
  }


newtype GetContentModerationResponse = GetContentModerationResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "ModerationLabels" :: NullOrUndefined (ContentModerationDetections)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype GetFaceDetectionRequest = GetFaceDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype GetFaceDetectionResponse = GetFaceDetectionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Faces" :: NullOrUndefined (FaceDetections)
  }


newtype GetFaceSearchRequest = GetFaceSearchRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (FaceSearchSortBy)
  }


newtype GetFaceSearchResponse = GetFaceSearchResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "Persons" :: NullOrUndefined (PersonMatches)
  }


newtype GetLabelDetectionRequest = GetLabelDetectionRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (LabelDetectionSortBy)
  }


newtype GetLabelDetectionResponse = GetLabelDetectionResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Labels" :: NullOrUndefined (LabelDetections)
  }


newtype GetPersonTrackingRequest = GetPersonTrackingRequest 
  { "JobId" :: (JobId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "SortBy" :: NullOrUndefined (PersonTrackingSortBy)
  }


newtype GetPersonTrackingResponse = GetPersonTrackingResponse 
  { "JobStatus" :: NullOrUndefined (VideoJobStatus)
  , "StatusMessage" :: NullOrUndefined (StatusMessage)
  , "VideoMetadata" :: NullOrUndefined (VideoMetadata)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "Persons" :: NullOrUndefined (PersonDetections)
  }


-- | <p>A <code>ClientRequestToken</code> input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { 
  }


-- | <p>Provides the input image either as bytes or an S3 object.</p> <p>You pass image bytes to a Rekognition API operation by using the <code>Bytes</code> property. For example, you would use the <code>Bytes</code> property to pass an image loaded from a local file system. Image bytes passed by using the <code>Bytes</code> property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Rekognition API operations. For more information, see <a>images-bytes</a>.</p> <p> You pass images stored in an S3 bucket to a Rekognition API operation by using the <code>S3Object</code> property. Images stored in an S3 bucket do not need to be base64-encoded.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>If you use the Amazon CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype Image = Image 
  { "Bytes" :: NullOrUndefined (ImageBlob)
  , "S3Object" :: NullOrUndefined (S3Object)
  }


newtype ImageBlob = ImageBlob String


newtype ImageId = ImageId String


-- | <p>Identifies face image brightness and sharpness. </p>
newtype ImageQuality = ImageQuality 
  { "Brightness" :: NullOrUndefined (Number)
  , "Sharpness" :: NullOrUndefined (Number)
  }


-- | <p>The input image size exceeds the allowed limit. For more information, see <a>limits</a>. </p>
newtype ImageTooLargeException = ImageTooLargeException 
  { 
  }


newtype IndexFacesRequest = IndexFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "ExternalImageId" :: NullOrUndefined (ExternalImageId)
  , "DetectionAttributes" :: NullOrUndefined (Attributes)
  }


newtype IndexFacesResponse = IndexFacesResponse 
  { "FaceRecords" :: NullOrUndefined (FaceRecordList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }


-- | <p>Amazon Rekognition experienced a service issue. Try your call again.</p>
newtype InternalServerError = InternalServerError 
  { 
  }


-- | <p>The provided image format is not supported. </p>
newtype InvalidImageFormatException = InvalidImageFormatException 
  { 
  }


-- | <p>Pagination token in the request is not valid.</p>
newtype InvalidPaginationTokenException = InvalidPaginationTokenException 
  { 
  }


-- | <p>Input parameter violated a constraint. Validate your parameter before calling the API operation again.</p>
newtype InvalidParameterException = InvalidParameterException 
  { 
  }


-- | <p>Amazon Rekognition is unable to access the S3 object specified in the request.</p>
newtype InvalidS3ObjectException = InvalidS3ObjectException 
  { 
  }


newtype JobId = JobId String


newtype JobTag = JobTag String


newtype KinesisDataArn = KinesisDataArn String


-- | <p>The Kinesis data stream Amazon Rekognition to which the analysis results of a Amazon Rekognition stream processor are streamed. For more information, see .</p>
newtype KinesisDataStream = KinesisDataStream 
  { "Arn" :: NullOrUndefined (KinesisDataArn)
  }


newtype KinesisVideoArn = KinesisVideoArn String


-- | <p>Kinesis video stream stream that provides the source streaming video for a Rekognition Video stream processor. For more information, see .</p>
newtype KinesisVideoStream = KinesisVideoStream 
  { "Arn" :: NullOrUndefined (KinesisVideoArn)
  }


-- | <p>Structure containing details about the detected label, including name, and level of confidence.</p>
newtype Label = Label 
  { "Name" :: NullOrUndefined (String)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Information about a label detected in a video analysis request and the time the label was detected in the video. </p>
newtype LabelDetection = LabelDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Label" :: NullOrUndefined (Label)
  }


newtype LabelDetectionSortBy = LabelDetectionSortBy String


newtype LabelDetections = LabelDetections (Array LabelDetection)


newtype Labels = Labels (Array Label)


-- | <p>Indicates the location of the landmark on the face.</p>
newtype Landmark = Landmark 
  { "Type" :: NullOrUndefined (LandmarkType)
  , "X" :: NullOrUndefined (Number)
  , "Y" :: NullOrUndefined (Number)
  }


newtype LandmarkType = LandmarkType String


newtype Landmarks = Landmarks (Array Landmark)


-- | <p/>
newtype LimitExceededException = LimitExceededException 
  { 
  }


newtype ListCollectionsRequest = ListCollectionsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype ListCollectionsResponse = ListCollectionsResponse 
  { "CollectionIds" :: NullOrUndefined (CollectionIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "FaceModelVersions" :: NullOrUndefined (FaceModelVersionList)
  }


newtype ListFacesRequest = ListFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype ListFacesResponse = ListFacesResponse 
  { "Faces" :: NullOrUndefined (FaceList)
  , "NextToken" :: NullOrUndefined (String)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }


newtype ListStreamProcessorsRequest = ListStreamProcessorsRequest 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListStreamProcessorsResponse = ListStreamProcessorsResponse 
  { "NextToken" :: NullOrUndefined (PaginationToken)
  , "StreamProcessors" :: NullOrUndefined (StreamProcessorList)
  }


newtype MaxFaces = MaxFaces Int


newtype MaxResults = MaxResults Int


-- | <p>Provides information about a single type of moderated content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see <a>moderation</a>.</p>
newtype ModerationLabel = ModerationLabel 
  { "Confidence" :: NullOrUndefined (Percent)
  , "Name" :: NullOrUndefined (String)
  , "ParentName" :: NullOrUndefined (String)
  }


newtype ModerationLabels = ModerationLabels (Array ModerationLabel)


-- | <p>Indicates whether or not the mouth on the face is open, and the confidence level in the determination.</p>
newtype MouthOpen = MouthOpen 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Indicates whether or not the face has a mustache, and the confidence level in the determination.</p>
newtype Mustache = Mustache 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see <a>api-video</a>.</p>
newtype NotificationChannel = NotificationChannel 
  { "SNSTopicArn" :: (SNSTopicArn)
  , "RoleArn" :: (RoleArn)
  }


newtype OrientationCorrection = OrientationCorrection String


newtype PageSize = PageSize Int


newtype PaginationToken = PaginationToken String


newtype Percent = Percent Number


-- | <p>Details about a person detected in a video analysis request.</p>
newtype PersonDetail = PersonDetail 
  { "Index" :: NullOrUndefined (PersonIndex)
  , "BoundingBox" :: NullOrUndefined (BoundingBox)
  , "Face" :: NullOrUndefined (FaceDetail)
  }


-- | <p>Details and tracking information for a single time a person is tracked in a video. Amazon Rekognition operations that track persons return an array of <code>PersonDetection</code> objects with elements for each time a person is tracked in a video. For more information, see . </p>
newtype PersonDetection = PersonDetection 
  { "Number" :: NullOrUndefined (Number)
  , "Person" :: NullOrUndefined (PersonDetail)
  }


newtype PersonDetections = PersonDetections (Array PersonDetection)


newtype PersonIndex = PersonIndex Number


-- | <p>Information about a person whose face matches a face(s) in a Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection (,information about the person (<a>PersonDetail</a>) and the timestamp for when the person was detected in a video. An array of <code>PersonMatch</code> objects is returned by . </p>
newtype PersonMatch = PersonMatch 
  { "Number" :: NullOrUndefined (Number)
  , "Person" :: NullOrUndefined (PersonDetail)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  }


newtype PersonMatches = PersonMatches (Array PersonMatch)


newtype PersonTrackingSortBy = PersonTrackingSortBy String


-- | <p>The X and Y coordinates of a point on an image. The X and Y values returned are ratios of the overall image size. For example, if the input image is 700x200 and the operation returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel coordinate on the image.</p> <p>An array of <code>Point</code> objects, <code>Polygon</code>, is returned by . <code>Polygon</code> represents a fine-grained polygon around detected text. For more information, see . </p>
newtype Point = Point 
  { "X" :: NullOrUndefined (Number)
  , "Y" :: NullOrUndefined (Number)
  }


newtype Polygon = Polygon (Array Point)


-- | <p>Indicates the pose of the face as determined by its pitch, roll, and yaw.</p>
newtype Pose = Pose 
  { "Roll" :: NullOrUndefined (Degree)
  , "Yaw" :: NullOrUndefined (Degree)
  , "Pitch" :: NullOrUndefined (Degree)
  }


-- | <p>The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Rekognition.</p>
newtype ProvisionedThroughputExceededException = ProvisionedThroughputExceededException 
  { 
  }


newtype RecognizeCelebritiesRequest = RecognizeCelebritiesRequest 
  { "Image" :: (Image)
  }


newtype RecognizeCelebritiesResponse = RecognizeCelebritiesResponse 
  { "CelebrityFaces" :: NullOrUndefined (CelebrityList)
  , "UnrecognizedFaces" :: NullOrUndefined (ComparedFaceList)
  , "OrientationCorrection" :: NullOrUndefined (OrientationCorrection)
  }


newtype RekognitionUniqueId = RekognitionUniqueId String


-- | <p>A collection with the specified ID already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }


-- | <p/>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }


-- | <p>The collection specified in the request cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype RoleArn = RoleArn String


newtype S3Bucket = S3Bucket String


-- | <p>Provides the S3 bucket name and object name.</p> <p>The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.</p> <p>For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see <a>manage-access-resource-policies</a>. </p>
newtype S3Object = S3Object 
  { "Bucket" :: NullOrUndefined (S3Bucket)
  , "Name" :: NullOrUndefined (S3ObjectName)
  , "Version" :: NullOrUndefined (S3ObjectVersion)
  }


newtype S3ObjectName = S3ObjectName String


newtype S3ObjectVersion = S3ObjectVersion String


newtype SNSTopicArn = SNSTopicArn String


newtype SearchFacesByImageRequest = SearchFacesByImageRequest 
  { "CollectionId" :: (CollectionId)
  , "Image" :: (Image)
  , "MaxFaces" :: NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }


newtype SearchFacesByImageResponse = SearchFacesByImageResponse 
  { "SearchedFaceBoundingBox" :: NullOrUndefined (BoundingBox)
  , "SearchedFaceConfidence" :: NullOrUndefined (Percent)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }


newtype SearchFacesRequest = SearchFacesRequest 
  { "CollectionId" :: (CollectionId)
  , "FaceId" :: (FaceId)
  , "MaxFaces" :: NullOrUndefined (MaxFaces)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  }


newtype SearchFacesResponse = SearchFacesResponse 
  { "SearchedFaceId" :: NullOrUndefined (FaceId)
  , "FaceMatches" :: NullOrUndefined (FaceMatchList)
  , "FaceModelVersion" :: NullOrUndefined (String)
  }


-- | <p>Indicates whether or not the face is smiling, and the confidence level in the determination.</p>
newtype Smile = Smile 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


newtype StartCelebrityRecognitionRequest = StartCelebrityRecognitionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartCelebrityRecognitionResponse = StartCelebrityRecognitionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartContentModerationRequest = StartContentModerationRequest 
  { "Video" :: (Video)
  , "MinConfidence" :: NullOrUndefined (Percent)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartContentModerationResponse = StartContentModerationResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartFaceDetectionRequest = StartFaceDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "FaceAttributes" :: NullOrUndefined (FaceAttributes)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartFaceDetectionResponse = StartFaceDetectionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartFaceSearchRequest = StartFaceSearchRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "FaceMatchThreshold" :: NullOrUndefined (Percent)
  , "CollectionId" :: (CollectionId)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartFaceSearchResponse = StartFaceSearchResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartLabelDetectionRequest = StartLabelDetectionRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "MinConfidence" :: NullOrUndefined (Percent)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartLabelDetectionResponse = StartLabelDetectionResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartPersonTrackingRequest = StartPersonTrackingRequest 
  { "Video" :: (Video)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "NotificationChannel" :: NullOrUndefined (NotificationChannel)
  , "JobTag" :: NullOrUndefined (JobTag)
  }


newtype StartPersonTrackingResponse = StartPersonTrackingResponse 
  { "JobId" :: NullOrUndefined (JobId)
  }


newtype StartStreamProcessorRequest = StartStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }


newtype StartStreamProcessorResponse = StartStreamProcessorResponse 
  { 
  }


newtype StatusMessage = StatusMessage String


newtype StopStreamProcessorRequest = StopStreamProcessorRequest 
  { "Name" :: (StreamProcessorName)
  }


newtype StopStreamProcessorResponse = StopStreamProcessorResponse 
  { 
  }


-- | <p>An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to . The request parameters for <code>CreateStreamProcessor</code> describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts. </p>
newtype StreamProcessor = StreamProcessor 
  { "Name" :: NullOrUndefined (StreamProcessorName)
  , "Status" :: NullOrUndefined (StreamProcessorStatus)
  }


newtype StreamProcessorArn = StreamProcessorArn String


-- | <p>Information about the source streaming video. </p>
newtype StreamProcessorInput = StreamProcessorInput 
  { "KinesisVideoStream" :: NullOrUndefined (KinesisVideoStream)
  }


newtype StreamProcessorList = StreamProcessorList (Array StreamProcessor)


newtype StreamProcessorName = StreamProcessorName String


-- | <p>Information about the Amazon Kinesis Data Streams stream to which a Rekognition Video stream processor streams the results of a video analysis. For more information, see .</p>
newtype StreamProcessorOutput = StreamProcessorOutput 
  { "KinesisDataStream" :: NullOrUndefined (KinesisDataStream)
  }


-- | <p>Input parameters used to recognize faces in a streaming video analyzed by a Amazon Rekognition stream processor.</p>
newtype StreamProcessorSettings = StreamProcessorSettings 
  { "FaceSearch" :: NullOrUndefined (FaceSearchSettings)
  }


newtype StreamProcessorStatus = StreamProcessorStatus String


-- | <p>Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.</p>
newtype Sunglasses = Sunglasses 
  { "Value" :: NullOrUndefined (Boolean)
  , "Confidence" :: NullOrUndefined (Percent)
  }


-- | <p>Information about a word or line of text detected by .</p> <p>The <code>DetectedText</code> field contains the text that Amazon Rekognition detected in the image. </p> <p>Every word and line has an identifier (<code>Id</code>). Each word belongs to a line and has a parent identifier (<code>ParentId</code>) that identifies the line of text in which the word appears. The word <code>Id</code> is also an index for the word within a line of words. </p> <p>For more information, see <a>text-detection</a>.</p>
newtype TextDetection = TextDetection 
  { "DetectedText" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (TextTypes)
  , "Id" :: NullOrUndefined (UInteger)
  , "ParentId" :: NullOrUndefined (UInteger)
  , "Confidence" :: NullOrUndefined (Percent)
  , "Geometry" :: NullOrUndefined (Geometry)
  }


newtype TextDetectionList = TextDetectionList (Array TextDetection)


newtype TextTypes = TextTypes String


-- | <p>Amazon Rekognition is temporarily unable to process the request. Try your call again.</p>
newtype ThrottlingException = ThrottlingException 
  { 
  }


newtype UInteger = UInteger Int


newtype ULong = ULong Number


newtype Url = Url String


newtype Urls = Urls (Array Url)


-- | <p>Video file stored in an Amazon S3 bucket. Amazon Rekognition video start operations such as use <code>Video</code> to specify a video for analysis. The supported file formats are .mp4, .mov and .avi.</p>
newtype Video = Video 
  { "S3Object" :: NullOrUndefined (S3Object)
  }


newtype VideoJobStatus = VideoJobStatus String


-- | <p>Information about a video that Amazon Rekognition analyzed. <code>Videometadata</code> is returned in every page of paginated responses from a Amazon Rekognition video operation.</p>
newtype VideoMetadata = VideoMetadata 
  { "Codec" :: NullOrUndefined (String)
  , "DurationMillis" :: NullOrUndefined (ULong)
  , "Format" :: NullOrUndefined (String)
  , "FrameRate" :: NullOrUndefined (Number)
  , "FrameHeight" :: NullOrUndefined (ULong)
  , "FrameWidth" :: NullOrUndefined (ULong)
  }


-- | <p>The file size or duration of the supplied media is too large. The maximum file size is 8GB. The maximum duration is 2 hours. </p>
newtype VideoTooLargeException = VideoTooLargeException 
  { 
  }
