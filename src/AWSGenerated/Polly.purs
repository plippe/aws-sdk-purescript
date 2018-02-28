

-- | <p>Amazon Polly is a web service that makes it easy to synthesize speech from text.</p> <p>The Amazon Polly service provides API operations for synthesizing high-quality speech from plain text and Speech Synthesis Markup Language (SSML), along with managing pronunciations lexicons that enable you to get the best results for your application domain.</p>
module AWS.Polly where

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

serviceName = "Polly" :: String


-- | <p>Deletes the specified pronunciation lexicon stored in an AWS Region. A lexicon which has been deleted is not available for speech synthesis, nor is it possible to retrieve it using either the <code>GetLexicon</code> or <code>ListLexicon</code> APIs.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
deleteLexicon :: forall eff. DeleteLexiconInput -> Aff (exception :: EXCEPTION | eff) DeleteLexiconOutput
deleteLexicon = Request.request serviceName "deleteLexicon" 


-- | <p>Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name. </p> <p>When synthesizing speech ( <code>SynthesizeSpeech</code> ), you provide the voice ID for the voice you want from the list of voices returned by <code>DescribeVoices</code>.</p> <p>For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the <code>DescribeVoices</code> operation you can provide the user with a list of available voices to select from.</p> <p> You can optionally specify a language code to filter the available voices. For example, if you specify <code>en-US</code>, the operation returns a list of all available US English voices. </p> <p>This operation requires permissions to perform the <code>polly:DescribeVoices</code> action.</p>
describeVoices :: forall eff. DescribeVoicesInput -> Aff (exception :: EXCEPTION | eff) DescribeVoicesOutput
describeVoices = Request.request serviceName "describeVoices" 


-- | <p>Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
getLexicon :: forall eff. GetLexiconInput -> Aff (exception :: EXCEPTION | eff) GetLexiconOutput
getLexicon = Request.request serviceName "getLexicon" 


-- | <p>Returns a list of pronunciation lexicons stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
listLexicons :: forall eff. ListLexiconsInput -> Aff (exception :: EXCEPTION | eff) ListLexiconsOutput
listLexicons = Request.request serviceName "listLexicons" 


-- | <p>Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
putLexicon :: forall eff. PutLexiconInput -> Aff (exception :: EXCEPTION | eff) PutLexiconOutput
putLexicon = Request.request serviceName "putLexicon" 


-- | <p>Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML input must be valid, well-formed SSML. Some alphabets might not be available with all the voices (for example, Cyrillic might not be read at all by English voices) unless phoneme mapping is used. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html">How it Works</a>.</p>
synthesizeSpeech :: forall eff. SynthesizeSpeechInput -> Aff (exception :: EXCEPTION | eff) SynthesizeSpeechOutput
synthesizeSpeech = Request.request serviceName "synthesizeSpeech" 


newtype Alphabet = Alphabet String
derive instance newtypeAlphabet :: Newtype Alphabet _
derive instance repGenericAlphabet :: Generic Alphabet _
instance showAlphabet :: Show Alphabet where
  show = genericShow
instance decodeAlphabet :: Decode Alphabet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlphabet :: Encode Alphabet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioStream = AudioStream String
derive instance newtypeAudioStream :: Newtype AudioStream _
derive instance repGenericAudioStream :: Generic AudioStream _
instance showAudioStream :: Show AudioStream where
  show = genericShow
instance decodeAudioStream :: Decode AudioStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioStream :: Encode AudioStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
instance decodeContentType :: Decode ContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentType :: Encode ContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLexiconInput = DeleteLexiconInput 
  { "Name" :: (LexiconName)
  }
derive instance newtypeDeleteLexiconInput :: Newtype DeleteLexiconInput _
derive instance repGenericDeleteLexiconInput :: Generic DeleteLexiconInput _
instance showDeleteLexiconInput :: Show DeleteLexiconInput where
  show = genericShow
instance decodeDeleteLexiconInput :: Decode DeleteLexiconInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLexiconInput :: Encode DeleteLexiconInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLexiconOutput = DeleteLexiconOutput Types.NoArguments
derive instance newtypeDeleteLexiconOutput :: Newtype DeleteLexiconOutput _
derive instance repGenericDeleteLexiconOutput :: Generic DeleteLexiconOutput _
instance showDeleteLexiconOutput :: Show DeleteLexiconOutput where
  show = genericShow
instance decodeDeleteLexiconOutput :: Decode DeleteLexiconOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLexiconOutput :: Encode DeleteLexiconOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeVoicesInput = DescribeVoicesInput 
  { "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeVoicesInput :: Newtype DescribeVoicesInput _
derive instance repGenericDescribeVoicesInput :: Generic DescribeVoicesInput _
instance showDescribeVoicesInput :: Show DescribeVoicesInput where
  show = genericShow
instance decodeDescribeVoicesInput :: Decode DescribeVoicesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVoicesInput :: Encode DescribeVoicesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeVoicesOutput = DescribeVoicesOutput 
  { "Voices" :: NullOrUndefined.NullOrUndefined (VoiceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeVoicesOutput :: Newtype DescribeVoicesOutput _
derive instance repGenericDescribeVoicesOutput :: Generic DescribeVoicesOutput _
instance showDescribeVoicesOutput :: Show DescribeVoicesOutput where
  show = genericShow
instance decodeDescribeVoicesOutput :: Decode DescribeVoicesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeVoicesOutput :: Encode DescribeVoicesOutput where
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


newtype Gender = Gender String
derive instance newtypeGender :: Newtype Gender _
derive instance repGenericGender :: Generic Gender _
instance showGender :: Show Gender where
  show = genericShow
instance decodeGender :: Decode Gender where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGender :: Encode Gender where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLexiconInput = GetLexiconInput 
  { "Name" :: (LexiconName)
  }
derive instance newtypeGetLexiconInput :: Newtype GetLexiconInput _
derive instance repGenericGetLexiconInput :: Generic GetLexiconInput _
instance showGetLexiconInput :: Show GetLexiconInput where
  show = genericShow
instance decodeGetLexiconInput :: Decode GetLexiconInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLexiconInput :: Encode GetLexiconInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLexiconOutput = GetLexiconOutput 
  { "Lexicon" :: NullOrUndefined.NullOrUndefined (Lexicon)
  , "LexiconAttributes" :: NullOrUndefined.NullOrUndefined (LexiconAttributes)
  }
derive instance newtypeGetLexiconOutput :: Newtype GetLexiconOutput _
derive instance repGenericGetLexiconOutput :: Generic GetLexiconOutput _
instance showGetLexiconOutput :: Show GetLexiconOutput where
  show = genericShow
instance decodeGetLexiconOutput :: Decode GetLexiconOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLexiconOutput :: Encode GetLexiconOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.</p>
newtype InvalidLexiconException = InvalidLexiconException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidLexiconException :: Newtype InvalidLexiconException _
derive instance repGenericInvalidLexiconException :: Generic InvalidLexiconException _
instance showInvalidLexiconException :: Show InvalidLexiconException where
  show = genericShow
instance decodeInvalidLexiconException :: Decode InvalidLexiconException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLexiconException :: Encode InvalidLexiconException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The NextToken is invalid. Verify that it's spelled correctly, and then try again.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified sample rate is not valid.</p>
newtype InvalidSampleRateException = InvalidSampleRateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidSampleRateException :: Newtype InvalidSampleRateException _
derive instance repGenericInvalidSampleRateException :: Generic InvalidSampleRateException _
instance showInvalidSampleRateException :: Show InvalidSampleRateException where
  show = genericShow
instance decodeInvalidSampleRateException :: Decode InvalidSampleRateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSampleRateException :: Encode InvalidSampleRateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.</p>
newtype InvalidSsmlException = InvalidSsmlException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidSsmlException :: Newtype InvalidSsmlException _
derive instance repGenericInvalidSsmlException :: Generic InvalidSsmlException _
instance showInvalidSsmlException :: Show InvalidSsmlException where
  show = genericShow
instance decodeInvalidSsmlException :: Decode InvalidSsmlException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSsmlException :: Encode InvalidSsmlException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LanguageCode = LanguageCode String
derive instance newtypeLanguageCode :: Newtype LanguageCode _
derive instance repGenericLanguageCode :: Generic LanguageCode _
instance showLanguageCode :: Show LanguageCode where
  show = genericShow
instance decodeLanguageCode :: Decode LanguageCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguageCode :: Encode LanguageCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LanguageName = LanguageName String
derive instance newtypeLanguageName :: Newtype LanguageName _
derive instance repGenericLanguageName :: Generic LanguageName _
instance showLanguageName :: Show LanguageName where
  show = genericShow
instance decodeLanguageName :: Decode LanguageName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguageName :: Encode LanguageName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastModified = LastModified Number
derive instance newtypeLastModified :: Newtype LastModified _
derive instance repGenericLastModified :: Generic LastModified _
instance showLastModified :: Show LastModified where
  show = genericShow
instance decodeLastModified :: Decode LastModified where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastModified :: Encode LastModified where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexemesCount = LexemesCount Int
derive instance newtypeLexemesCount :: Newtype LexemesCount _
derive instance repGenericLexemesCount :: Generic LexemesCount _
instance showLexemesCount :: Show LexemesCount where
  show = genericShow
instance decodeLexemesCount :: Decode LexemesCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexemesCount :: Encode LexemesCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides lexicon name and lexicon content in string format. For more information, see <a href="https://www.w3.org/TR/pronunciation-lexicon/">Pronunciation Lexicon Specification (PLS) Version 1.0</a>.</p>
newtype Lexicon = Lexicon 
  { "Content" :: NullOrUndefined.NullOrUndefined (LexiconContent)
  , "Name" :: NullOrUndefined.NullOrUndefined (LexiconName)
  }
derive instance newtypeLexicon :: Newtype Lexicon _
derive instance repGenericLexicon :: Generic Lexicon _
instance showLexicon :: Show Lexicon where
  show = genericShow
instance decodeLexicon :: Decode Lexicon where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexicon :: Encode Lexicon where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexiconArn = LexiconArn String
derive instance newtypeLexiconArn :: Newtype LexiconArn _
derive instance repGenericLexiconArn :: Generic LexiconArn _
instance showLexiconArn :: Show LexiconArn where
  show = genericShow
instance decodeLexiconArn :: Decode LexiconArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconArn :: Encode LexiconArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
newtype LexiconAttributes = LexiconAttributes 
  { "Alphabet" :: NullOrUndefined.NullOrUndefined (Alphabet)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "LexiconArn" :: NullOrUndefined.NullOrUndefined (LexiconArn)
  , "LexemesCount" :: NullOrUndefined.NullOrUndefined (LexemesCount)
  , "Size" :: NullOrUndefined.NullOrUndefined (Size)
  }
derive instance newtypeLexiconAttributes :: Newtype LexiconAttributes _
derive instance repGenericLexiconAttributes :: Generic LexiconAttributes _
instance showLexiconAttributes :: Show LexiconAttributes where
  show = genericShow
instance decodeLexiconAttributes :: Decode LexiconAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconAttributes :: Encode LexiconAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexiconContent = LexiconContent String
derive instance newtypeLexiconContent :: Newtype LexiconContent _
derive instance repGenericLexiconContent :: Generic LexiconContent _
instance showLexiconContent :: Show LexiconContent where
  show = genericShow
instance decodeLexiconContent :: Decode LexiconContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconContent :: Encode LexiconContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the content of the lexicon.</p>
newtype LexiconDescription = LexiconDescription 
  { "Name" :: NullOrUndefined.NullOrUndefined (LexiconName)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (LexiconAttributes)
  }
derive instance newtypeLexiconDescription :: Newtype LexiconDescription _
derive instance repGenericLexiconDescription :: Generic LexiconDescription _
instance showLexiconDescription :: Show LexiconDescription where
  show = genericShow
instance decodeLexiconDescription :: Decode LexiconDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconDescription :: Encode LexiconDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexiconDescriptionList = LexiconDescriptionList (Array LexiconDescription)
derive instance newtypeLexiconDescriptionList :: Newtype LexiconDescriptionList _
derive instance repGenericLexiconDescriptionList :: Generic LexiconDescriptionList _
instance showLexiconDescriptionList :: Show LexiconDescriptionList where
  show = genericShow
instance decodeLexiconDescriptionList :: Decode LexiconDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconDescriptionList :: Encode LexiconDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexiconName = LexiconName String
derive instance newtypeLexiconName :: Newtype LexiconName _
derive instance repGenericLexiconName :: Generic LexiconName _
instance showLexiconName :: Show LexiconName where
  show = genericShow
instance decodeLexiconName :: Decode LexiconName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconName :: Encode LexiconName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LexiconNameList = LexiconNameList (Array LexiconName)
derive instance newtypeLexiconNameList :: Newtype LexiconNameList _
derive instance repGenericLexiconNameList :: Generic LexiconNameList _
instance showLexiconNameList :: Show LexiconNameList where
  show = genericShow
instance decodeLexiconNameList :: Decode LexiconNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconNameList :: Encode LexiconNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.</p> <p>Verify that the lexicon exists, is in the region (see <a>ListLexicons</a>) and that you spelled its name is spelled correctly. Then try again.</p>
newtype LexiconNotFoundException = LexiconNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLexiconNotFoundException :: Newtype LexiconNotFoundException _
derive instance repGenericLexiconNotFoundException :: Generic LexiconNotFoundException _
instance showLexiconNotFoundException :: Show LexiconNotFoundException where
  show = genericShow
instance decodeLexiconNotFoundException :: Decode LexiconNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconNotFoundException :: Encode LexiconNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum size of the specified lexicon would be exceeded by this operation.</p>
newtype LexiconSizeExceededException = LexiconSizeExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLexiconSizeExceededException :: Newtype LexiconSizeExceededException _
derive instance repGenericLexiconSizeExceededException :: Generic LexiconSizeExceededException _
instance showLexiconSizeExceededException :: Show LexiconSizeExceededException where
  show = genericShow
instance decodeLexiconSizeExceededException :: Decode LexiconSizeExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLexiconSizeExceededException :: Encode LexiconSizeExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLexiconsInput = ListLexiconsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListLexiconsInput :: Newtype ListLexiconsInput _
derive instance repGenericListLexiconsInput :: Generic ListLexiconsInput _
instance showListLexiconsInput :: Show ListLexiconsInput where
  show = genericShow
instance decodeListLexiconsInput :: Decode ListLexiconsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLexiconsInput :: Encode ListLexiconsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListLexiconsOutput = ListLexiconsOutput 
  { "Lexicons" :: NullOrUndefined.NullOrUndefined (LexiconDescriptionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListLexiconsOutput :: Newtype ListLexiconsOutput _
derive instance repGenericListLexiconsOutput :: Generic ListLexiconsOutput _
instance showListLexiconsOutput :: Show ListLexiconsOutput where
  show = genericShow
instance decodeListLexiconsOutput :: Decode ListLexiconsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListLexiconsOutput :: Encode ListLexiconsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Speech marks are not supported for the <code>OutputFormat</code> selected. Speech marks are only available for content in <code>json</code> format.</p>
newtype MarksNotSupportedForFormatException = MarksNotSupportedForFormatException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMarksNotSupportedForFormatException :: Newtype MarksNotSupportedForFormatException _
derive instance repGenericMarksNotSupportedForFormatException :: Generic MarksNotSupportedForFormatException _
instance showMarksNotSupportedForFormatException :: Show MarksNotSupportedForFormatException where
  show = genericShow
instance decodeMarksNotSupportedForFormatException :: Decode MarksNotSupportedForFormatException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarksNotSupportedForFormatException :: Encode MarksNotSupportedForFormatException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum size of the lexeme would be exceeded by this operation.</p>
newtype MaxLexemeLengthExceededException = MaxLexemeLengthExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMaxLexemeLengthExceededException :: Newtype MaxLexemeLengthExceededException _
derive instance repGenericMaxLexemeLengthExceededException :: Generic MaxLexemeLengthExceededException _
instance showMaxLexemeLengthExceededException :: Show MaxLexemeLengthExceededException where
  show = genericShow
instance decodeMaxLexemeLengthExceededException :: Decode MaxLexemeLengthExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxLexemeLengthExceededException :: Encode MaxLexemeLengthExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of lexicons would be exceeded by this operation.</p>
newtype MaxLexiconsNumberExceededException = MaxLexiconsNumberExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMaxLexiconsNumberExceededException :: Newtype MaxLexiconsNumberExceededException _
derive instance repGenericMaxLexiconsNumberExceededException :: Generic MaxLexiconsNumberExceededException _
instance showMaxLexiconsNumberExceededException :: Show MaxLexiconsNumberExceededException where
  show = genericShow
instance decodeMaxLexiconsNumberExceededException :: Decode MaxLexiconsNumberExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxLexiconsNumberExceededException :: Encode MaxLexiconsNumberExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputFormat = OutputFormat String
derive instance newtypeOutputFormat :: Newtype OutputFormat _
derive instance repGenericOutputFormat :: Generic OutputFormat _
instance showOutputFormat :: Show OutputFormat where
  show = genericShow
instance decodeOutputFormat :: Decode OutputFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputFormat :: Encode OutputFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutLexiconInput = PutLexiconInput 
  { "Name" :: (LexiconName)
  , "Content" :: (LexiconContent)
  }
derive instance newtypePutLexiconInput :: Newtype PutLexiconInput _
derive instance repGenericPutLexiconInput :: Generic PutLexiconInput _
instance showPutLexiconInput :: Show PutLexiconInput where
  show = genericShow
instance decodePutLexiconInput :: Decode PutLexiconInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutLexiconInput :: Encode PutLexiconInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutLexiconOutput = PutLexiconOutput Types.NoArguments
derive instance newtypePutLexiconOutput :: Newtype PutLexiconOutput _
derive instance repGenericPutLexiconOutput :: Generic PutLexiconOutput _
instance showPutLexiconOutput :: Show PutLexiconOutput where
  show = genericShow
instance decodePutLexiconOutput :: Decode PutLexiconOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutLexiconOutput :: Encode PutLexiconOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCharacters = RequestCharacters Int
derive instance newtypeRequestCharacters :: Newtype RequestCharacters _
derive instance repGenericRequestCharacters :: Generic RequestCharacters _
instance showRequestCharacters :: Show RequestCharacters where
  show = genericShow
instance decodeRequestCharacters :: Decode RequestCharacters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCharacters :: Encode RequestCharacters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SampleRate = SampleRate String
derive instance newtypeSampleRate :: Newtype SampleRate _
derive instance repGenericSampleRate :: Generic SampleRate _
instance showSampleRate :: Show SampleRate where
  show = genericShow
instance decodeSampleRate :: Decode SampleRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampleRate :: Encode SampleRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An unknown condition has caused a service failure.</p>
newtype ServiceFailureException = ServiceFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceFailureException :: Newtype ServiceFailureException _
derive instance repGenericServiceFailureException :: Generic ServiceFailureException _
instance showServiceFailureException :: Show ServiceFailureException where
  show = genericShow
instance decodeServiceFailureException :: Decode ServiceFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceFailureException :: Encode ServiceFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Size = Size Int
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where
  show = genericShow
instance decodeSize :: Decode Size where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSize :: Encode Size where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SpeechMarkType = SpeechMarkType String
derive instance newtypeSpeechMarkType :: Newtype SpeechMarkType _
derive instance repGenericSpeechMarkType :: Generic SpeechMarkType _
instance showSpeechMarkType :: Show SpeechMarkType where
  show = genericShow
instance decodeSpeechMarkType :: Decode SpeechMarkType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpeechMarkType :: Encode SpeechMarkType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SpeechMarkTypeList = SpeechMarkTypeList (Array SpeechMarkType)
derive instance newtypeSpeechMarkTypeList :: Newtype SpeechMarkTypeList _
derive instance repGenericSpeechMarkTypeList :: Generic SpeechMarkTypeList _
instance showSpeechMarkTypeList :: Show SpeechMarkTypeList where
  show = genericShow
instance decodeSpeechMarkTypeList :: Decode SpeechMarkTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpeechMarkTypeList :: Encode SpeechMarkTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>SSML speech marks are not supported for plain text-type input.</p>
newtype SsmlMarksNotSupportedForTextTypeException = SsmlMarksNotSupportedForTextTypeException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeSsmlMarksNotSupportedForTextTypeException :: Newtype SsmlMarksNotSupportedForTextTypeException _
derive instance repGenericSsmlMarksNotSupportedForTextTypeException :: Generic SsmlMarksNotSupportedForTextTypeException _
instance showSsmlMarksNotSupportedForTextTypeException :: Show SsmlMarksNotSupportedForTextTypeException where
  show = genericShow
instance decodeSsmlMarksNotSupportedForTextTypeException :: Decode SsmlMarksNotSupportedForTextTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSsmlMarksNotSupportedForTextTypeException :: Encode SsmlMarksNotSupportedForTextTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SynthesizeSpeechInput = SynthesizeSpeechInput 
  { "LexiconNames" :: NullOrUndefined.NullOrUndefined (LexiconNameList)
  , "OutputFormat" :: (OutputFormat)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (SampleRate)
  , "SpeechMarkTypes" :: NullOrUndefined.NullOrUndefined (SpeechMarkTypeList)
  , "Text" :: (Text)
  , "TextType" :: NullOrUndefined.NullOrUndefined (TextType)
  , "VoiceId" :: (VoiceId)
  }
derive instance newtypeSynthesizeSpeechInput :: Newtype SynthesizeSpeechInput _
derive instance repGenericSynthesizeSpeechInput :: Generic SynthesizeSpeechInput _
instance showSynthesizeSpeechInput :: Show SynthesizeSpeechInput where
  show = genericShow
instance decodeSynthesizeSpeechInput :: Decode SynthesizeSpeechInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSynthesizeSpeechInput :: Encode SynthesizeSpeechInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SynthesizeSpeechOutput = SynthesizeSpeechOutput 
  { "AudioStream" :: NullOrUndefined.NullOrUndefined (AudioStream)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "RequestCharacters" :: NullOrUndefined.NullOrUndefined (RequestCharacters)
  }
derive instance newtypeSynthesizeSpeechOutput :: Newtype SynthesizeSpeechOutput _
derive instance repGenericSynthesizeSpeechOutput :: Generic SynthesizeSpeechOutput _
instance showSynthesizeSpeechOutput :: Show SynthesizeSpeechOutput where
  show = genericShow
instance decodeSynthesizeSpeechOutput :: Decode SynthesizeSpeechOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSynthesizeSpeechOutput :: Encode SynthesizeSpeechOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Text = Text String
derive instance newtypeText :: Newtype Text _
derive instance repGenericText :: Generic Text _
instance showText :: Show Text where
  show = genericShow
instance decodeText :: Decode Text where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeText :: Encode Text where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of the "Text" parameter is longer than the accepted limits. The limit for input text is a maximum of 3000 characters total, of which no more than 1500 can be billed characters. SSML tags are not counted as billed characters.</p>
newtype TextLengthExceededException = TextLengthExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTextLengthExceededException :: Newtype TextLengthExceededException _
derive instance repGenericTextLengthExceededException :: Generic TextLengthExceededException _
instance showTextLengthExceededException :: Show TextLengthExceededException where
  show = genericShow
instance decodeTextLengthExceededException :: Decode TextLengthExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextLengthExceededException :: Encode TextLengthExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TextType = TextType String
derive instance newtypeTextType :: Newtype TextType _
derive instance repGenericTextType :: Generic TextType _
instance showTextType :: Show TextType where
  show = genericShow
instance decodeTextType :: Decode TextType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTextType :: Encode TextType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The alphabet specified by the lexicon is not a supported alphabet. Valid values are <code>x-sampa</code> and <code>ipa</code>.</p>
newtype UnsupportedPlsAlphabetException = UnsupportedPlsAlphabetException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnsupportedPlsAlphabetException :: Newtype UnsupportedPlsAlphabetException _
derive instance repGenericUnsupportedPlsAlphabetException :: Generic UnsupportedPlsAlphabetException _
instance showUnsupportedPlsAlphabetException :: Show UnsupportedPlsAlphabetException where
  show = genericShow
instance decodeUnsupportedPlsAlphabetException :: Decode UnsupportedPlsAlphabetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedPlsAlphabetException :: Encode UnsupportedPlsAlphabetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The language specified in the lexicon is unsupported. For a list of supported languages, see <a href="http://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html">Lexicon Attributes</a>.</p>
newtype UnsupportedPlsLanguageException = UnsupportedPlsLanguageException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnsupportedPlsLanguageException :: Newtype UnsupportedPlsLanguageException _
derive instance repGenericUnsupportedPlsLanguageException :: Generic UnsupportedPlsLanguageException _
instance showUnsupportedPlsLanguageException :: Show UnsupportedPlsLanguageException where
  show = genericShow
instance decodeUnsupportedPlsLanguageException :: Decode UnsupportedPlsLanguageException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedPlsLanguageException :: Encode UnsupportedPlsLanguageException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Description of the voice.</p>
newtype Voice = Voice 
  { "Gender" :: NullOrUndefined.NullOrUndefined (Gender)
  , "Id" :: NullOrUndefined.NullOrUndefined (VoiceId)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LanguageName" :: NullOrUndefined.NullOrUndefined (LanguageName)
  , "Name" :: NullOrUndefined.NullOrUndefined (VoiceName)
  }
derive instance newtypeVoice :: Newtype Voice _
derive instance repGenericVoice :: Generic Voice _
instance showVoice :: Show Voice where
  show = genericShow
instance decodeVoice :: Decode Voice where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVoice :: Encode Voice where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VoiceId = VoiceId String
derive instance newtypeVoiceId :: Newtype VoiceId _
derive instance repGenericVoiceId :: Generic VoiceId _
instance showVoiceId :: Show VoiceId where
  show = genericShow
instance decodeVoiceId :: Decode VoiceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVoiceId :: Encode VoiceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VoiceList = VoiceList (Array Voice)
derive instance newtypeVoiceList :: Newtype VoiceList _
derive instance repGenericVoiceList :: Generic VoiceList _
instance showVoiceList :: Show VoiceList where
  show = genericShow
instance decodeVoiceList :: Decode VoiceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVoiceList :: Encode VoiceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VoiceName = VoiceName String
derive instance newtypeVoiceName :: Newtype VoiceName _
derive instance repGenericVoiceName :: Generic VoiceName _
instance showVoiceName :: Show VoiceName where
  show = genericShow
instance decodeVoiceName :: Decode VoiceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVoiceName :: Encode VoiceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
