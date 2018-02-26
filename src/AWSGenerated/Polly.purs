

-- | <p>Amazon Polly is a web service that makes it easy to synthesize speech from text.</p> <p>The Amazon Polly service provides API operations for synthesizing high-quality speech from plain text and Speech Synthesis Markup Language (SSML), along with managing pronunciations lexicons that enable you to get the best results for your application domain.</p>
module AWS.Polly where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Polly" :: String


-- | <p>Deletes the specified pronunciation lexicon stored in an AWS Region. A lexicon which has been deleted is not available for speech synthesis, nor is it possible to retrieve it using either the <code>GetLexicon</code> or <code>ListLexicon</code> APIs.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
deleteLexicon :: forall eff. DeleteLexiconInput -> Aff (err :: AWS.RequestError | eff) DeleteLexiconOutput
deleteLexicon = AWS.request serviceName "DeleteLexicon" 


-- | <p>Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name. </p> <p>When synthesizing speech ( <code>SynthesizeSpeech</code> ), you provide the voice ID for the voice you want from the list of voices returned by <code>DescribeVoices</code>.</p> <p>For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the <code>DescribeVoices</code> operation you can provide the user with a list of available voices to select from.</p> <p> You can optionally specify a language code to filter the available voices. For example, if you specify <code>en-US</code>, the operation returns a list of all available US English voices. </p> <p>This operation requires permissions to perform the <code>polly:DescribeVoices</code> action.</p>
describeVoices :: forall eff. DescribeVoicesInput -> Aff (err :: AWS.RequestError | eff) DescribeVoicesOutput
describeVoices = AWS.request serviceName "DescribeVoices" 


-- | <p>Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
getLexicon :: forall eff. GetLexiconInput -> Aff (err :: AWS.RequestError | eff) GetLexiconOutput
getLexicon = AWS.request serviceName "GetLexicon" 


-- | <p>Returns a list of pronunciation lexicons stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
listLexicons :: forall eff. ListLexiconsInput -> Aff (err :: AWS.RequestError | eff) ListLexiconsOutput
listLexicons = AWS.request serviceName "ListLexicons" 


-- | <p>Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
putLexicon :: forall eff. PutLexiconInput -> Aff (err :: AWS.RequestError | eff) PutLexiconOutput
putLexicon = AWS.request serviceName "PutLexicon" 


-- | <p>Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML input must be valid, well-formed SSML. Some alphabets might not be available with all the voices (for example, Cyrillic might not be read at all by English voices) unless phoneme mapping is used. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html">How it Works</a>.</p>
synthesizeSpeech :: forall eff. SynthesizeSpeechInput -> Aff (err :: AWS.RequestError | eff) SynthesizeSpeechOutput
synthesizeSpeech = AWS.request serviceName "SynthesizeSpeech" 


newtype Alphabet = Alphabet String


newtype AudioStream = AudioStream String


newtype ContentType = ContentType String


newtype DeleteLexiconInput = DeleteLexiconInput 
  { "Name" :: (LexiconName)
  }


newtype DeleteLexiconOutput = DeleteLexiconOutput 
  { 
  }


newtype DescribeVoicesInput = DescribeVoicesInput 
  { "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeVoicesOutput = DescribeVoicesOutput 
  { "Voices" :: NullOrUndefined (VoiceList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ErrorMessage = ErrorMessage String


newtype Gender = Gender String


newtype GetLexiconInput = GetLexiconInput 
  { "Name" :: (LexiconName)
  }


newtype GetLexiconOutput = GetLexiconOutput 
  { "Lexicon" :: NullOrUndefined (Lexicon)
  , "LexiconAttributes" :: NullOrUndefined (LexiconAttributes)
  }


-- | <p>Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.</p>
newtype InvalidLexiconException = InvalidLexiconException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The NextToken is invalid. Verify that it's spelled correctly, and then try again.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified sample rate is not valid.</p>
newtype InvalidSampleRateException = InvalidSampleRateException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.</p>
newtype InvalidSsmlException = InvalidSsmlException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype LanguageCode = LanguageCode String


newtype LanguageName = LanguageName String


newtype LastModified = LastModified Number


newtype LexemesCount = LexemesCount Int


-- | <p>Provides lexicon name and lexicon content in string format. For more information, see <a href="https://www.w3.org/TR/pronunciation-lexicon/">Pronunciation Lexicon Specification (PLS) Version 1.0</a>.</p>
newtype Lexicon = Lexicon 
  { "Content" :: NullOrUndefined (LexiconContent)
  , "Name" :: NullOrUndefined (LexiconName)
  }


newtype LexiconArn = LexiconArn String


-- | <p>Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>
newtype LexiconAttributes = LexiconAttributes 
  { "Alphabet" :: NullOrUndefined (Alphabet)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "LexiconArn" :: NullOrUndefined (LexiconArn)
  , "LexemesCount" :: NullOrUndefined (LexemesCount)
  , "Size" :: NullOrUndefined (Size)
  }


newtype LexiconContent = LexiconContent String


-- | <p>Describes the content of the lexicon.</p>
newtype LexiconDescription = LexiconDescription 
  { "Name" :: NullOrUndefined (LexiconName)
  , "Attributes" :: NullOrUndefined (LexiconAttributes)
  }


newtype LexiconDescriptionList = LexiconDescriptionList (Array LexiconDescription)


newtype LexiconName = LexiconName String


newtype LexiconNameList = LexiconNameList (Array LexiconName)


-- | <p>Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.</p> <p>Verify that the lexicon exists, is in the region (see <a>ListLexicons</a>) and that you spelled its name is spelled correctly. Then try again.</p>
newtype LexiconNotFoundException = LexiconNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The maximum size of the specified lexicon would be exceeded by this operation.</p>
newtype LexiconSizeExceededException = LexiconSizeExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ListLexiconsInput = ListLexiconsInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListLexiconsOutput = ListLexiconsOutput 
  { "Lexicons" :: NullOrUndefined (LexiconDescriptionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Speech marks are not supported for the <code>OutputFormat</code> selected. Speech marks are only available for content in <code>json</code> format.</p>
newtype MarksNotSupportedForFormatException = MarksNotSupportedForFormatException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The maximum size of the lexeme would be exceeded by this operation.</p>
newtype MaxLexemeLengthExceededException = MaxLexemeLengthExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The maximum number of lexicons would be exceeded by this operation.</p>
newtype MaxLexiconsNumberExceededException = MaxLexiconsNumberExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype NextToken = NextToken String


newtype OutputFormat = OutputFormat String


newtype PutLexiconInput = PutLexiconInput 
  { "Name" :: (LexiconName)
  , "Content" :: (LexiconContent)
  }


newtype PutLexiconOutput = PutLexiconOutput 
  { 
  }


newtype RequestCharacters = RequestCharacters Int


newtype SampleRate = SampleRate String


-- | <p>An unknown condition has caused a service failure.</p>
newtype ServiceFailureException = ServiceFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype Size = Size Int


newtype SpeechMarkType = SpeechMarkType String


newtype SpeechMarkTypeList = SpeechMarkTypeList (Array SpeechMarkType)


-- | <p>SSML speech marks are not supported for plain text-type input.</p>
newtype SsmlMarksNotSupportedForTextTypeException = SsmlMarksNotSupportedForTextTypeException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype SynthesizeSpeechInput = SynthesizeSpeechInput 
  { "LexiconNames" :: NullOrUndefined (LexiconNameList)
  , "OutputFormat" :: (OutputFormat)
  , "SampleRate" :: NullOrUndefined (SampleRate)
  , "SpeechMarkTypes" :: NullOrUndefined (SpeechMarkTypeList)
  , "Text" :: (Text)
  , "TextType" :: NullOrUndefined (TextType)
  , "VoiceId" :: (VoiceId)
  }


newtype SynthesizeSpeechOutput = SynthesizeSpeechOutput 
  { "AudioStream" :: NullOrUndefined (AudioStream)
  , "ContentType" :: NullOrUndefined (ContentType)
  , "RequestCharacters" :: NullOrUndefined (RequestCharacters)
  }


newtype Text = Text String


-- | <p>The value of the "Text" parameter is longer than the accepted limits. The limit for input text is a maximum of 3000 characters total, of which no more than 1500 can be billed characters. SSML tags are not counted as billed characters.</p>
newtype TextLengthExceededException = TextLengthExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype TextType = TextType String


-- | <p>The alphabet specified by the lexicon is not a supported alphabet. Valid values are <code>x-sampa</code> and <code>ipa</code>.</p>
newtype UnsupportedPlsAlphabetException = UnsupportedPlsAlphabetException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The language specified in the lexicon is unsupported. For a list of supported languages, see <a href="http://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html">Lexicon Attributes</a>.</p>
newtype UnsupportedPlsLanguageException = UnsupportedPlsLanguageException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Description of the voice.</p>
newtype Voice = Voice 
  { "Gender" :: NullOrUndefined (Gender)
  , "Id" :: NullOrUndefined (VoiceId)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LanguageName" :: NullOrUndefined (LanguageName)
  , "Name" :: NullOrUndefined (VoiceName)
  }


newtype VoiceId = VoiceId String


newtype VoiceList = VoiceList (Array Voice)


newtype VoiceName = VoiceName String
