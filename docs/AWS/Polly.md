## Module AWS.Polly

<p>Amazon Polly is a web service that makes it easy to synthesize speech from text.</p> <p>The Amazon Polly service provides API operations for synthesizing high-quality speech from plain text and Speech Synthesis Markup Language (SSML), along with managing pronunciations lexicons that enable you to get the best results for your application domain.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteLexicon`

``` purescript
deleteLexicon :: forall eff. DeleteLexiconInput -> Aff (err :: RequestError | eff) DeleteLexiconOutput
```

<p>Deletes the specified pronunciation lexicon stored in an AWS Region. A lexicon which has been deleted is not available for speech synthesis, nor is it possible to retrieve it using either the <code>GetLexicon</code> or <code>ListLexicon</code> APIs.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>

#### `describeVoices`

``` purescript
describeVoices :: forall eff. DescribeVoicesInput -> Aff (err :: RequestError | eff) DescribeVoicesOutput
```

<p>Returns the list of voices that are available for use when requesting speech synthesis. Each voice speaks a specified language, is either male or female, and is identified by an ID, which is the ASCII version of the voice name. </p> <p>When synthesizing speech ( <code>SynthesizeSpeech</code> ), you provide the voice ID for the voice you want from the list of voices returned by <code>DescribeVoices</code>.</p> <p>For example, you want your news reader application to read news in a specific language, but giving a user the option to choose the voice. Using the <code>DescribeVoices</code> operation you can provide the user with a list of available voices to select from.</p> <p> You can optionally specify a language code to filter the available voices. For example, if you specify <code>en-US</code>, the operation returns a list of all available US English voices. </p> <p>This operation requires permissions to perform the <code>polly:DescribeVoices</code> action.</p>

#### `getLexicon`

``` purescript
getLexicon :: forall eff. GetLexiconInput -> Aff (err :: RequestError | eff) GetLexiconOutput
```

<p>Returns the content of the specified pronunciation lexicon stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>

#### `listLexicons`

``` purescript
listLexicons :: forall eff. ListLexiconsInput -> Aff (err :: RequestError | eff) ListLexiconsOutput
```

<p>Returns a list of pronunciation lexicons stored in an AWS Region. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>

#### `putLexicon`

``` purescript
putLexicon :: forall eff. PutLexiconInput -> Aff (err :: RequestError | eff) PutLexiconOutput
```

<p>Stores a pronunciation lexicon in an AWS Region. If a lexicon with the same name already exists in the region, it is overwritten by the new lexicon. Lexicon operations have eventual consistency, therefore, it might take some time before the lexicon is available to the SynthesizeSpeech operation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>

#### `synthesizeSpeech`

``` purescript
synthesizeSpeech :: forall eff. SynthesizeSpeechInput -> Aff (err :: RequestError | eff) SynthesizeSpeechOutput
```

<p>Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML input must be valid, well-formed SSML. Some alphabets might not be available with all the voices (for example, Cyrillic might not be read at all by English voices) unless phoneme mapping is used. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html">How it Works</a>.</p>

#### `Alphabet`

``` purescript
newtype Alphabet
  = Alphabet String
```

#### `AudioStream`

``` purescript
newtype AudioStream
  = AudioStream String
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

#### `DeleteLexiconInput`

``` purescript
newtype DeleteLexiconInput
  = DeleteLexiconInput { "Name" :: LexiconName }
```

#### `DeleteLexiconOutput`

``` purescript
newtype DeleteLexiconOutput
  = DeleteLexiconOutput {  }
```

#### `DescribeVoicesInput`

``` purescript
newtype DescribeVoicesInput
  = DescribeVoicesInput { "LanguageCode" :: NullOrUndefined (LanguageCode), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeVoicesOutput`

``` purescript
newtype DescribeVoicesOutput
  = DescribeVoicesOutput { "Voices" :: NullOrUndefined (VoiceList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `Gender`

``` purescript
newtype Gender
  = Gender String
```

#### `GetLexiconInput`

``` purescript
newtype GetLexiconInput
  = GetLexiconInput { "Name" :: LexiconName }
```

#### `GetLexiconOutput`

``` purescript
newtype GetLexiconOutput
  = GetLexiconOutput { "Lexicon" :: NullOrUndefined (Lexicon), "LexiconAttributes" :: NullOrUndefined (LexiconAttributes) }
```

#### `InvalidLexiconException`

``` purescript
newtype InvalidLexiconException
  = InvalidLexiconException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The NextToken is invalid. Verify that it's spelled correctly, and then try again.</p>

#### `InvalidSampleRateException`

``` purescript
newtype InvalidSampleRateException
  = InvalidSampleRateException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified sample rate is not valid.</p>

#### `InvalidSsmlException`

``` purescript
newtype InvalidSsmlException
  = InvalidSsmlException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.</p>

#### `LanguageCode`

``` purescript
newtype LanguageCode
  = LanguageCode String
```

#### `LanguageName`

``` purescript
newtype LanguageName
  = LanguageName String
```

#### `LastModified`

``` purescript
newtype LastModified
  = LastModified Number
```

#### `LexemesCount`

``` purescript
newtype LexemesCount
  = LexemesCount Int
```

#### `Lexicon`

``` purescript
newtype Lexicon
  = Lexicon { "Content" :: NullOrUndefined (LexiconContent), "Name" :: NullOrUndefined (LexiconName) }
```

<p>Provides lexicon name and lexicon content in string format. For more information, see <a href="https://www.w3.org/TR/pronunciation-lexicon/">Pronunciation Lexicon Specification (PLS) Version 1.0</a>.</p>

#### `LexiconArn`

``` purescript
newtype LexiconArn
  = LexiconArn String
```

#### `LexiconAttributes`

``` purescript
newtype LexiconAttributes
  = LexiconAttributes { "Alphabet" :: NullOrUndefined (Alphabet), "LanguageCode" :: NullOrUndefined (LanguageCode), "LastModified" :: NullOrUndefined (LastModified), "LexiconArn" :: NullOrUndefined (LexiconArn), "LexemesCount" :: NullOrUndefined (LexemesCount), "Size" :: NullOrUndefined (Size) }
```

<p>Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <a href="http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html">Managing Lexicons</a>.</p>

#### `LexiconContent`

``` purescript
newtype LexiconContent
  = LexiconContent String
```

#### `LexiconDescription`

``` purescript
newtype LexiconDescription
  = LexiconDescription { "Name" :: NullOrUndefined (LexiconName), "Attributes" :: NullOrUndefined (LexiconAttributes) }
```

<p>Describes the content of the lexicon.</p>

#### `LexiconDescriptionList`

``` purescript
newtype LexiconDescriptionList
  = LexiconDescriptionList (Array LexiconDescription)
```

#### `LexiconName`

``` purescript
newtype LexiconName
  = LexiconName String
```

#### `LexiconNameList`

``` purescript
newtype LexiconNameList
  = LexiconNameList (Array LexiconName)
```

#### `LexiconNotFoundException`

``` purescript
newtype LexiconNotFoundException
  = LexiconNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.</p> <p>Verify that the lexicon exists, is in the region (see <a>ListLexicons</a>) and that you spelled its name is spelled correctly. Then try again.</p>

#### `LexiconSizeExceededException`

``` purescript
newtype LexiconSizeExceededException
  = LexiconSizeExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum size of the specified lexicon would be exceeded by this operation.</p>

#### `ListLexiconsInput`

``` purescript
newtype ListLexiconsInput
  = ListLexiconsInput { "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListLexiconsOutput`

``` purescript
newtype ListLexiconsOutput
  = ListLexiconsOutput { "Lexicons" :: NullOrUndefined (LexiconDescriptionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `MarksNotSupportedForFormatException`

``` purescript
newtype MarksNotSupportedForFormatException
  = MarksNotSupportedForFormatException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Speech marks are not supported for the <code>OutputFormat</code> selected. Speech marks are only available for content in <code>json</code> format.</p>

#### `MaxLexemeLengthExceededException`

``` purescript
newtype MaxLexemeLengthExceededException
  = MaxLexemeLengthExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum size of the lexeme would be exceeded by this operation.</p>

#### `MaxLexiconsNumberExceededException`

``` purescript
newtype MaxLexiconsNumberExceededException
  = MaxLexiconsNumberExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum number of lexicons would be exceeded by this operation.</p>

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `OutputFormat`

``` purescript
newtype OutputFormat
  = OutputFormat String
```

#### `PutLexiconInput`

``` purescript
newtype PutLexiconInput
  = PutLexiconInput { "Name" :: LexiconName, "Content" :: LexiconContent }
```

#### `PutLexiconOutput`

``` purescript
newtype PutLexiconOutput
  = PutLexiconOutput {  }
```

#### `RequestCharacters`

``` purescript
newtype RequestCharacters
  = RequestCharacters Int
```

#### `SampleRate`

``` purescript
newtype SampleRate
  = SampleRate String
```

#### `ServiceFailureException`

``` purescript
newtype ServiceFailureException
  = ServiceFailureException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An unknown condition has caused a service failure.</p>

#### `Size`

``` purescript
newtype Size
  = Size Int
```

#### `SpeechMarkType`

``` purescript
newtype SpeechMarkType
  = SpeechMarkType String
```

#### `SpeechMarkTypeList`

``` purescript
newtype SpeechMarkTypeList
  = SpeechMarkTypeList (Array SpeechMarkType)
```

#### `SsmlMarksNotSupportedForTextTypeException`

``` purescript
newtype SsmlMarksNotSupportedForTextTypeException
  = SsmlMarksNotSupportedForTextTypeException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>SSML speech marks are not supported for plain text-type input.</p>

#### `SynthesizeSpeechInput`

``` purescript
newtype SynthesizeSpeechInput
  = SynthesizeSpeechInput { "LexiconNames" :: NullOrUndefined (LexiconNameList), "OutputFormat" :: OutputFormat, "SampleRate" :: NullOrUndefined (SampleRate), "SpeechMarkTypes" :: NullOrUndefined (SpeechMarkTypeList), "Text" :: Text, "TextType" :: NullOrUndefined (TextType), "VoiceId" :: VoiceId }
```

#### `SynthesizeSpeechOutput`

``` purescript
newtype SynthesizeSpeechOutput
  = SynthesizeSpeechOutput { "AudioStream" :: NullOrUndefined (AudioStream), "ContentType" :: NullOrUndefined (ContentType), "RequestCharacters" :: NullOrUndefined (RequestCharacters) }
```

#### `Text`

``` purescript
newtype Text
  = Text String
```

#### `TextLengthExceededException`

``` purescript
newtype TextLengthExceededException
  = TextLengthExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The value of the "Text" parameter is longer than the accepted limits. The limit for input text is a maximum of 3000 characters total, of which no more than 1500 can be billed characters. SSML tags are not counted as billed characters.</p>

#### `TextType`

``` purescript
newtype TextType
  = TextType String
```

#### `UnsupportedPlsAlphabetException`

``` purescript
newtype UnsupportedPlsAlphabetException
  = UnsupportedPlsAlphabetException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The alphabet specified by the lexicon is not a supported alphabet. Valid values are <code>x-sampa</code> and <code>ipa</code>.</p>

#### `UnsupportedPlsLanguageException`

``` purescript
newtype UnsupportedPlsLanguageException
  = UnsupportedPlsLanguageException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The language specified in the lexicon is unsupported. For a list of supported languages, see <a href="http://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html">Lexicon Attributes</a>.</p>

#### `Voice`

``` purescript
newtype Voice
  = Voice { "Gender" :: NullOrUndefined (Gender), "Id" :: NullOrUndefined (VoiceId), "LanguageCode" :: NullOrUndefined (LanguageCode), "LanguageName" :: NullOrUndefined (LanguageName), "Name" :: NullOrUndefined (VoiceName) }
```

<p>Description of the voice.</p>

#### `VoiceId`

``` purescript
newtype VoiceId
  = VoiceId String
```

#### `VoiceList`

``` purescript
newtype VoiceList
  = VoiceList (Array Voice)
```

#### `VoiceName`

``` purescript
newtype VoiceName
  = VoiceName String
```


