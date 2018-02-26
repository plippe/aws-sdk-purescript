## Module AWS.Translate

<p>Provides translation between English and one of six languages, or between one of the six languages and English.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `translateText`

``` purescript
translateText :: forall eff. TranslateTextRequest -> Aff (err :: RequestError | eff) TranslateTextResponse
```

<p>Translates input text from the source language to the target language. You can translate between English (en) and one of the following languages, or between one of the following languages and English.</p> <ul> <li> <p>Arabic (ar)</p> </li> <li> <p>Chinese (Simplified) (zh)</p> </li> <li> <p>French (fr)</p> </li> <li> <p>German (de)</p> </li> <li> <p>Portuguese (pt)</p> </li> <li> <p>Spanish (es)</p> </li> </ul>

#### `BoundedLengthString`

``` purescript
newtype BoundedLengthString
  = BoundedLengthString String
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message" :: NullOrUndefined (String) }
```

<p>An internal server error occurred. Retry your request.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message" :: NullOrUndefined (String) }
```

<p>The request is invalid.</p>

#### `LanguageCodeString`

``` purescript
newtype LanguageCodeString
  = LanguageCodeString String
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message" :: NullOrUndefined (String) }
```

<p>Amazon Translate is unavailable. Retry your request later.</p>

#### `TextSizeLimitExceededException`

``` purescript
newtype TextSizeLimitExceededException
  = TextSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The size of the input text exceeds the length constraint for the <code>Text</code> field. Try again with a shorter text. </p>

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

<p>The number of requests exceeds the limit. Resubmit your request later.</p>

#### `TranslateTextRequest`

``` purescript
newtype TranslateTextRequest
  = TranslateTextRequest { "Text" :: BoundedLengthString, "SourceLanguageCode" :: LanguageCodeString, "TargetLanguageCode" :: LanguageCodeString }
```

#### `TranslateTextResponse`

``` purescript
newtype TranslateTextResponse
  = TranslateTextResponse { "TranslatedText" :: String, "SourceLanguageCode" :: LanguageCodeString, "TargetLanguageCode" :: LanguageCodeString }
```

#### `UnsupportedLanguagePairException`

``` purescript
newtype UnsupportedLanguagePairException
  = UnsupportedLanguagePairException { "Message" :: NullOrUndefined (String) }
```

<p>Amazon Translate cannot translate input text in the source language into this target language. For more information, see <a>how-to-error-msg</a>. </p>


