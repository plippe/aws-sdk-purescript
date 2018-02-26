

-- | <p>Provides translation between English and one of six languages, or between one of the six languages and English.</p>
module AWS.Translate where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Translate" :: String


-- | <p>Translates input text from the source language to the target language. You can translate between English (en) and one of the following languages, or between one of the following languages and English.</p> <ul> <li> <p>Arabic (ar)</p> </li> <li> <p>Chinese (Simplified) (zh)</p> </li> <li> <p>French (fr)</p> </li> <li> <p>German (de)</p> </li> <li> <p>Portuguese (pt)</p> </li> <li> <p>Spanish (es)</p> </li> </ul>
translateText :: forall eff. TranslateTextRequest -> Aff (err :: AWS.RequestError | eff) TranslateTextResponse
translateText = AWS.request serviceName "TranslateText" 


newtype BoundedLengthString = BoundedLengthString String


-- | <p>An internal server error occurred. Retry your request.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The request is invalid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined (String)
  }


newtype LanguageCodeString = LanguageCodeString String


-- | <p>Amazon Translate is unavailable. Retry your request later.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The size of the input text exceeds the length constraint for the <code>Text</code> field. Try again with a shorter text. </p>
newtype TextSizeLimitExceededException = TextSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The number of requests exceeds the limit. Resubmit your request later.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }


newtype TranslateTextRequest = TranslateTextRequest 
  { "Text" :: (BoundedLengthString)
  , "SourceLanguageCode" :: (LanguageCodeString)
  , "TargetLanguageCode" :: (LanguageCodeString)
  }


newtype TranslateTextResponse = TranslateTextResponse 
  { "TranslatedText" :: (String)
  , "SourceLanguageCode" :: (LanguageCodeString)
  , "TargetLanguageCode" :: (LanguageCodeString)
  }


-- | <p>Amazon Translate cannot translate input text in the source language into this target language. For more information, see <a>how-to-error-msg</a>. </p>
newtype UnsupportedLanguagePairException = UnsupportedLanguagePairException 
  { "Message" :: NullOrUndefined (String)
  }
