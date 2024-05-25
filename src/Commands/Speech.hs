{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Commands.Speech (speechCmd) where

import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)

{-
import qualified Data.ByteString as BS
import Network.HTTP.Simple ( getResponseBody, getResponseStatus )

import OpenAI.Common (runWithConfiguration, Configuration(..), Nullable (..), SecurityScheme (..))
import OpenAI.SecuritySchemes (bearerAuthenticationSecurityScheme)
import OpenAI.Configuration (defaultConfiguration)
import qualified OpenAI as OAI
-}

import OpenAI (think)
import Context (Context(..), Result (..))
import Action (Action(..))
import Actions.Audio (SpeechParams(..), AudioVerb(..))


import qualified Options.Runtime as Rto

speechCmd :: FilePath -> Text -> Maybe Text -> Maybe FilePath -> Rto.RunOptions -> IO ()
speechCmd outputPath narrator mbPrompt mbFile rtOpts = do
  putStrLn "@[run] starting."
  case rtOpts.apiKey of
    Nothing -> error "No OPENAI api key"
    Just aKey ->
      if rtOpts.model == "tts-1" || rtOpts.model == "tts-1-hd" then do
        userPrompt <-
          case mbFile of
            Nothing -> pure $ case mbPrompt of Nothing -> ""; Just aText -> aText
            Just aPath -> do
              fileText <- T.readFile aPath
              case mbPrompt of
                Nothing -> pure fileText
                Just aText -> pure (fileText <> "\n\n" <> aText)
        doSpeech rtOpts aKey narrator userPrompt outputPath
      else
        putStrLn $ "@[speechCmd] model " <> show rtOpts.model <> " is not supported for speech generation."


doSpeech :: Rto.RunOptions -> Text -> Text -> Text -> FilePath -> IO ()
doSpeech rtOpts aKey narrator userPrompt outputPath =
  if userPrompt == "" then
    putStrLn $ "@[doSpeech] err: empty prompt.\n"
  else
    let
      oaiContext = Simple aKey rtOpts.model
      action = Speech $ PromptPath userPrompt outputPath narrator
    in do
    rezA <- think $ Audio action oaiContext
    case rezA of
      Left errMsg ->
        putStrLn $ "@[doSpeech] err: " <> errMsg <> ".\n"
      Right NilResult -> pure ()
      _ -> putStrLn $ "@[doSpeech] got a response but it's not valid result.\n"


{-
doSpeech :: Rto.RunOptions -> Text -> Text -> FilePath -> IO ()
doSpeech rtOpts aKey userPrompt outputPath =
  let
    defaultConf = defaultConfiguration { configSecurityScheme = bearerAuthenticationSecurityScheme aKey }
    modelID = OAI.CreateSpeechRequestModel'Text rtOpts.model
    request = OAI.mkCreateSpeechRequest userPrompt modelID OAI.CreateSpeechRequestVoice'EnumNova
  in do
  rezA <- runWithConfiguration defaultConf $ OAI.createSpeech request
  let
    stCode = getResponseStatus rezA
  case stCode of
    ok200 ->
      case getResponseBody rezA of
        OAI.CreateSpeechResponseError errMsg ->
          putStrLn $ "@[speechCmd] err: CreateSpeechResponseError " <> errMsg
        OAI.CreateSpeechResponse200 content ->
          BS.writeFile outputPath content
    _ -> putStrLn $ "@[speechCmd] err: " <> show rezA
  pure ()

showNullable :: Show a => Nullable a -> String
showNullable aVal =
  case aVal of
    NonNull a -> show a
    Null -> "null"
-}
