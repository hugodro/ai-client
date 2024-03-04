{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Commands.Complete (completeCmd) where

{-
import Data.Text (unpack)
import Network.HTTP.Simple ( getResponseBody, getResponseStatus )

import OpenAI.Common (runWithConfiguration, Configuration(..), Nullable (..), SecurityScheme (..))
import OpenAI.SecuritySchemes (bearerAuthenticationSecurityScheme)
import OpenAI.Configuration (defaultConfiguration)
import qualified OpenAI as OAI
-}

import Data.Text (Text, unpack)

import OpenAI (think)
import Context (Context(..), Result (..))
import Action (Action(..))
import Actions.Chat (CompleteParams(..), ChatVerb(..))

import qualified Options.Runtime as Rto

completeCmd :: Text -> Rto.RunOptions -> IO ()
completeCmd userPrompt rtOpts = do
  putStrLn "@[run] starting."
  case rtOpts.apiKey of
    Nothing -> error "No OPENAI api key"
    Just aKey ->
      let
        oaiContext = Simple aKey rtOpts.model
        params = SimplePrompt userPrompt
        action = Complete params
      in do
      rezA <- think $ Chat action oaiContext
      case rezA of
        Left errMsg ->
          putStrLn $ "@[completeCmd] err: " <> errMsg <> ".\n"
        Right (TextResult content) ->
          putStrLn $ "@[completeCmd] reply:\n'" <> unpack content <> "\n"
        _ -> putStrLn $ "@[completeCmd] got a response but it's not valid result.\n"

{-
      let
        defaultConf = defaultConfiguration { configSecurityScheme = bearerAuthenticationSecurityScheme aKey }
        modelID = OAI.CreateChatCompletionRequestModel'Text rtOpts.model
        textPrompt = ""
        -- prompt = NonNull $ OAI.CreateCompletionRequestPrompt'NonNullableText textPrompt
        prompt = OAI.ChatCompletionRequestMessageChatCompletionRequestUserMessage $ OAI.mkChatCompletionRequestUserMessage (OAI.ChatCompletionRequestUserMessageContent'Text userPrompt)
        -- request = OAI.mkCreateCompletionRequest modelID prompt
        request = OAI.mkCreateChatCompletionRequest [prompt] modelID
      in do
      rezA <- runWithConfiguration defaultConf $ OAI.createChatCompletion request
      let
        stCode = getResponseStatus rezA
      case stCode of
        ok200 -> case getResponseBody rezA of
          OAI.CreateChatCompletionResponseError errMsg ->
            putStrLn $ "@[completeCmd] err: CreateChatCompletionResponseError " <> errMsg
          OAI.CreateChatCompletionResponse200 completion ->
            case completion.createChatCompletionResponseChoices of
              [] -> putStrLn "@[completeCmd] no content in response."
              [ aChoice ] ->
                putStrLn $ "@[completeCmd] resp: " <> showNullable aChoice.createChatCompletionResponseChoices'Message.chatCompletionResponseMessageContent
              choices ->
                putStrLn $ "@[completeCmd] " <> show (length choices) <> " choices, first is " <> show (head choices) <> ".\n"
        _ -> putStrLn $ "@[completeCmd] err: " <> show rezA
      pure ()

showNullable :: Nullable Text -> String
showNullable aVal =
  case aVal of
    NonNull a -> unpack a
    Null -> "null"
-}

