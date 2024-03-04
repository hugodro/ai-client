{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Commands.Image (imageCmd) where


{-
import Data.Text (Text, unpack)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Text.IO as Tio (writeFile)
import Network.HTTP.Simple ( getResponseBody, getResponseStatus, httpLBS, parseRequest )

import OpenAI.Common (runWithConfiguration, Configuration(..), Nullable (..), SecurityScheme (..))
import OpenAI.SecuritySchemes (bearerAuthenticationSecurityScheme)
import OpenAI.Configuration (defaultConfiguration)
import qualified OpenAI as OAI
-}

import Data.Text (Text)

import OpenAI (think)
import Context (Context(..), Result (..))
import Action (Action(..))
import Actions.Image (CreateImageParams(..), ImageVerb(..))

import qualified Options.Runtime as Rto

imageCmd :: FilePath -> Text -> Rto.RunOptions -> IO ()
imageCmd outputPath userPrompt rtOpts = do
  putStrLn "@[imageCmd] starting."
  case rtOpts.apiKey of
    Nothing -> error "No OPENAI api key"
    Just aKey ->
      if rtOpts.model == "dall-e-2" || rtOpts.model == "dall-e-3" then
        doImage rtOpts aKey userPrompt outputPath
      else
        putStrLn $ "@[imageCmd] model " <> show rtOpts.model <> " is not supported for image generation."


doImage :: Rto.RunOptions -> Text -> Text -> FilePath -> IO ()
doImage rtOpts aKey userPrompt outputPath =
  let
    oaiContext = Simple aKey rtOpts.model
    action = CreateImage $ PromptPath userPrompt outputPath
  in do
  rezA <- think $ Image action oaiContext
  case rezA of
    Left errMsg ->
      putStrLn $ "@[doImage] err: " <> errMsg
    Right NilResult -> pure ()
    _ -> putStrLn $ "@[doImage] thinking terminted but unknown error happened."

{-
doImage :: Rto.RunOptions -> Text -> Text -> FilePath -> IO ()
doImage rtOpts aKey userPrompt outputPath =
  let
    defaultConf = defaultConfiguration { configSecurityScheme = bearerAuthenticationSecurityScheme aKey }
    modelID = OAI.CreateImageRequestModel'NonNullableText rtOpts.model
    request = OAI.mkCreateImageRequest userPrompt -- { OAI.createImageRequestModel = modelID }
  in do
  rezA <- runWithConfiguration defaultConf $ OAI.createImage request
  let
    stCode = getResponseStatus rezA
  case stCode of
    ok200 ->
      case getResponseBody rezA of
        OAI.CreateImageResponseError errMsg ->
          putStrLn $ "@[doImage] err: CreateSpeechResponseError " <> errMsg
        OAI.CreateImageResponse200 imageResponse -> do
          putStrLn $ "@[doImage] size: " <> show imageResponse.imagesResponseCreated <> ".\n"
          mapM_ (saveImage outputPath) (zip [1..] imageResponse.imagesResponseData)
    _ -> putStrLn $ "@[doImage] err: " <> show rezA
  pure ()

saveImage :: FilePath -> (Int, OAI.Image) -> IO ()
saveImage outputPath (idx, anImage) = do
  let
    imgPath = outputPath <> "/" <> "image-" <> show idx <> ".png"
  case anImage.imageB64Json of
    Just b64 -> do
      putStrLn $ "@[saveImage] saving image to " <> imgPath
      Tio.writeFile imgPath b64
    Nothing ->
      case anImage.imageUrl of
        Just url -> do
          request <- parseRequest $ unpack url
          httpLBS request >>= \response -> LBS.writeFile imgPath (getResponseBody response)
        Nothing ->
          putStrLn "@[saveImage] no image content in response."
-}
