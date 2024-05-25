{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text)
import Options.Applicative


data EnvOptions = EnvOptions {
    home :: Maybe Text
    , apiKey :: Maybe Text
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
  , model :: Maybe String
 }
 deriving stock (Show)

data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  , modelGO :: String
  }

data Command =
  HelpCmd
  | VersionCmd
  | CompleteCmd Text
  | ImageCmd Text Text
  | SpeechCmd (Maybe Text) (Maybe Text) Text Text  -- prompt, inFile, narrator + outFile
  deriving stock (Show)

{- HERE: Additional structures for holding new command parameters:
Eg:
data ImportOpts = ImportOpts {
    taxonomy :: Text
    , path :: Text
  }
-}

parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "Invokes cloud-based AI services." <> header "aiclient - cloud AI."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
        mbModel = case globs.modelGO of
          "" -> Nothing
          aValue -> Just aValue
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
        , model = mbModel
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "gclientCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.config/aiclient.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )
    <*>
    strOption (
      long "model"
      <> short 'm'
      <> metavar "OPENAI_MODEL"
      <> value ""
      <> showDefault
      <> help "Model for processing request."
    )


commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("complete", completeOpts, "Chat completion.")
      , ("image", imageOpts, "Image processing.")
      , ("speech", speechOpts, "Speech processing.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> (cmdBuilder aCmd) <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))


completeOpts :: Parser Command
completeOpts =
  CompleteCmd <$> strArgument (metavar "PROMPT" <> help "Prompt for chat completion.")


imageOpts :: Parser Command
imageOpts =
  ImageCmd <$> strArgument (metavar "PROMPT" <> help "Description of the image content for generation.")
    <*> strArgument (metavar "OUTPUT" <> help "Filename for storing the resulting image.")

speechOpts :: Parser Command
speechOpts =
  mkSpeechCmd <$> strOption (
      long "prompt"
      <> short 'p'
      <> metavar "PROMPT"
      <> value ""
      <> help "Prompt for soeech generation."
    )
    <*> strOption (
      long "infile"
      <> short 'f'
      <> metavar "INPUT_FILE"
      <> value ""
      <> help "Prompt for soeech generation."
    )
    <*> strOption (
      long "narrator"
      <> short 'n'
      <> metavar "NARRATOR_NAME"
      <> value "nova"
      <> showDefault
      <> help "Narrator (alloy, echo, fable, onyx, nova, shimmer)."
    )
    <*> strArgument (metavar "OUTPUT" <> help "Filename for storing the resulting audio.")
  where
  mkSpeechCmd prompt infile narrator output =
    SpeechCmd (if prompt == "" then Nothing else Just prompt)
              (if infile == "" then Nothing else Just infile)
              narrator output
