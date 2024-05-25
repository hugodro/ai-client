module MainLogic
where

import Data.Text (pack, unpack)
import qualified System.Environment as Env

import qualified Options as Opt
import Commands as Cmd


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbHome <- Env.lookupEnv "gclientHOME"
      apiKey <- Env.lookupEnv "OPENAI_API_KEY"
      let
        envOptions = Opt.EnvOptions {
            home = pack <$> mbHome
            , apiKey = pack <$> apiKey
            -- TODO: put additional env vars.
          }
        rtOptions = Opt.mergeOptions cliOptions fileOptions envOptions
        -- switchboard to command executors:
        cmdExecutor =
          case aJob of
            Opt.HelpCmd -> Cmd.helpCmd
            Opt.VersionCmd -> Cmd.versionCmd
            Opt.CompleteCmd prompt -> Cmd.completeCmd prompt
            Opt.ImageCmd aText filePath -> Cmd.imageCmd (unpack filePath) aText
            Opt.SpeechCmd mbText mbFile narrator filePath -> Cmd.speechCmd (unpack filePath) narrator mbText (unpack <$> mbFile)
      cmdExecutor rtOptions
      -- TODO: return a properly kind of conclusion.
      pure ()
