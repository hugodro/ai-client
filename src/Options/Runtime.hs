module Options.Runtime (defaultRun, RunOptions (..)) where

import Data.Text (Text)



data RunOptions = RunOptions {
    debug :: Int
    , apiKey :: Maybe Text
    , model :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    -- HERE: Use if accessing the DB: , db = defaultDbConf
   , apiKey = Nothing
   , model = "gpt-3.5-turbo"
  }
