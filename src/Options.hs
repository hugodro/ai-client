module Options  (
  module Options.Cli
  , module Options.ConfFile
  , module Rt
  , mergeOptions
 )
where

import qualified Data.Text as DT

import Options.Cli
import Options.ConfFile
import qualified Options.Runtime as Rt


mergeOptions :: CliOptions -> FileOptions -> EnvOptions -> Rt.RunOptions
-- TODO: add the env param at the end and use env vars to bring in some values.
mergeOptions cli file _ =
  -- TODO: put proper priority filling of values for the Runtime Options.
  let
    defO = Rt.defaultRun
    -- Update from config file:
    fileO =
      let
        dbgO = case file.debug of
          Nothing -> defO
          Just aVal -> defO { Rt.debug = aVal }
        key0 = case file.apiKey of
          Nothing -> dbgO
          Just aVal -> dbgO { Rt.apiKey = Just $ DT.pack aVal }
      in
      key0
    -- TODO: update from CLI options
    cliO = case cli.debug of
      Nothing -> fileO
      Just aVal -> fileO { Rt.debug = aVal }
    model0 = case cli.model of
      Nothing -> cliO
      Just aVal -> cliO { Rt.model = DT.pack aVal }
    -- TODO: update from ENV options
    envO = model0
  in
  envO
