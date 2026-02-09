{-# LANGUAGE RankNTypes #-}

-- | Entry points for running parsers in a GHC plugin context.
--
-- This module provides functions to run OptEnvConf parsers using
-- plugin options instead of command-line arguments.
module AnnPluginOptEnvConf.Run
  ( -- * Running parsers
    runPluginParser,
    runPluginParserOn,

    -- * Errors
    PluginParseError (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Aeson as JSON
import OptEnvConf (Parser)
import OptEnvConf.ArgsBackend (initArgsState)
import OptEnvConf.Capability (Capabilities, allCapabilities)
import OptEnvConf.EnvMap (EnvMap)
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error (ParseError)
import OptEnvConf.Run (runParserOnWith)
import AnnPluginOptEnvConf.PluginOpts
import System.Environment (getEnvironment)

-- | Errors that can occur during plugin option parsing.
data PluginParseError
  = PluginParseErrors (NonEmpty ParseError)
  deriving (Show)

-- | Run a parser with plugin options, using the current environment.
--
-- This is the simplest entry point for parsing plugin configuration.
-- It reads environment variables from the current process environment
-- and does not use any configuration file.
--
-- @
-- plugin :: Plugin
-- plugin = defaultPlugin
--   { driverPlugin = \\opts env -> do
--       result <- runPluginParser opts mySettingsParser
--       case result of
--         Left err -> error $ show err
--         Right settings -> ...
--   }
-- @
runPluginParser ::
  -- | Plugin options (from @[CommandLineOption]@)
  [String] ->
  -- | Parser for the settings
  Parser a ->
  IO (Either PluginParseError a)
runPluginParser opts parser = do
  env <- EnvMap.parse <$> getEnvironment
  runPluginParserOn opts env Nothing allCapabilities parser

-- | Run a parser with full control over all inputs.
--
-- This allows specifying:
--
--   * Plugin options
--   * Environment variables (instead of reading from the process)
--   * Configuration object (e.g., from a YAML file)
--   * Capabilities for restricted parsing
runPluginParserOn ::
  -- | Plugin options (from @[CommandLineOption]@)
  [String] ->
  -- | Environment variables
  EnvMap ->
  -- | Configuration object (optional)
  Maybe JSON.Object ->
  -- | Parsing capabilities
  Capabilities ->
  -- | Parser for the settings
  Parser a ->
  IO (Either PluginParseError a)
runPluginParserOn opts envVars mConfig capabilities parser = do
  let backend = pluginOptsBackend
  let argsState = initArgsState backend opts
  result <-
    runParserOnWith
      backend
      capabilities
      Nothing -- No debug mode
      parser
      argsState
      envVars
      mConfig
  pure $ case result of
    Left errs -> Left (PluginParseErrors errs)
    Right a -> Right a
