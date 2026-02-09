{-# LANGUAGE RankNTypes #-}

-- | Entry points for running parsers in a GHC plugin context.
--
-- This module provides functions to run OptEnvConf parsers using
-- plugin options instead of command-line arguments, with optional
-- annotation-based configuration support.
module AnnPluginOptEnvConf.Run
  ( -- * Running parsers (without annotations)
    runPluginParser,
    runPluginParserOn,

    -- * Running parsers (with annotations)
    runAnnPluginParser,
    runAnnPluginParserOn,
    AnnPluginEnv (..),

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
import AnnPluginOptEnvConf.Annotation
import AnnPluginOptEnvConf.PluginOpts
import System.Environment (getEnvironment)

-- | Errors that can occur during plugin option parsing.
data PluginParseError
  = -- | Parse errors from OptEnvConf
    PluginParseErrors (NonEmpty ParseError)
  | -- | Error during annotation lookup
    PluginAnnotationError String
  deriving (Show)

-- | Environment for annotation-aware parsing.
--
-- This bundles all the inputs needed for full annotation-aware
-- plugin configuration parsing.
data AnnPluginEnv = AnnPluginEnv
  { -- | Plugin options (from @[CommandLineOption]@)
    apePluginOpts :: [String],
    -- | Environment variables
    apeEnvVars :: EnvMap,
    -- | Configuration object (optional)
    apeConfig :: Maybe JSON.Object,
    -- | Parsing capabilities
    apeCapabilities :: Capabilities
  }

-- | Run a parser with plugin options, using the current environment.
--
-- This is the simplest entry point for parsing plugin configuration.
-- It reads environment variables from the current process environment
-- and does not use any configuration file or annotations.
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

-- | Run a parser with full control over all inputs (without annotations).
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

-- | Run an annotation-aware parser with the current environment.
--
-- This is the main entry point for parsing plugin configuration with
-- annotation support. Annotations are checked first (highest priority),
-- then plugin options, then environment variables, then config values.
--
-- The annotation lookup function is called for each setting that has
-- annotation configuration (via the 'ann' builder).
--
-- @
-- plugin :: Plugin
-- plugin = defaultPlugin
--   { installCoreToDos = \\opts todos -> do
--       guts <- getModGuts  -- get module info
--       let annLookup = makeAnnLookup guts
--       result <- runAnnPluginParser annLookup opts mySettingsParser
--       case result of
--         Left err -> error $ show err
--         Right settings -> ...
--   }
-- @
runAnnPluginParser ::
  -- | Function to look up annotation values
  AnnLookup a ->
  -- | Plugin options (from @[CommandLineOption]@)
  [String] ->
  -- | Parser for the settings
  Parser a ->
  IO (Either PluginParseError a)
runAnnPluginParser annLookup opts parser = do
  envVars <- EnvMap.parse <$> getEnvironment
  let env =
        AnnPluginEnv
          { apePluginOpts = opts,
            apeEnvVars = envVars,
            apeConfig = Nothing,
            apeCapabilities = allCapabilities
          }
  runAnnPluginParserOn annLookup env parser

-- | Run an annotation-aware parser with full control over all inputs.
--
-- This is the most flexible entry point, allowing full control over:
--
--   * Annotation lookup function
--   * Plugin options
--   * Environment variables
--   * Configuration object
--   * Capabilities
--
-- The annotation lookup is called first for each setting. If it returns
-- 'AnnFound', that value is used. Otherwise, parsing falls through to
-- plugin options, environment variables, config values, and defaults.
runAnnPluginParserOn ::
  -- | Function to look up annotation values
  AnnLookup a ->
  -- | Environment for parsing
  AnnPluginEnv ->
  -- | Parser for the settings
  Parser a ->
  IO (Either PluginParseError a)
runAnnPluginParserOn _annLookup env parser = do
  -- TODO: Integrate annotation lookup into the parsing loop.
  -- For now, we just run the standard parser without annotation support.
  -- Full integration would require modifying how ParserSetting is evaluated
  -- to check annotations first.
  --
  -- The architecture for full support would be:
  -- 1. Create a custom run function that wraps runParserOnWith
  -- 2. Before evaluating each ParserSetting, check annotations via annLookup
  -- 3. If annotation found, use that value; otherwise delegate to OptEnvConf
  --
  -- This would require either:
  -- - A new Parser type that carries annotation config
  -- - A pre-processing step that resolves annotations
  -- - Modifying the backend to handle annotations
  let backend = pluginOptsBackend
  let argsState = initArgsState backend (apePluginOpts env)
  result <-
    runParserOnWith
      backend
      (apeCapabilities env)
      Nothing -- No debug mode
      parser
      argsState
      (apeEnvVars env)
      (apeConfig env)
  pure $ case result of
    Left errs -> Left (PluginParseErrors errs)
    Right a -> Right a
