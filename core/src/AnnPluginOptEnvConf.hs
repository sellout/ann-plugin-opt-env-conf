{-# LANGUAGE ApplicativeDo #-}

-- | OptEnvConf extended for GHC plugins with annotation support.
--
-- This library extends OptEnvConf for use by GHC plugins, providing:
--
--   * Plugin option parsing via @-fplugin-opt=Module.Name:option@
--   * Source annotation support via @{-# ANN expr value #-}@
--   * Environment variable support (unchanged from OptEnvConf)
--   * Configuration file support (unchanged from OptEnvConf)
--
-- = Priority Order
--
-- Configuration sources are checked in this order (highest priority first):
--
--   1. Source annotations (via 'ann' builder)
--   2. Plugin options (via 'long', 'short', 'option', 'switch', 'argument')
--   3. Environment variables (via 'env')
--   4. Configuration file values (via 'conf')
--   5. Default values (via 'value')
--
-- = Quick Start
--
-- Define your settings using the standard OptEnvConf combinators plus 'ann':
--
-- @
-- data MySettings = MySettings
--   { verbose :: Bool
--   , maxSteps :: Int
--   }
--
-- mySettingsParser :: Parser MySettings
-- mySettingsParser = MySettings
--   \<$\> setting
--         [ help "Enable verbose output"
--         , switch True
--         , long "verbose"
--         , short 'v'
--         , env "MY_PLUGIN_VERBOSE"
--         , value False
--         ]
--   \<*\> setting
--         [ help "Maximum optimization steps"
--         , reader auto
--         , option
--         , long "max-steps"
--         , env "MY_PLUGIN_MAX_STEPS"
--         , value 100
--         ]
-- @
--
-- Then use 'runPluginParser' in your plugin:
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
--
-- = Using Annotations
--
-- For annotation support, use 'runAnnPluginParser' with an annotation
-- lookup function:
--
-- @
-- data MyPluginAnn = MyPluginAnn
--   { verbose :: Maybe Bool
--   , maxSteps :: Maybe Int
--   } deriving (Data, Typeable)
--
-- -- In user's source file:
-- {-# ANN myFunction (MyPluginAnn (Just True) Nothing) #-}
-- @
module AnnPluginOptEnvConf
  ( -- * Running parsers
    -- ** Without annotations
    runPluginParser,
    runPluginParserOn,
    -- ** With annotations
    runAnnPluginParser,
    runAnnPluginParserOn,
    AnnPluginEnv (..),
    -- ** Errors
    PluginParseError (..),

    -- * Plugin options backend
    PluginOptsBackend (..),
    pluginOptsBackend,
    PluginOptsState,

    -- * Annotation support
    -- ** Lookup types
    AnnLookup,
    AnnResult (..),
    annResultToMaybe,
    -- ** Setting types
    AnnSetting (..),
    AnnValSetting (..),
    -- ** Builders
    ann,
    annWith,
    AnnBuilder (..),

    -- * Extended settings
    AnnPluginSetting (..),
    AnnPluginBuilder (..),
    annPluginSetting,
    fromBase,
    fromAnn,

    -- * Re-exports from OptEnvConf
    module OptEnvConf,
  )
where

import OptEnvConf
import AnnPluginOptEnvConf.Annotation
import AnnPluginOptEnvConf.PluginOpts
import AnnPluginOptEnvConf.Run
import AnnPluginOptEnvConf.Setting
