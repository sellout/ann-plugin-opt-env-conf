{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Trustworthy #-}

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

-- Re-exported from OptEnvConf - this is a large list but necessary for explicit imports
import "opt-env-conf" OptEnvConf
  ( -- Types

    -- Builders

    -- Readers

    -- Combinators

    -- Config

    -- Checks

    -- Running

    -- Path settings

    -- String settings

    -- Switch helpers

    -- Casing

    -- Completers

    -- Docs

    -- Re-exports from other modules

    Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
    Builder (Builder, unBuilder),
    Functor (fmap, (<$)),
    HasParser (settingsParser),
    Help,
    Metavar,
    Parser,
    Reader (Reader, unReader),
    Selective (select),
    Setting (Setting),
    allOrNothing,
    argument,
    asum,
    auto,
    checkEither,
    checkMapEither,
    checkMapEitherForgivable,
    checkMapIO,
    checkMapIOForgivable,
    checkMapMaybe,
    checkMapMaybeForgivable,
    checkMaybe,
    choice,
    combineConfigObjects,
    commaSeparated,
    commaSeparatedList,
    commaSeparatedSet,
    command,
    commands,
    completer,
    conf,
    confWith,
    confWith',
    configuredConfigFile,
    defaultCommand,
    directoryPath,
    directoryPathSetting,
    eitherReader,
    enableDisableSwitch,
    env,
    example,
    exists,
    filePath,
    filePathSetting,
    help,
    hidden,
    liftA,
    liftA3,
    listCompleter,
    listIOCompleter,
    long,
    makeDoubleSwitch,
    many,
    mapIO,
    maybeReader,
    metavar,
    mkCompleter,
    name,
    option,
    optional,
    parserConfDocs,
    parserDocs,
    parserEnvDocs,
    parserOptDocs,
    readSecretTextFile,
    reader,
    runHelpParser,
    runIO,
    runParser,
    runParserOn,
    runSettingsParser,
    secretTextFileOrBareSetting,
    secretTextFileSetting,
    setting,
    settingCompleter,
    settingConfigVals,
    settingDasheds,
    settingDefaultValue,
    settingEnvVars,
    settingExamples,
    settingHelp,
    settingHidden,
    settingMetavar,
    settingReaders,
    settingSwitchValue,
    settingTryArgument,
    settingTryOption,
    short,
    shownExample,
    some,
    someNonEmpty,
    str,
    strArgument,
    strOption,
    subAll,
    subArgs,
    subArgs_,
    subConfig,
    subConfig_,
    subEnv,
    subEnv_,
    subSettings,
    switch,
    toArgCase,
    toConfigCase,
    toEnvCase,
    toShellFunctionCase,
    value,
    valueWithShown,
    viaStringCodec,
    withCombinedYamlConfigs,
    withCombinedYamlConfigs',
    withConfig,
    withConfigurableYamlConfig,
    withDefault,
    withFirstYamlConfig,
    withLocalYamlConfig,
    withShownDefault,
    withYamlConfig,
    withoutConfig,
    xdgYamlConfigFile,
    yesNoSwitch,
    (<$>),
    (<**>),
  )
import "this" AnnPluginOptEnvConf.Annotation
  ( AnnBuilder (AnnBuilder),
    AnnLookup,
    AnnResult (AnnError, AnnFound, AnnNotAttempted, AnnNotFound),
    AnnSetting (AnnSetting),
    AnnValSetting (AnnValSetting),
    ann,
    annResultToMaybe,
    annSettingKeys,
    annSettingReaders,
    annSettingVals,
    annValSettingCodec,
    annValSettingKey,
    annWith,
    unAnnBuilder,
  )
import "this" AnnPluginOptEnvConf.PluginOpts
  ( PluginOptsBackend (PluginOptsBackend),
    PluginOptsState,
    pluginOptsBackend,
  )
import "this" AnnPluginOptEnvConf.Run
  ( AnnPluginEnv (AnnPluginEnv),
    PluginParseError (PluginAnnotationError, PluginParseErrors),
    apeCapabilities,
    apeConfig,
    apeEnvVars,
    apePluginOpts,
    runAnnPluginParser,
    runAnnPluginParserOn,
    runPluginParser,
    runPluginParserOn,
  )
import "this" AnnPluginOptEnvConf.Setting
  ( AnnPluginBuilder (AnnPluginBuilder),
    AnnPluginSetting (AnnPluginSetting),
    annPluginSetting,
    annPluginSettingAnns,
    annPluginSettingBase,
    apbAnn,
    apbBase,
    fromAnn,
    fromBase,
  )
