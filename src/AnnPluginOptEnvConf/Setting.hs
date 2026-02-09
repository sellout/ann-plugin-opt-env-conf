{-# LANGUAGE RankNTypes #-}

-- | Extended settings with annotation support.
--
-- This module provides an extended 'Setting' type that includes
-- annotation configuration alongside the standard OptEnvConf settings.
module AnnPluginOptEnvConf.Setting
  ( -- * Extended setting type
    AnnPluginSetting (..),

    -- * Builder
    AnnPluginBuilder (..),
    annPluginSetting,
    fromBase,
    fromAnn,

    -- * Re-exports for convenience
    module OptEnvConf.Setting,
    module AnnPluginOptEnvConf.Annotation,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import OptEnvConf.Setting
import AnnPluginOptEnvConf.Annotation

-- | A setting extended with annotation support.
--
-- This wraps an OptEnvConf 'Setting' and adds optional annotation
-- configuration that takes highest priority when parsing.
data AnnPluginSetting a = AnnPluginSetting
  { -- | The base OptEnvConf setting
    annPluginSettingBase :: !(Setting a),
    -- | Annotation settings (highest priority)
    annPluginSettingAnns :: !(Maybe (NonEmpty (AnnValSetting a)))
  }

-- | Builder for annotation-enabled plugin settings.
--
-- This wraps both a standard OptEnvConf 'Builder' and an 'AnnBuilder',
-- allowing both to be specified together.
data AnnPluginBuilder a = AnnPluginBuilder
  { -- | Standard OptEnvConf builder
    apbBase :: !(Builder a),
    -- | Annotation builder
    apbAnn :: !(AnnBuilder a)
  }

instance Semigroup (AnnPluginBuilder a) where
  (<>) b1 b2 =
    AnnPluginBuilder
      { apbBase = apbBase b1 <> apbBase b2,
        apbAnn = apbAnn b1 <> apbAnn b2
      }

instance Monoid (AnnPluginBuilder a) where
  mempty = AnnPluginBuilder mempty mempty
  mappend = (<>)

-- | Lift a standard OptEnvConf builder to an 'AnnPluginBuilder'.
fromBase :: Builder a -> AnnPluginBuilder a
fromBase b = AnnPluginBuilder b mempty

-- | Lift an annotation builder to an 'AnnPluginBuilder'.
fromAnn :: AnnBuilder a -> AnnPluginBuilder a
fromAnn a = AnnPluginBuilder mempty a

-- | Create an annotation-enabled plugin setting from builders.
--
-- This accepts a list of 'AnnPluginBuilder' values, combining the
-- standard OptEnvConf settings with annotation configuration.
--
-- @
-- myParser :: Parser MySettings
-- myParser = MySettings
--   \<$\> annPluginSetting
--         ( fromBase \<$\>
--           [ help "Enable verbose output"
--           , switch True
--           , long "verbose"
--           , value False
--           ]
--         ++ [fromAnn $ ann "verbose"]
--         )
-- @
--
-- For a cleaner API, see the combined builders in the main module.
annPluginSetting :: [AnnPluginBuilder a] -> AnnPluginSetting a
annPluginSetting builders =
  let combined = mconcat builders
      baseSetting = completeBuilder (apbBase combined)
      annSetting = completeAnnBuilder (apbAnn combined)
   in AnnPluginSetting
        { annPluginSettingBase = baseSetting,
          annPluginSettingAnns = annSettingVals annSetting
        }
