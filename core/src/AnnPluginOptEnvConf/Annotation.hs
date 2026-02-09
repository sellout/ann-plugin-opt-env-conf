{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fplugin-opt NoRecursion:ignore-methods:sconcat #-}

-- | Annotation-based configuration for GHC plugins.
--
-- This module provides types and builders for looking up configuration
-- values from GHC source annotations (@{-# ANN ... #-}@).
--
-- Annotations have the highest priority in the configuration hierarchy,
-- allowing per-function or per-module overrides of global settings.
module AnnPluginOptEnvConf.Annotation
  ( -- * Annotation settings
    AnnSetting (..),
    AnnValSetting (..),

    -- * Builder
    ann,
    annWith,
    AnnBuilder (..),
    AnnBuildInstruction (..),

    -- * Lookup types
    AnnLookup,
    AnnResult (..),
    annResultToMaybe,

    -- * Internal
    emptyAnnSetting,
    completeAnnBuilder,
  )
where

import "autodocodec" Autodocodec (HasCodec, ValueCodec, codec, maybeCodec)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (foldr)
import "base" Data.Function (($))
import "base" Data.Functor (Functor)
import "base" Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Semigroup (Semigroup, stimes, stimesMonoid, (<>))
import "base" Data.String (String)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "opt-env-conf" OptEnvConf.Reader (Reader)

-- | Configuration for looking up annotation values.
--
-- Similar to 'OptEnvConf.Setting.Setting', but specifically for
-- annotation-based configuration.
data AnnSetting a = AnnSetting
  { -- | Keys to look for in annotations
    annSettingKeys :: !(Maybe (NonEmpty String)),
    -- | Readers for parsing string annotation values
    annSettingReaders :: ![Reader a],
    -- | Config-style annotation values with codecs
    annSettingVals :: !(Maybe (NonEmpty (AnnValSetting a)))
  }

-- | A single annotation value configuration with its codec.
--
-- This is analogous to 'OptEnvConf.Setting.ConfigValSetting'.
data AnnValSetting a
  = forall void.
  AnnValSetting
  { annValSettingKey :: !String,
    annValSettingCodec :: !(ValueCodec void (Maybe a))
  }

-- | An empty annotation setting to build upon.
emptyAnnSetting :: AnnSetting a
emptyAnnSetting =
  AnnSetting
    { annSettingKeys = Nothing,
      annSettingReaders = [],
      annSettingVals = Nothing
    }

-- | Result of looking up an annotation.
data AnnResult a
  = -- | No annotation lookup was configured for this setting
    AnnNotAttempted
  | -- | Looked for annotation but didn't find one
    AnnNotFound
  | -- | Found an annotation with this value
    AnnFound a
  | -- | Error while looking up or parsing annotation
    AnnError String
  deriving (Show, Eq, Functor)

-- | Extract the value from an 'AnnResult' if found.
annResultToMaybe :: AnnResult a -> Maybe a
annResultToMaybe (AnnFound a) = Just a
annResultToMaybe _ = Nothing

-- | A function that looks up annotation values.
--
-- The function receives a list of keys to look for and returns
-- the result of the lookup. The keys correspond to field names
-- in the user's annotation type.
--
-- Example implementation:
--
-- @
-- data MyPluginAnn = MyPluginAnn { verbose :: Maybe Bool }
--   deriving (Data, Typeable)
--
-- myAnnLookup :: ModGuts -> AnnLookup
-- myAnnLookup guts keys = do
--   anns <- getAnnotations guts
--   case findAnn anns of
--     Nothing -> pure AnnNotFound
--     Just ann -> lookupField ann keys
-- @
type AnnLookup a = [String] -> IO (AnnResult a)

-- | Builder for annotation settings.
--
-- Use 'ann' and 'annWith' to create builders, then combine them
-- with other OptEnvConf builders.
newtype AnnBuilder a = AnnBuilder {unAnnBuilder :: [AnnBuildInstruction a]}

instance Semigroup (AnnBuilder a) where
  (<>) (AnnBuilder b1) (AnnBuilder b2) = AnnBuilder (b1 <> b2)
  stimes = stimesMonoid

instance Monoid (AnnBuilder a) where
  mempty = AnnBuilder []

-- | Instructions for building annotation settings.
data AnnBuildInstruction a
  = -- | Add a key to look for
    AnnBuildAddKey !String
  | -- | Add a reader for parsing string values
    AnnBuildAddReader !(Reader a)
  | -- | Add a config-style value lookup with codec
    AnnBuildAddVal !(AnnValSetting a)

-- | Look up an annotation value by key.
--
-- This is analogous to 'OptEnvConf.Setting.conf' for config values.
--
-- @
-- setting
--   [ help "Enable verbose output"
--   , switch True
--   , long "verbose"
--   , ann "verbose"  -- Look for annotation field "verbose"
--   , value False
--   ]
-- @
ann :: (HasCodec a) => String -> AnnBuilder a
ann key = annWith key codec

-- | Like 'ann' but with a custom codec for parsing the annotation value.
annWith :: String -> ValueCodec void a -> AnnBuilder a
annWith key c =
  let val =
        AnnValSetting
          { annValSettingKey = key,
            annValSettingCodec = maybeCodec c
          }
   in AnnBuilder [AnnBuildAddKey key, AnnBuildAddVal val]

-- | Complete an 'AnnBuilder' into an 'AnnSetting'.
completeAnnBuilder :: AnnBuilder a -> AnnSetting a
completeAnnBuilder b = applyAnnBuildInstructions (unAnnBuilder b) emptyAnnSetting

applyAnnBuildInstructions :: [AnnBuildInstruction a] -> AnnSetting a -> AnnSetting a
applyAnnBuildInstructions is s = foldr applyAnnBuildInstruction s is

applyAnnBuildInstruction :: AnnBuildInstruction a -> AnnSetting a -> AnnSetting a
applyAnnBuildInstruction bi s = case bi of
  AnnBuildAddKey k ->
    s {annSettingKeys = Just $ maybe (k :| []) (k <|) $ annSettingKeys s}
  AnnBuildAddReader r ->
    s {annSettingReaders = r : annSettingReaders s}
  AnnBuildAddVal v ->
    s {annSettingVals = Just $ maybe (v :| []) (v <|) $ annSettingVals s}
