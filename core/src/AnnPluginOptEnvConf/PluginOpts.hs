{-# LANGUAGE TypeFamilies #-}

-- | Backend for parsing GHC plugin command-line options.
--
-- This module provides an 'ArgsBackend' instance that parses options
-- passed via @-fplugin-opt=Module.Name:option@.
--
-- Plugin options come as a list of strings, typically in the form:
--
--   * @key=value@ for options
--   * @flag@ for switches (presence indicates the value)
--   * bare strings for positional arguments
module AnnPluginOptEnvConf.PluginOpts
  ( PluginOptsBackend (..),
    pluginOptsBackend,
    PluginOptsState,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import OptEnvConf.Args (Dashed (..))
import OptEnvConf.ArgsBackend
import OptEnvConf.Reader (Reader, runReader)

-- | The plugin options argument parsing backend.
--
-- This parses GHC plugin options which are passed as simple strings
-- without the @--@ prefix used in normal command-line parsing.
data PluginOptsBackend = PluginOptsBackend
  deriving (Show, Eq)

-- | The default plugin options backend instance.
pluginOptsBackend :: PluginOptsBackend
pluginOptsBackend = PluginOptsBackend

-- | State for tracking plugin options during parsing.
--
-- Each option is paired with a boolean indicating whether it has been consumed.
data PluginOptsState = PluginOptsState
  { posOptions :: [(String, Bool)]
  }
  deriving (Show, Eq)

instance ArgsBackend PluginOptsBackend where
  type ArgsState PluginOptsBackend = PluginOptsState

  initArgsState _ opts =
    PluginOptsState
      { posOptions = map (,False) opts
      }

  parseArg _ readers state = pure $ case NE.nonEmpty readers of
    Nothing -> [(ArgsNotAttempted, state)]
    Just rs ->
      -- Find unconsumed options that are NOT key=value pairs
      let unconsumed =
            [ (s, i)
              | ((s, False), i) <- zip (posOptions state) [0 ..],
                not (isKeyValue s)
            ]
       in if null unconsumed
            then [(ArgsNotFound, state)]
            else
              -- Return all possibilities plus a "not found" option for backtracking
              map (tryConsumeArgAt rs state) unconsumed ++ [(ArgsNotFound, state)]

  parseOpt _ dasheds readers state = pure $ case (dasheds, NE.nonEmpty readers) of
    ([], _) -> (ArgsNotAttempted, state)
    (_, Nothing) -> (ArgsNotAttempted, state)
    (ds, Just rs) ->
      let keys = dashedsToKeys ds
          result = findAndParseOption keys (posOptions state)
       in case result of
            Nothing -> (ArgsNotFound, state)
            Just (val, newOpts) ->
              case tryReaders rs val of
                Left errs -> (ArgsError (unlines $ NE.toList errs), state {posOptions = newOpts})
                Right a -> (ArgsFound a, state {posOptions = newOpts})

  parseSwitch _ dasheds val state = pure $ case dasheds of
    [] -> (ArgsNotAttempted, state)
    ds ->
      let keys = dashedsToKeys ds
          result = findSwitch keys (posOptions state)
       in case result of
            Nothing -> (ArgsNotFound, state)
            Just newOpts -> (ArgsFound val, state {posOptions = newOpts})

  recogniseLeftovers _ state =
    NE.nonEmpty [s | (s, False) <- posOptions state]

-- | Convert Dashed values to the keys we look for in plugin options.
--
-- For plugin options, we strip the dashes since options are passed
-- as @key=value@ rather than @--key=value@.
dashedsToKeys :: [Dashed] -> [String]
dashedsToKeys = mapMaybe $ \case
  DashedLong cs -> Just (NE.toList cs)
  DashedShort c -> Just [c]

-- | Check if a string is a key=value pair.
isKeyValue :: String -> Bool
isKeyValue = elem '='

-- | Parse a key=value string into its components.
parseKeyValue :: String -> Maybe (String, String)
parseKeyValue s = case break (== '=') s of
  (key, '=' : val) | not (null key) -> Just (key, val)
  _ -> Nothing

-- | Find an option matching one of the keys and return its value.
--
-- Marks the option as consumed in the returned state.
findAndParseOption :: [String] -> [(String, Bool)] -> Maybe (String, [(String, Bool)])
findAndParseOption keys opts = go opts []
  where
    go [] _ = Nothing
    go ((s, consumed) : rest) acc
      | consumed = go rest ((s, consumed) : acc)
      | otherwise = case parseKeyValue s of
          Just (key, val)
            | key `elem` keys ->
                Just (val, reverse acc ++ [(s, True)] ++ rest)
          _ -> go rest ((s, consumed) : acc)

-- | Find a switch matching one of the keys.
--
-- A switch is a bare string (not a key=value pair) that matches one of the keys.
-- Marks the option as consumed in the returned state.
findSwitch :: [String] -> [(String, Bool)] -> Maybe [(String, Bool)]
findSwitch keys opts = go opts []
  where
    go [] _ = Nothing
    go ((s, consumed) : rest) acc
      | consumed = go rest ((s, consumed) : acc)
      | s `elem` keys && not (isKeyValue s) =
          Just $ reverse acc ++ [(s, True)] ++ rest
      | otherwise = go rest ((s, consumed) : acc)

-- | Try to consume a positional argument at a specific index.
tryConsumeArgAt ::
  NonEmpty (Reader a) ->
  PluginOptsState ->
  (String, Int) ->
  (ArgsResult a, PluginOptsState)
tryConsumeArgAt rs state (s, idx) =
  case tryReaders rs s of
    Left errs -> (ArgsError (unlines $ NE.toList errs), state)
    Right a ->
      let newOpts = markConsumed idx (posOptions state)
       in (ArgsFound a, state {posOptions = newOpts})
  where
    markConsumed i xs = zipWith (\(x, c) j -> (x, c || i == j)) xs [0 ..]

-- | Try the readers in order, returning the first success or all errors.
tryReaders :: NonEmpty (Reader a) -> String -> Either (NonEmpty String) a
tryReaders rs s = go rs
  where
    go (r :| rest) = case runReader r s of
      Right a -> Right a
      Left err -> case NE.nonEmpty rest of
        Nothing -> Left (err :| [])
        Just ne -> case go ne of
          Right a -> Right a
          Left errs -> Left (err NE.<| errs)
