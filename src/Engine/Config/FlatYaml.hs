{-# LANGUAGE Strict, UnicodeSyntax #-}

module Engine.Config.FlatYaml
  ( FlatYaml
  , parseFlatYaml
  , childKeys
  , lookupKey
  , lookupMaybeKey
  , lookupTextKeyDefault
  , lookupIntKey
  , lookupWord32Key
  , lookupFloatKey
  , lookupBoolKey
  , lookupIntKeyDefault
  , lookupFloatKeyDefault
  , lookupBoolKeyDefault
  , ensure
  , renderPath
  , trim
  ) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Word (Word32)

import qualified Data.Map.Strict as M

type FlatYaml = Map [String] String

parseFlatYaml ∷ String → Either String FlatYaml
parseFlatYaml contents = snd <$> foldl step (Right ([], M.empty)) (zip [1 ∷ Int ..] (lines contents))
  where
    step
      ∷ Either String ([String], FlatYaml)
      → (Int, String)
      → Either String ([String], FlatYaml)
    step acc (lineNo, rawLine) = do
      (contexts, entries) <- acc
      let line = stripComment rawLine
          trimmed = dropWhile isSpace line
      if null trimmed
        then pure (contexts, entries)
        else do
          let indent = length line - length trimmed
          ensure (indent `mod` 2 == 0) ("line " <> show lineNo <> ": indentation must use multiples of two spaces")
          let level = indent `div` 2
          ensure (level <= length contexts) ("line " <> show lineNo <> ": invalid nesting level")
          (key, mValue) <- parseKeyValue lineNo trimmed
          let parent = take level contexts
              path = parent <> [key]
          case mValue of
            Nothing ->
              pure (path, entries)
            Just value -> do
              ensure (not (M.member path entries)) ("line " <> show lineNo <> ": duplicate key " <> renderPath path)
              pure (parent, M.insert path value entries)

childKeys ∷ [String] → FlatYaml → [String]
childKeys prefix entries =
  let prefixLen = length prefix
      keys =
        [ path !! prefixLen
        | path <- M.keys entries
        , take prefixLen path == prefix
        , length path > prefixLen
        ]
  in nub keys

lookupKey ∷ [String] → FlatYaml → Either String String
lookupKey key entries =
  case M.lookup key entries of
    Just value -> pure value
    Nothing -> Left ("missing key " <> renderPath key)

lookupMaybeKey ∷ [String] → FlatYaml → Maybe String
lookupMaybeKey key entries = M.lookup key entries

lookupTextKeyDefault ∷ [String] → String → FlatYaml → String
lookupTextKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> fallback
    Just value -> value

lookupIntKey ∷ [String] → FlatYaml → Either String Int
lookupIntKey key entries = do
  raw <- lookupKey key entries
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid integer for " <> renderPath key <> ": " <> raw)

lookupWord32Key ∷ [String] → FlatYaml → Either String Word32
lookupWord32Key key entries = do
  n <- lookupIntKey key entries
  ensure (n >= 0) ("value for " <> renderPath key <> " must be non-negative")
  ensure (n <= fromIntegral (maxBound ∷ Word32))
    ("value for " <> renderPath key <> " exceeds maximum (" <> show (maxBound ∷ Word32) <> ")")
  pure (fromIntegral n)

lookupFloatKey ∷ [String] → FlatYaml → Either String Float
lookupFloatKey key entries = do
  raw <- lookupKey key entries
  case reads raw of
    [(n, "")] -> pure n
    _ -> Left ("invalid float for " <> renderPath key <> ": " <> raw)

lookupBoolKey ∷ [String] → FlatYaml → Either String Bool
lookupBoolKey key entries = do
  raw <- lookupKey key entries
  case map toLower raw of
    "true" -> pure True
    "false" -> pure False
    "yes" -> pure True
    "no" -> pure False
    "on" -> pure True
    "off" -> pure False
    _ -> Left ("invalid bool for " <> renderPath key <> ": " <> raw)

lookupIntKeyDefault ∷ [String] → Int → FlatYaml → Either String Int
lookupIntKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupIntKey key entries

lookupFloatKeyDefault ∷ [String] → Float → FlatYaml → Either String Float
lookupFloatKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupFloatKey key entries

lookupBoolKeyDefault ∷ [String] → Bool → FlatYaml → Either String Bool
lookupBoolKeyDefault key fallback entries =
  case M.lookup key entries of
    Nothing -> pure fallback
    Just _ -> lookupBoolKey key entries

stripComment ∷ String → String
stripComment = reverse . dropWhile isSpace . reverse . takeWhile (/= '#')

parseKeyValue ∷ Int → String → Either String (String, Maybe String)
parseKeyValue lineNo content =
  case break (== ':') content of
    (_, []) ->
      Left ("line " <> show lineNo <> ": expected key: value")
    (rawKey, ':' : rawValue) -> do
      let key = trim rawKey
          value = trim rawValue
      ensure (not (null key)) ("line " <> show lineNo <> ": empty key")
      pure (key, if null value then Nothing else Just value)
    _ ->
      Left ("line " <> show lineNo <> ": expected key: value")

trim ∷ String → String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd ∷ (Char → Bool) → String → String
dropWhileEnd p = reverse . dropWhile p . reverse

ensure ∷ Bool → String → Either String ()
ensure cond err =
  if cond
    then pure ()
    else Left err

renderPath ∷ [String] → String
renderPath = intercalate "."
