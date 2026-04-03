{-# LANGUAGE Strict, UnicodeSyntax #-}

module Audio.Patch.Loader
  ( PatchGraph(..)
  , PatchNode(..)
  , OscillatorNode(..)
  , NoiseNode(..)
  , EnvelopeNode(..)
  , FilterNode(..)
  , OutputNode(..)
  , PatchConnection(..)
  , ConnectionKind(..)
  , PatchModulation(..)
  , PatchModulationDestination(..)
  , parsePatchGraphText
  , compilePatchGraph
  , compilePatchText
  , loadInstrumentPatchEither
  , loadInstrumentPatch
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)

import Data.Char (isAlphaNum, toLower)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)

import qualified Data.Map.Strict as M

import Audio.Envelope (ADSR(..))
import Audio.Filter.Biquad (FilterType(..))
import Audio.Filter.Types (FilterSlope(..), FilterSpec(..), FilterTarget(..), KeyTrack(..))
import Audio.Thread.Types (maxLayers, maxModRoutes)
import Audio.Types
import Engine.Config.FlatYaml
  ( FlatYaml
  , childKeys
  , ensure
  , lookupFloatKey
  , lookupFloatKeyDefault
  , lookupIntKeyDefault
  , lookupKey
  , lookupMaybeKey
  , lookupTextKeyDefault
  , parseFlatYaml
  , renderPath
  , trim
  )

data PatchGraph = PatchGraph
  { pgName        ∷ !(Maybe String)
  , pgNodes       ∷ !(Map String PatchNode)
  , pgConnections ∷ !(Map String PatchConnection)
  , pgModulations ∷ !(Map String PatchModulation)
  } deriving (Eq, Show)

data PatchNode
  = PatchNodeOscillator !OscillatorNode
  | PatchNodeNoise !NoiseNode
  | PatchNodeEnvelope !EnvelopeNode
  | PatchNodeFilter !FilterNode
  | PatchNodeOutput !OutputNode
  deriving (Eq, Show)

data OscillatorNode = OscillatorNode
  { onWaveform ∷ !Waveform
  , onPitch    ∷ !PitchSpec
  , onLevel    ∷ !Float
  , onAmpEnv   ∷ !(Maybe ADSR)
  } deriving (Eq, Show)

data NoiseNode = NoiseNode
  { nnWaveform ∷ !Waveform
  , nnLevel    ∷ !Float
  , nnAmpEnv   ∷ !(Maybe ADSR)
  } deriving (Eq, Show)

data EnvelopeNode = EnvelopeNode
  { enAdsr ∷ !ADSR
  } deriving (Eq, Show)

data FilterNode = FilterNode
  { fnType         ∷ !FilterType
  , fnCutoffHz     ∷ !Float
  , fnQ            ∷ !Float
  , fnSlope        ∷ !FilterSlope
  , fnKeyTrack     ∷ !Float
  , fnLayerTarget  ∷ !(Maybe String)
  , fnEnvAmountOct ∷ !Float
  , fnQEnvAmount   ∷ !Float
  } deriving (Eq, Show)

data OutputNode = OutputNode
  { outGain        ∷ !Float
  , outLayerSpread ∷ !Float
  , outLfo1RateHz  ∷ !Float
  , outPlayMode    ∷ !PlayMode
  , outPolyMax     ∷ !Int
  , outVoiceSteal  ∷ !VoiceSteal
  } deriving (Eq, Show)

data ConnectionKind
  = ConnectionSignal
  | ConnectionAmpEnvelope
  | ConnectionFilterEnvelope
  | ConnectionHardSync
  deriving (Eq, Show)

data PatchConnection = PatchConnection
  { pcFrom ∷ !String
  , pcTo   ∷ !String
  , pcKind ∷ !ConnectionKind
  } deriving (Eq, Show)

data PatchModulationDestination
  = PatchModDstPitchCents
  | PatchModDstFilterCutoffOct
  | PatchModDstAmpGain
  deriving (Eq, Show)

data PatchModulation = PatchModulation
  { pmSource      ∷ !ModSrc
  , pmTarget      ∷ !String
  , pmDestination ∷ !PatchModulationDestination
  , pmAmount      ∷ !Float
  } deriving (Eq, Show)

parsePatchGraphText ∷ String → Either String PatchGraph
parsePatchGraphText contents = do
  entries <- parseFlatYaml contents
  let nodeIds = childKeys ["patch", "nodes"] entries
      connectionIds = childKeys ["patch", "connections"] entries
      modulationIds = childKeys ["patch", "modulations"] entries
      patchName =
        case lookupMaybeKey ["patch", "name"] entries of
          Nothing -> Nothing
          Just raw ->
            let txt = trim raw
            in if null txt then Nothing else Just txt
  ensure (not (null nodeIds)) "patch.nodes must define at least one node"
  nodes <- mapM (parsePatchNode entries) nodeIds
  connections <- mapM (parsePatchConnection entries) connectionIds
  modulations <- mapM (parsePatchModulation entries) modulationIds
  pure
    PatchGraph
      { pgName = patchName
      , pgNodes = M.fromList nodes
      , pgConnections = M.fromList connections
      , pgModulations = M.fromList modulations
      }

compilePatchText ∷ String → Either String Instrument
compilePatchText contents = parsePatchGraphText contents >>= compilePatchGraph

loadInstrumentPatchEither ∷ FilePath → IO (Either String Instrument)
loadInstrumentPatchEither path = do
  contentsResult <- try (readFile path >>= \contents -> evaluate (length contents) >> pure contents) ∷ IO (Either SomeException String)
  pure $
    case contentsResult of
      Left ex ->
        Left ("Failed to read patch " <> path <> ": " <> displayException ex)
      Right contents ->
        case compilePatchText contents of
          Left err ->
            Left ("Invalid patch in " <> path <> ": " <> err)
          Right instrument ->
            Right instrument

loadInstrumentPatch ∷ FilePath → IO Instrument
loadInstrumentPatch path = do
  result <- loadInstrumentPatchEither path
  case result of
    Left err ->
      ioError (userError err)
    Right instrument ->
      pure instrument

compilePatchGraph ∷ PatchGraph → Either String Instrument
compilePatchGraph graph = do
  outputId <- requireSingleOutputNode graph
  outputNode <- requireOutputNode graph outputId
  validateConnectionRefs graph
  validateModulationRefs graph
  let allConnections = M.toList (pgConnections graph)
      signalConnections = filter ((== ConnectionSignal) . pcKind . snd) allConnections
      ampEnvelopeConnections = filter ((== ConnectionAmpEnvelope) . pcKind . snd) allConnections
      filterEnvelopeConnections = filter ((== ConnectionFilterEnvelope) . pcKind . snd) allConnections
      hardSyncConnections = filter ((== ConnectionHardSync) . pcKind . snd) allConnections

  outputEnvelopeConnection <- requireSingleNamedConnection "output amp envelope" outputId ampEnvelopeConnections
  outputEnvelope <- requireEnvelopeNode graph (pcFrom (snd outputEnvelopeConnection))

  let filterOutputConnections =
        filter (\(_, conn) -> pcTo conn == outputId && isFilterNodeId graph (pcFrom conn)) signalConnections
      dryOutputConnections =
        filter (\(_, conn) -> pcTo conn == outputId && isSignalSourceNodeId graph (pcFrom conn)) signalConnections

  (signalLayerIds, maybeFilterId, allowedSignalConnectionIds) <-
    case filterOutputConnections of
      [] -> do
        ensure (not (null dryOutputConnections)) "patch must connect at least one oscillator or noise node to the output"
        pure (map (pcFrom . snd) dryOutputConnections, Nothing, map fst dryOutputConnections)
      [(filterConnId, filterConn)] -> do
        ensure (null dryOutputConnections) "patch cannot mix direct output and filtered output in the same instrument"
        let filterId = pcFrom filterConn
            sourceConnections = filter (\(_, conn) -> pcTo conn == filterId) signalConnections
            sourceIds = map (pcFrom . snd) sourceConnections
        ensure (not (null sourceConnections)) ("filter node `" <> filterId <> "` must receive at least one signal input")
        pure (sourceIds, Just filterId, filterConnId : map fst sourceConnections)
      _ ->
        Left "patch must route signal through at most one filter node into the output"

  let unexpectedSignalConnectionIds =
        [ connId
        | (connId, _) <- signalConnections
        , connId `notElem` allowedSignalConnectionIds
        ]
  ensure (null unexpectedSignalConnectionIds)
    ("unsupported signal routing at connections: " <> unwords unexpectedSignalConnectionIds)

  ensure (not (null signalLayerIds)) "patch must compile at least one signal layer"
  ensure (length signalLayerIds == length (nub signalLayerIds)) "each oscillator or noise node may only appear once in the active signal path"
  ensure (length signalLayerIds <= maxLayers)
    ("patch uses " <> show (length signalLayerIds) <> " signal layers but the engine supports at most " <> show maxLayers)

  mapM_ (requireSignalSourceNode graph) signalLayerIds

  let orderedSignalLayerIds =
        [ nodeId
        | nodeId <- M.keys (pgNodes graph)
        , nodeId `elem` signalLayerIds
        ]
      layerIndexById = M.fromList (zip orderedSignalLayerIds [0 ..])
      layerAmpEnvelopeConnections =
        filter (\(_, conn) -> pcTo conn /= outputId) ampEnvelopeConnections

  layerEnvelopeResults <- mapM (compileLayerEnvelopeConnection graph orderedSignalLayerIds) layerAmpEnvelopeConnections
  let layerEnvelopePairs = mapMaybe id layerEnvelopeResults
  ensure (length layerEnvelopePairs == length layerAmpEnvelopeConnections)
    "layer amp envelope connections must target active oscillator or noise nodes"
  ensure (length layerEnvelopePairs == length (nub (map fst layerEnvelopePairs)))
    "each signal layer may have at most one amp envelope connection"
  let layerEnvelopeMap = M.fromList layerEnvelopePairs
  ensureNoInlineEnvelopeConflicts graph layerEnvelopeMap orderedSignalLayerIds

  filterEnvelope <- compileFilterEnvelope graph maybeFilterId outputEnvelope filterEnvelopeConnections
  filterSpec <- compileFilterSpec graph maybeFilterId filterEnvelope layerIndexById
  syncMap <- compileHardSyncMap graph orderedSignalLayerIds layerIndexById hardSyncConnections
  modRoutes <- compileModulations graph outputId maybeFilterId layerIndexById

  layers <- mapM (compileLayer graph layerEnvelopeMap syncMap) orderedSignalLayerIds

  let usedNodeIds =
        nub
          ( outputId
          : maybe [] pure maybeFilterId
          <> map (pcFrom . snd) ampEnvelopeConnections
          <> map (pcFrom . snd) filterEnvelopeConnections
          <> map (pcFrom . snd) hardSyncConnections
          <> map (pcTo . snd) hardSyncConnections
          <> orderedSignalLayerIds
          )
      unusedNodeIds = filter (`notElem` usedNodeIds) (M.keys (pgNodes graph))
  ensure (null unusedNodeIds)
    ("patch contains disconnected nodes: " <> unwords unusedNodeIds)

  pure
    Instrument
      { iOscs = layers
      , iLayerSpread = outLayerSpread outputNode
      , iAdsrDefault = enAdsr outputEnvelope
      , iGain = outGain outputNode
      , iFilter = filterSpec
      , iLfo1RateHz = outLfo1RateHz outputNode
      , iModRoutes = modRoutes
      , iPlayMode = outPlayMode outputNode
      , iPolyMax = outPolyMax outputNode
      , iVoiceSteal = outVoiceSteal outputNode
      }

parsePatchNode ∷ FlatYaml → String → Either String (String, PatchNode)
parsePatchNode entries nodeId = do
  let prefix = ["patch", "nodes", nodeId]
  nodeTypeRaw <- lookupKey (prefix <> ["type"]) entries
  node <-
    case canonical nodeTypeRaw of
      "oscillator" -> PatchNodeOscillator <$> parseOscillatorNode entries prefix
      "noise" -> PatchNodeNoise <$> parseNoiseNode entries prefix
      "envelope" -> PatchNodeEnvelope <$> parseEnvelopeNode entries prefix
      "filter" -> PatchNodeFilter <$> parseFilterNode entries prefix
      "output" -> PatchNodeOutput <$> parseOutputNode entries prefix
      other -> Left ("unsupported patch node type for patch.nodes." <> nodeId <> ": " <> other)
  pure (nodeId, node)

parsePatchConnection ∷ FlatYaml → String → Either String (String, PatchConnection)
parsePatchConnection entries connectionId = do
  let prefix = ["patch", "connections", connectionId]
  fromNode <- trim <$> lookupKey (prefix <> ["from"]) entries
  toNode <- trim <$> lookupKey (prefix <> ["to"]) entries
  kindRaw <- lookupKey (prefix <> ["kind"]) entries
  kind <- parseConnectionKind kindRaw
  ensure (not (null fromNode)) ("patch.connections." <> connectionId <> ".from must not be empty")
  ensure (not (null toNode)) ("patch.connections." <> connectionId <> ".to must not be empty")
  pure (connectionId, PatchConnection fromNode toNode kind)

parsePatchModulation ∷ FlatYaml → String → Either String (String, PatchModulation)
parsePatchModulation entries modulationId = do
  let prefix = ["patch", "modulations", modulationId]
  source <- parseModSrc =<< lookupKey (prefix <> ["source"]) entries
  target <- trim <$> lookupKey (prefix <> ["target"]) entries
  destination <- parsePatchModulationDestination =<< lookupKey (prefix <> ["destination"]) entries
  amount <- lookupFloatKey (prefix <> ["amount"]) entries
  ensure (not (null target)) ("patch.modulations." <> modulationId <> ".target must not be empty")
  pure
    ( modulationId
    , PatchModulation
        { pmSource = source
        , pmTarget = target
        , pmDestination = destination
        , pmAmount = amount
        }
    )

parseOscillatorNode ∷ FlatYaml → [String] → Either String OscillatorNode
parseOscillatorNode entries prefix = do
  waveform <- parseOscillatorWaveform =<< lookupKey (prefix <> ["waveform"]) entries
  level <- lookupFloatKeyDefault (prefix <> ["level"]) 1 entries
  validateNonNegative (prefix <> ["level"]) level
  pitch <- parsePitchSpec entries prefix
  ampEnv <- parseOptionalAdsr entries (prefix <> ["amp_envelope"])
  pure (OscillatorNode waveform pitch level ampEnv)

parseNoiseNode ∷ FlatYaml → [String] → Either String NoiseNode
parseNoiseNode entries prefix = do
  colorRaw <- pure (lookupTextKeyDefault (prefix <> ["color"]) "white" entries)
  mixAmount <- lookupFloatKeyDefault (prefix <> ["mix"]) 0.5 entries
  ensure (mixAmount >= 0 && mixAmount <= 1) (renderPath (prefix <> ["mix"]) <> " must be in [0,1]")
  level <- lookupFloatKeyDefault (prefix <> ["level"]) 1 entries
  validateNonNegative (prefix <> ["level"]) level
  waveform <-
    case canonical colorRaw of
      "white" -> pure WaveWhiteNoise
      "pink" -> pure WavePinkNoise
      "mix" -> pure (WaveNoiseMix mixAmount)
      other -> Left ("unsupported noise color for " <> renderPath (prefix <> ["color"]) <> ": " <> other)
  ampEnv <- parseOptionalAdsr entries (prefix <> ["amp_envelope"])
  pure (NoiseNode waveform level ampEnv)

parseEnvelopeNode ∷ FlatYaml → [String] → Either String EnvelopeNode
parseEnvelopeNode entries prefix = do
  attack <- lookupFloatKey (prefix <> ["attack"]) entries
  decay <- lookupFloatKey (prefix <> ["decay"]) entries
  sustain <- lookupFloatKey (prefix <> ["sustain"]) entries
  release <- lookupFloatKey (prefix <> ["release"]) entries
  validateAdsr prefix (ADSR attack decay sustain release)
  pure (EnvelopeNode (ADSR attack decay sustain release))

parseOptionalAdsr ∷ FlatYaml → [String] → Either String (Maybe ADSR)
parseOptionalAdsr entries prefix =
  case lookupMaybeKey (prefix <> ["attack"]) entries of
    Nothing -> pure Nothing
    Just _ -> do
      attack <- lookupFloatKey (prefix <> ["attack"]) entries
      decay <- lookupFloatKey (prefix <> ["decay"]) entries
      sustain <- lookupFloatKey (prefix <> ["sustain"]) entries
      release <- lookupFloatKey (prefix <> ["release"]) entries
      let adsr = ADSR attack decay sustain release
      validateAdsr prefix adsr
      pure (Just adsr)

parseFilterNode ∷ FlatYaml → [String] → Either String FilterNode
parseFilterNode entries prefix = do
  filterType <- parseFilterType =<< lookupKey (prefix <> ["mode"]) entries
  cutoffHz <- lookupFloatKey (prefix <> ["cutoff_hz"]) entries
  q <- lookupFloatKeyDefault (prefix <> ["q"]) 0.707 entries
  slope <- parseFilterSlope (lookupTextKeyDefault (prefix <> ["slope"]) "24" entries)
  keyTrack <- lookupFloatKeyDefault (prefix <> ["key_track"]) 0 entries
  layerTarget <- parseOptionalFilterLayerTarget entries prefix
  envAmount <- lookupFloatKeyDefault (prefix <> ["env_amount_oct"]) 0 entries
  qEnvAmount <- lookupFloatKeyDefault (prefix <> ["q_env_amount"]) 0 entries
  ensure (cutoffHz > 0) (renderPath (prefix <> ["cutoff_hz"]) <> " must be > 0")
  ensure (q > 0) (renderPath (prefix <> ["q"]) <> " must be > 0")
  ensure (keyTrack >= 0 && keyTrack <= 1) (renderPath (prefix <> ["key_track"]) <> " must be in [0,1]")
  pure
    FilterNode
      { fnType = filterType
      , fnCutoffHz = cutoffHz
      , fnQ = q
      , fnSlope = slope
      , fnKeyTrack = keyTrack
      , fnLayerTarget = layerTarget
      , fnEnvAmountOct = envAmount
      , fnQEnvAmount = qEnvAmount
      }

parseOptionalFilterLayerTarget ∷ FlatYaml → [String] → Either String (Maybe String)
parseOptionalFilterLayerTarget entries prefix =
  case lookupMaybeKey (prefix <> ["layer_target"]) entries of
    Nothing -> pure Nothing
    Just raw -> do
      let target = trim raw
      ensure (not (null target)) (renderPath (prefix <> ["layer_target"]) <> " must not be empty")
      pure (Just target)

parseOutputNode ∷ FlatYaml → [String] → Either String OutputNode
parseOutputNode entries prefix = do
  gain <- lookupFloatKeyDefault (prefix <> ["gain"]) 1 entries
  spread <- lookupFloatKeyDefault (prefix <> ["layer_spread"]) 0 entries
  lfo1RateHz <- lookupFloatKeyDefault (prefix <> ["lfo1_rate_hz"]) 0 entries
  playMode <- parsePlayMode (lookupTextKeyDefault (prefix <> ["play_mode"]) "poly" entries)
  polyMax <- lookupIntKeyDefault (prefix <> ["poly_max"]) 8 entries
  voiceSteal <- parseVoiceSteal (lookupTextKeyDefault (prefix <> ["voice_steal"]) "quietest" entries)
  validateNonNegative (prefix <> ["gain"]) gain
  validateNonNegative (prefix <> ["lfo1_rate_hz"]) lfo1RateHz
  ensure (spread >= 0 && spread <= 1) (renderPath (prefix <> ["layer_spread"]) <> " must be in [0,1]")
  ensure (polyMax > 0) (renderPath (prefix <> ["poly_max"]) <> " must be > 0")
  pure
    OutputNode
      { outGain = gain
      , outLayerSpread = spread
      , outLfo1RateHz = lfo1RateHz
      , outPlayMode = playMode
      , outPolyMax = polyMax
      , outVoiceSteal = voiceSteal
      }

parsePitchSpec ∷ FlatYaml → [String] → Either String PitchSpec
parsePitchSpec entries prefix = do
  octaves <- lookupIntKeyDefault (prefix <> ["pitch", "octaves"]) 0 entries
  semitones <- lookupFloatKeyDefault (prefix <> ["pitch", "semitones"]) 0 entries
  cents <- lookupFloatKeyDefault (prefix <> ["pitch", "cents"]) 0 entries
  hzOffset <- lookupFloatKeyDefault (prefix <> ["pitch", "hz_offset"]) 0 entries
  pure
    PitchSpec
      { psOctaves = octaves
      , psSemitones = semitones
      , psCents = cents
      , psHzOffset = hzOffset
      }

compileLayer ∷ PatchGraph → Map String EnvelopeNode → Map String SyncSpec → String → Either String OscLayer
compileLayer graph layerEnvelopeMap syncMap nodeId = do
  syncSpec <-
    case M.lookup nodeId syncMap of
      Nothing -> pure NoSync
      Just syncSpec' -> pure syncSpec'
  node <- requireNode graph nodeId
  case node of
    PatchNodeOscillator oscillator ->
      pure
        OscLayer
          { olWaveform = onWaveform oscillator
          , olPitch = onPitch oscillator
          , olLevel = onLevel oscillator
          , olSync = syncSpec
          , olAmpEnv = resolveLayerAmpEnv (onAmpEnv oscillator) (enAdsr <$> M.lookup nodeId layerEnvelopeMap)
          }
    PatchNodeNoise noise ->
      pure
        OscLayer
          { olWaveform = nnWaveform noise
          , olPitch = PitchSpec 0 0 0 0
          , olLevel = nnLevel noise
          , olSync = syncSpec
          , olAmpEnv = resolveLayerAmpEnv (nnAmpEnv noise) (enAdsr <$> M.lookup nodeId layerEnvelopeMap)
          }
    _ ->
      Left ("node `" <> nodeId <> "` is not a signal source and cannot compile into a layer")

resolveLayerAmpEnv ∷ Maybe ADSR → Maybe ADSR → Maybe ADSR
resolveLayerAmpEnv inlineEnvelope connectedEnvelope =
  case inlineEnvelope of
    Just adsr -> Just adsr
    Nothing -> connectedEnvelope

compileLayerEnvelopeConnection
  ∷ PatchGraph
  → [String]
  → (String, PatchConnection)
  → Either String (Maybe (String, EnvelopeNode))
compileLayerEnvelopeConnection graph layerNodeIds (_, conn)
  | pcTo conn `elem` layerNodeIds = do
      envelope <- requireEnvelopeNode graph (pcFrom conn)
      pure (Just (pcTo conn, envelope))
  | otherwise = pure Nothing

ensureNoInlineEnvelopeConflicts
  ∷ PatchGraph
  → Map String EnvelopeNode
  → [String]
  → Either String ()
ensureNoInlineEnvelopeConflicts graph layerEnvelopeMap layerNodeIds =
  mapM_ ensureNoConflict layerNodeIds
  where
    ensureNoConflict nodeId =
      ensure (not (hasInlineAmpEnvelope graph nodeId && M.member nodeId layerEnvelopeMap))
        ("signal layer `" <> nodeId <> "` cannot use both an inline amp_envelope block and an amp_envelope connection")

hasInlineAmpEnvelope ∷ PatchGraph → String → Bool
hasInlineAmpEnvelope graph nodeId =
  case M.lookup nodeId (pgNodes graph) of
    Just (PatchNodeOscillator oscillator) -> onAmpEnv oscillator /= Nothing
    Just (PatchNodeNoise noise) -> nnAmpEnv noise /= Nothing
    _ -> False

compileFilterEnvelope
  ∷ PatchGraph
  → Maybe String
  → EnvelopeNode
  → [(String, PatchConnection)]
  → Either String EnvelopeNode
compileFilterEnvelope _ Nothing outputEnvelope connections = do
  ensure (null connections) "filter envelope connections require an active filter node"
  pure outputEnvelope
compileFilterEnvelope graph (Just filterId) outputEnvelope connections =
  case connections of
    [] -> pure outputEnvelope
    _ -> do
      ensure (all ((== filterId) . pcTo . snd) connections)
        ("filter envelope connections must target the active filter node `" <> filterId <> "`")
      case connections of
        [(_, conn)] -> requireEnvelopeNode graph (pcFrom conn)
        _ -> Left ("filter node `" <> filterId <> "` must have at most one filter envelope connection")

compileFilterSpec ∷ PatchGraph → Maybe String → EnvelopeNode → Map String Int → Either String (Maybe FilterSpec)
compileFilterSpec _ Nothing _ _ = pure Nothing
compileFilterSpec graph (Just filterId) envelopeNode layerIndexById = do
  filterNode <- requireFilterNode graph filterId
  target <- compileFilterTarget filterId layerIndexById (fnLayerTarget filterNode)
  pure
    (Just
      FilterSpec
        { fType = fnType filterNode
        , fCutoffHz = fnCutoffHz filterNode
        , fQ = fnQ filterNode
        , fSlope = fnSlope filterNode
        , fKeyTrack = KeyTrack (fnKeyTrack filterNode)
        , fTarget = target
        , fEnvAmountOct = fnEnvAmountOct filterNode
        , fEnvADSR = enAdsr envelopeNode
        , fQEnvAmount = fnQEnvAmount filterNode
        }
    )

compileFilterTarget ∷ String → Map String Int → Maybe String → Either String FilterTarget
compileFilterTarget _ _ Nothing = pure FilterTargetAll
compileFilterTarget _ _ (Just targetRaw)
  | canonical targetRaw == "all" = pure FilterTargetAll
compileFilterTarget filterId layerIndexById (Just targetRaw) =
  case M.lookup targetRaw layerIndexById of
    Just layerIx -> pure (FilterTargetLayer layerIx)
    Nothing ->
      Left
        ( "filter node `" <> filterId <> "` layer_target must reference an active oscillator or noise node, or `all`: "
            <> targetRaw
        )

compileHardSyncMap
  ∷ PatchGraph
  → [String]
  → Map String Int
  → [(String, PatchConnection)]
  → Either String (Map String SyncSpec)
compileHardSyncMap graph layerNodeIds layerIndexById connections = do
  pairs <- mapM compileConnection connections
  let byTarget = M.fromListWith (++) [(targetId, [masterIx]) | (targetId, masterIx) <- pairs]
  mapM_ ensureSingleSync (M.toList byTarget)
  pure (M.map toSyncSpec byTarget)
  where
    toSyncSpec [masterIx] = HardSyncTo masterIx
    toSyncSpec _ = NoSync

    compileConnection (_, conn) = do
      ensure (pcFrom conn `elem` layerNodeIds) ("hard sync source `" <> pcFrom conn <> "` must be an active oscillator layer")
      ensure (pcTo conn `elem` layerNodeIds) ("hard sync target `" <> pcTo conn <> "` must be an active oscillator layer")
      _ <- requireOscillatorNode graph (pcFrom conn)
      _ <- requireOscillatorNode graph (pcTo conn)
      ensure (pcFrom conn /= pcTo conn) "hard sync source and target must be different nodes"
      case M.lookup (pcFrom conn) layerIndexById of
        Nothing -> Left ("missing hard sync source layer index for `" <> pcFrom conn <> "`")
        Just masterIx -> pure (pcTo conn, masterIx)

    ensureSingleSync (targetId, masterIxs) =
      ensure (length masterIxs == 1) ("oscillator `" <> targetId <> "` must have at most one hard sync source")

compileModulations
  ∷ PatchGraph
  → String
  → Maybe String
  → Map String Int
  → Either String [ModRoute]
compileModulations graph outputId maybeFilterId layerIndexById = do
  let namedModulations = M.toList (pgModulations graph)
  ensure (length namedModulations <= maxModRoutes)
    ("patch uses " <> show (length namedModulations) <> " modulation routes but the engine supports at most " <> show maxModRoutes)
  mapM compileNamedModulation namedModulations
  where
    compileNamedModulation (modulationId, modulation) = do
      dst <-
        case pmDestination modulation of
          PatchModDstPitchCents ->
            case M.lookup (pmTarget modulation) layerIndexById of
              Just layerIx ->
                pure (ModDstLayerPitchCents layerIx)
              Nothing ->
                Left
                  ( "patch.modulations."
                      <> modulationId
                      <> " must target an active oscillator or noise layer for pitch modulation: `"
                      <> pmTarget modulation
                      <> "`"
                  )
          PatchModDstFilterCutoffOct ->
            case maybeFilterId of
              Just filterId
                | pmTarget modulation == filterId ->
                    pure ModDstFilterCutoffOct
                | otherwise ->
                    Left
                      ( "patch.modulations."
                          <> modulationId
                          <> " must target the active filter node `"
                          <> filterId
                          <> "` for filter cutoff modulation"
                      )
              Nothing ->
                Left ("patch.modulations." <> modulationId <> " targets filter cutoff modulation but the patch has no active filter")
          PatchModDstAmpGain ->
            if pmTarget modulation == outputId
              then pure ModDstAmpGain
              else
                Left
                  ( "patch.modulations."
                      <> modulationId
                      <> " must target the output node `"
                      <> outputId
                      <> "` for amp gain modulation"
                  )
      pure
        ModRoute
          { mrSrc = pmSource modulation
          , mrDst = dst
          , mrAmount = pmAmount modulation
          }

requireSingleOutputNode ∷ PatchGraph → Either String String
requireSingleOutputNode graph =
  case [nodeId | (nodeId, PatchNodeOutput _) <- M.toList (pgNodes graph)] of
    [outputId] -> pure outputId
    [] -> Left "patch must define exactly one output node"
    _ -> Left "patch must define exactly one output node"

requireSingleNamedConnection
  ∷ String
  → String
  → [(String, PatchConnection)]
  → Either String (String, PatchConnection)
requireSingleNamedConnection label targetId connections =
  case filter (\(_, conn) -> pcTo conn == targetId) connections of
    [conn] -> pure conn
    [] -> Left ("patch requires exactly one " <> label <> " connection into `" <> targetId <> "`")
    _ -> Left ("patch allows only one " <> label <> " connection into `" <> targetId <> "`")

validateConnectionRefs ∷ PatchGraph → Either String ()
validateConnectionRefs graph =
  mapM_
    validateConnection
    (M.toList (pgConnections graph))
  where
    validateConnection (connectionId, conn) = do
      ensure (M.member (pcFrom conn) (pgNodes graph))
        ("patch.connections." <> connectionId <> ".from references unknown node `" <> pcFrom conn <> "`")
      ensure (M.member (pcTo conn) (pgNodes graph))
        ("patch.connections." <> connectionId <> ".to references unknown node `" <> pcTo conn <> "`")

validateModulationRefs ∷ PatchGraph → Either String ()
validateModulationRefs graph =
  mapM_
    validateModulation
    (M.toList (pgModulations graph))
  where
    validateModulation (modulationId, modulation) =
      ensure (M.member (pmTarget modulation) (pgNodes graph))
        ("patch.modulations." <> modulationId <> ".target references unknown node `" <> pmTarget modulation <> "`")

requireNode ∷ PatchGraph → String → Either String PatchNode
requireNode graph nodeId =
  case M.lookup nodeId (pgNodes graph) of
    Just node -> pure node
    Nothing -> Left ("unknown patch node `" <> nodeId <> "`")

requireOutputNode ∷ PatchGraph → String → Either String OutputNode
requireOutputNode graph nodeId = do
  node <- requireNode graph nodeId
  case node of
    PatchNodeOutput outputNode -> pure outputNode
    _ -> Left ("node `" <> nodeId <> "` is not an output node")

requireEnvelopeNode ∷ PatchGraph → String → Either String EnvelopeNode
requireEnvelopeNode graph nodeId = do
  node <- requireNode graph nodeId
  case node of
    PatchNodeEnvelope envelopeNode -> pure envelopeNode
    _ -> Left ("node `" <> nodeId <> "` is not an envelope node")

requireFilterNode ∷ PatchGraph → String → Either String FilterNode
requireFilterNode graph nodeId = do
  node <- requireNode graph nodeId
  case node of
    PatchNodeFilter filterNode -> pure filterNode
    _ -> Left ("node `" <> nodeId <> "` is not a filter node")

requireOscillatorNode ∷ PatchGraph → String → Either String OscillatorNode
requireOscillatorNode graph nodeId = do
  node <- requireNode graph nodeId
  case node of
    PatchNodeOscillator oscillatorNode -> pure oscillatorNode
    _ -> Left ("node `" <> nodeId <> "` is not an oscillator node")

requireSignalSourceNode ∷ PatchGraph → String → Either String ()
requireSignalSourceNode graph nodeId = do
  node <- requireNode graph nodeId
  case node of
    PatchNodeOscillator _ -> pure ()
    PatchNodeNoise _ -> pure ()
    _ -> Left ("node `" <> nodeId <> "` is not a signal source")

isSignalSourceNodeId ∷ PatchGraph → String → Bool
isSignalSourceNodeId graph nodeId =
  case M.lookup nodeId (pgNodes graph) of
    Just (PatchNodeOscillator _) -> True
    Just (PatchNodeNoise _) -> True
    _ -> False

isFilterNodeId ∷ PatchGraph → String → Bool
isFilterNodeId graph nodeId =
  case M.lookup nodeId (pgNodes graph) of
    Just (PatchNodeFilter _) -> True
    _ -> False

parseConnectionKind ∷ String → Either String ConnectionKind
parseConnectionKind raw =
  case canonical raw of
    "signal" -> pure ConnectionSignal
    "ampenvelope" -> pure ConnectionAmpEnvelope
    "filterenvelope" -> pure ConnectionFilterEnvelope
    "hardsync" -> pure ConnectionHardSync
    other -> Left ("unsupported patch connection kind: " <> other)

parseModSrc ∷ String → Either String ModSrc
parseModSrc raw =
  case canonical raw of
    "lfo1" -> pure ModSrcLfo1
    "ampenvelope" -> pure ModSrcEnvAmp
    "ampenv" -> pure ModSrcEnvAmp
    "envamp" -> pure ModSrcEnvAmp
    "filterenvelope" -> pure ModSrcEnvFilter
    "filterenv" -> pure ModSrcEnvFilter
    "envfilter" -> pure ModSrcEnvFilter
    "keytrack" -> pure ModSrcKeyTrack
    "channelaftertouch" -> pure ModSrcChanAftertouch
    "chanaftertouch" -> pure ModSrcChanAftertouch
    "polyaftertouch" -> pure ModSrcPolyAftertouch
    "noteaftertouch" -> pure ModSrcPolyAftertouch
    other -> Left ("unsupported patch modulation source: " <> other)

parsePatchModulationDestination ∷ String → Either String PatchModulationDestination
parsePatchModulationDestination raw =
  case canonical raw of
    "pitchcents" -> pure PatchModDstPitchCents
    "layerpitchcents" -> pure PatchModDstPitchCents
    "filtercutoffoct" -> pure PatchModDstFilterCutoffOct
    "cutoffoct" -> pure PatchModDstFilterCutoffOct
    "ampgain" -> pure PatchModDstAmpGain
    "gain" -> pure PatchModDstAmpGain
    other -> Left ("unsupported patch modulation destination: " <> other)

parseOscillatorWaveform ∷ String → Either String Waveform
parseOscillatorWaveform raw =
  case canonical raw of
    "sine" -> pure WaveSine
    "saw" -> pure WaveSaw
    "square" -> pure WaveSquare
    "triangle" -> pure WaveTriangle
    other -> Left ("unsupported oscillator waveform: " <> other)

parseFilterType ∷ String → Either String FilterType
parseFilterType raw =
  case canonical raw of
    "lowpass" -> pure FLP
    "lp" -> pure FLP
    "highpass" -> pure FHP
    "hp" -> pure FHP
    "bandpass" -> pure FBP
    "bp" -> pure FBP
    other -> Left ("unsupported filter mode: " <> other)

parseFilterSlope ∷ String → Either String FilterSlope
parseFilterSlope raw =
  case canonical raw of
    "12" -> pure S12
    "s12" -> pure S12
    "12db" -> pure S12
    "24" -> pure S24
    "s24" -> pure S24
    "24db" -> pure S24
    "36" -> pure S36
    "s36" -> pure S36
    "36db" -> pure S36
    "48" -> pure S48
    "s48" -> pure S48
    "48db" -> pure S48
    other -> Left ("unsupported filter slope: " <> other)

parsePlayMode ∷ String → Either String PlayMode
parsePlayMode raw =
  case canonical raw of
    "poly" -> pure Poly
    "monolegato" -> pure MonoLegato
    other -> Left ("unsupported play mode: " <> other)

parseVoiceSteal ∷ String → Either String VoiceSteal
parseVoiceSteal raw =
  case canonical raw of
    "quietest" -> pure StealQuietest
    other -> Left ("unsupported voice steal mode: " <> other)

validateNonNegative ∷ [String] → Float → Either String ()
validateNonNegative key value =
  ensure (value >= 0) (renderPath key <> " must be >= 0")

validateAdsr ∷ [String] → ADSR → Either String ()
validateAdsr prefix adsr = do
  ensure (aAttackSec adsr >= 0) (renderPath (prefix <> ["attack"]) <> " must be >= 0")
  ensure (aDecaySec adsr >= 0) (renderPath (prefix <> ["decay"]) <> " must be >= 0")
  ensure (aSustain adsr >= 0 && aSustain adsr <= 1) (renderPath (prefix <> ["sustain"]) <> " must be in [0,1]")
  ensure (aReleaseSec adsr >= 0) (renderPath (prefix <> ["release"]) <> " must be >= 0")

canonical ∷ String → String
canonical = map toLower . filter isAlphaNum . trim
