{-# LANGUAGE Strict, UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audio.Thread
  ( startAudioSystem
  , stopAudioSystem
  , sendAudio
  , AudioSystem(..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void)
import Data.IORef
import Data.Word (Word32, Word64)
import Foreign hiding (void)
import Foreign.C
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)

import qualified Data.Vector.Mutable as MV

import Engine.Core.Thread
import Engine.Core.Queue as Q
import Audio.Types
import Audio.Envelope
import Audio.Oscillator
import Audio.Filter
  ( FilterState(..)
  , filterStep
  , filterStateInit
  , filterRetune
  , keyTrackedBaseCutoff
  , filterEnvCutoff
  , filterEnvQ
  )
import Audio.Filter.Types (FilterSpec(..))
import Sound.Miniaudio
import Sound.Miniaudio.RingBuffer

--------------------------------------------------------------------------------
-- Voice representation (oscillator + ADSR + optional filter)
--------------------------------------------------------------------------------

data Voice = Voice
  { vOsc        ∷ !Osc
  , vFilter     ∷ !(Maybe FilterState)
  , vFiltEnv    ∷ !(Maybe EnvState)  -- optional filter envelope state
  , vFiltTick   ∷ !Int               -- frames until next filter envelope step
  , vNoteHz     ∷ !Float             -- cached Hz for key tracking

  , vHoldRemain ∷ !Int               -- frames until we trigger release

  , vBaseAmpL   ∷ !Float             -- amp * panL (no instrument gain)
  , vBaseAmpR   ∷ !Float             -- amp * panR (no instrument gain)
  , vAmpL       ∷ !Float             -- includes instrument gain
  , vAmpR       ∷ !Float

  , vADSR       ∷ !ADSR
  , vEnv        ∷ !EnvState
  , vNoteId     ∷ !(Maybe NoteId)         -- Nothing for one-shots
  , vInstrId    ∷ !(Maybe InstrumentId)   -- Nothing for one-shots

  , vStartedAt  ∷ !Word64
  }

data AudioState = AudioState
  { stVoices         ∷ !(MV.IOVector Voice)
  , stActiveCount    ∷ !Int
  , stMixBuf         ∷ !(ForeignPtr CFloat) -- interleaved stereo
  , stInstruments    ∷ !(MV.IOVector (Maybe Instrument)) -- length 256
  , stGlideSec       ∷ !(MV.IOVector Float) -- length 256
  , stLegFiltRetrig  ∷ !(MV.IOVector Bool)  -- length 256
  , stLegAmpRetrig   ∷ !(MV.IOVector Bool)  -- length 256
  , stNow            ∷ !Word64
  }

data AudioSystem = AudioSystem
  { asHandle     ∷ !AudioHandle
  , asThread     ∷ !ThreadState
  , asDevice     ∷ !(Ptr MaDevice)
  , asCfg        ∷ !(Ptr MaDeviceConfig)
  , asCallback   ∷ !(FunPtr MaDataCallback)
  , asUserData   ∷ !(StablePtr AudioUserData)
  , asRingBuffer ∷ !(Ptr MaRB)
  }

data AudioUserData = AudioUserData
  { aud_rb            ∷ !(Ptr MaRB)
  , aud_channels      ∷ !Word32
  , aud_underruns     ∷ !(IORef Word64) -- NEW: callback increments on short reads
  }

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

sendAudio ∷ AudioSystem → AudioMsg → IO ()
sendAudio sys msg = Q.writeQueue (audioQueue (asHandle sys)) msg

startAudioSystem ∷ IO AudioSystem
startAudioSystem = do
  -- Reasonable low-latency defaults that still perform well:
  -- - chunkFrames 256 => ~5.3ms of synthesis work per chunk @ 48k
  -- - ring buffer capacity 8192 => ~170ms max, but we'll keep target much lower
  -- - target queue ~= 4 chunks => ~21ms of queued audio
  let sr  = 48000 ∷ Word32
      ch  = 2     ∷ Word32
      chunkFrames = 256 ∷ Int
      rbCapacityFrames = 8192 ∷ Word32
      maxVoices = 256 ∷ Int

  q <- Q.newQueue
  let h = AudioHandle q sr ch

  rb <- rbCreateF32 rbCapacityFrames ch
  when (rb == nullPtr) $ error "rbCreateF32 failed"

  underrunRef <- newIORef 0
  udStable <- newStablePtr (AudioUserData rb ch underrunRef)
  let udPtr = castStablePtrToPtr udStable

  let cb ∷ MaDataCallback
      cb dev pOut _pIn frameCount = do
        ud <- hs_ma_device_get_user_data dev
        AudioUserData rb' ch' uref <- deRefStablePtr (castPtrToStablePtr ud)
        let outF ∷ Ptr CFloat
            outF = castPtr pOut
        got <- rbReadF32 rb' outF frameCount
        when (got < frameCount) $ do
          -- track underrun and zero-fill remainder
          modifyIORef' uref (+1)

          let samplesGot  = fromIntegral got * fromIntegral ch'
              samplesWant = fromIntegral frameCount * fromIntegral ch'
              remainingSamples = samplesWant - samplesGot
              byteCount = remainingSamples * sizeOf (undefined ∷ CFloat)
          fillBytes (outF `advancePtr` fromIntegral samplesGot) 0 byteCount

  cbFun <- mkMaDataCallback cb

  cfg <- hs_ma_device_config_init_playback MaFormatF32 ch sr cbFun udPtr
  when (cfg == nullPtr) $ error "Failed to allocate ma_device_config"

  dev <- hs_ma_device_malloc
  when (dev == nullPtr) $ error "Failed to allocate ma_device"

  r <- ma_device_init nullPtr cfg dev
  when (r /= MA_SUCCESS) $ error ("ma_device_init failed: " <> show r)

  r2 <- ma_device_start dev
  when (r2 /= MA_SUCCESS) $ error ("ma_device_start failed: " <> show r2)

  voicesVec <- MV.new maxVoices
  instVec <- MV.replicate 256 Nothing
  glideVec <- MV.replicate 256 0
  legFiltRetrigVec <- MV.replicate 256 False
  legAmpRetrigVec <- MV.replicate 256 False
  mixBuf <- mallocForeignPtrArray (chunkFrames * 2)
  stRef <- newIORef (AudioState voicesVec 0 mixBuf instVec glideVec legFiltRetrigVec legAmpRetrigVec 0)

  controlRef <- newIORef ThreadRunning
  tid <- forkIO $ runAudioLoop h controlRef stRef rb chunkFrames underrunRef

  let ts = ThreadState controlRef tid
  pure AudioSystem
    { asHandle = h
    , asThread = ts
    , asDevice = dev
    , asCfg = cfg
    , asCallback = cbFun
    , asUserData = udStable
    , asRingBuffer = rb
    }

stopAudioSystem ∷ AudioSystem → IO ()
stopAudioSystem sys = do
  writeIORef (tsRunning (asThread sys)) ThreadStopped
  Q.writeQueue (audioQueue (asHandle sys)) AudioShutdown
  threadDelay 20000

  void (ma_device_stop (asDevice sys))
  ma_device_uninit (asDevice sys)

  hs_ma_device_free (asDevice sys)
  hs_ma_device_config_free (asCfg sys)

  freeHaskellFunPtr (asCallback sys)
  freeStablePtr (asUserData sys)

  rbDestroy (asRingBuffer sys)

--------------------------------------------------------------------------------
-- Audio thread loop
--------------------------------------------------------------------------------

runAudioLoop
  ∷ AudioHandle
  → IORef ThreadControl
  → IORef AudioState
  → Ptr MaRB
  → Int
  → IORef Word64
  → IO ()
runAudioLoop h controlRef stRef rb chunkFrames underrunRef = go 0
  where
    -- print at most once per second, only if underruns increased
    go ∷ Word64 → IO ()
    go lastPrintedCount = do
      control <- readIORef controlRef
      case control of
        ThreadStopped -> pure ()
        ThreadPaused  -> threadDelay 10000 >> go lastPrintedCount
        ThreadRunning -> do
          st0 <- readIORef stRef
          st1 <- processMsgs h rb st0
          st2 <- renderIfNeeded h rb chunkFrames st1
          writeIORef stRef st2

          -- check underruns and print only on change (rate-limited)
          u <- readIORef underrunRef
          when (u /= lastPrintedCount) $ do
            putStrLn ("[audio] underruns: " <> show u)
            -- sleep a bit so we don't spam if underruns happen every callback
            threadDelay 1000000
            go u

          threadDelay 1000
          go lastPrintedCount

processMsgs ∷ AudioHandle → Ptr MaRB → AudioState → IO AudioState
processMsgs h rb st = do
  m <- Q.tryReadQueue (audioQueue h)
  case m of
    Nothing -> pure st
    Just msg ->
      case msg of
        AudioShutdown -> do
          rbReset rb
          pure st { stActiveCount = 0 }

        AudioStopAll -> do
          rbReset rb
          processMsgs h rb st { stActiveCount = 0 }

        AudioSetInstrument iid inst -> do
          st'  <- setInstrument iid inst st
          st'' <- applyInstrumentToActiveVoices (sampleRate h) iid inst st'
          processMsgs h rb st''

        AudioSetGlideSec iid gs -> do
          st' <- setGlide iid gs st
          processMsgs h rb st'

        AudioSetLegatoFilterRetrig iid rf -> do
          st' <- setLegatoFilterRetrig iid rf st
          processMsgs h rb st'

        AudioSetLegatoAmpRetrig iid ra -> do
          st' <- setLegatoAmpRetrig iid ra st
          processMsgs h rb st'

        AudioNoteOffInstrument iid -> do
          st' <- releaseInstrumentAllVoices iid st
          processMsgs h rb st'

        AudioPlayBeep a p f d e -> do
          st' <- addBeepVoice h st a p f d e
          processMsgs h rb st'

        AudioNoteOn iid a p nid eOverride -> do
          st' <- addInstrumentNote h st iid a p nid eOverride
          processMsgs h rb st'

        AudioNoteOff iid nid -> do
          st' <- releaseInstrumentNote iid nid st
          processMsgs h rb st'

--------------------------------------------------------------------------------
-- Instrument tables / config
--------------------------------------------------------------------------------

setInstrument ∷ InstrumentId → Instrument → AudioState → IO AudioState
setInstrument (InstrumentId i) inst st = do
  let vec = stInstruments st
  if i < 0 || i >= MV.length vec
    then do
      putStrLn ("[audio] warning: InstrumentId out of range: " <> show i)
      pure st
    else do
      MV.write vec i (Just inst)
      pure st

setGlide ∷ InstrumentId → Float → AudioState → IO AudioState
setGlide (InstrumentId i) gs st = do
  let vec = stGlideSec st
      gs' = max 0 gs
  if i < 0 || i >= MV.length vec
    then pure st
    else MV.write vec i gs' >> pure st

lookupGlide ∷ InstrumentId → AudioState → IO Float
lookupGlide (InstrumentId i) st = do
  let vec = stGlideSec st
  if i < 0 || i >= MV.length vec
    then pure 0
    else MV.read vec i

setLegatoFilterRetrig ∷ InstrumentId → Bool → AudioState → IO AudioState
setLegatoFilterRetrig (InstrumentId i) rf st = do
  let vec = stLegFiltRetrig st
  if i < 0 || i >= MV.length vec
    then pure st
    else MV.write vec i rf >> pure st

lookupLegatoFilterRetrig ∷ InstrumentId → AudioState → IO Bool
lookupLegatoFilterRetrig (InstrumentId i) st = do
  let vec = stLegFiltRetrig st
  if i < 0 || i >= MV.length vec
    then pure False
    else MV.read vec i

setLegatoAmpRetrig ∷ InstrumentId → Bool → AudioState → IO AudioState
setLegatoAmpRetrig (InstrumentId i) ra st = do
  let vec = stLegAmpRetrig st
  if i < 0 || i >= MV.length vec
    then pure st
    else MV.write vec i ra >> pure st

lookupLegatoAmpRetrig ∷ InstrumentId → AudioState → IO Bool
lookupLegatoAmpRetrig (InstrumentId i) st = do
  let vec = stLegAmpRetrig st
  if i < 0 || i >= MV.length vec
    then pure False
    else MV.read vec i

--------------------------------------------------------------------------------
-- Live instrument updates
--------------------------------------------------------------------------------

applyInstrumentToActiveVoices ∷ Word32 → InstrumentId → Instrument → AudioState → IO AudioState
applyInstrumentToActiveVoices srW iid inst st = do
  let vec = stVoices st
      n   = stActiveCount st
      srF = fromIntegral srW ∷ Float

      needsFiltEnv ∷ FilterSpec → Bool
      needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

  let go i
        | i >= n = pure ()
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v /= Just iid
              then go (i+1)
              else do
                let osc1 = (vOsc v) { oWaveform = iWaveform inst }

                let ampL' = vBaseAmpL v * iGain inst
                    ampR' = vBaseAmpR v * iGain inst

                let adsr' = iAdsrDefault inst

                let (mfilt', mfenv') =
                      case iFilter inst of
                        Nothing ->
                          (Nothing, Nothing)
                        Just spec ->
                          let env' =
                                if needsFiltEnv spec
                                  then case vFiltEnv v of
                                         Nothing -> Just envInit
                                         Just fe -> Just fe
                                  else Nothing

                              fs' =
                                case vFilter v of
                                  Nothing ->
                                    filterStateInit srF (vNoteHz v) spec
                                  Just fs0 ->
                                    let baseCutoff = keyTrackedBaseCutoff (vNoteHz v) spec
                                        qEff      = fQ spec
                                        fs1 = fs0 { fsSpec = spec }
                                        fs2 = filterRetune srF baseCutoff qEff fs1
                                    in fs2
                          in (Just fs', env')

                let v' = v
                      { vOsc      = osc1
                      , vAmpL     = ampL'
                      , vAmpR     = ampR'
                      , vADSR     = adsr'
                      , vFilter   = mfilt'
                      , vFiltEnv  = mfenv'
                      , vFiltTick = 0
                      }

                MV.write vec i v'
                go (i+1)

  go 0
  pure st

--------------------------------------------------------------------------------
-- NoteId conversion
--------------------------------------------------------------------------------

noteIdToHz ∷ NoteId → Float
noteIdToHz nid =
  case nid of
    NoteHz hz     -> hz
    NoteMidi midi -> midiToHz midi

midiToHz ∷ Int → Float
midiToHz n =
  440 * (2 ** ((fromIntegral n - 69) / 12))

--------------------------------------------------------------------------------
-- Voice creation + robust legato portamento (most-recent voice wins)
--------------------------------------------------------------------------------

findLegatoVoiceIxNewest ∷ InstrumentId → AudioState → IO (Maybe Int)
findLegatoVoiceIxNewest iid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i bestIx bestStamp
        | i >= n = pure bestIx
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid && eStage (vEnv v) /= EnvRelease && eStage (vEnv v) /= EnvDone
              then
                if vStartedAt v >= bestStamp
                  then go (i+1) (Just i) (vStartedAt v)
                  else go (i+1) bestIx bestStamp
              else go (i+1) bestIx bestStamp
  go 0 Nothing 0

addBeepVoice ∷ AudioHandle → AudioState → Float → Float → Float → Float → ADSR → IO AudioState
addBeepVoice h st amp pan freqHz durSec adsrSpec = do
  let n = stActiveCount st
      vec = stVoices st
      cap = MV.length vec
  if n >= cap
    then pure st
    else do
      let srF = fromIntegral (sampleRate h) ∷ Float
          holdFrames = max 0 (floor (realToFrac durSec * srF) ∷ Int)
          (baseL, baseR) = panGains amp pan
          ampL = baseL
          ampR = baseR
          adsr' = adsrSpec { aSustain = clamp01 (aSustain adsrSpec) }
          env0 = envInit
          osc0 = oscInit WaveSine srF freqHz
          v = Voice osc0 Nothing Nothing 0 freqHz holdFrames baseL baseR ampL ampR
                    adsr' env0 Nothing Nothing (stNow st)

      MV.write vec n v
      pure st { stActiveCount = n + 1, stNow = stNow st + 1 }

addInstrumentNote
  ∷ AudioHandle
  → AudioState
  → InstrumentId
  → Float
  → Float
  → NoteId
  → Maybe ADSR
  → IO AudioState
addInstrumentNote h st iid amp pan nid adsrOverride = do
  instM <- lookupInstrument iid st
  case instM of
    Nothing -> do
      putStrLn ("[audio] warning: instrument not defined: " <> show iid)
      pure st
    Just inst -> do
      let srF = fromIntegral (sampleRate h) ∷ Float
          freqHz = noteIdToHz nid
          holdFrames = maxBound `div` 4
          now' = stNow st + 1

          needsFiltEnv spec = fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0

      glideSec <- lookupGlide iid st
      legRetrigFilt <- lookupLegatoFilterRetrig iid st
      legRetrigAmp <- lookupLegatoAmpRetrig iid st
      mLegIx <- findLegatoVoiceIxNewest iid st

      case mLegIx of
        Just ix -> do
          v <- MV.read (stVoices st) ix

          let osc1 =
                oscSetGlideSec srF glideSec $
                oscSetHz srF freqHz $
                (vOsc v)

              ampL' = vBaseAmpL v * iGain inst
              ampR' = vBaseAmpR v * iGain inst
              adsr' = iAdsrDefault inst

              envAmp' = if legRetrigAmp then envInit else vEnv v

              (mfilt', mfenv') =
                case iFilter inst of
                  Nothing ->
                    (Nothing, Nothing)
                  Just spec ->
                    let fs' =
                          case vFilter v of
                            Nothing -> filterStateInit srF freqHz spec
                            Just fs0 ->
                              let baseCutoff = keyTrackedBaseCutoff freqHz spec
                                  qEff = fQ spec
                                  fs1 = fs0 { fsSpec = spec }
                                  fs2 = filterRetune srF baseCutoff qEff fs1
                              in fs2

                        env' =
                          if needsFiltEnv spec
                            then
                              if legRetrigFilt
                                then Just envInit
                                else case vFiltEnv v of
                                       Nothing -> Just envInit
                                       Just fe -> Just fe
                            else Nothing
                    in (Just fs', env')

              v' = v
                { vOsc = osc1
                , vNoteHz = freqHz
                , vNoteId = Just nid
                , vHoldRemain = holdFrames
                , vAmpL = ampL'
                , vAmpR = ampR'
                , vADSR = adsr'
                , vEnv = envAmp'
                , vFilter = mfilt'
                , vFiltEnv = mfenv'
                , vFiltTick = 0
                , vStartedAt = now'
                }

          MV.write (stVoices st) ix v'
          pure st { stNow = now' }

        Nothing -> do
          let n = stActiveCount st
              vec = stVoices st
              cap = MV.length vec
          if n >= cap
            then pure st { stNow = now' }
            else do
              let (baseL, baseR) = panGains amp pan
                  ampL = baseL * iGain inst
                  ampR = baseR * iGain inst
                  adsrSpec = maybe (iAdsrDefault inst) id adsrOverride
                  adsr' = adsrSpec { aSustain = clamp01 (aSustain adsrSpec) }
                  env0 = envInit

                  osc0 =
                    oscSetGlideSec srF glideSec $
                    oscInit (iWaveform inst) srF freqHz

                  filt0 =
                    case iFilter inst of
                      Nothing   -> Nothing
                      Just spec -> Just (filterStateInit srF freqHz spec)

                  filtEnv0 =
                    case iFilter inst of
                      Nothing   -> Nothing
                      Just spec ->
                        if needsFiltEnv spec then Just envInit else Nothing

                  vNew = Voice osc0 filt0 filtEnv0 0 freqHz holdFrames baseL baseR ampL ampR adsr'
                            env0 (Just nid) (Just iid) now'

              MV.write vec n vNew
              pure st { stActiveCount = n + 1, stNow = now' }

lookupInstrument ∷ InstrumentId → AudioState → IO (Maybe Instrument)
lookupInstrument (InstrumentId i) st = do
  let vec = stInstruments st
  if i < 0 || i >= MV.length vec
    then pure Nothing
    else MV.read vec i

releaseInstrumentNote ∷ InstrumentId → NoteId → AudioState → IO AudioState
releaseInstrumentNote iid nid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i
        | i >= n    = pure st
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid && vNoteId v == Just nid
              then do
                let fe' = fmap envRelease (vFiltEnv v)
                MV.write vec i v { vEnv = envRelease (vEnv v), vFiltEnv = fe' }
              else pure ()
            go (i+1)
  go 0

panGains ∷ Float → Float → (Float, Float)
panGains amp pan =
  let panClamped = max (-1) (min 1 pan)
      ang = (realToFrac (panClamped + 1) ∷ Double) * (pi/4)
      gL = realToFrac (cos ang) ∷ Float
      gR = realToFrac (sin ang) ∷ Float
  in (amp * gL, amp * gR)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

softClipCubic ∷ CFloat → CFloat
softClipCubic x =
  let ax = abs x
  in if ax <= 1
       then x - (x*x*x)/3
       else (signum x) * (2/3)

softClipTanh ∷ Float → Float
softClipTanh x = tanh x

clamp01 ∷ Float → Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x

releaseInstrumentAllVoices ∷ InstrumentId → AudioState → IO AudioState
releaseInstrumentAllVoices iid st = do
  let vec = stVoices st
      n   = stActiveCount st
      go i
        | i >= n = pure st
        | otherwise = do
            v <- MV.read vec i
            if vInstrId v == Just iid
              then do
                let fe' = fmap envRelease (vFiltEnv v)
                MV.write vec i v
                  { vEnv = envRelease (vEnv v)
                  , vFiltEnv = fe'
                  , vHoldRemain = 0
                  }
              else pure ()
            go (i+1)
  go 0

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderIfNeeded ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderIfNeeded h rb chunkFrames st0 = do
  -- Keep a small, stable queue depth based on chunk size.
  -- 4 chunks ~= 21ms at 48k with chunkFrames=256.
  let targetFrames = fromIntegral (chunkFrames * 4) ∷ Word32
      loop st = do
        availRead  <- rbAvailableRead rb
        availWrite <- rbAvailableWrite rb
        if availRead >= targetFrames || availWrite == 0
          then pure st
          else do
            let framesNow = min chunkFrames (fromIntegral availWrite)
            st' <- renderChunkFrames h rb framesNow st
            loop st'
  loop st0

renderChunkFrames ∷ AudioHandle → Ptr MaRB → Int → AudioState → IO AudioState
renderChunkFrames h rb framesNow st = do
  st' <- withForeignPtr (stMixBuf st) $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
        samples = framesNow * 2
        bytes = samples * sizeOf (undefined ∷ CFloat)
    fillBytes out 0 bytes
    mixVoices (fromIntegral (sampleRate h)) framesNow out st

  withForeignPtr (stMixBuf st') $ \out0 -> do
    let out = castPtr out0 ∷ Ptr CFloat
    written <- rbWriteF32 rb out (fromIntegral framesNow)
    when (written /= fromIntegral framesNow) $
      pure ()

  pure st'

mixVoices ∷ Float → Int → Ptr CFloat → AudioState → IO AudioState
mixVoices sr chunkFrames out st0 = do
  let vec = stVoices st0
      loop i st
        | i >= stActiveCount st = pure st
        | otherwise = do
            v <- MV.read vec i
            v' <- mixOneVoice sr chunkFrames out v
            if eStage (vEnv v') == EnvDone
              then do
                let lastIx = stActiveCount st - 1
                when (i /= lastIx) $
                  MV.read vec lastIx >>= MV.write vec i
                loop i st { stActiveCount = lastIx }
              else do
                MV.write vec i v'
                loop (i+1) st
  loop 0 st0

mixOneVoice sr chunkFrames out v0 = do
  let framesToDo = chunkFrames
      retuneEvery = 16 ∷ Int

      go :: Int -> Ptr CFloat -> Voice -> IO Voice
      go i p v
        | i >= framesToDo = pure v
        | otherwise = do
            let ampL = vAmpL v
                ampR = vAmpR v
                adsrSpec = vADSR v

            let hold = vHoldRemain v
                env0 = if hold <= 0 then envRelease (vEnv v) else vEnv v
                (env1, lvl) = envStep sr adsrSpec env0

                (osc1, x0) = oscStep (vOsc v)

                (mfilt1, mfenv1, tick1, x1) =
                  case vFilter v of
                    Nothing -> (Nothing, vFiltEnv v, vFiltTick v, x0)
                    Just fs0 ->
                      let spec = fsSpec fs0
                          tick0 = vFiltTick v
                          tickN = tick0 + 1
                          (mfenvN, envLvl) =
                            if fEnvAmountOct spec == 0 && fQEnvAmount spec == 0
                              then (vFiltEnv v, 0)
                              else case vFiltEnv v of
                                     Nothing -> (Just envInit, 0)
                                     Just fe0 ->
                                       let (fe1, e) = envStep sr (fEnvADSR spec) fe0
                                       in (Just fe1, e)

                          baseCutoff = keyTrackedBaseCutoff (vNoteHz v) spec
                          cutoffHz   = filterEnvCutoff baseCutoff envLvl spec
                          qEff       = if fQEnvAmount spec == 0 then fQ spec else filterEnvQ envLvl spec

                          fsRetuned =
                            if (fEnvAmountOct spec /= 0 || fQEnvAmount spec /= 0) && (tickN `mod` retuneEvery == 0)
                              then filterRetune sr cutoffHz qEff fs0
                              else fs0

                          (fs1, y0) = filterStep sr fsRetuned x0
                      in (Just fs1, mfenvN, tickN, y0)

                sL = realToFrac (x1 * (ampL * lvl)) :: CFloat
                sR = realToFrac (x1 * (ampR * lvl)) :: CFloat

            curL <- peek p
            curR <- peek (p `advancePtr` 1)
            poke p (curL + sL)
            poke (p `advancePtr` 1) (curR + sR)

            let v' = v
                  { vOsc = osc1
                  , vFilter = mfilt1
                  , vFiltEnv = mfenv1
                  , vFiltTick = tick1
                  , vEnv = env1
                  , vHoldRemain = hold - 1
                  }

            go (i+1) (p `advancePtr` 2) v'
  go 0 out v0
