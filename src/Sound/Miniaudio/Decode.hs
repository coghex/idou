{-# LANGUAGE Strict, UnicodeSyntax #-}

module Sound.Miniaudio.Decode
  ( DecodedAudio(..)
  , decodeAudioFileF32
  , decodeAudioFileStereoF32
  ) where

import Control.Monad (when)
import Foreign
import Foreign.C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data DecodedAudio = DecodedAudio
  { daChannels   ∷ !Int
  , daSampleRate ∷ !Word32
  , daFrames     ∷ !Int
  , daSamples    ∷ !(VU.Vector Float)
  } deriving (Eq, Show)

foreign import ccall "hs_ma_decode_file_f32"
  c_hs_ma_decode_file_f32
    ∷ CString
    → Word32
    → Ptr (Ptr CFloat)
    → Ptr Word64
    → Ptr Word32
    → Ptr Word32
    → IO CInt

foreign import ccall "hs_ma_free_decoded_audio"
  c_hs_ma_free_decoded_audio
    ∷ Ptr CFloat
    → IO ()

decodeAudioFileF32 ∷ Word32 → FilePath → IO DecodedAudio
decodeAudioFileF32 targetChannels path = do
  when (targetChannels <= 0) $
    ioError (userError "decodeAudioFileF32: targetChannels must be > 0")

  withCString path $ \cPath ->
    alloca $ \ppSamples ->
      alloca $ \pFrames ->
        alloca $ \pChannels ->
          alloca $ \pSampleRate -> do
            poke ppSamples nullPtr
            poke pFrames 0
            poke pChannels 0
            poke pSampleRate 0

            r <- c_hs_ma_decode_file_f32 cPath targetChannels ppSamples pFrames pChannels pSampleRate
            samplesPtr <- peek ppSamples
            when (r /= 0 || samplesPtr == nullPtr) $
              ioError (userError ("decodeAudioFileF32 failed for " <> path <> ": ma_result=" <> show r))

            channels <- peek pChannels
            frames <- peek pFrames
            sampleRate <- peek pSampleRate
            let sampleCount = fromIntegral (frames * fromIntegral channels) ∷ Int
            mvec <- VUM.new sampleCount
            let copyLoop i
                  | i >= sampleCount = pure ()
                  | otherwise = do
                      cf <- peekElemOff samplesPtr i
                      VUM.unsafeWrite mvec i (realToFrac cf)
                      copyLoop (i + 1)
            copyLoop 0
            c_hs_ma_free_decoded_audio samplesPtr
            samples <- VU.unsafeFreeze mvec

            pure DecodedAudio
              { daChannels = fromIntegral channels
              , daSampleRate = sampleRate
              , daFrames = fromIntegral frames
              , daSamples = samples
              }

decodeAudioFileStereoF32 ∷ FilePath → IO DecodedAudio
decodeAudioFileStereoF32 = decodeAudioFileF32 2
