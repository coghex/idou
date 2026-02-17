{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Sound.Miniaudio
  ( -- * Types
    MaResult, pattern MA_SUCCESS
  , MaDevice, MaDeviceConfig
  , MaFormat(..)

    -- * Callback
  , MaDataCallback
  , mkMaDataCallback

    -- * Helpers (allocate config/device)
  , hs_ma_device_malloc
  , hs_ma_device_free
  , hs_ma_device_config_init_playback
  , hs_ma_device_config_free

    -- * Device lifecycle
  , ma_device_init
  , ma_device_start
  , ma_device_stop
  , ma_device_uninit
  ) where

import Foreign
import Foreign.C

type MaResult = CInt
pattern MA_SUCCESS :: MaResult
pattern MA_SUCCESS = 0

data MaDevice
data MaDeviceConfig

-- Subset of ma_format.
data MaFormat
  = MaFormatU8
  | MaFormatS16
  | MaFormatS24
  | MaFormatS32
  | MaFormatF32
  deriving (Eq, Show)

toMaFormat :: MaFormat -> CInt
toMaFormat MaFormatU8  = 1
toMaFormat MaFormatS16 = 2
toMaFormat MaFormatS24 = 3
toMaFormat MaFormatS32 = 4
toMaFormat MaFormatF32 = 5

-- void dataCallback(ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount)
type MaDataCallback =
     Ptr MaDevice
  -> Ptr ()   -- pOutput
  -> Ptr ()   -- pInput (unused for playback-only)
  -> Word32   -- frameCount
  -> IO ()

foreign import ccall "wrapper"
  mkMaDataCallback :: MaDataCallback -> IO (FunPtr MaDataCallback)

-- C helper allocators
foreign import ccall "hs_ma_device_malloc"
  hs_ma_device_malloc :: IO (Ptr MaDevice)

foreign import ccall "hs_ma_device_free"
  hs_ma_device_free :: Ptr MaDevice -> IO ()

foreign import ccall "hs_ma_device_config_init_playback"
  c_hs_ma_device_config_init_playback
    :: CInt                 -- format
    -> Word32               -- channels
    -> Word32               -- sampleRate
    -> FunPtr MaDataCallback
    -> Ptr ()               -- pUserData
    -> IO (Ptr MaDeviceConfig)

hs_ma_device_config_init_playback
  :: MaFormat
  -> Word32
  -> Word32
  -> FunPtr MaDataCallback
  -> Ptr ()
  -> IO (Ptr MaDeviceConfig)
hs_ma_device_config_init_playback fmt ch sr cb userData =
  c_hs_ma_device_config_init_playback (toMaFormat fmt) ch sr cb userData

foreign import ccall "hs_ma_device_config_free"
  hs_ma_device_config_free :: Ptr MaDeviceConfig -> IO ()

-- miniaudio API
foreign import capi "miniaudio.h ma_device_init"
  ma_device_init
    :: Ptr ()            -- pContext (NULL for default)
    -> Ptr MaDeviceConfig
    -> Ptr MaDevice
    -> IO MaResult

foreign import capi "miniaudio.h ma_device_start"
  ma_device_start :: Ptr MaDevice -> IO MaResult

foreign import capi "miniaudio.h ma_device_stop"
  ma_device_stop :: Ptr MaDevice -> IO MaResult

foreign import capi "miniaudio.h ma_device_uninit"
  ma_device_uninit :: Ptr MaDevice -> IO ()
