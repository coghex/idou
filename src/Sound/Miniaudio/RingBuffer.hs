module Sound.Miniaudio.RingBuffer
  ( MaRB
  , rbCreateF32
  , rbDestroy
  , rbWriteF32
  , rbReadF32
  , rbAvailableRead
  ) where

import Foreign
import Foreign.C
import Data.Word (Word32)

data MaRB -- opaque C struct hs_ma_rb

foreign import ccall "hs_ma_rb_create_f32"
  c_rbCreateF32 :: Word32 -> Word32 -> IO (Ptr MaRB)

foreign import ccall "hs_ma_rb_destroy"
  rbDestroy :: Ptr MaRB -> IO ()

foreign import ccall "hs_ma_rb_write_f32"
  c_rbWriteF32 :: Ptr MaRB -> Ptr CFloat -> Word32 -> IO Word32

foreign import ccall "hs_ma_rb_read_f32"
  c_rbReadF32 :: Ptr MaRB -> Ptr CFloat -> Word32 -> IO Word32

foreign import ccall "hs_ma_rb_available_read"
  rbAvailableRead :: Ptr MaRB -> IO Word32

rbCreateF32 :: Word32 -> Word32 -> IO (Ptr MaRB)
rbCreateF32 capacityFrames channels = c_rbCreateF32 capacityFrames channels

rbWriteF32 :: Ptr MaRB -> Ptr CFloat -> Word32 -> IO Word32
rbWriteF32 = c_rbWriteF32

rbReadF32 :: Ptr MaRB -> Ptr CFloat -> Word32 -> IO Word32
rbReadF32 = c_rbReadF32
