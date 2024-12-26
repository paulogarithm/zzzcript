-- import Data.Word(Word8)
-- import Data.Bits((.&.))
import Foreign.Ptr(castPtr)
import Foreign.Storable(Storable, poke, peek)
import Foreign.Marshal.Alloc(alloca)
import IO.

nums = [0xbf, 0x94, 0xdc, 0x9e, 0xa, 0xb8, 0xbd, 0xa4, 0x80, 0x4]

-- readULEB128' :: Int -> 
-- readULEB128' shift res buf = 

rawCast :: Storable a => Storable b => a -> IO b
rawCast x = alloca (\p -> poke (castPtr p) (x) >> (peek p))

-- readULEB128 :: [Word8] -> (Float, Int)
-- readULEB128 buf | buf[0] >= 0x80 = (buf[0], 1)
--                 | otherwise = (0, 0)