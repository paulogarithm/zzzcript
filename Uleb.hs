import Data.Word
import Data.Bits
import Foreign.Storable(Storable, poke, peek)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(castPtr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Bits(shiftR, (.&.), (.|.))
import Data.Word(Word8, Word32, Word64)
import Data.ByteString.Builder (toLazyByteString, word8)

type Float32 = Float
type Float64 = Double

word64splitter :: Word64 -> (Word32, Word32)
word64splitter x = (fromIntegral (x .&. 0xFFFFFFFF), fromIntegral (x `shiftR` 32))

uleb128Encode :: Word32 -> [Word8]
uleb128Encode x = encode' x [] where
    encode' :: Word32 -> [Word8] -> [Word8]
    encode' x acc
      | x >= 0x80  = encode' (x `shiftR` 7) ((fromIntegral (x .&. 0x7F) .|. 0x80) : acc)
      | otherwise  = (fromIntegral x : acc)


doubleRawcastInt :: Float64 -> Word64
doubleRawcastInt f = unsafePerformIO i
    where i = alloca (\p -> poke (castPtr p) f >> peek p) :: IO Word64

myDoubleEncode :: Double -> [Word8]
myDoubleEncode f = let
    x = doubleRawcastInt f
    (lo, hi) = word64splitter x
  in uleb128Encode lo ++ uleb128Encode hi

-- Test encoding
-- main :: IO ()
-- main = do
--   let encoded = encodeFloat 3.14
--   print encoded
