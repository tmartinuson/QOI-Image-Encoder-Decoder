module Main where

import Control.Concurrent.Async
--import Codec.Picture
import Data.Word
import Debug.Trace
import Graphics.Image
import Graphics.Image.Interface
import qualified Data.Vector.Unboxed as VU
import Data.Bits (Bits(xor, shiftL))
import Control.Concurrent (yield)
import Data.Map
import qualified Control.Applicative as Map
import Data.Bits
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
--import Data.Vector.Generic



-- the input type of pixel
data PixelRaw = PixelRaw Int Int Int
  deriving (Show, Eq)

toQOIPixel :: PixelRaw -> QOIPixel
toQOIPixel (PixelRaw r g b) = QOIPixelRaw r g b

toQOIPixelDiffBeeg :: (Int, Int, Int) -> QOIPixel
toQOIPixelDiffBeeg (r, g, b) = QOIPixelDiffBeeg r g b

toQOIPixelDiffSmol :: (Int, Int, Int) -> QOIPixel
toQOIPixelDiffSmol (r, g, b) = QOIPixelDiffSmol r g b

-- QOI internal data types
data QOIPixel =
  QOIPixelRaw Int Int Int |
  QOIPixelDiffSmol Int Int Int |
  QOIPixelDiffBeeg Int Int Int |
  QOIPixelRun Int | -- maybe add prev pix?
  QOIPixelIndex Int
  deriving (Show, Eq)


--instance Eq QOIPixelRaw where
--  QOIPixelRaw r1 g1 b1 == QOIPixelRaw r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2

getPixelDiff :: PixelRaw -> PixelRaw -> (Int, Int, Int)
getPixelDiff (PixelRaw r1 g1 b1) (PixelRaw r2 g2 b2) = ((r1 - r2), (g1 - g2), (b1 - b2))

-- the hash function return values between [0..63], so the Map structure doesn't need to worry about having a limited size
hash :: PixelRaw -> Int
hash (PixelRaw r g b) = (r * 3 + g * 5 + b * 7 + 11) `mod` 64

isSmolDiff :: (Int, Int, Int) -> Bool
isSmolDiff (dr, dg, db) = abs (dr) < 4 &&  abs (dg) < 4 &&  abs (db) < 4

isBeegDiff :: (Int, Int, Int) -> Bool
isBeegDiff (dr, dg, db) = abs (dr) < 32 && abs (dg) < 16 && abs (db) < 16

-- Encoding Algorithm
-- prev pix == curr pix               -> QOI_OP_RUN
-- prev pix dif curr pix < smol_diff  -> QOI_OP_DIFF
-- prev pix dif curr pix < big_diff   -> QOI_OP_LUMA
-- curr pix seen before?              -> QOI_OP_INDEX
-- cant have shit in detroit huh      -> QOI_OP_RGB

--encode :: [QOIPixelRaw] -> [QOIPixel]
--encode path = do
--    file <- readImage path
--    let encodedImage = qoi file
--    return encodedImage

appendRun :: [QOIPixel] -> Int -> [QOIPixel]
appendRun out run
  | run > 0 = out ++ [QOIPixelRun run]
  | otherwise = out

-- first 2 args are the previous pixel and curr pixel.
-- Int is the run length, 0 if none
-- second argument is the map of seen pixels
-- last argument is array that is the array to write to
-- returns an encoded list
processPixels :: [PixelRaw] -> Int -> Map Int PixelRaw -> [QOIPixel] -> [QOIPixel]
processPixels [] _ _ out = out
processPixels [a] run seen out = (appendRun out run)
processPixels (prev:curr:rest) run seen out =
  -- Case 1 where current pixel is the same as the previous pixel
  if prev == curr
    then if run >= 62
      then processPixels (curr:rest) 0 seen (out ++ [QOIPixelRun run])
      else processPixels (curr:rest) (run + 1) seen out
    -- Case 2 if we've seen curr pixel before
    else if member (hash curr) seen
      then processPixels (curr:rest) 0 seen ((appendRun out run) ++ [QOIPixelIndex (hash curr)])
--  -- Case 3 small difference
      else if isSmolDiff (getPixelDiff prev curr)
        then processPixels (curr:rest) 0 (insert (hash curr) curr seen) ((appendRun out run) ++ [toQOIPixelDiffSmol (getPixelDiff prev curr)])
--  -- Case 4 large difference
      else if isBeegDiff (getPixelDiff prev curr)
       then processPixels (curr:rest) 0 (insert (hash curr) curr seen) ((appendRun out run) ++ [toQOIPixelDiffBeeg (getPixelDiff prev curr)])
--  -- Case 5 default case we just add it as it is
       else processPixels (curr:rest) 0 (insert (hash curr) curr seen) ((appendRun out run) ++ [(toQOIPixel curr)])


--encode :: [PixelRaw] -> [QOIPixel]
--encode pxs = do
--  let processed = processPixels pxs 0 empty []

mapPixels :: [(Pixel RGB Double)] -> [PixelRaw]
mapPixels arr = Prelude.map pixelToTuple arr


-- Convert HIP pixel class to internal QOIPixelRaw
pixelToTuple :: (Pixel RGB Double) -> PixelRaw
pixelToTuple (PixelRGB r g b) = PixelRaw (round $ 255 * (r :: Double)) (round $ 255 * (g :: Double)) (round $ 255 * (b :: Double))

-- Convert QOIPixel into its corresponding byte by spec
encodeQOIPixelToBinaryString :: QOIPixel -> B.ByteString
encodeQOIPixelToBinaryString (QOIPixelRaw r g b) =
  runPut $ do
    putWord8 254
    putWord8 (fromIntegral r)
    putWord8 (fromIntegral g)
    putWord8 (fromIntegral b)
encodeQOIPixelToBinaryString (QOIPixelDiffSmol dr dg db) =
  runPut $ do
    putWord8 ((1 `shiftL` 6) .|. (((fromIntegral dr) .&. 0x3) `shiftL` 4) .|. (((fromIntegral dg) .&. 0x3) `shiftL` 2) .|. (((fromIntegral db) .&. 0x3)))
encodeQOIPixelToBinaryString (QOIPixelDiffBeeg dr dg db) =
  runPut $ do
    putWord8 ((2 `shiftL` 6) .|. (((fromIntegral dg) .&. 0x3F)))
    let diffDr = dr - dg
    let diffDb = db - dg
    putWord8 ((((fromIntegral diffDr) .&. 0xF) `shiftL` 4) .|. ((fromIntegral diffDb) .&. 0xF))
encodeQOIPixelToBinaryString (QOIPixelRun run) =
  runPut $ do
    putWord8 ((3 `shiftL` 6) .|. (((fromIntegral run) .&. 0x3F)))
encodeQOIPixelToBinaryString (QOIPixelIndex index) =
  runPut $ do
    putWord8 ((0 `shiftL` 6) .|. (((fromIntegral index) .&. 0x3F)))

-- Helper function for mapping all QOIPixels to their binary representations above
encodeToBinary :: [(QOIPixel)] -> [B.ByteString]
encodeToBinary arr = Prelude.map encodeQOIPixelToBinaryString arr

-- Writes a list of ByteStrings to the given FilePath
writeByteStringListToDisk :: FilePath -> [B.ByteString] -> IO ()
writeByteStringListToDisk filePath byteStrings = B.writeFile filePath (mconcat byteStrings)

createHeader :: Int -> Int -> B.ByteString
createHeader width height =
  runPut $ do
    putWord8 113 -- q ie. Magic Bytes that spell qoif
    putWord8 111 -- o
    putWord8 105 -- i
    putWord8 102 -- f
    putWord32be (fromIntegral width)
    putWord32be (fromIntegral height)
    putWord8 3 -- RGB channels only supported
    putWord8 0 -- linear channels for colorspace

createEndMarker :: B.ByteString
createEndMarker = 
  runPut $ do
    putWord32le 0
    putWord8 0
    putWord8 0
    putWord8 0
    putWord8 1

--
main :: IO ()
main = do
--   Load image from file
  image <- readImageRGB VU "test.png"
  let pixels = mapPixels (VU.toList (toVector image))
  print pixels
  let fst = head pixels
  let processedPixels = processPixels pixels 0 (singleton (hash fst) fst) [(toQOIPixel fst)]
  print processedPixels
  let binaryEncoding = [createHeader (cols image) (rows image)] ++ (encodeToBinary processedPixels) ++ [createEndMarker]
  print binaryEncoding
  writeByteStringListToDisk "test.qoi" binaryEncoding
  print "Write successful"
  