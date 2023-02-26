module Main where

import Control.Concurrent.Async
--import Codec.Picture
import Data.Word
import Debug.Trace
import Graphics.Image
import Graphics.Image.Interface
import qualified Data.Vector.Unboxed as VU
import Data.Bits (Bits(xor))
import Control.Concurrent (yield)
--import Data.Vector.Generic


-- QOI file format header
--data Header = Header
--  { hMagic :: MatchASCII "QOI magic" "qoif"
--  , hWidth :: Word32
--  , hHeight :: Word32
--  , hChannels :: Word8
--  , hColorspace :: Word8
--  } deriving (Eq, Show, Generic, Binary)

-- QOI internal data types
data QOIPixel =
  QOIPixelRaw Double Double Double |
  QOIPixelDiff8 Double Double Double |
  QOIPixelDiff16 Double Double Double |
  QOIPixelRun Int | -- maybe add prev pix?
  QOIPixelIndex Int
  deriving Show

instance Eq QOIPixelRaw where
  QOIPixelRaw r1 g1 b1 == QOIPixelRaw r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2

hash :: QOIPixelRaw -> Int
hash (QOIPixelRaw r g b) = (r * 3 + g * 5 + b * 7 + 11) `mod` 64

parsePixel :: QOIPixel -> Int
parsePixel (QOIPixelRaw _ _ b) = round b
parsePixel (QOIPixelDiff8 _ _ b) = round b
parsePixel (QOIPixelDiff16 _ _ b) = round b
parsePixel (QOIPixelRun n) = n
parsePixel (QOIPixelIndex n) = n


-- Encoding Algorithm
-- prev pix == curr pix               -> QOI_OP_RUN
-- prev pix dif curr pix < smol_diff  -> QOI_OP_DIFF
-- prev pix dif curr pix < big_diff   -> QOI_OP_LUMA
-- curr pix seen before?              -> QOI_OP_INDEX
-- cant have shit in detroit huh      -> QOI_OP_RGB

--hash :: QOIPixelRaw -> Int
-- 	index_position = (r * 3 + g * 5 + b * 7 + a * 11) % 64

--encode :: [QOIPixelRaw] -> [QOIPixel]
--encode path = do
--    file <- readImage path
--    let encodedImage = qoi file
--    return encodedImage

processPixels [] = return ()
processPixels [a] = return ()
processPixels (prev:curr:rest) = do
    case curr of
        -- Case 1 where current pixel is the same as the previous pixel
        --x | x == prev -> print "Same pixel\n"
        -- Case 2 where current pixel gets stored as a diff of the previous
        --x | isDiff x -> addAsDiff x
        -- Case 3 where there is a larger diff between pixels
        --x | isLargeDiff x -> addAsLargeDiff x
        -- Case 4 where we have seen this pixel before, add to map
        --x | seenBefore x -> addAsIndex x
        -- Case 5 default case we just add it as it is
        x -> print x
    processPixels (curr:rest)


encode pixels = do
    processPixels pixels

mapPixels :: [(Pixel RGB Double)] -> [QOIPixel]
mapPixels arr = Prelude.map pixelToTuple arr


-- Convert HIP pixel class to internal QOIPixelRaw
pixelToTuple :: (Pixel RGB Double) -> QOIPixel
pixelToTuple (PixelRGB r g b) = QOIPixelRaw r g b
--
main :: IO ()
main = do
--   Load image from file
  image <- readImageRGB VU "test.png"
  let pixels = mapPixels (VU.toList (toVector image))
  Main.encode pixels