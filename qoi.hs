module Main where

import Control.Concurrent.Async
--import Codec.Picture
import Data.Word
import Debug.Trace
import Graphics.Image
import Graphics.Image.Interface
import qualified Data.Vector.Unboxed as VU
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



parsePixel :: QOIPixel -> Int
parsePixel (QOIPixelRaw _ _ b) = round b
parsePixel (QOIPixelDiff8 _ _ b) = round b
parsePixel (QOIPixelDiff16 _ _ b) = round b
parsePixel (QOIPixelRun n) = n
parsePixel (QOIPixelIndex n) = n


-- Encoding Algorithm
-- prev pix == curr pix               -> QOI_OP_RUN
-- prev pix dif curr pix < smol_diff  -> QOI_DIFF_8
-- prev pix dif curr pix < big_diff   -> QOI_DIFF_16
-- curr pix seen before?              -> QOI_INDEX
-- cant have shit in detroit huh      -> QOI_PIXEL

--encode :: FilePath -> IO DynamicImage
--encode path = do
--    file <- readImage path
--    let encodedImage = qoi file
--    return encodedImage

mapPixels :: [(Pixel RGB Double)] -> [QOIPixel]
mapPixels arr = Prelude.map pixelToTuple arr


-- Convert pixel to tuple of RGBA values
pixelToTuple :: (Pixel RGB Double) -> QOIPixel
pixelToTuple (PixelRGB r g b) = QOIPixelRaw r g b
--
main :: IO ()
main = do
--   Load image from file
  image <- readImageRGB VU "images/car.jpg"
  let pixels = mapPixels (VU.toList (toVector image))
  print pixels