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
import Data.Map
import qualified Control.Applicative as Map
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
processPixels [a] run seen out =
  if run > 0
  then out ++ [QOIPixelRun run]
  else if member (hash a) seen
    then (out ++ [QOIPixelIndex $ hash a])
    else (out ++ [toQOIPixel a])
processPixels (prev:curr:rest) run seen out = --TODO add to start of list first pixel when calling func
  -- Case 1 where current pixel is the same as the previous pixel
  if prev == curr
  then if run >= 62
    then processPixels (curr:rest) 0 seen (out ++ [QOIPixelRun run])
    else processPixels (curr:rest) (run + 1) seen out
  else if
  | prev == curr = if run >= 62
                   then processPixels (curr:rest) 0 seen (out ++ [QOIPixelRun run])
                   else processPixels (curr:rest) (run + 1) seen out
  -- Case 2 if we've seen curr pixel before
  | member (hash curr) seen = processPixels (curr:rest) 0 seen ((appendRun out run) ++ [QOIPixelIndex (hash curr)])
  -- Case 3 small difference
  | isSmolDiff (getPixelDiff prev curr) = processPixels (curr:rest) 0 (insert (hash curr) curr seen) ((appendRun out run) ++ [toQOIPixelDiffSmol (getPixelDiff prev curr)])
  -- Case 4 large difference
  | isBeegDiff (getPixelDiff prev curr) = processPixels (curr:rest) 0 (insert (hash curr) curr seen) ((appendRun out run) ++ [toQOIPixelDiffBeeg (getPixelDiff prev curr)])
  -- Case 5 default case we just add it as it is
  | otherwise = processPixels (curr:rest) 0 (insert (hash curr) curr seen) (out ++ [(toQOIPixel curr)])


--encode :: [PixelRaw] -> [QOIPixel]
--encode pxs = do
--  let processed = processPixels pxs 0 empty []

mapPixels :: [(Pixel RGB Double)] -> [PixelRaw]
mapPixels arr = Prelude.map pixelToTuple arr


-- Convert HIP pixel class to internal QOIPixelRaw
pixelToTuple :: (Pixel RGB Double) -> PixelRaw
pixelToTuple (PixelRGB r g b) = PixelRaw (round $ 255 * (r :: Double)) (round $ 255 * (g :: Double)) (round $ 255 * (b :: Double))

--
main :: IO ()
main = do
--   Load image from file
  image <- readImageRGB VU "test.png"
  let pixels = mapPixels (VU.toList (toVector image))
  let processedPixels = processPixels pixels 0 empty []
  print processedPixels